#' @title Execute a Flow
#'
#' @description This function allows to run a pipeline (flow) given some of its inputs.
#'
#' @param flow                  (a NIflow) The flow to be executed.
#' @param inputs                (list) Inputs to use, Default: list()
#' @param desired_outputs       (character array) List of outputs to compute, Default: NULL
#' @param initialize_outputs    (logical) initialize outputs?, Default: TRUE
#' @param ...                   Other arguments passed to subfunctions.
#'
#' @return A list with one (named) field for each \code{desired_output}.
#'
#' @seealso
#'  \code{\link[neurobase]{readnii}}
#' @importFrom neurobase readnii fast_readnii
#' @import igraph
#' @importFrom purrr map_dbl
#' @importFrom pryr object_size
#' @importFrom prettyunits pretty_bytes
#' @importFrom methods formalArgs
#'
#' @export
#'
execute <- function(flow,
                    inputs = list(),
                    desired_outputs = NULL,
                    initialize_outputs = TRUE,
                    ...) {

  expr <- substitute(desired_outputs)
  desired_outputs <- as.character(expr)

  if (class(expr) == "call")
    desired_outputs <- desired_outputs[-1]

  env <- parent.frame(2)
  desired_outputs <- desired_outputs %>% .search_names(envir = env)

  my_flow <- flow$get_private()
  my_flow %>% .execute_flow(inputs = inputs,
                            desired_outputs = desired_outputs,
                            initialize_outputs = initialize_outputs,
                            ...)

}

#' @title Execute a Flow
#'
#' @description This function allows to run a pipeline (flow) given some of its inputs.
#'
#' @param flow                  (a NIflow) The flow to be executed.
#' @param inputs                (list) Inputs to use, Default: list()
#' @param desired_outputs       (character array) List of outputs to compute, Default: NULL
#' @param initialize_outputs    (logical) initialize outputs?, Default: TRUE
#' @param cleanup               (logical) perform cleanup of intermediate results?, Default: TRUE
#' @param verbose               (logical) print information of the executions?, Default: FALSE
#' @param ...                   Other arguments passed to subfunctions.
#'
#' @return A list with one (named) field for each \code{desired_output}.
#'
#' @seealso
#'  \code{\link[neurobase]{readnii}}
#' @importFrom neurobase readnii fast_readnii
#' @import igraph
#' @importFrom purrr map_dbl
#' @importFrom pryr object_size
#' @importFrom prettyunits pretty_bytes
#' @importFrom methods formalArgs
#'
.execute_flow <- function(flow,
                          inputs = list(),
                          desired_outputs = NULL,
                          initialize_outputs = TRUE,
                          cleanup = TRUE,
                          verbose = FALSE,
                          ...) {


  # An auxiliary function to get a list size in memory.
  mem_list <- function(L) {

    L %>% map_dbl(object_size) %>%
      sum() %>%
      pretty_bytes()

  }

  stopifnot(inherits(flow, "NIflow"))

  input_names <- names(inputs)

  # Initialize computed_outputs
  if (initialize_outputs)
    flow$computed_outputs <- list()

  # Check that the desired outputs can be computed
  all_computable <- all(desired_outputs %in% flow$outputs)
  if (!all_computable) {

    not_computable <- desired_outputs[!(desired_outputs %in% flow$outputs)]

    warning("Some of the outputs cannot be computed.")

    flow$log(level = "WARNING",
             message = paste0("Some of the outputs cannot be computed: ",
                              str_flatten(not_computable, collapse = ", ")))

  }

  desired_outputs <- desired_outputs[desired_outputs %in% flow$outputs]

  results <- list()

  if (length(desired_outputs) > 0) {

    # Variables needed to perform cleanup on computed (and unneeded) outputs
    variables_cleanup <- lapply(adjacent_vertices(graph = flow$graph,
                                                  v = V(flow$graph)),
                                function(s) attr(s, "name"))

    is_computed <- rep(FALSE, length(flow$outputs))
    names(is_computed) <- flow$outputs

    # Read inputs
    for (name in input_names) {

      # If a file is provided as input, read it
      if (is.character(inputs[[name]]) && file.exists(inputs[[name]])) {

        flow$log(level = "DEBUG", message = paste0("Reading input ", name,
                                                   " from file ", inputs[[name]]))

        flow$computed_outputs[[name]] <-
          switch(tools::file_ext(inputs[[name]]),

                 "rds" = readRDS(inputs[[name]]),

                 "nii" = fast_readnii(inputs[[name]]),

                 "gz" = fast_readnii(inputs[[name]])

          )

      } else {

        # Another data type is provided

        flow$log(level = "DEBUG", message = paste0("Using provided input ", name))

        flow$computed_outputs[[name]] <- inputs[[name]]

      }

      is_computed[name] <- TRUE

    }

    # For each output
    pipelines <- list()
    pipelines_names <- list()
    for (output in desired_outputs) {

      # Define which parts of the flow must be processed
      pipeline <- flow$pipeline[[output]]

      to_compute <- flow %>% .which_to_compute(output = output,
                                               given_inputs = input_names)

      pipeline <- intersect(flow$outputs[pipeline], c(to_compute, output))
      pipelines_names[[output]] <- pipeline

      pipeline <- match(pipeline, flow$outputs)

      pipelines[[output]] <- pipeline

    }

    all_pipelines <- pipelines_names %>% unlist() %>% unique()

    all_pipelines <- c(all_pipelines, input_names) %>% unique()

    # Just variables which need to be computed in this execution.
    is_computed[all_pipelines] <- is_computed[all_pipelines] | FALSE

    all_to_remove <- c()

    mem <- flow$computed_outputs %>% mem_list()

    flow$log(level = "DEBUG",
             message = paste0("Memory used at init: ", mem))

    # Insert shims: variations of tempdir() to save temporary
    # results to a specific folder:
    .insert_wf4ni_shims(flow$work_dir)
    .create_tempdir()
    flow$log(level = "DEBUG",
             message = paste0("Switching tempdir to: ", flow$work_dir))

    for (output in desired_outputs) {

      pipeline <- pipelines[[output]]

      # Execute in order
      if (length(pipeline) > 0) {

        for (process_idx in pipeline) {

          intermediate_output <- flow$outputs[process_idx]

          # if this process is already computed, go to the next one
          if (!is.null(flow$computed_outputs[[intermediate_output]])) next
          if (is_computed[intermediate_output]) next

          if (verbose) {

            cat("Computing", intermediate_output, "...\n") # nocov

          }

          flow$log(level = "DEBUG",
                   message = paste0("Computing ", intermediate_output, "..."))

          process <- flow$processes[[intermediate_output]]
          my_inputs <- flow$inmediate_inputs[[intermediate_output]]

          # Execute the corresponding process
          params <- flow$computed_outputs[my_inputs] %>% unname()

          if (inherits(process, "function")) {

            param_names <- formalArgs(process)

            # Allow for functions with ... in its arguments
            if (!("..." %in% param_names))
              names(params) <- param_names[1:length(params)]

          }

          extra_args <- list(...)

          flow$computed_outputs[[intermediate_output]] <-
            flow$execute_process(what = process,
                                 args = c(params, extra_args))

          mem <- flow$computed_outputs %>% mem_list()

          flow$log(level = "DEBUG",
                   message = paste0("Memory used after computation: ", mem))

          # Save result:
          .save_result(value = flow$computed_outputs[[intermediate_output]],
                       name = intermediate_output)

          is_computed[intermediate_output] <- TRUE

          # if (verbose)
          #   print(is_computed) #nocov

          # Perform cleanup
          if (cleanup) {

            # Intermediate results that are not needed any more.
            # All of their children are already computed.
            all_computed <- sapply(variables_cleanup,
                                   function(s) {

                                     all(is_computed[s])

                                   })

            which_to_remove <- names(variables_cleanup)[all_computed]

            which_to_remove <- setdiff(which_to_remove, desired_outputs)

            which_to_remove <- setdiff(which_to_remove, all_to_remove)

            if (length(which_to_remove) > 0) {

              flow$log(level = "DEBUG",
                       message = paste0("Removed intermediate outputs: ",
                                        str_flatten(which_to_remove,
                                                    collapse = ", ")))

              if (verbose) {

                str_remove <- str_flatten(which_to_remove, collapse = ", ") #nocov
                cat("Removing", str_remove, "...\n") # nocov

              }
              flow$computed_outputs[which_to_remove] <- NULL

              all_to_remove <- c(all_to_remove, which_to_remove)

              # Release memory
              invisible(gc())

              mem <- flow$computed_outputs %>% mem_list()

              flow$log(level = "DEBUG",
                       message = paste0("Memory used after cleanup: ", mem))

            }


          }

        }

      }

    }

    # Remove the previously inserted shims.
    .remove_wf4ni_shims()
    flow$log(level = "DEBUG",
             message = paste0("Switching to base tempdir"))


    flow$log(level = "DEBUG",
             message = "Computed all results")

    results <- flow$computed_outputs[desired_outputs]

  }

  return(results)

}

#' @title Remove Previously Computed Outputs
#'
#' @description This function deletes previously computed outputs inside the flow.
#'
#' @param flow    (a NIflow object) The flow for which outputs are to be removed.
#'
#' @return (Invisibly) the same flow, without pre-computed outputs.
#'
.reset_outputs <- function(flow) {

  flow$computed_outputs <- list()

  flow$log(level = "DEBUG", message = "Outputs reset")

  return(invisible(flow))

}

#' @title Pipeline to Execute
#'
#' @description This internal function returns the list of steps to compute the given output.
#'
#' @param flow            (a NIflow object) The flow.
#' @param output          (character) The name of the output to compute.
#' @param given_inputs    (list) Additional inputs provided, Default: NULL
#'
#' @return A vector with the corresponding steps needed to calculate the \code{output}.
#'
#' @details DETAILS
#'
.which_to_compute <- function(flow, output, given_inputs = NULL) {

  # If it's an input, return c()
  if (output %in% given_inputs) return(c())

  # Explore the inmediate inputs
  required <- flow$inmediate_inputs[[output]]

  # If any of the inmediate inputs is provided, remove from the list
  to_compute <- setdiff(required, given_inputs)

  # Search the pipeline recursively
  result <- to_compute

  if (length(to_compute) > 0) {

    for (parent in to_compute)

      result <- c(result, .which_to_compute(flow, parent, given_inputs))

  }

  return(unique(result))

}
