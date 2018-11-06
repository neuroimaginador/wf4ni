#' @title Execute a Flow
#'
#' @description This function allows to run a pipeline (flow) given some of its inputs.
#'
#' @param flow                  (a NIflow) The flow to be executed.
#' @param inputs                (list) Inputs to use, Default: list()
#' @param given_inputs          (list) Additional inputs, Default: NULL
#' @param desired_outputs       (character array) List of outputs to compute, Default: NULL
#' @param initialize_outputs    (logical) initialize outputs?, Default: TRUE
#' @param cleanup               (logical) perform cleanup of intermediate results?, Default: TRUE
#' @param mode                  (character) Mode of execution (only used to run DLmodels), Default: c("debug", "faster", "medium", "slower")
#'
#' @return A list with one (named) field for each \code{desired_output}.
#'
#' @seealso
#'  \code{\link[neurobase]{readnii}}
#' @importFrom neurobase readnii
#' @import igraph
#'
.execute_flow <- function(flow, inputs = list(),
                          given_inputs = NULL,
                          desired_outputs = NULL,
                          initialize_outputs = TRUE,
                          cleanup = TRUE,
                          mode = c("debug", "faster", "medium", "slower"),
                          verbose = FALSE) {

  require(igraph)

  stopifnot(inherits(flow, "NIflow"))

  # Check that inputs is a named list of files and that all of them exist
  # all_exist <- all(sapply(inputs, file.exists))
  #
  # if (!all_exist) {
  #
  #   stop("Not all input files exist.")
  #
  #   flow$log(level = "ERROR",
  #            message = "Not all input files exist.")
  #
  # }

  input_names <- names(inputs)

  # Initialize computed_outputs
  if (initialize_outputs)
    flow$computed_outputs <- list()

  # Check that the desired outputs can be computed
  all_computable <- all(desired_outputs %in% flow$outputs)
  if (!all_computable) {

    warning("Some of the outputs cannot be computed.")

    flow$log(level = "WARNING",
             message = "Some of the outputs cannot be computed.")

  }

  desired_outputs <- desired_outputs[desired_outputs %in% flow$outputs]

  results <- list()

  if (length(desired_outputs) > 0) {

    inputs <- c(inputs, given_inputs)

    # Variables needed to perform cleanup on computed (and unneeded) outputs
    variables_cleanup <- lapply(igraph::adjacent_vertices(graph = flow$graph,
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

                 "nii" = neurobase::readnii(inputs[[name]]),

                 "gz" = neurobase::readnii(inputs[[name]])

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

    mem <- flow$computed_outputs %>%
      purrr::map_dbl(pryr::object_size) %>%
      sum() %>%
      prettyunits::pretty_bytes()

    flow$log(level = "DEBUG",
             message = paste0("Memory used at init: ", mem))

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

          switch(V(flow$graph)$type[process_idx],

                 "function" = {

                   params <- flow$computed_outputs[my_inputs] %>% unname()

                   param_names <- methods::formalArgs(process)

                   # Allow for functions with ... in its arguments
                   if (!("..." %in% param_names))
                     names(params) <- param_names[1:length(params)]

                   flow$computed_outputs[[intermediate_output]] <- do.call(what = process,
                                                                           args = params)


                 }
          )

          mem <- flow$computed_outputs %>%
            purrr::map_dbl(pryr::object_size) %>%
            sum() %>%
            prettyunits::pretty_bytes()

          flow$log(level = "DEBUG",
                   message = paste0("Memory used after computation: ", mem))

          is_computed[intermediate_output] <- TRUE

          if (verbose)
            print(is_computed)

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
                                        stringr::str_flatten(which_to_remove,
                                                             collapse = ", ")))

              if (verbose) {

                cat("Removing", stringr::str_flatten(which_to_remove,
                                                      collapse = ", "),
                    "...\n") # nocov

              }
              flow$computed_outputs[which_to_remove] <- NULL

              all_to_remove <- c(all_to_remove, which_to_remove)

              # Release memory
              invisible(gc())

              mem <- flow$computed_outputs %>%
                purrr::map_dbl(pryr::object_size) %>%
                sum() %>%
                prettyunits::pretty_bytes()

              flow$log(level = "DEBUG",
                       message = paste0("Memory used after cleanup: ", mem))
            }


          }

        }

      }

    }

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
