#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow                  (name) PARAM_DESCRIPTION
#' @param inputs                (call) PARAM_DESCRIPTION, Default: list()
#' @param given_inputs          (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param desired_outputs       (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param initialize_outputs    (logical) PARAM_DESCRIPTION, Default: TRUE
#' @param mode                  (call) PARAM_DESCRIPTION, Default: c("debug", "faster", "medium", "slower")
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso
#'  \code{\link[neurobase]{readnii}}
#' @export
#' @importFrom neurobase readnii
#' @import igraph
execute_flow <- function(flow, inputs = list(),
                         given_inputs = NULL,
                         desired_outputs = NULL,
                         initialize_outputs = TRUE,
                         mode = c("debug", "faster", "medium", "slower"),
                         verbose = FALSE) {

  require(igraph)

  stopifnot(inherits(flow, "DLflow"))

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
  # input_names <- input_names[input_names %in% flow$inputs]

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

    }

    # For each output
    for (output in desired_outputs) {

      # Define which parts of the flow must be processed
      pipeline <- flow$pipeline[[output]]

      to_compute <- flow %>% which_to_compute(output = output,
                                              given_inputs = input_names)

      pipeline <- intersect(flow$outputs[pipeline], c(to_compute, output))
      pipeline <- match(pipeline, flow$outputs)


      # Execute in order
      if (length(pipeline) > 0) {

        for (process_idx in pipeline) {

          intermediate_output <- flow$outputs[process_idx]

          # if this process is already computed, go to the next one
          if (!is.null(flow$computed_outputs[[intermediate_output]])) next

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

        }

      }

    }

    flow$log(level = "DEBUG",
             message = "Computed all results")

    results <- flow$computed_outputs[desired_outputs]

  }

  return(results)

}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export
reset_outputs <- function(flow) {

  flow$computed_outputs <- list()

  flow$log(level = "DEBUG", message = "Outputs reset")

  return(invisible(flow))

}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow            (name) PARAM_DESCRIPTION
#' @param output          (name) PARAM_DESCRIPTION
#' @param given_inputs    (NULL) PARAM_DESCRIPTION, Default: NULL
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso
#'
#' @export
#' @import
which_to_compute <- function(flow, output, given_inputs = NULL) {

  if (output %in% given_inputs) return(c())

  required <- flow$inmediate_inputs[[output]]

  # available <- intersect(required, given_inputs)
  to_compute <- setdiff(required, given_inputs)

  result <- to_compute

  if (length(to_compute) > 0) {

    for (parent in to_compute)

      result <- c(result, which_to_compute(flow, parent, given_inputs))

  }

  # result <- setdiff(result, flow$inputs)

  return(unique(result))

}
