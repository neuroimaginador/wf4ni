#' @title Add a Trainable Model
#'
#' @description This function is used to add a (trainable) DLmodel to the flow.
#'
#' @param flow       (a NIflow object) The flow where to add the DLmodel
#' @param scheme     (a DLscheme object) The DLscheme defining the DLmodel, Default: NULL
#' @param inputs     (list) List of needed inputs (just names, with or without quotes), Default: list()
#' @param output     (character) The expected output provided by the DLmodel.
#' @param subset     (NULL) Subset of the problem (see DLmodel especification), Default: NULL
#' @param verbose    (logical) verbose output?, Default: TRUE
#'
#' @return The flow with the DLmodel added
#'
#' @import igraph
#'
.add_trainable_model <- function(flow,
                                scheme = NULL,
                                inputs,
                                output,
                                subset = NULL,
                                verbose = FALSE) {

  suppressPackageStartupMessages(require(igraph))

  # Basic checks
  stopifnot(inherits(flow, "NIflow"))
  stopifnot(inherits(scheme, "DLscheme"))

  scheme$check()

  # stopifnot(!is.null(scheme$vol_layers_pattern))
  # stopifnot(!is.null(scheme$last_hidden_layers))
  # stopifnot(!is.null(scheme$units) | !is.null(scheme$output_width))

  if (is.null(scheme$units))
    scheme$units <- scheme$output_width ^ 3

  # Remaining parameters to be computed
  params <- scheme
  params$labels_subset <- subset

  class(params) <- c("DLscheme", class(params))

  # Store the model in the flow
  if (verbose)
    cat("Storing configuration\n") # nocov

  flow %>% .add_process(proc = params, inputs = inputs, output = output, trained = FALSE)

  return(invisible(flow))

}
