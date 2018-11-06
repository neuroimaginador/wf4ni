#' @title Reset Flow DLmodels
#'
#' @description This functions resets the specified DLmodels to its original state.
#'
#' @param flow       (a NIflow object) The flow.
#' @param outputs    (list) List of the names assigned to the DLmodels to reset.
#'
#' @return The flow with the DLmodels resetted.
#'
.reset_flow <- function(flow, outputs = "all") {

  # Basic input checks
  stopifnot(inherits(flow, "NIflow"))

  # Actual list of outputs
  if ("all" %in% outputs) {

    outputs <- flow$outputs

  } else {

    outputs <- intersect(outputs, flow$outputs)

  }

  # Substitute the model by its scheme
  for (output in outputs) {

    if (inherits(flow$processes[[output]], "DLmodel")) {

      # flow$processes[[output]] <- NULL
      flow$processes[[output]] <- flow$schemes[[output]]
      flow$trained[[output]] <- FALSE

    }

  }

  return(invisible(flow))

}
