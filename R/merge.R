#' Merge Two Flows
#'
#' @param flow1   (NIflow object) First flow to merge
#' @param flow2   (NIflow object) Second flow to merge
#' @param rename  (logical) Should inputs/outputs be renamed if the other flow has the same?. Default: TRUE.
#'
#' @return A \code{NIflow} that merges the two flows.
#' @export
#'
merge <- function(flow1, flow2, rename = TRUE) {

  # The result of the merge is a flow with the same inputs and outputs as
  # the two flows together.
  # We begin by cloning the first flow and then adding the inputs and outputs
  # of the second.
  res <- flow1$deep_clone()

  old_inputs <- flow1$get_inputs()
  old_outputs <- flow1$get_outputs()

  new_inputs <- flow2$get_inputs()
  new_outputs <- flow2$get_outputs()

  # Suffix for new outputs or inputs if duplicated
  new_name <- flow2$name()

  coincident_inputs <- new_inputs %in% old_inputs
  coincident_outputs <- new_outputs %in% old_outputs

  if (any(coincident_inputs) | any(coincident_outputs)) {

    if (!rename) {

      stop("Coincident input/output names in the two flows and rename = FALSE.")

    }

  }

  res_inputs <- new_inputs
  res_inputs[coincident_inputs] <- paste0(res_inputs[coincident_inputs], "_", new_name)

  res_outputs <- new_outputs
  res_outputs[coincident_outputs] <- paste0(res_outputs[coincident_outputs], "_", new_name)

  # Now, add everything
  res$add(inputs = res_inputs)

  internal_flow <- flow2$get_private()

  inmediate_inputs <- internal_flow$inmediate_inputs

  for (out_id in seq_along(res_outputs)) {

    out <- res_outputs[out_id]

    if (!out %in% res_inputs) {

      # If we're not adding an input
      old_out <- new_outputs[out_id]

      res$add(what = flow2$get_process(old_out),
              inputs = res_outputs[match(inmediate_inputs[old_out], new_outputs)],
              output = out)

    }

  }

  # We have to merge the dependencies
  new_pkgs <- internal_flow$pkgs
  names(new_pkgs) <- setdiff(res_outputs, res_inputs)

  internal_res <- res$get_private()
  internal_res$new_pkgs <- new_pkgs

  with(internal_res,
       pkgs <- c(pkgs, new_pkgs))

  return(res)

}