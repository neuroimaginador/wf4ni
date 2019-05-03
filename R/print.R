#' Print Flow Information
#'
#' @param flow   (a \code{NIflow} object) Flow to print
#'
#' @return Prints the name, inputs, outputs, package dependencies and memory used.
#' @export
#'
print.NIflow <- function(flow) {

  has_crayon <- requireNamespace("crayon", quietly = TRUE)

  if (!has_crayon) {

    bold <- red <- green <- underline <- blue <- function(s) s

  }

  # Name, inputs and outputs
  flow_name <- flow$name() %>% bold()
  flow_inputs <- str_flatten(flow$get_inputs() %>% green(), collapse = ", ")
  flow_outputs <- str_flatten(setdiff(flow$get_outputs(),
                                      flow$get_inputs()) %>% blue(),
                              collapse = ", ")

  cat(underline("NIflow object with:\n"),
      " name:", flow_name, "\n",
      " inputs:", flow_inputs, "\n",
      " outputs:", flow_outputs, "\n")

  # Dependencies
  flow_deps <- flow$get_dependencies()

  if (length(flow_deps) > 0) {

    cat("It depends on functions of the following packages:",
        str_flatten(flow_deps %>% red()), "\n")

  }

  # Currently used memory
  cat("Currently using", flow$memory_used() %>% prettyunits::pretty_bytes(),
      "of memory.\n")

  invisible(TRUE)

}

#' Summary of Flow
#'
#' @param flow   (a \code{NIflow} object) Flow to print summary of.
#'
#' @return Prints inputs, outputs and dependencies between them.
#' @export
#'
summary.NIflow <- function(flow) {

  # If available, pretty-print summary
  has_crayon <- requireNamespace("crayon", quietly = TRUE)

  if (!has_crayon) {

    underline <- function(s) s

  }

  print(flow)

  # Dependencies between inputs and outputs
  edges <- as_edgelist(flow$graph())
  inputs_needed <- edges[, 1]
  outputs <- edges[, 2]

  only_outputs <- setdiff(flow$get_outputs(), flow$get_inputs())

  relationships <- c()

  for (out in only_outputs) {

    relationships <- c(relationships,
                       paste0("  ", out, " needs ",
                              str_flatten(inputs_needed[which(outputs == out)],
                                          collapse = ", "),
                              " as inputs."))

  }

  if (length(relationships) > 0) {

    cat(underline("Summary of processes:\n"))

    cat(relationships, sep = "\n")

  }

  invisible(TRUE)

}
