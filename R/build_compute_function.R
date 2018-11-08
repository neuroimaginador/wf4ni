#' @title Build a Function to Compute Output from Flow
#'
#' @description This internal function creates the body of a function
#' which is able to execute the current flow and obtain the given output.
#'
#' @param args      (character) List of arguments to be used in the function definition.
#' @param output    (character) One of the flow's outputs, which the function will compute.
#'
#' @return A text that is a function definition. The function will be able to compute the given \code{output} from a flow.
#'
#' @seealso
#'  \code{\link[stringr]{str_flatten}}
#'  \code{\link[styler]{style_text}}
#'
#' @importFrom stringr str_flatten
#' @importFrom styler style_text
#'
.build_compute_function <- function(args,
                                    output) {

  args_txt <- c()

  for (i in seq_along(args)) {

    args_txt <- c(args_txt, paste0(args[i], " = ", args[i]))

  }

  my_function_txt <- paste0("get_", output, " <- ",
                            "function(",
                            str_flatten(args, collapse = ", "), ") {\n\n",
                            "result <- flow$execute(inputs = list(",
                            str_flatten(args_txt, collapse = ", "),
                            "),\n",
                            "desired_outputs = c('", output, "'))\n",
                            "\n", "return(result$", output, ")\n\n",
                            "}")

  return(style_text(my_function_txt))

}
