.build_compute_function <- function(args,
                                    output) {

  args_txt <- c()

  for (i in seq_along(args)) {

    args_txt <- c(args_txt, paste0(args[i], " = ", args[i]))

  }

  my_function_txt <- paste0("get_", output, " <- ",
                            "function(",
                            stringr::str_flatten(args,
                                                 collapse = ", "), ") {\n\n",
                            "result <- flow$execute(inputs = list(",
                            stringr::str_flatten(args_txt,
                                                 collapse = ", "),
                            "),\n",
                            "desired_outputs = c('", output, "'))\n",
                            "\n", "return(result$", output, ")\n\n",
                            "}")

  return(styler::style_text(my_function_txt))

}
