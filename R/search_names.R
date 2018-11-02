#' Search for Variables in Namespaces
#'
#' @param names   (character) Variable names as strings
#' @param n       (integer) Number of generations of environments to go back. Default is 4.
#'
#' @return If a variable exist in the environment with the corresponding name, its value is returned, otherwise the name is considered as a symbol as is returned as is.
#'
.search_names <- function(names, envir = parent.frame()) {

  names <- sapply(names, function(s) {

    if (s %in% ls(envir = envir)) {

      s <- get(s, envir = envir)

    }

    return(s)

  })

  return(names)

}
