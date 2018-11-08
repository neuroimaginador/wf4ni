#' Search for Variables in Namespaces
#'
#' @param names   (character) Variable names as strings
#' @param envir   (environment) Environment where to look for variable names
#'
#' @return If a variable exist in the environment with the corresponding name, its value is returned, otherwise the name is considered as a symbol as is returned as is.
#'
.search_names <- function(names, envir = parent.frame()) {

  names <- sapply(names, function(s) {

    if (s %in% ls(envir = envir)) {

      st <- get(s, envir = envir)

      if (is.character(st)) {

        s <- st

      }

    }

    return(s)

  })

  return(names)

}
