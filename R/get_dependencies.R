#' @title Get Dependencies of a Function
#'
#' @description This function allows to obtain the dependencies of a given function.
#'
#' @param script     (a function) A function to determine its dependencies
#'
#' @return A vector with the package dependencies of the given function.
#'
.get_dependencies <- function(script) {

  # Available packages
  r_pkgs <- unique(c(row.names(utils::installed.packages()), loadedNamespaces()))
  rInst <- paste0(r_pkgs, "::")

  # Let us take the body of the function
  if (inherits(script, "function")) {

    x <- utils::capture.output(print(script))

  } else {

    x <- readLines(script, warn = FALSE)

  }

  # Minimal cleaning of the output
  x <- gsub("^\\s+", "", x)
  x <- x[!grepl("^#", x)]

  # Look for required namespaces (including the namespace where the function
  # belongs to)
  x2 <- gsub(x = x[grepl("<environment: namespace:", x = x)],
             pattern = "<environment: namespace:", replacement = "")
  t0 <- sapply(r_pkgs, grep, x = x2, value = TRUE)
  t1 <- t0[which(sapply(t0, function(y) length(y) > 0))] %>% names()

  x <- x[!grepl("^<", x)]
  s0 <- sapply(paste0("\\b", rInst), grep, x = x, value = TRUE)
  s1 <- s0[which(sapply(s0, function(y) length(y) > 0))]

  names(s1) <- gsub("\\\\b", "", names(s1))

  ret <- c()
  if (length(names(s1)) > 0)
    ret <- sapply(names(s1), function(nm) {

      out <- unlist(lapply(s1[[nm]], function(xx) {

        y <- gsub("[\\\",\\(\\)]", "",
                  unlist(regmatches(xx,
                                    gregexpr(paste0(nm, "(.*?)[\\)\\(,]"), xx))))

        names(y) <- NULL

        if (any(y %in% paste0(nm, c("\"", "'"))))
          y <- NULL

        return(y)

      }))

      out <- gsub("\\$.*", "", out)
      out <- unique(out)

      return(out)

    })

  if (length(ret) > 0) {

    ret <- sapply(ret, function(L) strsplit(x = L, split = "::")[[1]][1])

  }

  # Look for "requires"
  lines <- grep("(require|library)", x)
  pkgs <- c()
  if (length(lines) > 0) {

    L <- unlist(strsplit(x[lines], split = "\\(|\\)"))
    pkgs <- L[L %in% row.names(utils::installed.packages())]

  }

  # Concatenate all dependencies
  pkg <- c(ret, pkgs, t1)

  return(as.vector(pkg))

}
