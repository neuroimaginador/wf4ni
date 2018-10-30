.get_dependencies <- function(script) {

  r_pkgs <- unique(c(row.names(utils::installed.packages()), loadedNamespaces()))
  rInst <- paste0(r_pkgs, "::")

  if (inherits(script, "function")) {

    x <- utils::capture.output(print(script))

  } else {

    x <- readLines(script, warn = FALSE)

  }

  x <- gsub("^\\s+", "", x)
  x <- x[!grepl("^#", x)]

  # namespace
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

  # requires
  lines <- grep("(require|library)", x)
  pkgs <- c()
  if (length(lines) > 0) {

    L <- unlist(strsplit(x[lines], split = "\\(|\\)"))
    pkgs <- L[L %in% row.names(utils::installed.packages())]

  }

  pkg <- c(ret, pkgs, t1)

  return(as.vector(pkg))

}
