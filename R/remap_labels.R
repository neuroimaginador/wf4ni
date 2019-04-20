remap_labels <- function(img, values = NULL) {

  if (is.null(values)) {

    values <- img %>% as.vector() %>% unique() %>% sort()
    values <- values[values > 0]

  }

  res <- 0 * img

  for (k in seq_along(values)) {

    res[img == values[k]] <- k

  }

  return(res)

}