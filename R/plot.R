
##%######################################################%##
#                                                          #
####                      PLOTTING                      ####
#                                                          #
##%######################################################%##

# Auxiliary function for plotting
.plot_flow <- function(flow,
                       to_file = "") {

  nWi <- function(labels, margin) {

    library(grid)

    result <- unit(0, "inch")
    for (label in labels) result <- result + stringWidth(label) +
        unit(margin$rl * 2, "inch")
    if (length(labels) > 1)
      result <- result + (length(labels) + 1) * unit(margin$orl,
                                                     "inch")
    return(convertX(result, "inches", TRUE))

  }

  nHi <- function(labels, margin) {

    library(grid)

    result <- unit(1, "lines") + unit(margin$tb * 2, "inch")
    if (length(labels) > 1)
      result <- result + unit(margin$otb * 2, "inch")
    return(convertY(result, "inches", TRUE))

  }

  stopifnot(inherits(flow, "NIflow"))

  suppressPackageStartupMessages(library(grid))
  suppressPackageStartupMessages(library(Rgraphviz))

  # Define color of each node as its type
  # num_types <- length(unique(V(flow$graph)$type))
  colors <- c("white", "lightblue")
  #alpha(colour = hue_pal()(num_types), alpha = 0.85)
  my_colors <- colors[as.numeric(as.factor(V(flow$graph)$type))]

  # Build a graph to be plotted usong Rgraphviz
  g <- as(graphAM(flow$graph %>% as_adj() %>% as.matrix(),
                  "directed"),
          "graphNEL")

  # Customize labels and node color
  labels <- flow$outputs

  parameters <- list()
  parameters$margin <- list()
  parameters$margin$rl <- parameters$margin$tb <- 0.125
  parameters$margin$orl <- parameters$margin$otb <- 0.08
  if (!is.character(parameters$shape))
    parameters$shape <- "roundrect"

  nAttrs <- list()
  nAttrs$width <- sapply(labels, function(x) {
    nWi(x, parameters$margin)
  })
  nAttrs$height <- sapply(labels, function(x) {
    nHi(x, parameters$margin)
  })

  nAttrs$fillcolor <- rep(my_colors, length(flow$outputs))
  nAttrs$color <- rep("blue", length(flow$outputs))

  nrNodes <- length(flow$outputs)

  nAttrs$fixedsize <- rep(TRUE, nrNodes)
  nAttrs <- lapply(nAttrs, function(x) {
    names(x) <- flow$outputs
    x
  })

  # The actual graph to be plotted
  ragraph <- agopen(g,
                    name = "graph",
                    attrs = list(node = list(shape = "box"),
                                 graph = list(rank = "same",
                                              rankdir = "TB")),
                    nodeAttrs = nAttrs)

  # Use grid to plot:
  my_grob <- grob(ragraph,
                  labels = flow$outputs,
                  parameters = parameters)


  if (nzchar(to_file)) {

    png(filename = to_file)

    grid.newpage()
    Rgraphviz::plot(my_grob[[1]])

    invisible(dev.off())

  } else {

    grid.newpage()
    Rgraphviz::plot(my_grob[[1]])

  }

}


#' @title Plot a Flow
#'
#' @description This function plots a flow using \link{igraph} plotting capabilities.
#'
#' @param flow           (a NIflow object) The flow.
#'
#' @return Nothing
#'
#' @seealso
#'  \code{\link[threejs]{graphjs}}

#'  \code{\link[igraph]{layout_with_sugiyama}}

#'  \code{\link[scales]{alpha}},\code{\link[scales]{hue_pal}}
#' @import igraph
#' @importFrom scales alpha hue_pal
#'
plot <- function(flow,
                 to_file = "") {

  my_flow <- flow$get_private()
  my_flow %>% .plot_flow(to_file)

}