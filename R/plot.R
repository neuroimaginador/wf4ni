##%######################################################%##
#                                                          #
####                      PLOTTING                      ####
#                                                          #
##%######################################################%##

# Auxiliary function for plotting
#' @importFrom grid unit stringWidth convertX convertY grob
#' @importFrom graph graphAM
#' @importFrom methods as
#' @importFrom igraph as_adj
#' @importFrom Rgraphviz agopen
#'
.plot_flow <- function(flow,
                       to_file = "") {

  nWi <- function(labels, margin) {

    result <- unit(0, "inch")
    for (label in labels) result <- result + stringWidth(label) +
        unit(margin$rl * 2, "inch")
    if (length(labels) > 1)
      result <- result + (length(labels) + 1) * unit(margin$orl,
                                                     "inch")
    return(convertX(result, "inches", TRUE))

  }

  nHi <- function(labels, margin) {

    result <- unit(1, "lines") + unit(margin$tb * 2, "inch")
    if (length(labels) > 1)
      result <- result + unit(margin$otb * 2, "inch")
    return(convertY(result, "inches", TRUE))

  }

  stopifnot(inherits(flow, "NIflow"))

  requireNamespace("grid", quietly = TRUE)
  requireNamespace("Rgraphviz", quietly = TRUE)

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

  nAttrs$fontsize <- rep(16, nrNodes)

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
#' @param to_file        (character) Path to a file to save the graph. If not present, plots the flow to the screen. Otherwise, it is stored in a PNG file with the given name.
#'
#' @return Nothing
#'
#' @import igraph
#' @importFrom scales alpha hue_pal
#' @importFrom grid unit stringWidth convertX convertY grob grid.newpage
#' @importFrom graph graphAM
#' @importFrom methods as
#' @importFrom igraph as_adj
#' @importFrom Rgraphviz agopen
#' @importFrom grDevices dev.off png
#'
plot <- function(flow,
                 to_file = "") {

  my_flow <- flow$get_private()
  my_flow %>% .plot_flow(to_file)

}