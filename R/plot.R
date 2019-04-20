
##%######################################################%##
#                                                          #
####                      PLOTTING                      ####
#                                                          #
##%######################################################%##

# Auxiliary function for plotting
.plot_flow <- function(flow,
                       as_interactive = interactive(),
                       to_file = "") {

  stopifnot(inherits(flow, "NIflow"))

  # Define color of each node as its type
  num_types <- length(unique(V(flow$graph)$type))
  colors <- alpha(colour = hue_pal()(num_types), alpha = 0.85)
  V(flow$graph)$color <- colors[as.numeric(as.factor(V(flow$graph)$type))]

  if (nzchar(to_file)) {

    png(filename = to_file)
    plot.igraph(flow$graph)
    invisible(dev.off())

  } else {

    if (as_interactive) {

      invisible(tkplot(flow$graph, curved = TRUE))

    } else {

      if (suppressPackageStartupMessages(require(hasseDiagram))) {

        M <- flow$graph %>% as_adj() %>% as.matrix()
        M1 <- matrix(FALSE,
                     nrow = nrow(M),
                     ncol = ncol(M))

        M1[M > 0] <- TRUE

        hasse(data = M1, labels = flow$outputs)
      }

    }

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
plot <- function(flow, as_interactive = interactive(),
                 to_file = "") {

  my_flow <- flow$get_private()
  my_flow %>% .plot_flow(as_interactive,
                         to_file)

}