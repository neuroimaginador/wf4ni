#' @title Create a Flow (internal)
#'
#' @description This internal function provides an environment usable from the class DLflow.
#'
#' @param inputs    (list of names) List of inputs used in the flow, Default: list()
#'
#' @return A DLflow object.
#'
#' @seealso
#'  \code{\link[igraph]{make_empty_graph}},\code{\link[igraph]{add_vertices}}
#' @importFrom igraph make_empty_graph add_vertices
#' @import igraph
#'
.create_flow <- function(name = "", inputs = list()) {

  require(igraph)

  # A flow is an environment
  flow <- new.env()
  flow$name <- as.character(name)

  # List of flow inputs and outputs
  flow$inputs <- list()
  flow$outputs <- list()

  # List of flow processes (both models and functions)
  flow$processes <- list()
  flow$schemes <- list()
  flow$trained <- list()
  flow$pkgs <- list()

  # List of pipelines to execute for each process and of required inputs
  flow$pipeline <- list()
  flow$required_inputs <- list()
  flow$inmediate_inputs <- list()

  # Create graph of dependencies
  flow$graph <- igraph::make_empty_graph(directed = TRUE)

  # Add inputs to the graph
  if (length(inputs) > 0) {

    flow$inputs <- inputs
    flow$graph <- flow$graph %>% igraph::add_vertices(nv = length(inputs),
                                                      name = unlist(inputs),
                                                      type = rep("Input", length(inputs)))

  }

  # List of all possible outputs of the flow
  flow$outputs <- unlist(inputs)

  # Add ability to log:
  flow$log_lines <- c()

  with(flow, expr = {

    log <- function(level = c("DEBUG", "INFO", "WARNING", "ERROR"),
                    message = "...") {

      line_to_add <- paste0("(", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ") [",
                            level[1], "] ",
                            message)

      log_lines <<- c(log_lines, line_to_add)

    }

  })

  class(flow) <- "DLflow"
  return(flow)

}

##%######################################################%##
#                                                          #
####                      PLOTTING                      ####
#                                                          #
##%######################################################%##


#' @title Plot a Flow
#'
#' @description This function plots a flow using \link{igraph} plotting capabilities.
#'
#' @param flow           (a DLflow object) The flow.
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
.plot_flow <- function(flow) {

  stopifnot(inherits(flow, "DLflow"))

  # Define color of each node as its type
  num_types <- length(unique(V(flow$graph)$type))
  colors <- scales::alpha(colour = scales::hue_pal()(num_types), alpha = 0.85)
  V(flow$graph)$color <- colors[as.numeric(as.factor(V(flow$graph)$type))]

  invisible(igraph::tkplot(flow$graph, curved = TRUE))

}


