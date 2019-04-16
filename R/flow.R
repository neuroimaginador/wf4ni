#' @title Create a Flow (internal)
#'
#' @description This internal function provides an environment usable from the class NIflow.
#'
#' @param name      (character) Name of the flow
#' @param inputs    (list of names) List of inputs used in the flow, Default: list()
#'
#' @return A NIflow object.
#'
#' @seealso
#'  \code{\link[igraph]{make_empty_graph}},\code{\link[igraph]{add_vertices}}
#' @importFrom igraph make_empty_graph add_vertices
#' @import igraph
#'
.create_flow <- function(name = "", inputs = list()) {

  # A flow is an environment
  flow <- new.env()
  flow$name <- as.character(name)

  # List of flow inputs and outputs
  flow$inputs <- list()
  flow$outputs <- list()

  # List of flow processes (both models and functions)
  flow$processes <- list()
  # flow$schemes <- list()
  # flow$trained <- list()
  flow$pkgs <- list()

  # List of pipelines to execute for each process and of required inputs
  flow$pipeline <- list()
  flow$required_inputs <- list()
  flow$inmediate_inputs <- list()

  # Create graph of dependencies
  flow$graph <- make_empty_graph(directed = TRUE)

  # Add inputs to the graph
  if (length(inputs) > 0) {

    flow$inputs <- inputs
    flow$graph <- flow$graph %>% add_vertices(nv = length(inputs),
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

  # Add function to execute processes (this way it can be extended to non-function objects)
  with(flow, expr = {

    execute_process <- function(what, args) {

      do.call(what = what, args = args)

    }

  })

  # Add a function to clone processes included in the flow
  with(flow, expr = {


    clone_process <- function(process) {

      return(process)

    }

  })

  # Add a function to save and load a process
  with(flow, expr = {

    export_process <- function(process, path, prefix) {

      saveRDS(object = process,
              file = file.path(path,
                               paste0(prefix, ".rds")))

      return(invisible(TRUE))

    }

    import_process <- function(filename) {

      output_name <- gsub(x = basename(filename), pattern = ".rds", replacement = "")

      output_process <- readRDS(filename)

      return(list(output_name, output_process))

    }

  })

  class(flow) <- "NIflow"
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

      if (require(hasseDiagram)) {

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


