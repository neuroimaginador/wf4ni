
#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow      (name) PARAM_DESCRIPTION
#' @param inputs    (call) PARAM_DESCRIPTION, Default: list()
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso
#'  \code{\link[igraph]{add_vertices}}
#' @export
#' @importFrom igraph add_vertices
add_inputs <- function(flow, inputs = list()) {

  # Basic checks
  stopifnot(inherits(flow, "DLflow"))

  # Add inputs to the graph
  if (length(inputs) > 0) {

    flow$log(level = "DEBUG",
             message = paste0("Adding inputs ", unlist(inputs)))

    flow$inputs <- c(flow$inputs, inputs)
    flow$graph <- flow$graph %>% igraph::add_vertices(nv = length(inputs),
                                                      name = unlist(inputs),
                                                      type = rep("Input", length(inputs)))

  }

  # List of all possible outputs of the flow
  flow$outputs <- c(flow$outputs, unlist(inputs))

  return(invisible(flow))

}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow       (name) PARAM_DESCRIPTION
#' @param proc       (name) PARAM_DESCRIPTION
#' @param inputs     (call) PARAM_DESCRIPTION, Default: ifelse(inherits(proc, "function"), list(names(formals(proc))),
#'    list())
#' @param output     (name) PARAM_DESCRIPTION
#' @param trained    (logical) PARAM_DESCRIPTION, Default: TRUE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso
#'  \code{\link[igraph]{add_vertices}},\code{\link[igraph]{add_edges}}
#' @export
#' @importFrom igraph add_vertices add_edges
#' @import
add_process <- function(flow,
                        proc,
                        inputs = ifelse(inherits(proc, "function"), list(names(formals(proc))), list()),
                        output,
                        trained = TRUE) {

  require(igraph)

  # Basic checks
  stopifnot(inherits(flow, "DLflow"))
  stopifnot(inherits(proc, "function"))
  output <- as.character(output)

  if (length(inputs) > 0) inputs <- unlist(inputs)

  type <- "DLmodel"
  if (inherits(proc, "function")) type <- "function"


  flow$log(level = "DEBUG",
           message = paste0("Adding process with inputs: ", unlist(inputs), " and output(s): ",
                            unlist(output)))

  # Add a node to the graph, with edges from its inputs to it.
  flow$graph <- flow$graph %>% igraph::add_vertices(nv = 1, name = output, type = type)
  new_vertex_idx <- length(V(flow$graph))

  if (length(inputs) > 0) {

    input_ids <- match(inputs, flow$outputs)
    flow$inmediate_inputs[[output]] <- flow$outputs[input_ids]

    flow$graph <- flow$graph %>% igraph::add_edges(edges = as.vector(rbind(input_ids, new_vertex_idx)))

  }

  # Add the model to the list of flow models
  flow$processes[[output]] <- proc
  flow$outputs <- c(flow$outputs, output)
  flow$trained[[output]] <- trained
  # if (inherits(proc, "DLscheme")) {
  #
  #   flow$schemes[[output]] <- proc
  #
  # }
  #
  # if (inherits(proc, "DLmodel")) {
  #
  #   scheme <- DLscheme$new()
  #
  #   scheme$from_model(proc)
  #   flow$schemes[[output]] <- scheme
  #
  # }

  # Add its pipeline (updating all previous pipelines)
  inputs <- which(V(flow$graph)$type == "Input")
  for (target_idx in setdiff(seq(new_vertex_idx), inputs)) {

    # Path from the current node to inputs
    paths <- flow$graph %>% all_simple_paths(from = target_idx, to = inputs, mode = "in")
    paths <- lapply(paths, unclass)
    paths <- lapply(paths, as.vector)
    nodes_for_target <- unique(unlist(paths))

    # Topological order of the graph
    pipeline <- topo_sort(flow$graph)

    # Restricted to nodes connected to the current node
    pipeline <- pipeline[pipeline %in% nodes_for_target]

    # Update the list of current required inputs and the pipeline for the current node
    flow$required_inputs[[V(flow$graph)$name[target_idx]]] <- intersect(pipeline, inputs)
    flow$pipeline[[V(flow$graph)$name[target_idx]]] <- setdiff(pipeline, inputs)

  }

  return(invisible(flow))

}