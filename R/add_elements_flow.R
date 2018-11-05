#' @title Add an Input to a Flow
#'
#' @description This function adds a node of type input to a flow.
#'
#' @param flow      (a flow) The flow where to add the input
#' @param inputs    (list or vector) List of the inputs to add, can be given with or without quotes, e.g. list(A, B) or list("A", "B"). Default: list()
#'
#' @return Returns (invisibly) the flow with added inputs.
#'
#' @seealso
#'  \code{\link[igraph]{add_vertices}}
#' @importFrom igraph add_vertices
#'
.add_inputs <- function(flow, inputs = list()) {

  # Basic checks
  stopifnot(inherits(flow, "DLflow"))

  suppressPackageStartupMessages(require(stringr))

  # Add inputs to the graph
  if (length(inputs) > 0) {

    flow$log(level = "DEBUG",
             message = paste0("Adding inputs ",
                              stringr::str_flatten(unlist(inputs),
                                                   collapse = ", ")))

    flow$inputs <- c(flow$inputs, inputs)
    flow$graph <- flow$graph %>% igraph::add_vertices(nv = length(inputs),
                                                      name = unlist(inputs),
                                                      type = rep("Input", length(inputs)))

  }

  # List of all possible outputs of the flow
  flow$outputs <- c(flow$outputs, unlist(inputs))

  return(invisible(flow))

}

#' @title Add a Function to a Flow
#'
#' @description This function adds another function to a given flow.
#'
#' @param flow       (a flow) The flow where to add a new function
#' @param proc       (a function) The function itself to be added
#' @param inputs     (list) List of the inputs needed for the function to be executed, defaults to the formal arguments of the function,
#'    list())
#' @param output     (name) The name to assign to the output of the function.
#' @param trained    (logical) is the model already trained?, Default: TRUE
#' @param ...        extra parameters for the function, allows to parameterize the flow.
#'
#' @return The flow with the function added
#'
#' @seealso
#'  \code{\link[igraph]{add_vertices}},\code{\link[igraph]{add_edges}}
#' @importFrom igraph add_vertices add_edges
#'
.add_process <- function(flow,
                         proc,
                         inputs = ifelse(inherits(proc, "function"), list(names(formals(proc))), list()),
                         output,
                         trained = TRUE,
                         ...) {

  suppressPackageStartupMessages(require(igraph))

  # Basic checks
  stopifnot(inherits(flow, "DLflow"))
  stopifnot(inherits(proc, "function"))
  output <- as.character(output)

  if (length(inputs) > 0) inputs <- unlist(inputs)

  type <- "DLmodel"
  if (inherits(proc, "function")) type <- "function"

  flow$log(level = "DEBUG",
           message = paste0("Adding process with inputs: ",
                            stringr::str_flatten(unlist(inputs), collapse = ", "),
                            " and output(s): ",
                            stringr::str_flatten(unlist(output), collapse = ", ")))

  # Add a node to the graph, with edges from its inputs to it.
  flow$graph <- flow$graph %>% igraph::add_vertices(nv = 1, name = output, type = type)
  new_vertex_idx <- length(V(flow$graph))

  if (length(inputs) > 0) {

    input_ids <- match(inputs, flow$outputs)
    flow$inmediate_inputs[[output]] <- flow$outputs[input_ids]

    flow$graph <- flow$graph %>% igraph::add_edges(edges = as.vector(rbind(input_ids, new_vertex_idx)))

  }

  # Add the model to the list of flow models
  additional_params <- list(...)

  if (length(additional_params) > 0) {

    require(pryr)

    proc <- proc %>% pryr::partial(...)

  }

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

  # Add package dependencies
  flow$pkgs[[output]] <- .get_dependencies(proc)

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
