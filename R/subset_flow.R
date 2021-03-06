#' @title Subset a Flow
#'
#' @description This function allows to extract sub-flows from a given flow.
#'
#' @param flow       (a NIflow object) The original flow.
#' @param outputs    (list) Names of the outputs to keep in the new flow.
#'
#' @return A new NIflow object, a copy of the original \code{flow}, but only the given \code{outputs} are kept, as well as their required inputs. That is, the result is a sub-flow whose final results are the given \code{outputs}.
#'
#' @seealso
#'  \code{\link[igraph]{topo_sort}},\code{\link[igraph]{make_empty_graph}},\code{\link[igraph]{add_vertices}},\code{\link[igraph]{add_edges}}
#' @importFrom igraph topo_sort make_empty_graph add_vertices add_edges
#' @import igraph
#'
.subset_flow <- function(flow, outputs) {

  # Basic input checks
  stopifnot(inherits(flow, "NIflow"))

  # For each required output, get the nodes required for its computation.
  needed_nodes <- c()
  for (out in outputs) {

    needed_nodes <- c(needed_nodes, flow$required_inputs[[out]], flow$pipeline[[out]])

  }

  # Ensure needed nodes are in correct topological order
  needed_nodes <- intersect(topo_sort(flow$graph), needed_nodes)
  needed_nodes <- flow$outputs[unique(needed_nodes)]

  # Create new flow
  new_flow <- new.env()
  new_flow$name <- paste0("subset_", flow$name)

  # List of flow inputs and outputs
  new_flow$inputs <- intersect(flow$inputs, needed_nodes)
  only_output <- setdiff(needed_nodes, new_flow$inputs)

  new_flow$outputs <- c(new_flow$inputs, only_output)

  # List of flow processes (both models and functions)
  new_flow$processes <- flow$processes[only_output]

  # List of pipelines to execute for each process and of required inputs
  new_flow$pipeline <- list()
  new_flow$required_inputs <- list()

  for (out in only_output) {

    original_inputs <- flow$outputs[flow$required_inputs[[out]]]
    new_inputs <- match(original_inputs, new_flow$inputs)
    new_flow$required_inputs[[out]] <- new_inputs

  }

  new_flow$inmediate_inputs <- flow$inmediate_inputs[new_flow$outputs]

  # Create graph of dependencies
  new_flow$graph <- make_empty_graph(directed = TRUE)

  # Add inputs to the graph
  if (length(new_flow$inputs) > 0) {

    new_flow$graph <- new_flow$graph %>% add_vertices(nv = length(new_flow$inputs),
                                                      name = unlist(new_flow$inputs),
                                                      type = rep("Input", length(new_flow$inputs)))

  }

  # Add nodes to the graph
  types <- V(flow$graph)$type[V(flow$graph)$name %in% only_output]
  new_flow$graph <- new_flow$graph %>% add_vertices(nv = length(only_output),
                                                    name = only_output,
                                                    type = types)

  for (out in only_output) {

    node_idx <- which(V(new_flow$graph)$name == out)

    inmediate_inputs <- match(new_flow$inmediate_inputs[[out]], new_flow$outputs)
    new_flow$graph <- new_flow$graph %>% add_edges(edges = as.vector(rbind(inmediate_inputs, node_idx)))

  }

  # Compute pipeline for each output
  inputs <- which(V(flow$graph)$type == "Input")
  for (target_name in only_output) {

    target_idx <- which(V(new_flow$graph)$name == target_name)

    # Path from the current node to inputs
    paths <- new_flow$graph %>% all_simple_paths(from = target_idx, to = inputs, mode = "in")
    paths <- lapply(paths, unclass)
    paths <- lapply(paths, as.vector)
    nodes_for_target <- unique(unlist(paths))

    # Topological order of the graph
    pipeline <- topo_sort(new_flow$graph)

    # Restricted to nodes connected to the current node
    pipeline <- pipeline[pipeline %in% nodes_for_target]

    # Update the list of current required inputs and the pipeline for the current node
    new_flow$pipeline[[target_name]] <- setdiff(pipeline, inputs)

  }

  # Add ability to log:
  new_flow$log_lines <- c()

  with(new_flow, expr = {

    log <- function(level = c("DEBUG", "INFO", "WARNING", "ERROR"),
                    message = "...") {

      line_to_add <- paste0("(", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ") [",
                            level[1], "] ",
                            message)

      log_lines <<- c(log_lines, line_to_add)

    }

  })

  # Inherit function to execute a process from its parent.
  new_flow$execute_process <- flow$execute_process

  # Inherit a function to clone a process from its parent
  new_flow$clone_process <- flow$clone_process

  # Inherit functions to export and import processes, from its parent
  new_flow$export_process <- flow$export_process
  new_flow$import_process <- flow$import_process

  # Create a deep copy of the flow processes (in case we want to extend to non-function objects)
  class(new_flow) <- "NIflow"

  for (proc_id in seq_along(new_flow$processes)) {

    proc <- new_flow$processes[[proc_id]]

    proc <- new_flow$clone_process(process = proc)

    new_flow$processes[[proc_id]] <- proc

  }

  return(invisible(new_flow))

}
