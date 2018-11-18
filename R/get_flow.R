flow_globals <- new.env()

with(flow_globals, {

  flow_cache_dir <- file.path(normalizePath("~"), "wf4ni_flows")

})

#' @title Get Location of Installed Flows
#'
#' @description This function returns the path where flows are being installed.
#'
#' @return The path where flows are installed.
#'
#' @export
#'
get_flows_dir <- function()
  flow_globals$flow_cache_dir

#' @title Set Location to Install Flows to
#'
#' @description This function allows to change the default location where to install flows.
#'
#' @param dir    (character) The new path.
#'
#' @return The new path where to install flows to.
#'
#' @export
#'
set_flows_dir <- function(dir)
  flow_globals$flow_cache_dir <- suppressWarnings(normalizePath(dir))

#' @param repo_name        (character) The name of the repository that hosts the flow.
#' @param verbose          (logical) print messages?, Default: FALSE
#'
#' @title Download and Import a Flow
#'
#' @description This function is used to download a flow from a repository and load it from the local storage. Only GitHub and Bitbucket are currently supported.
#'
#' @return The imported flow.
#'
#' @import crayon
#' @importFrom stringr str_flatten
#' @importFrom utils installed.packages
#'
#' @export
#'
get_flow <- function(repo_name, verbose = FALSE) {

  # Where the dataset is stored
  destination_folder <- get_flows_dir()

  # Download flow
  flow_file <- download_repo(repo_name = repo_name,
                             destination_folder = destination_folder)

  # Load flow
  flow <- .load_flow(filename = flow_file)

  # Final messages
  if (verbose) {

    has_crayon <- requireNamespace("crayon", quietly = TRUE)

    if (has_crayon) {

      flow_name <- bold(flow$name())
      destination_folder <- bold(destination_folder)
      deps <- str_flatten(bold(flow$get_dependencies()), collapse = ", ")

    } else {

      flow_name <- paste0("'", flow$name(), "'")
      deps <- str_flatten(flow$get_dependencies(), collapse = ", ")

    }

    cat("Flow", flow_name,
        "downloaded to", destination_folder, "\n")

    cat("This flow depends on the following packages: \n",
        deps,
        "\n\n")

    # Check dependencies of loaded flow.
    if (!flow$check_dependencies()) {

      cat("Currently, not all required packages are installed. ",
          "Please install\n them before executing this flow:\n") #nocov

      pkgs <- flow$get_dependencies() #nocov
      missing <- pkgs[!(pkgs %in% installed.packages())] #nocov

      missing_deps <- str_flatten(missing, collapse = ", ") #nocov
      if (has_crayon) missing_deps <- str_flatten(red(missing), collapse = ", ") #nocov

      cat(missing_deps, "\n\n") #nocov

    }

  }

  return(flow)

}