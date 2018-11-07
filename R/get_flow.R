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
#' @export
#'
get_flow <- function(repo_name, verbose = FALSE) {

  # Where the dataset is stored
  destination_folder <- get_flows_dir()

  # What happens when the output directory exists?
  if (!dir.exists(destination_folder)) {

    dir.create(destination_folder, showWarnings = FALSE, recursive = TRUE)

  }

  # Extract username and repo from given repo_name
  remote <- stringr::str_split(repo_name, pattern = "/")[[1]] %>% as.list()
  names(remote) <- c("username", "repo")

  # Check both remotes for the repo
  url_bit <- paste("api.bitbucket.org/2.0", "repositories", remote$username, remote$repo, sep = "/")
  url_git <- paste("api.github.com/repos", remote$username, remote$repo,
                   "tarball/master", sep = "/")

  exist_bit <- RCurl::url.exists(url_bit)
  exist_git <- RCurl::url.exists(url_git)

  if (!exist_bit & !exist_git) {

    stop("Flow not found in repositories.")

  }

  # At least in one of the remotes the repo exists.
  # Get the download URL.
  # If it is from GitHub
  destfile <- tempfile(fileext = ".zip")
  final_url <- url_git
  uncompress <- unzip

  # Or if there is an alternate repo in BitBucket
  if (exist_bit) {

    destfile <- tempfile(fileext = ".tar.gz")
    final_url <- paste("https://bitbucket.org", remote$username, remote$repo, "get/master.tar.gz",
                       sep = "/")
    uncompress <- untar

  }

  # Download the file
  download.file(url = final_url, destfile = destfile, quiet = !verbose)

  # Ok, unzip the dataset and create destination folder if it does not exist.
  L <- uncompress(destfile, exdir = get_flows_dir(), list = TRUE)
  uncompress(destfile, exdir = get_flows_dir())

  # Get folder and zipfile names
  if (is.list(L)) {

    extracted_folder <- L$Name[1]
    zip_file <- L$Name[2]


  } else {

    extracted_folder <- L[1]
    zip_file <- L[2]

  }

  # Move files to final destination
  file.rename(from = file.path(get_flows_dir(), zip_file),
              to = file.path(destination_folder, basename(zip_file)))

  file.remove(file.path(get_flows_dir(), extracted_folder))

  # Load flow
  flow <- .load_flow(filename = file.path(destination_folder,
                                          basename(zip_file)))

  # Final messages
  if (verbose) {

    has_crayon <- suppressPackageStartupMessages(require(crayon))

    if (has_crayon) {

      flow_name <- crayon::bold(flow$name())
      destination_folder <- crayon::bold(destination_folder)
      deps <- stringr::str_flatten(crayon::bold(flow$get_dependencies()), collapse = ", ")

    } else {

      flow_name <- paste0("'", flow$name(), "'")
      deps <- stringr::str_flatten(flow$get_dependencies(), collapse = ", ")

    }

    cat("Flow", flow_name,
        "downloaded to", destination_folder, "\n")

    cat("This flow depends on the following packages: \n",
        deps,
        "\n\n")

    # Check dependencies of loaded flow.
    if (!flow$check_dependencies()) {

      cat("Currently, not all required packages are installed. Please install\n",
          "them before executing this flow:\n")

      pkgs <- flow$get_dependencies()
      missing <- pkgs[!(pkgs %in% installed.packages())]

      missing_deps <- stringr::str_flatten(missing, collapse = ", ")
      if (has_crayon) missing_deps <- stringr::str_flatten(crayon::red(missing), collapse = ", ")

      cat(missing_deps, "\n\n")

    }

  }

  return(flow)

}