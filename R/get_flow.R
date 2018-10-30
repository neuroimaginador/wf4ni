flow_globals <- new.env()

with(flow_globals, {

  flow_cache_dir <- file.path(normalizePath("~"), "wf4ni_flows")

})

#' @export
get_flows_dir <- function()
  flow_globals$flow_cache_dir

#' @export
set_flows_dir <- function(dir) flow_globals$flow_cache_dir <- suppressWarnings(normalizePath(dir))

#' @export
get_flow <- function(repo_name,
                     hosts = c("github", "bitbucket"),
                     force_download = FALSE) {

  # Where the dataset is stored
  destination_folder <- get_flows_dir()

  # What happens when the output directory exists?
  if (!dir.exists(destination_folder)) {

    dir.create(destination_folder, showWarnings = FALSE, recursive = TRUE)

  }

  # Let's check possible hosts
  for (host in hosts) {

    if (host == "github") {

      remote <- devtools:::github_remote(repo_name)

      path <- try(devtools:::remote_download(remote), silent = TRUE)

      if (!inherits(path, "try-error")) {

        break

      }

    }

    if (host == "bitbucket") {

      remote <- devtools:::bitbucket_remote(repo_name)

      path <- try(devtools:::remote_download(remote), silent = TRUE)

      if (!inherits(path, "try-error")) {

        break

      }

    }

  }

  if (!inherits(path, "try-error")) {

    # Ok, unzip the dataset and create destination folder if it does not exist.
    L <- unzip(zipfile = path, exdir = get_flows_dir(), list = TRUE)
    unzip(zipfile = path, exdir = get_flows_dir())
    extracted_folder <- L$Name[1]

    # Get ZIP file
    zip_file <- L$Name[2]

    file.rename(from = file.path(get_flows_dir(), zip_file),
                to = file.path(destination_folder, basename(zip_file)))

    file.remove(file.path(get_flows_dir(), extracted_folder))

    flow <- load_flow(filename = file.path(destination_folder,
                                                    basename(zip_file)))

    cat("Flow ", flow$name(),
        " downloaded to ", destination_folder, "\n")

    cat("This flow depends on the following packages: \n",
        stringr::str_flatten(flow$get_dependencies(), collapse = ", "),
        "\n\n")

    if (!flow$check_dependencies()) {

      cat("Currently, not all required packages are installed. Please install\n",
          "them before executing this flow.\n\n")

    }

    return(flow)

  } else {

    stop("Flow not found in repositories.")

  }

}