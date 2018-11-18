#' Download a Repository from GitHub or BitBucket
#'
#' @param repo_name          (character) The name of the repository (username/repo).
#' @param destination_folder (character) Path where to download the repository.
#' @param verbose            (logical) print messages?, Default: FALSE
#'
#' @return The path where the repository has been downloaded.
#'
#' @importFrom stringr str_split
#' @importFrom RCurl url.exists
#' @importFrom utils unzip untar download.file
#'
#' @export
#'
#' @examples
#' download_repo("neuroimaginador/wf4ni", destination_folder = "~/Documents/wf4ni", verbose = TRUE)
#'
download_repo <- function(repo_name,
                          destination_folder = normalizePath("~"),
                          verbose = FALSE) {

  # What happens when the output directory exists?
  if (!dir.exists(destination_folder)) {

    dir.create(destination_folder, showWarnings = FALSE, recursive = TRUE) #nocov

  }

  destination_folder <- normalizePath(destination_folder)

  # Extract username and repo from given repo_name
  remote <- str_split(repo_name, pattern = "/")[[1]] %>% as.list()
  names(remote) <- c("username", "repo")

  # Check both remotes for the repo
  url_bit <- paste("api.bitbucket.org/2.0", "repositories", remote$username, remote$repo, sep = "/")
  url_git <- paste("api.github.com/repos", remote$username, remote$repo,
                   "tarball/master", sep = "/")

  exist_bit <- url.exists(url_bit)
  exist_git <- url.exists(url_git)

  if (!exist_bit & !exist_git) {

    stop("Repository not found.")

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
    final_url <- paste("https://bitbucket.org",
                       remote$username, remote$repo,
                       "get/master.tar.gz",
                       sep = "/")
    uncompress <- untar

  }

  # Download the file
  download.file(url = final_url, destfile = destfile, quiet = !verbose)

  tmp_dir <- tempdir()

  # Ok, unzip the dataset and create destination folder if it does not exist.
  L <- uncompress(destfile, exdir = tmp_dir, list = TRUE)
  uncompress(destfile, exdir = tmp_dir)

  # Get folder and zipfile names
  if (is.list(L)) {

    extracted_folder <- L$Name[1]
    other_contents <- L$Name[-1]


  } else {

    extracted_folder <- L[1]
    other_contents <- L[-1]

  }

  # Move files to final destination
  if (length(other_contents) == 1) {

    # Move files to final destination
    file.rename(from = file.path(tmp_dir, other_contents),
                to = file.path(destination_folder, basename(other_contents)))

    file.remove(file.path(tmp_dir, extracted_folder))

    return_path <- file.path(destination_folder, basename(other_contents))

  } else {

    files_to_copy <- list.files(file.path(tmp_dir, extracted_folder), recursive = TRUE, full.names = TRUE)

    new_files <- gsub(x = files_to_copy,
                      pattern = file.path(tmp_dir, extracted_folder),
                      replacement = normalizePath(destination_folder))

    new_folders <- new_files %>%
      sapply(dirname) %>%
      unique()

    new_folders %>% sapply(function(f) dir.create(f, showWarnings = FALSE, recursive = TRUE))

    file.rename(from = files_to_copy, to = new_files)

    return_path <- destination_folder

  }

  return(return_path)

}