##%######################################################%##
#                                                          #
####                 Dataset Management                 ####
#                                                          #
##%######################################################%##

data_globals <- new.env()

with(data_globals, {

  dataset_cache_dir <- file.path(normalizePath("~"), "dl4ni.data")
  atlas_cache_dir <- file.path(normalizePath("~"), "ni.atlases")

})

get_dataset_dir <- function() data_globals$dataset_cache_dir

set_dataset_dir <- function(dir) data_globals$dataset_cache_dir <- suppressWarnings(normalizePath(dir))

#' Download a Dataset
#'
#' @param dataset         (character) Name of the dataset
#' @param force_download  (logical) Force the download of the dataset?. Default: FALSE.
#' @param verbose         (logical) Print informative messages?. Default: FALSE.
#'
#' @return The path to the (downloaded, if necessary) dataset
#'
#' @export
#'
#' @examples
#' get_dataset("brain_extraction")
#'
get_dataset <- function(dataset,
                        force_download = FALSE,
                        verbose = FALSE) {

  # Where the dataset is stored
  repo_name <- paste0("neuroimaginador/dl4ni.", dataset)
  destination_folder <- file.path(get_dataset_dir(), dataset)

  # What happens when the output directory exists?
  if (dir.exists(destination_folder)) {

    if (force_download) {

      unlink(destination_folder, recursive = TRUE)
      message("Deleting previous folder.")

    } else {

      return(destination_folder)

    }

  }

  dataset_path <- download_repo(repo_name = repo_name,
                                destination_folder = destination_folder,
                                verbose = verbose)

  return(invisible(dataset_path))

}


##%######################################################%##
#                                                          #
####                  Atlas Management                  ####
#                                                          #
##%######################################################%##


get_atlases_dir <- function() data_globals$atlas_cache_dir

set_atlases_dir <- function(dir) data_globals$atlas_cache_dir <- suppressWarnings(normalizePath(dir))

#' Download a Set of Atlases
#'
#' @param atlas_name      (character) Name of the atlas to load
#' @param force_download  (logical) Force the download of the atlases?. Default: FALSE.
#' @param verbose         (logical) Print informative messages?. Default: FALSE.
#'
#' @return The path to the (downloaded, if necessary) atlas
#'
#' @export
#'
#' @examples
#' get_atlas("MNI152")
#'
get_atlas <- function(atlas_name,
                      force_download = FALSE,
                      verbose = FALSE) {

  # Where the dataset is stored
  repo_name <- "neuroimaginador/ni.atlases"
  destination_folder <- get_atlases_dir()

  # What happens when the output directory exists?
  if (dir.exists(destination_folder)) {

    if (force_download) {

      unlink(destination_folder, recursive = TRUE)
      message("Deleting previous folder.")

    } else {

      possible_atlases <- list.files(path = destination_folder,
                                     pattern = atlas_name, full.names = TRUE)

      return(possible_atlases)

    }

  }

  atlas_path <- download_repo(repo_name = repo_name,
                              destination_folder = destination_folder,
                              verbose = verbose)

  possible_atlases <- list.files(path = atlas_path,
                                 pattern = atlas_name, full.names = TRUE)

  return(invisible(possible_atlases))


}
