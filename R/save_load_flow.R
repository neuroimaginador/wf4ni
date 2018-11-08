#' @title Save a Flow
#'
#' @description This function saves a flow to a file.
#'
#' @param flow           (a NIflow object) The flow to save.
#' @param path           (character) Path where to save the file, Default: tempdir()
#' @param file_prefix    (character) File name, Default: the name of the \code{flow}
#'
#' @return Invisibly, the name of the output file
#'
#' @seealso
#'  \code{\link[zip]{zip}}
#' @importFrom zip zip
#' @import zip
#'
.save_flow <- function(flow, path = tempdir(), file_prefix = flow$name) {

  # Basic input check
  stopifnot(inherits(flow, "NIflow"))
  flow %>% .reset_outputs()

  flow$log(level = "DEBUG",
           message = paste0("Saving flow ", flow$name, " in ", path))

  # Output directory
  output_dir <- file.path(path, file_prefix)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Models/functions must be saved apart.
  processes <- flow$processes

  # In the output folder, save the flow and create a specific
  # folder for all the processes
  saveRDS(flow, file = file.path(output_dir, paste0(file_prefix, "_flow.rds")))
  processes_dir <- file.path(output_dir, "processes")
  dir.create(processes_dir, recursive = TRUE, showWarnings = FALSE)

  # Save all processes, according to their types
  for (proc_idx in seq_along(processes)) {

    proc <- processes[[proc_idx]]

    flow$export_process(process = proc,
                        path = processes_dir,
                        prefix = names(processes)[proc_idx])

  }

  # Create a compressed ZIP file
  output_file <- file.path(path, paste0(file_prefix, ".zip"))
  current_dir <- getwd()

  if (requireNamespace("zip", quietly = TRUE)) {

    output_file <- suppressWarnings(normalizePath(output_file))

    setwd(dirname(output_dir))

    file_list <- file.path(basename(output_dir),
                           list.files(output_dir,
                                      recursive = TRUE,
                                      all.files = TRUE,
                                      include.dirs = TRUE))

    suppressWarnings(
      zip(zipfile = output_file,
          files = file_list,
          recurse = FALSE)
    )

  }

  setwd(current_dir)
  unlink(output_dir, recursive = TRUE, force = TRUE)

  return(invisible(output_file))

}

#' @title Load a Flow
#'
#' @description This function imports a flow from disk
#'
#' @param filename    (character) The path to the file to import.
#' @param verbose     (logical) print information during process?, Default: FALSE
#'
#' @return A NIflow imported from the file.
#'
#' @importFrom utils unzip installed.packages
#'
.load_flow <- function(filename, verbose = FALSE) {

  stopifnot(file.exists(filename))

  # Unzip the file, if it exists
  flow_folder <- gsub(basename(filename), pattern = ".zip", replacement = "")

  output_dir <- file.path(dirname(filename), paste0("unzipped_", flow_folder))

  current_dir <- getwd()
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  setwd(output_dir)

  unzip(zipfile = filename, exdir = output_dir)

  output_dir <- file.path(output_dir, flow_folder)

  # Load flow
  if (verbose)
    cat("Loading main flow..\n") # nocov

  flow_file <- list.files(output_dir, pattern = "_flow.rds")
  flow <- readRDS(file.path(output_dir, flow_file))

  # For each process, incorporate it to the flow
  processes_dir <- file.path(output_dir, "processes")

  # Processes
  processes_files <- list.files(path = processes_dir, include.dirs = TRUE)

  for (file_f in processes_files) {

    import_res <- flow$import_process(filename = file.path(processes_dir, file_f))

    flow$processes[[import_res[[1]]]] <- import_res[[2]]

  }

  # Delete the output_dir
  setwd(current_dir)
  unlink(output_dir, recursive = TRUE, force = TRUE)

  final_flow <- NIflow$new(name = flow$name, inputs = flow$inputs)
  class(flow) <- "NIflow"
  final_flow$.__enclos_env__$private <- flow
  class(final_flow$.__enclos_env__$private) <- c("NIflow",
                                                 class(final_flow$.__enclos_env__$private))

  # Check dependencies of loaded flow.
  if (!final_flow$check_dependencies()) {

    has_crayon <- requireNamespace("crayon", quietly = TRUE)

    cat("Currently, not all required packages for this flow are installed. Please install\n them before executing this flow:\n") #nocov

    pkgs <- final_flow$get_dependencies() #nocov
    missing <- pkgs[!(pkgs %in% installed.packages())] #nocov

    missing_deps <- str_flatten(missing, collapse = ", ") #nocov
    if (has_crayon) missing_deps <- str_flatten(red(missing), collapse = ", ") #nocov

    cat(missing_deps, "\n\n") #nocov

  }

  # Return the flow
  return(final_flow)

}

#' @title Clone a Flow
#'
#' @description This functions allows to (deep) clone a flow
#'
#' @param flow    (a NIflow object) The flow to clone
#'
#' @return Another NIflow object which is an exact copy of the given \code{flow}.
#'
.clone_flow <- function(flow) {

  new_flow <- flow %>% .save_flow() %>% .load_flow()

  return(new_flow)

}

