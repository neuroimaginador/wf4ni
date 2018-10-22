#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow           (name) PARAM_DESCRIPTION
#' @param path           (call) PARAM_DESCRIPTION, Default: tempdir()
#' @param file_prefix    (call) PARAM_DESCRIPTION, Default: flow$name
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso
#'  \code{\link[zip]{zip}}
#' @export
#' @importFrom zip zip
#' @import zip
save_flow <- function(flow, path = tempdir(), file_prefix = flow$name) {

  # Basic input check
  stopifnot(inherits(flow, "DLflow"))
  flow %>% reset_outputs()

  flow$log(level = "DEBUG",
           message = paste0("Saving flow ", flow$name, " in ", path))

  # Output directory
  output_dir <- file.path(path, file_prefix)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Models must be saved apart.
  processes <- flow$processes

  # In the output folder, save the flow and create a specific folder for all the processes
  saveRDS(flow, file = file.path(output_dir, paste0(file_prefix, "_flow.rds")))
  processes_dir <- file.path(output_dir, "processes")
  dir.create(processes_dir, recursive = TRUE, showWarnings = FALSE)

  # Save all processes, according to their types
  for (proc_idx in seq_along(processes)) {

    proc <- processes[[proc_idx]]
    if (inherits(proc, "DLmodel")) {

      # A DLmodel should be saved with its own function
      proc$save(path = processes_dir, prefix = names(processes)[proc_idx])

    } else {

      # The remaining objects are saved in RDS format
      saveRDS(object = proc, file = file.path(processes_dir, paste0(names(processes)[proc_idx], ".rds")))

    }

  }

  # Create a compressed ZIP file
  output_file <- file.path(path, paste0(file_prefix, ".zip"))
  current_dir <- getwd()

  if (require(zip)) {

    setwd(dirname(output_dir))
    suppressWarnings(
      zip::zip(zipfile = output_file,
               files = file.path(basename(output_dir),
                                 list.files(output_dir, recursive = TRUE,
                                            all.files = TRUE, include.dirs = TRUE)),
               recurse = FALSE)
    )

  }

  setwd(current_dir)
  unlink(output_dir, recursive = TRUE, force = TRUE)

  return(invisible(output_file))

}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param filename    (name) PARAM_DESCRIPTION
#' @param verbose     (logical) PARAM_DESCRIPTION, Default: FALSE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export
load_flow <- function(filename, verbose = FALSE) {

  stopifnot(file.exists(filename))

  # print(filename)

  # Unzip the file, if it exists
  flow_folder <- gsub(basename(filename), pattern = ".zip", replacement = "")
  # print(flow_folder)

  output_dir <- file.path(dirname(filename), paste0("unzipped_", flow_folder))
  # print(output_dir)

  current_dir <- getwd()
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  setwd(output_dir)
  # print("Unzipping")
  unzip(zipfile = filename, exdir = output_dir)
  # print("Unzipped")

  output_dir <- file.path(output_dir, flow_folder)

  # Load flow
  if (verbose)
    cat("Loading main flow..\n") # nocov

  flow_file <- list.files(output_dir, pattern = "_flow.rds")
  flow <- readRDS(file.path(output_dir, flow_file))

  # For each process, incorporate it to the flow
  processes_dir <- file.path(output_dir, "processes")

  # Functions and models
  functions <- list.files(processes_dir, pattern = ".rds")
  models <- list.dirs(processes_dir, full.names = FALSE)
  models <- models[nzchar(models) > 0]

  for (file_f in functions) {

    function_name <- gsub(x = file_f, pattern = ".rds", replacement = "")

    f <- readRDS(file.path(processes_dir, file_f))

    if (inherits(f, "function")) {

      if (verbose)
        cat("Loading function:", function_name, "...\n") # nocov

    }

    if (inherits(f, "list")) {

      if (verbose)
        cat("Loading model scheme:", function_name, "...\n") # nocov

      class(f) <- c("DLscheme", class(f))
      flow$schemes[[function_name]] <- f

    }

    flow$processes[[function_name]] <- f

  }

  for (model_name in models) {

    if (verbose)
      cat("Loading model:", model_name, "...\n") # nocov

    flow$processes[[model_name]] <- load_model(path = processes_dir, prefix = model_name)

  }

  # Delete the output_dir
  setwd(current_dir)
  unlink(output_dir, recursive = TRUE, force = TRUE)

  # Return the flow
  return(flow)

}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export
clone_flow <- function(flow) {

  new_flow <- flow %>% save_flow() %>% load_flow()

  return(new_flow)

}