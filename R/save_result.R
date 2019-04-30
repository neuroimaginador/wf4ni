# Save an object to the temp folder.
#' @importFrom neurobase writenii
#' @importFrom ANTsRCore antsImageWrite
#'
.save_result <- function(value, name, folder = tempdir()) {

  if (inherits(value, "nifti") | inherits(value, "niftiImage")) {

    requireNamespace("neurobase", quietly = TRUE)

    my_file <- file.path(folder, paste0(name, ".nii.gz"))

    writenii(nim = value,
             filename = my_file)

    return(invisible(TRUE))

  }

  if (inherits(value, "antsImage")) {

    requireNamespace("ANTsRCore", quietly = TRUE)

    my_file <- file.path(folder, paste0(name, ".nii.gz"))

    antsImageWrite(image = value,
                   filename = my_file)

    return(invisible(TRUE))

  }

  my_file <- file.path(folder, paste0(name, ".rds"))

  saveRDS(object = value,
          file = my_file)

  return(invisible(TRUE))

}