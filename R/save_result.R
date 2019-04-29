# Save an object to the temp folder.
.save_result <- function(value, name, folder = tempdir()) {

  if (inherits(value, "nifti") | inherits(value, "niftiImage")) {

    my_file <- file.path(folder, paste0(name, ".nii.gz"))

    neurobase::writenii(nim = value,
                        filename = my_file)

    return(invisible(TRUE))

  }

  if (inherits(value, "antsImage")) {

    my_file <- file.path(folder, paste0(name, ".nii.gz"))

    ANTsRCore::antsImageWrite(image = value,
                              filename = my_file)

    return(invisible(TRUE))

  }

  my_file <- file.path(folder, paste0(name, ".rds"))

  saveRDS(object = value,
          file = my_file)

  return(invisible(TRUE))

}