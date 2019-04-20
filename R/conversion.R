get_as_antsImage <- function(img) {

  library(ANTsR)
  library(neurobase)

  if (is.character(img) && file.exists(img)) {

    return(antsImageRead(img))

  }

  if (is.antsImage(img)) {

    return(img)

  }

  if (is.nifti(img)) {

    my_file <- neurobase::tempimg(img)

    return(antsImageRead(my_file))

  }

}

get_as_nifti <- function(img) {

  library(ANTsR)
  library(neurobase)

  if (is.character(img) && file.exists(img)) {

    return(readnii(img))

  }

  if (is.antsImage(img)) {

    my_file <- tempfile(fileext = ".nii.gz")
    antsImageWrite(image = img, filename = my_file)

    return(readnii(my_file))

  }

  if (is.nifti(img)) {

    return(img)

  }

}