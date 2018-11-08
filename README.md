
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wf4ni

There are a lot of packages in the R ecosystem focused on several
aspects of Neuroimage processing, many of them listed in the [CRAN Task
View: Medical Imaging](https://CRAN.R-project.org/view=MedicalImaging)
section: `oro.nifti`, `oro.dicom`, `RNifti`, `fmri`, `divest` (among
others) for reading/writing of images; `ANTsR`, `fslr` for more advanced
processing methods; `RNiftyReg` specialized in registration, `mritc`
focused in image segmentation. Many more, for DTI, fMRI, molecular
imaging, and visualization are available in
[CRAN](https://CRAN.R-project.org).

This package has been written and is developed with some key features in
mind:

  - Simple definition of Neuroimaging flows, which can incorporate both
    `R` functions (including those from other packages, such as `ANTsR`
    or `fslr`) and *Deep Learning* models.
  - Simple definition of *Deep Learning* models. Many wrappers to common
    models ([U-Net](http://google.com/?q=unet),
    [SegNet](http://google.com/?q=segnet) …) have been included to ease
    the definition of models.
  - Highly customizable models, allowing concatenation and merging of
    models, layers, branching…
  - Creation of a common interface for all models built with this
    package, regarding training, testing and inference. This is achieved
    by the use of R6 classes.
  - Tackle different types of problems: classification, labelling or
    regression.
  - Work natively with NIfTI files.
  - Native 3D, although vector representations of images are allowed
    (and, in some cases, adviced).
  - Simple memory management. Users can impose memory constraints when
    training a model, batch size is automatically adjusted.
  - Dataset repositories. Some datasets regarding typical Neuroimaging
    problems (brain extraction, segmentation, parcellation…) are
    released in public repositories, and a mechanism to get these
    datasets into our local system to be able to train models is also
    included in the package.

## Requirements

The goal of wf4ni is to …

## Installation

Currently, this package is not in CRAN. The only way to get this package
installed is by obtaining the source and building and installing the
package in a local system.

    #!bash
    R CMD INSTALL /path/to/dl4ni

You can install the released version of wf4ni from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("wf4ni")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
