
[![Travis build status](https://travis-ci.org/ITSLeeds/opentripplanner.svg?branch=master)](https://travis-ci.org/ITSLeeds/opentripplanner)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Open Trip Planner for R
=======================

The goal of Open Trip Planner for R is to provide a simple R interface to [Open Trip Planner (OTP)](https://www.opentripplanner.org/). The OTP is a multimodal trip planning service. OTP is written in Java, and you will need both Java and R to use this package.

This package can be used to interface with a remote instance of OTP (e.g. a website) or help you set up and manage a local version of OTP for private use. For more information on what OTP is, see the [Dummies Guide vignette](https://github.com/ITSLeeds/opentripplanner/blob/master/vignettes/dummies_guide.Rmd).

Installation - Open Trip Planner
--------------------------------

To use Open Trip Planner on your local computer you will need to install Java 8 and download the latest version of OTP. Instructions on installing Java and setting up OTP can be found in the [getting-started vignette](https://github.com/ITSLeeds/opentripplanner/blob/master/vignettes/getting_started.Rmd).

Installation - R Package
------------------------

Install the package with **devtools** as follows:

``` r
install.packages("devtools") # If you do not already have the devtools package
devtools::install_github("ITSleeds/opentripplanner")
```

Tests
-----

Tests only run on machines that have the environment variable `I_have_OTP` with the value `TRUE`. You can add this with `usethis::edit_r_environ()`.

``` r
Sys.getenv("I_have_OTP")
#> [1] ""
```

Package Status
--------------

This package is part of ongoing research at the University of Leeds, it is provided "as is" and is likely to be updated and changed without warning to meet the research needs of the University. It is our intention to bring a stable version to CRAN as soon as possible.
