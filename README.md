---
output: github_document
---

[![Travis build status](https://travis-ci.org/ITSLeeds/opentripplanner.svg?branch=master)](https://travis-ci.org/ITSLeeds/opentripplanner) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Coverage status](https://codecov.io/gh/ITSLeeds/opentripplanner/branch/master/graph/badge.svg)](https://codecov.io/github/ITSLeeds/opentripplanner?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->



# Open Trip Planner for R

The goal of Open Trip Planner for R is to provide a simple R interface to [Open Trip Planner (OTP)](https://www.opentripplanner.org/). The OTP is a multimodal trip planning service. OTP is written in Java, and you will need both Java and R to use this package.

This package can be used to interface with a remote instance of OTP (e.g. a website) or help you set up and manage a local version of OTP for private use. For more information on what OTP is, see the [Prerequisites vignette](https://itsleeds.github.io/opentripplanner/articles/prerequisites.html).

Basic setup and routing functions are outlined in the [getting started vignette](https://itsleeds.github.io/opentripplanner/articles/opentripplanner.html), while advanced fucntionality such as batch routing, isochones, and customised setup is described in the [advanced features vignette](https://itsleeds.github.io/opentripplanner/articles/advanced_features.html)

## Installation

### Open Trip Planner

To use Open Trip Planner on your local computer you will need to install Java 8 and download the latest version of OTP. Instructions on installing Java and setting up OTP can be found in the [getting started vignette](https://itsleeds.github.io/opentripplanner/articles/opentripplanner.html).

### R Package

Install the package with **devtools** as follows:


```r
install.packages("devtools") # If you do not already have the devtools package
devtools::install_github("ITSleeds/opentripplanner")
```

## Usage

The package contains three groups of functions:

Functions for setting up a local instance of OTP:

1. `otp_build_graph()` To make a OTP graph from raw data
1. `otp_setup()` To star up a local instance of OTP

Functions for connecting to a local or remote instance of OTP:

1. `otp_connect()`

Functions for retrieving data from OTP:

1. `otp_plan()` To get routes from A to B
1. `otp_isochone()` To get isochrone maps
1. `otp_geocode()` To get the locations of named places e.g. road names

Results are returned as [sf objects](https://cran.r-project.org/web/packages/sf/index.html)

## Tests 

As this package does not work without a working connection to OTP, tests only run on machines that have the environment variable `I_have_OTP` with the value `TRUE`.
You can add this with `usethis::edit_r_environ()`.


```r
Sys.getenv("I_have_OTP")
#> [1] "FALSE"
```

## Acknowledgement

This package was built off the [tutorial by Marcus Young](https://github.com/marcusyoung/otp-tutorial) 

## Contribution

Please note that the `opentripplanner` project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.


## Package Status

This package is part of ongoing research at the University of Leeds, it is provided "as is" and is likely to be updated and changed without warning to meet the research needs of the University. It is our intention to bring a stable version to CRAN as soon as possible.
