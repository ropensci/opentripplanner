
[![Travis build
status](https://travis-ci.org/ITSLeeds/opentripplanner.svg?branch=master)](https://travis-ci.org/ITSLeeds/opentripplanner)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Open Trip Planner for R

The goal of Open Trip Planner for R is to provide a simple R interface
to Open Trip Planner (OTP).

## Installation - Open Trip Planner

To use Open Trip Planner for R you will need a running instance of OTP.
A basic tutorial on setting up OTP is available at
<http://docs.opentripplanner.org/en/latest/Basic-Tutorial/>, and an
intermediate tutorial complete with sample data is available at
<https://github.com/marcusyoung/otp-tutorial>.

Instructons for installing OTP on Linux can be found in the
[`getting-started`
vignette](https://github.com/ITSLeeds/opentripplanner/blob/master/vignettes/getting_started.Rmd).

## Installation - R Package

Install the package with **devtools** as follows:

``` r
# install.packages("devtools")
devtools::install_github("marcusyoung/opentripplanner")
```

## Tests

Tests only run on machines that have the environment variable
`I_have_OTP` with the value `TRUE`. You can add this with
`usethis::edit_r_environ()`.

``` r
Sys.getenv("I_have_OTP")
#> [1] ""
```
