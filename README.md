
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OpenTripPlanner for R <a href='https://itsleeds.github.io/'><img src='man/figures/logo.png' align="right" height=180/></a>

[![R build
status](https://github.com/ropensci/opentripplanner/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/opentripplanner/actions)
[![codecov](https://codecov.io/gh/ropensci/opentripplanner/branch/master/graph/badge.svg?token=iLEB77PnMk)](https://app.codecov.io/gh/ropensci/opentripplanner)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://badges.ropensci.org/295_status.svg)](https://github.com/ropensci/software-review/issues/295)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3558311.svg)](https://doi.org/10.5281/zenodo.3558311)
[![status](https://joss.theoj.org/papers/10.21105/joss.01926/status.svg)](https://joss.theoj.org/papers/10.21105/joss.01926)
[![](https://cranlogs.r-pkg.org/badges/grand-total/opentripplanner)](https://cran.r-project.org/package=opentripplanner)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/opentripplanner)](https://cran.r-project.org/package=opentripplanner)

**opentripplanner** is an R package that provides a simple yet flexible
interface to [OpenTripPlanner (OTP)](https://www.opentripplanner.org/).
OTP is a multimodal trip planning service written in Java. For more
information on what OTP is, see the [prerequisites
vignette](https://docs.ropensci.org/opentripplanner/articles/prerequisites.html).

**opentripplanner** can be used to interface with a remote instance of
OTP (e.g. a website) or help you set up and manage a local version of
OTP for private use. Basic setup and routing functions are outlined in
the [getting started
vignette](https://docs.ropensci.org/opentripplanner/articles/opentripplanner.html),
while advanced functionality such as batch routing, isochrones, and
customised setup is described in the [advanced features
vignette](https://docs.ropensci.org/opentripplanner/articles/advanced_features.html).

## What’s New

The newest version of the package 0.5.0 (Jan 2023) focuses on increased
routing speeds (3-4x faster) and expanding support for OTP v2.2. The new
version also drops legacy support for R 3.6, and so now requires R 4.0
or later. See
[news](https://docs.ropensci.org/opentripplanner/news/index.html) for
more details.

## Installation

### OpenTripPlanner

To use OpenTripPlanner on your local computer you will need to install
Java 8 and download the latest version of OTP. Instructions on
installing Java and setting up OTP can be found in the [prerequisites
vignette](https://docs.ropensci.org/opentripplanner/articles/prerequisites.html).

### R Package

To install the stable CRAN version:

``` r
install.packages("opentripplanner") # Install Package
library(opentripplanner)            # Load Package
```

Install the development version using **remotes**:

``` r
# If you do not already have the remotes package
install.packages("remotes")
# Install the package from GitHub
remotes::install_github("ropensci/opentripplanner")
# Load the package
library(opentripplanner)
```

## Usage

The package contains three groups of functions:

Functions for setting up a local instance of OTP:

1.  `otp_dl_jar()` To download the OTP Jar file;
2.  `otp_dl_demo()` To download the demo data for the Isle of Wight;
3.  `otp_check_java()` To check you have the correct version of Java;
4.  `otp_build_graph()` To make a OTP graph from raw data;
5.  `otp_setup()` To start up a local instance of OTP;
6.  `otp_make_config()` To make a config object;
7.  `otp_validate_config()` To validate a config object;
8.  `otp_write_config()` To save a config object as a json file.

Functions for connecting to a local or remote instance of OTP:

1.  `otp_connect()` To connect to OTP.

Functions for retrieving data from OTP:

1.  `otp_plan()` To get routes from A to B;
2.  `otp_geocode()` To get the locations of named places e.g. road names
    (OTP 1.x only);
3.  `otp_isochrone()` To get isochrone maps (OTP 1.x only);
4.  `otp_make_surface()` To make an analyst surface (OTP 1.x only);
5.  `otp_surface()` To evaluate a analyst surface (OTP 1.x only);
6.  `otp_traveltime()` To make a travel time matrix (OTP 1.x only);
7.  `otp_surface_isochrone()` To make a raster isochrone map (OTP 1.x
    only);

Results are returned as [sf
objects](https://CRAN.R-project.org/package=sf).

## Acknowledgement

This package was built off the [tutorial by Marcus
Young](https://github.com/marcusyoung/otp-tutorial).

## Contribution

Please note that the `opentripplanner` project is released with a
[Contributor Code of
Conduct](https://github.com/ropensci/opentripplanner/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms. Bug
reports and comments are welcome as Github
[Issues](https://github.com/ropensci/opentripplanner/issues) and code
submissions as [Pull
Requests](https://github.com/ropensci/opentripplanner/pulls).

## Citation

Please cite the JOSS paper in publications:

Morgan et al., (2019). OpenTripPlanner for R. Journal of Open Source
Software, 4(44), 1926, <https://doi.org/10.21105/joss.01926>

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
