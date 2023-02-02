# opentripplanner 0.5.1

* Updated dependency on purrr to require >= 1.0.0
* Fixed bug in `otp_plan` when using `toID` with thanks to @temporalista


# opentripplanner 0.5.0

Version 0.5.0 is a significant rewrite of the package focusing on substantially faster routing. The package has moved from using multiple R workers requesting and parsing results from OTP to a two stage process. Stage 1 uses `curl` to send asynchronous requests to OTP. This stage should be as fast as OTP, and supports OTP's limit of 1.25x the number of cores. Stage 2 parses the JSON and converts it into a form usable in R. The parsing currently uses a single thread, but has been optimised and can now parse around 700-800 routes/second which is faster than OTP at around 40-50 routes/second/core. 

Overall the new process is 3-4 times faster than v0.4.0 and uses less memory. Future development may add mulitcore support to the parsing stage for further speed improvements.

Breaking Changes

* surfaceID returned from `otp_make_surface` is now a list of lists to allow multiple inputs/outputs
* removed legacy support for old version of R (<4.0) that don't support `RcppSimdJson` for older versions of R use version 0.2.3
* Some columns returned by `otp_plan` have changed names. The names now start with "leg_" to show they are leg specific variables e.g. "mode" has become "leg_mode" 
 
Other Changes

* removed dependency on `raster` and `rgdal` replaced with `terra`
* replaced multi-core routeing with asynchronous requests using `curl` resulting in faster routing and lower resource usage
* remove `pbapply` dependency replaced with `progressr`
* remove dependency on `lubridate`
* default ncores argument changed from `1` to `round(parallel::detectCores() * 1.25) - 1`
* Typos fixed in documentation
* Support for OTP 2.2 and Java 17

# opentripplanner 0.4.0

* Fix broken or moved URLs
* `tibble` moved from imports to suggests
* Support for OTP 2.0
* Support for analyst features in OTP 1.x with new functions
* Support for `s2` features in the `sf` package

# opentripplanner 0.3.2

* Fix a bug with parsing fare data
* Fix bug with latest version of Java defaulting to 32Bit even when 64bit is available.
* Added more `try()` functions to reduce risk of crashes in large scale batch routing
* Added `flag64bit` argument to `otp_build_graph()` and `otp_setup()`
* Added `quiet` argument to `otp_build_graph()`
* Updated the Known Issues Vignette
* reduce initial wait time for `otp_setup()` from 60 seconds to 30 seconds
* switch to OTP 1.5.0 as default version in `otp_dl_jar()`

# opentripplanner 0.3.1

Limited support for version of R than can't install `RcppSimdJson`

# opentripplanner 0.3.0

Note that this version makes minor changes to how results are returned, for example column order. These changes are due to the new json parser and should not affect the overall results but may affect any dependent code.

* Significant refactor of code giving up to 50% reduction in routing time
* replaced `dplyr` with `data.table`
* replaced `httr` with `curl`
* replaced many `rjson` functions with `RcppSimdJson` equivalents
* replaced many `sf` functions with `sfheaders` equivalents
* fixed bug in `otp_plan` when `distance_balancing = TRUE`
* fixed bug #69
* In `otp_plan` will now return `fromPlace` and `toPlace` as the first two columns
* In `otp_plan` set `get_elevation = FALSE` as default this boosts performance
* Fixed bug in `distance_balancing` that gave sub-optimal balancing
* When `distance_balancing = TRUE` zero distance routes will not be found, as OTP will reject these in any case, this saves time with no impact on results.


# opentripplanner 0.2.3.0

* Added `distance_balancing` argument to `otp_plan` gives a small performance boost to multicore routing
* Added `get_elevation` argument to `otp_plan` default TRUE, when FALSE returns XY coordinates rather than XYZ coordinates and gives a 4% performance boost.
* Removed helper code for `dplyr::bind_rows` as no longer required for `dplyr 1.0.0`

# opentripplanner 0.2.2.0

* Changes to support `dplyr 1.0.0`, package now needs `vctrs 0.3.1`
* Added timezone support to `otp_connect`, `otp_plan`, and `otp_isochrone` fixing issue #54, see docs for details.
* Added `quiet` argument to `otp_dl_jar` and `otp_dl_demo`
* Fixed error in advanced features vignette, issue #57
* Switch from dplyr to data.table, issue #60

# opentripplanner 0.2.1.0

* Batch isochrones support added
* Fix bug in `correct_distances()` when input is of length <= 2 or the distances never decrease
* Fix bug in `polyline2linestring()` when elevation is length <= 2
* New input argument to `otp_plan()` and otp_isochrone routingOptions this allows support
    for many more routing options to be set. Arguments walkReluctance, transferPenalty, and
    minTransferTime have been removed and replaced with routingOptions.
* New functions `otp_routing_options()`, `otp_validate_routing_options()`, `otp_check_java()`
* New data for internal package checking
* Added support of the UK way property set (for OTP 1.5)

# opentripplanner 0.2.0.8

* Disabled CRAN tests that fail on solaris OS, due to different wording of error messages

# opentripplanner 0.2.0.6

* Fixed bug where routing fails due to missing fare data

# opentripplanner 0.2.0.4

* Added a `NEWS.md` file to track changes to the package.
* Fixed CRAN test error on certain OS
* Added CRAN badges 

# opentripplanner 0.2.0.3

* First CRAN release

# opentripplanner 0.2.0.0

* Release peer-reviewed by ROpenSci
