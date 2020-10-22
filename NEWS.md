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
