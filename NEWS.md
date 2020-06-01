# opentripplanner 0.2.1.X (In development)

* Changes to support dplyr 1.0.0
* Added timezone support to `otp_plan` fixing issue #54
* Switch from dplyr to data.table
* Added timezone support to `otp_connect`, `otp_plan`, and `otp_isochrone` fixing issue #54, see docs for details.
* Added `quiet` argument to `otp_dl_jar` and `otp_dl_demo`
* Fixed error in advanced features vignette, issue #57

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

* Fixed bug where routing failes due to missing fare data

# opentripplanner 0.2.0.4

* Added a `NEWS.md` file to track changes to the package.
* Fixed CRAN test error on certain OS
* Added CRAN badges 

# opentripplanner 0.2.0.3

* First CRAN release

# opentripplanner 0.2.0.0

* Release peer-reviewed by ROpenSci
