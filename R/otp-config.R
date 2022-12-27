#' Write config object as json file
#'
#' @description Takes a config list produced by `otp_make_config()` and saves it
#' as json file for OTP
#'
#' @param config A named list made/modified from `otp_make_config()`
#' @param dir Path to folder where data for OTP is to be stored
#' @param router name of the router, default is "default", must be a subfolder
#'   of dir/graphs
#' @family setup
#' @examples
#' \dontrun{
#' conf <- otp_make_config("build")
#' otp_write_config(conf, dir = tempdir())
#' }
#' @export

otp_write_config <- function(config,
                             dir = NULL,
                             router = "default") {
  # Validate and Clean
  type <- attributes(config)$config_type
  otp_validate_config(config)
  config <- otp_list_clean(config)

  message(
    "The config file will be saved to ",
    file.path(dir, "graphs", router)
  )

  exportJson <- rjson::toJSON(config, indent = 4)

  write(
    exportJson,
    file.path(
      dir,
      "graphs",
      router,
      paste0(type, "-config.json")
    )
  )
}

#' Remove NULL values from list
#' Modified from
#' https://www.rdocumentation.org/packages/rlist/versions/0.4.6.1/topics/list.clean
#'
#' @param .data list
#' @param fun function
#'
#' @noRd

otp_list_clean <- function(.data, fun = function(x) {
                             length(x) == 0L
                           }) {
  .data <- lapply(.data, function(.item) {
    if (is.list(.item)) {
      otp_list_clean(.item, fun)
    } else {
      .item
    }
  })
  setmembers <- `[<-`
  setmembers(.data, vapply(.data, fun, logical(1L)), NULL)
}



#' Validate Config Object
#' @description
#' Checks if the list of OTP configuration options is valid
#'
#' @param config A named list made/modified from `otp_make_config()`
#' @param type type of config file
#' @param verion version of OPT e.g. 1 or 2
#' @family setup
#' @details
#'
#' Performs basic validity checks on class, max/min values etc as appropriate,
#' some of more complex parameters are not checked. For more details see:
#'
#' http://docs.opentripplanner.org/en/latest/Configuration
#' http://dev.opentripplanner.org/javadoc/1.3.0/org/opentripplanner/routing/core/RoutingRequest.html
#' @examples
#' \dontrun{
#' conf <- otp_make_config("build")
#' otp_validate_config(conf)
#' }
#' @export

otp_validate_config <- function(config,
                                type = attributes(config)$config_type,
                                version = 1) {

  checkmate::assert_number(version, lower = 1, upper = 2)
  if(version == 1){
    otp_validate_config_v1(config, type)
  } else {
    otp_validate_config_v2(config, type)
  }

}

#' Make Config Object
#' @description
#' OTP can be configured using three json files `otp-config.json`,
#' `build-config.json`, and `router-config.json`. This function
#' creates a named list for each config file and
#' populates the defaults values.
#'
#' @param type Which type of config file to create, "otp", "build", "router"
#' @param version version of OPT e.g. 1 or 2
#' @family setup
#' @details
#' For more details see:
#' http://docs.opentripplanner.org/en/latest/Configuration
#' @examples
#' {
#'   conf <- otp_make_config("build")
#'   conf <- otp_make_config("router")
#' }
#' @export

otp_make_config <- function(type, version = 1) {

  checkmate::assert_number(version, lower = 1, upper = 2)
  if(version == 1){
    otp_make_config_v1(type)
  } else {
    otp_make_config_v2(type)
  }
}
