#' Set up and confirm a connection to an OTP instance.
#'
#' Defines the parameters required to connect to a router on an OTP instance
#' and, if required, confirms that the instance and router are query-able.
#'
#' @param hostname A string, e.g.
#'   "ec2-34-217-73-26.us-west-2.compute.amazonaws.com". Optional, default is
#'   "localhost".
#' @param router A string, e.g. "UK2018". Optional, default is "default". OTP
#'   can support multiple routers see advanced vignette for details.
#' @param url If a non-standard URL structure is used provide a full url,
#'   default is NULL
#' @param port A positive integer. Optional, default is 8080.
#' @param ssl Logical, indicates whether to use https. Optional, default is
#'   FALSE.
#' @param check Logical. If TRUE connection object is only returned if OTP
#'   instance and router are confirmed reachable. Optional, default is TRUE.
#' @param timezone Character, timezone, defaults to local timezone
#' @param otp_version Numeric, OTP Version in use default 1.5, if check is TRUE
#'   then `otp_check_version()` is called and otp_version is updated
#' @return Returns an S3 object of class otpconnect. If \code{check} is TRUE and
#'   the router is not reachable the object is not returned.
#' @family connect
#' @details The default URL structure for the OTP API is:
#' http://<hostname>:<port>/otp/routers/<router> For example:
#' http://localhost:8080/otp/routers/default
#'
#' Functions construct the URL from the parameters provided in otpconnect
#' objects. However some websites hosting OTP have modified the default URL
#' structure. If this is the case you can use the \code{url} parameter to bypass
#' the URL construction and provide a fully formed URL. In this case the
#' \code{hostname}, \code{router}, \code{port}, and \code{ssl} are ignored.
#' @examples
#' \dontrun{
#' otpcon <- otp_connect()
#' otpcon <- otp_connect(
#'   router = "UK2018",
#'   ssl = TRUE
#' )
#' otpcon <- otp_connect(
#'   hostname = "ec2.us-west-2.compute.amazonaws.com",
#'   router = "UK2018",
#'   port = 8888,
#'   ssl = TRUE
#' )
#' otpcon <- otp_connect(
#'   url = "https://api.digitransit.fi:443/routing/v1/routers/hsl"
#' )
#' }
#' @export
otp_connect <- function(hostname = "localhost",
                        router = "default",
                        url = NULL,
                        port = 8080,
                        ssl = FALSE,
                        check = TRUE,
                        timezone = Sys.timezone(),
                        otp_version = 1.5) {
  # argument checks

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_string(hostname, add = coll)
  checkmate::assert_string(router, add = coll)
  checkmate::assert_string(url, add = coll, null.ok = TRUE)
  checkmate::assert_string(timezone, add = coll)
  checkmate::assert_int(port, lower = 1, add = coll)
  checkmate::assert_logical(ssl, add = coll)
  checkmate::assert_logical(check, add = coll)
  checkmate::reportAssertions(coll)
  checkmate::assert_subset(timezone,
                           choices = OlsonNames(tzdir = NULL),
                           add = coll)

  otpcon <- list(
    hostname = hostname,
    router = router,
    url = url,
    port = port,
    ssl = ssl,
    timezone = timezone,
    otp_version = otp_version
  )

  # Set the name for the class
  class(otpcon) <- append(class(otpcon), "otpconnect")


  # If check then confirm router is queryable

  if (isTRUE(check)) {
    if (check_router(otpcon) == 200) {
      message("Router ", make_url(otpcon), " exists")

      otpcon$otp_version <- otp_check_version(otpcon, warn = FALSE)

      return(otpcon)
    } else {
      stop("Router ", make_url(otpcon), " does not exist. Error code ", check_router(otpcon))
    }
  } else {
    return(otpcon)
  }
}

# otpconnect class method to generate baseurl

#' Make URL
#' @param x otpcon
#' @family internal
#' @noRd
#'
make_url <- function(x) {
  UseMethod("make_url", x)
}

#' Make URL.defualt
#' @param x otpcon
#' @family internal
#' @noRd
#'
make_url.default <- function(x) {
  warning(
    "make_url does not know how to handle objects of class ",
    class(x),
    ", and can only be used on the class otpconnect"
  )
  return(NULL)
}

#' Make URL.optcon
#' @param x otpcon
#' @family internal
#' @noRd
#'
make_url.otpconnect <- function(x) {
  if (is.null(x$url)) {
    if (x$ssl) {
      url <- paste0(
        "https://",
        x$hostname,
        ":",
        x$port,
        "/otp/routers/",
        x$router
      )
    } else {
      url <- paste0(
        "http://",
        x$hostname,
        ":",
        x$port,
        "/otp/routers/",
        x$router
      )
    }
  } else {
    url <- x$url
  }

  return(url)
}



#' otpconnect method to check if router exists
#' @param x otpcon
#' @family internal
#' @noRd
#'
check_router <- function(x) {
  UseMethod("check_router", x)
}

#' otpconnect method to check if router exists (default)
#' @param x otpcon
#' @family internal
#' @noRd
#'
check_router.default <- function(x) {
  warning(
    "check_router does not know how to handle objects of class ",
    class(x),
    ", and can only be used on the class otpconnect"
  )
  return(NULL)
}

#' otpconnect method to check if router exists (otpcon)
#' @param x otpcon
#' @family internal
#' @noRd
#'
check_router.otpconnect <- function(x) {
  check <- try(curl::curl_fetch_memory(make_url(x)), silent = TRUE)
  if (class(check) == "try-error") {
    return(check[1])
  } else {
    return(check$status_code)
  }
}

#' Check the what version of OTP the server is running
#' @param otpcon otpcon object from otp_connect()
#' @param warn logical, if TRUE will check that OTP version matches contents of otpcon
#' @family setup
#' @export
#'
otp_check_version <- function(otpcon, warn = TRUE){

  if (is.null(otpcon$url)) {
    url <- paste0(ifelse(isTRUE(otpcon$ssl), 'https://', 'http://'),
              otpcon$hostname,
              ':',
              otpcon$port,
              '/otp')
  } else {
    url <- otpcon$url
  }

  ver <- try(curl::curl_fetch_memory(url), silent = TRUE)
  if ("try-error" %in% class(ver)) {
    return(ver[1])
  }

  ver <- rawToChar(ver$content)
  ver <- RcppSimdJson::fparse(ver)
  ver <- as.numeric(paste0(ver$serverVersion$major,
                           ".",
                           ver$serverVersion$minor))
  if(warn){
    if(ver != otpcon$otp_version){
      warning("The version of OTP running does not match otpcon$otp_version")
    }
  }

  return(ver)
}

