#' Set up and confirm a connection to an OTP instance.
#'
#' Defines the parameters required to connect to a router on an OTP
#' instance and, if required, confirms that the instance and router
#' are query-able.
#'
#' @param hostname A string, e.g. "ec2-34-217-73-26.us-west-2.compute.amazonaws.com".
#'     Optional, default is "localhost".
#' @param router A string, e.g. "UK2018". Optional, default is "default". OTP can support multiple routers see advanced vignette for details.
#' @param url If a non-standard URL structure is used provide a full url,
#'     default is NULL
#' @param port A positive integer. Optional, default is 8080.
#' @param ssl Logical, indicates whether to use https. Optional,
#'     default is FALSE.
#' @param check Logical. If TRUE connection object is only returned if OTP
#'     instance and router are confirmed reachable. Optional, default is TRUE.
#' @return Returns an S3 object of class otpconnect. If \code{check} is TRUE
#'     and the router is not reachable the object is not returned.
#' @family connect
#' @details
#' The default URL structure for the OTP API is:
#' http://<hostname>:<port>/otp/routers/<router>
#' For example: http://localhost:8080/otp/routers/default
#'
#' Functions construct the URL from the parameters provided in otpconnect
#' objects. However some websites hosting OTP have modified the default
#' URL structure. If this is the case you can use the \code{url} parameter
#' to bypass the URL construction and provide a fully formed URL. In this
#' case the \code{hostname}, \code{router}, \code{port}, and \code{ssl}
#' are ignored.
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
                        check = TRUE) {
  # argument checks

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_string(hostname, add = coll)
  checkmate::assert_string(router, add = coll)
  checkmate::assert_string(url, add = coll, null.ok = TRUE)
  checkmate::assert_int(port, lower = 1, add = coll)
  checkmate::assert_logical(ssl, add = coll)
  checkmate::assert_logical(check, add = coll)
  checkmate::reportAssertions(coll)

  otpcon <- list(
    hostname = hostname,
    router = router,
    url = url,
    port = port,
    ssl = ssl
  )

  # Set the name for the class
  class(otpcon) <- append(class(otpcon), "otpconnect")


  # If check then confirm router is queryable

  if (isTRUE(check)) {
    if (check_router(otpcon) == 200) {
      message("Router ", make_url(otpcon), " exists")
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
    url <- paste0(
      ifelse(isTRUE(x$ssl), "https://", "http://"),
      x$hostname,
      ":",
      x$port,
      "/otp/routers/",
      x$router
    )
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
  check <- try(httr::GET(make_url(x)), silent = TRUE)
  if (class(check) == "try-error") {
    return(check[1])
  } else {
    return(check$status_code)
  }
}
