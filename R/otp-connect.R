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
    add = coll
  )

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
  #class(otpcon) <- append(class(otpcon), "otpconnect")
  class(otpcon) <- c("list","otpconnect")

  # If check then confirm router is queryable

  if (isTRUE(check)) {
    chk <- check_routers(otpcon)
    if(!isTRUE(chk)){
      stop(chk)
    }
    otpcon$otp_version <- otp_check_version(otpcon, warn = FALSE)

  }
  return(otpcon)
}

#' Make Url
#' @param x otpcon
#' @param type character
#' @family internal
#' @noRd
#'
make_url <- function(x, type = "routers") {
  if(!"otpconnect" %in% class(x)){
    stop("Object is not of class otpconnect, class is ", class(x))
  }

  if(type == "routers"){
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
  } else {
    if (x$ssl) {
      url <- paste0(
        "https://",
        x$hostname,
        ":",
        x$port,
        "/otp/",
        type
      )
    } else {
      url <- paste0(
        "http://",
        x$hostname,
        ":",
        x$port,
        "/otp/",
        type
      )
    }

  }


  return(url)
}

#' otpconnect method to check if router exists
#' @param x otpcon
#' @family internal
#' @noRd
#'
check_router <- function(x) {
  if(!"otpconnect" %in% class(x)){
    stop("Object is not of class otpconnect, class is ", class(x))
  }
  check <- try(curl::curl_fetch_memory(make_url(x)), silent = TRUE)
  if (inherits(check, "try-error")) {
    return(check[1])
  } else {
    return(check$status_code)
  }
}

#' otpconnect method to check if router exists
#' @param otpcon otpcon
#' @family internal
#' @noRd
#'
check_routers <- function(otpcon) {
  if(!"otpconnect" %in% class(otpcon)){
    stop("Object is not of class otpconnect, class is ", class(otpcon))
  }

  if (is.null(otpcon$url)) {
    url <- paste0(
      ifelse(isTRUE(otpcon$ssl), "https://", "http://"),
      otpcon$hostname,
      ":",
      otpcon$port,
      "/otp/routers"
    )
  } else {
    # TODO: Fix this
    warning("this is not supported yet")
  }

  check <- try(curl::curl_fetch_memory(url), silent = TRUE)
  if (inherits(check, "try-error")) {
    return(paste0("Router ", make_url(otpcon), " does not exist"))
  }

  check <- rawToChar(check$content)
  check <- rjson::fromJSON(check)
  check <- unlist(lapply(check$routerInfo, function(x){x$routerId}))

  if(otpcon$router %in% check){
    message("Router ", make_url(otpcon), " exists")
    return(TRUE)
  } else {
    return(paste0("Router ", make_url(otpcon), " does not exist. Valid routers are: ", paste(check, collapse = ", ")))
  }
  return(FALSE)
}


#' Check the what version of OTP the server is running
#' @param otpcon otpcon object from otp_connect()
#' @param warn logical, if TRUE will check that OTP version matches contents of otpcon
#' @family setup
#' @export
#'
otp_check_version <- function(otpcon, warn = TRUE) {
  if (is.null(otpcon$url)) {
    url <- paste0(
      ifelse(isTRUE(otpcon$ssl), "https://", "http://"),
      otpcon$hostname,
      ":",
      otpcon$port,
      "/otp"
    )
  } else {
    url <- otpcon$url
  }

  ver <- try(curl::curl_fetch_memory(url), silent = TRUE)
  if (inherits(ver, "try-error")) {
    return(ver[1])
  }

  ver <- rawToChar(ver$content)
  ver <- RcppSimdJson::fparse(ver)
  ver_res <- suppressWarnings(as.numeric(paste0(
    ver$serverVersion$major,
    ".",
    ver$serverVersion$minor
  )))
  if(is.na(ver_res)){
    # Fix for 2.2
    ver_res <- as.numeric(paste0(
      ver$version$major,
      ".",
      ver$version$minor
    ))
  }


  if (warn) {
    if (ver_res != otpcon$otp_version) {
      warning("The version of OTP running does not match otpcon$otp_version")
    }
  }

  return(ver_res)
}
