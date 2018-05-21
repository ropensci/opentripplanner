#' Set up and confirm a connection to an OTP instance.
#'
#' Defines the parameters required to connect to a router on an OTP instance and
#' confirms that the instance is queryable.
#'
#' @param hostname A string, e.g. "ec2-34-217-73-26.us-west-2.compute.amazonaws.com".
#'     Optional, default is "localhost".
#' @param router A string, e.g. "UK2018". Optional, default is "default".
#' @param port A positive integer. Optional, default is 8080.
#' @param ssl A logical value, indicates whether to use https. Optional, default is FALSE.
#' @return Returns an S3 object of class otpconnect.
#' @examples
#' otpcon <- otp_connect()
#' otpcon <- otp_connect(router = "UK2018",
#'                       ssl = TRUE)
#' otpcon <- otp_connect(hostname = "ec2.us-west-2.compute.amazonaws.com",
#'                       router = "UK2018",
#'                       port = 8888,
#'                       ssl = TRUE)

#' @export
otp_connect <- function(hostname = "localhost",
                   router = "default",
                   port = 8080,
                   ssl = FALSE)
{
  # argument checks

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_string(hostname, add = coll)
  checkmate::assert_string(router, add = coll)
  checkmate::assert_int(port, lower = 1, add = coll)
  checkmate::assert_logical(ssl, add = coll)
  checkmate::reportAssertions(coll)

  otpcon <- list(
    hostname = hostname,
    router = router,
    port = port,
    ssl = ssl
  )

  ## Set the name for the class
  class(otpcon) <- append(class(otpcon),"otpconnect")
  return(otpcon)
}

# otpconnect class method to generate baseurl

make_url <- function(x)
{
  UseMethod("make_url", x)
}

make_url.default <- function(x)
{
  warning(paste0("make_url does not know how to handle objects of class ",
        class(x), ", and can only be used on the class otpconnect"))
  return(NULL)
}

make_url.otpconnect <- function(x)
{
  url <- paste0(
    ifelse(isTRUE(x$ssl), 'https://', 'http://'),
    x$hostname,
    ':',
    x$port,
    '/otp/routers/',
    x$router
  )
  return(url)
}


