#' Use OTP Geo-coder to find a location
#'
#' Geo-coding converts a named place, such as a street name into a lng/lat pair.
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Longitude/Latitude pair, e.g.
#'   `c(-0.134649,51.529258)`, or 2 column matrix of Longitude/Latitude pairs,
#'   or sf data frame of POINTS with CRS 4326
#' @param pointsset character, name of pointset
#' @param dir A character string, path to a directory containing the necessary
#'   files, see details
#' @param date_time POSIXct, a date and time, defaults to current date and time
#' @param maxWalkDistance Numeric passed to OTP in metres
#' @param arriveBy Logical, Whether the trip should depart or arrive at the
#'   specified date and time, default FALSE
#' @param timezone Character, what timezone to use, see as.POSIXct, default is
#'   local timezone
#' @family analyst
#' @return Returns a data.frame of travel times
#' @examples
#' \dontrun{
#' times <- otp_surface(otpcon, c(-1.17502, 50.64590), "lsoa", path_data)
#' }
#' @details THis function requires the analysis and pointset features to be
#' enabled during `otp_setup()`. Thus it will only work with OTP 1.x. For more
#' detail see the analyst vignette.
#'
#' @export
otp_surface <- function(otpcon = NULL,
                        fromPlace = NULL,
                        pointsset = NULL,
                        mode = "CAR",
                        date_time = Sys.time(),
                        maxWalkDistance = 800,
                        arriveBy = FALSE,
                        timezone = otpcon$timezone,
                        dir = otpcon$dir) {
  # Check for OTP2
  if (!is.null(otpcon$otp_version)) {
    if (otpcon$otp_version >= 2) {
      stop("Surface is not supported by OTP v2.X")
    }
  }

  if (is.null(timezone)) {
    warning("otpcon is missing the timezone variaible, assuming local timezone")
    timezone <- Sys.timezone()
  }

  # Validate Inputs
  date <- format(date_time, "%m-%d-%Y", tz = timezone)
  time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
  checkmate::assert_class(otpcon, "otpconnect")
  mode <- toupper(mode)
  checkmate::assert_subset(mode,
                           choices = c(
                             "TRANSIT", "WALK", "BICYCLE",
                             "CAR", "BUS", "RAIL"
                           ),
                           empty.ok = FALSE
  )
  mode <- paste(mode, collapse = ",")
  checkmate::assert_logical(arriveBy)
  arriveBy <- tolower(arriveBy)

  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  toPlace <- otp_clean_input(toPlace, "toPlace")

  if(!dir.exists(file.path(dir,"pointsets"))){
    dir.create(file.path(dir,"pointsets"))
  }

  write.csv(toPlace,
            file = file.path(dir,"pointsets","desinations.csv"),
            row.names = FALSE)


  # Make POST Request to build surface

  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/surfaces")
  routerUrl <- "http://localhost:8080/otp/surfaces"

  querylist <- list(
    batch = "true",
    fromPlace = "50.64590,-1.17502",
    date = date,
    time = time,
    mode = mode,
    maxWalkDistance = maxWalkDistance,
    arriveBy = arriveBy
  )

  # convert response content into text
  url <- build_url(routerUrl, querylist)
  h <- curl::new_handle()
  curl::handle_setopt(h, post = TRUE)
  text <- curl::curl_fetch_memory(url, handle = h)
  text <- rawToChar(text$content)
  #TODO: CHECK STATUS IS 200


  url2 <- "http://localhost:8080/otp/surfaces/0/indicator?targets=points&detail=true"
  text2 <- curl::curl_fetch_memory(url2)
  text2 <- rawToChar(text2$content)
  asjson <- rjson::fromJSON(text2)

}

#'  Create a pointset
#'
#' @param points Numeric vector, Longitude/Latitude pair, e.g.
#'   `c(-0.134649,51.529258)`, or 2 column matrix of Longitude/Latitude pairs,
#'   or sf data frame of POINTS with CRS 4326
#' @param name Character, name for pointset
#' @param dir A character string, path to a directory containing the necessary
#'   files, see details
#'
#' @family routing
#' @return
#' Returns a data.frame of SF POINTS or Coordinates of all
#'     the locations that match `query`
#' @examples
#' \dontrun{
#' locations <- otp_geocode(otpcon, "High Street")
#' }
#' @details
#' OTP will return a maximum of 10 results
#'
#' @export
otp_pointset <- function(points = NULL,
                        name = NULL,
                        dir = NULL) {

  points <- otp_clean_input(points, "points")

  if(!dir.exists(file.path(dir,"pointsets"))){
    dir.create(file.path(dir,"pointsets"))
  }

  write.csv(points,
            file = file.path(dir,"pointsets",name,".csv"),
            row.names = FALSE)

  return(NULL)
}
