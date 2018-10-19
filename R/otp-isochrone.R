#' Get the Isochrones from a location
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.529258,-0.134649)`
#' @param mode Character vector of modes of travel valid values TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR
#' @param date_time POSIXct, a date and time, defaults to current date and time
#' @param arriveBy Logical, Whether the trip should depart or arrive at the specified date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP
#' @param walkReluctance Numeric passed to OTP
#' @param transferPenalty Numeric passed to OTP
#' @param minTransferTime Numeric passed to OTP
#' @param cutoffSec Numeric vector, number of seconds to define the break points of each Isochrone
#' @return
#' Returns a data.frame of SF POLYGONS
#' @examples
#' isochrone1 <- otp_isochrone(otpcon, fromPlace = c(51.5292,-0.1346))
#' isochrone2 <- otp_isochrone(otpcon, fromPlace = c(51.5292,-0.1346), mode = c("WALK","TRANSIT"), cutoffSec = c(600,1200,1800))
#' @detials Isochrones are maps of equal travel time,
#' for a given location a map is produced shoing how long it takes to reach
#' each location.
#' @export
otp_isochrone <- function(otpcon = NA,
                     fromPlace = NA,
                     mode = "TRANSIT",
                     date_time = Sys.time(),
                     arriveBy = FALSE,
                     maxWalkDistance = 800,
                     walkReluctance = 5,
                     transferPenalty = 0,
                     minTransferTime = 600,
                     cutoffSec = c(600,1200,1800,2400, 3000, 3600))
{
  # Check Valid Inputs
  checkmate::assert_class(otpcon,"otpconnect")
  checkmate::assert_numeric(fromPlace, lower =  -180, upper = 180, len = 2)
  fromPlace <- paste(fromPlace, collapse = ",")
  mode <- toupper(mode)
  checkmate::assert_subset(mode, choices = c("TRANSIT","WALK","BICYCLE","CAR","BUS","RAIL"), empty.ok = F)
  mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m/%d/%Y")
  time <- format(date_time, '%I:%M:%S')
  checkmate::assert_numeric(cutoffSec, lower =  0)
  checkmate::assert_logical(arriveBy)
  arriveBy <- tolower(as.character(arriveBy))

  # Construct URL
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl,"/isochrone")

  query <- list(
    fromPlace = fromPlace,
    mode = mode,
    date = date,
    time = time,
    maxWalkDistance = maxWalkDistance,
    walkReluctance = walkReluctance,
    arriveBy = arriveBy,
    transferPenalty = transferPenalty,
    minTransferTime = minTransferTime
  )
  cutoffSec <-  as.list(cutoffSec)
  names(cutoffSec) = rep("cutoffSec",length(cutoffSec))
  query <- c(query,cutoffSec)

  req <- httr::GET(
    routerUrl,
    query = query
  )

  # convert response content into text
  text <- httr::content(req, as = "text", encoding = "UTF-8")

  if(nchar(text) < 200){
    warning("Failed to get isochrone, returning error message")
    return(text)
  }else{
    # parse to sf
    response = sf::st_read(text, quiet = T)
    response$id = seq(1:nrow(response))
    return(response)
  }

}

