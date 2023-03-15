#' Get the Isochrones from a location
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Longitude/Latitude pair,
#'     e.g. `c(-0.134649,51.529258)`,
#' or 2 column matrix of Longitude/Latitude pairs, or sf
#'     data frame of POINTS
#' @param fromID character vector same length as fromPlace
#' @param mode character vector of one or more modes of travel valid values
#'   TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, SUBWAY, TRAM, FERRY, BICYCLE_RENT,
#'   BICYCLE_PARK, CAR_PARK, CABLE_CAR, GONDOLA, FUNICULAR, AIRPLANE, default
#'   CAR. Not all combinations are valid e.g. c("WALK","BUS") is valid but
#'   c("WALK","CAR") is not.
#' @param date_time POSIXct, a date and time, defaults to current
#'     date and time
#' @param arriveBy Logical, Whether the trip should depart or
#'     arrive at the specified date and time, default FALSE
#' @param maxWalkDistance maximum distance to walk in metres
#' @param routingOptions named list passed to OTP see `otp_routing_options()`
#' @param cutoffSec Numeric vector, number of seconds to define
#'     the break points of each Isochrone
#' @param ncores number of cores to use in parallel processing
#' @param timezone character, timezone to use, default from otpcon
#' @family routing
#' @return
#' Returns a SF data.frame of POLYGONs
#' @examples
#' \dontrun{
#' isochrone1 <- otp_isochrone(otpcon, fromPlace = c(-0.1346, 51.5292))
#' isochrone2 <- otp_isochrone(otpcon,
#'   fromPlace = c(-0.1346, 51.5292),
#'   mode = c("WALK", "TRANSIT"), cutoffSec = c(600, 1200, 1800)
#' )
#' }
#' @details Isochrones are maps of equal travel time,
#' for a given location a map is produced showing how long it takes to reach
#' each location.
#'
#' Isochrones are only available from OTP v1.x and will not work with v2.0
#'
#' @export
otp_isochrone <- function(otpcon = NA,
                          fromPlace = NA,
                          fromID = NULL,
                          mode = "CAR",
                          date_time = Sys.time(),
                          arriveBy = FALSE,
                          maxWalkDistance = 1000,
                          routingOptions = NULL,
                          cutoffSec = c(600, 1200, 1800, 2400, 3000, 3600),
                          ncores = max(round(parallel::detectCores() * 1.25) - 1,1),
                          timezone = otpcon$timezone) {
  # Check for OTP2
  if (!is.null(otpcon$otp_version)) {
    if (otpcon$otp_version >= 2) {
      stop("Isochrones are not supported by OTP v2.X")
    }
  }

  # Check Valid Inputs
  checkmate::assert_numeric(ncores, lower = 1, len = 1, upper = max(c(round(parallel::detectCores() * 1.25 ) - 1,2)))
  checkmate::assert_class(otpcon, "otpconnect")
  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  mode <- toupper(mode)
  checkmate::assert_subset(mode,
                           choices = c(
                             "TRANSIT", "WALK", "BICYCLE",
                             "CAR", "BUS", "RAIL", "SUBWAY",
                             "TRAM", "FERRY","BICYCLE_RENT",
                             "BICYCLE_PARK","CAR_PARK","CABLE_CAR",
                             "GONDOLA","FUNICULAR","AIRPLANE"
                           ),
    empty.ok = FALSE
  )
  mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m-%d-%Y", tz = timezone)
  time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
  checkmate::assert_numeric(cutoffSec, lower = 0)
  checkmate::assert_logical(arriveBy)
  arriveBy <- tolower(as.character(arriveBy))

  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
  fromPlace <- paste0(fromPlace[,1],"%2C",fromPlace[,2])

  if (!is.null(fromID)) {
    if (length(fromID) != length(fromPlace)) {
      stop("The length of fromID and fromPlace are not the same")
    }
  }

  # if(!is.null(fromID)){
  #   fromID <- data.table::data.table(fromID = fromID,
  #                                    fromPlace = gsub("%2C",",",fromPlace))
  #   fromID <- unique(fromID)
  #   if(any(duplicated(fromID$fromID))){
  #     stop("Can't have two fromIDs with the same location, coordinates are rounded to 9 dp")
  #   }
  # }

  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/isochrone")

  query <- list(
    fromPlace = fromPlace,
    mode = mode,
    date = date,
    time = time,
    maxWalkDistance = maxWalkDistance,
    arriveBy = arriveBy
  )
  cutoffSec <- as.list(cutoffSec)
  names(cutoffSec) <- rep("cutoffSec", length(cutoffSec))
  query <- c(query, cutoffSec)

  if (!is.null(routingOptions)) {
    query <- c(query, routingOptions)
  }

  # Send Requests
  urls <- build_urls(routerUrl,fromPlace, toPlace = NULL, query)
  message(Sys.time()," sending ",length(urls)," isochrone requests using ",ncores," threads")
  results <- progressr::with_progress(otp_async(urls, ncores, TRUE))

  if(length(results) == 0){
    stop("No results returned, check your connection")
  }

  if (is.null(fromID)) {
    fromID <- gsub("%2C",",",fromPlace, fixed = TRUE)
  }

  results_sf <- purrr::map2(results, fromID, otp_process_results_iso)
  results_sf <- data.table::rbindlist(results_sf, use.names=TRUE)
  if(nrow(results_sf) > 0){
    results_sf <- sf::st_as_sf(results_sf)
  } else {
    warning("No results returned, check your inputs")
  }
  return(results_sf)
}

#' Process results
#'
#' @param text the text returned by OTP
#' @param fromID passed from main func
#' @family internal
#' @noRd

otp_process_results_iso <- function(text, fromID){

  response <- try(sf::st_read(text, quiet = TRUE), silent = TRUE)
  if(inherits(response, "try-error")){
    warning("Isochrone failed: ",text)
    return(NULL)
  }

  response$id <- seq(1, nrow(response))
  response$fromPlace <- fromID

  if (any(!sf::st_is_valid(response))) {
    suppressMessages(suppressWarnings(response <- sf::st_make_valid(response)))
  }


  response <- response[!sf::st_is_empty(response),]
  if(nrow(response) == 0){
    warning("Isochrone had empty geometry ")
    return(NULL)
  } else {
    response <- sf::st_cast(response, "MULTIPOLYGON")
  }


  return(response)

}
