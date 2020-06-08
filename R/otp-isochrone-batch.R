#' Get the Isochrones from a location
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Longitude/Latitude pair,
#'     e.g. `c(-0.134649,51.529258)`,
#' or 2 column matrix of Longitude/Latitude pairs, or sf
#'     data frame of POINTS
#' @param fromID character vector same length as fromPlace
#' @param mode character vector of one or more modes of travel valid values
#'     TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR. Not all
#'     combinations are valid e.g. c("WALK","BUS") is valid but
#'     c("WALK","CAR") is not.
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
#' @export
otp_isochrone <- function(otpcon = NA,
                          fromPlace = NA,
                          fromID = NULL,
                          mode = "TRANSIT",
                          date_time = Sys.time(),
                          arriveBy = FALSE,
                          maxWalkDistance = 1000,
                          routingOptions = NULL,
                          cutoffSec = c(600, 1200, 1800, 2400, 3000, 3600),
                          ncores = 1,
                          timezone = otpcon$timezone) {
  # Check Valid Inputs
  checkmate::assert_class(otpcon, "otpconnect")
  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  mode <- toupper(mode)
  checkmate::assert_subset(mode,
    choices = c(
      "TRANSIT", "WALK", "BICYCLE",
      "CAR", "BUS", "RAIL"
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

  if (!is.null(fromID)) {
    if (length(fromID) != nrow(fromPlace)) {
      stop("The length of fromID and fromPlace are not the same")
    }
  }

  if (ncores > 1) {
    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(
      cl = cl,
      varlist = c("otpcon", "fromPlace", "fromID"),
      envir = environment()
    )
    parallel::clusterEvalQ(cl, {
      loadNamespace("opentripplanner")
    })
    pbapply::pboptions(use_lb = TRUE)
    results <- pbapply::pblapply(seq(1, nrow(fromPlace)),
                                 otp_get_isochrone_results,
                                 otpcon = otpcon,
                                 fromPlace = fromPlace,
                                 fromID = fromID,
                                 mode = mode,
                                 date = date,
                                 time = time,
                                 arriveBy = arriveBy,
                                 maxWalkDistance = maxWalkDistance,
                                 routingOptions = routingOptions,
                                 cutoffSec = cutoffSec,
                                 cl = cl
    )
    parallel::stopCluster(cl)
    rm(cl)
  } else {
    results <- pbapply::pblapply(seq(1, nrow(fromPlace)),
                                 otp_get_isochrone_results,
                                 otpcon = otpcon,
                                 fromPlace = fromPlace,
                                 fromID = fromID,
                                 mode = mode,
                                 date = date,
                                 time = time,
                                 arriveBy = arriveBy,
                                 maxWalkDistance = maxWalkDistance,
                                 routingOptions = routingOptions,
                                 cutoffSec = cutoffSec
    )
  }



  results_class <- unlist(lapply(results, function(x) {
    "data.frame" %in% class(x)
  }))
  if (all(results_class)) {
    results_routes <- results[results_class]
    results_errors <- NA
  } else if (all(!results_class)) {
    results_routes <- NA
    results_errors <- results[!results_class]
  } else {
    results_routes <- results[results_class]
    results_errors <- results[!results_class]
  }

  # Bind together
  if (!all(class(results_routes) == "logical")) {
    results_routes <- dplyr::bind_rows(results_routes)
    # if (any(unlist(lapply(results, function(x) {
    #   "sf" %in% class(x)
    # })))) {
    #   suppressWarnings(results_routes <- dplyr::bind_rows(results_routes))
    #   results_routes <- as.data.frame(results_routes)
    #   results_routes$geometry <- sf::st_sfc(results_routes$geometry)
    #   results_routes <- sf::st_sf(results_routes)
    #   sf::st_crs(results_routes) <- 4326
    # } else {
    #   results_routes <- dplyr::bind_rows(results_routes)
    # }
  }


  if (!all(class(results_errors) == "logical")) {
    results_errors <- unlist(results_errors)
    warning(results_errors)
  }
  return(results_routes)

}

#' Get isochrone results
#'
#' helper function for otp_plan
#'
#' @param x numeric
#' @param otpcon otpcon
#' @param fromPlace fromplace
#' @param fromID fromID
#' @param ... all other variaibles
#'
#' @noRd
otp_get_isochrone_results <- function(x, otpcon, fromPlace, fromID, ...) {
  res <- otp_isochrone_internal(
    otpcon = otpcon,
    fromPlace = fromPlace[x, ],
    fromID = fromID[x],
    ...
  )
  return(res)
}


#' Internal fucntion for getting isochrones
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Longitude/Latitude pair,
#'     e.g. `c(-0.134649, 51.529258,)`
#' @param mode character vector of one or more modes of travel valid values
#'     TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR. Not all
#'     combinations are valid e.g. c("WALK","BUS") is valid but
#'     c("WALK","CAR") is not.
#' @param date_time POSIXct, a date and time, defaults to current
#'     date and time
#' @param arriveBy Logical, Whether the trip should depart or
#'     arrive at the specified date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP in metres
#' @param routingOptions Names list passed to OTP
#' @param cutoffSec Numeric vector, number of seconds to define
#'     the break points of each Isochrone
#' @family internal
#' @noRd

otp_isochrone_internal <- function(otpcon = NA,
                           fromPlace = NA,
                           fromID = NULL,
                           mode = NULL,
                           date = NULL,
                           time = NULL,
                           arriveBy = NULL,
                           maxWalkDistance = NULL,
                           routingOptions = NULL,
                           cutoffSec = NULL) {

  # Construct URL
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/isochrone")

  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
  fromPlace <- paste(fromPlace, collapse = ",")

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

  if(!is.null(routingOptions)){
    query <- c(query, routingOptions)
  }

  req <- httr::GET(
    routerUrl,
    query = query
  )

  # convert response content into text
  text <- httr::content(req, as = "text", encoding = "UTF-8")

  if (nchar(text) < 200) {
    return(paste0("Failed to get isochrone with error: ",text))
  } else {
    # parse to sf
    response <- sf::st_read(text, quiet = TRUE)
    response$id <- seq(1, nrow(response))
    if(any(!sf::st_is_valid(response))){
      suppressMessages(suppressWarnings(response <- sf::st_buffer(response, 0)))
    }

    if(!is.null(fromID)){
      response$fromPlace <- fromID
    } else {
      response$fromPlace <- fromPlace
    }

    return(response)
  }
}


