#' Get travel times between points
#'
#'
#' @description This function requires OTP 1.x and the analyst
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Longitude/Latitude pair, e.g.
#'   `c(-0.134649,51.529258)`, or 2 column matrix of Longitude/Latitude pairs,
#'   or sf data frame of POINTS with CRS 4326
#' @param toPlace Numeric vector, Longitude/Latitude pair, e.g.
#'   `c(-0.088780,51.506383)`, or 2 column matrix of Longitude/Latitude pairs,
#'   or sf data frame of POINTS with CRS 4326
#' @param fromID character vector same length as fromPlace
#' @param toID character vector same length as toPlace
#' @param mode character vector of one or more modes of travel valid values
#'   TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR. Not all combinations
#'   are valid e.g. c("WALK","BUS") is valid but c("WALK","CAR") is not.
#' @param date_time POSIXct, a date and time, defaults to current date and time
#' @param arriveBy Logical, Whether the trip should depart or arrive at the
#'   specified date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP in metres
#' @param routeOptions Named list of values passed to OTP use
#'   `otp_route_options()` to make template object.
#' @param numItineraries The maximum number of possible itineraries to return
#' @param ncores Numeric, number of cores to use when batch processing, default
#'   1, see details
#' @param timezone Character, what timezone to use, see as.POSIXct, default is
#'   local timezone
#'
#' @export
#' @family analyst
#' @return Returns an  data frame
#'
#' @details
#'
#' Make a travel time matrix using the analyst features in OPT 1.x
#' @examples
#' \dontrun{
#' }
#'
otp_traveltime <- function(otpcon = NA,
                           path_data = NULL,
                     fromPlace = NA,
                     toPlace = NA,
                     fromID = NULL,
                     toID = NULL,
                     mode = "CAR",
                     date_time = Sys.time(),
                     arriveBy = FALSE,
                     maxWalkDistance = 1000,
                     numItineraries = 3,
                     routeOptions = NULL,
                     ncores = 1,
                     timezone = otpcon$timezone) {
  # Check Valid Inputs

  # Back compatibility with 0.2.1
  if (is.null(timezone)) {
    warning("otpcon is missing the timezone variaible, assuming local timezone")
    timezone <- Sys.timezone()
  }

  # Back compatibility with RcppSimdJson <= 0.1.1
  RcppSimdJsonVersion <- try(utils::packageVersion("RcppSimdJson") >= "0.1.2", silent = TRUE)
  if (class(RcppSimdJsonVersion) == "try-error") {
    RcppSimdJsonVersion <- FALSE
  }

  if (!RcppSimdJsonVersion) {
    message("NOTE: You do not have 'RcppSimdJson' >= 0.1.2 installed")
    stop("This feature is not supported")
  }

  checkmate::assert_subset(timezone, choices = OlsonNames(tzdir = NULL))

  checkmate::assert_class(otpcon, "otpconnect")
  mode <- toupper(mode)
  checkmate::assert_subset(mode,
                           choices = c(
                             "TRANSIT", "WALK", "BICYCLE",
                             "CAR", "BUS", "RAIL"
                           ),
                           empty.ok = FALSE
  )
  #checkmate::assert_character(mode, len = 1)
  #mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m-%d-%Y", tz = timezone)
  time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
  checkmate::assert_numeric(maxWalkDistance, lower = 0, len = 1)
  checkmate::assert_numeric(numItineraries, lower = 1, len = 1)
  checkmate::assert_character(fromID, null.ok = FALSE)
  checkmate::assert_character(toID, null.ok = FALSE)
  checkmate::assert_logical(arriveBy)
  #arriveBy <- tolower(arriveBy)

  # Check Route Options
  if (!is.null(routeOptions)) {
    routeOptions <- otp_validate_routing_options(routeOptions)
  }

  # Special checks for fromPlace and toPlace
  fromPlace <- otp_clean_input(fromPlace, "fromPlace")

  if (!is.null(fromID)) {
    if (length(fromID) != nrow(fromPlace)) {
      stop("The length of fromID and fromPlace are not the same")
    }
  }

  if (!is.null(toID)) {
    if (length(toID) != nrow(toPlace)) {
      stop("The length of toID and toPlace are not the same")
    }
  }


  # Make a pointset for each fromPLACE
  toPlace <- sf::st_sf(data.frame(geometry = sf::st_geometry(toPlace)))
  pointsetname <- paste(sample(LETTERS, 6, TRUE), collapse = "")
  otp_pointset(toPlace, pointsetname, path_data)

  fromPlacelst <- split(fromPlace[,2:1], 1:nrow(fromPlace))

  if(ncores > 1){
    cl <- parallel::makeCluster(ncores, outfile = "otp_parallel_log.txt")
    parallel::clusterExport(
      cl = cl,
      varlist = c("otpcon", "pointsetname"),
      envir = environment()
    )
    parallel::clusterEvalQ(cl, {
      loadNamespace("opentripplanner")
    })
    pbapply::pboptions(use_lb = TRUE)
    res <- pbapply::pblapply(fromPlacelst,
                             otp_traveltime_internal,
                             otpcon = otpcon,
                             pointsetname = pointsetname,
                             mode = mode,
                             date_time = date_time,
                             arriveBy = arriveBy,
                             maxWalkDistance = maxWalkDistance,
                             routeOptions = routeOptions,
                             cl = cl)
    parallel::stopCluster(cl)
    rm(cl)
  } else {
    res <- pbapply::pblapply(fromPlacelst,
                             otp_traveltime_internal,
                             otpcon = otpcon,
                             pointsetname = pointsetname,
                             mode = mode,
                             date_time = date_time,
                             arriveBy = arriveBy,
                             maxWalkDistance = maxWalkDistance,
                             routeOptions = routeOptions)
  }

  names(res) <- fromID
  res <- res[lengths(res) > 0]
  res <- list2df(res)
  rownames(res) <- toID
  return(res)
}


otp_traveltime_internal <- function(fromPlace,
                                    otpcon,
                                    pointsetname,
                                    mode,
                                    date_time,
                                    arriveBy,
                                    maxWalkDistance,
                                    routeOptions){
  surface <- try(otp_make_surface(otpcon = otpcon,
                              fromPlace = fromPlace,
                              mode = mode,
                              date_time = date_time,
                              arriveBy = arriveBy,
                              maxWalkDistance = maxWalkDistance,
                              routeOptions = routeOptions), silent = TRUE)

  if ("try-error" %in% class(surface)) {
    warning("Failed to create surface for: ",paste(fromPlace, collapse = ", "))
    return(NULL)
  }

  times <- try(otp_surface(otpcon, surface, pointsetname, get_data = FALSE),
               silent = TRUE)

  if ("try-error" %in% class(times)) {
    warning("Failed to evaluate surface for: ",paste(fromPlace, collapse = ", "))
    return(NULL)
  }

  return(times$times)
}
