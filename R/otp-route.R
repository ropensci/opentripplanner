#' Get the geometry of a route from the OTP
#'
#' THe old otp_plan now depeciated
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.529258,-0.134649)`
#' @param toPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.506383,-0.088780,)`
#' @param mode Character vector of modes of travel valid values TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR
#' @param date_time POSIXct, a date and time, defaults to current date and time
#' @param arriveBy Logical, Whether the trip should depart or arrive at the specified date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP
#' @param walkReluctance Numeric passed to OTP
#' @param transferPenalty Numeric passed to OTP
#' @param minTransferTime Numeric passed to OTP
#' @param numItineraries The maximum number of possible itineraries to return
#' @param full_elevation Logical, should the full elevation profile be returned, default FALSE
#'
#' @export
#'
#' @details
#' This function returns a SF data.frame with one row for each leg of the journey
#' (a leg is defined by a change in mode). For transit more than one route option may be returned
#' and is indicated by the route_option column.
#'
#'
#' Elevation
#' OTP supports elevation data, and can return the elevation profile of the route if available.
#' OTP returns the elevation profile separately from the XY coordinates, this means there is not
#' direct match between the number of XY points and the number of Z points.  OTP also only returns
#' the elevation profile for the first leg of the route (this appears to be a bug).
#' As default the otp_plan function matches the elevation profile to the XY coordinates to return
#' a SF linestring with XYZ coordinates. If you require a more detailed elevation profile,
#' the full_elevation parameter will return a nested data.frame with three columns.
#' first and second are returned from OTP, while distance is the cumulative distance along the
#' route and is derived from First.
#'
otp_plan_old <- function(otpcon = NA,
           fromPlace = NA,
           toPlace = NA,
           mode = "CAR",
           date_time = Sys.time(),
           arriveBy = FALSE,
           maxWalkDistance = 1000,
           walkReluctance = 2,
           transferPenalty = 0,
           minTransferTime = 0,
           numItineraries = 3,
           full_elevation = FALSE)
{
  message("This is the old version and is depreciated")
  # Check Valid Inputs
  checkmate::assert_class(otpcon,"otpconnect")
  checkmate::assert_numeric(fromPlace, lower =  -180, upper = 180, len = 2)
  fromPlace <- paste(fromPlace, collapse = ",")
  checkmate::assert_numeric(toPlace, lower =  -180, upper = 180, len = 2)
  toPlace <- paste(toPlace, collapse = ",")
  mode <- toupper(mode)
  checkmate::assert_subset(mode, choices = c("TRANSIT","WALK","BICYCLE","CAR","BUS","RAIL"), empty.ok = F)
  mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m-%d-%Y")
  time <- tolower(format(date_time, '%I:%M%p'))
  checkmate::assert_numeric(maxWalkDistance, lower = 0, len = 1)
  checkmate::assert_numeric(walkReluctance, lower = 0, len = 1)
  checkmate::assert_numeric(transferPenalty, lower = 0, len = 1)
  checkmate::assert_numeric(numItineraries, lower = 1, len = 1)
  #time <- tolower(time)

  if(arriveBy){
    arriveBy <- 'true'
  }else{
    arriveBy <- 'false'
  }

  # Construct URL
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl,"/plan")

  req <- httr::GET(
    routerUrl,
    query = list(
      fromPlace = fromPlace,
      toPlace = toPlace,
      mode = mode,
      date = date,
      time = time,
      maxWalkDistance = maxWalkDistance,
      walkReluctance = walkReluctance,
      arriveBy = arriveBy,
      transferPenalty = transferPenalty,
      minTransferTime = minTransferTime
    )
  )

  # convert response content into text
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  # parse text to json
  asjson <- jsonlite::fromJSON(text)

  # Check for errors - if no error object, continue to process content
  if(is.null(asjson$error$id)){
    response <- otp_json2sf(asjson, full_elevation)
    return(response)
  } else {
    # there is an error - return the error code and message
    response <-
      list("errorId" = asjson$error$id,
           "errorMessage" = asjson$error$msg)
    warning("A routing error has occured, returing error message")
    return(response)
  }


}


#' Get multiple routes from the OTP
#'
#' This fucntion is depreciated in favor of the new otp_plan which can do both single and batch routes
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric matrix of two columns, Latitude/Longitude pairs
#' @param toPlace Numeric matrix of two columns, Latitude/Longitude pairs
#' @param ncores numeric, number of cores to use in parallel processing, default is 1
#' @param ... Other variables passed to otp_plan
#' @return
#' Returns a data.frame of SF POLYLINES
#' @export
#'
#' @details
#' This function is a batch version of otp_plan() and is useful if you want to produce many routes at once.
#'
otp_plan_batch <- function(otpcon = NA,
                           fromPlace = NA,
                           toPlace = NA,
                           ncores = 1,
                           ...)
{
  warning("otp_plan_batch is depreciated, see otp_plan")
  # Check Valid Inputs
  if(!"otpconnect" %in% class(otpcon)){
    message("otpcon is not a valid otpconnect object")
    stop()
  }
  if("sf" %in% class(fromPlace)){
    if(all(sf::st_geometry_type(fromPlace) == "POINT")){
      fromPlace <- sf::st_coordinates(fromPlace)
      fromPlace <- fromPlace[,c(2,1)]
    }else{
      message("fromPlace contains non-POINT geometry")
      stop()
    }
  }

  if("sf" %in% class(toPlace)){
    if(all(sf::st_geometry_type(toPlace) == "POINT")){
      toPlace <- sf::st_coordinates(toPlace)
      toPlace <- toPlace[,c(2,1)]
    }else{
      message("toPlace contains non-POINT geometry")
      stop()
    }
  }

  if(class(fromPlace) != "matrix" | ncol(fromPlace) != 2){
    message("fromPlace is not a valid matrix of latitude, longitude pairs")
    stop()
  }else{
    if(max(fromPlace[,1]) <= 90 & min(fromPlace[,1]) >= -90 &  max(fromPlace[,2]) <= 180 & min(fromPlace[,2]) >= -180){

    }else{
      message("fromPlace coordinates excced valid values +/- 90 and +/- 180 degrees")
      stop()
    }

  }
  if(class(toPlace) != "matrix" | ncol(toPlace) != 2){
    message("toPlace is not a valid matrix of latitude, longitude pairs")
    stop()
  }else{
    if(max(toPlace[,1]) <= 90 & min(toPlace[,1]) >= -90 &  max(toPlace[,2]) <= 180 & min(toPlace[,2]) >= -180){

    }else{
      message("toPlace coordinates exceed valid values +/- 90 and +/- 180 degrees")
      stop()
    }

  }

  if(nrow(fromPlace) != nrow(fromPlace)){
    message("Number of fromPlaces and toPlaces do not match")
    stop()
  }

  get_resutls <- function(x,otpcon, fromPlace, toPlace, ...){
    res <- otp_plan_old(otpcon = otpcon,
                    fromPlace = fromPlace[x,],
                    toPlace = toPlace[x,],
                    ... )
    res$fromPlace = paste(fromPlace[x,],collapse = ",")
    res$toPlace = paste(toPlace[x,],collapse = ",")
    return(res)
  }

  if(ncores > 1){
    cl = parallel::makeCluster(ncores)
    parallel::clusterExport(cl = cl,
                            varlist = c("otpcon","fromPlace","toPlace"),
                            envir = environment())
    parallel::clusterEvalQ(cl, {
      library(opentripplanner)
    })
    pbapply::pboptions(use_lb=TRUE)
    results <- pbapply::pblapply(seq(1,nrow(fromPlace)),
                                 get_resutls,
                                 otpcon = otpcon,
                                 fromPlace = fromPlace,
                                 toPlace = toPlace,
                                 ... = ...,
                                 cl = cl
    )
    parallel::stopCluster(cl)
    rm(cl)

  }else{
    results <- pbapply::pblapply(seq(1,nrow(fromPlace)),
                                 get_resutls,
                                 otpcon = otpcon,
                                 fromPlace = fromPlace,
                                 toPlace = toPlace,
                                 ... = ...
    )
  }



  results_class <- sapply(results,function(x){"sf" %in% class(x)})
  if(all(results_class)){
    results_routes <- results[results_class]
    results_errors <- NA
  }else if(all(!results_class)){
    results_routes <- NA
    results_errors <- results[!results_class]
  }else{
    results_routes <- results[results_class]
    results_errors <- results[!results_class]
  }

  # Bind together
  suppressWarnings(results_routes <- dplyr::bind_rows(results_routes))
  results_routes <- as.data.frame(results_routes)
  results_routes$geometry <- sf::st_sfc(results_routes$geometry)
  results_routes <- sf::st_sf(results_routes)
  sf::st_crs(results_routes) <- 4326

  if(length(results_errors) > 1){
    message("Some errors occurred")
    # Simplify Error Message
    results_errors <- sapply(results_errors, function(x){paste0("Error: ",x$errorId," from ",x$fromPlace," to ",x$toPlace," ",x$errorMessage)})
    print(results_errors)
  }
  return(results_routes)
}

