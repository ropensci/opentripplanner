#' Get the geometry of a route from the OTP
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.529258,-0.134649)`,
#' or 2 column matrix of Latitude/Longitude pairs, or sf dataframe of POINTS
#' @param toPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.506383,-0.088780,)`,
#' or 2 column matrix of Latitude/Longitude pairs, or sf dataframe of POINTS
#' @param mode Character vector of modes of travel valid values TRANSIT, WALK,
#' BICYCLE, CAR, BUS, RAIL, default CAR
#' @param date_time POSIXct, a date and time, defaults to current date and time
#' @param arriveBy Logical, Whether the trip should depart or arrive at the specified
#' date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP
#' @param walkReluctance Numeric passed to OTP
#' @param transferPenalty Numeric passed to OTP
#' @param minTransferTime Numeric passed to OTP
#' @param numItineraries The maximum number of possible itineraries to return
#' @param full_elevation Logical, should the full elevation profile be returned, default FALSE
#' @param ncores Numeric, number of cores to use when batch processing, default 1
#'
#'
#' @details
#' This function returns a SF data.frame with one row for each leg of the journey
#' (a leg is defined by a change in mode). For transit more than one route option may be returned
#' and is indicated by the route_option column.
#'
#' ## Batch Routing
#'
#' When passing a matrix or SF dataframe to fromPlace and toPlace `otp_plan` will route in batch mode.
#' In this case the `ncores` variaible will be used. `ncores == 2` will likly yield a 35% performance boost
#' but `ncores > 3` will not yield a perfomance improvement on typical computers, as you will reduce the
#' amount of resouce aviaible for OTP.
#'
#' ## Elevation
#'
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
otp_plan2 <- function(otpcon = NA,
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
                     full_elevation = FALSE,
                     get_geometry = TRUE,
                     ncores = 1)
{
  # Check Valid Inputs
  checkmate::assert_class(otpcon,"otpconnect")
  mode <- toupper(mode)
  checkmate::assert_subset(mode, choices = c("TRANSIT","WALK","BICYCLE","CAR","BUS","RAIL"), empty.ok = F)
  mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m-%d-%Y")
  time <- tolower(format(date_time, '%I:%M%p'))
  checkmate::assert_numeric(maxWalkDistance, lower = 0, len = 1)
  checkmate::assert_numeric(walkReluctance,  lower = 0, len = 1)
  checkmate::assert_numeric(transferPenalty, lower = 0, len = 1)
  checkmate::assert_numeric(numItineraries,  lower = 1, len = 1)

  if(arriveBy){
    arriveBy <- 'true'
  }else{
    arriveBy <- 'false'
  }

  # Special checks for fromPlace and toPlace
  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  toPlace <- otp_clean_input(toPlace, "toPlace")

  # Make sure number of fromPlace or toPlace match
  nrfp <- nrow(fromPlace)
  nrtp <- nrow(toPlace)
  if(nrfp != nrtp){
    if(      nrfp > nrtp & nrtp == 1){
      nrtp <- nrtp[rep(1, times = nrfp),]
      warning("repeating toPlace to match length of fromPlace")
    }else if(nrtp > nrfp & nrfp == 1){
      nrfp <- nrfp[rep(1, times = nrtp),]
      warning("repeating fromPlace to match length of toPlace")
    }else{
      stop("Number of fromPlaces and toPlaces do not match")
    }
  }


  get_resutls <- function(x,otpcon, fromPlace, toPlace, ...){
    res <- otp_plan(otpcon = otpcon,
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

#' Clean Batch Inputs
#'
#' Clean numeric, SF, or matrix prior to routing
#'
#' @param imp fromPlace or toPlace input
#' @param imp_name name of input
#'
otp_clean_input <- function(imp, imp_name){
  # For single point inputs
  if(all(class(imp) == "numeric")){
    checkmate::assert_numeric(imp, len = 2)
    imp <- matrix(imp, nrow = 1, byrow = T)
  }
  # For SF inputs
  if("sf" %in% class(imp)){
    if(all(sf::st_geometry_type(imp) == "POINT")){
      imp <- sf::st_coordinates(imp)
      imp <- imp[,c(2,1)]
    }else{
      stop(paste0(imp_name," contains non-POINT geometry"))
    }
  }
  # For matrix inputs
  if(all(class(imp) == "matrix")){
    checkmate::assert_matrix(imp, any.missing = F, min.rows = 1, min.cols = 2, max.cols = 2, null.ok = F)
    checkmate::assert_numeric(imp[,1], lower =  -90, upper = 90)
    checkmate::assert_numeric(imp[,2], lower =  -180, upper = 180)
    return(imp)
  }
  # Otherwise stop as invalid input
  stop(paste0(imp_name," is not in a valid fromat of either; numeric of length 2; matrix of latitude / longitude pairs; or sf dataframe of POINTS"))
}


#' Get the geometry of a route from the OTP
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
#'
#' @details
#' This function returns a SF data.frame with one row for each leg of the journey
#' (a leg is defined by a change in mode). For transit more than one route option may be returned
#' and is indicated by the route_option column.
#'
#'
#'
otp_plan_internal <- function(otpcon = NA,
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
