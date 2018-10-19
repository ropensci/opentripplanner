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
otp_plan <- function(otpcon = NA,
           fromPlace = NA,
           toPlace = NA,
           mode = "CAR",
           date_time = Sys.time(),
           arriveBy = FALSE,
           maxWalkDistance = 800,
           walkReluctance = 2,
           transferPenalty = 0,
           minTransferTime = 0,
           full_elevation = FALSE)
{
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
  date <- format(date_time, "%m/%d/%Y")
  time <- format(date_time, '%I:%M:%S')
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


#' Convert Google Encoded Polyline and elevation data into sf object
#'
#' OTP returns the 2d route as a polyline bean and the elevation profile as vector of numbers
#' But the number of points for each is not the same, as 2D line only has a point at change of directions
#' While elevation is regally spaced. If elevation is supplied the correct heights are matched
#'
#' @param line character - polyline
#' @param elevation numeric - vector of elevations

polyline2linestring <- function(line, elevation = NULL){
  line <- gepaf::decodePolyline(line)
  line <- as.matrix(line[,2:1])
  if(exists("elevation")){
    # Some modes don't have elevation e.g TRANSIT, check for this
    if(all(is.na(elevation))){
      ele <- rep(0,nrow(line))
    }else{
      elevation <-elevation[order(elevation$distance),]
      # Calculate the length of each segment
      dist <-sapply(seq(1,nrow(line)-1),function(x){geosphere::distm(line[x,], line[x+1,], fun = geosphere::distHaversine)})
      dist <- cumsum(dist)
      vals <- findInterval(dist, elevation$distance)
      vals[vals == 0] = 1L
      ele <- elevation$second[c(1,vals)]
    }
    linestring3D <- cbind(line, ele)
    linestring3D <- sf::st_linestring(linestring3D, dim = "XYZ")
    return(linestring3D)
  }else{
    linestring <- sf::st_linestring(line)
    return(linestring)
  }

}

#' Correct the elevation distances
#'
#' OTP returns elevation as a distance along the leg, resetting to 0 at each leg
#' but we need the distance along the total route. so calculate this
#' @param dists numeric from the elevation first column

correct_distances <- function(dists){
  res <- list()
  rebase <- 0
  for(k in seq(1,length(dists))){
    if(k == 1){
      dists_k <- dists[k]
      res[[k]] <- dists_k
    }else{
      dists_k <- dists[k]
      res_km1 <- res[[k-1]]
      if(dists_k == 0){
        rebase <- rebase +  dists[k-1]
        res[[k]] <- dists_k + rebase
      }else{
        res[[k]] <- dists_k + rebase
      }
    }
    #message(paste0("k = ",k," original value = ",dists_k," rebase = ",rebase," new value = ",res[[k]]))
  }

  res <- unlist(res)
  return(res)
}



#' Convert output from Open Trip Planner into sf object
#'
#' @param obj Object from the OTP API to process
#' @param full_elevation logical should the full elevation profile be returned (if available)

otp_json2sf <- function(obj, full_elevation = FALSE) {
  requestParameters <- obj$requestParameters
  plan <- obj$plan
  debugOutput <- obj$debugOutput

  itineraries <- plan$itineraries

  itineraries$startTime <- as.POSIXct(itineraries$startTime / 1000 , origin = '1970-01-01', tz = "GMT")
  itineraries$endTime <- as.POSIXct(itineraries$endTime / 1000 , origin = '1970-01-01', tz = "GMT")


  legs <- list()
  #Loop over itineraries
  for(i in seq(1,nrow(itineraries))){
    leg <- itineraries$legs[[i]]
    # split into parts
    vars <- leg
    vars$from <- NULL
    vars$to <- NULL
    vars$steps <- NULL
    vars$legGeometry <- NULL

    # Extract geometry
    legGeometry <- leg$legGeometry$points

    # Check for Elevations
    steps <- leg$steps
    elevation <- lapply(seq(1,length(legGeometry)), function(x){leg$steps[[x]]$elevation})
    if(sum(lengths(elevation))>0){
      # We have Elevation Data
      # Extract the elevation values
      elevation <- lapply(seq(1,length(legGeometry)), function(x){dplyr::bind_rows(elevation[[x]])})
      elevation <- lapply(seq(1,length(legGeometry)), function(x){if(nrow(elevation[[x]]) == 0){NA}else{elevation[[x]] }})
      # the x coordinate of elevation reset at each leg, correct for this
      for(l in seq(1,length(elevation))){
        if(!all(is.na(elevation[[l]]))){
          elevation[[l]]$distance <- correct_distances(elevation[[l]]$first)
        }
      }
      # process the lines into sf objects
      lines <- list()
      for(j in seq(1,length(legGeometry))){
        lines[[j]] <- polyline2linestring(line = legGeometry[j], elevation = elevation[[j]])
      }
    }else{
      lines <- polyline2linestring(legGeometry)
    }

    lines <- sf::st_sfc(lines, crs = 4326)

    vars$geometry <- lines
    vars <- sf::st_sf(vars)
    vars$route_option <- i

    #Add full elevation if required
    if(full_elevation){
      vars$elevation <- elevation
    }

    #return to list
    legs[[i]] <- vars
  }

  legs <- legs[!is.na(legs)]
  suppressWarnings(legs <- dplyr::bind_rows(legs))
  #rebuild the sf object
  legs <- as.data.frame(legs)
  legs$geometry <- sf::st_sfc(legs$geometry)
  legs <- sf::st_sf(legs)
  sf::st_crs(legs) <- 4326

  legs$startTime <- as.POSIXct(legs$startTime / 1000 , origin = '1970-01-01', tz = "GMT")
  legs$endTime <- as.POSIXct(legs$endTime / 1000 , origin = '1970-01-01', tz = "GMT")

  itineraries$legs <- NULL
  itineraries <- itineraries[legs$route_option,]
  itineraries <- dplyr::bind_cols(itineraries,legs)

  itineraries <- sf::st_as_sf(itineraries)
  sf::st_crs(itineraries) <- 4326

  return(itineraries)
}
