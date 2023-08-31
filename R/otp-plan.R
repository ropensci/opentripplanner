#' Get get a route or routes from the OTP
#'
#' @description This is the main routing function for OTP and can find single or
#'   multiple routes between `fromPlace` and `toPlace`.
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
#'   "TRANSIT","BUS", "RAIL", "SUBWAY","TRAM", "FERRY", "GONDOLA", "FUNICULAR",
#'   "AIRPLANE", "CABLE_CAR", "WALK", "BICYCLE", "BICYCLE_RENT", "BICYCLE_PARK",
#'   "CAR", "CAR_PARK", "CAR_HAIL", "CARPOOL", "CAR_DROPOFF", "CAR_PICKUP",
#'   default "CAR". Not all combinations are valid e.g. c("WALK","BUS") is valid but
#'   c("WALK","CAR") is not.
#' @param date_time POSIXct, a date and time, defaults to current date and time
#' @param arriveBy Logical, Whether the trip should depart or arrive at the
#'   specified date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP in meters
#' @param routeOptions Named list of values passed to OTP use
#'   `otp_route_options()` to make template object.
#' @param numItineraries The maximum number of possible itineraries to return
#' @param full_elevation Logical, should the full elevation profile be returned,
#'   default FALSE
#' @param ncores Numeric, number of cores to use when batch processing, default
#'   1, see details
#' @param get_geometry Logical, should the route geometry be returned, default
#'   TRUE, see details
#' @param timezone Character, what timezone to use, see as.POSIXct, default is
#'   local timezone
#' @param distance_balance Logical, use distance balancing, default false, see
#'   details
#' @param get_elevation Logical, default FALSE, if true XYZ coordinates returned
#'   else XY coordinates returned.
#'
#' @export
#' @family routing
#' @return Returns an SF data frame of LINESTRINGs
#'
#' @details This function returns a SF data.frame with one row for each leg of
#'   the journey (a leg is defined by a change in mode). For transit, more than
#'   one route option may be returned and is indicated by the `route_option`
#'   column. The number of different itineraries can be set with the
#'   `numItineraries` variable.
#'
#'   ## Batch Routing
#'
#'   When passing a matrix or SF data frame object to fromPlace and toPlace
#'   `otp_plan` will route in batch mode. In this case the `ncores` variable
#'   will be used. Increasing `ncores` will enable multicore routing, the max
#'   `ncores` should be 1.25 times the number of cores on your system. The
#'   default is 1.25 timees -1 for improved stability.
#'
#'   ## Distance Balancing
#'
#'   When using multicore routing each task does not take the same amount of
#'   time. This can result in wasted time between batches. Distance Balancing
#'   sorts the routing by the euclidean distance between fromPlace and toPlace
#'   before routing. This offers a small performance improvement of around five
#'   percent. As the original order of the inputs is lost fromID and toID must
#'   be provided.
#'
#'   ## Elevation
#'
#'   OTP supports elevation data and can return the elevation profile of the
#'   route if available. OTP returns the elevation profile separately from the
#'   XY coordinates, this means there is not direct match between the number of
#'   XY points and the number of Z points.  OTP also only returns the elevation
#'   profile for the first leg of the route (this appears to be a bug). If
#'   `get_elevation` is TRUE the otp_plan function matches the elevation profile
#'   to the XY coordinates to return an SF linestring with XYZ coordinates. If
#'   you require a more detailed elevation profile, the full_elevation parameter
#'   will return a nested data.frame with three columns. first and second are
#'   returned from OTP, while distance is the cumulative distance along the
#'   route and is derived from First.
#'
#'   ## Route Geometry
#'
#'   The `get_geometry` provides the option to not return the route geometry,
#'   and just return the meta-data (e.g. journey time). This may be useful when
#'   creating an Origin:Destination matrix and also provides a small performance
#'   boost by reduced processing of geometries.
#' @examples
#' \dontrun{
#' otpcon <- otp_connect()
#' otp_plan(otpcon, c(0.1, 55.3), c(0.6, 52.1))
#' otp_plan(otpcon, c(0.1, 55.3), c(0.6, 52.1),
#'   mode = c("WALK", "TRANSIT")
#' )
#' otp_plan(otpcon, c(0.1, 55.3), c(0.6, 52.1),
#'   mode = "BICYCLE", arriveBy = TRUE,
#'   date_time = as.POSIXct(strptime("2018-06-03 13:30", "%Y-%m-%d %H:%M"))
#' )
#' }
#'
otp_plan <- function(otpcon = NA,
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
                     full_elevation = FALSE,
                     get_geometry = TRUE,
                     ncores = max(round(parallel::detectCores() * 1.25) - 1,1),
                     timezone = otpcon$timezone,
                     distance_balance = FALSE,
                     get_elevation = FALSE) {
  # Check Valid Inputs

  # Back compatibility with 0.2.1
  if (is.null(timezone)) {
    warning("otpcon is missing the timezone variaible, assuming local timezone")
    timezone <- Sys.timezone()
  }

  checkmate::assert_subset(timezone, choices = OlsonNames(tzdir = NULL))

  checkmate::assert_class(otpcon, "otpconnect")
  mode <- toupper(mode)
  checkmate::assert_subset(mode,
                           choices = c(
                             "TRANSIT","BUS", "RAIL", "SUBWAY","TRAM", "FERRY",
                             "GONDOLA","FUNICULAR","AIRPLANE","CABLE_CAR",
                             "WALK",
                             "BICYCLE","BICYCLE_RENT","BICYCLE_PARK",
                             "CAR","CAR_PARK",
                             "CAR_HAIL","CARPOOL","CAR_DROPOFF","CAR_PICKUP"
                           ),
                           empty.ok = FALSE
  )
  mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m-%d-%Y", tz = timezone)
  time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
  checkmate::assert_numeric(maxWalkDistance, lower = 0, len = 1)
  checkmate::assert_numeric(numItineraries, lower = 1, len = 1)
  checkmate::assert_numeric(ncores, lower = 1, len = 1, upper = max(c(round(parallel::detectCores() * 1.25 ) - 1,2)))
  checkmate::assert_character(fromID, null.ok = TRUE)
  checkmate::assert_character(toID, null.ok = TRUE)
  checkmate::assert_logical(arriveBy)
  arriveBy <- tolower(arriveBy)
  checkmate::assert_logical(distance_balance, len = 1, null.ok = FALSE)
  checkmate::assert_logical(get_elevation, len = 1, null.ok = FALSE)

  if (distance_balance & (ncores > 1)) {
    if (is.null(fromID)) {
      stop("Distance balancing changes the order of the output, so fromID must not be NULL")
    }
    if (is.null(toID)) {
      stop("Distance balancing changes the order of the output, so toID must not be NULL")
    }
  }


  # Check Route Options
  if (!is.null(routeOptions)) {
    routeOptions <- otp_validate_routing_options(routeOptions)
  }

  # Special checks for fromPlace and toPlace
  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  toPlace <- otp_clean_input(toPlace, "toPlace")

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

  # Make sure number of fromPlace or toPlace match
  nrfp <- nrow(fromPlace)
  nrtp <- nrow(toPlace)
  if (nrfp != nrtp) {
    if (nrfp > nrtp & nrtp == 1) {
      toPlace <- toPlace[rep(1, times = nrfp), ]
      if (!is.null(toID)) {
        toID <- toID[rep(1, times = nrfp)]
      }
      message("repeating toPlace to match length of fromPlace")
    } else if (nrtp > nrfp & nrfp == 1) {
      fromPlace <- fromPlace[rep(1, times = nrtp), ]
      if (!is.null(fromID)) {
        fromID <- fromID[rep(1, times = nrtp)]
      }
      message("repeating fromPlace to match length of toPlace")
    } else {
      stop("Number of fromPlaces and toPlaces do not match")
    }
  }

  if (distance_balance & (ncores > 1)) {
    dists <- geodist::geodist(fromPlace, toPlace, paired = TRUE)

    # Remove 0m pairs as OTP will fail on them anyway
    dists_0 <- dists != 0
    fromPlace <- fromPlace[dists_0, ]
    toPlace <- toPlace[dists_0, ]
    fromID <- fromID[dists_0]
    toID <- toID[dists_0]
    dists <- dists[dists_0]

    dists <- order(dists, decreasing = TRUE)
    fromPlace <- fromPlace[dists, ]
    toPlace <- toPlace[dists, ]
    fromID <- fromID[dists]
    toID <- toID[dists]
  }

  # Build URLs
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/plan")

  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
  toPlace <- format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)

  fromPlace <- paste0(fromPlace[,1],"%2C",fromPlace[,2])
  toPlace <- paste0(toPlace[,1],"%2C",toPlace[,2])

  query <- list(
    mode = mode,
    date = date,
    time = time,
    maxWalkDistance = maxWalkDistance,
    arriveBy = arriveBy,
    numItineraries = numItineraries
  )

  if (otpcon$otp_version >= 2) {
    # maxWalkDistance causes itinaries to fail
    if (mode == "CAR" | grepl("TRANSIT", mode)) {
      query$maxWalkDistance <- NULL
    }
  }

  if (!is.null(routeOptions)) {
    query <- c(query, routeOptions)
  }

  # if(!is.null(fromID)){
  #   fromID <- data.table::data.table(fromID = fromID,
  #                                    fromPlace = gsub("%2C",",",fromPlace))
  #   fromID <- unique(fromID)
  #   if(any(duplicated(fromID$fromID))){
  #     stop("Can't have two fromIDs with the same location, coordinates are rounded to 9 dp")
  #   }
  # }
  # if(!is.null(toID)){
  #   toID <- data.table::data.table(toID = toID,
  #                                    toPlace = gsub("%2C",",",toPlace))
  #   toID <- unique(toID)
  #   if(any(duplicated(toID$toID))){
  #     stop("Can't have two toIDs with the same location, coordinates are rounded to 9 dp")
  #   }
  # }

  # Send Requests
  urls <- build_urls(routerUrl,fromPlace, toPlace, query)
  message(Sys.time()," sending ",length(urls)," routes requests using ",ncores," threads")
  progressr::handlers("cli")
  results <- progressr::with_progress(otp_async(urls, ncores))


  message(Sys.time()," processing results")
  results_routes <- RcppSimdJson::fparse(results,
                                         query = "/plan/itineraries",
                                         query_error_ok = TRUE,
                                         parse_error_ok = TRUE,
                                         always_list = TRUE)


  if(is.null(fromID)){
    fromID <- unlist(RcppSimdJson::fparse(results,
                                          query = "/requestParameters/fromPlace",
                                          query_error_ok = TRUE,
                                          parse_error_ok = TRUE), use.names = FALSE)
  }

  if(is.null(toID)){
    toID <- unlist(RcppSimdJson::fparse(results,
                                        query = "/requestParameters/fromPlace",
                                        parse_error_ok = TRUE,
                                        query_error_ok = TRUE), use.names = FALSE)

  }




  results_errors <- RcppSimdJson::fparse(results, query = "/error",
                                         query_error_ok = TRUE,
                                         parse_error_ok = TRUE,
                                         always_list = TRUE)
  results_errors <- results_errors[lengths(results_errors) > 0]


  if(sum(lengths(results_routes)) == 0){
    stop("No results returned, check your connection")
  }

  results_routes <- purrr::pmap(.l = list(itineraries = results_routes,
                                          fp = fromID,
                                          tp = toID
                                          ),
                                .f = otp_json2sf,
                                full_elevation = full_elevation,
                                get_geometry = get_geometry,
                                timezone = timezone,
                                get_elevation = get_elevation,
                                .progress = TRUE)


  results_routes <- data.table::rbindlist(results_routes, fill = TRUE, use.names=TRUE)

  origin <- .POSIXct(0, tz = "GMT")
  results_routes$startTime <- as.POSIXct.numeric(results_routes$startTime / 1000,
                                                 origin = origin, tz = timezone
  )

  results_routes$endTime <- as.POSIXct.numeric(results_routes$endTime / 1000,
                                               origin = origin, tz = timezone
  )

  results_routes$leg_startTime <- as.POSIXct.numeric(results_routes$leg_startTime / 1000,
                                                     origin = origin, tz = timezone
  )

  results_routes$leg_endTime <- as.POSIXct.numeric(results_routes$leg_endTime / 1000,
                                                   origin = origin, tz = timezone
  )

  # fix for bbox error from data.table
  results_routes <- results_routes[seq_len(nrow(results_routes)), ]
  results_routes <- as.data.frame(results_routes)

  # if(!is.null(fromID)){
  #   results_routes$fromPlace <- fromID$fromID[match(results_routes$fromPlace, fromID$fromPlace)]
  # }
  # if(!is.null(toID)){
  #   results_routes$toPlace <- toID$toID[match(results_routes$toPlace, toID$toPlace)]
  # }

  if(get_geometry){
    results_routes$geometry <- sf::st_as_sfc(results_routes$geometry, crs = 4326)
    results_routes <- df2sf(results_routes)
    colnms <- names(results_routes)
    colnms <- colnms[!colnms %in% c("fromPlace", "toPlace", "geometry")]
    results_routes <- results_routes[c("fromPlace", "toPlace", colnms, "geometry")]

  } else {
    colnms <- names(results_routes)
    colnms <- colnms[!colnms %in% c("fromPlace", "toPlace")]
    results_routes <- results_routes[c("fromPlace", "toPlace", colnms)]
  }

  if(length(results_errors) > 0){
    results_errors = purrr::map(results_errors, otp_parse_errors)
    results_errors = data.table::rbindlist(results_errors, use.names = FALSE)
    message(nrow(results_errors)," routes returned errors. Unique error messages are:\n")
    results_errors = as.data.frame(table(results_errors$msg))
    results_errors = results_errors[order(results_errors$Freq, decreasing = TRUE),]
    for(msgs in seq_len(nrow(results_errors))){
      message(results_errors$Freq[msgs],'x messages: "',results_errors$Var1[msgs],'"\n')
    }

  }

  message(Sys.time()," done")
  return(results_routes)
}

#' Parse Errors
#' @param x list
#' @family internal
#' @noRd
otp_parse_errors <- function(x){

    data.frame(id = x$id,
      msg = x$msg
    )

}


#' Parse Missing
#' @param x list
#' @family internal
#' @noRd
otp_parse_missing <- function(x){

  data.frame(id = 0,
             from = x$requestParameters$fromPlace,
             to = x$requestParameters$toPlace,
             msg = "No result was returned"
  )

}



#' Clean Batch Inputs
#'
#' Clean numeric, SF, or matrix prior to routing
#'
#' @param imp fromPlace or toPlace input
#' @param imp_name name of input
#' @family internal
#' @noRd

otp_clean_input <- function(imp, imp_name) {
  # For single point inputs
  if (all(class(imp) == "numeric")) {
    checkmate::assert_numeric(imp, len = 2)
    imp <- matrix(imp, nrow = 1, byrow = TRUE)
  }
  # For SF inputs
  if ("sf" %in% class(imp)) {
    if (all(sf::st_geometry_type(imp) == "POINT")) {
      imp <- sf::st_coordinates(imp)
      imp[] <- imp[, c(1, 2)]
    } else {
      stop(paste0(imp_name, " contains non-POINT geometry"))
    }
  }

  # For matrix inputs
  # if (all(class(imp) == "matrix")) { # to pass CRAN checks
  if ("matrix" %in% class(imp)) {
    checkmate::assert_matrix(imp,
                             any.missing = FALSE,
                             min.rows = 1,
                             min.cols = 2,
                             max.cols = 2,
                             null.ok = FALSE
    )
    checkmate::assert_numeric(imp[, 1],
                              lower = -180, upper = 180,
                              any.missing = FALSE, .var.name = paste0(imp_name, " Longitude")
    )
    checkmate::assert_numeric(imp[, 2],
                              lower = -90, upper = 90,
                              any.missing = FALSE, .var.name = paste0(imp_name, " Latitude")
    )
    imp[] <- imp[, 2:1] # Switch round lng/lat to lat/lng for OTP
    colnames(imp) <- c("lat", "lon")
    return(imp)
  }
  # Otherwise stop as invalid input
  stop(paste0(
    imp_name,
    " is not in a valid format ",
    paste(class(imp), collapse = ", ")
  ))
}


#' Make OTP requests
#'
#' @param routerUrl base url
#' @param fromPlace character fromPlace
#' @param toPlace character toPlace
#' @param query list of query parameters
#' @family internal
#' @noRd
build_urls <- function (routerUrl,fromPlace, toPlace, query){
  secs <- unlist(query, use.names = TRUE)
  secs <- paste0(names(secs), "=", secs)
  secs <- paste(secs, collapse = "&")
  secs <- gsub(",", "%2C", secs)
  if(is.null(toPlace)){
    secs <- paste0(routerUrl, "?", "fromPlace=",fromPlace,"&",secs)
  } else {
    secs <- paste0(routerUrl, "?", "fromPlace=",fromPlace,"&toPlace=",toPlace,"&",secs)
  }
  secs
}

#' Async Send  Requests
#'
#' @param urls vector of URLs for OTP
#' @param ncores Number of requests to send at once
#' @param iso_mode logical, use isochrone mode
#' @param post logical, make a post request
#' @family internal
#' @noRd
otp_async <- function(urls, ncores, iso_mode = FALSE, post = FALSE){

  t1 <- Sys.time()
  p <- progressr::progressor(length(urls))
  out <- vector('list', length(urls))
  pool <- curl::new_pool(host_con = ncores)
  lapply( seq_along(urls), function(i){
    h <- curl::new_handle()
    if(post){
      curl::handle_setopt(h, post = TRUE)
    }
    if(iso_mode){
      h <- curl::handle_setheaders(h, "Accept" = "application/json")
    }
    success <- function(res){
      p()
      out[[i]] <<- rawToChar(res$content)
    }
    failure <- function(res){
      p()
      cat("Error: ", res, "\n")
      out[[i]] <<- paste0("Error: ", res)
    }
    curl::curl_fetch_multi(urls[i],
                           done = success,
                           fail = failure,
                           pool = pool,
                           handle = h)
  })
  curl::multi_run(timeout = Inf, pool = pool)
  t2 <- Sys.time()
  message("Done in ",round(difftime(t2,t1, units = "mins"),1)," mins")
  return(unlist(out, use.names = FALSE))
}

#' Convert output from OpenTripPlanner into sf object
#'
#' @param itineraries Object from the OTP API to process
#' @param fp fromPlace
#' @param tp toPlace
#' @param full_elevation logical should the full elevation profile be returned (if available)
#' @param get_geometry logical, should geometry be returned
#' @param timezone character, which timezone to use, default "" means local time
#' @param get_elevation, logical, should xyz coordinate be returned
#' @family internal
#' @noRd
otp_json2sf <- function(itineraries, fp, tp,
                        full_elevation = FALSE,
                        get_geometry = TRUE,
                        timezone = "", get_elevation = FALSE) {

  if(is.null(itineraries)){
    return(NULL)
  }

  # Loop over itineraries
  legs <- purrr::map(itineraries$legs, parse_leg,
                     get_geometry = get_geometry,
                     get_elevation = get_elevation,
                     full_elevation = full_elevation
  )

  names(legs) <- seq_len(length(legs))
  legs <- legs[!is.na(legs)]
  legs <- data.table::rbindlist(legs, fill = TRUE, idcol = "route_option", use.names=TRUE)
  names(legs) <- paste0("leg_",names(legs))
  names(legs)[names(legs) == "leg_route_option"] <- "route_option"
  names(legs)[names(legs) == "leg_geometry"] <- "geometry"
  legs$route_option <- as.integer(legs$route_option)

  itineraries$legs <- NULL

  # Extract Fare Info
  fare <- itineraries$fare
  if (!is.null(fare)) {
    if (length(fare) == nrow(itineraries)) {
      itineraries$fare <- vapply(fare, fare_func, 1)
      itineraries$fare_currency <- vapply(fare,fare_currency_func, "c")
    } else {
      itineraries$fare <- NA
      itineraries$fare_currency <- NA
    }
  } else {
    itineraries$fare <- NA
    itineraries$fare_currency <- NA
  }

  itineraries <- itineraries[legs$route_option, ]
  itineraries <- cbind(itineraries, legs)
  itineraries$fromPlace <- fp
  itineraries$toPlace <- tp

  return(itineraries)
}

#' fare parse
#' @param x fare
#' @noRd
fare_func <- function(x) {
  x <- x$fare$regular$cents
  if (length(x) == 0) {
    x <- NA_real_
  } else {
    x / 100
  }
}

#' fare currency parse
#' @param x fare
#' @noRd
fare_currency_func <- function(x) {
  x <- x$fare$regular$currency$currency
  if (length(x) == 0) {
    x <- NA_character_
  }
  x
}



#' Correct the elevation distances
#'
#' OTP returns elevation as a distance along the leg,
#' resetting to 0 at each leg but we need the distance
#' along the total route. so calculate this. Sometimes
#' the legs don't reset at 0, so account for this by
#' looking for a drop in length, sometimes small drops
#' within a leg so allows an error factor to ignore
#' small drops.
#'
#' @param dists numeric from the elevation first column
#' @param err a tolerance for errors in otp results
#' @family internal
#' @noRd

correct_distances <- function(dists, err = 1) {
  lth <- length(dists)
  if (lth <= 2) {
    return(dists) # Can't break up 2 points
  }
  brks <- dists[seq(1, lth - 1)] > (dists[seq(2, lth)] + err)
  brks <- seq(1, lth)[brks]
  if (length(brks) == 0) {
    return(dists) # No places the length decreased
  }
  mxs <- c(0, cumsum(dists[brks]))
  reps <- c(0, brks, lth)
  reps <- reps[seq(2, length(reps))] - reps[seq(1, length(reps) - 1)]
  csum <- rep(mxs, times = reps)
  return(dists + csum)
}

#' Convert Google Encoded Polyline and elevation data into sf object
#'
#' OTP returns the 2d route as a polyline bean and the elevation profile as vector of numbers
#' But the number of points for each is not the same, as 2D line only has a point at change of directions
#' While elevation is regally spaced. If elevation is supplied the correct heights are matched
#'
#' @param line character - polyline
#' @param elevation numeric - vector of elevations
#' @family internal
#' @noRd

polyline2linestring <- function(line, elevation = NULL) {
  line <- googlePolylines::decode(line$points)[[1]]
  line <- matrix(c(line$lon, line$lat), ncol = 2, dimnames = list(NULL, c("lon", "lat")))
  if (!is.null(elevation)) {
    # Some modes don't have elevation e.g TRANSIT, check for this
    if (all(is.na(elevation))) {
      ele <- rep(0, nrow(line))
    } else {
      elevation$first <- NULL
      elevation <- elevation[order(elevation$distance, method = "radix"), ]
      # Calculate the length of each segment
      dist <- geodist::geodist(line, sequential = TRUE, measure = "cheap")
      dist <- cumsum(dist)
      vals <- findInterval(dist, elevation$distance)
      vals[vals == 0] <- 1L
      ele <- elevation$second[c(1, vals)]
    }
    return(sfheaders::sfg_linestring(cbind(line, ele)))
  } else {
    return(sfheaders::sfg_linestring(line))
  }
}
