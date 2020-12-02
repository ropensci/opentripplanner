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
#'   TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR. Not all combinations
#'   are valid e.g. c("WALK","BUS") is valid but c("WALK","CAR") is not.
#' @param date_time POSIXct, a date and time, defaults to current date and time
#' @param arriveBy Logical, Whether the trip should depart or arrive at the
#'   specified date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP in metres
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
#'   `ncores` should be the number of cores on your system - 1.
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
                     ncores = 1,
                     timezone = otpcon$timezone,
                     distance_balance = FALSE,
                     get_elevation = FALSE) {
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
    message("'opentripplanner' is in legacy mode with some features disabled")
    message("Either update 'RcppSimdJson' or revert to 'opentripplanner' v0.2.3")
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
  mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m-%d-%Y", tz = timezone)
  time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
  checkmate::assert_numeric(maxWalkDistance, lower = 0, len = 1)
  checkmate::assert_numeric(numItineraries, lower = 1, len = 1)
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
      warning("repeating toPlace to match length of fromPlace")
    } else if (nrtp > nrfp & nrfp == 1) {
      fromPlace <- fromPlace[rep(1, times = nrtp), ]
      if (!is.null(fromID)) {
        fromID <- fromID[rep(1, times = nrtp)]
      }
      warning("repeating fromPlace to match length of toPlace")
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

  if (RcppSimdJsonVersion) {
    if (ncores > 1) {
      cl <- parallel::makeCluster(ncores, outfile = "otp_parallel_log.txt")
      parallel::clusterExport(
        cl = cl,
        varlist = c("otpcon", "fromPlace", "toPlace", "fromID", "toID"),
        envir = environment()
      )
      parallel::clusterEvalQ(cl, {
        loadNamespace("opentripplanner")
      })
      pbapply::pboptions(use_lb = TRUE)
      results <- pbapply::pblapply(seq(1, nrow(fromPlace)),
        otp_get_results,
        otpcon = otpcon,
        fromPlace = fromPlace,
        toPlace = toPlace,
        fromID = fromID,
        toID = toID,
        mode = mode,
        date = date,
        time = time,
        arriveBy = arriveBy,
        maxWalkDistance = maxWalkDistance,
        numItineraries = numItineraries,
        routeOptions = routeOptions,
        full_elevation = full_elevation,
        get_geometry = get_geometry,
        timezone = timezone,
        get_elevation = get_elevation,
        cl = cl
      )
      parallel::stopCluster(cl)
      rm(cl)
    } else {
      results <- pbapply::pblapply(seq(1, nrow(fromPlace)),
        otp_get_results,
        otpcon = otpcon,
        fromPlace = fromPlace,
        toPlace = toPlace,
        fromID = fromID,
        toID = toID,
        mode = mode,
        date = date,
        time = time,
        arriveBy = arriveBy,
        maxWalkDistance = maxWalkDistance,
        numItineraries = numItineraries,
        routeOptions = routeOptions,
        full_elevation = full_elevation,
        get_geometry = get_geometry,
        get_elevation = get_elevation,
        timezone = timezone
      )
    }
  } else {
    results <- pbapply::pblapply(seq(1, nrow(fromPlace)),
      otp_get_results_legacy,
      otpcon = otpcon,
      fromPlace = fromPlace,
      toPlace = toPlace,
      fromID = fromID,
      toID = toID,
      mode = mode,
      date = date,
      time = time,
      arriveBy = arriveBy,
      maxWalkDistance = maxWalkDistance,
      numItineraries = numItineraries,
      routeOptions = routeOptions,
      full_elevation = full_elevation,
      get_geometry = get_geometry,
      get_elevation = get_elevation,
      timezone = timezone
    )
  }





  results_class <- unlist(lapply(results, function(x) {
    "data.frame" %in% class(x)
  }), use.names = FALSE)
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
    if (any(unlist(lapply(results, function(x) {
      "sf" %in% class(x)
    }), use.names = FALSE))) {
      results_routes <- data.table::rbindlist(results_routes, fill = TRUE)
      results_routes <- list2df(results_routes)
      results_routes <- df2sf(results_routes)
      # fix for bbox error from data.table
      results_routes <- results_routes[seq_len(nrow(results_routes)), ]
      colnms <- names(results_routes)
      colnms <- colnms[!colnms %in% c("fromPlace", "toPlace", "geometry")]
      results_routes <- results_routes[c("fromPlace", "toPlace", colnms, "geometry")]
    } else {
      results_routes <- data.table::rbindlist(results_routes, fill = TRUE)
    }
  }


  if (!all(class(results_errors) == "logical")) {
    results_errors <- unlist(results_errors, use.names = FALSE)
    results_errors <- paste0(results_errors, "\n")
    warning(results_errors)
  }
  return(results_routes)
}

#' Get OTP results
#'
#' helper function for otp_plan
#'
#' @param x numeric
#' @param otpcon otpcon
#' @param fromPlace fromplace
#' @param toPlace toPlace
#' @param fromID fromID
#' @param toID toID
#' @param ... all other variaibles
#'
#' @noRd
otp_get_results <- function(x, otpcon, fromPlace, toPlace, fromID, toID,
                            ...) {

  res <- try(otp_plan_internal(
    otpcon = otpcon,
    fromPlace = fromPlace[x, ],
    toPlace = toPlace[x, ],
    fromID = fromID[x],
    toID = toID[x],
    ...
  ), silent = TRUE)

  if ("try-error" %in% class(res)) {
    res <- paste0("Try Error occured for ",
                  paste(fromPlace, collapse = ","),
                  " ",
                  paste(toPlace, collapse = ","),
                  " ",
                  res[[1]])
    warning(res)
  }

  return(res)
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


#' Get the geometry of a route from the OTP
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.529258,-0.134649)`
#' @param toPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.506383,-0.088780,)`
#' @param fromID fromID
#' @param toID toID
#' @param mode Character vector of modes of travel valid values TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR
#' @param date date
#' @param time time
#' @param arriveBy Logical, Whether the trip should depart or arrive at the specified date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP
#' @param routeOptions names list passed to OTP
#' @param numItineraries The maximum number of possible itineraries to return
#' @param full_elevation Logical, should the full elevation profile be returned, default FALSE
#' @param get_geometry logical, should geometry be returned
#' @param timezone timezone to use
#' @param get_elevation Logical, should you get elevation
#' @param RcppSimdJsonVersion Logical, is RcppSimdJson Version >= 0.1.2
#' @family internal
#' @details
#' This function returns a SF data.frame with one row for each leg of the journey
#' (a leg is defined by a change in mode). For transit more than one route option may be returned
#' and is indicated by the route_option column.
#'
#' @noRd

otp_plan_internal <- function(otpcon = NA,
                              fromPlace = NA,
                              toPlace = NA,
                              fromID = NULL,
                              toID = NULL,
                              mode = "CAR",
                              date = date,
                              time = time,
                              arriveBy = FALSE,
                              maxWalkDistance = 1000,
                              numItineraries = 3,
                              routeOptions = NULL,
                              full_elevation = FALSE,
                              get_geometry = TRUE,
                              timezone = "",
                              get_elevation = FALSE) {


  # Construct URL
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/plan")

  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
  toPlace <- format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)

  fromPlace <- paste(fromPlace, collapse = ",")
  toPlace <- paste(toPlace, collapse = ",")

  query <- list(
    fromPlace = fromPlace,
    toPlace = toPlace,
    mode = mode,
    date = date,
    time = time,
    maxWalkDistance = maxWalkDistance,
    arriveBy = arriveBy,
    numItineraries = numItineraries
  )

  if(otpcon$otp_version >= 2){
    # maxWalkDistance causes itinaries to fail
    if(mode == "CAR"){
      query$maxWalkDistance <- NULL
    }
  }

  if (!is.null(routeOptions)) {
    query <- c(query, routeOptions)
  }

  url <- build_url(routerUrl, query)
  text <- curl::curl_fetch_memory(url)
  text <- rawToChar(text$content)

  asjson <- try(RcppSimdJson::fparse(text, query = "/plan/itineraries"),
                silent = TRUE
  )

  # Check for errors - if no error object, continue to process content
  if (!"try-error" %in% class(asjson) & !is.null(asjson)) {
    response <- otp_json2sf(asjson, full_elevation, get_geometry, timezone, get_elevation)
    # Add Ids
    if (is.null(fromID)) {
      response$fromPlace <- fromPlace
    } else {
      response$fromPlace <- fromID
    }
    if (is.null(toID)) {
      response$toPlace <- toPlace
    } else {
      response$toPlace <- toID
    }
    return(response)
  } else {
    asjson <- RcppSimdJson::fparse(text)
    # there is an error - return the error code and message
    if(is.null(asjson$error)){
      response <- paste0(
        "Error: No itinary returned",
        " from ", asjson$`requestParameters`$fromPlace,
        " to ", asjson$`requestParameters`$toPlace
      )
    } else {
      response <- paste0(
        "Error: ", asjson$error$id,
        " from ", asjson$`requestParameters`$fromPlace,
        " to ", asjson$`requestParameters`$toPlace,
        " ", asjson$error$msg
      )
    }
    return(response)
  }
}


#' Convert output from OpenTripPlanner into sf object
#'
#' @param itineraries Object from the OTP API to process
#' @param full_elevation logical should the full elevation profile be returned (if available)
#' @param get_geometry logical, should geometry be returned
#' @param timezone character, which timezone to use, default "" means local time
#' @param get_elevation, logical, should xyz coordinate be returned
#' @family internal
#' @noRd

otp_json2sf <- function(itineraries, full_elevation = FALSE, get_geometry = TRUE,
                        timezone = "", get_elevation = FALSE) {
  itineraries$startTime <- lubridate::as_datetime(itineraries$startTime / 1000,
    origin = "1970-01-01", tz = timezone
  )

  itineraries$endTime <- lubridate::as_datetime(itineraries$endTime / 1000,
    origin = "1970-01-01", tz = timezone
  )


  # Loop over itineraries
  legs <- lapply(itineraries$legs, parse_leg,
    get_geometry = get_geometry,
    get_elevation = get_elevation,
    full_elevation = full_elevation
  )

  names(legs) <- seq_len(length(legs))
  legs <- legs[!is.na(legs)]
  legs <- data.table::rbindlist(legs, fill = TRUE, idcol = "route_option")
  legs$route_option <- as.integer(legs$route_option)

  legs$startTime <- lubridate::as_datetime(legs$startTime / 1000,
    origin = "1970-01-01", tz = timezone
  )
  legs$endTime <- lubridate::as_datetime(legs$endTime / 1000,
    origin = "1970-01-01", tz = timezone
  )

  itineraries$legs <- NULL

  # Extract Fare Info
  fare <- itineraries$fare
  if (!is.null(fare)) {
    if (length(fare) == nrow(itineraries)) {
      itineraries$fare <- vapply(fare, function(x) {
        x <- x$fare$regular$cents
        if(length(x) == 0){
          x <- as.numeric(NA)
        } else {
          x / 100
        }
      }, 1)
      itineraries$fare_currency <- vapply(fare, function(x) {
        x <- x$fare$regular$currency$currency
        if(length(x) == 0){
          x <- as.character(NA)
        }
        x
      }, "char")
    } else {
      # warning("Unstructured fare data has been discarded")
      itineraries$fare <- NA
      itineraries$fare_currency <- NA
    }
  } else {
    itineraries$fare <- NA
    itineraries$fare_currency <- NA
  }

  names(legs)[names(legs) == "startTime"] <- "leg_startTime"
  names(legs)[names(legs) == "endTime"] <- "leg_endTime"
  names(legs)[names(legs) == "duration"] <- "leg_duration"

  itineraries <- itineraries[legs$route_option, ]
  itineraries <- cbind(itineraries, legs)


  if (get_geometry) {
    itineraries <- df2sf(itineraries)
  }

  return(itineraries)
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
#' @param err a tollerance for errors in otp results
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
  mxs <- dists[brks]
  mxs <- cumsum(mxs)
  mxs <- c(0, mxs)
  reps <- c(0, brks, lth)
  reps <- reps[seq(2, length(reps))] - reps[seq(1, length(reps) - 1)]
  csum <- rep(mxs, times = reps)
  res <- dists + csum
  return(res)
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
  line <- googlePolylines::decode(line)[[1]]
  line <- matrix(c(line$lon, line$lat), ncol = 2, dimnames = list(NULL, c("lon", "lat")))
  # line <- as.matrix(line[, 2:1])
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
    linestring3D <- cbind(line, ele)
    linestring3D <- sfheaders::sfg_linestring(linestring3D)
    return(linestring3D)
  } else {
    linestring <- sfheaders::sfg_linestring(line)
    return(linestring)
  }
}
