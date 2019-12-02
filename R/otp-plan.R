#' Get get a route or routes from the OTP
#'
#' @description
#' This is the main routing function for OTP and can find single or
#'     multiple routes between
#' `fromPlace` and `toPlace`.
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Longitude/Latitude pair,
#'     e.g. `c(-0.134649,51.529258)`,
#' or 2 column matrix of Longitude/Latitude pairs, or sf
#'     data frame of POINTS
#' @param toPlace Numeric vector, Longitude/Latitude pair,
#'     e.g. `c(-0.088780,51.506383)`, or 2 column matrix of
#'     Longitude/Latitude pairs, or sf data frame of POINTS
#' @param fromID character vector same length as fromPlace
#' @param toID character vector same length as toPlace
#' @param mode character vector of one or more modes of travel valid values
#'     TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR. Not all
#'     combinations are valid e.g. c("WALK","BUS") is valid but
#'     c("WALK","CAR") is not.
#' @param date_time POSIXct, a date and time, defaults to current
#'     date and time
#' @param arriveBy Logical, Whether the trip should depart or arrive
#'     at the specified date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP in metres
#' @param walkReluctance Numeric passed to OTP
#' @param transferPenalty Numeric passed to OTP
#' @param minTransferTime Numeric passed to OTP in seconds
#' @param numItineraries The maximum number of possible itineraries to return
#' @param full_elevation Logical, should the full elevation profile be returned,
#'     default FALSE
#' @param ncores Numeric, number of cores to use when batch processing,
#'     default 1, see details
#' @param get_geometry Logical, should the route geometry be returned,
#'     default TRUE, see details
#'
#' @export
#' @family routing
#' @return Returns an SF data frame of LINESTRINGs
#'
#' @details
#' This function returns a SF data.frame with one row for each leg
#' of the journey (a leg is defined by a change in mode). For transit,
#' more than one route option may be returned and is indicated by the
#' `route_option` column. The number of different itineraries can be
#' set with the `numItineraries` variable.
#'
#' ## Batch Routing
#'
#' When passing a matrix or SF data frame object to fromPlace and toPlace
#' `otp_plan` will route in batch mode. In this case the `ncores` variable
#' will be used. Increasing `ncores` will enable multicore routing, the max
#'  `ncores` should be the number of cores on your system - 1.
#'
#' ## Elevation
#'
#' OTP supports elevation data and can return the elevation profile of the
#' route if available. OTP returns the elevation profile separately from the
#' XY coordinates, this means there is not direct match between the number of
#'  XY points and the number of Z points.  OTP also only returns the
#' elevation profile for the first leg of the route (this appears to be a bug).
#' As a default, the otp_plan function matches the elevation profile to the
#' XY coordinates to return an SF linestring with XYZ coordinates. If you
#' require a more detailed elevation profile, the full_elevation parameter
#' will return a nested data.frame with three columns. first and second
#' are returned from OTP, while distance is the cumulative distance along the
#' route and is derived from First.
#'
#' ## Route Geometry
#'
#' The `get_geometry` provides the option to not return the route geometry,
#' and just return the meta-data (e.g. journey time). This may be useful when
#' creating an Origin:Destination matrix and also provides a small
#' performance boost by reduced processing of geometries.
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
                     walkReluctance = 2,
                     transferPenalty = 0,
                     minTransferTime = 0,
                     numItineraries = 3,
                     full_elevation = FALSE,
                     get_geometry = TRUE,
                     ncores = 1) {
  # Check Valid Inputs
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
  date <- format(date_time, "%m-%d-%Y")
  time <- tolower(format(date_time, "%I:%M%p"))
  checkmate::assert_numeric(maxWalkDistance, lower = 0, len = 1)
  checkmate::assert_numeric(walkReluctance, lower = 0, len = 1)
  checkmate::assert_numeric(transferPenalty, lower = 0, len = 1)
  checkmate::assert_numeric(numItineraries, lower = 1, len = 1)
  checkmate::assert_character(fromID, null.ok = TRUE)
  checkmate::assert_character(toID, null.ok = TRUE)
  checkmate::assert_logical(arriveBy)
  arriveBy <- tolower(arriveBy)

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



  if (ncores > 1) {
    cl <- parallel::makeCluster(ncores)
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
      walkReluctance = walkReluctance,
      transferPenalty = transferPenalty,
      minTransferTime = minTransferTime,
      numItineraries = numItineraries,
      full_elevation = full_elevation,
      get_geometry = get_geometry,
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
      walkReluctance = walkReluctance,
      transferPenalty = transferPenalty,
      minTransferTime = minTransferTime,
      numItineraries = numItineraries,
      full_elevation = full_elevation,
      get_geometry = get_geometry
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
    if (any(unlist(lapply(results, function(x) {
      "sf" %in% class(x)
    })))) {
      suppressWarnings(results_routes <- dplyr::bind_rows(results_routes))
      results_routes <- as.data.frame(results_routes)
      results_routes$geometry <- sf::st_sfc(results_routes$geometry)
      results_routes <- sf::st_sf(results_routes)
      sf::st_crs(results_routes) <- 4326
    } else {
      results_routes <- dplyr::bind_rows(results_routes)
    }
  }


  if (!all(class(results_errors) == "logical")) {
    results_errors <- unlist(results_errors)
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
  res <- otp_plan_internal(
    otpcon = otpcon,
    fromPlace = fromPlace[x, ],
    toPlace = toPlace[x, ],
    fromID = fromID[x],
    toID = toID[x],
    ...
  )

  # if ("data.frame" %in% class(res)) {
  #   res$fromPlace <- paste(fromPlace[x, ], collapse = ",")
  #   res$toPlace <- paste(toPlace[x, ], collapse = ",")
  # }
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
  #if (all(class(imp) == "matrix")) { # to pass CRAN checks
  if ("matrix" %in% class(imp)) {
    checkmate::assert_matrix(imp,
      any.missing = FALSE,
      min.rows = 1,
      min.cols = 2,
      max.cols = 2,
      null.ok = FALSE
    )
    checkmate::assert_numeric(imp[, 1], lower = -180, upper = 180)
    checkmate::assert_numeric(imp[, 2], lower = -90, upper = 90)
    imp[] <- imp[, 2:1] # Switch round lng/lat to lat/lng for OTP
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
#' @param walkReluctance Numeric passed to OTP
#' @param transferPenalty Numeric passed to OTP
#' @param minTransferTime Numeric passed to OTP
#' @param numItineraries The maximum number of possible itineraries to return
#' @param full_elevation Logical, should the full elevation profile be returned, default FALSE
#' @param get_geometry logical, should geometry be returned
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
                              walkReluctance = 2,
                              transferPenalty = 0,
                              minTransferTime = 0,
                              numItineraries = 3,
                              full_elevation = FALSE,
                              get_geometry = TRUE) {


  # Construct URL
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/plan")

  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
  toPlace <- format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)

  fromPlace <- paste(fromPlace, collapse = ",")
  toPlace <- paste(toPlace, collapse = ",")

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
      minTransferTime = minTransferTime,
      numItineraries = numItineraries
    )
  )

  # convert response content into text
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  # parse text to json
  asjson <- jsonlite::fromJSON(text)

  # Check for errors - if no error object, continue to process content
  if (is.null(asjson$error$id)) {
    response <- otp_json2sf(asjson, full_elevation, get_geometry)
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
    # there is an error - return the error code and message
    response <- paste0(
      "Error: ", asjson$error$id,
      " from ", asjson$`requestParameters`$fromPlace,
      " to ", asjson$`requestParameters`$toPlace,
      " ", asjson$error$msg
    )
    return(response)
  }
}


#' Convert output from OpenTripPlanner into sf object
#'
#' @param obj Object from the OTP API to process
#' @param full_elevation logical should the full elevation profile be returned (if available)
#' @param get_geometry logical, should geometry be returned
#' @family internal
#' @noRd

otp_json2sf <- function(obj, full_elevation = FALSE, get_geometry = TRUE) {
  requestParameters <- obj$requestParameters
  plan <- obj$plan
  debugOutput <- obj$debugOutput

  itineraries <- plan$itineraries

  itineraries$startTime <- as.POSIXct(itineraries$startTime / 1000,
    origin = "1970-01-01", tz = "GMT"
  )
  itineraries$endTime <- as.POSIXct(itineraries$endTime / 1000,
    origin = "1970-01-01", tz = "GMT"
  )


  legs <- list()
  # Loop over itineraries
  for (i in seq(1, nrow(itineraries))) {
    leg <- itineraries$legs[[i]]
    # split into parts
    vars <- leg
    vars$from <- NULL
    vars$to <- NULL
    vars$steps <- NULL
    vars$legGeometry <- NULL

    if (get_geometry) {
      # Extract geometry
      legGeometry <- leg$legGeometry$points

      # Check for Elevations
      steps <- leg$steps
      elevation <- lapply(seq(1, length(legGeometry)), function(x) {
        leg$steps[[x]]$elevation
      })
      if (sum(lengths(elevation)) > 0) {
        # We have Elevation Data
        # Extract the elevation values
        elevation <- lapply(seq(1, length(legGeometry)), function(x) {
          dplyr::bind_rows(elevation[[x]])
        })
        elevation <- lapply(seq(1, length(legGeometry)), function(x) {
          if (nrow(elevation[[x]]) == 0) {
            NA
          } else {
            elevation[[x]]
          }
        })
        # the x coordinate of elevation reset at each leg, correct for this
        for (l in seq(1, length(elevation))) {
          if (!all(is.na(elevation[[l]]))) {
            elevation[[l]]$distance <- correct_distances(elevation[[l]]$first)
          }
        }
        # process the lines into sf objects
        lines <- list()
        for (j in seq(1, length(legGeometry))) {
          lines[[j]] <- polyline2linestring(
            line = legGeometry[j],
            elevation = elevation[[j]]
          )
        }
      } else {
        lines <- polyline2linestring(legGeometry)
      }

      lines <- sf::st_sfc(lines, crs = 4326)

      vars$geometry <- lines
      vars <- sf::st_sf(vars)

      # Add full elevation if required
      if (full_elevation) {
        vars$elevation <- elevation
      }
    }

    vars$route_option <- i

    # return to list
    legs[[i]] <- vars
  }

  legs <- legs[!is.na(legs)]
  suppressWarnings(legs <- dplyr::bind_rows(legs))

  if (get_geometry) {
    # rebuild the sf object
    legs <- as.data.frame(legs)
    legs$geometry <- sf::st_sfc(legs$geometry)
    legs <- sf::st_sf(legs)
    sf::st_crs(legs) <- 4326
  }

  legs$startTime <- as.POSIXct(legs$startTime / 1000,
    origin = "1970-01-01", tz = "GMT"
  )
  legs$endTime <- as.POSIXct(legs$endTime / 1000,
    origin = "1970-01-01", tz = "GMT"
  )

  itineraries$legs <- NULL

  # Extract Fare Info and discard for now
  fare <- itineraries$fare
  if (!is.null(fare)) {
    itineraries$fare <- fare$fare$regular$cents / 100
    itineraries$fare_currency <- fare$fare$regular$currency$currency
  } else {
    itineraries$fare <- NA
    itineraries$fare_currency <- NA
  }


  itineraries <- itineraries[legs$route_option, ]
  itineraries <- dplyr::bind_cols(itineraries, legs)

  if (get_geometry) {
    itineraries <- sf::st_as_sf(itineraries)
    sf::st_crs(itineraries) <- 4326
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
  brks <- dists[seq(1, lth - 1)] > (dists[seq(2, lth)] + err)
  brks <- seq(1, lth)[brks]
  mxs <- list()
  brks_lth <- length(brks)
  for (k in seq(1, brks_lth)) {
    if (k == 1) {
      mxs[[k]] <- max(dists[seq(1, brks[k])])
    } else if (k <= brks_lth) {
      mxs[[k]] <- max(dists[seq(brks[k - 1] + 1, brks[k])])
    } else {
      stop("error in sequence of correct_distances")
    }
  }
  mxs <- unlist(mxs)
  mxs <- cumsum(mxs)
  mxs <- c(0, mxs)
  reps <- c(0, brks, lth)
  reps <- reps[seq(2, length(reps))] - reps[seq(1, length(reps) - 1)]
  csum <- rep(mxs, times = reps)
  res <- dists + csum
  return(res)
}
# correct_distances <- function(dists) {
#   res <- list()
#   rebase <- 0
#   for (k in seq(1, length(dists))) {
#     if (k == 1) {
#       dists_k <- dists[k]
#       res[[k]] <- dists_k
#     } else {
#       dists_k <- dists[k]
#       res_km1 <- res[[k - 1]]
#       if (dists_k == 0) {
#         rebase <- rebase + dists[k - 1]
#         res[[k]] <- dists_k + rebase
#       } else {
#         res[[k]] <- dists_k + rebase
#       }
#     }
#   }
#
#   res <- unlist(res)
#   return(res)
# }




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
  line <- as.matrix(line[, 2:1])
  if (!is.null(elevation)) {
    # Some modes don't have elevation e.g TRANSIT, check for this
    if (all(is.na(elevation))) {
      ele <- rep(0, nrow(line))
    } else {
      elevation <- elevation[order(elevation$distance), ]
      # Calculate the length of each segment
      dist <- geodist::geodist(line[seq(1, nrow(line) - 1), ],
        line[seq(2, nrow(line)), ],
        paired = TRUE,
        measure = "cheap"
      )
      dist <- cumsum(dist)
      vals <- findInterval(dist, elevation$distance)
      vals[vals == 0] <- 1L
      ele <- elevation$second[c(1, vals)]
    }
    linestring3D <- cbind(line, ele)
    linestring3D <- sf::st_linestring(linestring3D, dim = "XYZ")
    return(linestring3D)
  } else {
    linestring <- sf::st_linestring(line)
    return(linestring)
  }
}
