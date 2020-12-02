# Code to support the pre rcppsimdjson

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
#' @param full_elevation Logical, FALSE
#' @param get_geometry logical, should geometry be returned
#' @param timezone timezone to use
#' @param get_elevation Logical, FASLE
#' @family internal
#' @details
#' This function returns a SF data.frame with one row for each leg of the journey
#' (a leg is defined by a change in mode). For transit more than one route option may be returned
#' and is indicated by the route_option column.
#'
#' @noRd

otp_plan_internal_legacy <- function(otpcon = NA,
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


  # Disabled in Legacy mode
  full_elevation <- FALSE
  get_elevation <- FALSE

  if(otpcon$otp_version >= 2){
    stop("OTP2 is not supported in legacy mode")
  }

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

  if (!is.null(routeOptions)) {
    query <- c(query, routeOptions)
  }

  url <- build_url(routerUrl, query)
  text <- curl::curl_fetch_memory(url)
  text <- rawToChar(text$content)

  asjson <- json_parse_legacy(text)

  # Check for errors - if no error object, continue to process content
  if (!"try-error" %in% class(asjson)) {
    response <- otp_json2sf(asjson, full_elevation = FALSE, get_geometry, timezone, get_elevation = FALSE)
    response$legGeometry.length <- NULL
    response$legGeometry.points <- NULL
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
    response <- paste0(
      "Error: ", asjson$error$id,
      " from ", asjson$`requestParameters`$fromPlace,
      " to ", asjson$`requestParameters`$toPlace,
      " ", asjson$error$msg
    )
    return(response)
  }
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
otp_get_results_legacy <- function(x, otpcon, fromPlace, toPlace, fromID, toID,
                                   ...) {


  res <- try(otp_plan_internal_legacy(
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

#' Parse Json without rcppsimdjson
#'
#'
#' @param text object
#'
#' @noRd
json_parse_legacy <- function(text) {
  asjson <- rjson::fromJSON(text)
  asjson <- asjson$plan$itineraries

  for (j in seq_len(length(asjson))) {
    fizz <- asjson[[j]]
    legs <- fizz$legs
    fizz$legs <- NULL
    fizz$fare <- NULL
    fizz <- as.data.frame(fizz)

    for (i in seq_len(length(legs))) {
      legs[[i]]$from <- NULL
      legs[[i]]$to <- NULL
      legs[[i]]$steps <- NULL
      lg <- list(legs[[i]]$legGeometry)
      legs[[i]] <- as.data.frame(legs[[i]])
      legs[[i]]$legGeometry <- lg
      legs[[i]]$legGeometry.points <- NULL
      legs[[i]]$legGeometry.length <- NULL
    }

    # Get all names
    nms <- list()
    for (i in seq_len(length(legs))) {
      nms[[i]] <- names(legs[[i]])
    }
    nms <- unique(unlist(nms))

    # Add empty names
    for (i in seq_len(length(legs))) {
      sub <- legs[[i]]
      nms_miss <- nms[!nms %in% names(sub)]
      sub[, nms_miss] <- NA
      sub <- sub[, nms]
      legs[[i]] <- sub
    }

    legs <- do.call("rbind", legs)
    fizz$legs <- list(legs)
    asjson[[j]] <- fizz
  }

  asjson <- do.call("rbind", asjson)

  return(asjson)
}
