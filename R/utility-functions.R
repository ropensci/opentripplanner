#' Convert list to data frame
#'
#' A faster alternative to as.data.frame(list), similar to base::list2DF() added
#' in R 4.0.0 but back compatible
#'
#' @param l named list of equal length
#' @family internal
#' @noRd
list2df <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}

#' Build URL for otp_plan
#'
#' A faster alternative to as.data.frame(list), similar to base::list2DF() added
#' in R 4.0.0 but back compatible
#'
#' @param routerUrl output of make_url
#' @param query namesd list
#' @family internal
#' @noRd
build_url <- function(routerUrl, query) {
  secs <- unlist(query, use.names = TRUE)
  secs <- paste0(names(secs),"=",secs)
  secs <- paste(secs, collapse = "&")
  secs <- gsub(",","%2C",secs)
  secs <- paste0(routerUrl,"?",secs)
  secs
}

#' Parse a single leg otp_plan
#'
#' Function to replace looping over lists
#'
#' @param leg list
#' @param get_geometry Logical
#' @param get_elevation Logical
#' @param full_elevation Logical
#' @family internal
#' @noRd
parse_leg <- function(leg,
                      get_geometry = TRUE,
                      get_elevation = TRUE,
                      full_elevation = FALSE){
    # split into parts
    leg$from <- NULL
    leg$to <- NULL

    if (get_geometry) {
      # Extract geometry
      legGeometry <- leg$legGeometry$points
      leg$legGeometry <- NULL

      # Check for Elevations
      # transit legs have no steps and no elevation
      if((get_elevation | full_elevation)){

        if(length(leg$steps) > 0){
          if (sum(lengths(leg$steps[[1]]$elevation)) > 0) {
            # We have Elevation Data
            # Extract the elevation values

            elevation <- lapply(seq(1, length(leg$steps)), function(x) {
              leg$steps[[x]]$elevation
            })
            leg$steps <- NULL

            elevation_first <- unlist(lapply(elevation, function(x){
              vapply(x, `[[`, 1 ,1)
            }), use.names = FALSE)

            elevation_second <- unlist(lapply(elevation, function(x){
              vapply(x, `[[`, 1 ,2)
            }), use.names = FALSE)

            elevation_distance <- correct_distances(elevation_first)

            elevation <- data.frame(first = elevation_first,
                                    second = elevation_second,
                                    distance = elevation_distance)

          } else {
            elevation <- NULL
          }
        } else {
          elevation <- NA
          leg$steps <- NULL
        }


      } else {
        elevation <- NULL
        leg$steps <- NULL
      }

      lines <- polyline2linestring(legGeometry, elevation = elevation)

      lines <- sf::st_sfc(lines, crs = 4326)

      leg$geometry <- lines
      leg <- list2df(leg)
      leg <- sf::st_sf(leg)

      # Add full elevation if required
      if (full_elevation) {
        leg$elevation <- list(elevation)
      }
    } else {
      leg$legGeometry <- NULL
      leg$steps <- NULL
    }

    return(leg)

}

#' Parse a single leg otp_plan
#'
#' Function to replace looping over lists
#'
#' @param leg list
#' @param get_geometry Logical
#' @param get_elevation Logical
#' @param full_elevation Logical
#' @family internal
#' @noRd
parse_leg2 <- function(leg,
                       get_geometry = TRUE,
                       get_elevation = TRUE,
                       full_elevation = FALSE){
  # split into parts
  leg$from <- NULL
  leg$to <- NULL

  if (get_geometry) {
    # Extract geometry
    legGeometry <- leg$legGeometry[[1]]$points
    leg$legGeometry <- NULL

    # Check for Elevations
    # transit legs have no steps and no elevation
    if((get_elevation | full_elevation)){

      if(length(leg$steps) > 0){
        if (sum(lengths(leg$steps[[1]]$elevation)) > 0) {
          # We have Elevation Data
          # Extract the elevation values

          elevation <- lapply(seq(1, length(leg$steps)), function(x) {
            leg$steps[[x]]$elevation
          })
          leg$steps <- NULL
          elevation <- elevation[[1]]

          elevation_first <- unlist(lapply(elevation, function(x){
            vapply(x, `[[`, 1 ,1)
          }), use.names = FALSE)

          elevation_second <- unlist(lapply(elevation, function(x){
            vapply(x, `[[`, 1 ,2)
          }), use.names = FALSE)

          elevation_distance <- correct_distances(elevation_first)

          elevation <- data.frame(first = elevation_first,
                                  second = elevation_second,
                                  distance = elevation_distance)

        } else {
          elevation <- NULL
        }
      } else {
        elevation <- NA
        leg$steps <- NULL
      }


    } else {
      elevation <- NULL
      leg$steps <- NULL
    }

    lines <- polyline2linestring(legGeometry, elevation = elevation)

    lines <- sf::st_sfc(lines, crs = 4326)

    leg$geometry <- lines
    leg <- list2df(leg)
    leg <- sf::st_sf(leg)

    # Add full elevation if required
    if (full_elevation) {
      leg$elevation <- list(elevation)
    }
  } else {
    leg$legGeometry <- NULL
    leg$steps <- NULL
  }

  return(leg)

}

#' Parse a single itinerary otp_plan
#'
#' Function to replace looping over lists
#'
#' @param itinerary list
#' @param timezone timezone
#' @param get_geometry Logical
#' @param get_elevation Logical
#' @param full_elevation Logical
#' @family internal
#' @noRd
parse_itinerary <- function(itinerary,
                            timezone = "",
                            get_geometry = TRUE,
                            get_elevation = TRUE,
                            full_elevation = FALSE){
  itinerary$startTime <- as.POSIXct(itinerary$startTime / 1000,
                                    origin = "1970-01-01", tz = timezone
  )
  itinerary$endTime <- as.POSIXct(itinerary$endTime / 1000,
                                  origin = "1970-01-01", tz = timezone
  )

  # Loop over itineraries
  legs <- lapply(itinerary$legs, parse_leg,
                 get_geometry = get_geometry,
                 get_elevation = get_elevation,
                 full_elevation = full_elevation)

  legs <- legs[!is.na(legs)]
  legs <- data.table::rbindlist(legs, fill = TRUE)

  legs$startTime <- as.POSIXct(legs$startTime / 1000,
                               origin = "1970-01-01", tz = timezone
  )
  legs$endTime <- as.POSIXct(legs$endTime / 1000,
                             origin = "1970-01-01", tz = timezone
  )

  itinerary$legs <- NULL

  # Extract Fare Info
  fare <- itinerary$fare
  if (!is.null(fare)) {
    if(length(fare$fare) > 0){
      itinerary$fare <- fare$fare$regular$cents / 100
      itinerary$fare_currency <- fare$fare$regular$currency$currency
    } else {
      warning("Unstructured fare data has been discarded")
      itinerary$fare <- NA
      itinerary$fare_currency <- NA
    }
  } else {
    itinerary$fare <- NA
    itinerary$fare_currency <- NA
  }

  itinerary <- list2df(itinerary)

  names(legs)[names(legs) == "startTime"] <- "leg_startTime"
  names(legs)[names(legs) == "endTime"] <- "leg_endTime"
  names(legs)[names(legs) == "duration"] <- "leg_duration"
  itinerary <- cbind(itinerary, legs)

  return(itinerary)
}
