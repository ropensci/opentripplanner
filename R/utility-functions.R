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

#' Convert data frame to sf data frame
#'
#' A faster alternative to sf::st_sf() but without the checks
#'
#' @param df named list of equal length
#' @family internal
#' @noRd
df2sf <- function(df){
  class(df) <- c("sf","data.frame")
  attributes(df)$sf_column <- "geometry"
  df
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
# parse_leg <- function(leg,
#                        get_geometry = TRUE,
#                        get_elevation = TRUE,
#                        full_elevation = FALSE){
#   # split into parts
#   leg$from <- NULL
#   leg$to <- NULL
#
#   if (get_geometry) {
#     # Extract geometry
#     legGeometry <- leg$legGeometry[[1]]$points
#     leg$legGeometry <- NULL
#
#     # Check for Elevations
#     # transit legs have no steps and no elevation
#     if((get_elevation | full_elevation)){
#
#       if(length(leg$steps) > 0){
#         if (sum(lengths(leg$steps[[1]]$elevation)) > 0) {
#           # We have Elevation Data
#           # Extract the elevation values
#           elevation <- lapply(seq(1, length(leg$steps)), function(x) {
#             leg$steps[[x]]$elevation
#           })
#           leg$steps <- NULL
#           elevation <- elevation[[1]]
#
#           elevation_first <- unlist(lapply(elevation, function(x){
#             vapply(x, `[[`, 1 ,1)
#           }), use.names = FALSE)
#
#           elevation_second <- unlist(lapply(elevation, function(x){
#             vapply(x, `[[`, 1 ,2)
#           }), use.names = FALSE)
#
#           elevation_distance <- correct_distances(elevation_first)
#
#           elevation <- list(first = elevation_first,
#                             second = elevation_second,
#                             distance = elevation_distance)
#           elevation <- list2df(elevation)
#
#
#         } else {
#           elevation <- NULL
#         }
#       } else {
#         elevation <- NA
#         leg$steps <- NULL
#       }
#
#
#     } else {
#       elevation <- NULL
#       leg$steps <- NULL
#     }
#
#     lines <- polyline2linestring(legGeometry, elevation = elevation)
#
#     lines <- sf::st_sfc(lines, crs = 4326)
#
#     leg$geometry <- lines
#     leg <- list2df(leg)
#     leg <- sf::st_sf(leg)
#
#     # Add full elevation if required
#     if (full_elevation) {
#       leg$elevation <- list(elevation)
#     }
#   } else {
#     leg$legGeometry <- NULL
#     leg$steps <- NULL
#   }
#
#   return(leg)
#
# }
parse_leg <- function(leg,
                       get_geometry = TRUE,
                       get_elevation = TRUE,
                       full_elevation = FALSE){
  # split into parts
  leg$from <- NULL
  leg$to <- NULL

  if(get_elevation | full_elevation){
    elevation <- lapply(leg$steps, parse_elevation)
  } else {
    elevation <- NULL
  }

  if(full_elevation){
    leg$elevation <- elevation
  }

  leg$steps <- NULL

  if (get_geometry) {
    # Extract geometry
    legGeometry <- list()
    for(i in seq_len(nrow(leg))){
      legGeometry[[i]] <- polyline2linestring(line = leg$legGeometry[[i]]$points, elevation = elevation[[i]])
    }

    leg$geometry <- sf::st_sfc(legGeometry, crs = 4326)
    leg$legGeometry <- NULL
    #leg <- sf::st_sf(leg)
    leg <- df2sf(leg)

  } else {
    leg$legGeometry <- NULL
  }

  return(leg)
}

#' Parse elevation data
#'
#'
#' @param stp list - a step
#' @family internal
#' @noRd
parse_elevation <- function(stp){
  if(is.null(stp)){
    return(NA)
  }

  elev <- data.table::rbindlist(stp$elevation, idcol = "step")
  elev$distance <- correct_distances(elev$first)
  return(as.data.frame(elev))
}
