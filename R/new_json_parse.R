# parse_leg2 <- function(leg,
#                       get_geometry = TRUE,
#                       get_elevation = TRUE,
#                       full_elevation = FALSE){
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
#
#           elevation <- lapply(seq(1, length(leg$steps)), function(x) {
#             leg$steps[[x]]$elevation
#           })
#           leg$steps <- NULL
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
#           elevation <- data.frame(first = elevation_first,
#                                   second = elevation_second,
#                                   distance = elevation_distance)
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
#
#
# otp_json2sf <- function(obj, full_elevation = FALSE, get_geometry = TRUE,
#                         timezone = "") {
#   requestParameters <- obj$requestParameters
#   plan <- obj$plan
#   debugOutput <- obj$debugOutput
#
#   itineraries <- plan$itineraries
#
#   itineraries$startTime <- as.POSIXct(itineraries$startTime / 1000,
#                                       origin = "1970-01-01", tz = timezone
#   )
#   itineraries$endTime <- as.POSIXct(itineraries$endTime / 1000,
#                                     origin = "1970-01-01", tz = timezone
#   )
#
#
#   # Loop over itineraries
#   legs <- lapply(itineraries$legs, parse_leg2,
#                  get_geometry = get_geometry,
#                  get_elevation = get_elevation,
#                  full_elevation = full_elevation)
#
#   legs <- legs[!is.na(legs)]
#   legs <- data.table::rbindlist(legs, fill = TRUE)
#
#   legs$startTime <- as.POSIXct(legs$startTime / 1000,
#                                origin = "1970-01-01", tz = timezone
#   )
#   legs$endTime <- as.POSIXct(legs$endTime / 1000,
#                              origin = "1970-01-01", tz = timezone
#   )
#
#   itineraries$legs <- NULL
#
#   # Extract Fare Info
#   fare <- itineraries$fare
#   if (!is.null(fare)) {
#     if(length(fare$fare) > 0){
#       itineraries$fare <- fare$fare$regular$cents / 100
#       itineraries$fare_currency <- fare$fare$regular$currency$currency
#     } else {
#       warning("Unstructured fare data has been discarded")
#       itineraries$fare <- NA
#       itineraries$fare_currency <- NA
#     }
#   } else {
#     itineraries$fare <- NA
#     itineraries$fare_currency <- NA
#   }
#
#   itineraries <- list2df(itineraries)
#
#   names(legs)[names(legs) == "startTime"] <- "leg_startTime"
#   names(legs)[names(legs) == "endTime"] <- "leg_endTime"
#   names(legs)[names(legs) == "duration"] <- "leg_duration"
#   itineraries <- cbind(itineraries, legs)
#
#
#   if (get_geometry) {
#     itineraries <- sf::st_as_sf(itineraries)
#     sf::st_crs(itineraries) <- 4326
#   }
#
#   return(itineraries)
# }
