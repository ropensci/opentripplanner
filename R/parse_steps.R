# parse_steps <- function(stp, get_elevation = TRUE, full_elevation = TRUE) {
#   if (is.null(stp)) {
#     stop("Atempting to parse TRANSIT leg via steps function")
#   }
#   if ((get_elevation | full_elevation)) {
#     elev <- data.table::rbindlist(stp$elevation, idcol = "step")
#   } else {
#     elev <- NULL
#   }
#
#   if (full_elevation) {
#     elev$distance <- correct_distances(elev$first)
#   }
#
#   if (get_elevation) {
#     coords <- sf::st_linestring(
#       matrix(c(stp$lon, stp$lat, elev$second[!duplicated(elev$step)]),
#         ncol = 3,
#         dimnames = list(NULL, c("lon", "lat", "z"))
#       ),
#       dim = "XYZ"
#     )
#   } else {
#     coords <- sf::st_linestring(
#       matrix(c(stp$lon, stp$lat),
#         ncol = 2,
#         dimnames = list(NULL, c("lon", "lat"))
#       ),
#       dim = "XY"
#     )
#   }
#
#   return(list(coords, elev))
# }
#
# parse_elevation <- function(stp) {
#   if (is.null(stp)) {
#     return(NA)
#   }
#
#   elev <- data.table::rbindlist(stp$elevation, idcol = "step")
#   elev$distance <- correct_distances(elev$first)
#   return(as.data.frame(elev))
# }
#
#
# parse_transit <- function(legGeometry, get_elevation = TRUE) {
#   coords <- googlePolylines::decode(legGeometry$points)[[1]]
#   if (get_elevation) {
#     coords <- sf::st_linestring(
#       matrix(c(coords$lon, coords$lat, rep(0, nrow(coords))),
#         ncol = 3,
#         dimnames = list(NULL, c("lon", "lat", "z"))
#       ),
#       dim = "XYZ"
#     )
#   } else {
#     coords <- sf::st_linestring(
#       matrix(c(coords$lon, coords$lat),
#         ncol = 2,
#         dimnames = list(NULL, c("lon", "lat"))
#       ),
#       dim = "XY"
#     )
#   }
#
#   return(list(coords, NULL))
# }
#
# parse_leg2 <- function(leg,
#                        get_geometry = TRUE,
#                        get_elevation = TRUE,
#                        full_elevation = FALSE) {
#   # split into parts
#   leg$from <- NULL
#   leg$to <- NULL
#
#   if (get_geometry | full_elevation) {
#     res <- list()
#     for (i in seq_len(nrow(leg))) {
#       if (is.null(leg$steps[[i]])) {
#
#         res[[i]] <- parse_transit(leg$legGeometry[[i]], get_elevation)
#       } else {
#         res[[i]] <- parse_steps(leg$steps[[i]],
#           get_elevation = get_elevation,
#           full_elevation = full_elevation
#         )
#       }
#     }
#     leg$steps <- NULL
#     leg$legGeometry <- NULL
#
#     if (get_geometry) {
#       coords <- lapply(res, `[[`, 1)
#       leg$geometry <- sf::st_sfc(coords, crs = 4326)
#       leg <- sf::st_as_sf(leg)
#     }
#
#     if (full_elevation) {
#       leg$elevation <- lapply(res, `[[`, 2)
#     }
#   } else {
#     leg$steps <- NULL
#     leg$legGeometry <- NULL
#   }
#
#   return(leg)
# }
