# library(opentripplanner)
# # Path to a folder containing the OTP.jar file, change to where you saved the file.
# path_data <- file.path(tempdir(), "OTP")
# dir.create(path_data)
# path_otp <- otp_dl_jar(version = "1.4.0")
# otp_dl_demo(path_data)
# # Build Graph and start OTP
# log1 <- otp_build_graph(otp = path_otp, dir = path_data)
# log2 <- otp_setup(otp = path_otp, dir = path_data)
# otpcon <- otp_connect(timezone = "Europe/London")
#
# lsoa <- sf::st_read("https://github.com/ropensci/opentripplanner/releases/download/0.1/centroids.gpkg",
#                     stringsAsFactors = FALSE)
#
# toPlace   = lsoa[rep(seq(1, nrow(lsoa)), times = nrow(lsoa)),]
# fromPlace = lsoa[rep(seq(1, nrow(lsoa)), each  = nrow(lsoa)),]
#
# system.time({routes <- otp_plan(otpcon = otpcon,
#                                 fromPlace = fromPlace,
#                                 toPlace = toPlace,
#                                 fromID = fromPlace$geo_code,
#                                 toID = toPlace$geo_code,
#                                 ncores = 4)})
#116 seconds on 4 core CRAN 7306 routes of 7921 returned
#473 seconds on 4 core Futures 7310 routes of 7921 returned
#484 seconds on 4 core Futures 7310 routes of 7921 returned
#
#
# slow_func <- function(x, p){
#   #x <- x ** 2
#   p(sprintf("x=%g", x))
#   Sys.sleep(1)
#   return(x)
# }
#
#
# batch_function <- function(vals = rep(1:10, 2)){
#   vals <- vals[order(vals, decreasing = TRUE)]
#   progressr::handlers(global = TRUE)
#   progressr::handlers("progress")
#   p <- progressr::progressor(along = vals)
#
#   future::plan("future::multisession", workers = 4)
#   r <- future.apply::future_lapply(vals,
#                                    slow_func,
#                                    p = p,
#                                    future.scheduling = TRUE,
#                                    future.chunk.size = NULL)
#   future::plan("sequential")
#   r <- unlist(r)
#   return(r)
# }


# otp_get_results2 <- function(fromPlace, toPlace, fromID, toID, otpcon, p,
#                              ...) {
#   p()
#   res <- try(otp_plan_internal(
#     otpcon = otpcon,
#     fromPlace = fromPlace,
#     toPlace = toPlace,
#     fromID = fromID,
#     toID = toID,
#     ...
#   ), silent = TRUE)
#
#   if ("try-error" %in% class(res)) {
#     res <- paste0(
#       "Try Error occured for ",
#       paste(fromPlace, collapse = ","),
#       " ",
#       paste(toPlace, collapse = ","),
#       " ",
#       res[[1]]
#     )
#     warning(res)
#   }
#
#   return(res)
# }

# future::plan("future::multisession", workers = ncores)
# progressr::handlers(global = TRUE)
# progressr::handlers("progress")
# xs <- seq(1, nrow(fromPlace))
# p <- progressr::progressor(along = xs)


# results <- future.apply::future_lapply(xs,
#                                        otp_get_results,
#                                        otpcon = otpcon,
#                                        fromPlace = fromPlace,
#                                        toPlace = toPlace,
#                                        fromID = fromID,
#                                        toID = toID,
#                                        p = p,
#                                        mode = mode,
#                                        date = date,
#                                        time = time,
#                                        arriveBy = arriveBy,
#                                        maxWalkDistance = maxWalkDistance,
#                                        numItineraries = numItineraries,
#                                        routeOptions = routeOptions,
#                                        full_elevation = full_elevation,
#                                        get_geometry = get_geometry,
#                                        timezone = timezone,
#                                        get_elevation = get_elevation,
#                                        future.seed = TRUE,
#                                        future.scheduling = TRUE,
#                                        future.chunk.size = NULL)




# fromPlacel <- split(fromPlace, 1:nrow(fromPlace))
# toPlacel <- split(toPlace, 1:nrow(toPlace))
#
#
# results <- furrr::future_pmap(
#   .l = list(fromPlace = fromPlacel, toPlace = toPlacel, fromID = fromID, toID = toID),
#   .f = otp_get_results2,
#   otpcon = otpcon,
#   p = p,
#   mode = mode,
#   date = date,
#   time = time,
#   arriveBy = arriveBy,
#   maxWalkDistance = maxWalkDistance,
#   numItineraries = numItineraries,
#   routeOptions = routeOptions,
#   full_elevation = full_elevation,
#   get_geometry = get_geometry,
#   timezone = timezone,
#   get_elevation = get_elevation,
#   .progress = FALSE,
#   .options = furrr::furrr_options(seed = TRUE)
# )
