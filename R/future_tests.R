#'
#'
#'
#' library(opentripplanner)
#'
#' otpcon <- otp_connect(router = "great-britain-NTEM", port = 8091)
#' dl <- readRDS("C:/Users/earmmor/Downloads/desire_lines_scotland.Rds")
#' summary(dl$dist_euclidean)
#' dl <- dl[dl$dist_euclidean < 30000, ]
#'
#' fromPlace <- lwgeom::st_startpoint(dl)
#' toPlace <- lwgeom::st_endpoint(dl)
#'
#' fromPlace <- st_as_sf(data.frame(id = dl$geo_code1, geometry = fromPlace))
#' toPlace <- st_as_sf(data.frame(id = dl$geo_code2, geometry = toPlace))
#'
#' fromID = fromPlace$id
#' toID = toPlace$id
#'
#' timezone = otpcon$timezone
#' mode = "BICYCLE"
#' date_time = Sys.time()
#' date <- format(date_time, "%m-%d-%Y", tz = timezone)
#' time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
#' arriveBy = FALSE
#' maxWalkDistance = 1000
#' numItineraries = 3
#' routeOptions = NULL
#' full_elevation = FALSE
#' get_geometry = TRUE
#' ncores = 10
#'
#' distance_balance = FALSE
#' get_elevation = FALSE
#'
#' fromPlace <- opentripplanner:::otp_clean_input(fromPlace, "fromPlace")
#' toPlace <- opentripplanner:::otp_clean_input(toPlace, "toPlace")
#'
#' # n = 10000
#' # fromPlace <- fromPlace[1:n,]
#' # toPlace <- toPlace[1:n,]
#' # fromID = fromID[1:n]
#' # toID = toID[1:n]
#'
#' # optcon2 <- list()
#' # for(i in 1:2){
#' #   optcon2[[i]] <- otpcon
#' # }
#'
#' library(future)
#' library(furrr)
#'
#' message("Starting at ",Sys.time()," with ",ncores," workers")
#' future::plan(future::multisession, workers = ncores)
#' results = furrr::future_pmap(.l = list(fromPlace_lat = unname(fromPlace[,1]),
#'                          fromPlace_lng = unname(fromPlace[,2]),
#'                          toPlace_lat = unname(toPlace[,1]),
#'                          toPlace_lng = unname(toPlace[,2]),
#'                          fromID = fromID,
#'                          toID = toID
#'                         ),
#'                              .f = otp_get_results2,
#'                                   otpcon = otpcon,
#'                                  mode = mode,
#'                                  date = date,
#'                                  time = time,
#'                                  arriveBy = arriveBy,
#'                                  maxWalkDistance = maxWalkDistance,
#'                                  numItineraries = numItineraries,
#'                                  routeOptions = routeOptions,
#'                                  full_elevation = full_elevation,
#'                                  get_geometry = get_geometry,
#'                                  get_elevation = get_elevation,
#'                                  timezone = timezone,
#'                                   .options = furrr::furrr_options(seed=TRUE,
#'                                                                   scheduling = 1),
#'                                  .progress = TRUE)
#' future::plan(future::sequential)
#' message("\nFinished at ",Sys.time())
#'
#' results_class <- unlist(lapply(results, function(x) {
#'   "data.frame" %in% class(x)
#' }), use.names = FALSE)
#' if (all(results_class)) {
#'   results_routes <- results[results_class]
#'   results_errors <- NA
#' } else if (all(!results_class)) {
#'   results_routes <- NA
#'   results_errors <- results[!results_class]
#' } else {
#'   results_routes <- results[results_class]
#'   results_errors <- results[!results_class]
#' }
#'
#' # Bind together
#' if (!all(class(results_routes) == "logical")) {
#'   if (any(unlist(lapply(results, function(x) {
#'     "sf" %in% class(x)
#'   }), use.names = FALSE))) {
#'     results_routes <- data.table::rbindlist(results_routes, fill = TRUE)
#'     results_routes <- opentripplanner:::list2df(results_routes)
#'     results_routes <- opentripplanner:::df2sf(results_routes)
#'     # fix for bbox error from data.table
#'     results_routes <- results_routes[seq_len(nrow(results_routes)), ]
#'     colnms <- names(results_routes)
#'     colnms <- colnms[!colnms %in% c("fromPlace", "toPlace", "geometry")]
#'     results_routes <- results_routes[c("fromPlace", "toPlace", colnms, "geometry")]
#'   } else {
#'     results_routes <- data.table::rbindlist(results_routes, fill = TRUE)
#'   }
#' }
#'
#' qtm(results_routes)
#'
#'
#' otp_get_results2 <- function(x, otpcon, fromPlace_lat, fromPlace_lng, toPlace_lat, toPlace_lng,
#'                              fromID, toID,
#'                              mode,
#'                              date  ,
#'                              time  ,
#'                              arriveBy  ,
#'                              maxWalkDistance  ,
#'                              numItineraries  ,
#'                              routeOptions  ,
#'                              full_elevation  ,
#'                              get_geometry  ,
#'                              get_elevation  ,
#'                              timezone  ) {
#'
#'   #otpcon3 = unlist(otpcon, recursive = FALSE)
#'   class(otpcon) <- "otpconnect"
#'   #message(str(otpcon))
#'
#'   fromPlace = matrix(c(fromPlace_lat, fromPlace_lng), ncol = 2)
#'   toPlace = matrix(c(toPlace_lat, toPlace_lng), ncol = 2)
#'
#'   #print(fromPlace)
#'
#'   res <- try(otp_plan_internal2(x,
#'     otpcon = otpcon,
#'     fromPlace = fromPlace,
#'     toPlace = toPlace,
#'     fromID = fromID,
#'     toID = toID,
#'     mode = mode,
#'     date = date,
#'     time = time,
#'     arriveBy = arriveBy,
#'     maxWalkDistance = maxWalkDistance,
#'     numItineraries = numItineraries,
#'     routeOptions = routeOptions,
#'     full_elevation = full_elevation,
#'     get_geometry = get_geometry,
#'     get_elevation = get_elevation,
#'     timezone = timezone
#'   ), silent = FALSE)
#'
#'   if ("try-error" %in% class(res)) {
#'     res <- paste0(
#'       "Try Error occured for ",
#'       paste(fromPlace_lat[x], collapse = ","),
#'       " ",
#'       paste(fromPlace_lng[x], collapse = ","),
#'       " ",
#'       res[[1]]
#'     )
#'     warning(res)
#'   }
#'
#'   return(res)
#' }
#'
#' otp_plan_internal2 <- function(x = NULL,
#'                               otpcon = NA,
#'                               fromPlace = NA,
#'                               toPlace = NA,
#'                               fromID = NULL,
#'                               toID = NULL,
#'                               mode = "CAR",
#'                               date = date,
#'                               time = time,
#'                               arriveBy = FALSE,
#'                               maxWalkDistance = 1000,
#'                               numItineraries = 3,
#'                               routeOptions = NULL,
#'                               full_elevation = FALSE,
#'                               get_geometry = TRUE,
#'                               timezone = "",
#'                               get_elevation = FALSE) {
#'
#'
#'   # Construct URL
#'   routerUrl <- opentripplanner:::make_url(otpcon)
#'   routerUrl <- paste0(routerUrl, "/plan")
#'
#'   fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
#'   toPlace <- format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)
#'
#'   fromPlace <- paste(fromPlace, collapse = ",")
#'   toPlace <- paste(toPlace, collapse = ",")
#'
#'   query <- list(
#'     fromPlace = fromPlace,
#'     toPlace = toPlace,
#'     mode = mode,
#'     date = date,
#'     time = time,
#'     maxWalkDistance = maxWalkDistance,
#'     arriveBy = arriveBy,
#'     numItineraries = numItineraries
#'   )
#'
#'   if (otpcon$otp_version >= 2) {
#'     # maxWalkDistance causes itinaries to fail
#'     if (mode == "CAR" | grepl("TRANSIT", mode)) {
#'       query$maxWalkDistance <- NULL
#'     }
#'   }
#'
#'   if (!is.null(routeOptions)) {
#'     query <- c(query, routeOptions)
#'   }
#'
#'   url <- opentripplanner:::build_url(routerUrl, query)
#'   #message(url)
#'   text <- curl::curl_fetch_memory(url)
#'   text <- rawToChar(text$content)
#'
#'   asjson <- try(RcppSimdJson::fparse(text, query = "/plan/itineraries"),
#'                 silent = TRUE
#'   )
#'
#'   # Check for errors - if no error object, continue to process content
#'   if (!"try-error" %in% class(asjson) & !is.null(asjson)) {
#'     response <- opentripplanner:::otp_json2sf(asjson, full_elevation, get_geometry, timezone, get_elevation)
#'     # Add Ids
#'     if (is.null(fromID)) {
#'       response$fromPlace <- fromPlace
#'     } else {
#'       response$fromPlace <- fromID
#'     }
#'     if (is.null(toID)) {
#'       response$toPlace <- toPlace
#'     } else {
#'       response$toPlace <- toID
#'     }
#'     return(response)
#'   } else {
#'     asjson <- RcppSimdJson::fparse(text)
#'     # there is an error - return the error code and message
#'     if (is.null(asjson$error)) {
#'       response <- paste0(
#'         "Error: No itinary returned",
#'         " from ", asjson$`requestParameters`$fromPlace,
#'         " to ", asjson$`requestParameters`$toPlace
#'       )
#'     } else {
#'       response <- paste0(
#'         "Error: ", asjson$error$id,
#'         " from ", asjson$`requestParameters`$fromPlace,
#'         " to ", asjson$`requestParameters`$toPlace,
#'         " ", asjson$error$msg
#'       )
#'     }
#'     return(response)
#'   }
#' }
#'
#'
#' otp_async_old <- function(){
#'
#'   routerUrl <- opentripplanner:::make_url(otpcon)
#'   routerUrl <- paste0(routerUrl, "/plan")
#'
#'   fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
#'   toPlace <- format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)
#'
#'   fromPlace <- paste0(fromPlace[,1],"%2C",fromPlace[,2])
#'   toPlace <- paste0(toPlace[,1],"%2C",toPlace[,2])
#'
#'   query <- list(
#'     mode = mode,
#'     date = date,
#'     time = time,
#'     maxWalkDistance = maxWalkDistance,
#'     arriveBy = arriveBy,
#'     numItineraries = numItineraries
#'   )
#'
#'   if (otpcon$otp_version >= 2) {
#'     # maxWalkDistance causes itinaries to fail
#'     if (mode == "CAR" | grepl("TRANSIT", mode)) {
#'       query$maxWalkDistance <- NULL
#'     }
#'   }
#'
#'   secs <- build_urls(routerUrl,fromPlace, toPlace, query)
#'
#'   # cb <- function(req){
#'   #   rawToChar(req$content)
#'   # }
#'
#'
#'   slow_sum <- function(x) {
#'     p <- progressr::progressor(along = x)
#'     sum <- 0
#'     for (kk in seq_along(x)) {
#'       Sys.sleep(0.1)
#'       sum <- sum + x[kk]
#'       p(message = sprintf("Added %g", x[kk]))
#'     }
#'     sum
#'   }
#'
#'   progressr::with_progress(slow_sum(1:5))
#'
#'
#'
#'
#'
#'   # out <- curl::multi_run(timeout = Inf, pool = pool)
#'
#'   myfunc = function(n){
#'     t1 <- Sys.time()
#'     p <- progressr::progressor(n)
#'     success <- function(res){
#'       #cat("Request done! Status:", res$status, "\n")
#'       p()
#'       data <<- c(data, rawToChar(res$content))
#'     }
#'     failure <- function(msg){
#'       cat("Oh noes! Request failed!", msg, "\n")
#'     }
#'     pool <- curl::new_pool(host_con = 30)
#'     data <- list()
#'     for(i in 1:n){
#'       curl::curl_fetch_multi(secs[i], success, failure , pool = pool)
#'     }
#'
#'     curl::multi_run(timeout = Inf, pool = pool)
#'     t2 <- Sys.time()
#'     message("Done in ",round(difftime(t2,t1, units = "mins"),1)," mins")
#'     return(data)
#'   }
#'   data <-  progressr::with_progress(myfunc(10000))
#'
#'   data2 <- lapply(data, function(text){
#'     asjson <- try(RcppSimdJson::fparse(text, query = "/plan/itineraries"),
#'                   silent = TRUE
#'     )
#'
#'     # Check for errors - if no error object, continue to process content
#'     if (!"try-error" %in% class(asjson) & !is.null(asjson)) {
#'       response <- opentripplanner:::otp_json2sf(asjson, full_elevation, get_geometry, timezone, get_elevation)
#'       # Add Ids
#'       # if (is.null(fromID)) {
#'       #   response$fromPlace <- fromPlace
#'       # } else {
#'       #   response$fromPlace <- fromID
#'       # }
#'       # if (is.null(toID)) {
#'       #   response$toPlace <- toPlace
#'       # } else {
#'       #   response$toPlace <- toID
#'       # }
#'       return(response)
#'     } else {
#'       asjson <- RcppSimdJson::fparse(text)
#'       # there is an error - return the error code and message
#'       if (is.null(asjson$error)) {
#'         response <- paste0(
#'           "Error: No itinary returned",
#'           " from ", asjson$`requestParameters`$fromPlace,
#'           " to ", asjson$`requestParameters`$toPlace
#'         )
#'       } else {
#'         response <- paste0(
#'           "Error: ", asjson$error$id,
#'           " from ", asjson$`requestParameters`$fromPlace,
#'           " to ", asjson$`requestParameters`$toPlace,
#'           " ", asjson$error$msg
#'         )
#'       }
#'       return(response)
#'     }
#'   })
#'
#'
#'   results_class <- unlist(lapply(data2, function(x) {
#'     "data.frame" %in% class(x)
#'   }), use.names = FALSE)
#'   if (all(results_class)) {
#'     results_routes <- data2[results_class]
#'     results_errors <- NA
#'   } else if (all(!results_class)) {
#'     results_routes <- NA
#'     results_errors <- data2[!results_class]
#'   } else {
#'     results_routes <- data2[results_class]
#'     results_errors <- data2[!results_class]
#'   }
#'
#'   results_routes <- dplyr::bind_rows(results_routes)
#'
#'   qtm(results_routes)
#'
#'
#'
#' }
#'
#' build_urls <- function (routerUrl,fromPlace, toPlace, query){
#'   secs <- unlist(query, use.names = TRUE)
#'   secs <- paste0(names(secs), "=", secs)
#'   secs <- paste(secs, collapse = "&")
#'   secs <- gsub(",", "%2C", secs)
#'   secs <- paste0(routerUrl, "?", "fromPlace=",fromPlace,"&toPlace=",toPlace,"&",secs)
#'   secs
#' }
#'
#'
#'
#' # # Path to a folder containing the OTP.jar file, change to where you saved the file.
#' # path_data <- file.path(tempdir(), "OTP")
#' # dir.create(path_data)
#' # path_otp <- otp_dl_jar(version = "1.4.0")
#' # otp_dl_demo(path_data)
#' # # Build Graph and start OTP
#' # log1 <- otp_build_graph(otp = path_otp, dir = path_data)
#' # log2 <- otp_setup(otp = path_otp, dir = path_data)
#' # otpcon <- otp_connect(timezone = "Europe/London")
#' #
#' # lsoa <- sf::st_read("https://github.com/ropensci/opentripplanner/releases/download/0.1/centroids.gpkg",
#' #                     stringsAsFactors = FALSE)
#' #
#' # toPlace   = lsoa[rep(seq(1, nrow(lsoa)), times = nrow(lsoa)),]
#' # fromPlace = lsoa[rep(seq(1, nrow(lsoa)), each  = nrow(lsoa)),]
#' #
#' # system.time({routes <- otp_plan(otpcon = otpcon,
#' #                                 fromPlace = fromPlace,
#' #                                 toPlace = toPlace,
#' #                                 fromID = fromPlace$geo_code,
#' #                                 toID = toPlace$geo_code,
#' #                                 ncores = 4)})
#' #116 seconds on 4 core CRAN 7306 routes of 7921 returned
#' #473 seconds on 4 core Futures 7310 routes of 7921 returned
#' #484 seconds on 4 core Futures 7310 routes of 7921 returned
#' #
#' #
#' # slow_func <- function(x, p){
#' #   #x <- x ** 2
#' #   p(sprintf("x=%g", x))
#' #   Sys.sleep(1)
#' #   return(x)
#' # }
#' #
#' #
#' # batch_function <- function(vals = rep(1:10, 2)){
#' #   vals <- vals[order(vals, decreasing = TRUE)]
#' #   progressr::handlers(global = TRUE)
#' #   progressr::handlers("progress")
#' #   p <- progressr::progressor(along = vals)
#' #
#' #   future::plan("future::multisession", workers = 4)
#' #   r <- future.apply::future_lapply(vals,
#' #                                    slow_func,
#' #                                    p = p,
#' #                                    future.scheduling = TRUE,
#' #                                    future.chunk.size = NULL)
#' #   future::plan("sequential")
#' #   r <- unlist(r)
#' #   return(r)
#' # }
#'
#'
#' # otp_get_results2 <- function(fromPlace, toPlace, fromID, toID, otpcon, p,
#' #                              ...) {
#' #   p()
#' #   res <- try(otp_plan_internal(
#' #     otpcon = otpcon,
#' #     fromPlace = fromPlace,
#' #     toPlace = toPlace,
#' #     fromID = fromID,
#' #     toID = toID,
#' #     ...
#' #   ), silent = TRUE)
#' #
#' #   if ("try-error" %in% class(res)) {
#' #     res <- paste0(
#' #       "Try Error occured for ",
#' #       paste(fromPlace, collapse = ","),
#' #       " ",
#' #       paste(toPlace, collapse = ","),
#' #       " ",
#' #       res[[1]]
#' #     )
#' #     warning(res)
#' #   }
#' #
#' #   return(res)
#' # }
#'
#' # future::plan("future::multisession", workers = ncores)
#' # progressr::handlers(global = TRUE)
#' # progressr::handlers("progress")
#' # xs <- seq(1, nrow(fromPlace))
#' # p <- progressr::progressor(along = xs)
#'
#'
#' # results <- future.apply::future_lapply(xs,
#' #                                        otp_get_results,
#' #                                        otpcon = otpcon,
#' #                                        fromPlace = fromPlace,
#' #                                        toPlace = toPlace,
#' #                                        fromID = fromID,
#' #                                        toID = toID,
#' #                                        p = p,
#' #                                        mode = mode,
#' #                                        date = date,
#' #                                        time = time,
#' #                                        arriveBy = arriveBy,
#' #                                        maxWalkDistance = maxWalkDistance,
#' #                                        numItineraries = numItineraries,
#' #                                        routeOptions = routeOptions,
#' #                                        full_elevation = full_elevation,
#' #                                        get_geometry = get_geometry,
#' #                                        timezone = timezone,
#' #                                        get_elevation = get_elevation,
#' #                                        future.seed = TRUE,
#' #                                        future.scheduling = TRUE,
#' #                                        future.chunk.size = NULL)
#'
#'
#'
#'
#' # fromPlacel <- split(fromPlace, 1:nrow(fromPlace))
#' # toPlacel <- split(toPlace, 1:nrow(toPlace))
#' #
#' #
#' # results <- furrr::future_pmap(
#' #   .l = list(fromPlace = fromPlacel, toPlace = toPlacel, fromID = fromID, toID = toID),
#' #   .f = otp_get_results2,
#' #   otpcon = otpcon,
#' #   p = p,
#' #   mode = mode,
#' #   date = date,
#' #   time = time,
#' #   arriveBy = arriveBy,
#' #   maxWalkDistance = maxWalkDistance,
#' #   numItineraries = numItineraries,
#' #   routeOptions = routeOptions,
#' #   full_elevation = full_elevation,
#' #   get_geometry = get_geometry,
#' #   timezone = timezone,
#' #   get_elevation = get_elevation,
#' #   .progress = FALSE,
#' #   .options = furrr::furrr_options(seed = TRUE)
#' # )
#'
#'
#' #' Get the geometry of a route from the OTP
#' #'
#' #' @param otpcon OTP connection object produced by otp_connect()
#' #' @param fromPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.529258,-0.134649)`
#' #' @param toPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.506383,-0.088780,)`
#' #' @param fromID fromID
#' #' @param toID toID
#' #' @param mode Character vector of modes of travel valid values TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR
#' #' @param date date
#' #' @param time time
#' #' @param arriveBy Logical, Whether the trip should depart or arrive at the specified date and time, default FALSE
#' #' @param maxWalkDistance Numeric passed to OTP
#' #' @param routeOptions names list passed to OTP
#' #' @param numItineraries The maximum number of possible itineraries to return
#' #' @param full_elevation Logical, should the full elevation profile be returned, default FALSE
#' #' @param get_geometry logical, should geometry be returned
#' #' @param timezone timezone to use
#' #' @param get_elevation Logical, should you get elevation
#' #' @param RcppSimdJsonVersion Logical, is RcppSimdJson Version >= 0.1.2
#' #' @family internal
#' #' @details
#' #' This function returns a SF data.frame with one row for each leg of the journey
#' #' (a leg is defined by a change in mode). For transit more than one route option may be returned
#' #' and is indicated by the route_option column.
#' #'
#' #' @noRd
#'
#' # otp_plan_internal <- function(otpcon = NA,
#' #                               fromPlace = NA,
#' #                               toPlace = NA,
#' #                               fromID = NULL,
#' #                               toID = NULL,
#' #                               mode = "CAR",
#' #                               date = date,
#' #                               time = time,
#' #                               arriveBy = FALSE,
#' #                               maxWalkDistance = 1000,
#' #                               numItineraries = 3,
#' #                               routeOptions = NULL,
#' #                               full_elevation = FALSE,
#' #                               get_geometry = TRUE,
#' #                               timezone = "",
#' #                               get_elevation = FALSE) {
#' #
#' #
#' #   # Construct URL
#' #   routerUrl <- make_url(otpcon)
#' #   routerUrl <- paste0(routerUrl, "/plan")
#' #
#' #   fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
#' #   toPlace <- format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)
#' #
#' #   fromPlace <- paste(fromPlace, collapse = ",")
#' #   toPlace <- paste(toPlace, collapse = ",")
#' #
#' #   query <- list(
#' #     fromPlace = fromPlace,
#' #     toPlace = toPlace,
#' #     mode = mode,
#' #     date = date,
#' #     time = time,
#' #     maxWalkDistance = maxWalkDistance,
#' #     arriveBy = arriveBy,
#' #     numItineraries = numItineraries
#' #   )
#' #
#' #   if (otpcon$otp_version >= 2) {
#' #     # maxWalkDistance causes itinaries to fail
#' #     if (mode == "CAR" | grepl("TRANSIT", mode)) {
#' #       query$maxWalkDistance <- NULL
#' #     }
#' #   }
#' #
#' #   if (!is.null(routeOptions)) {
#' #     query <- c(query, routeOptions)
#' #   }
#' #
#' #   url <- build_url(routerUrl, query)
#' #   text <- curl::curl_fetch_memory(url)
#' #   text <- rawToChar(text$content)
#' #
#' #   asjson <- try(RcppSimdJson::fparse(text, query = "/plan/itineraries"),
#' #                 silent = TRUE
#' #   )
#' #
#' #   # Check for errors - if no error object, continue to process content
#' #   if (!"try-error" %in% class(asjson) & !is.null(asjson)) {
#' #     response <- otp_json2sf(asjson, full_elevation, get_geometry, timezone, get_elevation)
#' #     # Add Ids
#' #     if (is.null(fromID)) {
#' #       response$fromPlace <- fromPlace
#' #     } else {
#' #       response$fromPlace <- fromID
#' #     }
#' #     if (is.null(toID)) {
#' #       response$toPlace <- toPlace
#' #     } else {
#' #       response$toPlace <- toID
#' #     }
#' #     return(response)
#' #   } else {
#' #     asjson <- RcppSimdJson::fparse(text)
#' #     # there is an error - return the error code and message
#' #     if (is.null(asjson$error)) {
#' #       response <- paste0(
#' #         "Error: No itinary returned",
#' #         " from ", asjson$`requestParameters`$fromPlace,
#' #         " to ", asjson$`requestParameters`$toPlace
#' #       )
#' #     } else {
#' #       response <- paste0(
#' #         "Error: ", asjson$error$id,
#' #         " from ", asjson$`requestParameters`$fromPlace,
#' #         " to ", asjson$`requestParameters`$toPlace,
#' #         " ", asjson$error$msg
#' #       )
#' #     }
#' #     return(response)
#' #   }
#' # }
#'
#'
#' #' Get OTP results
#' #'
#' #' helper function for otp_plan
#' #'
#' #' @param x numeric
#' #' @param otpcon otpcon
#' #' @param fromPlace fromplace
#' #' @param toPlace toPlace
#' #' @param fromID fromID
#' #' @param toID toID
#' #' @param ... all other variaibles
#' #'
#' #' @noRd
#' # otp_get_results <- function(x, otpcon, fromPlace, toPlace, fromID, toID, p,
#' #                             ...) {
#' #   #p()
#' #   res <- try(otp_plan_internal(
#' #     otpcon = otpcon,
#' #     fromPlace = fromPlace[x, ],
#' #     toPlace = toPlace[x, ],
#' #     fromID = fromID[x],
#' #     toID = toID[x],
#' #     ...
#' #   ), silent = TRUE)
#' #
#' #   if ("try-error" %in% class(res)) {
#' #     res <- paste0(
#' #       "Try Error occured for ",
#' #       paste(fromPlace, collapse = ","),
#' #       " ",
#' #       paste(toPlace, collapse = ","),
#' #       " ",
#' #       res[[1]]
#' #     )
#' #     warning(res)
#' #   }
#' #
#' #   return(res)
#' # }
