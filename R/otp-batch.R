#' Get multiple routes from the OTP
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric matrix of two columns, Latitude/Longitude pairs
#' @param toPlace Numeric matrix of two columns, Latitude/Longitude pairs
#' @param ncores numeric, number of cores to use in parallel processing, default is 1
#' @param ... Other variables passed to otp_plan
#' @return
#' Returns a data.frame of SF POLYLINES
#' @export
#'
#' @details
#' This function is a batch version of otp_plan() and is useful if you want to produce many routes at once.
#'
otp_plan_batch <- function(otpcon = NA,
                            fromPlace = NA,
                            toPlace = NA,
                            ncores = 1,
                            ...)
{
  # Check Valid Inputs
  if(!"otpconnect" %in% class(otpcon)){
    message("otpcon is not a valid otpconnect object")
    stop()
  }
  if("sf" %in% class(fromPlace)){
    if(all(sf::st_geometry_type(fromPlace) == "POINT")){
      fromPlace <- sf::st_coordinates(fromPlace)
      fromPlace <- fromPlace[,c(2,1)]
    }else{
      message("fromPlace contains non-POINT geometry")
      stop()
    }
  }

  if("sf" %in% class(toPlace)){
    if(all(sf::st_geometry_type(toPlace) == "POINT")){
      toPlace <- sf::st_coordinates(toPlace)
      toPlace <- toPlace[,c(2,1)]
    }else{
      message("toPlace contains non-POINT geometry")
      stop()
    }
  }

  if(class(fromPlace) != "matrix" | ncol(fromPlace) != 2){
    message("fromPlace is not a valid matrix of latitude, longitude pairs")
    stop()
  }else{
    if(max(fromPlace[,1]) <= 90 & min(fromPlace[,1]) >= -90 &  max(fromPlace[,2]) <= 180 & min(fromPlace[,2]) >= -180){

    }else{
      message("fromPlace coordinates excced valid values +/- 90 and +/- 180 degrees")
      stop()
    }

  }
  if(class(toPlace) != "matrix" | ncol(toPlace) != 2){
    message("toPlace is not a valid matrix of latitude, longitude pairs")
    stop()
  }else{
    if(max(toPlace[,1]) <= 90 & min(toPlace[,1]) >= -90 &  max(toPlace[,2]) <= 180 & min(toPlace[,2]) >= -180){

    }else{
      message("toPlace coordinates exceed valid values +/- 90 and +/- 180 degrees")
      stop()
    }

  }

  if(nrow(fromPlace) != nrow(fromPlace)){
    message("Number of fromPlaces and toPlaces do not match")
    stop()
  }

  get_resutls <- function(x,otpcon, fromPlace, toPlace, ...){
    res <- otp_plan(otpcon = otpcon,
                    fromPlace = fromPlace[x,],
                    toPlace = toPlace[x,],
                    ... )
    res$fromPlace = paste(fromPlace[x,],collapse = ",")
    res$toPlace = paste(toPlace[x,],collapse = ",")
    return(res)
  }

  if(ncores > 1){
    cl = parallel::makeCluster(ncores)
    parallel::clusterExport(cl = cl,
                            varlist = c("otpcon","fromPlace","toPlace"),
                            envir = environment())
    parallel::clusterEvalQ(cl, {
      library(opentripplanner)
    })
    pbapply::pboptions(use_lb=TRUE)
    results <- pbapply::pblapply(seq(1,nrow(fromPlace)),
                                 get_resutls,
                                 otpcon = otpcon,
                                 fromPlace = fromPlace,
                                 toPlace = toPlace,
                                 ... = ...,
                                 cl = cl
    )
    parallel::stopCluster(cl)
    rm(cl)

  }else{
    results <- pbapply::pblapply(seq(1,nrow(fromPlace)),
                                 get_resutls,
                                 otpcon = otpcon,
                                 fromPlace = fromPlace,
                                 toPlace = toPlace,
                                 ... = ...
    )
  }



  results_class <- sapply(results,function(x){"sf" %in% class(x)})
  if(all(results_class)){
    results_routes <- results[results_class]
    results_errors <- NA
  }else if(all(!results_class)){
    results_routes <- NA
    results_errors <- results[!results_class]
  }else{
    results_routes <- results[results_class]
    results_errors <- results[!results_class]
  }

  # Bind together
  suppressWarnings(results_routes <- dplyr::bind_rows(results_routes))
  results_routes <- as.data.frame(results_routes)
  results_routes$geometry <- sf::st_sfc(results_routes$geometry)
  results_routes <- sf::st_sf(results_routes)
  sf::st_crs(results_routes) <- 4326

  if(length(results_errors) > 1){
    message("Some errors occurred")
    print(results_errors)
  }
  return(results_routes)
}
