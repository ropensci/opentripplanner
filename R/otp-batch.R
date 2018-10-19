#' Get multiple routes from the OTP
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric matrix of two columns, Latitude/Longitude pairs
#' @param toPlace Numeric matrix of two columns, Latitude/Longitude pairs
#' @param ... Other variaibles passed to otp_plan
#' @return
#' Returns a data.frame of SF POLYLINES
#' @export
#'
#' @detials
#' This function is a batch version of otp_plan() and is useful if you want to produce many routes at once.
#'
otp_plan_batch <- function(otpcon = NA,
                     fromPlace = NA,
                     toPlace = NA,
                     ...)
{
  # Check Valid Inputs
  if(!"otpconnect" %in% class(otpcon)){
    message("otpcon is not a valid otpconnect object")
    stop()
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
      message("toPlace coordinates excced valid values +/- 90 and +/- 180 degrees")
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


  results <- pbapply::pblapply(seq(1,nrow(fromPlace)),
                               get_resutls,
                               otpcon = otpcon,
                               fromPlace = fromPlace,
                               toPlace = toPlace,
                               ... = ...
                               )


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
    message("Some errors occured")
    print(results_errors)
  }
  return(results_routes)
}
