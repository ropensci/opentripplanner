#' Make routingOptions object
#' @description
#' OTP supports a wide selection of routing options `otp_plan()` accepts a
#' named list of these options. This function produces an empty named list
#' of valid options supported by both this package and OTP.
#'
#' @family routing
#' @details
#' Supports almost all of the possible options in OTP 1.4. Note that some
#' of the most popular option (mode, date, time, etc.) are set directly
#' in `otp_plan()`. If you want to permenaty set an option many are supported
#' in the config files, see help on `otp_make_config()`.
#'
#' http://dev.opentripplanner.org/apidoc/1.4.0/resource_PlannerResource.html
#' @examples
#' \dontrun{
#' routingOptions <- otp_routing_options()
#' routingOptions$walkSpeed <- 1.5
#' routingOptions <- otp_validate_routing_options(routingOptions)
#' }
#' @export
otp_routing_options <- function(){
  opts <- list(alightSlack = NULL,
              bannedAgencies = NULL,
              bannedRoutes = NULL,
              bannedStops = NULL,
              bannedStopsHard = NULL,
              bannedTrips = NULL,
              batch = NULL,
              bikeBoardCost = NULL,
              bikeSpeed = NULL,
              bikeSwitchCost = NULL,
              bikeSwitchTime = NULL,
              boardSlack = NULL,
              clampInitialWait = NULL,
              disableAlertFiltering  = NULL,
              disableRemainingWeightHeuristic = NULL,
              flexFlagStopBufferSize = NULL,
              flexIgnoreDrtAdvanceBookMin = NULL,
              flexUseEligibilityServices = NULL,
              flexUseReservationServices = NULL,
              geoidElevation = NULL,
              ignoreRealtimeUpdates = NULL,
              #intermediatePlaces = NULL,
              #locale = NULL,
              maxHours = NULL,
              maxPreTransitTime = NULL,
              maxTransfers = NULL,
              minTransferTime = NULL,
              nonpreferredTransferPenalty = NULL,
              optimize = NULL,
              otherThanPreferredRoutesPenalty = NULL,
              #pathComparator = NULL,
              preferredAgencies = NULL,
              preferredRoutes = NULL,
              reverseOptimizeOnTheFly = NULL,
              showIntermediateStops = NULL,
              startTransitStopId = NULL,
              startTransitTripId = NULL,
              transferPenalty  = NULL,
              triangleSafetyFactor = NULL,
              triangleSlopeFactor = NULL,
              triangleTimeFactor = NULL,
              unpreferredAgencies = NULL,
              unpreferredRoutes = NULL,
              useRequestedDateTimeInMaxHours = NULL,
              waitAtBeginningFactor = NULL,
              waitReluctance = NULL,
              walkBoardCost = NULL,
              walkReluctance = NULL,
              walkSpeed = NULL,
              wheelchair = NULL,
              whiteListedAgencies = NULL,
              whiteListedRoutes = NULL)
  return(opts)
}

#' Validate routingOptions object
#' @description
#' OTP supports a wide selection of routing options `otp_plan()` accepts a
#' named list of these options. This function validates a named list of inputs
#' and removes any empty inputs.
#' @param opts a named list of options possibly from `otp_routing_options()`
#' @family routing
#' @details
#' Supports almost all of the possible options in OTP 1.4. Note that some
#' of the most popular option (mode, date, time, etc.) are set directly
#' in `otp_plan()`. If you want to permenaty set an option many are supported
#' in the config files, see help on `otp_make_config()`.
#' http://dev.opentripplanner.org/apidoc/1.4.0/resource_PlannerResource.html
#' @examples
#' \dontrun{
#' routingOptions <- otp_routing_options()
#' routingOptions$walkSpeed <- 1.5
#' routingOptions <- otp_validate_routing_options(routingOptions)
#' }
#' @export

otp_validate_routing_options <- function(opts){

  checkmate::assert_list(opts)

  # Check the names are correct
  names_list <- c("alightSlack","bannedAgencies","bannedRoutes","bannedStops",
                  "bannedStopsHard","bannedTrips","batch","bikeBoardCost",
                  "bikeSpeed","bikeSwitchCost","bikeSwitchTime",
                  "boardSlack","clampInitialWait","disableAlertFiltering",
                  "disableRemainingWeightHeuristic","flexFlagStopBufferSize",
                  "flexIgnoreDrtAdvanceBookMin","flexUseEligibilityServices",
                  "flexUseReservationServices","geoidElevation",
                  "ignoreRealtimeUpdates",
                  "maxHours","maxPreTransitTime",
                  "maxTransfers","minTransferTime",
                  "nonpreferredTransferPenalty",
                  "optimize","otherThanPreferredRoutesPenalty",
                  "preferredAgencies",
                  "preferredRoutes","reverseOptimizeOnTheFly",
                  "showIntermediateStops","startTransitStopId",
                  "startTransitTripId","transferPenalty",
                  "triangleSafetyFactor","triangleSlopeFactor",
                  "triangleTimeFactor","unpreferredAgencies",
                  "unpreferredRoutes","useRequestedDateTimeInMaxHours",
                  "waitAtBeginningFactor","waitReluctance","walkBoardCost",
                  "walkReluctance","walkSpeed","wheelchair","whiteListedAgencies",
                  "whiteListedRoutes")
  names_unknown <- names(opts)[!names(opts) %in% names_list]
  if(length(names_unknown) > 0){
    stop("Unknown routeOptions: ",paste(names_unknown, collapse = ", "))
  }

  # Validate Values
  checkmate::assert_logical(opts$batch,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$disableAlertFiltering,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$disableRemainingWeightHeuristic,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$geoidElevation,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$ignoreRealtimeUpdates,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$maxTransfers,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$reverseOptimizeOnTheFly,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$showIntermediateStops,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$useRequestedDateTimeInMaxHours,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$wheelchair,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$flexIgnoreDrtAdvanceBookMin,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$flexUseEligibilityServices,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_logical(opts$flexUseReservationServices,
                            len = 1, null.ok = TRUE
  )


  # Integer
  checkmate::assert_integer(opts$alightSlack,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_integer(opts$bikeBoardCost,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_integer(opts$bikeSwitchCost,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_integer(opts$bikeSwitchTime,
                            len = 1, null.ok = TRUE, lower = 0
  )
  checkmate::assert_integer(opts$maxPreTransitTime,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_integer(opts$nonpreferredTransferPenalty,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_integer(opts$otherThanPreferredRoutesPenalty,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_integer(opts$transferPenalty,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_integer(opts$walkBoardCost,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_integer(opts$minTransferTime,
                            len = 1, null.ok = TRUE
  )


  # Character
  checkmate::assert_character(opts$bannedAgencies,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$bannedRoutes,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$bannedStops,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$bannedStopsHard,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$bannedTrips,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$boardSlack,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$preferredAgencies,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$preferredRoutes,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$optimize,
                              len = 1,
                              null.ok = TRUE
  )
  checkmate::assert_choice(opts$optimize,
                              choices = c("FLAT","GREENWAYS",
                                          "QUICK","SAFE",
                                          "TRANSFERS","TRIANGLE"),
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$startTransitStopId,
                              len = 1,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$startTransitTripId,
                              len = 1,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$unpreferredAgencies,
                              len = 1,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$unpreferredRoutes,
                              len = 1,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$whiteListedAgencies,
                              len = 1,
                              null.ok = TRUE
  )
  checkmate::assert_character(opts$whiteListedRoutes,
                              len = 1,
                              null.ok = TRUE
  )

  # Numeric
  checkmate::assert_numeric(opts$bikeSpeed,
                            len = 1, null.ok = TRUE, lower = 0
  )
  checkmate::assert_numeric(opts$clampInitialWait,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_numeric(opts$maxHours,
                            len = 1, null.ok = TRUE, lower = 0
  )
  checkmate::assert_numeric(opts$triangleSafetyFactor,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_numeric(opts$triangleSlopeFactor,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_numeric(opts$triangleTimeFactor,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_numeric(opts$waitAtBeginningFactor,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_numeric(opts$waitReluctance,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_numeric(opts$walkReluctance,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_numeric(opts$walkSpeed,
                            len = 1, null.ok = TRUE
  )
  checkmate::assert_numeric(opts$flexFlagStopBufferSize,
                            len = 1, null.ok = TRUE
  )

  opts <- opts[lengths(opts) > 0]
  if(length(opts) == 0){
    opts <- NULL
  }
  return(opts)
}
