#' Write config object as json file
#'
#' @description
#' Takes a config list produced by `otp_make_config()` and saves it as json
#' file for OTP
#'
#' @param config A named list made/modified from `otp_make_config()`
#' @param dir Path to folder where data for OTP is to be stored
#' @param router name of the router, default is "default", must be a subfolder of dir/graphs
#' @family setup
#' @examples
#' \dontrun{
#' conf <- otp_make_config("build")
#' otp_write_config(conf, dir = tempdir())
#' }
#' @export

otp_write_config <- function(config,
                             dir = NULL,
                             router = "default") {
  # Validate and Clean
  type <- attributes(config)$config_type
  otp_validate_config(config)
  config <- otp_list_clean(config)

  message(
    "The config file will be saved to ",
    file.path(dir, "graphs", router)
  )

  # Convert to JSON
  jsonlite::write_json(
    config,
    file.path(
      dir,
      "graphs",
      router,
      paste0(type, "-config.json")
    ),
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
}

#' Remove NULL values from list
#' Modified from
#' https://www.rdocumentation.org/packages/rlist/versions/0.4.6.1/topics/list.clean
#'
#' @param .data list
#' @param fun function
#'
#' @noRd

otp_list_clean <- function(.data, fun = function(x) {
                             length(x) == 0L
                           }) {
  .data <- lapply(.data, function(.item) {
    if (is.list(.item)) {
      otp_list_clean(.item, fun)
    } else {
      .item
    }
  })
  setmembers <- `[<-`
  setmembers(.data, vapply(.data, fun, logical(1L)), NULL)
}



#' Validate Config Object
#' @description
#' Checks if the list of OTP configuration options is valid
#'
#' @param config A named list made/modified from `otp_make_config()`
#' @param type type of config file
#' @family setup
#' @details
#'
#' Performs basic validity checks on class, max/min values etc as appropriate,
#' some of more complex parameters are not checked. For more details see:
#'
#' http://docs.opentripplanner.org/en/latest/Configuration
#' http://dev.opentripplanner.org/javadoc/1.3.0/org/opentripplanner/routing/core/RoutingRequest.html
#' @examples
#' \dontrun{
#' conf <- otp_make_config("build")
#' otp_validate_config(conf)
#' }
#' @export

otp_validate_config <- function(config, type = attributes(config)$config_type) {
  checkmate::assert_subset(type,
    choices = c("otp", "build", "router"),
    empty.ok = FALSE
  )

  if (type == "router") {
    # Logical
    checkmate::assert_logical(config$routingDefaults$allowBikeRental,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$arriveBy,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$batch,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$compactLegsByReversedSearch,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$disableAlertFiltering,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$disableRemainingWeightHeuristic,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$driveOnRight,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$geoidElevation,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$ignoreRealtimeUpdates,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$kissAndRide,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$longDistance,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$maxTransfers,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$onlyTransitTrips,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$parkAndRide,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$reverseOptimizeOnTheFly,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$reverseOptimizing,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$showIntermediateStops,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$softPreTransitLimiting,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$softWalkLimiting,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$useBikeRentalAvailabilityInformation,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$useRequestedDateTimeInMaxHours,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$useTraffic,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$walkingBike,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$routingDefaults$wheelchairAccessible,
      len = 1, null.ok = TRUE
    )

    # Integer
    checkmate::assert_integer(config$routingDefaults$alightSlack,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeBoardCost,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeSwitchCost,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeSwitchTime,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_integer(config$routingDefaults$carDropoffTime,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$elevatorBoardCost,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeParkAndRide,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeParkCost,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeParkTime,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeRentalDropoffCost,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeRentalDropoffTime,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeRentalPickupCost,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeRentalPickupTime,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$bikeWalkingOptions,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$elevatorBoardTime,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$elevatorHopCost,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$elevatorHopTime,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$maxPreTransitTime,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$MIN_SIMILARITY,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$nonpreferredTransferPenalty,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$numItineraries,
      len = 1, null.ok = TRUE, lower = 1
    )
    checkmate::assert_integer(config$routingDefaults$otherThanPreferredRoutesPenalty,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$transferPenalty,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$transferSlack,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$useUnpreferredRoutesPenalty,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_integer(config$routingDefaults$walkBoardCost,
      len = 1, null.ok = TRUE
    )



    # Character
    checkmate::assert_character(config$routingDefaults$bannedAgencies,
      null.ok = TRUE
    )
    checkmate::assert_character(config$routingDefaults$bannedRoutes,
      null.ok = TRUE
    )
    checkmate::assert_character(config$routingDefaults$bannedStops,
      null.ok = TRUE
    )
    checkmate::assert_character(config$routingDefaults$bannedStopsHard,
      null.ok = TRUE
    )
    checkmate::assert_character(config$routingDefaults$bannedTrips,
      null.ok = TRUE
    )
    checkmate::assert_character(config$routingDefaults$boardSlack,
      null.ok = TRUE
    )
    checkmate::assert_character(config$routingDefaults$preferredAgencies,
      null.ok = TRUE
    )
    checkmate::assert_character(config$routingDefaults$preferredRoutes,
      null.ok = TRUE
    )

    # Numeric
    checkmate::assert_numeric(config$routingDefaults$bikeSpeed,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_numeric(config$routingDefaults$carSpeed,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_numeric(config$routingDefaults$clampInitialWait,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$carAccelerationSpeed,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_numeric(config$routingDefaults$carDecelerationSpeed,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_numeric(config$routingDefaults$maxHours,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_numeric(config$routingDefaults$maxSlope,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_numeric(config$routingDefaults$maxTransferWalkDistance,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_numeric(config$routingDefaults$maxWalkDistance,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_numeric(config$routingDefaults$maxWeight,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$preTransitOverageRate,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$preTransitPenalty,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$softWalkOverageRate,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$softWalkPenalty,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$stairsReluctance,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$triangleSafetyFactor,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$triangleSlopeFactor,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$triangleTimeFactor,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$turnReluctance,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$waitAtBeginningFactor,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$waitReluctance,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$walkReluctance,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$routingDefaults$walkSpeed,
      len = 1, null.ok = TRUE
    )

    # Not Checked
    not_checked <- c(
      "worstTime",
      "unpreferredAgencies",
      "unpreferredRoutes",
      "traversalCostModel",
      "startingTransitStopId",
      "startingTransitTripId",
      "to",
      "routerId",
      "dateTime",
      "dominanceFunction",
      "extensions",
      "intermediatePlaces",
      "locale",
      "modes",
      "optimize",
      "rctx"
    )
    message(paste0(
      "The folloing values where not checked: ",
      paste(not_checked, collapse = ", ")
    ))
  } else if (type == "build") {
    # logical
    checkmate::assert_logical(config$transit,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$streets,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$embedRouterConfig,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$areaVisibility,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$staticParkAndRide,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$htmlAnnotations,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$useTransfersTxt,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$parentStopLinking,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$stationTransfers,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$platformEntriesLinking,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$matchBusRoutesToStreets,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$fetchElevationUS,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$staticBikeRental,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$staticBikeParkAndRide,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$banDiscouragedWalking,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$banDiscouragedBiking,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_logical(config$extraEdgesStopPlatformLink,
      len = 1, null.ok = TRUE
    )

    # Interger
    checkmate::assert_integer(config$maxHtmlAnnotationsPerFile,
      len = 1, null.ok = TRUE, lower = 1
    )
    checkmate::assert_integer(config$maxInterlineDistance,
      len = 1, null.ok = TRUE, lower = 1
    )
    checkmate::assert_integer(config$islandWithStopsMaxSize,
      len = 1, null.ok = TRUE, lower = 1
    )
    checkmate::assert_integer(config$islandWithoutStopsMaxSize,
      len = 1, null.ok = TRUE, lower = 1
    )

    # Numeric
    checkmate::assert_numeric(config$elevationUnitMultiplier,
      len = 1, null.ok = TRUE
    )
    checkmate::assert_numeric(config$subwayAccessTime,
      len = 1, null.ok = TRUE, lower = 0
    )
    checkmate::assert_numeric(config$maxTransferDistance,
      len = 1, null.ok = TRUE,
      lower = 0
    )

    # Character
    checkmate::assert_subset(config$osmWayPropertySet,
      empty.ok = TRUE,
      choices = c("default", "norway")
    )
    checkmate::assert_subset(config$stopClusterMode,
      empty.ok = TRUE,
      choices = c("proximity", "parentStation")
    )
  } else if (type == "otp") {
    otp_validate_config(config, type = "build")
    otp_validate_config(config, type = "router")
  }
  return(TRUE)
}

#' Make Config Object
#' @description
#' OTP can be configured using three json files `otp-config.json`,
#' `build-config.json`, and `router-config.json`. This function
#' creates a named list for each config file and
#' populates the defaults values.
#'
#' @param type Which type of config file to create, "otp", "build", "router"
#' @family setup
#' @details
#' For more details see:
#' http://docs.opentripplanner.org/en/latest/Configuration
#' @examples
#' {
#'   conf <- otp_make_config("build")
#'   conf <- otp_make_config("router")
#' }
#' @export

otp_make_config <- function(type) {
  checkmate::assert_subset(type,
    choices = c("otp", "build", "router"),
    empty.ok = FALSE
  )

  if (type == "otp") {
    config_router <- otp_make_config("router")
    config_build <- otp_make_config("build")

    config <- c(config_router, config_build)
  } else if (type == "build") {
    config_names <- c(
      "htmlAnnotations", "transit", "useTransfersTxt",
      "parentStopLinking", "stationTransfers",
      "stopClusterMode", "subwayAccessTime", "streets",
      "embedRouterConfig", "areaVisibility", "platformEntriesLinking",
      "matchBusRoutesToStreets", "fetchElevationUS", "elevationBucket",
      "elevationUnitMultiplier", "fares", "osmNaming",
      "osmWayPropertySet", "staticBikeRental", "staticParkAndRide",
      "staticBikeParkAndRide", "maxHtmlAnnotationsPerFile",
      "maxInterlineDistance", "islandWithoutStopsMaxSize",
      "islandWithStopsMaxSize", "banDiscouragedWalking",
      "banDiscouragedBiking", "maxTransferDistance",
      "extraEdgesStopPlatformLink"
    )
    config <- rep(list(NULL), times = length(config_names))
    names(config) <- config_names
    # Assign Default Values
    # Logical
    config[c(
      "transit", "streets", "embedRouterConfig",
      "areaVisibility", "staticParkAndRide"
    )] <- TRUE
    config[c(
      "htmlAnnotations", "useTransfersTxt", "parentStopLinking",
      "stationTransfers", "platformEntriesLinking",
      "matchBusRoutesToStreets", "fetchElevationUS", "staticBikeRental",
      "staticBikeParkAndRide", "banDiscouragedWalking",
      "banDiscouragedBiking", "extraEdgesStopPlatformLink"
    )] <- FALSE

    # Interger
    config[["maxHtmlAnnotationsPerFile"]] <- 1000L
    config[["maxInterlineDistance"]] <- 200L
    config[["islandWithStopsMaxSize"]] <- 5L
    config[["islandWithoutStopsMaxSize"]] <- 40L

    # Numeric
    config[["elevationUnitMultiplier"]] <- 1
    config[["subwayAccessTime"]] <- 2
    config[["maxTransferDistance"]] <- 2000

    # Character
    config[["osmWayPropertySet"]] <- "default"
    config[["stopClusterMode"]] <- "proximity"

    # Other
    config["elevationBucket"] <- NULL
    config["fares"] <- NULL
    config["osmNaming"] <- NULL
  } else if (type == "router") {
    config_names <- c(
      "routingDefaults", "timeout", "timeouts",
      "requestLogFile", "boardTimes",
      "alightTimes", "updaters"
    )
    config <- rep(list(NULL), times = length(config_names))
    names(config) <- config_names

    routingDefaults_names <- c(
      "alightSlack",
      "allowBikeRental",
      "arriveBy",
      "bannedAgencies",
      "bannedRoutes",
      "bannedStops",
      "bannedStopsHard",
      "bannedTrips",
      "batch",
      "bikeBoardCost",
      "bikeParkAndRide",
      "bikeParkCost",
      "bikeParkTime",
      "bikeRentalDropoffCost",
      "bikeRentalDropoffTime",
      "bikeRentalPickupCost",
      "bikeRentalPickupTime",
      "bikeSpeed",
      "bikeSwitchCost",
      "bikeSwitchTime",
      "bikeWalkingOptions",
      "boardSlack",
      "carAccelerationSpeed",
      "carDecelerationSpeed",
      "carDropoffTime",
      "carSpeed",
      "clampInitialWait",
      "compactLegsByReversedSearch",
      "dateTime",
      "disableAlertFiltering",
      "disableRemainingWeightHeuristic",
      "dominanceFunction",
      "driveOnRight",
      "elevatorBoardCost",
      "elevatorBoardTime",
      "elevatorHopCost",
      "elevatorHopTime",
      "extensions",
      "from",
      "geoidElevation",
      "ignoreRealtimeUpdates",
      "intermediatePlaces",
      "kissAndRide",
      "locale",
      "longDistance",
      "maxHours",
      "maxPreTransitTime",
      "maxSlope",
      "maxTransfers",
      "maxTransferWalkDistance",
      "maxWalkDistance",
      "maxWeight",
      "MIN_SIMILARITY",
      "modes",
      "nonpreferredTransferPenalty",
      "numItineraries",
      "onlyTransitTrips",
      "optimize",
      "otherThanPreferredRoutesPenalty",
      "parameters",
      "parkAndRide",
      "preferredAgencies",
      "preferredRoutes",
      "preTransitOverageRate",
      "preTransitPenalty",
      "rctx",
      "reverseOptimizeOnTheFly",
      "reverseOptimizing",
      "routerId",
      "showIntermediateStops",
      "softPreTransitLimiting",
      "softWalkLimiting",
      "softWalkOverageRate",
      "softWalkPenalty",
      "stairsReluctance",
      "startingTransitStopId",
      "startingTransitTripId",
      "to",
      "transferPenalty",
      "transferSlack",
      "traversalCostModel",
      "triangleSafetyFactor",
      "triangleSlopeFactor",
      "triangleTimeFactor",
      "turnReluctance",
      "unpreferredAgencies",
      "unpreferredRoutes",
      "useBikeRentalAvailabilityInformation",
      "useRequestedDateTimeInMaxHours",
      "useTraffic",
      "useUnpreferredRoutesPenalty",
      "waitAtBeginningFactor",
      "waitReluctance",
      "walkBoardCost",
      "walkingBike",
      "walkReluctance",
      "walkSpeed",
      "wheelchairAccessible",
      "worstTime"
    )

    routingDefaults <- rep(list(NULL),
      times = length(routingDefaults_names)
    )
    names(routingDefaults) <- routingDefaults_names
    routingDefaults[["bikeSpeed"]] <- 5
    routingDefaults[["bikeSwitchCost"]] <- 0L
    routingDefaults[["bikeSwitchTime"]] <- 0L
    routingDefaults[["clampInitialWait"]] <- -1
    routingDefaults[["walkReluctance"]] <- 2
    routingDefaults[["walkSpeed"]] <- 1.34


    config[["routingDefaults"]] <- routingDefaults

    config[["timeout"]] <- NULL
    config[["timeouts"]] <- c(5, 4, 3, 1)
    config[["requestLogFile"]] <- NULL

    boardTimes <- rep(list(NULL), times = 2)
    names(boardTimes) <- c("boardTimes", "alightTimes")
    boardTimes_sub <- list(NULL)
    names(boardTimes_sub) <- "AIRPLANE"
    alightTimes_sub <- list(NULL)
    names(alightTimes_sub) <- "AIRPLANE"
    boardTimes[["boardTimes"]] <- boardTimes_sub
    boardTimes[["alightTimes"]] <- alightTimes_sub

    config[["boardTimes"]] <- boardTimes
    config[["updaters"]] <- NULL
  }

  att <- list(type)
  names(att) <- "config_type"
  attributes(config) <- c(attributes(config), att)
  return(config)
}
