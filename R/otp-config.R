#' Write config object as json file
#'
#' @description
#' Takes a config list produced by `otp_make_config()` and saves it as json file for OTP
#'
#' @param config A named list made/modified from `otp_make_config()`
#' @param dir Path to folder where data for OTP is to be stored
#' @param router name of the router, default is "default"
#'
#' @seealso
#' `otp_make_config()`
#'
#' @export

otp_write_config <- function(config,
                             dir = NULL,
                             router = "default") {
  # Validate and Clean
  type <- attributes(config)$config_type
  otp_validate_config(config)
  config <- otp_list_clean(config)

  # Convert to JSON
  config <- jsonlite::toJSON(config, pretty = TRUE, null = "null", na = "null")
  jsonlite::write_json(config, file.path(dir, "graphs", router, paste0(type, "-config.json")))
}

#' Remove NULL values from list
#' Modified from https://www.rdocumentation.org/packages/rlist/versions/0.4.6.1/topics/list.clean
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
#' Checks if the list of OTP configiration options is valid
#'
#' @param config A named list made/modified from `otp_make_config()`
#' @param type type of config file
#'
#' @details
#'
#' Performs basic validity checks on class, max/min values etc as appropiate, some of
#' more compex parameters are not checked. For more details see:
#'
#' http://docs.opentripplanner.org/en/latest/Configuration
#' http://dev.opentripplanner.org/javadoc/1.3.0/org/opentripplanner/routing/core/RoutingRequest.html
#'
#' @export

otp_validate_config <- function(config, type = attributes(config)$config_type) {
  # checkmate::assert_list(config, any.missing = FALSE, names = "unique", null.ok = FALSEALSE)
  checkmate::assert_subset(type, choices = c("otp", "build", "router"), empty.ok = FALSE)

  if (type == "router") {
    # Logical
    checkmate::assert_logical(config$routingDefaults$allowBikeRental, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$arriveBy, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$batch, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$compactLegsByReversedSearch, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$disableAlertFiltering, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$disableRemainingWeightHeuristic, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$driveOnRight, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$geoidElevation, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$ignoreRealtimeUpdates, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$kissAndRide, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$longDistance, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$maxTransfers, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$onlyTransitTrips, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$parkAndRide, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$reverseOptimizeOnTheFly, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$routingDefaults$reverseOptimizing, len = 1, null.ok = TRUE)

    # Integer
    checkmate::assert_integer(config$routingDefaults$alightSlack, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeBoardCost, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeSwitchCost, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeSwitchTime, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_integer(config$routingDefaults$carDropoffTime, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$elevatorBoardCost, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeParkAndRide, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeParkCost, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeParkTime, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeRentalDropoffCost, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeRentalDropoffTime, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeRentalPickupCost, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeRentalPickupTime, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$bikeWalkingOptions, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$elevatorBoardTime, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$elevatorHopCost, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$elevatorHopTime, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$maxPreTransitTime, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$MIN_SIMILARITY, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$nonpreferredTransferPenalty, len = 1, null.ok = TRUE)
    checkmate::assert_integer(config$routingDefaults$numItineraries, len = 1, null.ok = TRUE, lower = 1)
    checkmate::assert_integer(config$routingDefaults$otherThanPreferredRoutesPenalty, len = 1, null.ok = TRUE)

    # Character
    checkmate::assert_character(config$routingDefaults$bannedAgencies, null.ok = TRUE)
    checkmate::assert_character(config$routingDefaults$bannedRoutes, null.ok = TRUE)
    checkmate::assert_character(config$routingDefaults$bannedStops, null.ok = TRUE)
    checkmate::assert_character(config$routingDefaults$bannedStopsHard, null.ok = TRUE)
    checkmate::assert_character(config$routingDefaults$bannedTrips, null.ok = TRUE)
    checkmate::assert_character(config$routingDefaults$boardSlack, null.ok = TRUE)
    checkmate::assert_character(config$routingDefaults$preferredAgencies, null.ok = TRUE)
    checkmate::assert_character(config$routingDefaults$preferredRoutes, null.ok = TRUE)

    # Numeric
    checkmate::assert_numeric(config$routingDefaults$bikeSpeed, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_numeric(config$routingDefaults$carSpeed, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_numeric(config$routingDefaults$clampInitialWait, len = 1, null.ok = TRUE)
    checkmate::assert_numeric(config$routingDefaults$carAccelerationSpeed, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_numeric(config$routingDefaults$carDecelerationSpeed, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_numeric(config$routingDefaults$maxHours, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_numeric(config$routingDefaults$maxSlope, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_numeric(config$routingDefaults$maxTransferWalkDistance, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_numeric(config$routingDefaults$maxWalkDistance, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_numeric(config$routingDefaults$maxWeight, len = 1, null.ok = TRUE)
    checkmate::assert_numeric(config$routingDefaults$preTransitOverageRate, len = 1, null.ok = TRUE)
    checkmate::assert_numeric(config$routingDefaults$preTransitPenalty, len = 1, null.ok = TRUE)



    foo <- c(
      "routerId", # The router ID -- internal ID to switch between router implementation (or graphs)
      "showIntermediateStops", # Whether the planner should return intermediate stops lists for transit legs.
      "softPreTransitLimiting",
      "softWalkLimiting",
      "softWalkOverageRate",
      "softWalkPenalty",
      "stairsReluctance", # Used instead of walk reluctance for stairs
      "startingTransitStopId", # A transit stop that this trip must start from
      "startingTransitTripId", # A trip where this trip must start from (depart-onboard routing)
      "to", # The end location
      "transferPenalty", # An extra penalty added on transfers (i.e.
      "transferSlack", # A global minimum transfer time (in seconds) that specifies the minimum amount of time that must pass between exiting one transit vehicle and boarding another.
      "traversalCostModel", # The model that computes turn/traversal costs.
      "triangleSafetyFactor", # For the bike triangle, how important safety is
      "triangleSlopeFactor", # For the bike triangle, how important slope is
      "triangleTimeFactor", # For the bike triangle, how important time is.
      "turnReluctance", # Multiplicative factor on expected turning time.
      "unpreferredAgencies", # Set of unpreferred agencies for given user.
      "unpreferredRoutes", # Set of unpreferred routes for given user.
      "useBikeRentalAvailabilityInformation", # Whether or not bike rental availability information will be used to plan bike rental trips
      "useRequestedDateTimeInMaxHours", # Whether maxHours limit should consider wait/idle time between the itinerary and the requested arrive/depart time.
      "useTraffic", # Should traffic congestion be considered when driving?
      "useUnpreferredRoutesPenalty", # Penalty added for using every unpreferred route.
      "waitAtBeginningFactor", # How much less bad is waiting at the beginning of the trip (replaces waitReluctance on the first boarding)
      "waitReluctance", # How much worse is waiting for a transit vehicle than being on a transit vehicle, as a multiplier.
      "walkBoardCost", # This prevents unnecessary transfers by adding a cost for boarding a vehicle.
      "walkingBike",
      "walkReluctance", # A multiplier for how bad walking is, compared to being in transit for equal lengths of time.
      "walkSpeed", # max walk/bike speed along streets, in meters per second
      "wheelchairAccessible", # Whether the trip must be wheelchair accessible.
      "worstTime"
    )

    # Not Checked
    # "dateTime",                # The epoch date/time that the trip should depart (or arrive, for requests where arriveBy is true)
    # "dominanceFunction",       # The function that compares paths converging on the same vertex to decide which ones continue to be explored.
    # "extensions",              # Extensions to the trip planner will require additional traversal options beyond the default set.
    # "intermediatePlaces",      # An ordered list of intermediate locations to be visited.
    # "locale",
    # "modes",                   # The set of TraverseModes that a user is willing to use.
    # "optimize",                # The set of characteristics that the user wants to optimize for -- defaults to QUICK, or optimize for transit time.
    # "rctx",                    # The routing context used to actually carry out this search.
  } else if (type == "build") {
    # logical
    checkmate::assert_logical(config$transit, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$streets, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$embedRouterConfig, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$areaVisibility, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$staticParkAndRide, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$htmlAnnotations, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$useTransfersTxt, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$parentStopLinking, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$stationTransfers, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$platformEntriesLinking, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$matchBusRoutesToStreets, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$fetchElevationUS, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$staticBikeRental, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$staticBikeParkAndRide, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$banDiscouragedWalking, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$banDiscouragedBiking, len = 1, null.ok = TRUE)
    checkmate::assert_logical(config$extraEdgesStopPlatformLink, len = 1, null.ok = TRUE)

    # Interger
    checkmate::assert_integer(config$maxHtmlAnnotationsPerFile, len = 1, null.ok = TRUE, lower = 1)
    checkmate::assert_integer(config$maxInterlineDistance, len = 1, null.ok = TRUE, lower = 1)
    checkmate::assert_integer(config$islandWithStopsMaxSize, len = 1, null.ok = TRUE, lower = 1)
    checkmate::assert_integer(config$islandWithoutStopsMaxSize, len = 1, null.ok = TRUE, lower = 1)

    # Numeric
    checkmate::assert_numeric(config$elevationUnitMultiplier, len = 1, null.ok = TRUE)
    checkmate::assert_numeric(config$subwayAccessTime, len = 1, null.ok = TRUE, lower = 0)
    checkmate::assert_numeric(config$maxTransferDistance, len = 1, null.ok = TRUE, lower = 0)

    # Character
    checkmate::assert_subset(config$osmWayPropertySet, empty.ok = TRUE, choices = c("default", "norway"))
    checkmate::assert_subset(config$stopClusterMode, empty.ok = TRUE, choices = c("proximity", "parentStation"))
  } else if (type == "otp") {
    otp_validate_config(config, type = "build")
    otp_validate_config(config, type = "router")
  } else {
    message("No checks performed")
  }
  return(TRUE)
}

#' Make Config Object
#' @description
#' OTP can be configured using three json files `otp-config.json`, `build-config.json`,
#' and `router-config.json`. This function creates a named list for each config file and
#' populates the defaults values.
#'
#' @param type Which type of config file to create, "otp", "build", "router"
#'
#' @details
#'
#' For more details see:
#' http://docs.opentripplanner.org/en/latest/Configuration
#' @export

otp_make_config <- function(type) {
  checkmate::assert_subset(type, choices = c("otp", "build", "router"), empty.ok = F)

  if (type == "otp") {
    config_router <- otp_make_config("router")
    config_build <- otp_make_config("build")

    config <- c(config_router, config_build)

  } else if (type == "build") {
    config_names <- c(
      "htmlAnnotations", "transit", "useTransfersTxt", "parentStopLinking", "stationTransfers",
      "stopClusterMode", "subwayAccessTime", "streets", "embedRouterConfig", "areaVisibility", "platformEntriesLinking",
      "matchBusRoutesToStreets", "fetchElevationUS", "elevationBucket", "elevationUnitMultiplier", "fares", "osmNaming",
      "osmWayPropertySet", "staticBikeRental", "staticParkAndRide", "staticBikeParkAndRide", "maxHtmlAnnotationsPerFile",
      "maxInterlineDistance", "islandWithoutStopsMaxSize", "islandWithStopsMaxSize", "banDiscouragedWalking",
      "banDiscouragedBiking", "maxTransferDistance", "extraEdgesStopPlatformLink"
    )
    config <- rep(list(NULL), times = length(config_names))
    names(config) <- config_names
    # Assign Default Values
    # Logical
    config[c("transit", "streets", "embedRouterConfig", "areaVisibility", "staticParkAndRide")] <- TRUE
    config[c(
      "htmlAnnotations", "useTransfersTxt", "parentStopLinking", "stationTransfers", "platformEntriesLinking",
      "matchBusRoutesToStreets", "fetchElevationUS", "staticBikeRental", "staticBikeParkAndRide", "banDiscouragedWalking",
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
      "routingDefaults", "timeout", "timeouts", "requestLogFile", "boardTimes",
      "alightTimes", "updaters"
    )
    config <- rep(list(NULL), times = length(config_names))
    names(config) <- config_names

    routingDefaults_names <- c(
      "alightSlack",
      "allowBikeRental",
      "arriveBy", # Whether the trip should depart at dateTime (false, the default), or arrive at dateTime.
      "bannedAgencies", # Do not use certain named agencies
      "bannedRoutes", # Do not use certain named routes
      "bannedStops", # Do not use certain stops.
      "bannedStopsHard", # Do not use certain stops.
      "bannedTrips", # Do not use certain trips
      "batch", # when true, do not use goal direction or stop at the target, build a full SPT
      "bikeBoardCost", # Separate cost for boarding a vehicle with a bicycle, which is more difficult than on foot.
      "bikeParkAndRide",
      "bikeParkCost", # Cost of parking a bike.
      "bikeParkTime", # Time to park a bike
      "bikeRentalDropoffCost", # Cost of dropping-off a rented bike
      "bikeRentalDropoffTime", # Time to drop-off a rented bike
      "bikeRentalPickupCost", # Cost of renting a bike.
      "bikeRentalPickupTime", # Time to rent a bike
      "bikeSpeed",
      "bikeSwitchCost", # Cost of getting on and off your own bike
      "bikeSwitchTime", # Time to get on and off your own bike
      "bikeWalkingOptions", # Options specifically for the case that you are walking a bicycle.
      "boardSlack", # Invariant: boardSlack + alightSlack <= transferSlack.
      "carAccelerationSpeed", # The acceleration speed of an automobile, in meters per second per second.
      "carDecelerationSpeed", # The deceleration speed of an automobile, in meters per second per second.
      "carDropoffTime", # Time to park a car in a park and ride, w/o taking into account driving and walking cost (time to park, switch off, pick your stuff, lock the car, etc...)
      "carSpeed",
      "clampInitialWait", # The maximum wait time in seconds the user is willing to delay trip start.
      "compactLegsByReversedSearch", # When true, do a full reversed search to compact the legs of the GraphPath.
      "dateTime", # The epoch date/time that the trip should depart (or arrive, for requests where arriveBy is true)
      "disableAlertFiltering", # Option to disable the default filtering of GTFS-RT alerts by time.
      "disableRemainingWeightHeuristic", # If true, the remaining weight heuristic is disabled.
      "dominanceFunction", # The function that compares paths converging on the same vertex to decide which ones continue to be explored.
      "driveOnRight", # If true, cost turns as they would be in a country where driving occurs on the right; otherwise, cost them as they would be in a country where driving occurs on the left.
      "elevatorBoardCost", # What is the cost of boarding an elevator?
      "elevatorBoardTime", # How long does it take to get an elevator, on average (actually, it probably should be a bit *more* than average, to prevent optimistic trips)? Setting it to "seems like forever," while accurate, will probably prevent OTP from working correctly.
      "elevatorHopCost", # What is the cost of travelling one floor on an elevator?
      "elevatorHopTime", # How long does it take to advance one floor on an elevator?
      "extensions", # Extensions to the trip planner will require additional traversal options beyond the default set.
      "from", # The start location
      "geoidElevation", # Whether to apply the ellipsoid->geoid offset to all elevations in the response
      "ignoreRealtimeUpdates", # When true, realtime updates are ignored during this search.
      "intermediatePlaces", # An ordered list of intermediate locations to be visited.
      "kissAndRide",
      "locale",
      "longDistance",
      "maxHours", # The maximum duration of a returned itinerary, in hours.
      "maxPreTransitTime", # The maximum time (in seconds) of pre-transit travel when using drive-to-transit (park and ride or kiss and ride).
      "maxSlope", # The maximum slope of streets for wheelchair trips.
      "maxTransfers",
      "maxTransferWalkDistance", # The maximum distance (in meters) the user is willing to walk for transfer legs.
      "maxWalkDistance", # The maximum distance (in meters) the user is willing to walk for access/egress legs.
      "maxWeight", # The worst possible weight that we will accept when planning a trip.
      "MIN_SIMILARITY",
      "modes", # The set of TraverseModes that a user is willing to use.
      "nonpreferredTransferPenalty", # Penalty for using a non-preferred transfer
      "numItineraries", # The maximum number of itineraries to return.
      "onlyTransitTrips", # Accept only paths that use transit (no street-only paths).
      "optimize", # The set of characteristics that the user wants to optimize for -- defaults to QUICK, or optimize for transit time.
      "otherThanPreferredRoutesPenalty", # Penalty added for using every route that is not preferred if user set any route as preferred.
      "parameters", # The complete list of incoming query parameters.
      "parkAndRide",
      "preferredAgencies", # Set of preferred agencies by user.
      "preferredRoutes", # Set of preferred routes by user.
      "preTransitOverageRate",
      "preTransitPenalty",
      "rctx", # The routing context used to actually carry out this search.
      "reverseOptimizeOnTheFly", # When true, reverse optimize this search on the fly whenever needed, rather than reverse-optimizing the entire path when it's done.
      "reverseOptimizing", # This is true when a GraphPath is being traversed in reverse for optimization purposes.
      "routerId", # The router ID -- internal ID to switch between router implementation (or graphs)
      "showIntermediateStops", # Whether the planner should return intermediate stops lists for transit legs.
      "softPreTransitLimiting",
      "softWalkLimiting",
      "softWalkOverageRate",
      "softWalkPenalty",
      "stairsReluctance", # Used instead of walk reluctance for stairs
      "startingTransitStopId", # A transit stop that this trip must start from
      "startingTransitTripId", # A trip where this trip must start from (depart-onboard routing)
      "to", # The end location
      "transferPenalty", # An extra penalty added on transfers (i.e.
      "transferSlack", # A global minimum transfer time (in seconds) that specifies the minimum amount of time that must pass between exiting one transit vehicle and boarding another.
      "traversalCostModel", # The model that computes turn/traversal costs.
      "triangleSafetyFactor", # For the bike triangle, how important safety is
      "triangleSlopeFactor", # For the bike triangle, how important slope is
      "triangleTimeFactor", # For the bike triangle, how important time is.
      "turnReluctance", # Multiplicative factor on expected turning time.
      "unpreferredAgencies", # Set of unpreferred agencies for given user.
      "unpreferredRoutes", # Set of unpreferred routes for given user.
      "useBikeRentalAvailabilityInformation", # Whether or not bike rental availability information will be used to plan bike rental trips
      "useRequestedDateTimeInMaxHours", # Whether maxHours limit should consider wait/idle time between the itinerary and the requested arrive/depart time.
      "useTraffic", # Should traffic congestion be considered when driving?
      "useUnpreferredRoutesPenalty", # Penalty added for using every unpreferred route.
      "waitAtBeginningFactor", # How much less bad is waiting at the beginning of the trip (replaces waitReluctance on the first boarding)
      "waitReluctance", # How much worse is waiting for a transit vehicle than being on a transit vehicle, as a multiplier.
      "walkBoardCost", # This prevents unnecessary transfers by adding a cost for boarding a vehicle.
      "walkingBike",
      "walkReluctance", # A multiplier for how bad walking is, compared to being in transit for equal lengths of time.
      "walkSpeed", # max walk/bike speed along streets, in meters per second
      "wheelchairAccessible", # Whether the trip must be wheelchair accessible.
      "worstTime" # The worst possible time (latest for depart-by and earliest for arrive-by) to accept
    )

    routingDefaults <- rep(list(NULL), times = length(routingDefaults_names))
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
  } else {
    config <- NULL
  }

  att <- list(type)
  names(att) <- "config_type"
  attributes(config) <- c(attributes(config), att)
  return(config)
}
