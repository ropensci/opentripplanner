otp_write_config <- function(config,
                             dir = NULL,
                             router = "default"){
  checkmate::assert_list(config, any.missing = FALSE, names = "unique", null.ok = FALSE)
  att <- attributes(config)$config_type
  checkmate::assert_subset(att, choices = c("otp","build","router"), empty.ok = F)

  jsonlite::toJSON(config, pretty = TRUE, null = "null", na = "null")
  jsonlite::write_json(config, paste0(dir,"/",router,"/graphs/",att,".json"))
}

otp_validate_config <- function(config,
                             dir = NULL,
                             router = "default"){
  checkmate::assert_list(config, any.missing = FALSE, names = "unique", null.ok = FALSE)
  att <- attributes(config)$config_type
  checkmate::assert_subset(att, choices = c("otp","build","router"), empty.ok = F)

  jsonlite::toJSON(config, pretty = TRUE, null = "null", na = "null")
  jsonlite::write_json(config, paste0(dir,"/",router,"/graphs/",att,".json"))
}

#' Make the built in config files
otp_make_config <- function(type){
  checkmate::assert_subset(type, choices = c("otp","build","router"), empty.ok = F)

  if(type == "otp"){


  }else if(type == "build"){

    config_names <- c("htmlAnnotations","transit","useTransfersTxt","parentStopLinking","stationTransfers",
    "stopClusterMode","subwayAccessTime","streets","embedRouterConfig","areaVisibility","platformEntriesLinking",
    "matchBusRoutesToStreets","fetchElevationUS","elevationBucket","elevationUnitMultiplier","fares","osmNaming",
    "osmWayPropertySet","staticBikeRental","staticParkAndRide","staticBikeParkAndRide","maxHtmlAnnotationsPerFile",
    "maxInterlineDistance","islandWithoutStopsMaxSize","islandWithStopsMaxSize","banDiscouragedWalking",
    "banDiscouragedBiking","maxTransferDistance","extraEdgesStopPlatformLink")
    config <- rep(list(NA), times = length(config_names))
    names(config) <- config_names
    # Assign Default Values
    # Logical
    config[c("transit","streets","embedRouterConfig","areaVisibility","staticParkAndRide")] <- TRUE
    config[c("htmlAnnotations","useTransfersTxt","parentStopLinking","stationTransfers","platformEntriesLinking",
             "matchBusRoutesToStreets","fetchElevationUS","staticBikeRental","staticBikeParkAndRide","banDiscouragedWalking",
             "banDiscouragedBiking","extraEdgesStopPlatformLink")] <- FALSE
    # Numeric
    config[["elevationUnitMultiplier"]] <- 1
    config[["subwayAccessTime"]] <- 2
    config[["islandWithStopsMaxSize"]] <- 5
    config[["islandWithoutStopsMaxSize"]] <- 40
    config[["maxInterlineDistance"]] <- 200
    config[["maxHtmlAnnotationsPerFile"]] <- 1000
    config[["maxTransferDistance"]] <- 2000

    # Character
    config[["osmWayPropertySet"]] <- "default"
    config[["stopClusterMode"]] <- "proximity"

    # Other
    config["elevationBucket"] <- NULL
    config["fares"] <- NULL
    config["osmNaming"] <- NULL




  }else if(type == "router"){
    config_names <- c("routingDefaults","timeout","timeouts","requestLogFile","boardTimes",
                      "alightTimes","updaters")
    config <- rep(list(NA), times = length(config_names))
    names(config) <- config_names

    #config["routingDefaults"] <- NULL



  }else{
    config <- NA
  }

  att <- list(type)
  names(att) <- "config_type"
  attributes(config) <- c(attributes(config),att)
  return(config)
}
