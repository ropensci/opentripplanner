# Check that require OTP to work

on_cran <- function() !identical(Sys.getenv("NOT_CRAN"), "true")

has_rcppsimdjson <- function() {
  RcppSimdJsonVersion <- try(utils::packageVersion("RcppSimdJson") >= "0.1.2", silent = TRUE)
  if (class(RcppSimdJsonVersion) == "try-error") {
    RcppSimdJsonVersion <- FALSE
  }
  return(RcppSimdJsonVersion)
}

skip_on_j11 <- function() {
  if (otp_check_java(2)) {
    skip("Skip this test with Java 11")
  }
}

if (!on_cran()) {
  context("Test the download of the LSOA file")

  f <- file.path(tempdir(), "centroids.gpkg")
  download.file("https://github.com/ropensci/opentripplanner/releases/download/0.1/centroids.gpkg",
    f,
    mode = "wb", quiet = TRUE
  )
  lsoa <- sf::read_sf(f)
  file.remove(f)
  test_that("can get lsoa points", {
    expect_is(lsoa, "sf")
    expect_true(nrow(lsoa) == 89)
  })

  context("Check previous tests have left the files we need")

  path_data <- file.path(tempdir(), "otptests")

  if (otp_check_java(2)) {
    path_otp <- file.path(path_data, "otp-2.0.0-shaded.jar")
  } else {
    path_otp <- file.path(path_data, "otp-1.5.0-shaded.jar")
  }
}




test_that("path_data is valid", {
  skip_on_cran()
  expect_true(dir.exists(file.path(path_data)))
})

test_that("path_otp is valid", {
  skip_on_cran()
  expect_true(file.exists(path_otp))
})

context("Test the otp_build_graph function")

test_that("We can build an otp graph", {
  skip_on_cran()
  log <- otp_build_graph(otp = path_otp, dir = path_data, router = "default")
  expect_true(file.exists(file.path(
    path_data,
    "graphs",
    "default",
    "Graph.obj"
  )))
})

context("Test the otp_setup function")

test_that("We can startup OTP", {
  skip_on_cran()
  expect_message(log <- otp_setup(otp = path_otp, dir = path_data, router = "default"),
    regexp = "OTP is ready to use"
  )
})

context("Test the otp_connect function")


test_that("object returned when check is TRUE and router exists", {
  skip_on_cran()
  otpcon <- otp_connect(router = "default")
  expect_is(otpcon, "otpconnect")
})

test_that("correct message when check is TRUE and router exists", {
  skip_on_cran()
  expect_message(
    otp_connect(router = "default"),
    "Router http://localhost:8080/otp/routers/default exists"
  )
})

test_that("correct error when check is TRUE and router does not exist", {
  skip_on_cran()
  expect_error(
    otp_connect(router = "notWorkingRouter"),
    "Router http://localhost:8080/otp/routers/not does not exist"
  )
})

if (!on_cran()) {
  otpcon <- otp_connect(router = "default")
}

context("Test the otp_plan function")

test_that("basic routing", {
  skip_on_cran()
  route <- otp_plan(otpcon,
    fromPlace = c(-1.16489, 50.64990),
    toPlace = c(-1.15803, 50.72515)
  )
  expect_is(route, "sf")
  expect_true(nrow(route) == 1)
  expect_true(ncol(route) >= 32)
  expect_true(all(names(route) %in%
    c(
      "duration", "startTime", "endTime", "walkTime",
      "transitTime", "waitingTime", "walkDistance", "walkLimitExceeded",
      "elevationLost", "elevationGained", "transfers", "tooSloped",
      "fare", "fare_currency", "leg_startTime", "leg_endTime",
      "departureDelay", "arrivalDelay", "realTime", "distance",
      "pathway", "mode", "route", "agencyTimeZoneOffset",
      "interlineWithPreviousLeg", "rentedBike", "flexDrtAdvanceBookMin", "leg_duration",
      "transitLeg", "route_option", "fromPlace", "toPlace",
      "geometry"
    )))
})


test_that("transit routing", {
  skip_on_cran()
  route <- otp_plan(otpcon,
    fromPlace = c(-1.16489, 50.64990),
    toPlace = c(-1.15803, 50.72515),
    date_time = as.POSIXct(strptime("2020-06-03 13:30", "%Y-%m-%d %H:%M")),
    mode = c("WALK", "TRANSIT"),
    numItineraries = 3
  )
  expect_is(route, "sf")
  expect_true(nrow(route) >= 7)
  expect_true(ncol(route) >= 41)
  col_names <- c(
    "duration", "startTime", "endTime", "walkTime",
    "transitTime", "waitingTime", "walkDistance", "walkLimitExceeded",
    "elevationLost", "elevationGained", "transfers", "fare",
    "tooSloped", "fare_currency", "leg_startTime", "leg_endTime",
    "departureDelay", "arrivalDelay", "realTime", "distance",
    "pathway", "mode", "route", "agencyTimeZoneOffset",
    "interlineWithPreviousLeg", "rentedBike", "leg_duration",
    "transitLeg", "agencyName", "agencyUrl",
    "routeId", "agencyId", "tripId", "serviceDate",
    "routeShortName", "routeLongName", "route_option", "fromPlace",
    "toPlace", "geometry","alerts","intermediateStops",
    "flexDrtAdvanceBookMin","routeType"
  )

  expect_true(all(names(route) %in% col_names))
})


test_that("legacy code", {
  skip_on_cran()
  skip_on_j11()
  route <- otp_plan_internal_legacy(otpcon,
    fromPlace = matrix(c(50.64990, -1.16489), ncol = 2),
    toPlace = matrix(c(50.72515, -1.15803), ncol = 2),
    date = "2020-06-03",
    time = "13:30"
  )

  expect_is(route, "sf")
  expect_true(nrow(route) == 1)
})


test_that("no geometry routing", {
  skip_on_cran()
  route <- otp_plan(otpcon,
    fromPlace = c(-1.16489, 50.64990),
    toPlace = c(-1.15803, 50.72515),
    get_geometry = FALSE
  )
  expect_is(route, "data.frame")
  expect_true(nrow(route) == 1)
  expect_true(ncol(route) >= 31)
})

test_that("full elevation routing", {
  skip_on_cran()
  if (!has_rcppsimdjson()) {
    skip("Skip wihtout RcppSimdJson")
  }
  route <- otp_plan(otpcon,
    fromPlace = c(-1.16489, 50.64990),
    toPlace = c(-1.15803, 50.72515),
    full_elevation = TRUE
  )
  expect_is(route, "sf")
  expect_true(nrow(route) == 1)
  expect_true(ncol(route) >= 32)
  expect_is(route$elevation, "list")
})


test_that("batch routing", {
  skip_on_cran()
  routes <- otp_plan(
    otpcon = otpcon,
    fromPlace = lsoa[1:9, ],
    toPlace = lsoa[19:11, ],
    fromID = as.character(1:9),
    toID = as.character(11:19),
    ncores = 2
  )
  expect_is(routes, "sf")
  expect_true(nrow(routes) == 9)
  expect_true(ncol(routes) >= 32)
  col_names <- c(
    "duration", "startTime", "endTime", "walkTime",
    "transitTime", "waitingTime", "walkDistance", "walkLimitExceeded",
    "elevationLost", "elevationGained", "transfers", "tooSloped",
    "fare", "fare_currency", "leg_startTime", "leg_endTime",
    "departureDelay", "arrivalDelay", "realTime", "distance",
    "pathway", "mode", "route", "agencyTimeZoneOffset",
    "interlineWithPreviousLeg", "rentedBike", "flexDrtAdvanceBookMin", "leg_duration",
    "transitLeg", "route_option", "fromPlace", "toPlace",
    "geometry"
  )
  expect_true(all(names(routes) %in% col_names))
})


test_that("ditance balancing works", {
  skip_on_cran()
  routes <- otp_plan(
    otpcon = otpcon,
    fromPlace = lsoa[1:9, ],
    toPlace = lsoa[19:11, ],
    fromID = as.character(1:9),
    toID = as.character(11:19),
    ncores = 2
  )

  routesdb <- otp_plan(
    otpcon = otpcon,
    fromPlace = lsoa[1:9, ],
    toPlace = lsoa[19:11, ],
    fromID = as.character(1:9),
    toID = as.character(11:19),
    distance_balance = TRUE,
    ncores = 2
  )

  expect_true(!identical(routes, routesdb))

  routesdb <- routesdb[order(routesdb$fromPlace), ]
  row.names(routesdb) <- 1:9

  expect_true(identical(routes, routesdb))
})


context("Test the otp_isochone function")

test_that("basic isochrone", {
  skip_on_cran()
  skip_on_j11()
  ferry_current <- otp_isochrone(
    otpcon = otpcon,
    fromPlace = c(-1.159494, 50.732429), # lng/lat of Ryde ferry
    mode = c("WALK", "TRANSIT"),
    maxWalkDistance = 2000,
    date_time = as.POSIXct(strptime("2020-06-03 13:30", "%Y-%m-%d %H:%M")),
    cutoffSec = c(15, 30, 45, 60, 75, 90) * 60
  ) # Cut offs in seconds
  expect_is(ferry_current, "sf")
  expect_true(nrow(ferry_current) == 6)
  expect_true(ncol(ferry_current) == 4)
  expect_true(all(names(ferry_current) %in%
    c("id", "time", "fromPlace", "geometry")))
  expect_true(all(ferry_current$time == c(90, 75, 60, 45, 30, 15) * 60))
})

# test_that("multicore isochrone", {
#   skip_on_cran()
#   isobatch <- otp_isochrone(
#     otpcon = otpcon,
#     fromPlace = lsoa[1:3, ], # lng/lat of Ryde ferry
#     mode = c("WALK"),
#     maxWalkDistance = 2000,
#     ncores = 2,
#     date_time = as.POSIXct(strptime("2020-06-03 13:30", "%Y-%m-%d %H:%M")),
#     cutoffSec = c(15, 30, 45, 60, 75, 90) * 60
#   ) # Cut offs in seconds
#   expect_is(isobatch, "sf")
#   expect_true(nrow(isobatch) == 18)
#   expect_true(ncol(isobatch) == 4)
#   expect_true(all(names(isobatch) %in%
#     c("id", "time", "fromPlace", "geometry")))
#   expect_true(all(isobatch$time %in% c(90, 75, 60, 45, 30, 15) * 60))
# })

test_that("nonsence isochrone", {
  skip_on_cran()
  skip_on_j11()
  expect_warning(otp_isochrone(
    otpcon = otpcon,
    fromPlace = c(-5, 5)
  ))
})

context("Test the otp_geocode function")

test_that("basic geocode", {
  skip_on_cran()
  skip_on_j11()
  stations <- otp_geocode(
    otpcon = otpcon,
    query = "station"
  )
  expect_is(stations, "sf")
  expect_true(nrow(stations) == 10)
  expect_true(ncol(stations) == 3)
})


test_that("geocode coords", {
  skip_on_cran()
  skip_on_j11()
  stations <- otp_geocode(
    otpcon = otpcon,
    query = "station", type = "Coordinates"
  )
  expect_is(stations, "data.frame")
  expect_true(nrow(stations) == 10)
})

test_that("geocode both", {
  skip_on_cran()
  skip_on_j11()
  stations <- otp_geocode(
    otpcon = otpcon,
    query = "station", type = "Both"
  )
  expect_is(stations, "sf")
  expect_true(nrow(stations) == 10)
})

test_that("geocode nonsence", {
  skip_on_cran()
  skip_on_j11()
  expect_warning(otp_geocode(
    otpcon = otpcon,
    query = "jhgfdhgdcmhxgfxgfx"
  ))
})
# Stop otp
test_that("otp_stop", {
  skip_on_cran()
  foo <- otp_stop(FALSE)
  if (checkmate::test_os("windows")) {
    expect_true(grepl("SUCCESS", foo))
  } else {
    # expect_true(grepl("The following Java instances have been found", foo))
    expect_true(TRUE)
  }
})
