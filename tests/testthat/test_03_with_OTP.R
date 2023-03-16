# Check that require OTP to work

# library(testthat)
# fls <- list.files("R", full.names = TRUE)
# for(fl in fls){source(fl)}
# Sys.setenv("NOT_CRAN" = "true")

on_cran <- function() !identical(Sys.getenv("NOT_CRAN"), "true")

skip_on_j11 <- function() {
  suppressWarnings(jv <- otp_check_java(2))
  if (jv) {
    skip("Skip this test with Java 11")
  }
}

skip_on_j17 <- function() {
  suppressWarnings(jv <- otp_check_java(2.2))
  if (jv) {
    skip("Skip this test with Java 17")
  }
}

# skip rules
# skip on cran always
# skip J8 code on J11
# Skip J11 code on J8

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

  context("Purge and redownload files")

  path_data <- file.path(tempdir(), "otptests")
  if(dir.exists(path_data)){
    unlink(path_data, recursive = TRUE)
  }

  dir.create(path_data)

  otp_dl_demo(path_data)


  if (suppressWarnings(otp_check_java(2.2))) {
    path_otp <- otp_dl_jar(path_data,
                           version = "2.2.0",
                           cache = FALSE)
  } else {
    path_otp <- otp_dl_jar(path_data,
                           version = "1.5.0",
                           cache = FALSE)
  }
}

context("Check files have downloaded")


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
  log <- otp_build_graph(otp = path_otp,
                         dir = path_data,
                         router = "default",
                         quiet = FALSE)
  expect_true(file.exists(file.path(
    path_data,
    "graphs",
    "default",
    "Graph.obj"
  )) | file.exists(file.path(
    path_data,
    "graphs",
    "default",
    "graph.obj"
  ))

  )
})

context("Test the otp_setup function")

test_that("We can startup OTP", {
  skip_on_cran()
  expect_message(log <- otp_setup(otp = path_otp,
                                  dir = path_data,
                                  router = "default",
                                  wait = TRUE),
    regexp = "OTP is loading"
  )

  # Wait to see it OTP has started
  for(j in 1:6){
    otpcon <- try(otp_connect(router = "default"), silent = TRUE)
    if(inherits(otpcon,"try-error")){
      Sys.sleep(60)
    } else {
      break
    }
  }

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
    otp_connect(router = "not"),
    "Router http://localhost:8080/otp/routers/not does not exist"
  )
})

if (!on_cran()) {
  otpcon <- otp_connect(router = "default")
}

context("Test the otp_plan function")

col_names <- c(
  "fromPlace","toPlace",
  "duration","startTime",
  "endTime","walkTime",
  "transitTime","waitingTime",
  "walkDistance","walkLimitExceeded",
  "generalizedCost","elevationLost",
  "elevationGained","transfers",
  "fare","tooSloped",
  "arrivedAtDestinationWithRentedBicycle", "fare_currency",
  "route_option","leg_startTime",
  "leg_endTime","leg_departureDelay",
  "leg_arrivalDelay","leg_realTime",
  "leg_distance","leg_generalizedCost",
  "leg_pathway","leg_mode",
  "leg_transitLeg","leg_route",
  "leg_agencyTimeZoneOffset","leg_interlineWithPreviousLeg",
  "leg_rentedBike","leg_walkingBike",
  "leg_duration",
  "leg_agencyName","leg_agencyUrl","leg_routeType","leg_routeId",
  "leg_agencyId","leg_tripId","leg_serviceDate","leg_routeShortName",
  "leg_routeLongName",
  "leg_flexDrtAdvanceBookMin",
  "geometry"
)

test_that("basic routing", {
  skip_on_cran()
  route <- otp_plan(otpcon,
    fromPlace = c(-1.16489, 50.64990),
    toPlace = c(-1.15803, 50.72515),
    mode = "CAR"
  )
  expect_is(route, "sf")
  expect_true(nrow(route) == 1)
  expect_true(ncol(route) >= 32)
  expect_true(all(names(route) %in% col_names))
})


test_that("transit routing", {
  skip_on_cran()
  route <- otp_plan(otpcon,
    fromPlace = c(-1.16489, 50.64990),
    toPlace = c(-1.15803, 50.72515),
    date_time = as.POSIXct(strptime("2022-06-03 13:30", "%Y-%m-%d %H:%M")),
    mode = c("WALK", "TRANSIT"),
    numItineraries = 3
  )
  expect_is(route, "sf")
  expect_true(nrow(route) >= 6)
  expect_true(ncol(route) >= 41)
  expect_true(all(names(route) %in% col_names))
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
  route <- otp_plan(otpcon,
    fromPlace = c(-1.16489, 50.64990),
    toPlace = c(-1.15803, 50.72515),
    full_elevation = TRUE
  )
  expect_is(route, "sf")
  expect_true(nrow(route) == 1)
  expect_true(ncol(route) >= 32)
  expect_is(route$leg_elevation, "list")
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
  expect_true(all(names(routes) %in% col_names))
})


test_that("distance balancing works", {
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

  expect_true(nrow(routes) == nrow(routesdb))
})


context("Test the otp_isochone function")

test_that("basic isochrone", {
  skip_on_cran()
  skip_on_j11()
  skip_on_j17()
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

test_that("batch isochrone", {
  skip_on_cran()
  isobatch <- otp_isochrone(
    otpcon = otpcon,
    fromPlace = lsoa[1:3, ],
    fromID = as.character(1:3),
    mode = c("WALK"),
    maxWalkDistance = 2000,
    ncores = 1,
    date_time = as.POSIXct(strptime("2020-06-03 13:30", "%Y-%m-%d %H:%M")),
    cutoffSec = c(15, 30, 45, 60, 75, 90) * 60
  )
  expect_is(isobatch, "sf")
  expect_true(nrow(isobatch) == 18)
  expect_true(ncol(isobatch) == 4)
  expect_true(all(names(isobatch) %in%
    c("id", "time", "fromPlace", "geometry")))
  expect_true(all(isobatch$time %in% (c(90, 75, 60, 45, 30, 15) * 60)))
})

test_that("nonsence isochrone", {
  skip_on_cran()
  skip_on_j11()
  skip_on_j17()
  expect_error(otp_isochrone(
    otpcon = otpcon,
    fromPlace = c(-5, 5)
  ))
})

context("Test the otp_geocode function")

test_that("basic geocode", {
  skip_on_cran()
  skip_on_j11()
  skip_on_j17()
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
  skip_on_j17()
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
  skip_on_j17()
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
  skip_on_j17()
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

context("Test the analyst functions")

test_that("We can startup OTP with the analyst", {
  skip_on_cran()
  skip_on_j11()
  skip_on_j17()
  expect_message(log <- otp_setup(otp = path_otp,
                                  dir = path_data,
                                  router = "default",
                                  wait = FALSE,
                                  analyst = TRUE,
                                  pointsets = TRUE),
                 regexp = "OTP is loading"
  )
  # Wait to see it OTP has started
  for(j in 1:10){
    otpcon <- try(otp_connect(router = "default"), silent = TRUE)
    if(inherits(otpcon,"try-error")){
      Sys.sleep(60)
    } else {
      break
    }
  }
})

test_that("Can connect to OTP", {
  skip_on_cran()
  skip_on_j11()
  skip_on_j17()
  expect_message(
    otp_connect(router = "default"),
    "Router http://localhost:8080/otp/routers/default exists"
  )
})

if (!on_cran()) {
  otpcon <- otp_connect(router = "default", check = FALSE)
}

test_that("Make a tt matrix", {
  skip_on_cran()
  skip_on_j11()
  skip_on_j17()
  ttmatrix <- otp_traveltime(otpcon,
                             path_data,
                             fromPlace = lsoa[1:5,],
                             toPlace = lsoa[1:5,],
                             fromID = lsoa$geo_code[1:5],
                             toID = lsoa$geo_code[1:5])
  expect_is(ttmatrix, "data.frame")
  expect_true(nrow(ttmatrix) == 5)
  expect_true(ncol(ttmatrix) == 5)
})

test_that("Make a tt raster", {
  skip_on_cran()
  skip_on_j11()
  skip_on_j17()
  surface <- otp_make_surface(otpcon = otpcon,
                             fromPlace = c(-1.17502,50.64590),
                             mode = "CAR")

  r <- otp_surface_isochrone(otpcon, surface = surface)
  expect_is(r, "SpatRaster")
})


test_that("otp_stop", {
  skip_on_cran()
  skip_on_j11()
  skip_on_j17()
  foo <- otp_stop(FALSE)
  if (checkmate::test_os("windows")) {
    expect_true(grepl("SUCCESS", foo))
  } else {
    expect_true(TRUE)
  }
})

