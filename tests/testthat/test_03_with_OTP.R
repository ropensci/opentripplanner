# Check that require OTP to work

skip_no_otp <- function() {
  if (!identical(Sys.getenv("I_have_OTP"), "TRUE")) {
    skip("Not running full test.")
  }
}

context("Test the download of the LSOA file")

f <- file.path(tempdir(), "centroids.gpkg")
download.file("https://github.com/ropensci/opentripplanner/releases/download/0.1/centroids.gpkg", f,  mode = "wb")
lsoa <- sf::read_sf(f)
file.remove(f)
test_that("can get lsoa points", {
  expect_is(lsoa, "sf")
  expect_true(nrow(lsoa) == 89)
})

context("Check previos tests have left the files we need")

path_data <- file.path(tempdir(), "otptests")
path_otp <- file.path(path_data, "otp.jar")

test_that("path_data is valid", {
  skip_no_otp()
  expect_true(dir.exists(file.path(path_data)))
})

test_that("path_otp is valid", {
  skip_no_otp()
  expect_true(file.exists(path_otp))
})


context("Test the otp_build_graph function")

test_that("We can build an otp graph", {
  skip_no_otp()
  log <- otp_build_graph(otp = path_otp, dir = path_data)
  expect_true(file.exists(file.path(
    path_data,
    "graphs",
    "default",
    "Graph.obj"
  )))
})

context("Test the otp_setup function")

test_that("We can startup OTP", {
  skip_no_otp()
  expect_message(otp_setup(otp = path_otp, dir = path_data),
    regexp = "OTP is ready to use"
  )
})

context("Test the otp_connect function")


test_that("object returned when check is TRUE and router exists", {
  skip_no_otp()
  otpcon <- otp_connect()
  expect_is(otpcon, "otpconnect")
})

test_that("correct message when check is TRUE and router exists", {
  skip_no_otp()
  expect_message(
    otp_connect(),
    "Router http://localhost:8080/otp/routers/default exists"
  )
})

test_that("correct error when check is TRUE and router does not exist", {
  skip_no_otp()
  expect_error(
    otp_connect(router = "test"),
    "Router http://localhost:8080/otp/routers/test does not exist"
  )
})

if (identical(Sys.getenv("I_have_OTP"), "TRUE")) {
  otpcon <- otp_connect()
}

context("Test the otp_plan function")

test_that("basic routing", {
  skip_no_otp()
  route <- otp_plan(otpcon,
    fromPlace = c(-1.17502, 50.64590),
    toPlace = c(-1.15339, 50.72266)
  )
  expect_is(route, "sf")
  expect_true(nrow(route) == 1)
})


test_that("transit routing", {
  skip_no_otp()
  route <- otp_plan(otpcon,
    fromPlace = c(-1.17502, 50.64590),
    toPlace = c(-1.15339, 50.72266),
    date_time = as.POSIXct(strptime("2018-06-03 13:30", "%Y-%m-%d %H:%M")),
    mode = c("WALK", "TRANSIT")
  )
  expect_is(route, "sf")
  expect_true(nrow(route) == 9)
})


test_that("no geometry routing", {
  skip_no_otp()
  route <- otp_plan(otpcon,
    fromPlace = c(-1.17502, 50.64590),
    toPlace = c(-1.15339, 50.72266),
    get_geometry = FALSE
  )
  expect_is(route, "data.frame")
  expect_true(nrow(route) == 1)
})

test_that("full elevation routing", {
  skip_no_otp()
  route <- otp_plan(otpcon,
    fromPlace = c(-1.17502, 50.64590),
    toPlace = c(-1.15339, 50.72266),
    full_elevation = TRUE
  )
  expect_is(route, "sf")
  expect_is(route$elevation, "list")
})


test_that("batch routing", {
  skip_no_otp()
  routes <- otp_plan(
    otpcon = otpcon,
    fromPlace = lsoa[1:10, ],
    toPlace = lsoa[11:20, ]
  )
  expect_is(routes, "sf")
  expect_true(nrow(routes) == 10)
})


context("Test the otp_isochone function")

test_that("basic isochrone", {
  skip_no_otp()
  ferry_current <- otp_isochrone(
    otpcon = otpcon,
    fromPlace = c(-1.159494, 50.732429), # lng/lat of Ryde ferry
    mode = c("WALK", "TRANSIT"),
    maxWalkDistance = 2000,
    date_time = as.POSIXct(strptime("2018-06-03 13:30", "%Y-%m-%d %H:%M")),
    cutoffSec = c(15, 30, 45, 60, 75, 90) * 60
  ) # Cut offs in seconds
  expect_is(ferry_current, "sf")
  expect_true(nrow(ferry_current) == 6)
})

test_that("nonsence isochrone", {
  skip_no_otp()
  expect_warning(otp_isochrone(
    otpcon = otpcon,
    fromPlace = c(-5, 5)
  ))
})

context("Test the otp_geocode function")

test_that("basic geocode", {
  skip_no_otp()
  stations <- otp_geocode(
    otpcon = otpcon,
    query = "station"
  )
  expect_is(stations, "sf")
  expect_true(nrow(stations) == 10)
})


test_that("geocode coords", {
  skip_no_otp()
  stations <- otp_geocode(
    otpcon = otpcon,
    query = "station", type = "Coordinates"
  )
  expect_is(stations, "data.frame")
  expect_true(nrow(stations) == 10)
})

test_that("geocode both", {
  skip_no_otp()
  stations <- otp_geocode(
    otpcon = otpcon,
    query = "station", type = "Both"
  )
  expect_is(stations, "sf")
  expect_true(nrow(stations) == 10)
})

test_that("geocode nonsence", {
  skip_no_otp()
  expect_warning(otp_geocode(
    otpcon = otpcon,
    query = "jhgfdhgdcmhxgfxgfx"
  ))
})
# Stop otp
test_that("otp_stop", {
  skip_no_otp()
  foo <- otp_stop(FALSE)
  if (checkmate::check_os("windows")) {
    expect_true(grepl("SUCCESS", foo))
  } else {
    expect_true(grepl("The following Java instances have been found", foo))
  }
})
