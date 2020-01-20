# This tests will run without OTP setup.
context("Test function without an OTP connection")
# make sure no OTP
# foo <- suppressWarnings(otp_stop(warn = FALSE))
# rm(foo)

# setup empy files
otpcon <- otp_connect(check = FALSE)

context("Download required files")

dir.create(file.path(tempdir(), "otptests"))
path_data <- file.path(tempdir(), "otptests")


test_that("need valid path to download", {
  expect_error(otp_dl_demo("orghkfjbgkfxjbgkj"),
    regexp = "Can't find folder"
  )
})


otp_dl_demo(path_data)

test_that("download example data", {
  expect_true(file.exists(file.path(
    path_data, "graphs", "default", "isle-of-wight.osm.pbf"
  )))
  expect_true(file.exists(file.path(
    path_data, "graphs", "default", "iow-rail-gtfs.zip"
  )))
  expect_true(file.exists(file.path(
    path_data, "graphs", "default", "iow-bus-gtfs.zip"
  )))
  expect_true(file.exists(file.path(
    path_data, "graphs", "default", "IOW_DEM.tif"
  )))
  expect_true(file.exists(file.path(
    path_data, "graphs", "default", "router-config.json"
  )))


  expect_true(!file.exists(file.path(path_data, "isle-of-wight-demo.zip")))
})



path_otp <- otp_dl_jar(path_data)

test_that("download otp", {
  expect_true(file.exists(file.path(path_otp)))
})


test_that("default object is created and make_url method works", {
  expect_is(otpcon, "otpconnect")
  expect_match(make_url(otpcon), "http://localhost:8080/otp/routers/default")
  # warnings
  x <- structure(as.list(1:2), class = "myclass")
  expect_warning(make_url(x))
  expect_warning(check_router(x))
})

test_that("can't connect to non-existant router", {
  expect_error(otp_connect(router = "bananas"))
})

test_that("can't build graph without opt", {
  expect_error(otp_build_graph(otp = path_otp))
})

test_that("can't setup without opt", {
  expect_error(otp_setup(otp = path_otp, dir = path_data))
})

test_that("otp_plan input validation", {
  expect_error(otp_plan(otpcon),
    regexp = "fromPlace is not in a valid format"
  )
  expect_error(otp_plan(otpcon, fromPlace = c(1.23, 1.23)),
    regexp = "toPlace is not in a valid format"
  )
  expect_error(otp_plan(otpcon,
    fromPlace = c(1.23, 1.23),
    toPlace = c(1.23, 1.23)
  ),
  regexp = "Failed to connect to localhost port 8080"
  )
  expect_error(otp_plan(otpcon,
    fromPlace = matrix(c(1.23, 1.23, 2.34, 2.34), ncol = 2),
    toPlace = matrix(c(1.23, 1.23, 2.34, 2.34, 3.45, 3.45), ncol = 2)
  ),
  regexp = "Number of fromPlaces and toPlaces do not match"
  )
  expect_error(otp_plan(otpcon,
    toPlace = matrix(c(1.23, 1.23, 2.34, 2.34), ncol = 2),
    fromPlace = matrix(c(1.23, 1.23), ncol = 2)
  ),
  regexp = "Failed to connect to localhost port 8080"
  )
  expect_error(otp_plan(otpcon,
    toPlace = matrix(c(1.23, 1.23, 2.34, 2.34), ncol = 2),
    fromPlace = matrix(c(1.23, 1.23), ncol = 2),
    toID = c("A", "B", "C")
  ),
  regexp = "The length of toID and toPlace are not the same"
  )
  expect_error(otp_plan(otpcon,
    toPlace = matrix(c(1.23, 1.23, 2.34, 2.34), ncol = 2),
    fromPlace = matrix(c(1.23, 1.23), ncol = 2),
    fromID = c("A", "B", "C")
  ),
  regexp = "The length of fromID and fromPlace are not the same"
  )
  expect_error(otp_plan(otpcon,
    fromPlace = matrix(c(1.23, 1.23, 2.34, 2.34), ncol = 2),
    toPlace = matrix(c(1.23, 1.23), ncol = 2)
  ),
  regexp = "Failed to connect to localhost port 8080"
  )
  # ncore
  expect_error(otp_plan(otpcon,
    fromPlace = matrix(c(1.23, 1.23, 2.34, 2.34), ncol = 2),
    toPlace = matrix(c(1.23, 1.23), ncol = 2),
    ncore = 2
  ),
  regexp = "Failed to connect to localhost port 8080"
  )
  # linestring
  ls <- sf::st_as_sf(data.frame(
    id = 1,
    geom = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(1, 1), c(2, 1))))
  ))
  expect_error(otp_plan(otpcon,
    fromPlace = ls,
    toPlace = matrix(c(1, 1), ncol = 2)
  ),
  regexp = "contains non-POINT geometry"
  )
})

test_that("otp_geocode input validation", {
  expect_error(otp_geocode(otpcon),
    regexp = "Assertion on 'query' failed: Must be of type 'character', not 'NULL'."
  )
  expect_error(otp_geocode(otpcon, query = "test"),
    regexp = "Failed to connect to localhost port 8080"
  )
})

test_that("otp_isochrone input validation", {
  expect_error(otp_isochrone(otpcon),
    regexp = "Assertion on 'fromPlace' failed: Must have length 2, but has length 1."
  )
  expect_error(otp_isochrone(otpcon, fromPlace = c(1, 1)),
    regexp = "Failed to connect to localhost port 8080"
  )
})

test_that("otp_make_config tests", {
  config_router <- otp_make_config("router")
  config_build <- otp_make_config("build")
  config_otp <- otp_make_config("otp")

  expect_is(config_router, "list")
  expect_is(config_build, "list")
  expect_is(config_otp, "list")

  expect_true(otp_validate_config(config_router))
  expect_true(otp_validate_config(config_build))
  expect_true(otp_validate_config(config_otp))

  dir.create(file.path(tempdir(), "otptests", "graphs"))
  dir.create(file.path(tempdir(), "otptests", "graphs", "configtests"))

  otp_write_config(config_router, dir = file.path(tempdir(), "otptests"), router = "configtests")
  otp_write_config(config_build, dir = file.path(tempdir(), "otptests"), router = "configtests")
  otp_write_config(config_otp, dir = file.path(tempdir(), "otptests"), router = "configtests")

  expect_true(file.exists(file.path(
    tempdir(), "otptests", "graphs", "configtests", "router-config.json"
  )))
  expect_true(file.exists(file.path(
    tempdir(), "otptests", "graphs", "configtests", "build-config.json"
  )))
  expect_true(file.exists(file.path(
    tempdir(), "otptests", "graphs", "configtests", "otp-config.json"
  )))
})

test_that("otp_build_graph input validation", {
  expect_error(otp_build_graph(otp = paste0(path_otp, "Z"), dir = path_data),
    regexp = "File does not exist:"
  )
})

test_that("otp_setup input validation", {
  expect_error(otp_setup(otp = paste0(path_otp, "Z"), dir = path_data),
    regexp = "File does not exist:"
  )
})


test_that("otp_stop tests", {
  # Don't know how to test this
  expect_true(TRUE)
})
