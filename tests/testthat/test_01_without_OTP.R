# This tests will run without OTP setup.
context("Test function without an OTP connection")
# make sure no OTP
# foo <- suppressWarnings(otp_stop(warn = FALSE))
# rm(foo)

# setup empy files
otpcon <- otp_connect(check = FALSE)
dir.create(file.path(tempdir(), "otp"))
dir.create(file.path(tempdir(), "otp", "graphs"))
dir.create(file.path(tempdir(), "otp", "graphs", "default"))
path_data <- file.path(tempdir(), "otp")
path_otp <- file.path(path_data, "otp.jar")

test_that("default object is created and make_url method works", {
  expect_is(otpcon, "otpconnect")
  expect_match(make_url(otpcon), "http://localhost:8080/otp/routers/default")
  # warnings
  x <- structure(as.list(1:2), class = "myclass")
  expect_warning(make_url(x))
  expect_warning(check_router(x))
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
  expect_error(otp_plan(otpcon, fromPlace = c(1, 1)),
    regexp = "toPlace is not in a valid format"
  )
  expect_error(otp_plan(otpcon,
    fromPlace = c(1, 1),
    toPlace = c(1, 1)
  ),
  regexp = "Failed to connect to localhost port 8080: Connection refused"
  )
})

test_that("otp_geocode input validation", {
  expect_error(otp_geocode(otpcon),
    regexp = "Assertion on 'query' failed: Must be of type 'character', not 'NULL'."
  )
  expect_error(otp_geocode(otpcon, query = "test"),
    regexp = "Failed to connect to localhost port 8080: Connection refused"
  )
})

test_that("otp_isochrone input validation", {
  expect_error(otp_isochrone(otpcon),
    regexp = "Assertion on 'fromPlace' failed: Must have length 2, but has length 1."
  )
  expect_error(otp_isochrone(otpcon, fromPlace = c(1, 1)),
    regexp = "Failed to connect to localhost port 8080: Connection refused"
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

  dir.create(file.path(tempdir(), "graphs"))
  dir.create(file.path(tempdir(), "graphs", "default"))

  otp_write_config(config_router, dir = tempdir())
  otp_write_config(config_build, dir = tempdir())
  otp_write_config(config_otp, dir = tempdir())

  expect_true(file.exists(file.path(tempdir(), "graphs", "default", "router-config.json")))
  expect_true(file.exists(file.path(tempdir(), "graphs", "default", "build-config.json")))
  expect_true(file.exists(file.path(tempdir(), "graphs", "default", "otp-config.json")))
})

test_that("otp_stop tests", {
  # Don't know how to test this
  expect_true(TRUE)
})
