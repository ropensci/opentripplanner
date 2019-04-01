# This tests will run without OTP setup.
context("Test function without an OTP connection")
otpcon <- otp_connect(check = FALSE)

test_that("default object is created and make_url method works", {
  expect_is(otpcon, "otpconnect")
  expect_match(make_url(otpcon), "http://localhost:8080/otp/routers/default")
})

test_that("can't build graph without opt", {
  expect_error(otp_build_graph(otp = "otp.jar"))
})

test_that("can't setup without opt", {
  expect_error(otp_setup(otp = "otp.jar"))
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

test_that("otp_plan input validation", {
  expect_error(otp_geocode(otpcon),
    regexp = "Assertion on 'query' failed: Must be of type 'character', not 'NULL'."
  )
  expect_error(otp_geocode(otpcon, query = "test"),
    regexp = "Failed to connect to localhost port 8080: Connection refused"
  )
})


# otp_isochrone()
# otp_make_config()
# otp_plan(otpcon)
# otp_setup()
# otp_stop()
# otp_validate_config()
# otp_write_config()
