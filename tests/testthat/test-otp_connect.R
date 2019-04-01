context("Test the otp_connect function")

skip_no_otp <- function() {
  if (!identical(Sys.getenv("I_have_OTP"), "TRUE")) {
    skip("Not running full test.")
  }
}


# the following tests require an OTP instance at http://localhost:8080/otp with "default" router

test_that("object returned when check is TRUE and router exists", {
  skip_no_otp()
  otpcon <- otp_connect()
  expect_is(otpcon, "otpconnect")
})

test_that("correct message when check is TRUE and router exists", {
  skip_no_otp()
  expect_message(otp_connect(), "Router http://localhost:8080/otp/routers/default exists")
})

test_that("correct error when check is TRUE and router does not exist", {
  skip_no_otp()
  expect_error(otp_connect(router = "test"), "Router http://localhost:8080/otp/routers/test does not exist")
})

context("Test the multiple routes")

test_that("can do multiple routes", {
  skip_no_otp()
  otpcon <- otp_connect()
  lsoa <- sf::st_read("https://github.com/ITSLeeds/opentripplanner/releases/download/0.1/centroids.gpkg")
  toPlace <- lsoa[1:10, ]
  fromPlace <- lsoa[11:20, ]
  routes <- otp_plan(
    otpcon = otpcon,
    fromPlace = fromPlace,
    toPlace = toPlace
  )

  expect_is(routes, "sf")
})
