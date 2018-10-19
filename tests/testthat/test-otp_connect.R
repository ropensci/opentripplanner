context("Test the otp_connect function")

skip_no_otp <- function() {
  if(!identical(Sys.getenv("I_have_OTP"), "TRUE"))
    skip("Not running full test.")
}
test_that("default object is created and make_url method works", {
  otpcon <- otp_connect(check = FALSE)
  expect_is(otpcon, "otpconnect")
  skip_no_otp()
  expect_match(make_url(otpcon), "http://localhost:8080/otp/routers/default")
})

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


test_that("object is not returned when check is TRUE and router does not exist", {
  #otpcon <- otp_connect(router = "test")
  skip_no_otp()
  expect_false(exists("otpcon"))
})

test_that("correct error when check is TRUE and router does not exist", {
  skip_no_otp()
  expect_error(otp_connect(router = "test"), "Router http://localhost:8080/otp/routers/test does not exist")
})

