# This tests will run without OTP setup.
context("Test internal functions")

# otp-config
context("Test internal functions from otp-config.R")

test_that("test otp_list_clean", {
  ll <- list("foo","bar",NULL)
  ll <- otp_list_clean(ll)
  expect_is(ll, "list")
  expect_true(length(ll) == 2)
})

# otp-plan
context("Test internal functions from otp-plan.R")

test_that("test otp_clean_input", {
  otp_clean_input(c(1,2))


})

test_that("test otp_json2sf", {

})

test_that("test correct_distances", {

})

test_that("test polyline2linestring", {

})





