# Checks against a  public API
# Very limited testing to abide by fair use policy

otpcon <- otp_connect(url = "https://api.digitransit.fi/routing/v1/routers/hsl")


context("Test the otp_plan function")

test_that("basic routing", {
 route <- otp_plan(otpcon,
   fromPlace = c(24.904353, 60.220485),
   toPlace = c(24.952068, 60.192033),
   fromID = "A",
   toID = "B"
 )
 expect_is(route, "sf")
 expect_true(nrow(route) == 1)
})


# context("Test the otp_isochone function")
#
# test_that("basic isochrone", {
#  isochrone <- otp_isochrone(
#    otpcon = otpcon,
#    fromPlace = c(24.904353, 60.220485),
#    mode = c("WALK", "TRANSIT"),
#    cutoffSec = c(15, 30, 45, 60, 75, 90) * 60
#  ) # Cut offs in seconds
#  expect_is(isochrone, "sf")
#  expect_true(nrow(isochrone) == 6)
# })
#
# context("Test the otp_geocode function")
#
# test_that("basic geocode", {
#  museum <- otp_geocode(otpcon = otpcon, query = "museum")
#  expect_is(museum, "sf")
#  expect_true(nrow(museum) > 1)
# })
