# Checks against a  public API
# Very limited testing to abide by fair use policy

otpcon <- otp_connect(
  hostname = "demo.planner.plannerstack.com",
  router = "default",
  port = 80
)


context("Test the otp_plan function")

# test_that("basic routing", {
#  route <- otp_plan(otpcon,
#    fromPlace = c(4.46594, 51.92394),
#    toPlace = c(4.96582, 52.38901),
#    fromID = "A",
#    toID = "B"
#  )
#  expect_is(route, "sf")
#  expect_true(nrow(route) == 1)
#})


context("Test the otp_isochone function")

#test_that("basic isochrone", {
#  isochrone <- otp_isochrone(
#    otpcon = otpcon,
#    fromPlace = c(4.46594, 51.92394),
#    mode = c("WALK", "TRANSIT"),
#    cutoffSec = c(15, 30, 45, 60, 75, 90) * 60
#  ) # Cut offs in seconds
#  expect_is(isochrone, "sf")
#  expect_true(nrow(isochrone) == 6)
#})

context("Test the otp_geocode function")

#test_that("basic geocode", {
#  museum <- otp_geocode(otpcon = otpcon, query = "Rijksmuseum")
#  expect_is(museum, "sf")
#  expect_true(nrow(museum) > 1)
#})
