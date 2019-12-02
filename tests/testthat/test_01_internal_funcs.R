# This tests will run without OTP setup.
context("Test internal functions")

# otp-config
context("Test internal functions from otp-config.R")

test_that("test otp_list_clean", {
  ll <- list("foo", "bar", NULL)
  ll <- otp_list_clean(ll)
  expect_is(ll, "list")
  expect_true(length(ll) == 2)
})

# otp-plan
context("Test internal functions from otp-plan.R")

test_that("test otp_clean_input", {
  expect_error(otp_clean_input(c(181, 2), "foo"), "is not <= 180")
  expect_error(otp_clean_input(c(-181, 2), "foo"), "is not >= -180")
  expect_error(otp_clean_input(c(0, 91), "foo"), "is not <= 90")
  expect_error(otp_clean_input(c(0, -91), "foo"), "is not >= -90")
  expect_error(otp_clean_input(1, "foo"), " Must have length 2")
  expect_error(otp_clean_input("1", "2"), "is not in a valid format")

  r1 <- otp_clean_input(c(1, 2), "foo")
  expect_is(r1, "matrix")
  expect_true(nrow(r1) == 1)
  expect_true(ncol(r1) == 2)

  r2 <- otp_clean_input(matrix(c(1, 2, 3, 4), ncol = 2), "foo")
  expect_is(r2, "matrix")
  expect_true(nrow(r2) == 2)
  expect_true(ncol(r2) == 2)

  r3 <- sf::st_as_sf(data.frame(
    id = 1,
    geometry = sf::st_sfc(sf::st_point(c(1, 1)))
  ))
  r3 <- otp_clean_input(r3, "foo")
  expect_is(r3, "matrix")
  expect_true(nrow(r3) == 1)
  expect_true(ncol(r3) == 2)
})

test_that("test otp_json2sf", {
  expect_true(TRUE)
})

test_that("test correct_distances", {
  r1 <- correct_distances(c(0, 1, 2, 3, 0, 1, 2))
  expect_identical(r1, c(0, 1, 2, 3, 3, 4, 5))
})

test_that("test polyline2linestring", {
  r1 <- polyline2linestring("_p~iF~ps|U_ulLnnqC_mqNvxq`@")
  t1 <- sf::st_linestring(matrix(c(
    -120.2, 38.5,
    -120.95, 40.7,
    -126.453, 43.252
  ),
  ncol = 2, byrow = TRUE
  ))
  # Out by millionths of a degree
  r1 <- sf::st_coordinates(r1)
  t1 <- sf::st_coordinates(t1)
  r1 <- round(r1, 5)
  t1 <- round(t1, 5)

  # strip out attributs
  attributes(t1) <- NULL
  attributes(r1) <- NULL

  expect_identical(r1, t1)

  # Check with Geometries
  r2 <- polyline2linestring(
    "_p~iF~ps|U_ulLnnqC_mqNvxq`@",
    data.frame(
      first = c(0, 5, 8),
      second = c(3, 7, 10),
      distance = c(0, 250000, 530000)
    )
  )
  t2 <- sf::st_linestring(matrix(c(
    -120.2, 38.5, 3,
    -120.95, 40.7, 7,
    -126.453, 43.252, 10
  ),
  ncol = 3, byrow = TRUE
  ))
  # Out by millionths of a degree
  r2 <- sf::st_coordinates(r2)
  t2 <- sf::st_coordinates(t2)
  r2 <- round(r2, 5)
  t2 <- round(t2, 5)

  # strip out attributs
  attributes(t2) <- NULL
  attributes(r2) <- NULL

  expect_identical(r2, t2)
})

# otp_checks
skip_otp <- function() {
  if (identical(Sys.getenv("I_have_OTP"), "TRUE")) {
    skip("Not running full test.")
  }
}



context("Test otp_checks")
dir.create(file.path(tempdir(), "otp2"))
path_data <- file.path(tempdir(), "otp2")
path_otp <- file.path(tempdir(), "otp2", "otp.jar")


test_that("test otp_checks without graph, missing files", {
  expect_error(otp_checks(
    otp = path_otp,
    dir = path_data,
    router = "default",
    graph = FALSE
  ),
  "does not exist"
  )
})


dir.create(file.path(path_data, "graphs"))
dir.create(file.path(path_data, "graphs", "default"))
file.create(path_otp)

# test_that("test otp_checks without graph, with files", {
#   skip_otp()
#   expect_false(otp_checks(otp = path_otp,
#                                             dir = path_data,
#                                             router = "default",
#                                             graph = FALSE))
# })

test_that("test otp_checks with graph, missing files", {
  expect_error(otp_checks(
    otp = path_otp,
    dir = path_data,
    router = "default",
    graph = TRUE
  ),
  "File does not exist"
  )
})

# file.create(file.path(path_data, "graphs", "default", "Graph.obj"))

# test_that("test otp_checks with graph, with files", {
#   skip_otp()
#   expect_false(otp_checks(otp = path_otp,
#                                            dir = path_data,
#                                            router = "default",
#                                            graph = TRUE))
# })
