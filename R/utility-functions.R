#' Convert list to data frame
#'
#' A faster alternative to as.data.frame(list), similar to base::list2DF() added
#' in R 4.0.0 but back compatible
#'
#' @param l named list of equal length
#' @family internal
#' @noRd
list2df <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}

#' Convert data frame to sf data frame
#'
#' A faster alternative to sf::st_sf() but without the checks
#'
#' @param df named list of equal length
#' @family internal
#' @noRd
df2sf <- function(df) {
  class(df) <- c("sf", "data.frame")
  attributes(df)$sf_column <- "geometry"
  df
}

#' Build URL for otp_plan
#'
#'
#' @param routerUrl output of make_url
#' @param query namesd list
#' @family internal
#' @noRd
build_url <- function(routerUrl, query) {
  secs <- unlist(query, use.names = TRUE)
  secs <- paste0(names(secs), "=", secs)
  secs <- paste(secs, collapse = "&")
  secs <- gsub(",", "%2C", secs)
  secs <- paste0(routerUrl, "?", secs)
  secs
}


#' Parse a single leg otp_plan
#'
#' Function to replace looping over lists
#'
#' @param leg list
#' @param get_geometry Logical
#' @param get_elevation Logical
#' @param full_elevation Logical
#' @family internal
#' @noRd

parse_leg <- function(leg,
                      get_geometry = TRUE,
                      get_elevation = TRUE,
                      full_elevation = FALSE) {
  # split into parts
  leg$from <- NULL
  leg$to <- NULL

  if (get_elevation | full_elevation) {
    elevation <- purrr::map(leg$steps, parse_elevation)
  } else {
    elevation <- list(NULL)
  }

  leg$steps <- NULL
  leg$legElevation <- NULL #2.2 Only

  if (full_elevation) {
    leg$elevation <- elevation
  }

  if (get_geometry) {
    # Extract geometry
    legGeometry <- purrr::map2(.x = leg$legGeometry,
                               .y = elevation,
                               .f = polyline2linestring)
    leg$geometry <- legGeometry
    leg$legGeometry <- NULL
  } else {
    leg$legGeometry <- NULL
  }

  return(leg)
}

#' Parse elevation data
#'
#'
#' @param stp list - a step
#' @family internal
#' @noRd
parse_elevation <- function(stp) {
  if (is.null(stp)) {
    return(NA)
  }
  # Check for OTP1 or OTP2
  if(inherits(stp$elevation, "character")){
    elev <- strsplit(stp$elevation,",")
    elev <- purrr::map(elev, as.numeric)
    elev <- purrr::map(elev, split_alternating)
    elev <- data.table::rbindlist(elev, idcol = "step")
  } else {
    elev <- data.table::rbindlist(stp$elevation, idcol = "step")
  }
  elev$distance <- correct_distances(elev$first)
  return(as.data.frame(elev))
}

#' Split vector into two vectors by alternating values
#' @param x vector
#' @family internal
#' @noRd
split_alternating <- function(x){
  odd <- rep(c(TRUE,FALSE), length(x)/2)
  return(data.frame(first = x[odd],
                    second = x[!odd],
                    check.names = FALSE,
                    check.rows = FALSE))
}
