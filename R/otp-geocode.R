#'  Use OTP Geo-coder to find a location
#'
#'  Geo-coding converts a named place, such as a street name into a
#'      lng/lat pair.
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param query Character, The query string we want to geocode
#' @param autocomplete logical Whether we should use the query
#'     string to do a prefix match, default FALSE
#' @param stops Logical, Search for stops, either by name or
#'     stop code, default TRUE
#' @param clusters Logical, Search for clusters by their name,
#'     default FALSE
#' @param corners Logical, Search for street corners using at
#'     least one of the street names, default TRUE
#' @param type Character, How should results be returned can
#'     be "SF" or "Coordinates" or "Both", Default "SF"
#' @family routing
#' @return
#' Returns a data.frame of SF POINTS or Coordinates of all
#'     the locations that match `query`
#' @examples
#' \dontrun{
#' locations <- otp_geocode(otpcon, "High Street")
#' }
#' @details
#' OTP will return a maximum of 10 results
#'
#' @export
otp_geocode <- function(otpcon = NULL,
                        query = NULL,
                        autocomplete = FALSE,
                        stops = TRUE,
                        clusters = FALSE,
                        corners = TRUE,
                        type = "SF") {
  # Validate Inputs
  checkmate::assert_class(otpcon, "otpconnect", null.ok = FALSE)
  checkmate::assert_character(query,
    null.ok = FALSE, len = 1,
    min.chars = 1, any.missing = FALSE
  )
  checkmate::assert_logical(autocomplete, null.ok = FALSE)
  checkmate::assert_logical(stops, null.ok = FALSE)
  checkmate::assert_logical(clusters, null.ok = FALSE)
  checkmate::assert_logical(corners, null.ok = FALSE)
  checkmate::assert_choice(type,
    choices = c("SF", "Coordinates", "Both"),
    null.ok = FALSE
  )


  autocomplete <- tolower(as.character(autocomplete))
  stops <- tolower(as.character(stops))
  clusters <- tolower(as.character(clusters))
  corners <- tolower(as.character(corners))

  # Construct URL
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/geocode")

  querylist <- list(
    query = query,
    autocomplete = autocomplete,
    stops = stops,
    clusters = clusters,
    corners = corners
  )

  req <- httr::GET(routerUrl,
    query = querylist
  )

  # convert response content into text
  text <- httr::content(req, as = "text", encoding = "UTF-8")

  if (nchar(text) == 2) {
    warning(paste0("Failed to find '", query, "'"))
    return(NA)
  } else {
    # parse text to json
    asjson <- jsonlite::fromJSON(text)
    # parse to sf
    if (type %in% c("SF", "Both")) {
      if (type == "SF") {
        remove <- TRUE
      } else {
        remove <- FALSE
      }
      response <- sf::st_as_sf(asjson,
        coords = c("lng", "lat"),
        remove = remove,
        crs = 4326
      )
      return(response)
    } else {
      return(asjson)
    }
  }
}
