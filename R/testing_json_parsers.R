if(FALSE){
  library(opentripplanner)
  # Path to a folder containing the OTP.jar file, change to where you saved the file.
  path_data <- file.path(tempdir(), "OTP")
  dir.create(path_data)
  path_otp <- otp_dl_jar(path_data)
  otp_dl_demo(path_data)
  # Build Graph and start OTP
  log1 <- otp_build_graph(otp = path_otp, dir = path_data)
  log2 <- otp_setup(otp = path_otp, dir = path_data)
  otpcon <- otp_connect(timezone = "Europe/London")

  fromPlace <- c(50.64380, -1.17142)
  toPlace <- c(50.72602, -1.15974)
  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
  toPlace <- format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)

  fromPlace <- paste(fromPlace, collapse = ",")
  toPlace <- paste(toPlace, collapse = ",")

  routerUrl <- opentripplanner:::make_url.otpconnect(otpcon)
  routerUrl <- paste0(routerUrl, "/plan")


  query <- list(
    fromPlace = fromPlace,
    toPlace = toPlace,
    mode = "WALK,TRANSIT"
  )

  req <- httr::GET(
    routerUrl,
    query = query
  )

  # profvis::profvis({for(q in 1:10){
  #   f2 <- otp_json2sf_alt(rjson::fromJSON(text))
  # }}, interval = 0.005)


  text <- httr::content(req, as = "text", encoding = "UTF-8")
  # parse text to json

  #asjson <- jsonlite::fromJSON(text)
  asjson <- rjson::fromJSON(text)
  # json_example_drive <- asjson
  # json_example_transit <- asjson


  # bench::mark(f1 <- opentripplanner:::otp_json2sf(jsonlite::fromJSON(text)),
  #             f2 <- otp_json2sf_alt(rjson::fromJSON(text)),
  #             check = FALSE, relative = TRUE)

  for(a in 1:ncol(f1)){
    if(!identical(f1[[a]], f2[[a]])){
      message(a)
    }
  }

  foo1 <- sf::st_coordinates(f1)
  foo2 <- sf::st_coordinates(f2)
  plot(foo1[,3])
  plot(foo2[,3])
  object.size(f1) / object.size(f2)

}
