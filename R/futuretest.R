# library(opentripplanner)
# # Path to a folder containing the OTP.jar file, change to where you saved the file.
# path_data <- file.path(tempdir(), "OTP")
# dir.create(path_data)
# path_otp <- otp_dl_jar(version = "1.4.0")
# otp_dl_demo(path_data)
# # Build Graph and start OTP
# log1 <- otp_build_graph(otp = path_otp, dir = path_data)
# log2 <- otp_setup(otp = path_otp, dir = path_data)
# otpcon <- otp_connect(timezone = "Europe/London")
#
# lsoa <- sf::st_read("https://github.com/ropensci/opentripplanner/releases/download/0.1/centroids.gpkg",
#                     stringsAsFactors = FALSE)
#
# toPlace   = lsoa[rep(seq(1, nrow(lsoa)), times = nrow(lsoa)),]
# fromPlace = lsoa[rep(seq(1, nrow(lsoa)), each  = nrow(lsoa)),]
#
# system.time({routes <- otp_plan(otpcon = otpcon,
#                    fromPlace = fromPlace,
#                    toPlace = toPlace,
#                    fromID = fromPlace$geo_code,
#                    toID = toPlace$geo_code,
#                    ncores = 4)})
# #164?? second on CRAN
