#' Download OTP Jar File
#'
#' @description
#' Download the OTP jar file from maven.org
#'
#' @param path path to folder where OTP is to be stored
#' @param version a character string of the version number default is "1.4.0"
#' @param file_name file name to give the otp default "otp.jar"
#' @param url URL to the dowload server
#' @return
#' The path to the OTP file
#' @family setup
#' @examples
#' \dontrun{
#' otp_dl_jar(tempdir())
#' }
#' @export

otp_dl_jar <- function(path = NULL,
                       version = "1.4.0",
                       file_name = "otp.jar",
                       url = "https://repo1.maven.org/maven2/org/opentripplanner/otp") {
  url <- paste0(url, "/", version, "/otp-", version, "-shaded.jar")
  destfile <- file.path(path, file_name)
  message("The OTP will be saved to ", destfile)
  utils::download.file(url = url, destfile = destfile, mode = "wb")
  return(destfile)
}


#' Download Demo Data
#'
#' @description
#' Download the demonstration data for the Isle of Wight
#'
#' @param path_data path to folder where data for OTP is to be stored
#' @param url URL to data
#' @family setup
#' @examples
#' \dontrun{
#' otp_dl_demo(tempdir())
#' }
#' @export

otp_dl_demo <- function(
  path_data = NULL,
  url = paste0("https://github.com/ropensci/opentripplanner/",
               "releases/download/0.1/isle-of-wight-demo.zip")
  ) {
  if (!dir.exists(path_data)) {
    stop(paste0("Can't find folder ", path_data))
  }
  if (!dir.exists(file.path(path_data, "graphs"))) {
    dir.create(file.path(path_data, "graphs"))
  }
  if (!dir.exists(file.path(path_data, "graphs", "default"))) {
    dir.create(file.path(path_data, "graphs", "default"))
  }
  message("The demo data will be saved to ", path_data)

  utils::download.file(
    url = url,
    destfile = file.path(path_data, "isle-of-wight-demo.zip"),
    mode = "wb"
  )
  utils::unzip(file.path(path_data, "isle-of-wight-demo.zip"),
    exdir = file.path(path_data, "graphs", "default")
  )
  unlink(file.path(path_data, "isle-of-wight-demo.zip"))
}
