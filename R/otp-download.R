#' Download OTP Jar File
#'
#' @description Download the OTP jar file from maven.org
#'
#' @param path path to folder where OTP is to be stored
#' @param version a character string of the version number default is "1.4.0"
#' @param file_name file name to give the otp default "otp.jar"
#' @param url URL to the dowload server
#' @param quiet logical, passed to download.file, default FALSE
#' @param cache logical, default TRUE, see details
#' @return The path to the OTP file
#' @details As of version 0.3.0.0 `otp_dl_jar` will cache the JAR file within
#' the package and ignore the `path` argument. You can force a new download to
#' be saved in the `path` location by setting `cache = FALSE`.
#' @family setup
#' @examples
#' \dontrun{
#' otp_dl_jar(tempdir())
#' }
#' @export

otp_dl_jar <- function(path = NULL,
                       version = "1.4.0",
                       file_name = paste0("otp-", version, "-shaded.jar"),
                       url = "https://repo1.maven.org/maven2/org/opentripplanner/otp",
                       quiet = FALSE,
                       cache = TRUE) {
  if(cache){
    # Check we can find the package
    libs <- .libPaths()[1]
    if(!checkmate::test_directory_exists(file.path(libs,"opentripplanner"))){
      cache <- FALSE
    }
  }

  if(cache){
    # Check for JAR folder can find the package
    if(!checkmate::test_directory_exists(file.path(libs,"opentripplanner","jar"))){
      dir.create(file.path(libs,"opentripplanner","jar"))
    }
    destfile <- file.path(libs,"opentripplanner","jar",file_name)
    if(checkmate::test_file_exists(destfile)){
      return(destfile)
    }
  } else {
    checkmate::assert_directory_exists(path)
    destfile <- file.path(path, file_name)
  }

  url <- paste0(url, "/", version, "/otp-", version, "-shaded.jar")
  message("The OTP will be saved to ", destfile)
  utils::download.file(url = url, destfile = destfile, mode = "wb", quiet = quiet)
  return(destfile)
}


#' Download Demo Data
#'
#' @description
#' Download the demonstration data for the Isle of Wight
#'
#' @param path_data path to folder where data for OTP is to be stored
#' @param url URL to data
#' @param quiet logical, passed to download.file, default FALSE
#' @family setup
#' @examples
#' \dontrun{
#' otp_dl_demo(tempdir())
#' }
#' @export

otp_dl_demo <- function(
  path_data = NULL,
  url = paste0("https://github.com/ropensci/opentripplanner/",
               "releases/download/0.1/isle-of-wight-demo.zip"),
  quiet = FALSE
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
    mode = "wb",
    quiet = quiet
  )
  utils::unzip(file.path(path_data, "isle-of-wight-demo.zip"),
    exdir = file.path(path_data, "graphs", "default")
  )
  unlink(file.path(path_data, "isle-of-wight-demo.zip"))
}
