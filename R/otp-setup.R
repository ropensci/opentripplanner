#' Build an OTP Graph
#'
#' @description
#' OTP is run in Java and requires Java commands to be typed into the
#' command line. The function allows the parameters to be defined in
#' R and automatically passed to Java. This function builds a OTP
#' graph from the Open Street Map and other files.
#'
#' @param otp A character string, path to the OTP .jar file
#' @param dir A character string, path to a directory containing the
#'     necessary files, see details
#' @param memory A positive integer. Amount of memory to assign to
#'     the OTP in MB, default is 2048
#' @param router A character string for the name of the router, must
#'     subfolder of  dir/graphs, default "default". See vignettes for details.
#' @param analyst Logical, should analyst feature be built, default FALSE. See advanced vignette for details.
#' @return
#' Character vector of messages produced by OTP, and will return the message
#'     "Graph built" if successful
#' @details
#' The OTP .jar file can be downloaded from
#'     https://repo1.maven.org/maven2/org/opentripplanner/otp/
#'
#' To build an OTP graph requires the following files to be in the directory
#' specified by the dir variable.
#'
#' /graphs - A sub-directory
#'
#'   /default - A sub-directory with the name of the OTP router used in router' variable
#'
#'     osm.pbf - Required, pbf file containing the Open Street Map
#'
#'     router-config.json - Required, json file containing configurations settings for the OTP
#'
#'     gtfs.zip - Optional, and number of GTFS files with transit timetables
#'
#'     terrain.tif - Optional, GeoTiff image of terrain map
#'
#' The function will accept any file name for the .jar file, but it must be
#' the only .jar file in that directory OTP can support multiple routers
#' (e.g. different regions), each router must have its own sub-directory in
#' the graphs directory
#' @family setup
#' @examples
#' \dontrun{
#' log <- otp_build_graph(otp = "C:/otp/otp.jar", dir = "C:/data")
#' }
#' @export
otp_build_graph <- function(otp = NULL,
                            dir = NULL,
                            memory = 2048,
                            router = "default",
                            analyst = FALSE) {

  # Run Checks
  checkmate::assert_numeric(memory, lower = 500)

  text <- paste0(
    "java -Xmx",
    memory,
    'M -jar "',
    otp,
    '" --build "',
    dir,
    "/graphs/",
    router,
    '"'
  )

  if (analyst) {
    text <- paste0(text, " --analyst")
  }

  check <- otp_checks(otp = otp, dir = dir, router = router, graph = FALSE)
  if (!check) {
    stop()
  }
  message(paste0(
    Sys.time(),
    " Basic checks completed, building graph, this may take a few minutes"
  ))
  message("The graph will be saved to ", dir)
  set_up <- try(system(text, intern = TRUE))

  # Check for errors
  if (any(grepl("ERROR", set_up, ignore.case = TRUE)) & length(set_up) < 10) {
    message(paste0(Sys.time(), " Failed to build graph with message:"))
    message(set_up)
  } else {
    message(paste0(Sys.time(), " Graph built"))
  }
  return(set_up)
}


#' Set up an OTP instance.
#'
#' @description
#' OTP is run in Java and requires Java commands to be typed into the
#' command line. The function allows the parameters to be defined in
#' R and automatically passed to Java. This function sets up a local
#' instance of OTP, for remote versions see documentation.
#'
#' The function assumes you have run otp_build_graph()
#' @param otp A character string, path to the OTP .jar file
#' @param dir A character string, path to a directory containing the
#'     necessary files, see details
#' @param memory A positive integer. Amount of memory to assign to
#'     the OTP in MB, default is 2048
#' @param router A character for the name of the router to use, must
#'     be subfolder of dir/graphs, default "default". See
#'     vignettes for details.
#' @param port A positive integer. Optional, default is 8080.
#' @param securePort A positive integer. Optional, default is 8081.
#' @param analyst Logical. Should the analyst features be loaded?
#'     Default FALSE
#' @param wait Logical, Should R wait until OTP has loaded before
#'     running next line of code, default TRUE
#' @family setup
#' @return
#' This function does not return a value to R. If wait is TRUE R
#' will wait until OTP is running (maximum of 5 minutes).
#' After 5 minutes (or if wait is FALSE) the function will return
#' R to your control, but the OTP will keep loading.
#' @details
#'
#' To run an OTP graph must have been created using otp_build_graph
#' and the following files to be in the directory specified by the
#' dir variable.
#'
#' /graphs - A sub-directory
#'
#'   /default - A sub-directory with the name of the OTP router used in 'router' variable
#'
#'     graph.obj  OTP graph
#'
#' @examples
#' \dontrun{
#' otp_setup(
#'   otp = "C:/otp/otp.jar",
#'   dir = "C:/data"
#' )
#' otp_setup(
#'   otp = "C:/otp/otp.jar",
#'   dir = "C:/data",
#'   memory = 5000,
#'   analyst = TRUE
#' )
#' }
#' @export
otp_setup <- function(otp = NULL,
                      dir = NULL,
                      memory = 2048,
                      router = "default",
                      port = 8080,
                      securePort = 8081,
                      analyst = FALSE,
                      wait = TRUE) {
  # Run Checks
  checkmate::assert_numeric(memory, lower = 500)
  memory <- floor(memory)

  # Setup request
  text <- paste0(
    "java -Xmx", memory, 'M -jar "',
    otp,
    '" --router ', router,
    ' --graphs "', dir, '/graphs"',
    " --server --port ", port,
    " --securePort ", securePort
  )

  if (analyst) {
    text <- paste0(text, " --analyst")
  }

  # Run extra checks
  check <- otp_checks(otp = otp, dir = dir, router = router, graph = TRUE)
  if (!check) {
    stop()
  }

  # Set up OTP
  if (!checkmate::testOS("solaris")) {
    set_up <- try(system(text, intern = FALSE, wait = FALSE))
  } else {
    stop("You're on and unknow OS, this function is not yet supported")
  }

  # Check for errors
  if (grepl("ERROR", set_up[2], ignore.case = TRUE)) {
    stop(paste0(
      "Failed to start OTP with message: ",
      set_up[2]
    ))
  }

  message(paste0(
    Sys.time(),
    " OTP is loading and may take a while to be useable"
  ))

  if (wait) {
    Sys.sleep(60)

    # Check if connected
    for (i in 1:30) {
      # message(paste0("Attempt ",i))
      otpcon <- try(otp_connect(
        hostname = "localhost",
        router = router,
        port = port,
        ssl = FALSE,
        check = TRUE
      ), silent = TRUE)

      if ("otpconnect" %in% class(otpcon)) {
        message(paste0(
          Sys.time(),
          " OTP is ready to use Go to localhost:",
          port,
          " in your browser to view the OTP"
        ))
        utils::browseURL(paste0(
          ifelse(otpcon$ssl,
            "https://", "http://"
          ),
          "localhost:", port
        ))
        break
      } else {
        if (i < 30) {
          Sys.sleep(30)
        } else {
          message(paste0(
            Sys.time(),
            " OTP is taking an unusually long time to load, releasing R to your control, OTP will continue in the background"
          ))
        }
      }
    }
  }
}

#' Stop and OTP Instance
#'
#' @description
#' OTP is run in Java and requires Java commands to be typed
#' into the command line. The function allows the parameters
#' to be defined in R and automatically passed to Java.
#' This function stops an already running OTP instance
#' @param warn Logical, should you get a warning message
#' @param kill_all Logical, should all Java instances be killed?
#'
#' @details
#' The function assumes you have run otp_setup()
#' @return This function return a message but no object
#' @family setup
#' @examples
#' \dontrun{
#' otp_stop(kill_all = FALSE)
#' }
#' @export
otp_stop <- function(warn = TRUE, kill_all = TRUE) {
  if (warn && interactive()) {
    readline(
      prompt =
        "This will force Java to close, Press [enter] to continue, [escape] to abort"
    )
  }

  if (checkmate::testOS("linux") | checkmate::testOS("mac")) {
    message("The following Java instances have been found:")
    system("ps -A |grep java")
    if (!kill_all) {
      kill_all <- utils::askYesNo("Kill all of them?")
    }

    if (kill_all) {
      system("pkill -9 java", intern = TRUE)
    } else {
      message(
        "Kill the instances manually, e.g. with:\n",
        "kill -9 PID\n",
        "where PID is the id of the Java instance"
      )
    }
  } else if (checkmate::testOS("windows")) {
    system("Taskkill /IM java.exe /F", intern = TRUE)
  } else {
    message("You're on an unknown OS, this function is not yet supported")
  }
}


#' Basic OTP Setup Checks
#'
#' Checks to run before setting up the OTP
#'
#' @param dir A character string path to a folder containing the necessary
#'     files, see details
#' @param router A character string for the name of the router, must be a
#'     subfolder of dir/graphs, default "default"
#' @param graph Logical, check for graph, default = FALSE
#' @param otp Path to otp.jar
#' @family internal
#' @noRd

otp_checks <- function(otp = NULL, dir = NULL, router = NULL, graph = FALSE) {
  # Checks
  checkmate::assertDirectoryExists(dir)
  checkmate::assertDirectoryExists(paste0(dir, "/graphs/", router))
  checkmate::assertFileExists(otp, extension = "jar")

  # Check that the graph exists, and is over 5KB
  if (graph) {
    checkmate::assertFileExists(paste0(dir, "/graphs/", router, "/Graph.obj"))
    size <- file.info(paste0(dir, "/graphs/", router, "/Graph.obj"))
    size <- size$size
    if (size < 5000) {
      warning("Graph.obj exists but is very small, the build process may have failed")
      return(FALSE)
    }
  }

  # Check we have correct verrsion of Java
  java_version <- try(system2("java", "-version", stdout = TRUE, stderr = TRUE))
  if (class(java_version) == "try-error") {
    warning("R was unable to detect a version of Java")
    return(FALSE)
  } else {
    java_version <- java_version[1]
    java_version <- strsplit(java_version, "\"")[[1]][2]
    java_version <- strsplit(java_version, "\\.")[[1]][1:2]
    java_version <- as.numeric(paste0(java_version[1], ".", java_version[2]))
    if (is.na(java_version)) {
      warning("OTP requires Java version 8 ")
      return(FALSE)
    }
    if (java_version < 1.8 | java_version >= 1.9) {
      warning("OTP requires Java version 8 ")
      return(FALSE)
    }
  }

  return(TRUE)
}
