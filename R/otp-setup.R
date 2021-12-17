#' Build an OTP Graph
#'
#' @description OTP is run in Java and requires Java commands to be typed into
#'   the command line. The function allows the parameters to be defined in R and
#'   automatically passed to Java. This function builds a OTP graph from the
#'   Open Street Map and other files.
#'
#' @param otp A character string, path to the OTP .jar file
#' @param dir A character string, path to a directory containing the necessary
#'   files, see details
#' @param memory A positive integer. Amount of memory to assign to the OTP in
#'   MB, default is 2048
#' @param router A character string for the name of the router, must subfolder
#'   of  dir/graphs, default "default". See vignettes for details.
#' @param flag64bit Logical, if true the -d64 flag is added to Java instructions,
#'   ignored if otp_version >= 2
#' @param quiet Logical, if FALSE the Java commands will be printed to console
#' @param otp_version Numeric, version of OTP to build, default NULL when version
#'   is auto-detected
#' @return Character vector of messages produced by OTP, and will return the
#'   message "Graph built" if successful
#' @details The OTP .jar file can be downloaded from
#'   https://repo1.maven.org/maven2/org/opentripplanner/otp/
#'
#'   To build an OTP graph requires the following files to be in the directory
#'   specified by the dir variable.
#'
#'   /graphs - A sub-directory
#'
#'   /default - A sub-directory with the name of the OTP router used in router'
#'   variable
#'
#'   osm.pbf - Required, pbf file containing the Open Street Map
#'
#'   router-config.json - Required, json file containing configurations settings
#'   for the OTP
#'
#'   gtfs.zip - Optional, and number of GTFS files with transit timetables
#'
#'   terrain.tif - Optional, GeoTiff image of terrain map
#'
#'   The function will accept any file name for the .jar file, but it must be
#'   the only .jar file in that directory OTP can support multiple routers (e.g.
#'   different regions), each router must have its own sub-directory in the
#'   graphs directory
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
                            flag64bit = TRUE,
                            quiet = TRUE,
                            otp_version = NULL) {

  # Run Checks
  checkmate::assert_numeric(memory, lower = 500)
  checkmate::assert_numeric(otp_version, lower = 1, upper = 2.999, null.ok = TRUE)

  # Check OTP version
  if (is.null(otp_version)) {
    otp_version <- otp_version_check(otp)
  }

  check <- otp_checks(otp = otp, dir = dir, router = router, graph = FALSE, otp_version = otp_version)
  if (!check) {
    warnings()
    stop("Basic checks failed, please check your inputs")
  }

  if (otp_version >= 2) {
    text <- paste0(
      "java -Xmx", memory, "M"
    )

    text <- paste0(
      text, ' -jar "',
      otp,
      '" --build --save "',
      dir,
      "/graphs/",
      router,
      '"'
    )
  } else {
    text <- paste0(
      "java -Xmx", memory, "M"
    )

    if (flag64bit) {
      text <- paste0(text, " -d64")
    }

    text <- paste0(
      text, ' -jar "',
      otp,
      '" --build "',
      dir,
      "/graphs/",
      router,
      '"'
    )
  }

  message(paste0(
    Sys.time(),
    " Basic checks completed, building graph, this may take a few minutes"
  ))

  message("The graph will be saved to ", dir, "/graphs/", router)

  if (!quiet) {
    message("Command Sent to Java:")
    message(text)

  }

  set_up <- try(system(text, intern = TRUE))
  message(" ")


  if ("try-error" %in% class(set_up)) {
    stop(paste0("Graph Build Failed: ", set_up[1]))
  }


  # Check for errors
  msg <- set_up[grepl("Graph building took", set_up, ignore.case = TRUE)]
  if (length(msg) == 0) {
    message("Error: OTP did not report a sucessfull graph build")
    message("Last reported steps:")
    for (i in seq(length(set_up) - 5, length(set_up))) {
      message(set_up[i])
    }
  } else {
    message(substr(msg, 42, nchar(msg)))
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
#' @param pointsets Logical. Should the pointsets be loaded?
#'     Default FALSE
#' @param wait Logical, Should R wait until OTP has loaded before
#'     running next line of code, default TRUE
#' @param flag64bit Logical, if true the -d64 flag is added to Java instructions,
#'     ignored if otp_version >= 2
#' @param quiet Logical, if FALSE the Java commands will be printed to console
#' @param otp_version Numeric, version of OTP to build, default NULL when version
#'     is auto-detected
#' @param open_browser Logical, if TRUE web browser is loaded when OTP is ready
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
                      pointsets = FALSE,
                      wait = TRUE,
                      flag64bit = TRUE,
                      quiet = TRUE,
                      otp_version = NULL,
                      open_browser = TRUE) {

  # Run Checks
  checkmate::assert_numeric(memory, lower = 500)
  checkmate::assert_numeric(otp_version, lower = 1, upper = 2.999, null.ok = TRUE)
  checkmate::assert_numeric(port)
  checkmate::assert_numeric(securePort)
  checkmate::assert_logical(analyst)
  checkmate::assert_logical(pointsets)
  checkmate::assert_logical(wait)
  checkmate::assert_logical(flag64bit)
  memory <- floor(memory)

  # Check OTP version
  if (is.null(otp_version)) {
    otp_version <- otp_version_check(otp)
  }

  if (otp_version >= 2) {
    flag64bit <- FALSE
  }

  # Setup request
  if (otp_version >= 2) {
    text <- paste0(
      "java -Xmx", memory, "M"
    )

    text <- paste0(
      text, ' -jar "',
      otp,
      '" --load "',
      dir,
      "/graphs/",
      router,
      '"',
      ' --port ', port,
      ' --securePort ', securePort

    )
  } else {
    text <- paste0(
      'java -Xmx', memory, 'M'
    )

    if (flag64bit) {
      text <- paste0(text, ' -d64')
    }
    text <- paste0(text,
      ' -jar "',
      otp,
      '" --router ', router,
      ' --graphs "', dir, '/graphs"',
      ' --server --port ', port,
      ' --securePort ', securePort
    )
  }

  if (analyst) {
    if (otp_version >= 2) {
      message("Analyst is not supported by OTP 2.x")
    } else {
      text <- paste0(text, " --analyst")
    }
  }

  if (pointsets) {
    if (otp_version >= 2) {
      message("Analyst is not supported by OTP 2.x")
    } else {
      dir_poinsets <- file.path(dir,"pointsets")
      if(!dir.exists(dir_poinsets)){
        dir.create(dir_poinsets)
        #stop("PointSets requested but folder ",dir_poinsets," does not exist")
      }
      text <- paste0(text, ' --pointSets "',dir_poinsets,'"')
    }
  }


  # Run extra checks
  check <- otp_checks(otp = otp, dir = dir, router = router, graph = TRUE, otp_version = otp_version)
  if (!check) {
    stop("Basic checks have failed, please check your inputs")
  }

  if (!quiet) {
    message("Command Sent to Java:")
    message(text)

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
    Sys.sleep(30)

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
        if(open_browser){
          utils::browseURL(paste0(
            ifelse(otpcon$ssl,
                   "https://", "http://"
            ),
            "localhost:", port
          ))
        }
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
#' @param otp Path to otp.jar
#' @param dir A character string path to a folder containing the necessary
#'     files, see details
#' @param router A character string for the name of the router, must be a
#'     subfolder of dir/graphs, default "default"
#' @param graph Logical, check for graph, default = FALSE
#' @param otp_version Numeric, OTP version number e.g. 1.5
#' @family internal
#' @noRd

otp_checks <- function(otp = NULL,
                       dir = NULL,
                       router = NULL,
                       graph = FALSE,
                       otp_version = NULL) {
  # Checks
  checkmate::assertDirectoryExists(dir)
  checkmate::assertDirectoryExists(paste0(dir, "/graphs/", router))
  checkmate::assertFileExists(otp, extension = "jar")


  if (graph) {
    # Check that the graph exists, and is over 5KB
    chkG <- file.exists(paste0(dir, "/graphs/", router, "/Graph.obj"))
    chkg <- file.exists(paste0(dir, "/graphs/", router, "/graph.obj"))

    if(!(chkG | chkg)){
      stop("File does not exist")
    }

    if(chkG){
      size <- file.info(paste0(dir, "/graphs/", router, "/Graph.obj"))
      size <- size$size
      if (size < 5000) {
        warning("Graph.obj exists but is very small, the build process may have failed")
        return(FALSE)
      }
    }

    if(chkg){
      size <- file.info(paste0(dir, "/graphs/", router, "/graph.obj"))
      size <- size$size
      if (size < 5000) {
        warning("graph.obj exists but is very small, the build process may have failed")
        return(FALSE)
      }
    }

  } else {
    # Check the data to build a graph exists
    fls <- list.files(file.path(dir, "/graphs/", router))
    if(length(fls) == 0){
      warning(paste0("There are no files in ",file.path(dir, "/graphs/", router)))
      return(FALSE)
    }
    fls <- fls[grepl(".osm.pbf", fls)]
    if(length(fls) == 0){
      warning(paste0("There are no osm.pbf files in ",file.path(dir, "/graphs/", router)))
      return(FALSE)
    }
  }

  if (otp_check_java(otp_version = otp_version)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Check Java version
#'
#' Check if you have the correct version of Java for running OTP locally
#' @param otp_version numeric, OTP version number default 1.5
#' @family setup
#' @export
#'
otp_check_java <- function(otp_version = 1.5) {
  checkmate::assert_numeric(otp_version, lower = 1, upper = 2.999)
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
      warning("R is unable to tell what version of Java you have")
      return(FALSE)
    }

    if (otp_version < 2) {
      if (java_version >= 1.8 & java_version < 1.9) {
        message("You have the correct version of Java for OTP 1.x")
        return(TRUE)
      }

      if (java_version == 11) {
        warning("You have OTP 1.x but the version of Java for OTP 2.x")
        return(FALSE)
      }

      warning("OTP 1.x requires Java version 1.8 you have version ", java_version)
      return(FALSE)
    }

    if (otp_version >= 2) {
      if (java_version >= 1.8 & java_version < 1.9) {
        warning("You have OTP 2.x but the version of Java for OTP 1.x")
        return(FALSE)
      }

      if (java_version == 11) {
        message("You have the correct version of Java for OTP 2.x")
        return(TRUE)
      }

      warning("OTP 2.x requires Java version 11 you have version ", java_version)
      return(FALSE)
    }

    warning("OTP requires Java version 1.8 you have version ", java_version)
    return(FALSE)
  }
}

#' Check OPT Version from file path
#'
#' @param otp character
#' @family internal
#' @noRd
#'
otp_version_check <- function(otp) {
  otp_version <- strsplit(otp, "/")[[1]]
  otp_version <- otp_version[length(otp_version)]
  otp_version <- gsub("[^[:digit:]., ]", "", otp_version)
  otp_version <- substr(otp_version, 1, 1)
  if (otp_version == "1") {
    return(1)
  } else if (otp_version == "2") {
    return(2)
  } else {
    stop("Unable to detect OTP version, please specify using otp_version")
  }
}
