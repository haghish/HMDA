#' @title Initialize or Restart H2O Cluster for HMDA Analysis
#'
#' @description This function initializes an H2O cluster named \code{"HMDA"},
#' used for Holistic Multi-Model Domain Analysis (HMDA). It wraps
#' around \code{h2o.init()} and attempts to establish a connection to the H2O Java server using
#' the specified resource parameters. If a custom Java path is provided, the function sets the
#' \code{JAVA_HOME} environment variable accordingly. It retries starting the H2O
#' server up to 10 times until a connection is established.
#' If an H2O cluster is already running, this function will restart the cluster.
#' For further details on initialization parameters, refer to the H2O documentation:
#' \url{https://www.rdocumentation.org/packages/h2o/versions/2.4.3.11/topics/h2o.init}.
#'
#' @param ip character. The IP address of the H2O server. Default is \code{"localhost"}.
#' @param port integer. The port number on which the H2O server listens. Default is \code{54321}.
#' @param cpu integer. The number of CPUs to dedicate for HMDA analysis. The default value of \code{-1}
#'   uses all available CPUs.
#' @param ram integer. Specifies the maximum memory allocation in Gigabytes for the H2O cluster.
#'   By default, all available memory is used. A larger memory allocation is recommended, especially
#'   for multicore processing.
#' @param java character. A string specifying the path to the executable 64-bit Java JDK on Microsoft
#'   Windows machines. If provided, this value will be assigned to the \code{JAVA_HOME} environment variable.
#' @param ... Additional arguments passed to \code{h2o.init()}.
#'
#' @return An object representing the connection to the H2O cluster.
#'
#' @importFrom h2o h2o.init h2o.clusterInfo
#' @author E. F. Haghish
#' @examples
#' \dontrun{
#'   # Example 1: Initialize the H2O cluster using all available resources.
#'   hmda.init()
#'   h2o.clusterInfo() #check the status of the cluster with H2O
#'
#'   # Example 2: Initialize the H2O cluster with specific settings for HMDA analysis:
#'   # Use 4 CPUs, allocate 8 GB of memory, set a minimum of 4 GB, and specify a custom JAVA path.
#'   conn <- hmda.init(ip = "localhost", port = 54321, cpu = 4, ram = 8, min_mem_size = 4,
#'                     java = "C:/Program Files/Java/jdk1.8.0_241")
#' }
#'
#' @export

hmda.init <- function(cpu = -1,
                      ram = NULL,
                      java = NULL,
                      ip = "localhost",
                      port = 54321,
                      verbatim = FALSE,
                      ...) {

  # If JAVA is provided, add it to the environment
  # ============================================================
  if (!is.null(java)) {
    Sys.setenv(JAVA_HOME = java)
  }

  # Check the h2o cloud status
  # ============================================================
  # Attempt to get current connection
  connection_exists <- TRUE
  tryCatch(
    expr = {
      if (verbatim) suppressWarnings(h2o.clusterInfo())
      else capture.output(suppressWarnings(h2o.clusterInfo()) , file = nullfile())
    },
    error = function(e) {
      connection_exists <<- FALSE
    }
  )

  if (connection_exists) {
    message("h2o cluster is being restarted")
    try(h2o.shutdown(FALSE), silent = TRUE)
    Sys.sleep(5)
  }

  # Run H2O on the statistics server
  # ============================================================
  keepTrying <- TRUE
  connection <- NULL
  test       <- 1
  while (keepTrying) {
    # h2o.init(jvm_custom_args = c("-help"))
    # h2o.init(jvm_custom_args = c("-session_timeout=100"))
    # bind_to_localhost = FALSE
    # h2o.init(jvm_custom_args=c("-Dsys.ai.h2o.heartbeat.benchmark.enabled=true"))
    tryCatch(suppressWarnings(connection <- h2o::h2o.init(nthreads = cpu,
                                                          startH2O = TRUE,
                                                          name = "HMDA",
                                         min_mem_size = ram,
                                         ip = ip, port = port,
                                         #max_mem_size = max_mem_size,
                                         #ignore_config = ignore_config,
                                         insecure = TRUE,
                                         https = FALSE,
                                         #log_level = if (debug) "DEBUG" else "FATA",
                                         bind_to_localhost = FALSE,
                                         ...)),
             error = function(cond) {
               message("connection to JAVA server failed...\n");
               return()})
    if (!is.null(connection)) {
      keepTrying <- FALSE
    }
    else {
      test <- test + 1
      message("The Java server could not be initiated. It will retry in 3 seconds...")
      Sys.sleep(3)
    }

    if (test > 10) stop("The attempt to start the H2O server was unsuccessful \ndue to an issue within your system...\n")
  }

  return(connection)
}

