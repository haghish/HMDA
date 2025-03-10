#' Partition a data frame into training, testing, and (optionally) validation sets
#' and upload it to H2O local server on your machine
#'
#' This function partitions a data frame into training, testing, and optionally
#' validation sets. If \code{y} (the name of an outcome column) is provided and
#' is a factor or character, the function uses stratified splitting to preserve
#' class proportions. Otherwise, it uses a basic random split. By default, the
#' sum of \code{train} and \code{test} must be 1 if \code{validation} is not
#' given. If \code{validation} is specified, \code{train} + \code{test} +
#' \code{validation} must equal 1.
#'
#' If \code{global = TRUE}, all partitioned data frames are written into the
#' global environment and uploaded to an h2o server with names matching those in
#' the returned list (e.g., "hmda.train" will also generate "hmda.train.hex").
#'
#' @param df A data frame.
#' @param y A string indicating the name of the outcome column. If provided, it
#'   should match a column name in \code{df}.
#' @param train A numeric value indicating the proportion of data for the
#'   training set.
#' @param test A numeric value indicating the proportion of data for the
#'   testing set.
#' @param validation Optional numeric value indicating the proportion of data
#'   for the validation set. Defaults to \code{NULL}.
#' @param global logical. If \code{TRUE}, the created data frames will be
#'   assigned to the global environment and also uploaded to h2o with matching
#'   names.
#' @param seed A numeric seed for reproducibility. Defaults to 2025.
#'
#' @return A named list containing the training, testing, and (optionally)
#'   validation data frames.
#'
#' @examples
#' \dontrun{
#' # Install/load required packages:
#' # install.packages("splitTools")
#' library(splitTools)
#' library(h2o)
#' h2o.init()
#'
#' # Using the iris dataset:
#' data(iris)
#'
#' # 1) Random partition: 80% train, 20% test
#' splits <- hmda.partition(
#'   df = iris,
#'   train = 0.8,
#'   test = 0.2,
#'   global = FALSE
#' )
#' train_data <- splits$hmda.train
#' test_data  <- splits$hmda.test
#'
#' # 2) Stratified partition: 70% train, 15% test, 15% validation
#' splits_strat <- hmda.partition(
#'   df = iris,
#'   y = "Species",
#'   train = 0.7,
#'   test = 0.15,
#'   validation = 0.15,
#'   global = FALSE
#' )
#' train_data_strat <- splits_strat$hmda.train
#' test_data_strat  <- splits_strat$hmda.test
#' validation_data_strat <- splits_strat$hmda.validation
#'
#' # 3) With global=TRUE, data frames are created in the global environment
#' #    and also uploaded to h2o (e.g., hmda.train and hmda.train.hex).
#' }
#'
#' @export

hmda.partition <- function(df,
                           y = NULL,
                           train = 0.80,
                           test = 0.20,
                           validation = NULL,
                           global = FALSE,
                           seed = 2025) {

  if (!requireNamespace("splitTools", quietly = TRUE)) {
    stop("Package 'splitTools' is required. Please install it.")
  }

  # Basic checks on df
  if (missing(df) || !is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }

  # If y is provided, check if it exists in df
  if (!is.null(y) && !y %in% names(df)) {
    stop("The provided 'y' does not match any column in 'df'.")
  }

  # Validate that train/test/(validation) sum to 1
  if (is.null(validation)) {
    if (abs(train + test - 1) > .Machine$double.eps^0.5) {
      stop("train + test must sum to 1 if 'validation' is NULL.")
    }
  } else {
    if (abs(train + test + validation - 1) > .Machine$double.eps^0.5) {
      stop("train + test + validation must sum to 1 if 'validation' is specified.")
    }
  }

  set.seed(seed)

  # Decide how to split: stratified (for categorical) or random
  if (!is.null(y) && (is.factor(df[[y]]) || is.character(df[[y]]))) {
    # Stratified partition
    split_args <- list(
      y = df[[y]],
      type = "stratified"
    )
  } else {
    # Basic random partition
    split_args <- list(
      y = seq_len(nrow(df)),
      type = "basic"
    )
  }

  # Create proportions vector
  if (is.null(validation)) {
    split_args$p <- c(train, test)
  } else {
    split_args$p <- c(train, test, validation)
  }

  # Perform partition using splitTools
  splits <- do.call(splitTools::partition, split_args)
  hmda.train <- df[splits[[1]], , drop = FALSE]
  hmda.test  <- df[splits[[2]], , drop = FALSE]
  hmda.train.hex <- h2o::as.h2o(hmda.train, destination_frame = "hmda.train.hex")
  hmda.test.hex <- h2o::as.h2o(hmda.train, destination_frame = "hmda.test.hex")

  # Create the outputs
  if (!is.null(validation)) {
    hmda.validation <- df[splits[[3]], , drop = FALSE]
    hmda.validation.hex <- h2o::as.h2o(df[splits[[3]], , drop = FALSE], destination_frame = "hmda.validation.hex")
    out <- list(
      hmda.train = hmda.train,
      hmda.test = hmda.test,
      hmda.validation = hmda.validation,
      hmda.train.hex = hmda.train.hex,
      hmda.test.hex = hmda.test.hex,
      hmda.validation.hex = hmda.validation.hex
    )
  }
  else {
    out <- list(
      hmda.train = hmda.train,
      hmda.test = hmda.test,
      hmda.train.hex = hmda.train.hex,
      hmda.test.hex = hmda.test.hex
    )
  }



  # If validation is not NULL, add it to 'out'


  # If global = TRUE, assign data frames and h2o frames to global env
  if (isTRUE(global)) {
    if (!requireNamespace("h2o", quietly = TRUE)) {
      stop("Package 'h2o' must be installed to upload frames to h2o.")
    }

    # Optionally, check if h2o is running;
    # if (!h2o::h2o.connection()) {
    #   h2o::h2o.init()
    # }

    for (nm in names(out)) {
      # Assign data frame globally
      message(paste0("uploading ",nm, ".hex data to the local h2o server on your machine"))
      assign(nm, out[[nm]], envir = .GlobalEnv)
      # Upload to h2o with the same name
      assign(
        paste0(nm, ".hex"),
        h2o::as.h2o(out[[nm]], destination_frame = paste0(nm,".hex")),
        envir = .GlobalEnv
      )
    }
  }

  return(out)
}
