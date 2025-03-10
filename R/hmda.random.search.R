#' Search for Hyperparameters with random search
#'
#' This function runs an automated hyperparameter search for machine learning models
#' using H2O AutoML. It calls \code{h2o.automl()} (from the \pkg{h2o} package) with the
#' specified arguments and then extracts each model's hyperparameters using
#' \code{automlModelParam()} from the \pkg{h2otools} package. The function returns a list
#' with the AutoML object and a merged data frame that includes both leaderboard
#' performance metrics and hyperparameter settings for each model.
#'
#' @param algorithm Character vector. The algorithms to include in the AutoML run.
#'   Supported options typically include \code{"drf"} (Distributed Random Forest) and \code{"gbm"}
#'   (Gradient Boosting Machine). Default is \code{c("drf", "gbm")}.
#' @param x Vector. The names or indices of the predictor columns.
#' @param y Character. The name (or index) of the response column.
#' @param training_frame An H2OFrame containing the training data.
#'   Default is \code{h2o.getFrame("hmda.train.hex")}.
#' @param validation_frame An H2OFrame for early stopping. Default is \code{NULL}.
#' @param max_models Integer. The maximum number of models to build in the AutoML run.
#'   Default is \code{100}.
#' @param fold_assignment Character. The method for assigning folds in cross-validation.
#'   Default is \code{"Modulo"}.
#' @param nfolds Integer. The number of folds for cross-validation. Default is \code{10}.
#' @param seed Integer. A seed for reproducibility. Default is \code{NULL}.
#' @param fold_column Character. The column name for cross-validation fold assignment.
#'   Default is \code{NULL}.
#' @param weights_column Character. The column name for observation weights. Default is \code{NULL}.
#' @param keep_cross_validation_predictions Logical. Whether to keep cross-validation predictions.
#'   Default is \code{TRUE}.
#' @param stopping_rounds Integer. The number of rounds with no improvement to stop training.
#'   Default is \code{NULL}.
#' @param stopping_metric Character. The metric to use for early stopping.
#'   Default is \code{"AUTO"}.
#' @param stopping_tolerance Numeric. The relative tolerance for early stopping.
#'   Default is \code{NULL}.
#' @param ... Additional arguments passed to \code{h2o.automl()}.
#'
#' @return A list with two components:
#'   \describe{
#'     \item{grid_search}{The H2O AutoML object returned by \code{h2o.automl()}.}
#'     \item{hyperparameters}{A data frame that merges the leaderboard with hyperparameter
#'       settings extracted from each model. New hyperparameters encountered are added as new columns.}
#'   }
#'
#' @details This function leverages H2O AutoML to run an automated hyperparameter search.
#' It then extracts a leaderboard from the AutoML run and iterates over the model IDs to collect
#' detailed hyperparameter settings via \code{automlModelParam()}. The leaderboard and hyperparameter
#' data frames are merged on the \code{model_id} column. This function requires that the following
#' packages be installed:
#'   \itemize{
#'     \item \pkg{h2o}
#'     \item \pkg{h2otools}
#'     \item \pkg{dplyr}
#'   }
#'
#' @importFrom h2o h2o.automl h2o.getFrame h2o.getModel
#' @importFrom h2otools automlModelParam
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#'   # Initialize H2O (if not already running)
#'   library(h2o)
#'   h2o.init()
#'
#'   # Define predictors and response
#'   predictors <- c("var1", "var2", "var3")
#'   response <- "target"
#'
#'   # Run the hyperparameter search using DRF and GBM algorithms.
#'   result <- hmda.search.param(algorithm = c("drf", "gbm"),
#'                               x = predictors,
#'                               y = response,
#'                               training_frame = h2o.getFrame("hmda.train.hex"),
#'                               max_models = 50,
#'                               nfolds = 5,
#'                               stopping_metric = "AUC",
#'                               stopping_rounds = 3)
#'
#'   # Access the AutoML object:
#'   automl_obj <- result$grid_search
#'
#'   # Access the merged hyperparameters and performance summary:
#'   hyperparam_table <- result$hyperparameters
#'   print(hyperparam_table)
#' }
#'
#' @export
hmda.random.search <- function(algorithm = c("drf", "gbm"),
                              x,
                              y,
                              training_frame = h2o.getFrame("hmda.train.hex"),
                              validation_frame = NULL,
                              max_models = 100,
                              fold_assignment = "Modulo",
                              nfolds = 10,
                              seed = NULL,
                              fold_column = NULL,
                              weights_column = NULL,
                              keep_cross_validation_predictions = TRUE,
                              stopping_rounds = NULL,
                              stopping_metric = "AUTO",
                              stopping_tolerance = NULL,
                              order = "logloss",
                              ...) {

  # Perform automatic model search with H2O AutoML
  search <- h2o.automl(x = x,
                       y = y,
                       training_frame = training_frame,
                       validation_frame = validation_frame,
                       include_algos = algorithm,
                       fold_assignment = fold_assignment,
                       nfolds = nfolds,
                       seed = seed,
                       fold_column = fold_column,
                       weights_column = weights_column,
                       keep_cross_validation_predictions = keep_cross_validation_predictions,
                       max_models = max_models,
                       stopping_rounds = stopping_rounds,
                       stopping_metric = stopping_metric,
                       stopping_tolerance = stopping_tolerance,
                       ... )

  # Get the leaderboard dataset as a data frame
  leaderboard <- as.data.frame(search@leaderboard)

  # Initialize an empty data frame for hyperparameters
  hyperparameters <- NULL

  # Collect the hyperparameters of each model
  for (i in leaderboard$model_ids) {
    print(i)
    hyperparameters <- bind_rows(hyperparameters, automlModelParam(h2o.getModel(i)))
  }

  # Merge the leaderboard with the hyperparameters data frame,
  # ensuring that new columns are added if models contain additional parameters.
  merged <- merge(leaderboard, hyperparameters, by = "model_id", all = TRUE)
  merged <- merged[order(merged[, order], decreasing = TRUE), ]

  # Return the results as a list
  results <- list(
    grid_search = search,
    hyperparameters = merged
  )

  class(results) <- "hmda.random.search"

  return(results)
}

