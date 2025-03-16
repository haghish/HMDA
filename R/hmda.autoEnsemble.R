#' @title Build Stacked Ensemble Model Using autoEnsemble R package
#' @description This function is a wrapper within the HMDA package that
#'   builds a stacked ensemble model by combining multiple H2O models. It
#'   leverages the \pkg{autoEnsemble} package to stack a set of trained models
#'   (e.g., from HMDA grid) into a stronger meta-learner. For more
#'   details on autoEnsemble, please see the GitHub repository at
#'   \url{https://github.com/haghish/autoEnsemble} and the CRAN package page at
#'   \url{https://CRAN.R-project.org/package=autoEnsemble}.
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.stackedEnsemble h2o.getModel h2o.auc h2o.aucpr h2o.mcc
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
# @importFrom h2otools h2o.get_ids
#' @importFrom curl curl
#' @importFrom autoEnsemble ensemble
#'
#' @param models A grid object, such as HMDA grid, or a character vector of H2O model IDs.
#'   The \code{h2o.get_ids} function from \pkg{h2otools} can be used to extract model
#'   IDs from grids.
#' @param training_frame An H2OFrame (or data frame already uploaded to the H2O server)
#'   that contains the training data used to build the base models.
#' @param newdata An H2OFrame (or data frame already uploaded to the H2O server) to be used
#'   for evaluating the ensemble. If not specified, performance on the training data is used
#'   (for instance, cross-validation performance).
#' @param family A character string specifying the model family.
#' @param strategy A character vector specifying the ensemble strategy. The available
#'   strategy is \code{"search"} (default). The \code{"search"} strategy searches for
#'   the best combination of top-performing diverse models.
#' @param model_selection_criteria A character vector specifying the performance metrics
#'   to consider for model selection. The default is \code{c("auc", "aucpr", "mcc", "f2")}.
#'   Other possible criteria include \code{"f1point5"}, \code{"f3"}, \code{"f4"},
#'   \code{"f5"}, \code{"kappa"}, \code{"mean_per_class_error"}, \code{"gini"}, and
#'   \code{"accuracy"}.
#' @param min_improvement Numeric. The minimum improvement in the evaluation metric
#'   required to continue the ensemble search.
#' @param max Integer. The maximum number of models for each selection criterion.
#'   If \code{NULL}, a default value based on the top rank percentage is used.
#' @param top_rank Numeric vector. Specifies the percentage (or percentages) of the
#'   top models that should be considered for ensemble selection. If the strategy is
#'   \code{"search"}, the function searches for the best combination of models from
#'   the top to the bottom ranked; if the strategy is \code{"top"}, only the first value
#'   is used. Default is \code{seq(0.01, 0.99, 0.01)}.
#' @param stop_rounds Integer. The number of consecutive rounds with no improvement
#'   in the performance metric before stopping the search.
#' @param reset_stop_rounds Logical. If \code{TRUE}, the stopping rounds counter is
#'   reset each time an improvement is observed.
#' @param stop_metric Character. The metric used for early stopping; the default is
#'   \code{"auc"}. Other options include \code{"aucpr"} and \code{"mcc"}.
#' @param seed Integer. A random seed for reproducibility. Default is \code{-1}.
#' @param verbatim Logical. If \code{TRUE}, the function prints additional
#'   progress information for debugging purposes.
#'
#' @return A list containing:
#'   \describe{
#'     \item{model}{The ensemble model built by autoEnsemble.}
#'     \item{top_models}{A data frame of the top-ranked base models that were used
#'           in building the ensemble.}
#'   }
#'
#'
#' @details
#'   This wrapper function integrates with the HMDA package workflow to build a
#'   stacked ensemble model from a set of base H2O models. It calls the
#'   \code{ensemble()} function from the \pkg{autoEnsemble} package to construct the
#'   ensemble. The function is designed to work within HMDA's framework, where base
#'   models are generated via grid search or AutoML. For more details on the autoEnsemble
#'   approach, see:
#'   \itemize{
#'     \item GitHub: \url{https://github.com/haghish/autoEnsemble}
#'     \item CRAN: \url{https://CRAN.R-project.org/package=autoEnsemble}
#'   }
#'
#'   The ensemble strategy \code{"search"} (default) searches for the best combination
#'   of top-performing and diverse models to improve overall performance. The wrapper
#'   returns both the final ensemble model and the list of top-ranked models used in the
#'   ensemble.
#'
#'
#' @examples
#' \dontrun{
#' # load the required libraries for building the base-learners and the ensemble models
#' library(h2o)
# library(h2otools)
#' library(autoEnsemble)
#'
#' # initiate the h2o server
#' h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)
#'
#' # upload data to h2o cloud
#' prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#' prostate <- h2o.importFile(path = prostate_path, header = TRUE)
#'
#' ### H2O provides 2 types of grid search for tuning the models, which are
#' ### AutoML and Grid. Below, I tune 2 set of model grids and use them both
#' ### for building the ensemble, just to set an example ...
#'
#' #######################################################
#' ### PREPARE AutoML Grid (takes a couple of minutes)
#' #######################################################
#' # run AutoML to tune various models (GLM, GBM, XGBoost, DRF, DeepLearning) for 120 seconds
#' y <- "CAPSULE"
#' prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
#' aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 120,
#'                  include_algos=c("DRF","GLM", "XGBoost", "GBM", "DeepLearning"),
#'
#'                  # this setting ensures the models are comparable for building a meta learner
#'                  seed = 2023, nfolds = 10,
#'                  keep_cross_validation_predictions = TRUE)
#'
#' #######################################################
#' ### PREPARE H2O Grid (takes a couple of minutes)
#' #######################################################
#' # make sure equal number of "nfolds" is specified for different grids
#' grid <- h2o.grid(algorithm = "gbm", y = y, training_frame = prostate,
#'                  hyper_params = list(ntrees = seq(1,50,1)),
#'                  grid_id = "ensemble_grid",
#'
#'                  # this setting ensures the models are comparable for building a meta learner
#'                  seed = 2023, fold_assignment = "Modulo", nfolds = 10,
#'                  keep_cross_validation_predictions = TRUE)
#'
#' #######################################################
#' ### PREPARE ENSEMBLE MODEL
#' #######################################################
#'
#' ### get the models' IDs from the AutoML and grid searches.
#' ### this is all that is needed before building the ensemble,
#' ### i.e., to specify the model IDs that should be evaluated.
#'
#' ids    <- c(h2o.get_ids(aml), h2o.get_ids(grid))
#' top    <- ensemble(models = ids, training_frame = prostate, strategy = "top")
#' search <- ensemble(models = ids, training_frame = prostate, strategy = "search")
#'
#' #######################################################
#' ### EVALUATE THE MODELS
#' #######################################################
#' h2o.auc(aml@leader)                          # best model identified by h2o.automl
#' h2o.auc(h2o.getModel(grid@model_ids[[1]]))   # best model identified by grid search
#' h2o.auc(top$model).                          # ensemble model with 'top' search strategy
#' h2o.auc(search$model).                       # ensemble model with 'search' search strategy
#'
#' }
#' @export
#' @author E. F. Haghish

hmda.autoEnsemble <- function(models,
                              training_frame,
                              newdata = NULL,
                              family = "binary",
                              strategy = c("search"),
                              model_selection_criteria = c("auc","aucpr","mcc","f2"),
                              min_improvement = 0.00001,
                              max = NULL,
                              top_rank = seq(0.01, 0.99, 0.01),
                              stop_rounds = 3,
                              reset_stop_rounds = TRUE,
                              stop_metric = "auc",
                              seed = -1,
                              verbatim = FALSE) {

  return(
    ensemble(
      models = models,
      training_frame = training_frame,
      newdata = newdata,
      family = family,
      strategy = strategy,
      model_selection_criteria = model_selection_criteria,
      min_improvement = min_improvement,
      max = max,
      top_rank = top_rank,
      stop_rounds = stop_rounds,
      reset_stop_rounds = reset_stop_rounds,
      stop_metric = stop_metric,
      seed = seed,
      verbatim = verbatim
    )
  )
}
