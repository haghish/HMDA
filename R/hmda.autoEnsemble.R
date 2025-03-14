#' @title Builds Stacked Ensemble Model from H2O Models
#' @description Multiple trained H2O models are stacked to create an ensemble
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.stackedEnsemble h2o.getModel h2o.auc h2o.aucpr h2o.mcc
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
# @importFrom h2otools h2o.get_ids
#' @importFrom curl curl
#' @importFrom autoEnsemble ensemble
#' @param models H2O search grid or AutoML grid or a character vector of H2O model IDs.
#'               the \code{"h2o.get_ids"} function from \code{"h2otools"} can
#'               retrieve the IDs from grids.
#' @param training_frame h2o training frame (data.frame) for model training
#' @param newdata h2o frame (data.frame). the data.frame must be already uploaded
#'                on h2o server (cloud). when specified, this dataset will be used
#'                for evaluating the models. if not specified, model performance
#'                on the training dataset will be reported.
#' @param family model family. currently only \code{"binary"} classification models
#'               are supported.
#' @param strategy character. the current available strategies are \code{"search"}
#'                 (default) and \code{"top"}. The \code{"search"} strategy searches
#'                 for the best combination of top-performing diverse models
#'                 whereas the \code{"top"} strategy is more simplified and just
#'                 combines the specified of top-performing diverse models without
#'                 examining the possibility of improving the model by searching for
#'                 larger number of models that can further improve the model. generally,
#'                 the \code{"search"} strategy is preferable, unless the computation
#'                 runtime is too large and optimization is not possible.
#' @param max integer. specifies maximum number of models for each criteria to be extracted. the
#'            default value is the \code{"top_rank"} percentage for each model selection
#'            criteria.
#' @param model_selection_criteria character, specifying the performance metrics that
#'        should be taken into consideration for model selection. the default are
#'        \code{"c('auc', 'aucpr', 'mcc', 'f2')"}. other possible criteria are
#'        \code{"'f1point5', 'f3', 'f4', 'f5', 'kappa', 'mean_per_class_error', 'gini', 'accuracy'"},
#'        which are also provided by the \code{"evaluate"} function.
#' @param min_improvement numeric. specifies the minimum improvement in model
#'                        evaluation metric to qualify further optimization search.
#' @param top_rank numeric vector. specifies percentage of the top models taht
#'                 should be selected. if the strategy is \code{"search"}, the
#'                 algorithm searches for the best best combination of the models
#'                 from top ranked models to the bottom. however, if the strategy
#'                 is \code{"top"}, only the first value of the vector is used
#'                 (default value is top 1\%).
#' @param stop_rounds integer. number of stoping rounds, in case the model stops
#'                    improving
#' @param reset_stop_rounds logical. if TRUE, every time the model improves the
#'                          stopping rounds penalty is resets to 0.
#' @param stop_metric character. model stopping metric. the default is \code{"auc"},
#'                    but \code{"aucpr"} and \code{"mcc"} are also available.
#' @param seed random seed (recommended)
#' @param verbatim logical. if TRUE, it reports additional information about the
#'                 progress of the model training, particularly used for debugging.
#' @return a list including the ensemble model and the top-rank models that were
#'         used in the model
#' @author E. F. Haghish
#'
#' @examples
#'
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



hmda.autoEnsemble <- function(
    models,
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
    verbatim = FALSE
    #loaded = TRUE,
    #path = NULL
    ) {

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
      #loaded = TRUE,
      #path = NULL)
}
