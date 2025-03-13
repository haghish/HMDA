#' @title Weighted average of SHAP values and weighted SHAP confidence intervals
#'        for a grid of fine-tuned models or base-learners of a stacked ensemble
#'        model
#' @description Weighted average of SHAP values and weighted SHAP confidence intervals
#'              provide a measure of feature importance for a grid of fine-tuned models
#'              or base-learners of a stacked ensemble model. Instead of reporting
#'              relative SHAP contributions for a single model, this function
#'              takes the variability in feature importance of multiple models
#'              into account and computes weighted mean and confidence intervals
#'              for each feature, taking the performance metric of each model as
#'              the weight. The function also provides a plot of the weighted
#'              SHAP values and confidence intervals. Currently only models
#'              trained by h2o machine learning software or autoEnsemble
#'              package are supported.
#' @param models H2O search grid, AutoML grid, or a character vector of H2O model IDs.
#'               the \code{"h2o.get_ids"} function from \code{"h2otools"} can retrieve
#'               the IDs from grids.
#' @param newdata h2o frame (data.frame). the data.frame must be already uploaded
#'                on h2o server (cloud). when specified, this dataset will be used
#'                for evaluating the models. if not specified, model performance
#'                on the training dataset will be reported.
#' @param performance_metric character, specifying the performance metric to be
#'                           used for weighting the SHAP values (mean and 95% CI). The default is
#'                           "r2" (R Squared).
#'                           For binary classification, other options include
#'                           "aucpr" (area under the precision-recall curve),
#'                           "auc" (area under the ROC curve),
#'                           and "f2" (F2 score).
#' @param standardize_performance_metric logical. if TRUE, performance_metric, which is
#'                                       used as weights vector is standardized such
#'                                       that the sum of the weights vector would be equal
#'                                       to the length of the vector. the default value
#'                                       is FALSE.
#' @param performance_type character, specifying where the performance metric should
#'                         be retrieved from. "train" means the performance of the
#'                         training process should be reported, "valid" indicates that
#'                         the performance of the validation process should be reported,
#'                         and "xval" means the cross-validation performance to be
#'                         retrieved.
#' @param minimum_performance the minimum performance metric for a recognizable model.
#'                            any model with performance equal or lower than this
#'                            argument will have weight of zero in computing the
#'                            weighted mean and CI SHAP values. the default value is
#'                            zero.
#' @param method character, specifying the method used for identifying the most
#'               important features according to their weighted SHAP values.
#'               The default selection method is "shapratio", a method that filters
#'               for features where the proportion of their relative weighted SHAP
#'               value exceeds the 'cutoff'. This approach calculates the relative
#'               contribution of each feature's weighted SHAP value against the
#'               aggregate of all features, with those surpassing the 'cutoff'
#'               being selected as top feature.
#'               Alternatively, the "mean" option can be specified, indicating
#'               any feature with normalized weighted mean SHAP contribution above
#'               the specified 'cutoff' should be selected. Another
#'               alternative options is "lowerCI", which includes
#'               features whose lower weighted confidence interval exceeds the
#'               predefined 'cutoff' value (default is relative SHAP of 1%).
#' @param cutoff numeric, specifying the cutoff for the method used for selecting
#'               the top features.
#' @param top_n_features integer. if specified, the top n features with the
#'                       highest weighted SHAP values will be selected, overrullung
#'                       the 'cutoff' and 'method' arguments. specifying top_n_feature
#'                       is also a way to reduce computation time, if many features
#'                       are present in the data set. The default is NULL, which means
#'                       the shap values will be computed for all features.
#' @param n_models minimum number of models that should meet the 'minimum_performance'
#'                 criterion in order to compute WMSHAP and CI. If the intention
#'                 is to compute global summary SHAP values (at feature level) for
#'                 a single model, set n_models to 1. The default is 10.
#' @param sample_size integer. number of rows in the \code{newdata} that should
#'                    be used for SHAP assessment. By default, all rows are used,
#'                    which is the recommended procedure for scientific analyses.
#'                    However, SHAP analysis is time consuming and in the process
#'                    of code development, lower values can be used for quicker
#'                    shapley analyses.
#' @param plot logical. if TRUE, the weighted mean and confidence intervals of
#'             the SHAP values are plotted. The default is TRUE.
# @param normalize_to character. The default value is "upperCI", which sets the feature with
#                     the maximum SHAP value to one, allowing the higher CI to
#                     go beyond one. Setting this value is mainly for aesthetic
#                     reason to adjust the Plot, but also, it can influence the
#                     feature selection process, depending on the method in use,
#                     because it changes how the SHAP values should be normalized.
#                     the alternative is 'feature', specifying that
#                     in the normalization of the SHAP values, the maximum confidence
#                     interval of the weighted SHAP values should be equal to
#                     "1", in order to limit the plot values to maximum of one.
#' @importFrom shapley shapley
#' @importFrom utils setTxtProgressBar txtProgressBar globalVariables
#' @importFrom stats weighted.mean
#' @importFrom h2o h2o.stackedEnsemble h2o.getModel h2o.auc h2o.aucpr h2o.r2
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
#'             h2o.shap_summary_plot
# @importFrom h2otools h2o.get_ids
#' @importFrom curl curl
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar coord_flip ggtitle xlab
#'             ylab theme_classic theme scale_y_continuous margin expansion
#' @author E. F. Haghish
#' @return a list including the GGPLOT2 object, the data frame of SHAP values,
#'         and performance metric of all models, as well as the model IDs.
#' @examples
#'
#' \dontrun{
#' # load the required libraries for building the base-learners and the ensemble models
#' library(h2o)            #shapley supports h2o models
#' library(shapley)
#'
#' # initiate the h2o server
#' h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)
#'
#' # upload data to h2o cloud
#' prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#' prostate <- h2o.importFile(path = prostate_path, header = TRUE)
#'
#' set.seed(10)
#'
#' ### H2O provides 2 types of grid search for tuning the models, which are
#' ### AutoML and Grid. Below, I demonstrate how weighted mean shapley values
#' ### can be computed for both types.
#'
#' #######################################################
#' ### PREPARE AutoML Grid (takes a couple of minutes)
#' #######################################################
#' # run AutoML to tune various models (GBM) for 60 seconds
#' y <- "CAPSULE"
#' prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
#' aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 120,
#'                  include_algos=c("GBM"),
#'
#'                  # this setting ensures the models are comparable for building a meta learner
#'                  seed = 2023, nfolds = 10,
#'                  keep_cross_validation_predictions = TRUE)
#'
#' ### call 'shapley' function to compute the weighted mean and weighted confidence intervals
#' ### of SHAP values across all trained models.
#' ### Note that the 'newdata' should be the testing dataset!
#' result <- shapley(models = aml, newdata = prostate, performance_metric = "aucpr", plot = TRUE)
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
#' result2 <- shapley(models = grid, newdata = prostate, performance_metric = "aucpr", plot = TRUE)
#'
#' #######################################################
#' ### PREPARE autoEnsemble STACKED ENSEMBLE MODEL
#' #######################################################
#'
#' ### get the models' IDs from the AutoML and grid searches.
#' ### this is all that is needed before building the ensemble,
#' ### i.e., to specify the model IDs that should be evaluated.
#' library(autoEnsemble)
#' ids    <- c(h2o.get_ids(aml), h2o.get_ids(grid))
#' autoSearch <- ensemble(models = ids, training_frame = prostate, strategy = "search")
#' result3 <- shapley(models = autoSearch, newdata = prostate,
#'                    performance_metric = "aucpr", plot = TRUE)
#'
#'
#' }
#' @export




hmda.wmshap <- function(models,
                        newdata,
                        #nboot = NULL,
                        plot = TRUE,
                        performance_metric = "r2",
                        standardize_performance_metric = FALSE,
                        performance_type = "xval",
                        minimum_performance = 0,
                        method = c("shapratio"),
                        cutoff = 0.01,
                        top_n_features = NULL,
                        n_models = 10,
                        sample_size = nrow(newdata)
                        #normalize_to = "upperCI"
) {
  return(
    shapley(models = models,
            newdata = newdata,
            #nboot = NULL,
            plot = plot,
            performance_metric = performance_metric,
            standardize_performance_metric = standardize_performance_metric,
            performance_type = performance_type,
            minimum_performance = minimum_performance,
            method = method,
            cutoff = cutoff,
            top_n_features = top_n_features,
            n_models = n_models,
            sample_size = nrow(newdata)
            #normalize_to = "upperCI"
            )
  )
}
