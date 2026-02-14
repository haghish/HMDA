#' @title Plot WMSHAP contributions
#' @description This function applies different criteria to visualize WMSHAP contributions
#' @param wmshap object of class 'shapley', as returned by the 'shapley' function
#'               or hmda.wmshap function
#' @param plot Character. Plot type passed to \code{shapley.plot()}. Common options are
#'   \code{"bar"} and \code{"wmshap"}.
#' @param method Character. The column name in \code{summaryShaps} used
#'                           for feature selection. Default is \code{"mean"}, which
#'                           selects important features which have weighted mean shap
#'                           ratio (WMSHAP) higher than the specified cutoff. Other
#'                           alternative is "lowerCI", which selects features which
#'                           their lower bound of confidence interval is higher than
#'                           the cutoff.
#' @param cutoff numeric, specifying the cutoff for the method used for selecting
#'               the top features.
#' @param top_n_features Integer. If specified, the top n features with the
#'                       highest weighted SHAP values will be selected, overrullung
#'                       the 'cutoff' and 'method' arguments.
#' @param features character vector, specifying the feature to be plotted.
#' @param legendstyle character, specifying the style of the plot legend, which
#'                    can be either 'continuous' (default) or 'discrete'. the
#'                    continuous legend is only applicable to 'wmshap' plots and
#'                    other plots only use 'discrete' legend.
#' @param scale_colour_gradient character vector for specifying the color gradients
#'                              for the plot.
#' @param labels Optional named character vector mapping feature names to display labels, e.g.,
#'   \code{c(feature1 = "Label 1", feature2 = "Label 2")}.
#'
#' @importFrom shapley shapley.plot
#' @author E. F. Haghish
#' @return ggplot object
#' @examples
#'
#' \dontrun{
#'   library(HMDA)
#'   library(h2o)
#'   hmda.init()
#'
#'   # Import a sample binary outcome dataset into H2O
#'   train <- h2o.importFile(
#'   "https://s3.amazonaws.com/h2o-public-test-data/smalldata/higgs/higgs_train_10k.csv")
#'   test <- h2o.importFile(
#'   "https://s3.amazonaws.com/h2o-public-test-data/smalldata/higgs/higgs_test_5k.csv")
#'
#'   # Identify predictors and response
#'   y <- "response"
#'   x <- setdiff(names(train), y)
#'
#'   # For binary classification, response should be a factor
#'   train[, y] <- as.factor(train[, y])
#'   test[, y] <- as.factor(test[, y])
#'
#'   params <- list(learn_rate = c(0.01, 0.1),
#'                  max_depth = c(3, 5, 9),
#'                  sample_rate = c(0.8, 1.0)
#'   )
#'
#'   # Train and validate a cartesian grid of GBMs
#'   hmda_grid1 <- hmda.grid(algorithm = "gbm", x = x, y = y,
#'                           grid_id = "hmda_grid1",
#'                           training_frame = train,
#'                           nfolds = 10,
#'                           ntrees = 100,
#'                           seed = 1,
#'                           hyper_params = params)
#'
#'   # compute weighted mean shap values
#'   wmshap <- hmda.wmshap(models = hmda_grid1,
#'                         newdata = test,
#'                         performance_metric = "aucpr",
#'                         standardize_performance_metric = FALSE,
#'                         performance_type = "xval",
#'                         minimum_performance = 0,
#'                         method = "mean",
#'                         cutoff = 0.01,
#'                         plot = TRUE)
#'
#' #######################################################
#' ### PLOT THE WEIGHTED MEAN SHAP VALUES
#' #######################################################
#' hmda.plot(wmshap, plot = "bar")
#' }
#' @export

hmda.plot <- function(wmshap,
                         plot = "bar",
                         method = "mean",
                         cutoff = 0.01,
                         top_n_features = NULL,
                         features = NULL,
                         legendstyle = "continuous",
                         scale_colour_gradient = NULL,
                         labels = NULL) {

  shapley.plot(wmshap,
           plot = plot,
           method = method,
           cutoff = cutoff,
           top_n_features = top_n_features,
           features = features,
           legendstyle = legendstyle,
           scale_colour_gradient = scale_colour_gradient,
           labels = labels)
}


