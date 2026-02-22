#' @title WMSHAP row-level plot for a single observation (participant or data row)
#' @description
#' Computes and visualizes Weighted Mean SHAP contributions (WMSHAP) for a single row
#' (subject/observation) across multiple models in a \code{shapley} object.
#' For each feature, the function computes a weighted mean of row-level SHAP contributions
#' across models using \code{shapley$weights} and reports an approximate 95% confidence
#' interval summarizing variability across models.
#' @param wmshap object of class 'shapley', as returned by the 'shapley' function
#'               or hmda.wmshap function
#' @param row_index Integer (length 1). The row/subject identifier to visualize. This is
#'                  matched against the \code{index} column in \code{shapley$results}.
#' @param top_n_features Integer. If specified, the top n features with the
#'                       highest weighted SHAP values will be selected. This
#'                       will be overrulled by the 'features' argument.
#' @param features Optional character vector of feature names to plot. If \code{NULL},
#'                 all available features in \code{shapley$results} are used.
#'                 Specifying the \code{features} argument will override the
#'                 \code{top_n_features} argument.
#' @param nonzeroCI Logical. If \code{TRUE}, it avoids ploting features that have
#'                  a confidence interval crossing zero.
#' @param plot Logical. If \code{TRUE}, prints the plot.
#' @param print Logical. If \code{TRUE}, prints the computed summary table for the row.
#' @return a list including the GGPLOT2 object and the data frame of WMSHAP summary values.
#'
#' @importFrom shapley shapley.row.plot
#' @author E. F. Haghish
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
#' ### PLOT THE WEIGHTED MEAN SHAP VALUES FOR A PARTICULAR CASE
#' #######################################################
#' hmda.row.plot(wmshap, row_index = 13)
#' }
#' @export

hmda.row.plot <- function(wmshap,
                          row_index,
                          top_n_features = NULL,
                          features = NULL,
                          nonzeroCI = FALSE,
                          plot = TRUE,
                          print = FALSE) {

  shapley.row.plot(wmshap,
                   row_index = row_index,
                   top_n_features = top_n_features,
                   features = features,
                   nonzeroCI = nonzeroCI,
                   plot = plot,
                   print = print)
}


