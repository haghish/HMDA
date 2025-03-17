#' @title compute and plot weighted mean SHAP contributions at group level (factors or domains)
#' @description This function applies different criteria to visualize SHAP contributions
#' @param shapley object of class 'shapley', as returned by the 'shapley' function
#' @param plot character, specifying the type of the plot, which can be either
#'            'bar', 'waffle', or 'shap'. The default is 'bar'.
#' @param method character, specifying the method used for identifying the most
#'               important features according to their weighted SHAP values.
#'               The default selection method is "AUTO", which selects a method
#'               based on number of models that have been evaluated because
#'               lowerCI method is not applicable to SHAP values of a single
#'               model. If 'lowerCI' is specified,
#'               features whose lower weighted confidence interval exceeds the
#'               predefined 'cutoff' value would be reported.
#'               Alternatively, the "mean" option can be specified, indicating
#'               any feature with normalized weighted mean SHAP contribution above
#'               the specified 'cutoff' should be selected. Another
#'               alternative options is "shapratio", a method that filters
#'               for features where the proportion of their relative weighted SHAP
#'               value exceeds the 'cutoff'. This approach calculates the relative
#'               contribution of each feature's weighted SHAP value against the
#'               aggregate of all features, with those surpassing the 'cutoff'
#'               being selected as top feature.
#' @param domains character list, specifying the domains for grouping the features'
#'                contributions. Domains are clusters of features' names, that
#'                can be used to compute WMSHAP at higher level, along with
#'                their 95% confidence interval. This computation can be used to
#'                better understand how a cluster of features influence the
#'                outcome. Note that either of 'features' or 'domains' arguments
#'                can be specified at the time.
#' @param legendstyle character, specifying the style of the plot legend, which
#'                    can be either 'continuous' (default) or 'discrete'. the
#'                    continuous legend is only applicable to 'shap' plots and
#'                    other plots only use 'discrete' legend.
#' @param scale_colour_gradient character vector for specifying the color gradients
#'                              for the plot.
#' @param print logical. if TRUE, the WMSHAP summary table for the given row is printed
#' @importFrom shapley shapley.domain
#' @importFrom stats na.omit aggregate formula
#' @importFrom h2o h2o.shap_summary_plot h2o.getModel
#' @importFrom ggplot2 scale_colour_gradient2 theme guides guide_legend guide_colourbar
#'             margin element_text theme_classic labs ylab xlab ggtitle
#' @author E. F. Haghish
#' @return ggplot object
#' @examples
#'
#' \dontrun{
#' library(HMDA)
#'   hmda.init()
#'   h2o.removeAll()
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
#'                           hyper_params = gbm_params1)
#'
#'   # Assess the performances of the models
#'   grid_performance <- hmda.grid.analysis(hmda_grid1)
#'
#'   # Return the best 2 models according to each metric
#'   hmda.best.models(grid_performance, n_models = 2)
#'
#'   # build an autoEnsemble model & test it with the testing dataset
#'   meta <- hmda.autoEnsemble(models = hmda_grid1, training_frame = train)
#'   print(h2o.performance(model = meta$model, newdata = test))
#'
#'   # compute weighted mean shap values
#'   wmshap <- hmda.wmshap(models = hmda_grid1,
#'                         newdata = test,
#'                         performance_metric = "aucpr",
#'                         standardize_performance_metric = FALSE,
#'                         performance_type = "xval",
#'                         minimum_performance = 0,
#'                         method = "shapratio",
#'                         cutoff = 0.01,
#'                         plot = TRUE)
#'
#'   # define domains to combine their WMSHAP values
#'   ...
#' }
#' @export
#' @author E. F. Haghish


hmda.domain <- function(shapley,
                        domains,
                        plot = "bar",
                        method = "AUTO",
                        legendstyle = "continuous",
                        scale_colour_gradient = NULL, #this is a BUG because it is not implemented
                        # COLORCODE IS MISSING :(
                        print = FALSE) {

  return(
    shapley.domain(shapley = shapley,
             domains = domains,
             plot = plot,
             method = method,
             legendstyle = legendstyle,
             scale_colour_gradient = scale_colour_gradient, #this is a BUG because it is not implemented
             # COLORCODE IS MISSING :(
             print = print)
  )
}
