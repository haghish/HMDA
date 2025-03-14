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
#' @importFrom waffle waffle
#' @importFrom h2o h2o.shap_summary_plot h2o.getModel
#' @importFrom ggplot2 scale_colour_gradient2 theme guides guide_legend guide_colourbar
#'             margin element_text theme_classic labs ylab xlab ggtitle
#' @author E. F. Haghish
#' @return ggplot object
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
#' ### H2O provides 2 types of grid search for tuning the models, which are
#' ### AutoML and Grid. Below, I demonstrate how weighted mean shapley values
#' ### can be computed for both types.
#'
#' set.seed(10)
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
#' result <- shapley(models = aml, newdata = prostate, plot = TRUE)
#'
#' #######################################################
#' ### PLOT THE WEIGHTED MEAN SHAP VALUES
#' #######################################################
#'
#' shapley.plot(result, plot = "bar")
#' shapley.plot(result, plot = "waffle")
#' }
#' @export


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
