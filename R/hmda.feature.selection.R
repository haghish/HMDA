#' @title Feature Selection Based on Weighted SHAP Values
#' @description This function selects "important", "inessential", and "irrelevant"
#'   features based on a summary of weighted mean SHAP values obtained from a prior
#'   analysis. It uses the SHAP summary table (found in the \code{wmshap} object)
#'   to identify features that are deemed important according to a specified method
#'   and cutoff. Features with a lower confidence interval (lowerCI) below zero
#'   are labeled as "irrelevant", while the remaining features are classified as
#'   "inessential" if they do not meet the importance criteria.
#'
#' @param wmshap A list object (typically returned by a weighted SHAP analysis)
#'   that must contain a data frame \code{summaryShaps} with at least the columns
#'   \code{"feature"}, \code{"mean"}, and \code{"lowerCI"}. It may also contain
#'   additional columns for alternative selection methods.
#' @param method Character. Specifies the method to use for feature selection.
#'   The default is \code{"shapratio"}, which selects features based on whether
#'   their relative weighted SHAP value exceeds the cutoff. Other methods may be
#'   implemented in the future. Default is \code{"shapratio"}.
#' @param cutoff Numeric. The threshold cutoff for the selection method. Features
#'   with a weighted SHAP value (or ratio) greater than or equal to this value
#'   are considered important. Default is \code{0.01}.
#' @param top_n_features Integer. If specified, the function selects the top
#'   \code{top_n_features} features (based on the sorted SHAP mean values),
#'   overriding the cutoff and method arguments. If \code{NULL}, all features that
#'   meet the cutoff criteria are used. Default is \code{NULL}.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{important}{A character vector of features deemed important.}
#'     \item{inessential}{A character vector of features considered inessential
#'           (present in the data but not meeting the importance criteria).}
#'     \item{irrelevant}{A character vector of features deemed irrelevant,
#'           defined as those with a lower confidence interval (lowerCI) below zero.}
#'   }
#' @details
#'   The function performs the following steps:
#'   \enumerate{
#'     \item Retrieves the SHAP summary table from the \code{wmshap} object.
#'     \item Sorts the summary table in descending order based on the \code{mean}
#'           SHAP value.
#'     \item Identifies all features available in the summary.
#'     \item Classifies features as \strong{irrelevant} if their \code{lowerCI}
#'           value is below zero.
#'     \item If \code{top_n_features} is not specified, selects \strong{important}
#'           features as those whose value for the specified \code{method} column
#'           meets or exceeds the \code{cutoff}; the remaining features (excluding
#'           those marked as irrelevant) are classified as \strong{inessential}.
#'     \item If \code{top_n_features} is provided, the function selects the top
#'           \code{n} features (based on the sorted order) as important, with the
#'           rest (excluding irrelevant ones) being inessential.
#'   }
#'
#' @examples
#' \dontrun{
#'   # Example: Create a hyperparameter grid for GBM models.
#'   predictors <- c("var1", "var2", "var3")
#'   response <- "target"
#'
#'   # Define hyperparameter ranges
#'   hyper_params <- list(
#'     ntrees = seq(50, 150, by = 25),
#'     max_depth = c(5, 10, 15),
#'     learn_rate = c(0.01, 0.05, 0.1),
#'     sample_rate = c(0.8, 1.0),
#'     col_sample_rate = c(0.8, 1.0)
#'   )
#'
#'   # Run the grid search
#'   grid <- hmda.grid(
#'     algorithm = "gbm",
#'     x = predictors,
#'     y = response,
#'     training_frame = h2o.getFrame("hmda.train.hex"),
#'     hyper_params = hyper_params,
#'     nfolds = 10,
#'     stopping_metric = "AUTO"
#'   )
#'
#'   # Print the grid search results
#'   print(grid)
#' }
#'
#' @export
#' @author E. F. Haghish

hmda.feature.selection <- function(wmshap,
                          method = c("shapratio"),
                          cutoff = 0.01,
                          top_n_features = NULL) {
  # Exclude features that do not meet the criteria
  # ====================================================
  summaryShaps <- wmshap$summaryShaps

  # Sort the results
  summaryShaps <- summaryShaps[order(summaryShaps$mean, decreasing = TRUE), ]

  # get a list of all features with wmshap values
  all_features <- as.vector(summaryShaps[ , "feature"])

  # define the removable items, where lowerCI is bellow zero
  irrelevant  <- as.vector(summaryShaps[summaryShaps[, "lowerCI"] < 0, "feature"])

  # select the features
  if (is.null(top_n_features)) {
    important <- as.vector(summaryShaps[summaryShaps[,method] >= cutoff, "feature"])
    inessential <- all_features[!all_features %in% c(important, irrelevant)]
  }
  else {
    important <- as.vector(summaryShaps[1:top_n_features, "feature"])
    inessential <- all_features[!all_features %in% c(important, irrelevant)]
  }

  results <- list(
    important = important,
    inessential = inessential,
    irrelevant= irrelevant
  )

  return(results)
}
