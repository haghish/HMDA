#' @title Fast Computation of Global SHAP Values
#' @description Computes global SHAP importance from an H2O model on a new dataset
#'
#' @param model An H2O model object.
#' @param newdata An H2OFrame dataframe
#' @param x A character vector of predictor names. Only these columns are used
#'              when computing and summarizing SHAP values.
#'
#' @return A data frame with one row per predictor and the following columns:
#'   \describe{
#'     \item{predictor}{The predictor name.}
#'     \item{single_shap_abs_mean}{The mean absolute SHAP value of the predictor
#'          across observations in \code{newdata}.}
#'     \item{single_shap_ratio}{The normalized SHAP importance ratio. The values
#'          are scaled to sum to one across all predictors.}
#'     \item{single_shap_rank}{The rank of the predictor based on
#'          \code{single_shap_ratio}. Rank 1 indicates the largest global SHAP
#'          importance.}
#'   }
#'
#' @importFrom h2o h2o.predict_contributions
#' @importFrom stats setNames
#' @export

hmda.fastcompute.globalshap <- function(model, newdata, x) {
  shap_values <- h2o::h2o.predict_contributions(model, newdata[, x])
  shap_values <- as.data.frame(shap_values)

  feature_columns <- intersect(x, names(shap_values))
  shap_raw <- colMeans(abs(shap_values[, feature_columns, drop = FALSE]),
                       na.rm = TRUE)

  shap_raw_all <- setNames(rep(0, length(x)), x)
  shap_raw_all[names(shap_raw)] <- shap_raw
  shap_ratio <- shap_raw_all / sum(shap_raw_all)

  out <- data.frame(
    predictor = x,
    single_shap_abs_mean = as.numeric(shap_raw_all),
    single_shap_ratio = as.numeric(shap_ratio),
    stringsAsFactors = FALSE
  )

  out$single_shap_rank <- rank(-out$single_shap_ratio, ties.method = "min")
  out <- out[order(out$single_shap_rank), ]
  rownames(out) <- NULL

  return(out)
}
