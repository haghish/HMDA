#' @title Fast Computation of Global WMSHAP Values
#' @description Computes global weighted mean SHAP (WMSHAP) values from a set
#'   of retained H2O models. For each model, SHAP contribution values are
#'   computed on a new dataset and summarized as mean absolute SHAP values.
#'   These values are normalized within each model to obtain model-specific
#'   SHAP importance ratios. The ratios are then combined using model weights
#'   derived from a selected performance metric.
#'
#' @param model_ids Character vector of H2O model IDs.
#' @param model_performance A named numeric vector of model performance values.
#'   The names must correspond to the model IDs. These values are used to compute
#'   the model weights.
#' @param newdata An H2OFrame containing the data on which SHAP contributions
#'   should be computed.
#' @param x A character vector of predictor names. Only these columns are used
#'   when computing and summarizing SHAP values.
#' @param performance_metric Character string specifying the metric used for
#'   weighting the retained models. Supported values are \code{"aucpr"},
#'   \code{"auc"}, \code{"logloss"}, and \code{"r2"}. The input is
#'   case-insensitive. For \code{"aucpr"}, \code{"auc"}, and \code{"r2"},
#'   higher values receive larger weights. For \code{"logloss"}, lower values
#'   receive larger weights.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{importance}{A data frame with one row per predictor. It contains
#'          the predictor name, the performance-weighted mean absolute SHAP
#'          value, the global WMSHAP ratio, and the WMSHAP rank.}
#'     \item{model_shap_table}{A long-format data frame containing the
#'          model-specific SHAP summaries before aggregation. It includes the
#'          model ID, performance metric, model weight, model performance,
#'          predictor name, mean absolute SHAP value, and within-model SHAP
#'          importance ratio.}
#'   }
#'
#' @details
#' Global WMSHAP is computed by first normalizing mean absolute SHAP values
#' within each retained model and then taking the performance-weighted mean
#' of these model-specific SHAP ratios.
#'
#' If fewer than two model IDs are supplied, the function returns missing
#' WMSHAP values. This is intentional because WMSHAP is defined here as a
#' multi-model summary.
#'
#' @importFrom h2o h2o.getModel h2o.predict_contributions
#' @importFrom stats setNames
#' @export

hmda.fastcompute.globalwmshap <- function(
    model_ids,
    model_performance,
    newdata,
    x,
    performance_metric = c("aucpr", "auc", "logloss", "r2")
    ) {

  performance_metric <- match.arg(
    tolower(performance_metric),
    choices = c("aucpr", "auc", "logloss", "r2")
  )

  model_ids <- unique(as.character(model_ids))

  model_performance <- as.numeric(
    model_performance[match(model_ids, names(model_performance))]
  )

  # WMSHAP is a multi-model summary. Therefore, the model set should be larger than 1
  if (length(model_ids) <= 1) {
    return(list(
      importance = data.frame(
        predictor = x,
        wmshap_abs_mean = NA_real_,
        wmshap_ratio = NA_real_,
        wmshap_rank = NA_real_,
        stringsAsFactors = FALSE
      ),
      model_shap_table = data.frame()
    ))
  }

  # Compute normalized model weights from the selected performance metric
  model_weights <- helper.compute.model.weights(
    model_performance = model_performance,
    performance_metric = performance_metric
  )

  # Define variables for weighted mean of the absolute SHAP values and within-model WMSHAP ratios
  wmshap_abs_mean <- setNames(rep(0, length(x)), x)
  wmshap_ratio <- setNames(rep(0, length(x)), x)
  model_shap_list <- vector("list", length(model_ids))

  for (m in seq_along(model_ids)) {
    model <- h2o::h2o.getModel(model_ids[m])

    # Compute local SHAP contributions for the evaluation dataset.
    shap_values <- h2o::h2o.predict_contributions(model, newdata[, x])
    shap_values <- as.data.frame(shap_values)

    # ensure x  are in the shap_values data frame
    feature_columns <- intersect(x, names(shap_values))

    # compute global SHAP: mean of absolute local SHAP contributions per feature
    shap_raw <- colMeans(abs(shap_values[, feature_columns, drop = FALSE]), na.rm = TRUE)

    # Keep all requested predictors in the same order, including predictors
    shap_raw_all <- setNames(rep(0, length(x)), x)
    shap_raw_all[names(shap_raw)] <- shap_raw

    # Mean absolute SHAP magnitude, weighted by model performance.
    wmshap_abs_mean <- wmshap_abs_mean + model_weights[m] * shap_raw_all

    # Normalize feature importance WITHIN the model before
    # applying the model-performance weight.
    model_total <- sum(shap_raw_all, na.rm = TRUE)

    if (model_total > 0) {
      shap_ratio_all <- shap_raw_all / model_total
    } else {
      shap_ratio_all <- setNames(rep(NA_real_, length(x)), x)
    }

    wmshap_ratio <- wmshap_ratio + model_weights[m] * shap_ratio_all

    model_shap_list[[m]] <- data.frame(
      model_id = model_ids[m],
      performance_metric = performance_metric,
      model_weight = model_weights[m],
      model_performance = model_performance[m],
      predictor = x,
      shap_abs_mean = as.numeric(shap_raw_all),
      shap_ratio = as.numeric(shap_ratio_all),
      stringsAsFactors = FALSE
    )
  }


  ratio_total <- sum(wmshap_ratio, na.rm = TRUE)

  if (!is.finite(ratio_total) || ratio_total <= 0) {
    wmshap_ratio[] <- NA_real_
  } else {
    wmshap_ratio <- wmshap_ratio / ratio_total
  }

  importance <- data.frame(
    predictor = x,
    wmshap_abs_mean = as.numeric(wmshap_abs_mean),
    wmshap_ratio = as.numeric(wmshap_ratio),
    stringsAsFactors = FALSE
  )

  importance$wmshap_rank <- rank(
    -importance$wmshap_ratio,
    ties.method = "min",
    na.last = "keep"
  )

  importance <- importance[order(importance$wmshap_rank), ]
  rownames(importance) <- NULL

  return(list(
    importance = importance,
    model_shap_table = do.call(rbind, model_shap_list)
  ))
}
