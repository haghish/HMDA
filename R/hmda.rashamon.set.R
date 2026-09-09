#' @title Select a Near-Optimal from a Model Grid
#' @description Selects a set of near-optimal models from a grid-analysis table.
#'   The function can either retain all models within a given percentage of the
#'   best model, or retain the top \code{n} models according to a selected
#'   performance metric.
#'
#' @param grid_analysis A data frame containing model IDs and performance
#'   metrics, usually returned by \code{hmda.grid.analysis()}.
#' @param metric Character string specifying the metric used for model
#'   selection and weighting. Supported values include \code{"aucpr"},
#'   \code{"auc"}, \code{"logloss"}, and \code{"r2"}.
#' @param criterion Character string specifying the selection rule. Supported
#'   values are \code{"within_percent"} and \code{"top_n"}.
#' @param distance_percentage Numeric. The allowed distance from the best model
#'   when \code{criterion = "within_percent"}. For higher-is-better metrics,
#'   models are retained if their performance is at least
#'   \code{best * (1 - distance_percentage)}. For lower-is-better metrics, models
#'   are retained if their value is at most
#'   \code{best * (1 + distance_percentage)}. Default is \code{0.01}.
#' @param top_n Integer. Number of top-ranked models to retain when
#'   \code{criterion = "top_n"}. Default is \code{5}.
#' @param fallback_min_models Integer. Minimum number of models to retain. If the
#'   selected model based on within_perent set contains fewer models than this value, the function
#'   falls back to the best \code{fallback_min_models} models according to
#'   \code{metric}. Default is \code{2}.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{model_ids}{Character vector of the final selected model IDs. This
#'          includes the fallback models if the fallback rule was used.}
#'     \item{pure_model_ids}{Character vector of model IDs selected by the
#'          requested rule before applying the minimum-size fallback.}
#'     \item{selected_model_table}{A data frame containing the selected model
#'          IDs, the selection metric, the metric value, and the normalized model
#'          weight for each retained model.}
#'     \item{all_model_performance}{A named numeric vector containing the metric
#'          value for all valid models in \code{grid_analysis}. The names are
#'          the model IDs.}
#'     \item{diagnostics}{A one-row data frame describing the selection rule,
#'          the number of models retained before and after fallback, whether the
#'          fallback was used, the best and second-best metric values, the
#'          top-two metric gap, and the cutoff used for selection.}
#'   }
#'
#' @details
#' The function is used to define the model set from which WMSHAP values are
#' computed. In the \code{"within_percent"} criterion, the function retains
#' models that are close to the best-performing model according to the selected
#' metric.
#'
#' For metrics where larger values indicate better performance, such as
#' \code{"aucpr"}, \code{"auc"}, and \code{"r2"}, models are ranked in
#' decreasing order. For metrics where smaller values indicate better
#' performance, such as \code{"logloss"}, models are ranked in increasing order.
#'
#' The argument \code{fallback_min_models} is useful when the
#' \code{"within_percent"} criterion retains only a single model. In that case,
#' WMSHAP would no longer be meaningfully multi-model. The fallback rule can be
#' used to force a minimum number of retained models while recording whether
#' this correction was necessary.
#'
#' @examples
#' \dontrun{
#'   # Assume grid_analysis was returned by hmda.grid.analysis()
#'
#'   # Retain all models within 1 percent of the best AUCPR model.
#'   rashomon_1pct <- hmda.rashomon.set(
#'     grid_analysis = grid_analysis,
#'     metric = "aucpr",
#'     criterion = "within_percent",
#'     distance_percentage = 0.01,
#'     fallback_min_models = 2
#'   )
#'
#'   # Access the final retained model IDs.
#'   rashomon_1pct$model_ids
#'
#'   # Inspect whether the fallback rule was used.
#'   rashomon_1pct$diagnostics
#'
#'   # Retain the top 5 models according to AUCPR.
#'   rashomon_top5 <- hmda.rashomon.set(
#'     grid_analysis = grid_analysis,
#'     metric = "aucpr",
#'     criterion = "top_n",
#'     top_n = 5
#'   )
#'
#'   rashomon_top5$selected_model_table
#' }
#' @export

hmda.rashomon.set <- function(grid_analysis,
                             metric = "aucpr",
                             criterion = c("within_percent", "top_n"),
                             distance_percentage = 0.01,
                             top_n = 5,
                             fallback_min_models = 2) {

  metric <- tolower(metric)
  criterion <- match.arg(criterion)

  model_id_column <- helper.get.model.id.column(grid_analysis)
  metric_column <- helper.get.metric.column(grid_analysis, metric)

  model_ids <- as.character(grid_analysis[[model_id_column]])
  metric_values <- as.numeric(grid_analysis[[metric_column]])
  names(metric_values) <- model_ids

  valid <- !is.na(metric_values) & !is.na(model_ids) & nzchar(model_ids)
  model_ids <- model_ids[valid]
  metric_values <- metric_values[valid]

  if (length(model_ids) == 0) {
    stop("No valid models were available for model selection.")
  }

  ranked_index <- helper.order.models.by.metric(metric_values, metric)
  ranked_ids <- model_ids[ranked_index]
  ranked_values <- metric_values[ranked_index]

  best_value <- ranked_values[1]
  second_value <- if (length(ranked_values) >= 2) ranked_values[2] else NA_real_

  if (helper.metric.is.lower.better(metric)) {
    top2_gap <- second_value - best_value
  } else {
    top2_gap <- best_value - second_value
  }

  if (criterion == "within_percent") {

    if (helper.metric.is.lower.better(metric)) {
      cutoff <- if (abs(best_value) > .Machine$double.eps) {
        best_value * (1 + distance_percentage)
      } else {
        best_value + distance_percentage
      }
      pure_ids <- model_ids[metric_values <= cutoff]
    } else {
      cutoff <- if (abs(best_value) > .Machine$double.eps) {
        best_value * (1 - distance_percentage)
      } else {
        best_value - distance_percentage
      }
      pure_ids <- model_ids[metric_values >= cutoff]
    }

  } else {
    cutoff <- NA_real_
    n_take <- min(top_n, length(ranked_ids))
    pure_ids <- ranked_ids[seq_len(n_take)]
  }

  pure_ids <- unique(as.character(pure_ids))
  pure_retained_models <- length(pure_ids)

  final_ids <- pure_ids
  fallback_used <- FALSE

  if (length(final_ids) < fallback_min_models && length(ranked_ids) >= fallback_min_models) {
    final_ids <- unique(as.character(ranked_ids[seq_len(fallback_min_models)]))
    fallback_used <- TRUE
  }

  selected_values <- metric_values[final_ids]
  selected_weights <- helper.compute.model.weights(
    model_performance = selected_values,
    performance_metric = metric
  )

  selected_model_table <- data.frame(
    model_id = final_ids,
    metric = metric,
    metric_value = as.numeric(selected_values),
    model_weight = as.numeric(selected_weights),
    stringsAsFactors = FALSE
  )

  out <- list(
    model_ids = final_ids,
    pure_model_ids = pure_ids,
    selected_model_table = selected_model_table,
    all_model_performance = metric_values,
    diagnostics = data.frame(
      metric = metric,
      criterion = criterion,
      distance_percentage = ifelse(criterion == "within_percent", distance_percentage, NA_real_),
      top_n = ifelse(criterion == "top_n", top_n, NA_integer_),
      fallback_min_models = fallback_min_models,
      pure_retained_models = pure_retained_models,
      final_retained_models = length(final_ids),
      fallback_used = fallback_used,
      best_metric_value = best_value,
      second_metric_value = second_value,
      top2_metric_gap = top2_gap,
      cutoff = cutoff,
      stringsAsFactors = FALSE
    )
  )

  return(out)
}

