
#' @importFrom h2o h2o.predict_contributions
#' @author E. F. Haghish

helper.metric.is.lower.better <- function(metric) {
  metric <- tolower(metric)
  metric %in% c("logloss", "mae", "mse", "rmse", "rmsle", "mean_per_class_error")
}

helper.get.metric.column <- function(grid_analysis, metric) {
  metric <- tolower(metric)
  metric_column <- names(grid_analysis)[tolower(names(grid_analysis)) == metric][1]

  if (is.na(metric_column)) {
    stop(paste0("Could not find ", metric, " column in grid_analysis."))
  }

  return(metric_column)
}

helper.compute.model.weights <- function(model_performance,
                                  performance_metric = c("aucpr", "auc", "logloss", "r2")) {

  performance_metric <- match.arg(tolower(performance_metric),
                                  choices = c("aucpr", "auc", "logloss", "r2"))

  raw_performance <- as.numeric(model_performance)
  n_models <- length(raw_performance)

  if (n_models == 0) {
    return(numeric(0))
  }

  # If performance values are unavailable, fall back to equal weights.
  if (all(is.na(raw_performance))) {
    warning("All model performance values are NA. Using equal weights for all models!")
    return(rep(1 / n_models, n_models))
  }

  # For logloss, smaller values indicate better performance, so these values should be transformed.
  if (helper.metric.is.lower.better(performance_metric)) {
    weight_score <- 1 / pmax(raw_performance, .Machine$double.eps)
  } else {
    # For auc, aucpr, and r2, larger values indicate better performance.
    weight_score <- raw_performance

    # R2 can be negative in some settings. If this happens, it can be shifted so that
    # weights remain non-negative while preserving the model ordering.
    if (performance_metric == "r2" && any(weight_score < 0, na.rm = TRUE)) {
      weight_score <- weight_score - min(weight_score, na.rm = TRUE)
    }

    weight_score <- pmax(weight_score, 0)
  }

  weight_score[is.na(weight_score)] <- 0

  if (sum(weight_score, na.rm = TRUE) <= 0) {
    model_weights <- rep(1 / n_models, n_models)
  } else {
    model_weights <- weight_score / sum(weight_score, na.rm = TRUE)
  }

  return(model_weights)
}

helper.get.model.id.column <- function(grid_analysis) {
  if ("model_ids" %in% names(grid_analysis)) return("model_ids")
  if ("model_id" %in% names(grid_analysis)) return("model_id")
  stop("Could not identify the model-id column in grid_analysis.")
}

helper.order.models.by.metric <- function(metric_values, metric) {
  metric <- tolower(metric)
  if (helper.metric.is.lower.better(metric)) {
    order(metric_values, decreasing = FALSE, na.last = NA)
  } else {
    order(metric_values, decreasing = TRUE, na.last = NA)
  }
}

helper.value.or.default <- function(value, default) {
  if (is.null(value)) return(default)
  return(value)
}

helper.clean.h2o.memory <- function(analysis_config, # analysis.configuration
                             verbose = FALSE) {

  remove_reps <- helper.value.or.default(analysis_config$h2o_remove_all_repetitions, 2)
  r_gc_reps <- helper.value.or.default(analysis_config$r_gc_repetitions, 2)
  h2o_gc_reps <- helper.value.or.default(analysis_config$h2o_jvm_gc_repetitions, 3)
  timeout_secs <- helper.value.or.default(analysis_config$h2o_cleanup_timeout_secs, 120)

  # First remove all frames, models, grids, predictions, and other objects from
  # the H2O distributed key-value store. Repeating this is intentional: in long
  # loops, some temporary objects can appear after delayed computations finish.
  for (i in seq_len(remove_reps)) {
    try(h2o::h2o.removeAll(timeout_secs = timeout_secs), silent = TRUE)
  }

  # Remove ordinary R references that are no longer reachable and trigger R-side
  # garbage collection. This does not remove H2O objects by itself, but it helps
  # clear local wrappers and temporary R data frames.
  for (i in seq_len(r_gc_reps)) {
    invisible(gc(verbose = FALSE))
  }

  # Request JVM garbage collection on the H2O backend. This is an internal H2O
  # helper, so it is wrapped in try(). If the helper is unavailable in a future
  # H2O version, the simulation will continue after h2o.removeAll() and gc().
  h2o_gc_fun <- try(get(".h2o.garbageCollect",
                        envir = asNamespace("h2o"),
                        inherits = FALSE),
                    silent = TRUE)

  if (!inherits(h2o_gc_fun, "try-error") && is.function(h2o_gc_fun)) {
    for (i in seq_len(h2o_gc_reps)) {
      try(h2o_gc_fun(), silent = TRUE)
    }
  }

  if (verbose) {
    remaining <- try(nrow(h2o::h2o.ls()), silent = TRUE)
    if (!inherits(remaining, "try-error")) {
      cat("H2O cleanup complete. Remaining H2O keys:", remaining, "\n")
    }
  }

  invisible(TRUE)
}


