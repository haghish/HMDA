#' @title Select Best Models Across All Models in HMDA Grid
#' @description
#' Scans an HMDA grid analysis data frame for performance metric columns and, for each metric,
#' selects the best-performing models according to the correct optimization direction
#' (lower is better for some metrics; higher is better for others). The function returns a
#' subset of the input data frame containing the union of selected model IDs.
#'
#' @param df         A data frame of class \code{"hmda.grid.analysis"} containing
#'                   model performance results. It must include a column named
#'                   \code{model_ids}.
#' @param n_models   Integer. The number of top models to select per metric.
#'                   If both \code{n_models} and \code{distance_percentage} are \code{NULL},
#'                   defaults to 1.
#' @param metrics Character vector of performance metric column names to consider. Only metrics present
#'   in \code{df} are used.
#' @param distance_percentage Numeric in (0, 1). Alternative to \code{n_models}. Selects all models within
#'   a given percentage of the best value for each metric (direction-aware). You must specify either
#'   \code{n_models} or \code{distance_percentage}, not both.
#' @param hyperparam Logical. If \code{TRUE}, returns all columns for the selected models (including
#'   hyperparameters). If \code{FALSE}, returns only \code{model_ids} plus the selected metric columns.
#'
#'
#' @return A data frame containing the union of selected models across all considered metrics.
#'   If \code{hyperparam = FALSE}, the output includes \code{model_ids} and the metric columns found in \code{df}.
#'   If \code{hyperparam = TRUE}, the output includes all columns from \code{df} for the selected models.
#'
#' @details
#'   The function uses a predefined set of H2O performance metrics along with
#'   their desired optimization directions:
#'   \describe{
#'     \item{logloss, mae, mse, rmse, rmsle, mean_per_class_error}{Lower values
#'           are better.}
#'     \item{auc, aucpr, r2, accuracy, f1, mcc, f2}{Higher values are better.}
#'   }
#'
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
#'   # Assess the performances of the models
#'   grid_performance <- hmda.grid.analysis(grid)
#'
#'   # Return the best 2 models according to each metric
#'   hmda.best.models(grid_performance, n_models = 2)
#'
#'   # return all models with performance metric as high as 98% of the best model, for each metric
#'   hmda.best.models(grid_performance, distance_percentage = 0.98)
#' }
#'
#' @importFrom utils head
#' @export
#' @author E. F. Haghish
hmda.best.models <- function(df,
                             n_models = NULL,
                             distance_percentage = NULL,
                             metrics = c("logloss", "mae", "mse", "rmse", "rmsle",
                                         "mean_per_class_error", "auc", "aucpr",
                                         "r2", "accuracy", "f1", "mcc", "f2"),
                             hyperparam = FALSE) {

  # Syntax check
  # ============================================================
  if (!inherits(df, "hmda.grid.analysis")) {
    warning("'df' is not of class 'hmda.grid.analysis'... watch out!")
  }

  if (!is.null(n_models) & !is.null(distance_percentage)) {
    stop("either specify 'n_models' or 'distance_percentage'")
  }
  else if (is.null(n_models) & is.null(distance_percentage)) {
    n_models <- 1
  }

  if (!"model_ids" %in% names(df)) {
    stop("The data frame must contain a 'model_ids' column.")
  }

  sort_direction_map <- c()

  # Initialize a vector to hold best model IDs across metrics
  best_model_ids <- c()

  # Define known performance metrics and their optimization directions
  directions <- c(
    logloss = "minimize",
    mae = "minimize",
    mse = "minimize",
    rmse = "minimize",
    rmsle = "minimize",
    mean_per_class_error = "minimize",
    auc = "maximize",
    aucpr = "maximize",
    r2 = "maximize",
    accuracy = "maximize",
    f1 = "maximize",
    mcc = "maximize",
    f2 = "maximize"
  )

  # sort_direction <- c(
  #   logloss = FALSE,
  #   mae = FALSE,
  #   mse = FALSE,
  #   rmse = FALSE,
  #   rmsle = FALSE,
  #   mean_per_class_error = FALSE,
  #   auc = TRUE,
  #   aucpr = TRUE,
  #   r2 = TRUE,
  #   accuracy = TRUE,
  #   f1 = TRUE,
  #   mcc = TRUE,
  #   f2 = TRUE
  # )

  #directions <- directions[directions %in% metrics]

  # Loop over each known metric and determine the top n_models best model IDs
  for (met in metrics) {
    if (met %in% names(df)) {
      vals <- df[[met]]
      if (all(is.na(vals))) next  # Skip metric if all values are NA

      dir <- directions[[met]]
      if (is.null(dir)) dir <- "minimize"

      # Order indices according to performance (top n_models)
      if (dir == "maximize") {
        ordered_idx <- order(vals, decreasing = TRUE)
        sort_direction_map <- c(sort_direction_map, TRUE)
      } else {
        ordered_idx <- order(vals, decreasing = FALSE)
        sort_direction_map <- c(sort_direction_map, FALSE)
      }

      # Select the models for each metric
      if (!is.null(n_models)) {
        top_idx <- head(ordered_idx, n_models)
        best_model_ids <- c(best_model_ids, df$model_ids[top_idx])
      }
      else if (!is.null(distance_percentage)) {
        if (dir == "maximize") {
          best_value <- max(df[[met]], na.rm = TRUE)
          best_model_ids <- c(best_model_ids, df$model_ids[which(df[[met]] >= best_value * distance_percentage)])
        }
        else {
          best_value <- min(df[[met]], na.rm = TRUE)
          best_model_ids <- c(best_model_ids, df$model_ids[which(df[[met]] <= best_value * (1+distance_percentage))])
        }
      }
    }
  }

  # # Get the unique union of best model IDs
  # best_model_ids <- unique(best_model_ids)

  # Determine which known metric columns exist in df
  existing_metrics <- intersect(metrics, names(df))

  # print(df[df$model_ids %in% best_model_ids, c("model_ids", existing_metrics), drop = FALSE])

  # Subset the original data frame for the best models and only include
  # the model_ids and the performance metric columns.
  if (hyperparam) result <- df[df$model_ids %in% best_model_ids, , drop = FALSE]
  else result <- df[df$model_ids %in% best_model_ids, c("model_ids", existing_metrics), drop = FALSE]


  # sort the columns for all the existing_metrics columns in order
  # ==========================================================
  # result <- result[with(result, order(existing_metrics)), ]
  order_by_cols <- function(df, cols, decreasing = FALSE, na.last = TRUE) {
    stopifnot(all(cols %in% names(df)))
    if (length(decreasing) == 1L) decreasing <- rep(decreasing, length(cols))
    stopifnot(length(decreasing) == length(cols))

    keys <- Map(function(col, dec) {
      k <- xtfrm(df[[col]])     # numeric ranking for any type
      if (dec) -k else k
    }, cols, decreasing)

    ord <- do.call(order, c(keys, list(na.last = na.last)))
    df[ord, , drop = FALSE]
  }
  result <- order_by_cols(result, existing_metrics, decreasing = sort_direction_map)
  # ord <- do.call(order, c(as.list(result[existing_metrics]), list(na.last = TRUE)))
  # result <- result[ord, , drop = FALSE]


  # Drop metric columns that are entirely NA in the resulting subset
  for (col in existing_metrics) {
    if (all(is.na(result[[col]]))) {
      result[[col]] <- NULL
    }
  }

  return(result)
}



