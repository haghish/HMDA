#' Suggest Hyperparameters for Tree-based H2O Algorithms
#'
#' This function suggests hyperparameter values for tree-based algorithms in the H2O R package.
#' It currently focuses on three main algorithms:
#' \itemize{
#'   \item "gbm" (H2O Gradient Boosting Machine),
#'   \item "drf" (H2O Distributed Random Forest),
#' }
#' It takes a desired number of models (\code{n_models}) and attempts to construct hyperparameter
#' grids whose Cartesian product is near that size.
#'
#' @param algorithm A character string specifying the H2O algorithm name. Valid inputs are:
#'   \code{"gbm"}, \code{"drf"}, or \code{"xgboost"} (case-insensitive).
#' @param n_models An integer specifying the approximate number of total model combinations
#'   desired in the hyperparameter grid. Must be at least 100.
#'
#' @details
#' \strong{Important}:
#' \itemize{
#'   \item If \code{n_models} is less than 100, the function stops with an error.
#'         This is because \emph{the higher the number of models, the more robust HMDA becomes}.
#'   \item \code{mtries} in DRF supports a special value of \code{-1}, which means
#'         "use all features." In other words, if \code{mtries} is set to \code{-1}, H2O will
#'         include every predictor in each split.
#' }
#'
#' The function does not run the grid search. Instead, it returns a list of hyperparameter
#' values for quick prototyping. Use the returned list with \code{h2o.grid()} or other H2O
#' training functions.
#'
#' The strategy used is:
#' \enumerate{
#'   \item Define a default set of candidate hyperparameter values for the chosen algorithm.
#'   \item Calculate the total number of possible combinations (the Cartesian product).
#'   \item If it exceeds \code{n_models}, prune the largest sets until the total product is near
#'         or below \code{n_models}.
#'   \item If it is under \code{n_models} by a wide margin, attempt to add more values (less common).
#' }
#' The final size may not match \code{n_models} exactly, but this approach helps avoid extremely large
#' grids or grids that are too small.
#'
#' @return A named list of hyperparameter value vectors, where each element of the list corresponds
#'   to a hyperparameter name (e.g., \code{max_depth}, \code{ntrees}, etc.). This list can then be
#'   passed to H2O's grid search functions.
#'
#' @examples
#' \dontrun{
#'   library(h2o)
#'   h2o.init()
#'
#'   # Example 1: Suggest hyperparameters for GBM with about 120 models in the grid
#'   hyperparams_gbm <- hmda.suggest.hyperparameter("gbm", 120)
#'   hyperparams_gbm
#'
#'   # Example 2: Suggest hyperparameters for DRF with about 150 models
#'   hyperparams_drf <- hmda.suggest.hyperparameter("drf", 150)
#'   hyperparams_drf
#'   # Notice the special -1 value for mtries, meaning all features are used.
#'
#'   # Example 3: Suggest hyperparameters for XGBoost with about 200 models
#'   hyperparams_xgb <- hmda.suggest.hyperparameter("xgboost", 200)
#'   hyperparams_xgb
#'
#'   # Then use the suggested hyperparameters in an H2O grid search
#'   # e.g., for H2O DRF:
#'   # h2o.grid(
#'   #   algorithm = "drf",
#'   #   grid_id = "my_drf_grid",
#'   #   hyper_params = hyperparams_drf,
#'   #   training_frame = train,
#'   #   validation_frame = valid,
#'   #   x = features,
#'   #   y = "response_column"
#'   # )
#' }
#'
#' @export
hmda.suggest.param <- function(algorithm,
                               n_models,
                               x = NULL,
                               family = NULL) {
  # -------------------------------------------------------------------------
  # 1) Check the arguments
  # -------------------------------------------------------------------------
  if (n_models < 100) {
    stop("Number of models cannot be less than 100. \nThe higher the number of models, the more robust HMDA becomes.")
  }

  # Match argument for safety
  if (!is.null(family)) {
    if (family != "classification" & family != "regression") {
      stop("family should be either 'classification' or 'regression'")
    }
  }

  if (length(x) < 20) x <- NULL

  # -------------------------------------------------------------------------
  # 2) Internal helper to prune or expand hyperparameters to get product near n_models
  # -------------------------------------------------------------------------
  prune_or_expand <- function(hparams, target) {
    current_prod <- prod(sapply(hparams, length))

    # Simple loop to prune until we're near or below target
    while (current_prod > target && current_prod > 1) {
      # Find the hyperparameter with the largest length
      max_len_param <- which.max(sapply(hparams, length))
      if (length(hparams[[max_len_param]]) > 1) {
        # Drop the last value
        hparams[[max_len_param]] <- hparams[[max_len_param]][-length(hparams[[max_len_param]])]
      }
      current_prod <- prod(sapply(hparams, length))
    }

    # Optional expansion (only if extremely below the target)
    while (current_prod < (target / 2)) {
      min_len_param <- which.min(sapply(hparams, length))

      # Attempt a midpoint if numeric
      candidate_vals <- hparams[[min_len_param]]
      if (all(is.numeric(candidate_vals))) {
        diffs <- diff(sort(candidate_vals))
        max_diff_idx <- which.max(diffs)
        new_val <- mean(sort(candidate_vals)[c(max_diff_idx, max_diff_idx + 1)])
        new_val <- round(new_val, 3)
        if (!new_val %in% candidate_vals) {
          hparams[[min_len_param]] <- sort(c(candidate_vals, new_val))
        }
      }
      new_prod <- prod(sapply(hparams, length))
      if (new_prod == current_prod) break  # no change, avoid infinite loop
      current_prod <- new_prod
    }

    hparams
  }

  # -------------------------------------------------------------------------
  # 3) Normalize algorithm name (lowercase) and validate
  # -------------------------------------------------------------------------
  algorithm <- tolower(algorithm)
  if (!algorithm %in% c("gbm", "drf", "xgboost")) {
    stop("Invalid algorithm. Valid options are 'gbm', 'drf', or 'xgboost'.")
  }

  # -------------------------------------------------------------------------
  # 4) Define default hyperparameters
  # -------------------------------------------------------------------------
  if (algorithm == "gbm") {
    # Common hyperparameters for H2O GBM
    hyperparams <- list(
      max_depth       = c(3, 5, 7, 9),
      ntrees          = c(50, 100, 150),
      learn_rate      = c(0.01, 0.05, 0.1),
      sample_rate     = c(0.8, 1.0),
      col_sample_rate = c(0.8, 1.0)
    )
  } else if (algorithm == "drf") {
    # Common hyperparameters for H2O DRF
    # Note: mtries = -1 means "use all features" in H2O.
    if (is.null(x) & is.null(family)) {
      hyperparams <- list(
        ntrees      = c(50, 100, 150),
        max_depth   = c(20, 15, 25, 5, 10),
        sample_rate = c(0.632, 0.6, 0.66, 0.70, 0.58)
        # mtries      = suggest_mtries(p, family = family)
        # You may add more hyperparameters if you wish (e.g., binomial_double_trees, etc.).
      )
    } else if (!is.null(x) & !is.null(family)) {
      hyperparams <- list(
        ntrees      = c(50, 100, 150),
        max_depth   = c(5, 10, 15),
        sample_rate = c(0.632, 0.6, 0.66, 0.70, 0.58),
        mtries      = suggest_mtries(length(x), family = family)
        # You may add more hyperparameters if you wish (e.g., binomial_double_trees, etc.).
      )

    # otherwise define the default
    } else {
      hyperparams <- list(
        ntrees      = c(50, 100, 150),
        max_depth   = c(20, 15, 25, 5, 10),
        sample_rate = c(0.632, 0.6, 0.66, 0.70, 0.58)
        # mtries      = suggest_mtries(p, family = family)
        # You may add more hyperparameters if you wish (e.g., binomial_double_trees, etc.).
      )
    }
  } else {
    # XGBoost hyperparameters in H2O
    hyperparams <- list(
      ntrees           = c(50, 100, 150),
      max_depth        = c(3, 5, 7, 9),
      learn_rate       = c(0.01, 0.05, 0.1),
      min_rows         = c(5, 10),
      subsample        = c(0.8, 1.0),
      col_sample_rate  = c(0.8, 1.0)
    )
  }

  # -------------------------------------------------------------------------
  # 5) Prune or expand hyperparameters based on n_models
  # -------------------------------------------------------------------------
  hyperparams <- prune_or_expand(hyperparams, n_models)

  # -------------------------------------------------------------------------
  # 6) Return the final suggestion
  # -------------------------------------------------------------------------
  return(hyperparams)
}

#print(hmda.suggest.hyperparameter(algorithm = "DRF", n_models = 100, x = names(df)[1:500], family = "classification"))
