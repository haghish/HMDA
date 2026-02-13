#' @title Compare SHAP plots across selected models
#'
#' @description
#' Produces side-by-side comparison plots of SHAP contributions for multiple models.
#' Models can be provided explicitly via \code{model_id}, or selected automatically from
#' an \code{hmda.grid.analysis} data frame using \code{hmda.best.models()} for each metric in
#' \code{metrics}. Two plot styles are supported:
#' \describe{
#'   \item{\code{"shap"}}{H2O SHAP summary plot (beeswarm-style) for each model.}
#'   \item{\code{"bar"}}{Bar plot based on a single-model \code{shapley::shapley()} run.}
#' }
#'
#'@param hmda.grid.analysis A data frame of class \code{"hmda.grid.analysis"} containing model
#'   evaluation results and a \code{model_ids} column. Used only when \code{model_id} is \code{NULL}.
#' @param newdata An \code{H2OFrame} used for SHAP computation. Required for both plot types.
#' @param model_id Optional character vector of H2O model IDs. If provided, the function compares
#'   these models directly and ignores \code{hmda.grid.analysis} and \code{metrics}.
#' @param metrics Character vector of metric names used to select the best model per metric from
#'   \code{hmda.grid.analysis} via \code{hmda.best.models(..., n_models = 1)}.
#' @param plot Character. Plot type: \code{"shap"} (default) or \code{"bar"}.
#' @param top_n_features Integer. Number of top features shown in each plot.
#' @param ylimits Numeric vector of length 2 giving y-axis limits for \code{plot = "shap"}.
#'                the default is c(-1, 1), which is only specified for aesthetic reasons,
#'                to make the plots comparable. Consider expanding these limits based on
#'                your data.
#'
#' @return A \code{gtable}/grob object returned by \code{gridExtra::grid.arrange()} combining the plots.
#'   The combined plot is also printed.
#'
#' @details
#' When \code{model_id} is \code{NULL}, the function selects one model per metric from \code{metrics}
#' using \code{hmda.best.models()}. When \code{model_id} is provided, models are labeled as
#' \code{"Model 1"}, \code{"Model 2"}, etc.
#'
#' @examples
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
#'   # Assess the performances of the models
#'   grid_performance <- hmda.grid.analysis(hmda_grid1)
#'
#'   # compare the best models acording to each performance metric
#'   hmda.compare.shap.plot(hmda.grid.analysis = grid_performance,
#'                          newdata = test,
#'                          metrics = c("aucpr", "mcc", "f2"),
#'                          plot = "bar",
#'                          top_n_features = 5)
#'
#'   # Return the best 2 models according to each metric
#'   best_models <- hmda.best.models(grid_performance, n_models = 2)
#'
#'   # compare the specified models based on their model_ids
#'   hmda.compare.shap.plot(hmda.grid.analysis = grid_performance,
#'                          model_id = best_models$model_ids[1:3],
#'                          newdata = test,
#'                          metrics = c("aucpr", "mcc", "f2"),
#'                          plot = "bar",
#'                          top_n_features = 5)
#'
#' }
#'
#' @importFrom shapley shapley shapley.plot
#' @importFrom h2o h2o.getModel h2o.shap_summary_plot
#' @importFrom ggplot2 scale_y_continuous ggtitle xlab ylab theme_classic labs theme element_text margin
#' @importFrom gridExtra grid.arrange
#' @export
#' @author E. F. Haghish

hmda.compare.shap.plot <- function(hmda.grid.analysis,
                                   newdata = NULL,
                                   model_id = NULL,
                                   metrics = c("aucpr", "mcc", "f2"),
                                   plot = "shap",
                                   top_n_features = 4,
                                   ylimits = c(-1, 1)) {

  # store the plots
  plot_list <- list()
  MODEL_IDS   <- NULL
  MODEL_NAMES <- NULL
  n = 0

  # get the model ids
  if (is.null(model_id)) {
    for (m in metrics) {
      MODEL_IDS  <- c(MODEL_IDS, hmda.best.models(hmda.grid.analysis, n_models = 1, metrics = m)$model_ids)
      MODEL_NAMES<-c(MODEL_NAMES, paste("\nHighest", toupper(m)))
    }
  }
  else {
    MODEL_IDS <- model_id
    for (m in seq(length(MODEL_IDS))) {
      MODEL_NAMES<-c(MODEL_NAMES, paste("\nModel", m))
    }
  }

  for (id in MODEL_IDS) {
    n <- n + 1

    # get the model
    # ============================================================
    model <- h2o.getModel(id)

    if (plot == "shap") {
      p <- h2o.shap_summary_plot(model,
                                 newdata,
                                 top_n_features = top_n_features) +

        scale_y_continuous(limits = ylimits,
                           breaks = seq(ylimits[1], ylimits[2], by = 0.5)) +

        ggtitle(MODEL_NAMES[n]) +
        xlab("") +
        ylab("") +
        theme_classic() +
        labs(colour = "Normalized values\n") +
        theme(
          legend.position="none",
          legend.justification = "right",
          legend.title.align = 0.5,
          legend.direction = "horizontal",
          legend.text=element_text(colour="black", size=6, face="bold"),
          plot.title = element_text(size = 10),
          legend.key.height = grid::unit(0.3, "cm"),
          legend.key.width = grid::unit(1, "cm"),
          #legend.margin=margin(grid::unit(0,"cm")),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
          #plot.margin = margin(t = -0.3, r = .25, b = .25, l = .25, unit = "cm")  # Reduce top plot margin
        )
    }

    if (plot == "bar") {
      resultsnow <- shapley(
        n_models = 1,
        standardize_performance_metric = TRUE, #for a single model
        models = id,
        newdata = newdata,
        top_n_features = top_n_features,
        plot = FALSE)

      p <- shapley.plot(resultsnow, top_n_features = top_n_features)

      p <- p +
        ggtitle(MODEL_NAMES[n]) +
        xlab("") +
        ylab("")
      #   theme_classic() +
      theme(
        #     legend.position="none",
        #     legend.justification = "right",
        #     legend.title.align = 0.5,
        #     legend.direction = "horizontal",
        #     legend.text=element_text(colour="black", size=6, face="bold"),
        plot.title = element_text(size = 10),
        #     legend.key.height = grid::unit(0.3, "cm"),
        #     legend.key.width = grid::unit(1, "cm"),
        #     #legend.margin=margin(grid::unit(0,"cm")),
        #     legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        #     #plot.margin = margin(t = -0.3, r = .25, b = .25, l = .25, unit = "cm")  # Reduce top plot margin
      )
    }

    # save the plot in the list
    plot_list[[n]] <- p
  }


  # COMPARE PLOTS
  # ============================================================
  combined <- grid.arrange(grobs = plot_list,
                           nrow = 1,
                           left = "Top 5 features",
                           bottom = "Comparison of SHAP contributions of top 5 features across three GBM models"
  )

  print(combined)
  return(combined)
}




