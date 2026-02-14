#' @title Plot model performance metrics across a grid of models
#'
#' @description
#' Creates a line plot comparing multiple (maximize-type) performance metrics across a set of models.
#' The input data frame is typically the output of \code{hmda.grid.analysis()} and must contain
#' a \code{model_ids} column and one or more numeric metric columns (e.g., \code{aucpr}, \code{mcc}, \code{f2}).
#'
#' The function can either plot the first \code{top_models} rows (\code{criteria = "top_models"})
#' or include all models that achieve at least \code{distance_percentage} times the best value
#' for at least one metric (\code{criteria = "distance_percentage"}).
#'
#' @param df A data frame of class \code{"hmda.grid.analysis"} containing a column \code{model_ids}
#'   and numeric metric columns.
#' @param metrics Character vector of column names in \code{df} to be plotted.
#' @param criteria Character. One of \code{"top_models"} or \code{"distance_percentage"} (default).
#' @param top_models Integer. Number of top rows to plot when \code{criteria = "top_models"}.
#' @param distance_percentage Numeric in (0, 1]. When \code{criteria = "distance_percentage"},
#'   includes models with metric values \eqn{\ge} best(metric) * distance_percentage for at least one metric.
#' @param plot Logical. If \code{TRUE}, prints the plot.
#' @param title Character. Add title to the plot.
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
#'   # plot the metrics of models that are within 95% of the best models
#'   # for each of the specified metrics
#'   hmda.plot.metrics(grid_performance,
#'                     criteria = "distance_percentage",
#'                     distance_percentage = 0.95,
#'                     metrics = c("auc", "aucpr", "r2", "mcc", "f2"))
#'
#' }
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 scale_y_continuous ggtitle theme_classic labs theme
#'             element_text margin scale_color_brewer geom_line geom_point
#'             scale_x_continuous geom_vline
#' @export
#' @author E. F. Haghish

# plot the AUCPR, MCC, and F2 values on a line with ggplot2
hmda.plot.metrics <- function(df,
                              metrics = c("auc", "aucpr", "r2", "mcc", "f2"),
                              criteria = "distance_percentage",
                              top_models = 100,
                              distance_percentage = 0.95,
                              plot = TRUE,
                              title = NULL) {

  IDS  <- NULL

  # Basic Syntax
  # ============================================================
  if (is.null(title)) title <- paste0("Comparison of ", metrics, "across top models")

  # subset models
  # ============================================================
  if (criteria == "top_models") {
    df <- df[1:top_models, ]
  }
  else if (criteria == "distance_percentage") {

    # for each metric get the best model performance
    for (met in metrics) {
      best_value <- max(df[[met]], na.rm = TRUE)
      IDS <- c(IDS, df$model_ids[which(df[[met]] >= best_value * distance_percentage)])
    }

    IDS <- unique(IDS)
    df <- df[df$model_ids %in% IDS, ]
  }

  # change the IDs to index
  grid_legnth <- nrow(df)
  df$model_ids <- as.integer(seq(1, grid_legnth))

  melted <- melt(df, id.vars = "model_ids", measure.vars = metrics)

  # avoid Rstudio package check notes for unrecognized global variables
  model_ids <- melted$model_ids
  value     <- melted$value
  variable  <- melted$variable

  trends_plot <- ggplot(melted, aes(x = model_ids, y = value, color = variable)) +
    scale_color_brewer(palette = "Set1") +
    geom_line(linewidth = 1, alpha = .3) +
    geom_point(size = 2, alpha = .5) +
    scale_x_continuous(breaks = seq(0, grid_legnth, by = 10)) +
    scale_y_continuous(#limits = c(min, max) , breaks = seq(0.6, 0.8, by = 0.1), expand = c(0, 0)
    ) +
    labs(x = "\nIndex of models (sorted by AUCPR)",
         y = "Performance metrics\n",
         color = "") +
    ggtitle(title) +
    theme_classic() +
    theme(
      legend.position="top",
      legend.justification = "right",
      legend.title.align = 0.5,
      legend.direction = "horizontal",
      legend.text=element_text(colour="black", size=8, face="bold"),
      plot.title = element_text(size = 12),
      legend.key.height = grid::unit(0.4, "cm"),
      legend.key.width = grid::unit(1.2, "cm"),
      #legend.margin=margin(grid::unit(0,"cm")),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
      #plot.margin = margin(t = 0.3, r = .3, b = .25, l = .2, unit = "cm")  # Reduce top plot margin
    )

  # get the colors used in the plot
  #plot_colors <- scales::hue_pal()(length(metrics))

  # make a loop for "metrics", get the best value of each metric, and plot the circle with a higher-alpha color
  for (i in seq_along(metrics)) {
    m <- metrics[i]
    best_metric_index <- which.max(df[[m]])
    trends_plot <- trends_plot +
      geom_point(data = melted[melted$model_ids == best_metric_index & melted$variable == m, ], size = 2, alpha = 1) +
      # geom_point(data = melted[melted$model_ids == best_metric_index & melted$variable == m, ],
      #            aes(x = model_ids, y = value),
      #            #color = "black",
      #            size = 3,
      #            shape = 21,
      #            #fill = "yellow"
      # ) +
      geom_vline(xintercept = best_metric_index,
                 linetype="dashed",
                 # add color black with alpha set to 0.5
                 #color = "red",
                 alpha = 0.15

      ) #+
    # annotate("text", x = best_metric_index + 5, y = melted$value[melted$model_ids == best_metric_index & melted$variable == m],
    #          label = paste("Best", toupper(m)),
    #          angle = 90, vjust = -0.5, size = 3)
  }

  # make a loop for "metrics" and print the vertical line for all of them
  # for (i in seq_along(metrics)) {
  #   m <- metrics[i]
  #   best_metric_index <- which.max(df[[m]])
  #   trends_plot <- trends_plot +
  #     geom_vline(xintercept = best_metric_index,
  #                linetype="dashed",
  #                #color = plot_colors[i]
  #                ) +
  #     annotate("text", x = best_metric_index + 5, y = 0.6 + (i - 1) * 0.05,
  #              label = paste("Best", toupper(m)),
  #              #color = plot_colors[i],
  #              angle = 90, vjust = -0.5, size = 3)
  # }

  if (plot) print(trends_plot)

  return(trends_plot)
}



