



#' @export
#' @author E. F. Haghish

# plot the AUCPR, MCC, and F2 values on a line with ggplot2
hmda.plot.metrics <- function(df,
                                metrics,
                                criteria = "top_models",
                                top_models = 100,
                                distance_percentage = 0.99,
                                plot = TRUE) {
  library(reshape2)
  library(ggplot2)
  IDS  <- NULL
  # range  <- NULL
  # min <- NULL
  # max <- NULL

  # subset models
  # ============================================================
  if (criteria == "top_models") {
    df <- df[1:top_models, ]
    # range <- c(min(df[, metrics]), max(df[, metrics]))
  }
  else if (criteria == "distance_percentage") {

    # for each metric get the best model performance
    for (met in metrics) {
      best_value <- max(df[[met]], na.rm = TRUE)
      IDS <- c(IDS, df$model_ids[which(df[[met]] >= best_value * distance_percentage)])
      # range <- c(range, best_value, best_value * distance_percentage)
    }

    IDS <- unique(IDS)
    df <- df[df$model_ids %in% IDS, ]
  }

  # # round range to go to closest .05
  # min <- floor(range[1] * 20) / 20
  # max <- ceiling(range[2] * 20) / 20

  # change the IDs to index
  grid_legnth <- nrow(df)
  df$model_ids <- as.integer(seq(1, grid_legnth))


  melted <- melt(df, id.vars = "model_ids", measure.vars = metrics)


  trends_plot <- ggplot(melted, aes(x = model_ids, y = value, color = variable)) +
    scale_color_brewer(palette = "Set1") +
    geom_line(size = 1, alpha = .3) +
    geom_point(size = 2, alpha = .5) +
    scale_x_continuous(breaks = seq(0, grid_legnth, by = 10)) +
    scale_y_continuous(#limits = c(min, max) , breaks = seq(0.6, 0.8, by = 0.1), expand = c(0, 0)
    ) +
    labs(x = "\nIndex of models (sorted by AUCPR)",
         y = "Performance metrics\n",
         color = "") +
    #ggtitle("Comparison of AUCPR, MCC, and F2 across top 100 GBM models") +
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

  return(plot)
}



