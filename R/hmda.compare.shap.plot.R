

#' @importFrom shapley shapley shapley.plot
#'
#' @export

hmda.compare.shap.plot <- function(hmda.grid.analysis,
                                   newdata = train.hex,
                                   model_id = NULL,
                                   metrics = c("aucpr", "mcc", "f2"),
                                   plot = "shap",
                                   top_n_features = 4) {

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
                                 top_n = top_n_features) +

        scale_y_continuous(limits = c(-1, 1),
                           breaks = seq(-1, 1, by = 0.5)) +

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
  library(gridExtra)
  combined <- grid.arrange(grobs = plot_list,
                           nrow = 1,
                           left = "Top 5 features",
                           bottom = "Comparison of SHAP contributions of top 5 features across three GBM models"
  )

  print(combined)
  return(combined)
}

# print(compare.shap.plot(hmda.grid.analysis = grid_analysis,
#                         newdata = splits$hmda.test.hex,
#                         metrics = c("aucpr", "mcc", "f2"),
#                         plot = "bar",
#                         top_n_features = 5))


