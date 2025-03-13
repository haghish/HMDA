





#' @export

hmda.grid.analysis <- function(mygrid,
                               performance_metrics = c("logloss", "mae", "mse", "rmse", "rmsle", "auc",
                                                       "aucpr", "mean_per_class_error", "r2"),
                               sort_by = "logloss",
                               plot = TRUE) {

  library(ggplot2)

  mygrid <- h2o.getGrid(mygrid@grid_id, sort_by = sort_by, decreasing = "TRUE")

  # prepare the performance dataset
  performance <- as.data.frame(mygrid@summary_table)

  # get the additional metrics requested by the user
  # ============================================================
  newperf <- performance_metrics[!performance_metrics %in% sort_by]
  #if (j == "auc") performance[n, j] <- h2o.auc(h2o.getModel(unlist(mygrid@model_ids)[1]), xval = TRUE)

  IDS <- performance$model_ids
  for (j in newperf) {
    performance[, j] <- NA
    for (i in 1:length(IDS)) {
      MODEL <- h2o.getModel(IDS[i])
      if (j == "auc") performance[i, j] <- h2o.auc(MODEL, xval = TRUE)
      else if (j == "aucpr") performance[i, j] <- h2o.aucpr(MODEL, xval = TRUE)
      else if (j == "r2") performance[i, j] <- h2o.r2(MODEL, xval = TRUE)
      else if (j == "loggloss") performance[i, j] <- h2o.logloss(MODEL, xval = TRUE)
      else if (j == "mae") performance[i, j] <- h2o.mae(MODEL, xval = TRUE)
      else if (j == "mse") performance[i, j] <- h2o.mse(MODEL, xval = TRUE)
      else if (j == "rmse") performance[i, j] <- h2o.rmse(MODEL, xval = TRUE)
      else if (j == "r2") performance[i, j] <- h2o.r2(MODEL, xval = TRUE)

      # for thresholds metrics...
      else if (j %in% c("f1","f2","mcc","kappa")) {
        PERF <- h2o.performance(model = MODEL, xval= TRUE)
        if (j == "f1") performance[i, j] <- h2o.F1(PERF)
        else if (j == "f2") performance[i, j] <- h2o.F2(PERF)
        else if (j == "mcc") performance[i, j] <- h2o.mcc(PERF)
        else if (j == "kappa") performance[i, j] <- h2otools::kappa(PERF)
      }
    }
  }


  # # order the metrics
  # # ============================================================
  # if (sort_by %in% c("logloss", "mae", "mse", "rmse", "rmsle", "mean_per_class_error")) {
  #   performance <- performance[order(performance[,sort_by], decreasing = FALSE)]
  # }
  # else if (sort_by %in% c("auc", "aucpr", "r2", "f2", "mcc")) {
  #   performance <- performance[order(performance[,sort_by], decreasing = TRUE)]
  # }

  # # Sort the vector in ascending order
  # vector      <- performance[, sort_by]
  #
  # # Create a data frame with an index and the sorted values
  # df <- data.frame(Index = seq_along(v_sorted), Value = v_sorted)

  # v_sorted <- sort(v)
  #

  return(performance)
}

# a <- hmda.grid.analysis(mygrid = mygrid, performance_metrics = c("logloss","auc","aucpr"), sort_by = "logloss")
#
# MODEL <- h2o.getModel(performance$model_ids[1])
#
# h2o.metric(object = perf)
