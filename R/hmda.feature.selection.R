
#' @description
#' Specify important, inessential, and irrelevant items
#'

#' @export

hmda.feature.selection <- function(wmshap,
                          method = c("shapratio"),
                          cutoff = 0.01,
                          top_n_features = NULL) {
  # Exclude features that do not meet the criteria
  # ====================================================
  summaryShaps <- wmshap$summaryShaps

  # Sort the results
  summaryShaps <- summaryShaps[order(summaryShaps$mean, decreasing = TRUE), ]

  # get a list of all features with wmshap values
  all_features <- as.vector(summaryShaps[ , "feature"])

  # define the removable items, where lowerCI is bellow zero
  irrelevant  <- as.vector(summaryShaps[summaryShaps[, "lowerCI"] < 0, "feature"])

  # select the features
  if (is.null(top_n_features)) {
    important <- as.vector(summaryShaps[summaryShaps[,method] >= cutoff, "feature"])
    inessential <- all_features[!all_features %in% c(important, irrelevant)]
  }
  else {
    important <- as.vector(summaryShaps[1:top_n_features, "feature"])
    inessential <- all_features[!all_features %in% c(important, irrelevant)]
  }

  results <- list(
    important = important,
    inessential = inessential,
    irrelevant= irrelevant
  )

  return(results)
}
