

#' @export

hmda.features <- function(wmshap,
                          method = c("shapratio"),
                          cutoff = 0.01) {
  # Exclude features that do not meet the criteria
  # ====================================================
  summaryShaps <- wmshap$summaryShaps
  summaryShaps <- summaryShaps[summaryShaps[,method] >= cutoff, ]

  # Sort the results
  summaryShaps <- summaryShaps[order(summaryShaps$mean, decreasing = TRUE), ]
  return(as.vector(summaryShaps$feature))
}
