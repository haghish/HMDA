
#' @export

check_efa <- function(df,
                      features,
                      min_unique = 4,
                      min_intercorrelation = .3,
                      verbatim = FALSE) {

  # Vector to store messages for unsuitable features
  unsuitable_messages <- c()
  minimum_unique_observations <- c()

  # Check if given features are included in the dataframe
  # ====================================================
  for (feature in features) {
    if (!feature %in% names(df)) {
      unsuitable_messages <- c(
        unsuitable_messages,
        paste0("Feature '", feature, "' not found in the dataframe.")
      )
      next
    }

    col_data <- df[[feature]]

    # Check if the column is numeric
    # ====================================================
    if (!is.numeric(col_data)) {
      unsuitable_messages <- c(
        unsuitable_messages,
        paste0("Feature '", feature, "' is not numeric.")
      )
    }

    # Check for sufficient variability (at least 2 unique non-missing values)
    # ====================================================
    if (length(unique(na.omit(col_data))) < min_unique) {
      minimum_unique_observations <- c(minimum_unique_observations, feature)
    }

    # Check for a high proportion of missing values (threshold: 20%)
    # ====================================================
    if (mean(is.na(col_data)) > 0.2) {
      unsuitable_messages <- c(
        unsuitable_messages,
        paste0("Feature '", feature, "' has more than 20% missing values.")
      )
    }
  }

  if (length(minimum_unique_observations) > 0) {
    unsuitable_messages <- c(
      unsuitable_messages,
      paste0("The following features have constant or near-constant values. Make sure it is not categorical!:\n '",
             paste0(minimum_unique_observations, collapse = " "))
    )
  }


  # If more than one feature exists, check the correlation matrix rank
  # ====================================================
  if (length(features) > 1 && all(features %in% names(df))) {
    sub_df <- df[features]
    cor_matrix <- try(cor(sub_df, use = "pairwise.complete.obs"), silent = TRUE)
    if (inherits(cor_matrix, "try-error")) {
      message("Error computing correlation matrix for features. Check for missing values or insufficient variation.")
    } else {
      rank_cor <- qr(cor_matrix)$rank
      if (rank_cor < length(features)) {
        unsuitable_messages <- c(
          unsuitable_messages,
          "The correlation matrix of the features is not full rank; some variables may be redundant."
        )
      }
    }

    # Identify features with low intercorrelations
    #p.mat <- cor.mtest(efDf)

    # Identify items that have low correlations with other items
    N <- abs(cor_matrix) >= .4
    intercorrelations <- rowSums(N) - 1 # -1 because the item is always correlated with itself

    poor_features <- names(intercorrelations[intercorrelations==0])
    if (length(poor_features) > 0) {
      unsuitable_messages <- c(
        unsuitable_messages,
        paste0("The following features have low intercorrelations:\n '", paste0(poor_features, collapse = " "))
      )
    }
  }

  # Print messages if any issues are found
  # ====================================================
  if (length(unsuitable_messages) > 0) {
    #message("The following issues were found with the features:")
    for (msg in unsuitable_messages) {
      message(msg)
    }
    return(FALSE)
  } else {
    if (verbatim) message("All features appear suitable for exploratory factor analysis with the minrank algorithm.")
    return(TRUE)
  }
}

# Example usage:

#importantFeatures <- importantFeatures[importantFeatures %in% colnames(mlim)]
# check_efa(na.omit(raw), importantFeatures)
