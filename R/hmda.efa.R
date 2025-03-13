

#' @export

hmda.efa <- function(df,
                     features,
                     algorithm = "minrank",
                     rotation = "promax",
                     parallel.analysis = TRUE,
                     nfactors = NULL,
                     dict = dictionary(df, attribute = "label"),
                     minimum_loadings = 0.30,
                     exclude_features = NULL,
                     ignore_binary = TRUE,
                     intercorrelation = 0.3,
                     reverse_features = NULL,
                     plot = FALSE,
                     factor_names = NULL) {

  library(psych)
  library(pander)
  pa <- NULL

  # Check the data for items not suitable for efa
  # ====================================================
  if (is.null(nfactors) & !parallel.analysis) stop("either run parallel.analysis or specify number of factors in 'nfactor' argument")

  # Function to reverse the variable's direction
  # ====================================================
  trans <- function(x, reverse = FALSE) {
    if (reverse) x <- -x
    x <- (x - min(x, na.rm = T)) / diff(range(x, na.rm = TRUE))
    return(x)
  }

  if (!is.null(reverse_features)) {
    for (i in reverse_features) df[,i] <- trans(df[,i], reverse = TRUE)
  }

  # Exclude unwanted features
  # ====================================================
  if (!is.null(exclude_features)) {
    features <- features[!features %in% exclude_features]
  }

  # Compute number of factors
  # ====================================================
  if (parallel.analysis){
    pa <- capture(psych::fa.parallel(df[, features], fa="fa", fm=algorithm, plot = plot))
    # if (sum(pa$value$fa.values > 0) > pa$value$nfact) {
    #   message(paste(pa$value$nfact, "factors are recommended, but there might be better solutions (for example, up to", sum(pa$value$fa.values > 0), "factors)"))
    # }
    # else message(paste(pa$value$nfact, "factors are recommended"))
    message(paste(pa$value$nfact, "factors are recommended"))
  }

  # Run the exploratory factor analysis
  # ====================================================
  EFAresults <- psych::fa(r=df[, features],
                          nfactors = if (!is.null(nfactors)) nfactors else pa$value$nfact,
                          fm = algorithm,
                          rotate = rotation)



  loadings <- EFAresults$loadings
  #
  # EFAresults <- factanal(~ .,
  #                        data = df[, features],
  #                        factors = if (!is.null(nfactors)) nfactors else pa$value$nfact,
  #                        rotation = rotation,
  #                        na.action = na.exclude)

  loadings <- EFAresults$loadings

  # Add the labels to the table
  if (!is.null(dict)) {
    for (i in 1:length(rownames(loadings))) {
      index <- dict[,1] == rownames(loadings)[i]
      if (sum(index) == 1) rownames(loadings)[i] <- dict[index, 2]
    }
  }

  # tidy up the loadings
  loadings[loadings > - minimum_loadings & loadings < minimum_loadings] <- 0
  if (!is.null(factor_names)) colnames(loadings) <- factor_names
  loadings <- round(loadings, 2)
  loadings <- fa.sort(loadings)

  print(loadings)

  # a better plot would be nice ???
  if (plot) fa.diagram(EFAresults)

  # Factor reliability
  # ====================================================

  # 1. Calculate factor scores
  factor_scores <- factor.scores(df[, features], EFAresults)$scores

  # 2. Compute reliability for each factor
  reliability_results <- omega(factor_scores)
  print(reliability_results)

  # Factor correlation
  # ====================================================
  factor_correlations <- round(EFAresults$Phi,2)

  # Prepare the results
  # ====================================================
  results <- list(
    parallel.analysis = pa,
    efa = EFAresults,
    efa_loadings = loadings,
    efa_reliability = reliability_results,
    factor_correlations = factor_correlations
  )

  return(results)
}

# importantFeatures <- hmda.features(wmshap, method = "shapratio", cutoff = 0.01)
# importantFeatures <- importantFeatures[importantFeatures %in% colnames(raw)]
# check_efa(raw, importantFeatures)
#pa <- psych::fa.parallel(raw[, importantFeatures], fa="fa", fm="minrank", plot=FALSE)

# importantFeatures <- hmda.features(wmshap, method = "shapratio", cutoff = 0.008)
# importantFeatures <- hmda.features(wmshap, top_n_features = 30)
# importantFeatures <- hmda.features(wmshap, method = "lowerCI", cutoff = 0.1)

# importantFeatures <- hmda.feature.selection(wmshap, method = "shapratio", cutoff = 0.005)
# importantFeatures$important
#
# check_efa(df=raw, features = importantFeatures$important, min_intercorrelation = 0.1)

# hmda.efa(df=raw, features = importantFeatures$important, nfactors = 10,
#          minimum_loadings = 0.25,
#          exclude_features = c("ParIn2_1", "Gende1_1", "AlcDe2_2", "AlcDe2_1", "PolA2n10",
#                               "ParIn2_5", "ParJo1_4", "ParIn2_5"), algorithm = "ml")

# importantFeatures <- hmda.feature.selection(wmshap, method = "shapratio", cutoff = 0.005)
# hmda.efa(df=raw, features = importantFeatures$important, nfactors = 11, algorithm = "minrank",
#          minimum_loadings = 0.3, dict = NULL,
#          exclude_features = c("ParIn2_1", "Gende1_1", "AlcDe2_2", "AlcDe2_1", "PolA2n10",
#                               "ParIn2_5", "ParJo1_4", "ParIn2_5", "Heigh1_1", "GmTy2_01"))
#
