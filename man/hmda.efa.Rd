% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmda.efa.R
\name{hmda.efa}
\alias{hmda.efa}
\title{Perform Exploratory Factor Analysis with HMDA}
\usage{
hmda.efa(
  df,
  features,
  algorithm = "minres",
  rotation = "promax",
  parallel.analysis = TRUE,
  nfactors = NULL,
  dict = dictionary(df, attribute = "label"),
  minimum_loadings = 0.3,
  exclude_features = NULL,
  ignore_binary = TRUE,
  intercorrelation = 0.3,
  reverse_features = NULL,
  plot = FALSE,
  factor_names = NULL
)
}
\arguments{
\item{df}{A data frame containing the items for EFA.}

\item{features}{A vector of feature names (or indices) in \code{df} to
include in the factor analysis.}

\item{algorithm}{Character. The factor extraction method to use.
Default is \code{"minres"}. Other methods supported by
\pkg{psych} (e.g., "ml", "minchi") may also be used.}

\item{rotation}{Character. The rotation method to apply to the factor
solution. Default is \code{"promax"}.}

\item{parallel.analysis}{Logical. If \code{TRUE}, runs parallel analysis using
\code{psych::fa.parallel} to recommend the number of
factors. Default is \code{TRUE}.}

\item{nfactors}{Integer. The number of factors to extract. If \code{NULL}
and \code{parallel.analysis = TRUE}, the number of
factors recommended by the parallel analysis is used.}

\item{dict}{A data frame dictionary with at least two columns:
\code{"name"} and \code{"description"}. Used to replace
feature names with human-readable labels. Default is
\code{dictionary(df, attribute = "label")}.}

\item{minimum_loadings}{Numeric. Any factor loading with an absolute value
lower than this threshold is set to zero. Default is
\code{0.30}.}

\item{exclude_features}{Character vector. Features to exclude from the analysis.
Default is \code{NULL}.}

\item{ignore_binary}{Logical. If \code{TRUE}, binary items may be ignored
in the analysis. Default is \code{TRUE}.}

\item{intercorrelation}{Numeric. (Unused in current version) Intended to set
a minimum intercorrelation threshold between items.
Default is \code{0.3}.}

\item{reverse_features}{A vector of feature names for which the scoring
should be reversed prior to analysis. Default is
\code{NULL}.}

\item{plot}{Logical. If \code{TRUE}, a factor diagram is plotted
using \code{psych::fa.diagram}. Default is \code{FALSE}.}

\item{factor_names}{Character vector. Optional names to assign to the
extracted factors (i.e., new column names for loadings).}
}
\value{
A list with the following components:
  \describe{
    \item{parallel.analysis}{The output from the parallel analysis, if run.}
    \item{efa}{The full exploratory factor analysis object returned by
               \code{psych::fa}.}
    \item{efa_loadings}{A matrix of factor loadings after zeroing out values
               below the \code{minimum_loadings} threshold, rounded and sorted.}
    \item{efa_reliability}{The reliability results (omega) computed from the
               factor scores.}
    \item{factor_correlations}{A matrix of factor correlations, rounded to 2
               decimal places.}
  }
}
\description{
Performs exploratory factor analysis (EFA) on a specified set
  of features from a data frame using the \pkg{psych} package. The function
  optionally runs parallel analysis to recommend the number of factors, applies
  a rotation method, reverses specified features, and cleans up factor loadings
  by zeroing out values below a threshold. It then computes factor scores and
  reliability estimates, and finally returns a list containing the EFA results,
  cleaned loadings, reliability metrics, and factor correlations.
}
\details{
This function first checks that the number of factors is either provided
  or determined via parallel analysis (if \code{parallel.analysis} is \code{TRUE}).
  A helper function \code{trans()} is defined to reverse and standardize item
  scores for features specified in \code{reverse_features}. Unwanted features can be
  excluded via \code{exclude_features}. The EFA is then performed using
  \code{psych::fa()} with the chosen extraction algorithm and rotation method.
  Loadings are cleaned by zeroing out values below the \code{minimum_loadings}
  threshold, rounded, and sorted. Factor scores are computed with
  \code{psych::factor.scores()} and reliability is estimated using the
  \code{omega()} function. Finally, factor correlations are extracted from the
  EFA object.
}
\examples{
\dontrun{
  # Example: Perform EFA on a dataset 'raw' using selected features.
  # Assume 'raw' is a data frame and 'importantFeatures' is a vector of
  # feature names determined to be important (e.g., from a SHAP analysis).
  importantFeatures <- c("feature1", "feature2", "feature3")
  efa_results <- hmda.efa(df = raw,
                          features = importantFeatures,
                          nfactors = 3,
                          algorithm = "minres",
                          rotation = "promax",
                          minimum_loadings = 0.30,
                          exclude_features = c("featureX"),
                          reverse_features = c("feature2"),
                          plot = TRUE,
                          factor_names = c("Factor1", "Factor2", "Factor3"))

  # View the sorted factor loadings
  print(efa_results$efa_loadings)

  # View the reliability analysis
  print(efa_results$efa_reliability)

  # View the factor correlation matrix
  print(efa_results$factor_correlations)
}

}
\author{
E. F. Haghish
}
