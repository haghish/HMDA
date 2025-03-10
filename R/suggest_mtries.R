#' Suggest Alternative Values for mtries in Random Forest
#'
#' Provides a set of suggested values for the \code{mtries} parameter based on the
#' number of features (\code{p}) and the modeling family. These values serve as
#' alternatives to the common defaults: \code{sqrt(p)} for classification and
#' \code{p/3} for regression.
#'
#' @param p Integer. The number of features (predictors) in the dataset.
#' @param family Character string. Either \code{"classification"} or \code{"regression"}.
#'
#' @details
#' \strong{Default Behavior in Random Forest}:
#' \itemize{
#'   \item \code{mtries = -1} in H2O means "use all features."
#'   \item Traditional implementations in other frameworks often use
#'         \code{sqrt(p)} for classification and \code{p/3} for regression.
#' }
#'
#' \strong{Suggested Alternatives}:
#' \itemize{
#'   \item For classification, people often try \code{log2(p)}, or smaller exponents
#'         of \code{p} (e.g., \code{p^(1/3)}), in addition to \code{sqrt(p)}.
#'   \item For regression, people sometimes try smaller fractions of \code{p},
#'         such as \code{p/5} or \code{p/2}, instead of \code{p/3}.
#' }
#' The best choice can depend on the problem size, data structure, and correlation between predictors.
#'
#' @return An integer vector of suggested values for \code{mtries}.
#'
#' @examples
#' # Classification example
#' my_mtries_suggestions(p = 100, family = "classification")
#'
#' # Regression example
#' my_mtries_suggestions(p = 100, family = "regression")
#'
#' @export
suggest_mtries <- function(p, family = c("classification", "regression")) {

  # Match argument for safety
  family <- match.arg(family)

  # Some “safe” minimum to avoid 0 or negative
  # In case p is very small
  safe_floor <- function(x) {
    v <- floor(x)
    #max(v, 1)
  }

  # Prepare suggestion sets
  if (family == "classification") {
    # Common default: sqrt(p)
    # Additional suggestions: log2(p), p^(1/3), p/2, etc.
    candidates <- c(
      sqrt(p),
      log2(p),
      p^(1/3),
      (sqrt(p) + log2(p) + p^(1/3))/2,
      (sqrt(p) + log2(p))/2,
      sqrt(p) + (p^(1/3))/2
      #p/2  # Might be large, but sometimes used
    )

  } else {
    # family == "regression"
    # Common default: p/3
    # Additional suggestions: p/2, p/5, sqrt(p), etc.
    candidates <- c(
      p/3,
      p/4,
      p/2,
      p/5,
      (p/3 + p/2)/2 #conservative list
    )
  }

  # Convert to integer and ensure non-zero
  candidates_int <- unique(safe_floor(candidates))

  # Return
  return(candidates_int)
}


#print(suggest_mtries(p = 600, family = "regression"))
