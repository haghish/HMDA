% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmda.adjust.params.R
\name{hmda.adjust.params}
\alias{hmda.adjust.params}
\title{Adjust Hyperparameter Combinations}
\usage{
hmda.adjust.params(params, n_models)
}
\arguments{
\item{params}{A list of hyperparameter vectors.}

\item{n_models}{Integer. The desired target number of model
combinations.}
}
\value{
A list of hyperparameter vectors that has been pruned or
        expanded so that the product of their lengths is near
        \code{n_models}.
}
\description{
This internal function prunes or expands a list of
  hyperparameters so that the total number of model combinations,
  computed as the product of the lengths of each parameter vector,
  is near the desired target (\code{n_models}). It first prunes the
  parameter with the largest number of values until the product is
  less than or equal to \code{n_models}. Then, if the product is much
  lower than the target (less than half of \code{n_models}), it attempts
  to expand the parameter with the smallest number of values by adding
  a midpoint value (if numeric).
}
\details{
The function calculates the current product of the
  lengths of the hyperparameter vectors. In a loop, it removes the
  last element from the parameter vector with the largest length
  until the product is less than or equal to \code{n_models}. If the
  resulting product is less than half of \code{n_models}, the function
  attempts to expand the parameter with the smallest length by
  computing a midpoint between the two closest numeric values. The
  expansion stops if no new value can be added, to avoid an infinite
  loop.
}
\examples{
  # Example 1: Adjust a hyperparameter grid for 100 models.
  params <- list(
    alpha = c(0.1, 0.2, 0.3, 0.4),
    beta = c(1, 2, 3, 4, 5),
    gamma = c(10, 20, 30)
  )
  new_params <- hmda.adjust.params(params, n_models = 100)
  print(new_params)

  # Example 2: The generated hyperparameters range between min and max of each
  # vector in the list
  params <- list(
    alpha = c(0.1, 0.2),
    beta = c(1, 2, 3),
    gamma = c(10, 20)
  )
  new_params <- hmda.adjust.params(params, n_models = 1000)
  print(new_params)

}
\author{
E. F. Haghish
}
