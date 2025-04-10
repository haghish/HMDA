% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/best_of_family.R
\name{best_of_family}
\alias{best_of_family}
\title{Select Best Models by Performance Metrics}
\usage{
best_of_family(df)
}
\arguments{
\item{df}{A data frame containing model performance results.
It must include a column named \code{"model_id"} and one
or more numeric columns for performance metrics.}
}
\value{
An integer or character vector of unique \code{model_id}
        values corresponding to the best model for each performance
        metric.
}
\description{
Detects all performance metric columns in a data frame,
  and for each metric, identifies the best model based on whether
  a higher or lower value is preferred. The function returns a vector
  of unique model IDs corresponding to the best models across all
  detected metrics.
}
\details{
The function first detects numeric columns (other than
        \code{"model_id"}) as performance metrics. It then uses a
        predefined mapping to determine the optimal direction for each
        metric: for example, higher values of \code{auc} and
        \code{aucpr} are better, while lower values of \code{logloss},
        \code{mean_per_class_error}, \code{rmse}, and \code{mse} are
        preferred. For any metric not in the mapping, the function
        assumes that lower values indicate better performance.

        For each metric, the function identifies the row index that
        produces the best value according to the corresponding direction
        (using \code{which.max()} or \code{which.min()}). It then extracts
        the \code{model_id} from that row. The final result is a unique
        set of model IDs that represent the best models across all metrics.
}
\author{
E. F. Haghish
}
