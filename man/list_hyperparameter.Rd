% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_hyperparameter.R
\name{list_hyperparameter}
\alias{list_hyperparameter}
\title{Create Hyperparameter List from a leaderboard dataset}
\usage{
list_hyperparameter(df)
}
\arguments{
\item{df}{A data frame containing model results with
hyperparameter columns.}
}
\value{
A named list where each hyperparameter element is a vector of unique
        values for a hyperparameter.
}
\description{
Detects columns in a data frame that contain
             hyperparameters for H2O DRF/GBM algorithms and returns a list
             with the unique values from each parameter column.
}
\details{
This function scans the column names of the input data
frame for common H2O hyperparameter names, such as "ntrees",
"max_depth", "min_rows", "sample_rate",
"col_sample_rate_per_tree", "min_split_improvement",
"learn_rate", "mtries", and "seed". It extracts the unique
values from each matching column and returns them in a list.
The resulting list can be used as a hyperparameter grid for
tuning via H2O grid search functions.
}
\author{
E. F. Haghish
}
