% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idsu.R
\docType{data}
\name{idsu}
\alias{idsu}
\title{Adolescents who report suicide attempt vs. suicidal ideation}
\format{
A data frame with 1612 rows and 429 variables, addressing the following domains.
Note that each domain includes a number of variables, which are not documented in this
file, but can be found on the DOI link above:
\describe{
  \item{* Work and income}{}
  \item{* Background variables}{}
  \item{* Family relationships}{}
  \item{* Substance use and relationships to drugs}{}
  \item{* Mental health}{}
  \item{* Well-being}{}
  \item{* Somatic health}{}
  \item{* Culture and media usage}{}
  \item{* Personal interests}{}
  \item{* Spritual life}{}
  \item{* Sexuality and sexual behaviors}{}
  \item{* Social behavior, activities, and deviations}{}
  \item{* Political interests}{}
  \item{* Academic achievement}{}
}
}
\source{
\url{https://doi.org/10.18712/NSD-NSD1882-V1}
}
\usage{
data(idsu)
}
\description{
A dataset from the NSD study "Ung i Norge 2002" available via Surveybanken. This survey
examines youth attitudes, values, behaviors, mental health, and well-being in Norway. It includes both demographic
information and a series of Likert-scale items that measure various aspects of attitudes toward
suicide, mental health, and social support. For detailed documentation of the variables,
visit: \url{https://doi.org/10.18712/NSD-NSD1882-V1}
}
\details{
This dataset is part of the NSD study "Ung i Norge 2002" as documented in the Surveybanken.
This dataset is only a subset of the data. The \code{outcome} variable is a binary, where its values
are either "ideation" or "suicide", meaning, suicidal ideation and suicide attempt respectively.
}
\examples{
\dontrun{
  # Load the dataset (assuming it is included in the package)
  data(idsu)
}

}
\references{
OsloMet. (2012). Young people in Norway 2002 (Version 1) [Dataset].
            NSD Norwegian Centre for Research Data AS.
            https://doi.org/10.18712/NSD-NSD1882-V1
}
