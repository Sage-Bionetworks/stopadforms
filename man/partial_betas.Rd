% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{partial_betas}
\alias{partial_betas}
\title{Partial beta weights of certain variables}
\format{
A data frame with 23 rows and 4 columns:
\describe{
  \item{section}{Name of the section as it appears in the raw data, e.g. "naming"}
  \item{variable}{Name of the field as it appears in the raw data, e.g. "is_off_label"}
  \item{partial_beta}{Partial beta weight}
  \item{notes}{Notes about the calculations for that variable}
}
}
\usage{
partial_betas
}
\description{
The partial beta weight is one of the multipliers used to calculate the score
for a section.
}
\keyword{datasets}
