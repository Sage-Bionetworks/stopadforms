% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-gather-clean.R
\name{change_logical_responses}
\alias{change_logical_responses}
\title{Change logical responses}
\usage{
change_logical_responses(data)
}
\arguments{
\item{data}{Dataframe with response column and variable column.}
}
\description{
Change logical responses TRUE/FALSE to Yes/No. Additionally, need to handle
the variable "is_solution" which sometimes has 0/1 instead of TRUE/FALSE.
}
