% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-certified-user.R
\name{check_certified_user}
\alias{check_certified_user}
\title{Check if user is certified}
\usage{
check_certified_user(id, syn)
}
\arguments{
\item{id}{User ID}

\item{syn}{Synapse client object}
}
\value{
A condition object indicating whether or not the given user is a
  certified Synapse user.
}
\description{
Check if user has completed and passed the Certified User Quiz.
}
\examples{
\dontrun{
syn <- synapse$Synapse()
syn$login()
check_certified_user("3384770")
}
}
