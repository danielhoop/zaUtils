% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{fullUsername}
\alias{fullUsername}
\title{Getting the full name of a user, using a mapping between BIT/Agroscope account numbers and real persons names.}
\usage{
fullUsername()
}
\value{
The full name of the user, if it could be found in the mapping, else the username given by \code{Sys.info()["user"]}.
If a username starting with \code{"U"} is returned by \code{Sys.info()["user"]}, then the first character is replaced by \code{"A"}.
}
\description{
Getting the full name of a user, using a mapping between BIT/Agroscope account numbers and real persons names.
}
\author{
Daniel Hoop
}
