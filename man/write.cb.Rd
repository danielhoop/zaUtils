% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{write.cb}
\alias{write.cb}
\title{Write data to the clipboard.}
\usage{
write.cb(data, names = c("col", "rowcol", "row", "no"), ...)
}
\arguments{
\item{names}{Dimension names to be saved in the file. \code{"col"} for colnames, \code{"rowcol"} for rownames & colnames, \code{"row"} for rownames, and \code{"no"} for no dimnames.}

\item{...}{Further arguments to be passed to \code{read.table}.}
}
\description{
Write data to the clipboard.
}
\author{
Daniel Hoop
}
