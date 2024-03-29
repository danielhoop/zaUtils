% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{getCol}
\alias{getCol}
\title{Get columns from object and provide hints in case of failure.}
\usage{
getCol(obj, col = NULL, expr = NULL)
}
\arguments{
\item{obj}{The object to extract columns from. Can be a matrix or data.frame}

\item{col}{The columns to extract.}

\item{expr}{Optionally instead of 'col', an expression can be given that should be evaluated. E.g. you could do this: \code{getCol(df, expr=with(df, a <- b))}}
}
\description{
Tries to extract columns ('col') from an object ('obj').
If it fails, then it provides hints on the names in 'obj' that match closest to 'col'.
}
