% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{perc.selective}
\alias{perc.selective}
\title{Transform all decimal columns/rows in a matrix/data.frame into percentages,
but only if all elements of the columns/rows are between the limits.
This allows distinction between decimal numbers and other numbers that vary in a greater range.}
\usage{
perc.selective(x, digits = 1, limits = c(0, 1), add.name = "(\%)", margin = 2)
}
\description{
Transform all decimal columns/rows in a matrix/data.frame into percentages,
but only if all elements of the columns/rows are between the limits.
This allows distinction between decimal numbers and other numbers that vary in a greater range.
}
\author{
Daniel Hoop
}
\keyword{internal}
