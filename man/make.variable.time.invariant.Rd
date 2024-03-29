% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{make.variable.time.invariant}
\alias{make.variable.time.invariant}
\title{Makes a variable time invariant. E.g. if an observation has mostly value 1, but sometimes 2. The value 2 is rather a measurement error than the true value.}
\usage{
make.variable.time.invariant(
  x,
  index,
  method = c("mostFrequent", "mean", "median")
)
}
\arguments{
\item{x}{The vector of values that shall be checked. If a matrix/data.frame, then the function will be applied to all columns.}

\item{index}{The index, e.g. the id of each observation.}

\item{method}{The method to be applied.}
}
\value{
The corrected vector \code{x}.
}
\description{
In this case, the value 2 will be replaced by either 1 (\code{method = "mostFrequent"}), 1.xxx (\code{method = "mean"}) or the median value (\code{method = "median"}).
}
\examples{
x <-     c(1,1,2, 3,3,4)
index <- c(0,0,0, 1,1,1)
make.variable.time.invariant(x, index)
# [1] 1 1 1 3 3 3
}
\author{
Daniel Hoop
}
