% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{equal.n.decimals}
\alias{equal.n.decimals}
\title{Round all columns in \code{data.frame} or \code{matrix} to the same number of decimal places.}
\usage{
equal.n.decimals(x, digits = 2)
}
\description{
Round all columns in \code{data.frame} or \code{matrix} to the same number of decimal places.
}
\examples{
x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:10))
equal.n.decimals(x)
x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:9, 10.001))
equal.n.decimals(x)
x <- data.frame(a=seq(0.1, 1, 0.1) , b=rep("a", 10))
equal.n.decimals(x)
}
\author{
Daniel Hoop
}
\keyword{internal}
