% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{removePrefixFromTypeCode}
\alias{removePrefixFromTypeCode}
\title{Remove the '15' prefix from the ZA2015 type code.}
\usage{
removePrefixFromTypeCode(x)
}
\description{
Remove the '15' prefix from the ZA2015 type code.
}
\examples{
x <- c(15234, 15, 1512, 1215)
removePrefixFromTypeCode(x)
}
\author{
Daniel Hoop
}
