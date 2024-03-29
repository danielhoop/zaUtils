% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{kruskal.groups}
\alias{kruskal.groups}
\title{Assign groups to character labels. Groups that do not have the same character, are significantly different.}
\usage{
kruskal.groups(
  y,
  trt,
  sig.level = 0.05,
  p.adj = "holm",
  median.mean = c("mean", "median", "both"),
  digits = 2,
  ranked = c("no", "character.left", "character.right", "rankmean", "intern")
)
}
\arguments{
\item{y}{The variable of interest.}

\item{trt}{The treatment vector.}

\item{sig.level}{The significance level which is relevant for the grouping with letters.}

\item{p.adj}{The probability adjustment method.}

\item{median.mean}{UNKNOWN}

\item{digits}{The number of digits in the result.}

\item{ranked}{UNKNOWN}
}
\value{
A list with different attributes, the most important of which has the name "summary".
}
\description{
Assign groups to character labels. Groups that do not have the same character, are significantly different.
}
\examples{
n <- 1000
y <-   c(rnorm(n, 0, 0.1), rnorm(n, 1, 0.1), rnorm(n, 0,  0.15))
trt <- c(rep(1, n),        rep(2, n),        rep(3, n))
kruskal.groups(y = y, trt = trt)
}
\author{
Daniel Hoop
}
