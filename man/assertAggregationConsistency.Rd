% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{assertAggregationConsistency}
\alias{assertAggregationConsistency}
\title{Assert that all aggregations defined in \code{aggrInfo} are consistent within \code{data}.}
\usage{
assertAggregationConsistency(data, aggrInfo, tol = 0.1)
}
\arguments{
\item{data}{The \code{data.frame} within which the aggregation consistency should be asserted.}

\item{aggrInfo}{The structure containing the aggregation info. Refer to the example to see how it must be structured.}

\item{tol}{The tolerance which should be allowed when comparing the sum of disaggregated levels with the higher aggregation level.}
}
\value{
An invisible message "All aggregations are consistent.", if no inconsistencies were found. Otherwise the function will throw an error.
}
\description{
Assert that all aggregations defined in \code{aggrInfo} are consistent within \code{data}.
}
\examples{
data <- data.frame(
  total = NA,
    a = NA,
      a_1 = NA,
        a_11 = c(1,2,3),
        a_12 = c(1,2,3),
      a_2 = c(4,5,6),
    b = NA,
      b_1 = c(3,4,5),
      b_2 = c(3,4,5))
data <- within(data, {
  a_1 <- a_11 + a_12
  a <- a_1 + a_2
  b <- b_1 + b_2
  total <- a + b
})
aggrInfo <- list(
  # The first entry in aggrInfo must always be a character of length 1.
  "total",
  # Then, nested lists can follow.
  list("a",
       list("a_1",
            c("a_11", "a_12")),
       "a_2"),
  list("b",
       c("b_1", "b_2"))
)
# This will work because all aggregations are consistent.
assertAggregationConsistency(data = as.matrix(data), aggrInfo = aggrInfo)
# Now it will fail because we introduce an inconsistency.
data[, "a_2"] <- rep(1, nrow(data))
assertAggregationConsistency(data = data, aggrInfo = aggrInfo)
}
\seealso{
\code{\link{assertDatasetEquality}}
}
\author{
Daniel Hoop
}
