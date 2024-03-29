% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{c.1b1}
\alias{c.1b1}
\title{Concenates given vectors ... element by element.}
\usage{
\method{c}{`1b1`}(
  ...,
  add.names = c("char", "num", "obj.names", "own.names", "none"),
  own.names = NULL,
  names.at.front = FALSE,
  sep.sign = "_"
)
}
\arguments{
\item{add.names}{If add.names="own.names" then own.names must be given.}

\item{sep.sign}{Separates the original rownames from the add.names-endings}
}
\description{
Concenates given vectors ... element by element.
}
\examples{
a <- 1:10
names(a) <- LETTERS[1:10]
b <- c <- a
c.1b1(a,b,c,add.names="none", sep.sign="_")
c.1b1(a,b,c,add.names="num", names.at.front=FALSE, sep.sign="_")
c.1b1(a,b,c,add.names="own.names", own.names=c("x","y","z"), names.at.front=TRUE, sep.sign="_")
}
\seealso{
\code{\link{rbind.1b1}}
}
\author{
Daniel Hoop
}
