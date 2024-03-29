% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{write.table.zipped}
\alias{write.table.zipped}
\title{Create a zip file that contains a talbe written by `write.table`.}
\usage{
write.table.zipped(x, file, RtoolsBin = NULL, ...)
}
\arguments{
\item{x}{data.frame to be saved}

\item{file}{file path and name}

\item{RtoolsBin}{The directory called "bin" inside the installation location of Rtools.}

\item{...}{arguments to \code{write.table}: \code{append},
    \code{col.names}, \code{sep}, \code{dec} and \code{qmethod}
    cannot be altered.
  }
}
\description{
Create a zip file that contains a talbe written by `write.table`.
}
\details{
An alternative would be:\cr
`write.table(x, file=gzfile("filename.csv.gz")))`\cr
Combined with
 `read.table(gzfile("filename.csv.gz"))`
}
\author{
Daniel Hoop
}
