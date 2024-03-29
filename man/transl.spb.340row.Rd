% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{transl.spb.340row}
\alias{transl.spb.340row}
\title{Translate the rows in the ZA-BH-SpB-Table 340 to the Betriebszweigs-Namen.}
\usage{
transl.spb.340row(x, give.tab = FALSE)
}
\arguments{
\item{x}{The number to be translated to enterprise name.}

\item{give.tab}{Return translation vector without translating anything.}
}
\description{
Translate the rows in the ZA-BH-SpB-Table 340 to the Betriebszweigs-Namen.
}
\examples{
transl.spb.340row("30000")
}
\author{
Daniel Hoop
}
