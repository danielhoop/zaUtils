% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaUtils.R
\name{readConversionSpb}
\alias{readConversionSpb}
\title{Read conversion rules for Stichprobe Betriebsfuehrung}
\usage{
readConversionSpb(
  file,
  allowFileFormats = c("xlsx", ", sep", "; sep"),
  dropInactive = TRUE,
  conversionDate = as.Date(paste0(substr(Sys.time(), 1, 4), "-01-01")),
  expandRanges = FALSE,
  accountSystemFile = NULL,
  verbose = FALSE
)
}
\arguments{
\item{file}{The file containing the conversion rules}

\item{allowFileFormats}{The file formats that are allowed.\cr\code{"xlsx"}   Excel\cr\code{", sep"}   comma separated\cr\code{"; sep"}   semicolon separated.}

\item{dropInactive}{Logical value indicating if non-active "Merkmale" should be kicked from the table (this is strongly recommended, therefore, default is \code{TRUE}).}

\item{conversionDate}{The date when the conversion was exported or when it is applied. If specified, only these rules are kept that are valid at the time of conversion (using the colums 'von' and 'bis' in the conversion table).\cr
Use something like \code{as.Date(Sys.time())} or \code{as.Date("01.01.2022", format = "\%d.\%m.\%Y")} to create an object of class "Date". Default is \code{as.Date(paste0(substr(Sys.time(), 1, 4), "-01-01"))}.}

\item{expandRanges}{Logical value indicating if account ranges should be expaned (e.g. 311-313 -> 311,312,313)}

\item{accountSystemFile}{If \code{expandRanges == TRUE}, then the file containing the ZA account system has to be provided (usually something like 'za-accountsystem.xml').}

\item{verbose}{Logical value indicating if warnings should be displayed when some conversion rule ranges do not contain any existing accounts.}
}
\description{
Read conversion rules for Stichprobe Betriebsfuehrung
}
\details{
Both, \code{dropInactive} and \code{conversionDate} have to be specified!
}
\author{
Daniel Hoop
}
