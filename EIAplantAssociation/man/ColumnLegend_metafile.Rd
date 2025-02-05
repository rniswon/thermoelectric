\name{ColumnLegend_metafile}
\alias{ColumnLegend_metafile}
\docType{data}
\title{ColumnLegend_metafile}
\description{
%%  ~~ A description of all flags created and used by the `executeBogenAssoc()` function. This object is meant to be viewed in Microsoft Excel® with color coding.  To create the formatted Microsoft Excel® version of this object use the `writeColumnLegend_metafile()` function. ~~
}
\usage{data("ColumnLegend_metafile")}
\format{
  A data frame with 42 observations on the following 4 variables.
  \describe{
    \item{\code{FlagColumn}}{a character vector}
    \item{\code{Description}}{a character vector}
    \item{\code{removeAutomatically}}{a numeric vector}
    \item{\code{flagManual}}{a numeric vector}
  }
}

\examples{
data(ColumnLegend_metafile)
outputDir<-tempdir()
writeColumnLegend_metafile(outputDir)
}
\keyword{datasets}
