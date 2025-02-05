\name{combogencoo_cool_type2015}
\alias{combogencoo_cool_type2015}
\docType{data}
\title{combogencoo_cool_type2015}
\description{
%%  ~~ An example combogencoo_cool_type file for 2015 that assigns boiler-generator-cooling associations to specific cooling types.  Can be used as a starting point for `bocoo_associate()` or as a format guide for a user developed bogencoo-cooling type assignments file for use in the `condenserDuty` package ~~
}
\usage{data("combogencoo_cool_type2015")}
\format{
  A data frame with 1809 observations on the following 7 variables.
  \describe{
    \item{\code{bogencoo}}{a character vector indicating boiler-generator-cooling association unit}
    \item{\code{LAKE..OF..OC..RC.}}{a numeric vector between 0-1 indicating percentage of condenserDuty to be assigned to "lake" cooling}
    \item{\code{RIVER..OF.}}{a numeric vector between 0-1 indicating percentage of condenserDuty to be assigned to "river" cooling}
    \item{\code{POND..OC..RC.}}{a numeric vector between 0-1 indicating percentage of condenserDuty to be assigned to "pond" cooling}
    \item{\code{TOWER..RF..RI..RN.}}{a numeric vector between 0-1 indicating percentage of condenserDuty to be assigned to "tower" cooling}
    \item{\code{DC}}{a numeric vector between 0-1 indicating percentage of condenserDuty to be assigned to "dry" cooling}
    \item{\code{OS}}{a numeric vector between 0-1 indicating percentage of condenserDuty to be assigned to "saline" cooling}
  }
}

\source{
%%  ~~ reference to a publication from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(combogencoo_cool_type2015)
}
\keyword{datasets}
