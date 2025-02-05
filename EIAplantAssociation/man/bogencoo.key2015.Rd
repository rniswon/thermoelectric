\name{bogencoo.key2015}
\alias{bogencoo.key2015}
\docType{data}
\title{bogencoo.key2015}
\description{
%%  ~~ A published data.frame linking boiler-generator units to boiler-generator-cooling units for 2015 to be used by the `bocoo_associate()` function using the `useStandardBogencoo.key=TRUE` argument to fill in missing boiler-generator-cooling assignments not provided by the EIA `inputData.list2015$bocoo` table. ~~
}
\usage{data("bogencoo.key2015")}
\format{
  A data frame with 1947 observations on the following 2 variables.
  \describe{
    \item{\code{combogen}}{a character vector in the following format "combogen"^Plant.Code^boiler-generator-unit-number}
    \item{\code{combogencoo}}{a character vector in the following format "combogencoo"^Plant.Code^boiler-generator-cooling-unit-number}
  }
}

\source{
%%  ~~ reference to a publication from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(bogencoo.key2015)
}
\keyword{datasets}
