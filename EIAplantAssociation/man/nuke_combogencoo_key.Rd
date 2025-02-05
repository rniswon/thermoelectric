\name{nuke_combogencoo_key}
\alias{nuke_combogencoo_key}
\docType{data}
\title{nuke_combogencoo_key}
\description{
%%  ~~ A dataset containing boiler-generator-cooling associations for nuclear plants by plant_unit. ~~
}
\usage{data("nuke_combogencoo_key")}
\format{
  A data frame with 99 observations on the following 2 variables.
  \describe{
    \item{\code{combogencoo}}{a character string in the form of "combogencoo"^Plant.Code^boiler-generator-cooling-unit-id}
    \item{\code{plant_unit}}{a character string in the form of "gen"^Plant.Code^Nuclear.Unit.Id}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(nuke_combogencoo_key)
}
\keyword{datasets}
