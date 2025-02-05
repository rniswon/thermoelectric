\name{crosswalk2015}
\alias{crosswalk2015}
\docType{data}
\title{ExampleCrosswalk2015}
\description{
%%  ~~ An example of a crosswalk to be used by the plant_association and/or condenserDuty models to import and format EIA data (https://www.eia.gov/electricity/) necessary to generate boiler-generator-cooling associations and calculate condenser duty at thermoelectric plants.  EIA file, sheet, and column names are not consistent across years and therefore it is important to reformat the EIA data to make it usable regardless of what year is being analyzed.  The importThermoEIA package contains tools that utilize a crosswalk between EIA file, sheet, and column names and table and column names used by the plant_association and condenserDuty packages.  However, the importThermoEIA package is flexible enough to allow the user to create a custom crosswalk with custom script table and column names specific to the user's code rather than the plant_association and condenserDuty packages. More information in the importThermoEIA vignette ~~
}
\usage{data("crosswalk2015")}
\format{
  A data frame with 210 observations on the following 8 variables.
  \describe{
    \item{\code{InputFile}}{a character vector of EIA file names for the year 2015}
    \item{\code{InputSheetName}}{a character vector of Excel sheet names for the year 2015}
    \item{\code{InputSheetDescription}}{a character vector of EIA data descriptions of a given Excel sheet for the year 2015}
    \item{\code{InputColumnName}}{a character vector of EIA column names for the year 2015}
    \item{\code{InputColumnDescription}}{a character vector of EIA data descriptions of a given column for the year 2015}
    \item{\code{scriptImportFunc}}{a character vector of required import functions to be used if preparing data for the plant_association and/or condenserDuty packages}
    \item{\code{scriptTableName}}{a character vector of required tables to be used if preparing data for the plant_association and/or condenserDuty packages}
    \item{\code{scriptColnames}}{a character vector of required column names to be used if preparing data for the plant_association and/or condenserDuty packages}
  }
}
\details{
%%  ~~ This crosswalk can be used as a starting point in developing a crosswalk for years other than 2015 using the `import_EIAData()` function.  When developing a crosswalk for use by the plant_association and/or condenserDuty models it is important to maintain all of the contents of the scriptImportFunc, scriptTableName, and scriptColnames data.  Missing tables or columns will result in failure to execute functions within those packages. ~~
}
\source{
%%  ~~ https://www.eia.gov/electricity/ ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(crosswalk2015)

}
\keyword{datasets}
