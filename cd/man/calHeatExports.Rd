% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatExport_fun.R, R/calHeatExports.R
\name{calHeatExports}
\alias{calHeatExports}
\title{calHeatExports}
\usage{
calHeatExports(gen_fuel_data)

calHeatExports(gen_fuel_data)
}
\arguments{
\item{gen_fuel_data}{data.frame EIA-923 Monthly Generation and Fuel Consumption Time
Series File, 2015 Final Revision, input file 2015_GenerationAndFuel.csv}
}
\value{
\code{export_results} data.frame of monthly heat exports by plant

\code{export_results} data.frame of monthly heat exports by plant
}
\description{
calculates plant level monthly heat exports, for output file plant_exports.csv \\cr \\cr
Executed By: non_nuke_condenser_duty.R \\cr

calculates plant level monthly heat exports, for output file plant_exports.csv \cr \cr
Executed By: non_nuke_condenser_duty.R \cr
}
\examples{
calHeatExports(sheet1_gen_fuel_data_df)
calHeatExports(sheet1_gen_fuel_data_df)
}
