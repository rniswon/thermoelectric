% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_boiler_fuel_data.R
\name{import_boiler_fuel_data}
\alias{import_boiler_fuel_data}
\title{import_boiler_fuel_data}
\usage{
import_boiler_fuel_data(boiler_fuel_data_file_path)
}
\arguments{
\item{boiler_fuel_data_file_path}{path to ~/thermoelectric-water-consumption-models/
CondenserDutyModel/Data/2015_BoilerFuelData.csv}
}
\value{
\code{boiler_fuel_data} formatted EIA-923 Monthly Boiler Fuel Consumption and Emissions Time Series
File, 2015 Final Revision data
}
\description{
import and format for use the
~/thermoelectric-water-consumption-models/CondenserDutyModel/Data/2015_BoilerFuelData.csv
file \cr \cr
Executed By: CondenserDutyModel_new.R \cr
}
\examples{
import_boiler_fuel_data(paste0(inputData_path,'/2015_BoilerFuelData.csv'))
}
