#'@title calFuelHeat
#'@description calculate fuel heat by quantity consumed*heat content for fuel (MMbtu/Unit) \cr \cr
#'Executed By: \itemize{\item analyzeFuelHeat.R 
#'                        \item fuel_heat_analysis.R} \cr
#'@param quant_consumed quantity heat consumed values from EIA-923 Monthly Boiler Fuel Consumption 
#'                      and Emissions Time Series File, 2015 Final Revision, input file 
#'                      2015_BoilerFuelData.csv
#'@param unit_heat heat content for fuel (MMbtu/Unit) values from data.frame EIA-923 Monthly Boiler 
#'                 Fuel Consumption and Emissions Time Series File, 2015 Final Revision, input file 
#'                 2015_BoilerFuelData.csv
#'@return `fuel_heat` annual fuel-specific fuel heat, and boiler total fuel heat at each boiler
#'@export


#Calculates fuel heat values
calFuelHeat <- function(quant_consumed, unit_heat){
  fuel_heat <- quant_consumed * unit_heat
  return(fuel_heat)
}