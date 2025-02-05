#'@title calFuelDominance
#'@description calculate dominant fuel type, flag dominant fuels, majority of boiler fuel, 
#'             and fuels with zero consmption \cr \cr
#'Executed By: \itemize{\item analyzeFuelHeat.R 
#'                        \item fuel_heat_analysis.R} \cr
#'@param fuel_pct fuel_specific_total / boiler_total calculated using values from 
#'                EIA-923 Monthly Boiler Fuel Consumption and Emissions Time Series File, 
#'                2015 Final Revision, input file 2015_BoilerFuelData.csv
#'@param dom_pct numeric threshold for dominant fuels
#'@param maj_thrsh numeric threshold for boiler majority fuel
#'@return `dom_fuel` vector that flags dominant fuels, majority of boiler fuel, and fuels 
#'                   with zero consmption, assigns dominant fuel codes
#'@examples
#'sapply(raw_fuel_heat_df_2$fuel_pct, calFuelDominance, dom_thrsh = dom_thrsh, maj_thrsh = maj_thrsh)


calFuelDominance <- function(fuel_pct, dom_thrsh, maj_thrsh){
  if(is.na(fuel_pct) | fuel_pct==0){
    dom_fuel <- NA
  } else if(fuel_pct >= dom_thrsh){
    dom_fuel <- "dominant"
  } else if(fuel_pct >= maj_thrsh & fuel_pct < dom_thrsh){
    dom_fuel <- "boiler_majority"
  } else if(fuel_pct < maj_thrsh){
    dom_fuel <- "boiler_minor"
  }
  return(dom_fuel)
}