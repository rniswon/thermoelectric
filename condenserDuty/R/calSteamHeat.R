#'@title calSteamHeat
#'@description calculates monthly steam heat from fuel heat and boiler efficiency \cr \cr
#'Executed By: \itemize{\item estEfficiency.R 
#'                        \item steamHeatAnalysis.R} \cr
#'@param fuel_heat numeric fuel heat subset of fuel_heat_table.csv, with only 
#'                    Reported.Prime.Mover=="ST"
#'@param boiler_efficiency numeric boiler efficiency from 2015 Form EIA-860 Data - Schedule 6C, 
#'                             'Boiler Information - Design Parameters', input file 2015_BoilerDesignInfo.csv
#'@return `steam_heat` numeric steam heat from fuel heat and boiler efficiency
#'@export

calSteamHeat <- function(fuel_heat, boiler_efficiency){
  steam_heat <- fuel_heat * boiler_efficiency
  return(steam_heat)
}