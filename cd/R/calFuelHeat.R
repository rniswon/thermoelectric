#' @title calFuelHeat
#' @author Amy Galanter
#' @author Andrew Halper
#' @description Calculate fuel heat by quantity consumed * heat content
#'              for fuel (MMbtu/Unit)
#' @param quant_consumed quantity of heat consumed values from EIA-923
#'                       Monthly Boiler Fuel Consumption and Emissions
#'                       Time Series File, 2015 Final Revision, input
#'                       file `2015_BoilerFuelData.csv`.
#' @param unit_heat heat content for fuel (MMbtu/Unit) values from
#'                  data.frame EIA-923 Monthly Boiler Fuel Consumption
#'                  and Emissions Time Series File, 2015 Final
#'                  Revision, input file `2015_BoilerFuelData.csv`.
#' @return `fuel_heat` annual fuel-specific fuel heat, and boiler
#'                     total fuel heat at each boiler
#' @examples as.data.frame(
#'             mapply(calFuelHeat,
#'                    boiler_fuel_data[(16:27)],
#'                    boiler_fuel_data[28:39]),
#'                    stringsAsFactors = F)
#'           )

# Calculates fuel heat values
calFuelHeat <- function(quant_consumed, unit_heat) {
  fuel_heat <- quant_consumed * unit_heat
  return(fuel_heat)
}
