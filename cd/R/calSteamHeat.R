#' @title calSteamHeat
#' @author Amy Galanter
#' @author Andrew Halper
#' @description Calculates monthly steam heat from fuel heat and
#'              boiler efficiency.
#' @param fuel_heat Numeric; fuel heat subset of
#'                  `fuel_heat_table.csv`, with only
#'                  Reported.Prime.Mover == "ST".
#' @param boiler_efficiency Numeric; boiler efficiency from 2015 Form
#'                          EIA-860 Data - Schedule 6C, "Boiler
#'                          Information - Design Parameters", input
#'                          file `2015_BoilerDesignInfo.csv`.
#' @return `steam_heat` Numeric; steam heat from fuel heat and boiler
#'                      efficiency.
#' @examples as.data.frame(
#'             apply(steam_heat_rpt_eff[5:16],
#'                   2,
#'                   calSteamHeat,
#'                   boiler_efficiency = steam_heat_rpt_eff$boiler_efficiency)
#'           )

calSteamHeat <- function(fuel_heat, boiler_efficiency) {
  steam_heat <- fuel_heat * boiler_efficiency
  return(steam_heat)
}
