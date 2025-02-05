#'@title calNominalLoss
#'@description calculate nominal losses \cr \cr
#'Executed By: analyzeNominalLosses.R \cr
#'@param prime_mover Reported.Prime.Mover from fuel_heat_table.csv
#'@param fuel_heat annual fuel-specific fuel heat, and boiler total fuel heat at each boiler, flags 
#'                    dominant fuels, majority of boiler fuel, and fuels with zero consmption, assigns 
#'                    dominant fuel codes, output of the analyzeFuelHeat() function
#'@return `nominal_loss` data.frame containing monthly nominal losses
#'@examples
#'sapply(fuel_heat_df[, months], calNominalLoss, prime_mover = fuel_heat_df$Reported.Prime.Mover)


#Calculate nominal losses

calNominalLoss <- function(prime_mover, fuel_heat){
  nominal_loss <- ifelse(prime_mover != "ST", fuel_heat * 0.22, fuel_heat * 0.02 )
  return(nominal_loss)
}

