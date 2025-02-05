#'@title calSTCondenserDuty
#'@description calculates steam bogen condenser duty \cr \cr
#'Executed By: analyzeSteamCD.R \cr
#'@param steam_heat monthly steam heat by bogen, output of the assignBogen() function
#'@param netgen monthly net generation by bogen, output of the assignBogen() function
#'@param nominal_loss monthly nominal losses by bogen, output of the assignBogen()
#'@return `condenser_duty` data.frame containing monthly condenser duty for stream plants
#'@examples
#'calSTCondenserDuty(cd_cal_table_2[4:15], cd_cal_table_2[28:39], cd_cal_table_2[16:27])


#Function to calculate steam bogen condenser duty
calSTCondenserDuty <- function(steam_heat, netgen, nominal_loss){
  condenser_duty <- steam_heat - nominal_loss - netgen * 3.412
  names(condenser_duty) <- gsub("steam_heat_","CD_", names(condenser_duty))
  return(condenser_duty)
}

