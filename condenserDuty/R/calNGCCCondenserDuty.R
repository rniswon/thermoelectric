#'@title calNGCCCondenserDuty
#'@description calculate ngcc condenser duty \cr \cr
#'Executed By: analyzeNGCCCD.R \cr
#'@param fuel_heat monthly fuel heat by bogen, output from assignBogen() function
#'@param netgen monthly net generation by bogen, output of the assignBogen() function
#'@param nominal_loss monthly nominal losses by bogen, output of the assignBogen()
#'@return `condenser_duty` data.frame containing monthly condenser duty for 
#'                         general_mover = NGCC by bogen
#'@export

#Function to calculate ngcc condenser duty
calNGCCCondenserDuty <- function(fuel_heat, netgen, nominal_loss){
    condenser_duty <- fuel_heat - nominal_loss - netgen * 3.412
    names(condenser_duty) <- gsub("fuel_heat_","CD_", names(condenser_duty))
  return(condenser_duty)
}


