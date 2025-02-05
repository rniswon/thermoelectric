#'@title calNukeCd
#'@description calculate condenser duty for nuclear plants \cr \cr
#'Executed By: \itemize{\item NukeCDcal.R
#'                        \item NuclearCD_funs.R} \cr
#'@param mo_fuel_heat data.frame from EIA-923 Monthly Generation and Fuel Consumption Time Series 
#'                     File, 2015 Final Revision containing monthly fuel heat
#'@param net_gen data.frame from EIA-923 Monthly Generation and Fuel Consumption Time Series 
#'                     File, 2015 Final Revision containing monthly net generation
#'@param percent_losses numeric value setting percent heat loss, default value 0.02
#'@return `condenser_duty` data.frame of monthly condenser duty values for nuclear plants by plant and 
#'                     nuclear unit id
#'@examples
#'calNukeCd(nuke_cd[5:16], nuke_cd[17:28])

#Nuclear CD function
calNukeCd <- function(mo_fuel_heat, net_gen, percent_losses=0.02){
  if(nrow(mo_fuel_heat) != nrow(net_gen)){stop("'mo_fuel_heat', and 'net_gen' must have the same dimensions.")}
  net_gen_heat <- net_gen * 3.412
  monthly_losses <- mo_fuel_heat * percent_losses
  condenser_duty <- mo_fuel_heat - net_gen_heat - monthly_losses
  colnames(condenser_duty) = gsub("Tot_MMBtu.", "CD_", colnames(condenser_duty))
  return(condenser_duty)
}