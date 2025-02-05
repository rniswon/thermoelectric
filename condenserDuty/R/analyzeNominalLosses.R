#'@title analyzeNominalLosses
#'@description Calculate monthly summed nominal losses by plant and boiler, for output file nominal_losses.csv \cr \cr
#'Executed By: non_nuke_condenser_duty.R \cr
#'@param fuel_heat_df annual fuel-specific fuel heat, and boiler total fuel heat at each boiler, flags 
#'                    dominant fuels, majority of boiler fuel, and fuels with zero consmption, assigns 
#'                    dominant fuel codes, output of the analyzeFuelHeat() function
#'@param page3 logical indicating whether page 3 boilerFuelData is used as input, if FALSE page 1 gen_fuel_data is used                       
#'@return `nominal_losses` data.frame monthly summed nominal losses by plant and boiler
#'@export

#Calculate and sum nominal losses
analyzeNominalLosses <- function(fuel_heat_df,page3){
  months <- c("january", "february", "march", "april", "may", "june", "july",
              "august", "september", "october", "november", "december")
  
  nominal_loss_vals <- sapply(fuel_heat_df[, months], calNominalLoss, 
                              prime_mover = fuel_heat_df$Reported.Prime.Mover)
  if (is.null(nrow(nominal_loss_vals))){
    nominal_loss_vals<-as.data.frame(t(nominal_loss_vals))
  }


  if(page3){
    indexes<-c(1:4)
  }else{
    indexes<-c(1:3)
  }
  
nominal_losses <- cbind.data.frame(fuel_heat_df[indexes], nominal_loss_vals)
if(page3){
  nominal_losses <- nominal_losses %>% mutate(total = rowSums(select(., months), na.rm = T)) %>% 
    mutate(plant_bo = paste(Plant.Code, Boiler.ID, sep = "^")) 
}else{
nominal_losses <- nominal_losses %>% mutate(total = rowSums(select(., months), na.rm = T))
}
return(nominal_losses)
}
