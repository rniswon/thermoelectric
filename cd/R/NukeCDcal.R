#'@title NukeCDcal
#'@description condenser duty model execution for nuclear plants \cr \cr
#'Executed By: CondenserDutyModel_new.R \cr
#'@param gen_fuel_data data.frame from EIA-923 Monthly Generation and Fuel Consumption Time Series 
#'                     File, 2015 Final Revision
#'@return `cd_results` data.frame of monthly condenser duty values for nuclear plants by plant and 
#'                     nuclear unit id
#'@examples
#'NukeCDcal(sheet1_gen_fuel_data)

NukeCDcal <- function(gen_fuel_data){
  sheet1_nukes <- gen_fuel_data %>% subset(!is.na(Nuclear.Unit.Id) & Nuclear.Unit.Id!=".")
  
  nuke_fuel_heat <- sheet1_nukes %>% select(c("Plant.Code", "Nuclear.Unit.Id", "Reported.Prime.Mover",
                                              "Reported.Fuel.Type.Code"), contains("Tot_MMBtu"))
  
  nuke_netgen <- sheet1_nukes %>% select(c("Plant.Code", "Nuclear.Unit.Id", "Reported.Prime.Mover",
                                           "Reported.Fuel.Type.Code"), contains("Netgen"))
  
  nuke_cd <- full_join(nuke_fuel_heat, nuke_netgen)
  
  cd_vals <- calNukeCd(nuke_cd[5:16], nuke_cd[17:28])
  
  cd_results <- cbind(nuke_cd[1:4], cd_vals)
  
  cd_results <- cd_results %>% mutate(plant_unit = paste("gen", Plant.Code, Nuclear.Unit.Id, sep = "^"))
  
  return(cd_results)
}

