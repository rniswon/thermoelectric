#'@title calHeatExports
#'@description calculates plant level monthly heat exports, for output file plant_exports.csv \cr \cr
#'Executed By: non_nuke_condenser_duty.R \cr
#'@param gen_fuel_data data.frame EIA-923 Monthly Generation and Fuel Consumption Time 
#'                               Series File, 2015 Final Revision, input file 2015_GenerationAndFuel.csv
#'@return `export_results` data.frame of monthly heat exports by plant
#'@examples
#'calHeatExports(sheet1_gen_fuel_data_df)

#Function calculates plant level monthly heat exports
calHeatExports <- function(gen_fuel_data){
  exports <- gen_fuel_data %>% 
    filter(Combined.Heat.And.Power.Plant != "N") %>% 
    select(c(Plant.Code, contains("Combined."), Reported.Prime.Mover,
             Reported.Fuel.Type.Code), contains("Tot_MMBtu."), contains("Elec_MMBtu."))
  
  non_exporters <- gen_fuel_data %>% 
    filter(Combined.Heat.And.Power.Plant == "N") %>% 
    select(c(Plant.Code, contains("Combined."), Reported.Prime.Mover,
             Reported.Fuel.Type.Code), contains("Tot_MMBtu."))
  
  non_exporters[5:16] <- 0
  
  heat_exported <- exports[5:16] - exports[17:28]
  heat_exported <- cbind(exports[1:4], heat_exported)
  export_results <- rbind(heat_exported, non_exporters)
  colnames(export_results) <- gsub("Tot_MMBtu.", "Exported_Heat_", colnames(export_results))
  
  export_results <- export_results %>%  
    group_by(Plant.Code) %>%
    summarize_at(names(.)[5:16], sum, na.rm = T) %>% 
    mutate(plant_total = rowSums(select(.,c(2:13)), na.rm = T))
  
return(export_results)
}



