#'@title mergeCD_ENV
#'@description combines condenser duty output and environmental data and formatted for input into 
#'             the TOWER and FEWSR models \cr \cr
#'Outputs the following files: rawInput_FEWSR_TOWER.csv - condenser duty output and environmental data 
#'                                                        and formatted for input into the TOWER and FEWSR models
#'Executed By: CondenserDutyModel_new.R \cr
#'@param combined_duties_df monthly condender duty for all plant types by plant and cooling type
#'@param environmental_variables_df monthly environmental variables used by the TOWER and FEWSR models
#'@param outputData_path character string indicating path for output files
#'@return `duties_envVar_final` data.frame that combines condenser duty output and environmental data 
#'                              and formatted for input into the TOWER and FEWSR models
#'@examples
#'mergeCD_ENV(no_dupe_final_CD_w_nukes_cooling, environmental.variables)


mergeCD_ENV<-function(combined_duties_df, environmental_variables_df, outputData_path){
  modeld_env_vars <- environmental_variables_df %>% filter(Plant.Code %in% combined_duties_df$Plant.Code)
  duties_envVar<-full_join(combined_duties_df, modeld_env_vars)
  

 # duties_envVar <- duties_envVar[complete.cases(duties_envVar),]
  #duties_envVar_final <- duties_envVar[!duplicated(duties_envVar[2:13]),]
  duties_envVar_final <- duties_envVar[!duplicated(duties_envVar[1:15]),]#changed 11.16.20 to include percent allocation and cooling

  write.csv(duties_envVar_final, paste0(outputData_path,'/rawInput_FEWSR_TOWER.csv'), row.names = F)
  return(duties_envVar_final)
}

