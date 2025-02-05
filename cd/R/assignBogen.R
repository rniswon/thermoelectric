#'@title assignBogen
#'@description calculates monthly fuel heat by bogen, assigns bogen values to the variable 
#'             data frames used in the condenser duty calculation. \cr \cr
#'Executed By: non_nuke_condenser_duty.R \cr
#'@param nominal_losses Page3 monthly summed nominal losses by plant and boiler, output from the 
#'                analyzeNominalLosses() function
#'@param nominal_losses_page1 Page1 monthly summed nominal losses by plant and Reported.Prime.Mover, 
#'                            output from the analyzeNominalLosses() function
#'@param netgen Page4 net generation data with negatives replaced with zeros and generators 
#'              with negative annual net generation removed
#'@param netgen_page1 Page1 net generation data with negatives replaced with zeros and generators 
#'              with negative annual net generation removed              
#'@param fuel_heat_table Page3 annual fuel-specific fuel heat, and boiler total fuel heat at each boiler, 
#'                        flags dominant fuels, majority of boiler fuel, and fuels with zero 
#'                        consmption, assigns dominant fuel codes, output of the analyzeFuelHeat() 
#'                        function
#'@param fuel_heat_table_page1 Page1 annual fuel-specific fuel heat, and RPM total fuel heat at each RPM, 
#'                        flags dominant fuels, majority of RPM fuel, and fuels with zero 
#'                        consmption, assigns dominant fuel codes, output of the analyzeFuelHeat() 
#'                        function
#'@param final_steam_heat Page3 monthly steam heat by plant and boiler, SteamHeatanalysis() function output
#'@param final_steam_heat_page1 Page1 monthly steam heat by plant and RPM, SteamHeatanalysis() function output
#'@param sheet3_key Bogen associations by Boiler, executeBogenAssoc() function output
#'@param sheet4_key Bogen associations by Generator, executeBogenAssoc() function output
#'@return `results` list containing bogen_fuel_heat, bogen_nom_loss, bogen_netgen, bogen_steam_heat
#'@examples
#'assignBogen(nominal_losses, nominal_losses_page1,
#'            cd_net_generation$pos_netgen, cd_net_generation_page1$pos_netgen,
#'            fuel_heat_table, fuel_heat_table_page1,
#'            final_steam_heat,final_steam_heat_page1,
#'            sheet3_key, sheet4_key)

#Function assigns bogen values to the variable data frames used in the condenser duty calculation.

assignBogen <- function(nominal_losses, nominal_losses_page1,
                        netgen, netgen_page1,
                        fuel_heat_table, fuel_heat_table_page1,
                        final_steam_heat,final_steam_heat_page1,
                        sheet3_key, sheet4_key){
  suppressWarnings(remove(tbl_bogen))
  suppressWarnings(remove(tbl_bogen2))
  suppressWarnings(remove(tbl_bogen3))
  select_RPM=c("CA", "CS", "CT", "ST",NA)

  
  sheet3_key$plant_bo<-paste(sheet3_key$Plant.Code,sheet3_key$Boiler.ID,sep="^")
  
#add bogen to tables
  bogen_fuel_heat<-joinPage1.3.4(page3.4=fuel_heat_table,page1=fuel_heat_table_page1,
                                 key=sheet3_key,joinCols=c("plant_bo","plant_bo_bf.923"),select_RPM)
  
  bogen_nom_loss<-joinPage1.3.4(page3.4=nominal_losses,page1=nominal_losses_page1,
                                 key=sheet3_key,joinCols=c("plant_bo","plant_bo_bf.923"),select_RPM)
  
  bogen_netgen<-joinPage1.3.4(page3.4=netgen,page1=netgen_page1,
                                key=sheet4_key,joinCols=c("plant_gen","plant_gen.923"),select_RPM)
  
  bogen_steam_heat<-joinPage1.3.4(page3.4=final_steam_heat,page1=final_steam_heat_page1,
                              key=sheet3_key,joinCols=c("plant_bo","plant_bo_bf.923"),select_RPM)
  
  
  results <- named.list(bogen_fuel_heat,
                        bogen_nom_loss,
                        bogen_netgen,
                        bogen_steam_heat)
  
  list2env(results, .GlobalEnv)
  return(results)
}

