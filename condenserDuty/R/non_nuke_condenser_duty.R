#'@title non_nuke_condenser_duty
#'@description condenser duty model execution for non nuclear plants. \cr \cr  
#'Outputs the following files:\itemize{\item fuel_heat_table.csv - annual fuel-specific fuel heat, and boiler total fuel heat at each boiler
#'                        \item fuel_heat_ST_only.csv - subset of fuel_heat_table.csv, with only Reported.Prime.Mover=="ST"
#'                        \item plant_exports.csv - monthly heat exports by plant
#'                        \item final_steam_heat.csv - monthly steam heat by plant and boiler
#'                        \item pos_netgen.csv - annual and montly process net generation by plant and generator
#'                        \item nominal_losses.csv - monthly summed nominal losses by plant and boiler
#'                        \item steam_bogen_cd.csv - monthly condenser duty for stream plants by bogen
#'                        \item ngcc_bogen_cd.csv - monthly condenser duty for general_mover = NGCC by bogen
#'                        \item bogen_fuel_heat.csv - monthly fuel heat by bogen
#'                        \item bogen_nominal_losses.csv - monthly nominal losses by bogen
#'                        \item bogen_netgen.csv - monthly net generation by bogen
#'                        \item bogen_steam_heat.csv - monthly steam heat by bogen
#'                        \item net_condenser_duty.csv - monthly net condenser duty by bogen non-nuclear plants
#'                        \item total_condenser_duty.csv - monthly total condenser duty by bogen non-nuclear plants} \cr
#'Executed By: CondenserDutyModel_new.R \cr
#'@param plantList vector of all plant codes from 2015, input file 2015_Plants.csv
#'@param boilerFuelData data.frame EIA-923 Monthly Boiler Fuel Consumption and Emissions Time 
#'                           Series File, 2015 Final Revision, input file 2015_BoilerFuelData.csv
#'@param boilerDesignData data.frame 2015 Form EIA-860 Data - Schedule 6C, 
#'                             'Boiler Information - Design Parameters', input file 2015_BoilerDesignInfo.csv
#'@param gen_fuel_data data.frame EIA-923 Monthly Generation and Fuel Consumption Time 
#'                               Series File, 2015 Final Revision, input file 2015_GenerationAndFuel.csv
#'@param generation.data data.frame EIA-923 Monthly Generating Unit Net Generation Time 
#'                               Series File, 2015 Final Revision, input file 2015_GenerationData.csv
#'@param dom_thrsh numeric threshold for dominant fuels
#'@param maj_thrsh numeric threshold for boiler majority fuel
#'@param fuel.codes Fuel generalization list defined as 
#'                  `list(biomass = c("AB",'BLQ','MSB','MSN','OBL','OBS','SLW','TDF','WDS'),
#'                        coal = c("ANT","BIT","LIG","PC","SUB","WC"),
#'                        gas = c("BFG","LFG","NG","OBG","OG","PG","SGC","SGP"),
#'                        oil = c("DFO","JF","KER","RFO","WO"),
#'                        other = c("OTH"))`
#'@param pub_efficiency list of published weighted boiler efficiencies defined as
#'                           pub_efficiency <- list("0.781596394425121" = "biomass",
#'                                                  "0.873790282218364" = "coal",
#'                                                  "0.861342667396523" = "gas",
#'                                                  "0.886779729944024" = "oil")
#'@param use_published_efficiencies TRUE/FALSE indicating whether or not to use published efficiencies
#'                                  default value TRUE
#'@param sheet3_key data.frame plant_boiler to bogen associations, input file sheet3_key.csv
#'@param sheet4_key data.frame plant_gen to bogen associations, input file sheet4_key.csv
#'@param combogencoo_key associates boilers-generators-cooling systems
#'@param combogencoo_cooling_type_df List of bogencoo's cooling types for 2015 and 2010
#'@param nuke_combogencoo_key
#'@param combogencoo_cooly_type_nukes
#'@param plantInfo
#'@param outputData_path character string indicating path for output files
#'@return `cd_with_exports` data.frame of CondenserDutyModel results by bogen for non-nuclear plants,
#'                          saved as output file ~/thermoelectric-water-consumption-models/CondenserDutyModel/Output/net_condenser_duty.csv
#'@export

non_nuke_condenser_duty <- function(plantList, boilerFuelData, boilerDesignData, gen_fuel_data, generation.data,
                                    dom_thrsh, maj_thrsh, fuel.codes, pub_efficiency,
                                    use_published_efficiencies = T, sheet3_key, sheet4_key,
                                    combogencoo_key, combogencoo_cooling_type_df,
                                    nuke_combogencoo_key,combogencoo_cooly_type_nukes, 
                                    plantInfo,
                                    outputData_path){
#Calculate fuel heat
fuel_heat_table <- analyzeFuelHeat(plantList, data=boilerFuelData, dom_thrsh, maj_thrsh,
                                   fuel.codes,sheet3_key,sheet4_key,combogencoo_key, combogencoo_cooling_type_df,
                                   page3=TRUE)

fuel_heat_table_page1<-analyzeFuelHeat(plantList, gen_fuel_data, dom_thrsh, maj_thrsh, 
                                       fuel.codes,sheet3_key,sheet4_key,combogencoo_key, combogencoo_cooling_type_df,
                                       page3=FALSE)


#get ST only fuelHeat
fuel_heat_ST_only <- fuel_heat_table %>% filter(Reported.Prime.Mover == "ST")
fuel_heat_ST_only_page1 <- fuel_heat_table_page1 %>% filter(Reported.Prime.Mover == "ST")

#Calculate heat exports
plant_exports <- calHeatExports(gen_fuel_data)
list2env(list(plant_exports = plant_exports), .GlobalEnv)

#calculate steam heat 
final_steam_heat <- SteamHeatanalysis(fuel_heat_ST_only, boilerDesignData, 
                                      pub_efficiency, use_published_efficiencies,
                                      page3=TRUE)
final_steam_heat_page1 <- SteamHeatanalysis(fuel_heat_ST_only_page1, boilerDesignData, 
                                      pub_efficiency, use_published_efficiencies,
                                      page3=FALSE)

#Process net generation
cd_net_generation <- processNetGeneration(generation.data, plantList, page4=TRUE)
cd_net_generation_page1 <- processNetGeneration(gen_fuel_data, plantList, page4=FALSE)

#Calculate nominal losses
nominal_losses <- analyzeNominalLosses(fuel_heat_table,page3=T)
nominal_losses_page1<-analyzeNominalLosses(fuel_heat_table_page1,page3=F)

#Assign Bogens
data_by_bogen <- assignBogen(nominal_losses, nominal_losses_page1,
                             cd_net_generation$pos_netgen, cd_net_generation_page1$pos_netgen,
                             fuel_heat_table, fuel_heat_table_page1,
                             final_steam_heat,final_steam_heat_page1,
                             sheet3_key, sheet4_key)

postProcessVars<-combinePostProcessVars(plantDom,plantDom_page1,bogen_netgen,bogen_fuel_heat,gen_fuel_data,
                                                  combogencoo_key, combogencoo_cooling_type_df,combogencoo_cooly_type_nukes,sheet4_key,
                                                  nuke_combogencoo_key,plantInfo,plantList)
assign("postProcessVars",postProcessVars,envir = .GlobalEnv)
write.csv(postProcessVars,file=paste0(outputData_path,"postProcessVars.csv"),row.names = F)

#Calculate Bogen condenser duty
#st_bogen_cd <- analyzeSteamCD(bogen_netgen, bogen_steam_heat, bogen_nom_loss)
st_bogen_cd <- analyzeSteamCD(data_by_bogen$bogen_netgen, data_by_bogen$bogen_steam_heat, data_by_bogen$bogen_nom_loss)
st_bogen_cd$general_mover <- "ST"

#ngcc_bogen_cd <- analyzeNGCCCD(bogen_netgen, bogen_fuel_heat, bogen_nom_loss)
ngcc_bogen_cd <- analyzeNGCCCD(data_by_bogen$bogen_netgen, data_by_bogen$bogen_fuel_heat, data_by_bogen$bogen_nom_loss)
ngcc_bogen_cd$general_mover <- "NGCC"

if(is.na(nrow(st_bogen_cd))) {stop("Problem with stean condesner duty calculations.")}
if(is.na(nrow(ngcc_bogen_cd))) {stop("Problem with ngcc condesner duty calculations.")}

non_nuke_cd <- rbind(st_bogen_cd, ngcc_bogen_cd)


if(is.na(nrow(non_nuke_cd))) {stop("Problem with combined non-nuke condenser duty table.")}

#Process heat exports
cd_with_exports <- processExports(plantList, plant_exports, non_nuke_cd,
                                  sheet4_key, combogencoo_key, combogencoo_cooling_type_df,
                                  outputData_path)


#aggregate intermediate values
assign("data_by_bogen",data_by_bogen,envir = .GlobalEnv)

#Write non-nuke ouput files
write.csv(fuel_heat_table, paste0(outputData_path,'/fuel_heat_table.csv'), row.names = F)
write.csv(fuel_heat_ST_only, paste0(outputData_path,'/fuel_heat_ST_only.csv'), row.names = F)
write.csv(plant_exports, paste0(outputData_path,'/plant_exports.csv'), row.names = F)
write.csv(final_steam_heat, paste0(outputData_path,'/final_steam_heat.csv'), row.names = F)
write.csv(cd_net_generation$pos_netgen, paste0(outputData_path,'/pos_netgen.csv'), row.names = F)
write.csv(nominal_losses, paste0(outputData_path,'/nominal_losses.csv'), row.names = F)
write.csv(st_bogen_cd, paste0(outputData_path,'/steam_bogen_cd.csv'), row.names = F)
write.csv(ngcc_bogen_cd, paste0(outputData_path,'/ngcc_bogen_cd.csv'), row.names = F)
write.csv(data_by_bogen$bogen_fuel_heat, paste0(outputData_path,'/bogen_fuel_heat.csv'), row.names = F)
write.csv(data_by_bogen$bogen_nom_loss, paste0(outputData_path,'/bogen_nominal_losses.csv'), row.names = F)
write.csv(data_by_bogen$bogen_netgen, paste0(outputData_path,'/bogen_netgen.csv'), row.names = F)
write.csv(data_by_bogen$bogen_steam_heat, paste0(outputData_path,'/bogen_steam_heat.csv'), row.names = F)
write.csv(cd_with_exports, paste0(outputData_path,'/net_condenser_duty.csv'), row.names = F)
write.csv(non_nuke_cd, paste0(outputData_path,'/total_condenser_duty.csv'), row.names = F)

return(cd_with_exports)
}
