#'@title runCondenserDuty
#'@description This model calculates condenser duty at nuclear and non-nuclear plants modeled 
#'             for water use estimation. Condenser duty calculations are based on data from the 
#'             2015 editions of the EIA860 and EIA923 databases on net power generation and fuel 
#'             consumption at power plants, as well as associations for boiler, generators,and 
#'             cooling systems. Citations for the EIA reports are included below. \cr \cr
#'Executed By: ThermoElectricWaterConsumptionModels.R \cr
#'@param inputData_path character string indicating path to input files
#'@param outputData_path character string indicating path for output files
#'@examples
#'runCondenserDuty(inputData_path, outputData_path)

####Non-nucear and Nuclear Condenser Duty Model####
#Date: August 13, 2019
#AUthor:Victor Roland, Hydrologist Lower Mississippi-Gulf Water Science Center

########################################################################################
#Description: This model calculates condenser duty at nuclear and non-nuclear          #
#plants modeled for water use estimation. Condenser duty calculations are based        #
#on data from the 2015 editions of the EIA860 and EIA923 databases on net power        #
#generation and fuel onsumption at power plants, as well as associations for boiler,   #
#generators,and cooling systems. Citations for the EIA reports are included below.     #
#                                                                                      #
#References                                                                            #
#                                                                                      #
#                                                                                      #  
#                                                                                      #
#                                                                                      #
#                                                                                      #
#                                                                                      #
#Instructions: (1) To run the condenser duty models unzip and store the condesner duty #
#directory in the desired directory on your computer, this locaion will be the working #
#directory. It is important to maintain the directory structure within the condenser   #
#duty directory as the required input files are retrieved from                         #
#predetermined locations within the directory structure.                               #
#(2) Change in line xx change the location of the working directory to the path to the #
#location of the working directory estamblished in step one.                           #
#(3) To run the model select all lines of the code by pressing ctrl+a together.        #
#(4) After selectig all lines, run the model by pressing ctrl+enter to run the model   #
#(5) Model output files are stored in the R environment as 'steam.ngcc.condenser.duty' #
#and 'nuclear.condenser.duty'.                                                         #
#(6) CSV files of model output are located in the Output directory of the Condeser Duty#
#directory.                                                                            #
#                                                                                      #
#                                                                                      #
#                                                                                      #
#                                                                                      #
########################################################################################


# 
# #Load model functions#
# loadCondenserDutyFunctions("condenser_duty_funs")
# loadCondenserDutyFunctions("import_funs")
# loadCondenserDutyFunctions("data_prep_funs")
# loadCondenserDutyFunctions("plant_network_funs")

runCondenserDuty<-function(inputData_path, outputData_path, inputData.list, bogen.out.list){
  
  #unpack input data
  unPackList(lists = list(inputData.list = inputData.list,
                          bogen.out.list = bogen.out.list),
             parentObj = list(NA, NA))
  
  #format plantlist
  # plantList$Plant.Code<-sapply(plantList$plant_bo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))
  # plantList<-as.data.frame(plantList[,names(plantList)=="Plant.Code"])
  # names(plantList)<-"Plant.Code"
  

  # sheet1_gen_fuel_data<-gen_fuel_data
  # sheet4_gen_fuel_data<-generation.data
  # boiler_design_data<-boilerDesignData
  # boiler_fuel_data<-boilerFuelData
  # modeled_plants<-plantList
  # combogencoo_key<-bogencoo.key
  
#set here location
#getHere()

#Import model input data#

# sheet1_gen_fuel_data <- import_sheet1_gen_and_fuel_data(paste0(inputData_path,'/2015_GenerationAndFuel.csv'))
# sheet4_gen_fuel_data <- import_sheet4_gen_and_fuel_data(paste0(inputData_path,'/2015_GenerationData.csv'))
#boiler_design_data <- import_boiler_design_data(paste0(inputData_path,'/2015_BoilerDesignInfo.csv'))
#boiler_fuel_data <- import_boiler_fuel_data(paste0(inputData_path,'/2015_BoilerFuelData.csv'))
# page4_bogen_key <- read.csv(paste0(inputData_path,'/sheet4_key.csv'),header = T,stringsAsFactors = F)
# page3_bogen_key <- read.csv(paste0(inputData_path,'/sheet3_key.csv'),header = T,stringsAsFactors = F)





#Import environmental variables
#Import wet and dry bulb temperature data nearest 3 weather stations
#dbwb_data<-read.csv(paste0(inputData_path,'/3sta_DBWB.csv'), header = T, stringsAsFactors = F)
####Import wind speed data####
#ws_data <- read.csv(paste0(inputData_path,'/3sta_WS.csv'), header=T, stringsAsFactors = F)
# ####Import water temperature data####
#wt_data <- read.csv(paste0(inputData_path,'/2015_WT_Predictions.csv'), header=T, skip=3)


#Import cooling system assignments and type data for nuke plants
#combogencoo_cooly_type <- read.csv(paste0(inputData_path,'/combogencoo_cool_type.csv'), header=T, skip=3, stringsAsFactors = F)
#alternate name combogencoo_key_df
#combogencoo_key <- read.csv(paste0(inputData_path,'/combogencoo_key.csv'), header=T, stringsAsFactors = F)
nuke_combogencoo_key <- read.csv(paste0(inputData_path,'/nuke_combogencoo_key.csv'), header=T, stringsAsFactors = F)
#nuke_cooling_type_key <- read.csv(paste0(inputData_path,'/nuke_cooling_type_key.csv'), header=T, stringsAsFactors = F)


#Import list of plants being modeled
#modeled_plants <- read.csv(paste0(inputData_path,'/2015_Plants.csv'), header = T, stringsAsFactors = F)

#Fuel generalization list
fuel.codes <- list(
  biomass = c("AB",'BLQ','MSB','MSN','OBL','OBS','SLW','TDF','WDS'),
  coal = c("ANT","BIT","LIG","PC","SUB","WC"),
  gas = c("BFG","LFG","NG","OBG","OG","PG","SGC","SGP"),
  oil = c("DFO","JF","KER","RFO","WO"),
  other = c("OTH"))

#publiched wieghted boiler efficiencies
pub_efficiency <- list(
  "0.781596394425121" = "biomass",
  "0.873790282218364" = "coal",
  "0.861342667396523" = "gas",
  "0.886779729944024" = "oil",
  "0.872816614"="other")

sheet3_key$Reported.Prime.Mover_page1<-ifelse(regexpr("NGCC",sheet3_key$bogen)>0 & is.na(sheet3_key$Reported.Prime.Mover_page1),
                                              "CA",ifelse(regexpr("ST",sheet3_key$bogen)>0 & is.na(sheet3_key$Reported.Prime.Mover_page1),
                                                          "ST",sheet3_key$Reported.Prime.Mover_page1))
sheet4_key$Reported.Prime.Mover_page1<-ifelse(regexpr("NGCC",sheet4_key$bogen)>0 & is.na(sheet4_key$Reported.Prime.Mover_page1),
                                              "CA",ifelse(regexpr("ST",sheet4_key$bogen)>0 & is.na(sheet4_key$Reported.Prime.Mover_page1),
                                                          "ST",sheet4_key$Reported.Prime.Mover_page1))
####Run Condenser Duty models####
#Run non-nuclear condenser duty model. 
#Function arguments: plant_list, boiler_fuel_data, boiler_design_data, EIA 923 sheet1 gen fuel data, 
#EIA 923 sheet 4 generation and fuel data, threshold for dominant fuels, threshold for boiler majority fuel,
#list of published weighted mean fuel specific efficiency, use published efficiency values (T/F) default is true,
#boiler bogen key, generator bogen key.
non_nuke_CD_summary <- non_nuke_condenser_duty(plantList$Plant.Code, boilerFuelData, boilerDesignData, gen_fuel_data, 
                        generation.data, dom_thrsh=0.8, maj_thrsh=0.5, fuel.codes, pub_efficiency,
                        use_published_efficiencies = T, sheet3_key, sheet4_key,
                        bogencoo.key, combogencoo_cooly_type,
                        nuke_combogencoo_key,combogencoo_cooly_type_nukes,
                        plantInfo,
                        outputData_path)
# print("non_nuke_CD_summary")
#print(non_nuke_CD_summary[non_nuke_CD_summary$Plant.Code %in% c(2098,7296),]) #missing 7296

#Nuclear condenser duty model
nuke_CD_summary <- NukeCDcal(gen_fuel_data)


####Combine Steam Condenser Duties, NGCC Condenser Duties, Nuclear Conenser Duties, and Cooling System Data
combogencoo_cooly_type<-rbind.fill(combogencoo_cooly_type,combogencoo_cooly_type_nukes)
duties_with_cooling <- assignCooling(sheet4_key, bogencoo.key, combogencoo_cooly_type, nuke_combogencoo_key, 
              non_nuke_CD_summary, nuke_CD_summary, outputData_path)

list2env(duties_with_cooling, .GlobalEnv)


#thermEff<-thermalEfficiencyByBogen(bogen_fuel_heat,bogen_netgen, gen_fuel_data)
#write.csv(thermEff,file=paste0(outputData_path,"/thermalEfficiencyByBogen.csv"),row.names=F)

eia_year<-unique(na.omit(sheet3_key$YEAR))
compileCD<-compileCDintermediate(bogen_fuel_heat,bogen_netgen,bogen_nom_loss,bogen_steam_heat, gen_fuel_data,
                                non_nuke_CD_summary,nuke_CD_summary,plantList$Plant.Code,plant_exports)
write.csv(compileCD,file=paste0(outputData_path,"/compileCDintermediate",eia_year,".csv"),row.names=F)
# print("duties_with_cooling")
# print("non_nuke_duties_w_cooling")
# print(subset(duties_with_cooling$non_nuke_duties_w_cooling,Plant.Code==2098)) #2098 missing

###Collect and process environmental variables####
#The environmental variables used by the TOWER FEWSR models are: wet bulb (WB) and dry bulb (DB) temperatures, 
#wind speed (WS), and natural water temperature (WT).
#All of the environmental data are exported to a list, that is exported to the output directory, and combined
#with condenser duty data before being used as input to the TOWER and FEWSR models.

#Environmental data function call
#environmental.variables <- getEnvVars(modeled_plants$Plant.Code, dbwb_data, ws_data, wt_data,
#                                      outputData_path, getEnvVars=T)

####Merge Condenser duties with environmental variables.####
#The following table is the raw input for the tower and FEWSR models.
#final_plant_output <- mergeCD_ENV(no_dupe_final_CD_w_nukes_cooling, environmental.variables,outputData_path)

}


