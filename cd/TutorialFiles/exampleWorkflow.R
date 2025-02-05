################################################
## Section 1 : Loading R packages And Set Paths
################################################
#install.packages("devtools")

#path to Thermoelectric Repository
repoPath<-"C:/Users/lgorman/OneDrive - DOI/Repos/Thermoelectric/"

#load CondenserDuty package and dependencies
devtools::install_deps(paste0(repoPath,"/cd"), upgrade = "never")
devtools::load_all(paste0(repoPath,"/cd"),recompile = FALSE)

#load plant_association package and dependencies
devtools::install_deps(paste0(repoPath,"/plant_association"), upgrade = "never")
devtools::load_all(paste0(repoPath,"/plant_association"),recompile = FALSE)

# #install the remotes package
# install.packages("remotes")
# 
# #install smwrBase from gitlab
# remotes::install_gitlab("water/analysis-tools/smwrBase",
#                         host = "code.usgs.gov")

#directory where raw EIA pulled from the web will be saved
EIAsaveOut<-"D:/"

#path to EIA crosswalks
#found on Teams WaterUseModels/Thermoelectric/AssociationKeys/EIA_crosswalks_complete
EIAcrossPath<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/EIA_crosswalks_complete/"

#path to CondenserDuty input data files
inputData_path<-paste0(repoPath,"/cd/TutorialFiles/")

#path to save bogen association output (path must exist)
pathWrite<-"D:/bogenAssociationOutput"

#path to save CD model results (path must exist)
outputData_path<-"D:/CDresults/"

#specify which year's data to pull
eia_year<-2015


################################################
## Section 2 : Pull EIA data from Web
################################################
#path to save EIA data for given year
#directory inside this directory named with the eia_year (i.e. 2015)
EIAyearSave<-paste0(EIAsaveOut,"EIA_",eia_year)

#run import module function eia_webpull
#eia_webpull(eia_year,EIAyearSave)

#path to crosswalk being used to start
path_InputData_Metafile<-paste0(repoPath,"cd/TutorialFiles/UserControlCrosswalk2015.xlsx")

#path to EIA raw data
#path should be to directory with extracted data from zip files (created by eia_webpull)
path_EIAInputData<-paste(EIAyearSave,eia_year,sep="/")

#if saving formatted tables as csv set outputCSV<-TRUE otherwise outpuCSV<-FALSE
outputCSV<-TRUE
#path to save formatted tables in csv form (set path_outputCSV<-NA if outputCSV<-FALSE)
path_outputCSV<-EIAsaveOut

#execute import_EIAData (repeat as necessary)
inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,path_outputCSV)

#import master plant list  
plantList<-openxlsx::read.xlsx(paste0(repoPath,"cd/TutorialFiles/TE_MPL_allyears.xlsx"),
                               sheet="plant years vert")

#format Plant.Code column  
plantList$Plant.Code<-plantList$EIA_PLANT_ID

#select only plants for the year being analyzed
plantList<-plantList[plantList$year==eia_year,]

#add plantList to inputData.list (MUST be called plantList for execution to continue)
inputData.list$plantList<-plantList


################################################
## Section 3 : Generate Bogen Associations
################################################
#run bogen associations
#select_RPM argument is used to filter input data so that only Reported.Prime.Movers in 
#select_RPM are considered
bogen.out.list<-executeBogenAssoc(inputData_path,pathWrite,inputData.list,select_RPM=c("CA", "CS", "CT", "ST",NA), eia_year)

#saving the bogen.out.list for future use is highly recommended
#Sys.Date() will attached the date that the file is saved to the file name
save(bogen.out.list,file = paste0(pathWrite,"bogen.out.list.",eia_year,"_",Sys.Date()))

#define list of output tables to include
outTables<-list(auto_sheet3_key=bogen.out.list$sheet3_key, #required
                auto_sheet3_flags=bogen.out.list$binary_Auto3,
                auto_sheet4_key=bogen.out.list$sheet4_key, #required
                auto_sheet4_flags=bogen.out.list$binary_Auto4,
                EIA_associationTable = inputData.list$bogen,
                generator.data.860 = inputData.list$gen_860,
                retiredGenerators.860 = inputData.list$retiredGenerators,
                Boiler_InfoDesign_Parameters.860 = inputData.list$boilerDesignData,
                boilerFuelData_923 = inputData.list$boilerFuelData,
                generation.data.923 = inputData.list$generation.data,
                page1GenFuelData.923 = inputData.list$gen_fuel_data,
                FuelHeatByLine = bogen.out.list$bf.923.ungrouped,
                NetGenByLine = bogen.out.list$generation.data.line124,
                FuelHeatByBoilerID = bogen.out.list$bfd.gb,
                NetGenByGeneratorID = bogen.out.list$generation.data.gb,
                FuelHeatByBogen = bogen.out.list$sumFuelHeatByBogen,
                NetGenByBogen = bogen.out.list$sumNetGenByBogen,
                ThermalEfficiencyByBogen = bogen.out.list$thermalEffByBogen)

#create a compiled file
compilePlantInfo_excel(outputPath=paste0(outputData_path,"compiled_manualBogens_",eia_year,".xlsx"),
                       plantList=unique(bogen.out.list$manualPlants$Plant.Code),
                       outTables = outTables,
                       shiftCols=1,
                       inputData_path,
                       fileComment = "All Plants flagged with manualFlags")

#apply first file of edits
new.bogen.list<-manualBogenReplace(outputData_path=inputData_path,
                                   fileName="compiled_manualBogens_2015_1stEdit.xlsx",
                                   data.list=bogen.out.list,        
                                   replaceTables=c("sheet3_key","sheet4_key"),lastReplace=FALSE)

#replace tables in bogen.out.list with manualedits
bogen.out.list$sheet3_key<-new.bogen.list$sheet3_key
bogen.out.list$sheet4_key<-new.bogen.list$sheet4_key

#apply final file of edits 
#this example shows only 2 edited files, but multiple can exist
#repeat lines above for every file that is not the final file
new.bogen.list<-manualBogenReplace(outputData_path=inputData_path,
                                   fileName="compiled_manualBogens_2015_2ndEdit.xlsx",
                                   data.list=bogen.out.list,        
                                   replaceTables=c("sheet3_key","sheet4_key"),lastReplace=TRUE)

#remove plants with missing bogens
new.bogen.list$sheet3_key<-new.bogen.list$sheet3_key %>% filter(!is.na(bogen))
new.bogen.list$sheet4_key<-new.bogen.list$sheet4_key %>% filter(!is.na(bogen))

#replace tables in bogen.out.list with manualedits
bogen.out.list$sheet3_key<-new.bogen.list$sheet3_key
bogen.out.list$sheet4_key<-new.bogen.list$sheet4_key

#save the edited bogen.out.list for future use
save(bogen.out.list,file = paste0(pathWrite,"bogen.out.list.",eia_year,"_ManualComplete"))



################################################
## Section 4 : Generate Bogencoo Associations
################################################

#execute cooling system association
bogencoo.key.list<-bogencoo.key.list<-bocoo.associate(inputData.list,
                                                      bogen.out.list,
                                                      inputData_path)

#save the bogencoo.key and combogencoo_cooly_type_nukes to the `bogen.out.list` for future use
bogen.out.list$bogencoo.key<-bogencoo.key.list$bogencoo.key
bogen.out.list$combogencoo_cooly_type_nukes<-bogencoo.key.list$combogencoo_cooly_type_nukes

#select output tables for the compiled file
outTables<-list(auto_bogencoo.key = bogencoo.key.list$bogencoo.key, #required
                combogencoo_cool_type_key = bogencoo.key.list$combogencoo_cooly_type, #required
                EIA_coolingSystemInfo = inputData.list$cooling,
                EIA_BocooAssoc = inputData.list$bocoo,
                auto_sheet3_flags=bogen.out.list$binary_Auto3,
                auto_sheet3_key=bogen.out.list$sheet3_key, # do not make manual bogen edits at this step
                auto_sheet4_flags=bogen.out.list$binary_Auto4,
                auto_sheet4_key=bogen.out.list$sheet4_key, # do not make manual bogen edits at this step
                EIA_associationTable = inputData.list$bogen,
                generator.data.860 = inputData.list$gen_860,
                retiredGenerators.860 = inputData.list$retiredGenerators,
                Boiler_InfoDesign_Parameters.860 = inputData.list$boilerDesignData,
                boilerFuelData_923 = inputData.list$boilerFuelData,
                generation.data.923 = inputData.list$generation.data,
                FuelHeatByLine = bogen.out.list$bf.923.ungrouped,
                NetGenByLine = bogen.out.list$generation.data.line124,
                FuelHeatByBoilerID = bogen.out.list$bfd.gb,
                NetGenByGeneratorID = bogen.out.list$generation.data.gb,
                FuelHeatByBogen = bogen.out.list$sumFuelHeatByBogen,
                NetGenByBogen = bogen.out.list$sumNetGenByBogen,
                ThermalEfficiencyByBogen = bogen.out.list$thermalEffByBogen)

#save bogencoo.key.list for future use
save(bogencoo.key.list,file = paste0(pathWrite,"bogencoo.key.list.",eia_year))

#generate compiled manual bogencoo file
compilePlantInfo_excel(outputPath=paste0(pathWrite,"compiled_manualBogencoos_",eia_year,".xlsx"),
                       plantList=unique(bogencoo.key.list$manualBogencoo$Plant.Code),
                       outTables,
                       shiftCols=1,
                       inputData_path,
                       fileComment = "All Plants flagged with as manual bogencoos")



#1st file with edits
new.bogencoo.list<-manualBogenReplace(outputData_path = inputData_path,
                                      fileName="compiled_manualBogencoos_2015_1stEdit.xlsx",
                                      data.list=bogencoo.key.list, 
                                      replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
                                      lastReplace=FALSE)

#apply edits to bogencoo.key.list
bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type

#2nd file with edits
new.bogencoo.list<-manualBogenReplace(outputData_path = inputData_path,
                                      fileName="compiled_manualBogencoos_2015_2ndEdit.xlsx",
                                      data.list=bogencoo.key.list, 
                                      replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
                                      lastReplace=F)
#apply edits to bogencoo.key.list
bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type

#3rd file with edits
new.bogencoo.list<-manualBogenReplace(outputData_path = inputData_path,
                                      fileName="compiled_manualBogencoos_2015_3rdEdit.xlsx",
                                      data.list=bogencoo.key.list, 
                                      replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
                                      lastReplace=F)

#apply edits to bogencoo.key.list
bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type

#4th and last file with edits
new.bogencoo.list<-manualBogenReplace(outputData_path = inputData_path,
                                      fileName="compiled_manualBogencoos_2015_4thEdit.xlsx",
                                      data.list=bogencoo.key.list, 
                                      replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
                                      lastReplace=T) #set lastReplace=TRUE for last file


#apply edits to bogencoo.key
bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type

#save bogencoo.key with manual edits applied
save(bogencoo.key.list,file = paste0(pathWrite,"bogencoo.key.list.",eia_year,"_ManualComplete"))

#add bogencoo.key and combogencoo_cooly_type to bogen.out.list and save
bogen.out.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
bogen.out.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type


#save the final version of the bogen.out.list to be used by the CondenserDuty Model
save(bogencoo.key.list,file = paste0(pathWrite,"bogencoo.key.list.",eia_year,"_ManualComplete"))
bogen.out.list$bogencoo.key<-bogencoo.key.list$bogencoo.key
bogen.out.list$combogencoo_cooly_type<-bogencoo.key.list$combogencoo_cooly_type
bogen.out.list$combogencoo_cooly_type_nukes<-bogencoo.key.list$combogencoo_cooly_type_nukes
save(bogen.out.list,file = paste0(pathWrite,"bogen.out.list.",eia_year,"_FinalForCD"))


#save combogencoo_cool_type key for future use
#combine both nuclear and non-nuclear units
coolType<-rbind.fill(bogen.out.list$combogencoo_cooly_type,bogen.out.list$combogencoo_cooly_type_nukes)
coolType<-coolType[order(coolType$Plant.Code),]
#remove extra columns
coolType<-coolType[c(3:9)]
write.csv(coolType,file=paste0(pathWrite,"combogencoo_cool_type",eia_year,".csv"),row.names = F)

################################################
## Section 5 : Run the CondenserDuty Model
################################################

## Run the condenser duty model to for steam, natural gass fuel combined cycle, and nuclear plants.
runCondenserDuty(inputData_path, outputData_path, inputData.list, bogen.out.list)
