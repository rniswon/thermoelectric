# Step 1: Set working directory to ~/thermoelectric-water-consumption-models/, set directory for input/output 
#         data, and load Condenser Duty Model functions. If using Rprojects, path to current open project 
#         must be ~/thermoelectric-water-consumption-models/, otherwise the current project must be closed prior 
#         to running the thermoelectric-water-consumption-models.
setwd("./")
inputData_path<-"./cd/Data/"
# outputData_path<-"E:/Corona_VPN/WBEEP/Bogen_Output_7.14.22/"
# pathWrite<-outputData_path
outputData_path<-"./cd/Output/"
pathWrite<-"./plant_association/Output/"
pathData<-"./cd/Data"

#ImportModule
#dest<-tempdir()
dest<-"D:/EIA_2015"
eia_year<-2015
path_InputData_Metafile<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/EIA_crosswalks_complete/UserControlCrosswalk2015.xlsx"
#path_InputData_Metafile<-"E:/Corona_VPN/Repos/Thermoelectric_github/importModule/userControlFiles/UserControlCrosswalk_test.xlsx"
#path_InputData_Metafile<-"E:/Corona_VPN/Repos/Thermoelectric_github/importModule/userControlFiles/UserControlCrosswalk_2015.xlsx"
path_EIAInputData<-paste0(dest,"/",eia_year)
outputCSV<-TRUE
path_outputCSV<-dest
updateComment<-"Recent updates include processing non-nuclear units of plants with nuclear units"
updated<-"2022-12-04"



#########DO NOT EDIT BELOW THIS LINE#########
devtools::install_deps("./cd", upgrade = "never")
devtools::load_all("./cd",recompile = FALSE)
devtools::install_deps("./plant_association", upgrade = "never")
devtools::load_all("./plant_association",recompile = FALSE)


  plantList<-openxlsx::read.xlsx("C:/Users/lgorman/OneDrive - DOI/WBEEP/TE_MPL_allyears.xlsx",sheet="plant years vert")
  plantList$Plant.Code<-plantList$EIA_PLANT_ID
  plantList<-plantList[plantList$year==eia_year,]


#nukePlants<-read.csv(paste0(pathData,'/nuke_combogencoo_key.csv'))

#run import module
#eia_webpull(eia_year,dest)
inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,path_outputCSV)

inputData.list$plantList<-plantList
#inputData.list$nukePlants<-nukePlants

#run bogen associations
#bogen.out.list<-executeBogenAssoc(inputData_path,pathWrite,inputData.list,select_RPM=c("CA", "CS", "CT", "ST",NA), eia_year)

#identical(test_bogen.key,bogen.out.list)
# test_bogen.key<-bogen.out.list
# save(test_bogen.key,file="./plant_association/test/test_bogen.key")

outTables<-list(auto_sheet3_key=bogen.out.list$sheet3_key,
                auto_sheet3_flags=bogen.out.list$binary_Auto3,
                auto_sheet4_key=bogen.out.list$sheet4_key,
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

###############################
if (eia_year==2015){
#get tables for compile file 2015 ONLY
#sheet3_key
origCol<-"plant_bo"
autoCols<-c("plant_bo","plant_bo_bf.923","plant_bo_assoc")
orig_key<-read.csv("./cd/Data/sheet3_key.csv")
orig_key$Plant.Code<-sapply(orig_key$plant_bo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))
orig_sheet3_key<-orig_key
auto_key<-bogen.out.list$sheet3_key
compare.list<-compareKeys_2D(orig_key,auto_key,origCol,autoCols)
onlyAutoBogens<-compare.list$onlyAutoBogens
onlyOrigBogens<-compare.list$onlyOrigBogens
matched<-compare.list$matched

nrow(onlyAutoBogens)
nrow(onlyOrigBogens)
nrow(matched)

#sheet4_key
origCol<-"plant_gen"
autoCols<-c("plant_gen","plant_gen.923","plant_gen.860","plant_gen_assoc")
orig_key<-read.csv("./cd/Data//sheet4_key.csv")
orig_key$Plant.Code<-sapply(orig_key$plant_gen, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))
orig_sheet4_key<-orig_key
auto_key<-bogen.out.list$sheet4_key
compare.list<-compareKeys_2D(orig_key,auto_key,origCol,autoCols)
onlyAutoBogens2<-compare.list$onlyAutoBogens
onlyOrigBogens2<-compare.list$onlyOrigBogens
matched<-compare.list$matched

nrow(onlyAutoBogens2)
nrow(onlyOrigBogens2)
nrow(matched)

testManual<-bogen.out.list$sheet3_key %>% filter(!Plant.Code %in% onlyAutoBogens$Plant.Code & 
                                          !Plant.Code %in% onlyOrigBogens$Plant.Code &
                                          !Plant.Code %in% onlyAutoBogens2$Plant.Code &
                                          !Plant.Code %in% onlyOrigBogens2$Plant.Code
)

manualPlants2<-bogen.out.list$manualPlants
manualPlants2$falsePositive<-ifelse(manualPlants2$Plant.Code %in% testManual$Plant.Code,1,0) 


testManual2<-bogen.out.list$sheet3_key %>% filter(Plant.Code %in% onlyAutoBogens$Plant.Code | 
                                          Plant.Code %in% onlyOrigBogens$Plant.Code |
                                          Plant.Code %in% onlyAutoBogens2$Plant.Code |
                                          Plant.Code %in% onlyOrigBogens2$Plant.Code
)
bogen.out.list$binary_Auto3$flag_ManualPlant<-ifelse(bogen.out.list$binary_Auto3$Plant.Code %in% manualPlants2$Plant.Code,1,0)
bogen.out.list$binary_Auto3$flag_MisMatch<-ifelse(bogen.out.list$binary_Auto3$Plant.Code %in% testManual2$Plant.Code,0,1)

testManual3<-bogen.out.list$sheet4_key %>% filter(Plant.Code %in% onlyAutoBogens$Plant.Code | 
                                          Plant.Code %in% onlyOrigBogens$Plant.Code |
                                          Plant.Code %in% onlyAutoBogens2$Plant.Code |
                                          Plant.Code %in% onlyOrigBogens2$Plant.Code
)
bogen.out.list$binary_Auto4$flag_ManualPlant<-ifelse(bogen.out.list$binary_Auto4$Plant.Code %in% manualPlants2$Plant.Code,1,0)
bogen.out.list$binary_Auto4$flag_MisMatch<-ifelse(bogen.out.list$binary_Auto4$Plant.Code %in% testManual3$Plant.Code,0,1)


outTables<-list(orig_sheet3_key=orig_sheet3_key,
                auto_sheet3_flags=bogen.out.list$binary_Auto3,
                auto_sheet3_key=bogen.out.list$sheet3_key,
                orig_sheet4_key=orig_sheet4_key,
                auto_sheet4_flags=bogen.out.list$binary_Auto4,
                auto_sheet4_key=bogen.out.list$sheet4_key,
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
}
##############################
# test<-read.xlsx(paste0(outputData_path,"compiled_manualBogens_2015_updated_2022-09-02_LGSmanualEdit.xlsx"),sheet="PlantList")
# View(bogen.out.list$manualPlants %>% filter(!Plant.Code %in% test$PlantList))
# 
# save(bogen.out.list,file = paste0(outputData_path,"bogen.out.list.",eia_year,"_",updated))
# #generate compiled file
# 
# compilePlantInfo_excel(outputPath=paste0(outputData_path,"compiled_manualBogens_",eia_year,"_updated_",updated,".xlsx"),
#                        plantList=unique(bogen.out.list$manualPlants$Plant.Code),outTables,shiftCols=1,inputData_path,
#                        fileComment = paste("All Plants flagged with manualFlags",updateComment))
# shell.exec(normalizePath(paste0(outputData_path,"compiled_manualBogens_",eia_year,".xlsx")))

#load("E:/Corona_VPN/WBEEP/compiledmanualBogens_2008-2020/bogen.out.list.2015_2022-09-05")
load(paste0(outputData_path,"bogen.out.list.",eia_year,"_",updated))
#manual Bogen Replace
# new.bogen.list<-manualBogenReplace(outputData_path,fileName=paste0("compiled_manualBogens_",eia_year,".xlsx"), 
#                                    data.list=bogen.out.list, replaceTables=c("sheet3_key","sheet4_key"),lastReplace=TRUE)
# new.bogen.list<-manualBogenReplace(outputData_path,fileName="compiled_manualBogens_2015_LGS_8.11.22.xlsx",
#                                    data.list=bogen.out.list, replaceTables=c("sheet3_key","sheet4_key"),lastReplace=TRUE)
new.bogen.list<-manualBogenReplace(outputData_path,fileName="compiled_manualBogens_2015_updated_2022-09-02_LGSmanualEdit.xlsx",
                                   data.list=bogen.out.list, replaceTables=c("sheet3_key","sheet4_key"),lastReplace=FALSE)
#remove plants with missing bogens
new.bogen.list$sheet3_key<-new.bogen.list$sheet3_key %>% filter(!is.na(bogen))
new.bogen.list$sheet4_key<-new.bogen.list$sheet4_key %>% filter(!is.na(bogen))
#replace auto bogen with manualedits
bogen.out.list$sheet3_key<-new.bogen.list$sheet3_key
bogen.out.list$sheet4_key<-new.bogen.list$sheet4_key

new.bogen.list<-manualBogenReplace(outputData_path,fileName="compiled_manualBogens_2015_updated_2022-12-04_LGSedit.xlsx",
                                   data.list=bogen.out.list, replaceTables=c("sheet3_key","sheet4_key"),lastReplace=F)
#remove plants with missing bogens
new.bogen.list$sheet3_key<-new.bogen.list$sheet3_key %>% filter(!is.na(bogen))
new.bogen.list$sheet4_key<-new.bogen.list$sheet4_key %>% filter(!is.na(bogen))
#replace auto bogen with manualedits
bogen.out.list$sheet3_key<-new.bogen.list$sheet3_key
bogen.out.list$sheet4_key<-new.bogen.list$sheet4_key



#remove 56309
bogen.out.list$sheet3_key<-bogen.out.list$sheet3_key %>% filter(Plant.Code!=56309)
bogen.out.list$sheet4_key<-bogen.out.list$sheet4_key %>% filter(Plant.Code!=56309)

save(bogen.out.list,file = paste0(outputData_path,"bogen.out.list.",eia_year,"_ManualComplete_updated_",updated))

#load(paste0(outputData_path,"bogen.out.list.",eia_year,"_ManualComplete_updated_",updated))
#bogencoo_key
# bogencoo.key.list<-bocoo.associate(inputData.list$bocoo,bogen.out.list$sheet3_key,
#                                    inputData.list$gen_fuel_data,inputData.list$boilerFuelData,cooling,
#                                    inputData_path, save_image = FALSE)
bogencoo.key.list<-bocoo.associate(inputData.list,bogen.out.list,inputData_path,save_image=FALSE)
bogencoo.key<-bogencoo.key.list$bogencoo.key
bogen.out.list$bogencoo.key<-bogencoo.key
bogen.out.list$combogencoo_cooly_type_nukes<-bogencoo.key.list$combogencoo_cooly_type_nukes

# combogencoo_cooly_type_nukes <- read.csv(paste0(inputData_path,'/combogencoo_cool_type.csv'), header=T,  stringsAsFactors = F)
# combogencoo_cooly_type_nukes$Plant.Code<-sapply(combogencoo_cooly_type_nukes$bogencoo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][2]))
# combogencoo_cooly_type_nukes$manualEdit<-rep(0,nrow(combogencoo_cooly_type_nukes))
# combogencoo_cooly_type_nukes<-combogencoo_cooly_type_nukes %>% select(manualEdit,Plant.Code,
#                                                           names(combogencoo_cooly_type_nukes)[!names(combogencoo_cooly_type_nukes) %in% c("manualEdit","Plant.Code")])
# combogencoo_cooly_type_nukes<-combogencoo_cooly_type_nukes %>% filter(Plant.Code %in% nukes$Plant.Code)
# bogen.out.list$combogencoo_cooly_type_nukes<-combogencoo_cooly_type_nukes
#bogencoo.key.list$combogencoo_cooly_type_nukes<-combogencoo_cooly_type_nukes


outTables<-list(
                auto_bogencoo.key = bogencoo.key,
                combogencoo_cool_type_key = bogencoo.key.list$combogencoo_cooly_type,
                EIA_coolingSystemInfo = inputData.list$cooling,
                EIA_BocooAssoc = inputData.list$bocoo,
                auto_sheet3_flags=bogen.out.list$binary_Auto3,
                auto_sheet3_key=bogen.out.list$sheet3_key,
                auto_sheet4_flags=bogen.out.list$binary_Auto4,
                auto_sheet4_key=bogen.out.list$sheet4_key,
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
##################################for 2015################
if (eia_year==2015){
orig_bogencoo.key<-read.csv(paste0(inputData_path,'/combogencoo_key.csv'), header=T, stringsAsFactors = F)
orig_bogencoo.key$Plant.Code<-sapply(orig_bogencoo.key$combogen, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][2]))

# coolingEIA<-openxlsx::read.xlsx("E:/Corona_VPN/WBEEP/testImport/2015/6_2_EnviroEquip_Y2015.xlsx",sheet="Cooling",startRow=2)
# coolingSys<-openxlsx::read.xlsx("E:/Corona_VPN/WBEEP/testImport/2015/EIA923_Schedule_8_Annual_Environmental_Information_2015_Final_Revision.xlsx",
#                                 sheet="8D Cooling System Information",startRow=5)
# coolingSys$Plant.Code<-coolingSys$Plant.ID
# coolingSys<-coolingSys %>% select(Plant.Code,names(coolingSys)[names(coolingSys)!="Plant.Code"])

outTables<-list(orig_bogencoo.key = orig_bogencoo.key,
                auto_bogencoo.key = bogencoo.key,
                combogencoo_cool_type_key = bogencoo.key.list$combogencoo_cooly_type,
                EIA_BocooAssoc = inputData.list$bocoo,
                EIA_CoolingEnviroEquip = inputData.list$cooling,
                # EIA_coolingSystemInfo = coolingSys,
                orig_sheet3_key=orig_sheet3_key,
                auto_sheet3_flags=bogen.out.list$binary_Auto3,
                auto_sheet3_key=bogen.out.list$sheet3_key,
                orig_sheet4_key=orig_sheet4_key,
                auto_sheet4_flags=bogen.out.list$binary_Auto4,
                auto_sheet4_key=bogen.out.list$sheet4_key,
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

}
###################################


#EIA-860 file 6_2_EnviroEquip_Y2015, tab "cooling"
#EIA923_Schedule_8_Annual_Environmental_Information_2015_Final, tab "8D Cooling System Information"
#generate compiled file
save(bogencoo.key.list,file = paste0(outputData_path,"bogencoo.key.list.",eia_year,"_updated_",updated))
# compilePlantInfo_excel(outputPath=paste0(outputData_path,"compiled_manualBogencoos_",eia_year,"_updated_",updated,".xlsx"),
#                        plantList=unique(bogencoo.key.list$manualBogencoo$Plant.Code),outTables,shiftCols=1,inputData_path,
#                        fileComment = paste("All Plants flagged with manualBogencooFlag",updateComment))

#add addtional manuals
addManual<-read.xlsx("C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledmanualBogens_2008-2020/Additional_manualBogencoos2015.xlsx",sheet="Sheet1")
compilePlantInfo_excel(outputPath=paste0(outputData_path,"compiled_manualBogencoos_",eia_year,"_updated_",updated,".xlsx"),
                       plantList=unique(unique(bogencoo.key.list$manualBogencoo$Plant.Code),unique(addManual$Plant.Code)),outTables,shiftCols=1,inputData_path,
                       fileComment = paste("All Plants needing edit after coolType mismatch verification",updateComment))
# shell.exec(normalizePath(paste0(outputData_path,"compiled_manualBogencoos_",eia_year,".xlsx")))

test<-read.xlsx(paste0(outputData_path,"compiled_manualBogencoos_2015_updated_2022-09-05_LGSedit2_MAHedit.xlsx"),sheet="PlantList")
View(bogencoo.key.list$manualBogencoo %>% filter(!Plant.Code %in% test$PlantList))
#manual Bogencoo Replace
# load(paste0(outputData_path,"bogencoo.key.list.",eia_year,"_updated_",updated))
new.bogencoo.list<-manualBogenReplace(outputData_path,fileName="compiled_manualBogencoos_2015_updated_2022-09-05_LGSedit2.xlsx",
                                      data.list=bogencoo.key.list, replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
                                      lastReplace=FALSE)
bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type
new.bogencoo.list<-manualBogenReplace(outputData_path,fileName="compiled_manualBogencoos_2015_updated_2022-09-05_LGSedit2_MAHedit.xlsx",
                                      data.list=bogencoo.key.list, replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
                                      lastReplace=F)
bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type

new.bogencoo.list<-manualBogenReplace(outputData_path,fileName="compiled_manualBogencoos_2015_updated_2022-12-04_LGSedit.xlsx",
                                      data.list=bogencoo.key.list, replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
                                      lastReplace=F)
bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type

new.bogencoo.list<-manualBogenReplace(outputData_path,fileName="compiled_manualBogencoos_2015_updated_2022-12-04_LGSedit2.xlsx",
                                      data.list=bogencoo.key.list, replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
                                      lastReplace=T)
bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type
# bogencoo.key.listAdd<-bogencoo.key.list
# bogencoo.key.listAdd$manualBogencoo<-data.frame(Plant.Code=c(unique(addManual$Plant.Code)))
# new.bogencoo.list<-manualBogenReplace(outputData_path,fileName="compiled_manualBogencoosAddition_2015_updated_2022-09-05_LGSedit.xlsx", 
#                                       data.list=bogencoo.key.listAdd, replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
#                                       lastReplace=F)
# bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
# bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type
# bogencoo.key.listAdd<-bogencoo.key.list
# bogencoo.key.listAdd$manualBogencoo<-data.frame(Plant.Code=2098)
# new.bogencoo.list<-manualBogenReplace(outputData_path,fileName="compiled_manualBogencoosAddition2_2015_updated_2022-09-05_LGSedit.xlsx", 
#                                       data.list=bogencoo.key.list, replaceTables=c("bogencoo.key","combogencoo_cooly_type"),
#                                       lastReplace=FALSE)
# 
# bogencoo.key.list$bogencoo.key<-new.bogencoo.list$bogencoo.key
# bogencoo.key.list$combogencoo_cooly_type<-new.bogencoo.list$combogencoo_cooly_type

save(bogencoo.key.list,file = paste0(outputData_path,"bogencoo.key.list.",eia_year,"_ManualComplete_updated_",updated))
bogen.out.list$bogencoo.key<-bogencoo.key.list$bogencoo.key
bogen.out.list$combogencoo_cooly_type<-bogencoo.key.list$combogencoo_cooly_type
bogen.out.list$combogencoo_cooly_type_nukes<-bogencoo.key.list$combogencoo_cooly_type_nukes
save(bogen.out.list,file = paste0(outputData_path,"bogen.out.list.",eia_year,"_FinalForCD2_updated_",updated))

#load(paste0(outputData_path,"bogencoo.key.list.",eia_year,"_ManualComplete_updated_",updated))
#load(paste0(outputData_path,"bogen.out.list.",eia_year,"_FinalForCD_updated_",updated))


coolType2015<-rbind.fill(bogen.out.list$combogencoo_cooly_type,bogen.out.list$combogencoo_cooly_type_nukes)
coolType2015<-coolType2015[order(coolType2015$Plant.Code),]
coolType2015<-coolType2015[c(3:9)]
write.csv(coolType2015,file=paste0(inputData_path,"combogencoo_cool_type2015.csv"),row.names = F)


# write.csv(bogen.out.list$sheet3_key,file=paste0(outputData_path,"sheet3_key.csv",eia_year,"_updated_",updated),row.names = F)
# write.csv(bogen.out.list$sheet4_key,file=paste0(outputData_path,"sheet4_key.csv",eia_year,"_updated_",updated),row.names = F)
# write.csv(bogen.out.list$bogencoo.key,file=paste0(outputData_path,"bogencoo.key.csv",eia_year,"_updated_",updated),row.names = F)
# write.csv(bogen.out.list$combogencoo_cooly_type,file=paste0(outputData_path,"combogencoo_cool_type.csv",eia_year,"_updated_",updated),row.names = F)


# Step 2:
## Run the condenser duty model to for steam, natural gass fuel combined cycle, and nuclear plants.
#source(here('CondenserDutyModel', 'CondenserDutyModel_new.R'))
runCondenserDuty(inputData_path, outputData_path, inputData.list, bogen.out.list)

# Step 3:
## Prepare raw output of the condenser duty model for input to the TOWER and FEWSR models.
source(here('CondenserDutyModel', 'R', 'PrepTowerFEWSRinput.R'))

# Step 4:
## Run the tower model.
source(here('TOWER_model', 'R', 'Tower_model.R'))

# Step 5:
## Run the FEWSR model.
### FEWSR Model Data Prep
#To run the FEWSR model, the input data files must be opened in Microsoft Excel or similar program.
#Step 3, generates the data files and stores them within the extdata folder inside of the FEWSR inst directory.
#Open the data files (ends in "_input")and corresponding header file (ends in "_header"). Insert the contents 
#of the header as new rows above the content of the inpu data file. Once finished save the file of combined 
#header and input data with the same name as the input file and the ".xlsx" extension. Save files to the location
# "FEWSR_model/inst/extdata".

### The next three lines execute the FEWSR models for each waterbody type. When prompted to enter a location
#for storing output, select the "Output" folder within the FEWSR directory, or another user specified location.
#The outout argument allows the user to specify Excel formated output ("xlsx"), comma-seperated variable format ("csv")
#or both "both"

#### Rivers
fewsr(file.path(system.file("extdata", "FEWS_River_plants_input.xlsx", package = "FEWSR")), type = "river", output = "both")


#### Ponds
fewsr(file.path(system.file("extdata", "FEWS_Pond_plants_input.xlsx", package = "FEWSR")), sheet = "FEWS pond only input", type = "pond", output = "both")


#### Big lakes
fewsr(file.path(system.file("extdata", "FEWS_BIG_Lake_plants_input.xlsx", package = "FEWSR")), sheet = 1, type = "lake", output = "both")


#Sys.getenv("HOME")
