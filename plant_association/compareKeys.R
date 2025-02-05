sheet3_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/condenserDuty/Data/sheet3_key.csv")
#sheet3_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/auto_sheet3_bogen.key.csv")

auto_sheet3_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/auto_sheet3_bogen.key.csv")
#auto_sheet3_key<-read.csv("E:/Corona_VPN/WBEEP/EIA_2016/outputCSV/auto_sheet3_bogen.key.csv")

sheet4_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/condenserDuty/Data/sheet4_key.csv")
#sheet4_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/auto_sheet4_bogen.key.csv")

auto_sheet4_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/auto_sheet4_bogen.key.csv")
#auto_sheet4_key<-read.csv("E:/Corona_VPN/WBEEP/EIA_2016/outputCSV/auto_sheet4_bogen.key.csv")


#format plant_bo in sheet3_key
 sheet3_key$Plant.Code<-sapply(sheet3_key$plant_bo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))
 sheet4_key$Plant.Code<-sapply(sheet4_key$plant_gen, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))
 
 
#sheet3 working 5.29.21
 origCol<-"plant_bo"
 autoCols<-c("plant_bo","plant_bo_bf.923","plant_bo_assoc")
 orig_key<-sheet3_key
 auto_key<-auto_sheet3_key
 compare.list<-compareKeys_2D(orig_key,auto_key,origCol,autoCols)
 onlyAutoBogens<-compare.list$onlyAutoBogens
 onlyOrigBogens<-compare.list$onlyOrigBogens
 matched<-compare.list$matched
 
 nrow(onlyAutoBogens)
 nrow(onlyOrigBogens)
 nrow(matched)
 
 testManual<-anti_join(onlyAutoBogens,manualPlants,by="Plant.Code")
 testManual2<-anti_join(onlyOrigBogens,manualPlants,by="Plant.Code")
 
 

 
 pathOut<-"E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/"
 write.csv(onlyOrigBogens,paste0(pathOut,"onlyIn_origSheet3_key.csv"),row.names = FALSE)
 write.csv(onlyAutoBogens,paste0(pathOut,"onlyIn_autoSheet3_key.csv"),row.names = FALSE)
 
 #check for retired Boilers that match
 test<-auto_key %>% filter(flag_BoilerRetired==1)
 test2<-anti_join(test,onlyAutoBogens,by="plant_bo")
 write.csv(test2,file="E:/Corona_VPN/WBEEP/testAssoc/RetiredBoilers_inOrigSheet3Key.csv",row.names = FALSE)
 
 #check for Bad Boiler Status that match
 test<-auto_key %>% filter(flag_Boiler.Status==1)
 test2<-anti_join(test,onlyAutoBogens,by="plant_bo")
 write.csv(test2,file="E:/Corona_VPN/WBEEP/testAssoc/flag_Boiler.Status.csv",row.names = FALSE)
 
 #########################################
 #sheet 4
 origCol<-"plant_gen"
 autoCols<-c("plant_gen","plant_gen.923","plant_gen.860","plant_gen_assoc")
 orig_key<-sheet4_key
 auto_key<-auto_sheet4_key
 compare.list<-compareKeys_2D(orig_key,auto_key,origCol,autoCols)
 onlyAutoBogens2<-compare.list$onlyAutoBogens
 onlyOrigBogens2<-compare.list$onlyOrigBogens
 matched<-compare.list$matched
 
 nrow(onlyAutoBogens2)
 nrow(onlyOrigBogens2)
 nrow(matched)
 
 testManual_A<-anti_join(onlyAutoBogens2,manualPlants,by="Plant.Code")
 testManual2_B<-anti_join(onlyOrigBogens2,manualPlants,by="Plant.Code")
 
 
 testManual3<-auto_sheet3_key %>% filter(!Plant.Code %in% onlyAutoBogens$Plant.Code & 
                                                 !Plant.Code %in% onlyOrigBogens$Plant.Code &
                                         !Plant.Code %in% onlyAutoBogens2$Plant.Code &
                                         !Plant.Code %in% onlyOrigBogens2$Plant.Code
                                         )
 
 testManual4<-auto_sheet3_key %>% filter(Plant.Code %in% onlyAutoBogens$Plant.Code | 
                                                 Plant.Code %in% onlyOrigBogens$Plant.Code |
                                                 Plant.Code %in% onlyAutoBogens2$Plant.Code |
                                                 Plant.Code %in% onlyOrigBogens2$Plant.Code
 )
 
 manualPlants2<-manualPlants
 manualPlants2$falsePositive<-ifelse(manualPlants2$Plant.Code %in% testManual3$Plant.Code,1,0) 
 
 retire2015<-data.frame(Plant.Code=plantsRetire2015)
 retire2015$flag_ManualPlant<-ifelse(retire2015$Plant.Code %in% manualPlants2$Plant.Code,1,0)
 retire2015$flag_MisMatch<-ifelse(retire2015$Plant.Code %in% testManual3$Plant.Code,0,1)
 write.csv(retire2015, file="D:/retired2015Plants.csv",row.names = F)
 
 retireBoilerPlants<-data.frame(Plant.Code=unique(test2$Plant.Code))
 retireBoilerPlants$flag_ManualPlant<-ifelse(retireBoilerPlants$Plant.Code %in% manualPlants2$Plant.Code,1,0)
 retireBoilerPlants$flag_MisMatch<-ifelse(retireBoilerPlants$Plant.Code %in% testManual3$Plant.Code,0,1)
 write.csv(retireBoilerPlants, file="D:/retireBoilerPlants_posNetGen.csv",row.names = F)
 
 
 testManual3<-inner_join(testManual3,manualPlants[c("Plant.Code")],by="Plant.Code")
 
 length(unique(testManual3$Plant.Code))
 

 

 
 write.csv(manualPlants2,file=paste0(pathWrite,"manualPlants_8.16.21.csv"),row.names = F,na="0")
 
 pathOut<-"E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/"
 write.csv(onlyOrigBogens2,paste0(pathOut,"onlyIn_origSheet4_key.csv"),row.names = FALSE)
 write.csv(onlyAutoBogens2,paste0(pathOut,"onlyIn_autoSheet4_key.csv"),row.names = FALSE)
 
 #check for Bad Generator Status that match
 testb<-auto_sheet4_key %>% filter(flag_Generator.Status==1)
 test3<-anti_join(testb,onlyAutoBogens2,by="plant_gen")
 write.csv(test3,file="E:/Corona_VPN/WBEEP/testAssoc/flag_Generator.Status.csv",row.names = FALSE)
 
 
 #check for Bad Generator Status that match
 testb<-auto_sheet4_key %>% filter(flag_GeneratorRetired==1)
 test3<-anti_join(testb,onlyAutoBogens2,by="plant_gen")
 write.csv(test3,file="E:/Corona_VPN/WBEEP/testAssoc/RetiredGenerators_inOrigSheet4Key.csv",row.names = FALSE)
 
 #########################################
 #compile plants in excel

 plantList<-sort(unique(c(onlyAutoBogens$Plant.Code,onlyOrigBogens$Plant.Code,
                          onlyAutoBogens2$Plant.Code,onlyOrigBogens2$Plant.Code)))
 plantList<-plantList[!plantList %in% manualPlants$Plant.Code]
 outTables<-list(orig_sheet3_key=sheet3_key,
                 auto_sheet3_key=auto_sheet3_key,
                 orig_sheet4_key=sheet4_key,
                 auto_sheet4_key=auto_sheet4_key,
                 EIA_associationTable = bogen,
                 generator.data.860 = generator.data,
                 Boiler_InfoDesign_Parameters.860 = boilerDesignData,
                 boilerFuelData_923 = boilerFuelData,
                 generation.data.923 = sheet4GenFuelData)
 outputPath<-paste0(pathOut,"/compileALLtables_mismatchedPlants.xlsx")
 #outputPath<-"E:/Corona_VPN/WBEEP/testAssoc/compilePlants_NoManualFlag.xlsx"
 library(openxlsx)
 compilePlantInfo_excel(outputPath,plantList,outTables)
 
 
 ################################################
 onlyAutoOld<-read.csv("E:/Corona_VPN/WBEEP/testAssoc/output_5.25.21/onlyIn_autoSheet3_key.csv")
 onlyAutoOld<-onlyAutoOld %>% filter(!Plant.Code %in% onlyAutoBogens$Plant.Code)
 unique(onlyAutoOld$Plant.Code)
 
 onlyOrigOld<-read.csv("E:/Corona_VPN/WBEEP/testAssoc/output_5.25.21/onlyIn_origSheet3_key.csv")
 onlyOrigOld<-onlyOrigOld %>% filter(!Plant.Code %in% onlyOrigBogens$Plant.Code)
 unique(onlyOrigOld$Plant.Code)
 
 
