library(openxlsx)
auto_sheet3_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/auto_sheet3_bogen.key.csv")
#auto_sheet3_key<-read.csv("E:/Corona_VPN/WBEEP/EIA_2016/outputCSV/auto_sheet3_bogen.key.csv")

orig_sheet3_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/condenserDuty/Data/sheet3_key.csv")
#orig_sheet3_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/auto_sheet3_bogen.key.csv")

orig_sheet4_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/condenserDuty/Data/sheet4_key.csv")
#orig_sheet4_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/auto_sheet4_bogen.key.csv")

auto_sheet4_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/auto_sheet4_bogen.key.csv")
#auto_sheet4_key<-read.csv("E:/Corona_VPN/WBEEP/EIA_2016/outputCSV/auto_sheet4_bogen.key.csv")


orig_sheet3_key$Plant.Code<-sapply(orig_sheet3_key$plant_bo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))
orig_sheet4_key$Plant.Code<-sapply(orig_sheet4_key$plant_gen, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))


plantList<-sort(unique(c(onlyAutoBogens$Plant.Code,onlyOrigBogens$Plant.Code,
                         onlyAutoBogens2$Plant.Code,onlyOrigBogens2$Plant.Code)))
outTables<-list(orig_sheet3_key=orig_sheet3_key,
                auto_sheet3_flags=binary_Auto3,
                auto_sheet3_key=auto_sheet3_key,
                orig_sheet4_key=orig_sheet4_key,
                auto_sheet4_flags=binary_Auto4,
                auto_sheet4_key=auto_sheet4_key,
                EIA_associationTable = bogen,
                generator.data.860 = generator.data,
                retiredGenerators.860 = retiredGenerators,
                Boiler_InfoDesign_Parameters.860 = boilerDesignData,
                boilerFuelData_923 = boilerFuelData,
                generation.data.923 = sheet4GenFuelData)
outputPath<-"E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/Output/compileALLtables_mismatchedPlants.xlsx"

compilePlantInfo_excel(outputPath,plantList,outTables)

timsOrphanGens<-c(50099,50273,54015,55090,55302,56038,58518)

compilePlantInfo_excel(outputPath="D:/unFlaggedPlants_2.10.22.xlsx",plantList=unFlaggedPlants,outTables)

compilePlantInfo_excel(outputPath="D:/compiled_56309.xlsx",plantList=56309,outTables)

# wb<-createWorkbook()
# addWorksheet(wb,sheetName="DataCompare")
# startRow<-1
# sheet3_key2<-sheet3_key
# sheet3_key<-auto_sheet3_key
# plantList<-sort(unique(c(onlyAutoBogens$Plant.Code,onlyOrigBogens$Plant.Code)))
# plantList<-52026
# 
# for (p in plantList){
#   onlyAutoSub<-onlyAutoBogens %>% filter(Plant.Code==p)
#   onlyOrigSub<-onlyOrigBogens %>% filter(Plant.Code==p)
#   
#   #header
#   headerSub<-data.frame(Plant.Code=p,onlyAuto=nrow(onlyAutoSub),onlyOrig=nrow(onlyOrigSub))
#   addStyle<-addStyle(wb,"DataCompare",rows=startRow,cols=1:3,style=createStyle(fgFill = "yellow"))
#   writeData(wb,"DataCompare",headerSub,startRow = startRow)
#   startRow<-startRow+5
#   
#   #orig_key
#   writeData(wb,"DataCompare","orig_sheet3_key",startRow = startRow)
#   addStyle(wb,"DataCompare",rows=startRow,cols=1:100,
#            style=createStyle(fgFill = "lightskyblue1"))
#   writeData(wb,"DataCompare",sheet3_key2 %>% filter(Plant.Code==p),startRow = startRow+1)
#   startRow<-startRow+1+nrow(sheet3_key2 %>% filter(Plant.Code==p))+3
#   
#   #auto_key
#   writeData(wb,"DataCompare","auto_sheet3_key",startRow = startRow)
#   addStyle(wb,"DataCompare",rows=startRow,cols=1:100,
#            style=createStyle(fgFill = "lightskyblue1"))
#   writeData(wb,"DataCompare",sheet3_key %>% filter(Plant.Code==p),startRow =  startRow+1)
#   startRow<-startRow+1+nrow(sheet3_key %>% filter(Plant.Code==p))+3
#   
#   #boiler-gen association table
#   writeData(wb,"DataCompare","EIA_associationTable",startRow = startRow)
#   addStyle(wb,"DataCompare",rows=startRow,cols=1:100,
#            style=createStyle(fgFill = "lightskyblue1"))
#   writeData(wb,"DataCompare",bogen %>% filter(Plant.Code==p),startRow =  startRow+1)
#   startRow<-startRow+1+nrow(bogen %>% filter(Plant.Code==p))+3
#   
#   #generator.data.860
#   writeData(wb,"DataCompare","generator.data.860",startRow = startRow)
#   addStyle(wb,"DataCompare",rows=startRow,cols=1:100,
#            style=createStyle(fgFill = "lightskyblue1"))
#   writeData(wb,"DataCompare",generator.data %>% filter(Plant.Code==p),startRow =  startRow+1)
#   startRow<-startRow+1+nrow(generator.data %>% filter(Plant.Code==p))+3 
#   
#   #boilerDesignData.860
#   writeData(wb,"DataCompare","Boiler Info & Design Parameters 860",startRow = startRow)
#   addStyle(wb,"DataCompare",rows=startRow,cols=1:100,
#            style=createStyle(fgFill = "lightskyblue1"))
#   writeData(wb,"DataCompare",boilerDesignData %>% filter(Plant.Code==p),startRow =  startRow+1)
#   startRow<-startRow+1+nrow(boilerDesignData %>% filter(Plant.Code==p))+3 
#   
#   #boilerFuelData
#   writeData(wb,"DataCompare","boilerFuelData_923",startRow = startRow)
#   addStyle(wb,"DataCompare",rows=startRow,cols=1:100,
#            style=createStyle(fgFill = "lightskyblue1"))
#   writeData(wb,"DataCompare",boilerFuelData %>% filter(Plant.Code==p),startRow =  startRow+1)
#   startRow<-startRow+1+nrow(boilerFuelData %>% filter(Plant.Code==p))+3
# 
#   #generation.data.923
#   writeData(wb,"DataCompare","generation.data.923",startRow = startRow)
#   addStyle(wb,"DataCompare",rows=startRow,cols=1:100,
#            style=createStyle(fgFill = "lightskyblue1"))
#   writeData(wb,"DataCompare",sheet4GenFuelData %>% filter(Plant.Code==p),startRow =  startRow+1)
#   startRow<-startRow+1+nrow(sheet4GenFuelData %>% filter(Plant.Code==p))+3
#   
#   addStyle(wb,"DataCompare",rows=startRow,cols=1:100,style=createStyle(fgFill = "black"))
#   startRow<-startRow+3
#   
# }
# 
# saveWorkbook(wb,file = "E:/Corona_VPN/WBEEP/testAssoc/compilePlant_52026.xlsx",overwrite = TRUE)
# 
# 
