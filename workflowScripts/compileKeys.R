#path to repo
repoPath<-"C:/Users/lgorman/OneDrive - DOI/Repos/Thermoelectric/"
#output directory
outputData_path2<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledmanualBogens_2008-2020/"
outputData_path<-"D:/compiledmanualBogens_2008-2020/"
#directory to save EIA data import
EIAsaveOut<-"D:/"
#path to EIA crosswalks
#found on Teams WaterUseModels/Thermoelectric/AssociationKeys/EIA_crosswalks_complete
EIAcrossPath<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/EIA_crosswalks_complete/"
#path to binary keys
#from teams WaterUseModels/Thermoelectric/AssociationKeys/binaryKeysForCD
binaryPath<-outputData_path
#path to MasterPLantList
MPLpath<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/TE_MPL_allyears.xlsx"
#load development packages and dependencies
devtools::install_deps(paste0(repoPath,"/cd"), upgrade = "never")
devtools::load_all(paste0(repoPath,"/cd"),recompile = FALSE)
devtools::install_deps(paste0(repoPath,"/plant_association"), upgrade = "never")
devtools::load_all(paste0(repoPath,"/plant_association"),recompile = FALSE)
#####################################
#NO EDITS REQUIRED BELOW THIS LINE
####################################
inputData_path<-paste0(repoPath,"/cd/Data/")
pathData<-inputData_path
pathWrite<-outputData_path
years<-seq(2008,2020,1)

for(y in years){
  print(y)
  
  updated<-"2022-12-04"
  #ImportModule
  dest<-paste0(EIAsaveOut,"EIA_",y)
  eia_year<-y
  path_InputData_Metafile<-paste0(EIAcrossPath,"UserControlCrosswalk",y,".xlsx")
  path_EIAInputData<-paste0(dest,"/",eia_year)
  outputCSV<-TRUE
  path_outputCSV<-dest
  plantList<-openxlsx::read.xlsx(MPLpath,sheet="plant years vert")
  plantList$Plant.Code<-plantList$EIA_PLANT_ID
  plantList<-plantList[plantList$year==eia_year,]
  #run import module
  #eia_webpull(eia_year,dest)
  inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,path_outputCSV)
  inputData.list$plantList<-plantList
  
  select_RPM=c("CA", "CS", "CT", "ST",NA)
  
  #compile crosswalks
  cross<-openxlsx::read.xlsx(path_InputData_Metafile,sheet="Sheet1")
  cross$YEAR<-rep(y,nrow(cross))
  datePulled<-file.info(paste0(substr(path_EIAInputData,1,nchar(path_EIAInputData)-4),"/eia860",eia_year,".zip"))$ctime
  cross$downloadDate<-rep(datePulled,nrow(cross))
  
  if(y==2008){
    compileCross<-cross
  }else{
    compileCross<-rbind(compileCross,cross)
  }
  
  #load bogen.out.list
 load(paste0(outputData_path2,"bogen.out.list.",eia_year,"_FinalForCD_updated_",updated))
 list2env(bogen.out.list, .GlobalEnv)
 sheet3_key<-sheet3_key %>% select(Plant.Code,
                                   Boiler.ID,
                                   bogen,
                                   contains("Reported.Prime.Mover"),
                                   manualBogenEdit,
                                   YEAR,
                                   plant_bo,
                                   plant_bo_bf.923)
 
 sheet3_key$sheet3_key<-rep(1,nrow(sheet3_key))
 
 sheet4_key<-sheet4_key %>% select(Plant.Code,
                                   Generator.ID,
                                   bogen,
                                   plant_gen,
                                   plant_gen.923,
                                   YEAR)
 sheet4_key$sheet4_key<-rep(1,nrow(sheet4_key))
 
 bogencoo.key<-bogencoo.key %>% select(Plant.Code,
                                      
                                   Bogen,
                                   combogencoo,
                                   manualBogencooEdit,
                                   YEAR)
 names(bogencoo.key)[names(bogencoo.key)=="Bogen"]<-"bogen"
 names(bogencoo.key)[names(bogencoo.key)=="combogencoo"]<-"bogencoo"
 
 bogencoo.key$bogencoo.key<-rep(1,nrow(bogencoo.key))
 
 #combogencoo_cooly_type<-rbind.fill(combogencoo_cooly_type,combogencoo_cooly_type_nukes)
 combogencoo_cooly_type<-combogencoo_cooly_type %>% select(-manualBogencooEditFileName,
                                                           -manualBogencooEditDateTime)
 combogencoo_cooly_type$YEAR<-rep(y,nrow(combogencoo_cooly_type))
 combogencoo_cooly_type$combogencoo_cooly_type<-rep(1,nrow(combogencoo_cooly_type))
 
 combogencoo_cooly_type_nukes<-combogencoo_cooly_type_nukes %>% select(-manualEdit)
 combogencoo_cooly_type_nukes$YEAR<-rep(y,nrow(combogencoo_cooly_type_nukes))
 combogencoo_cooly_type_nukes$combogencoo_cooly_type_nukes<-rep(1,nrow(combogencoo_cooly_type_nukes))
 
 # sheet3_key_plantlevel<-sheet3_key %>% filter(regexpr("NGCC",bogen)>0 | regexpr("NGCC",bogen)>0)
 # sheet3_key<-sheet3_key %>% filter(regexpr("NGCC",bogen)<0 & regexpr("NGCC",bogen)<0)
 # 
 # sheet4_key_plantlevel<-sheet4_key %>% filter(regexpr("NGCC",bogen)>0 | regexpr("NGCC",bogen)>0)
 # sheet4_key<-sheet4_key %>% filter(regexpr("NGCC",bogen)<0 & regexpr("NGCC",bogen)<0)
 # 
 #compileKey<-inner_join(sheet3_key,sheet4_key,by=c("Plant.Code","bogen","YEAR"))
 compileKey<-merge(sheet3_key,sheet4_key,by=c("Plant.Code","bogen","YEAR"),all.x = T,all.y = T)
 #compileKey<-rbind(compileKey,compileKey_plantlevel)
 compileKey<-merge(compileKey,bogencoo.key,by=c("Plant.Code","bogen","YEAR"),all.x = T,all.y = T)
 combogencoo_cooly_type<-rbind.fill(combogencoo_cooly_type,combogencoo_cooly_type_nukes)
 compileKey<-merge(compileKey,combogencoo_cooly_type,by=c("Plant.Code","bogencoo","YEAR"),all.x = T,all.y = T)

 
 compileKey<-compileKey %>% dplyr::rowwise() %>% 
   dplyr::mutate(manualBogencooEdit = sum(manualBogencooEdit.x,manualBogencooEdit.y))
 
 compileKey<-compileKey %>% select(Plant.Code,
                                   Boiler.ID,
                                   Generator.ID,
                                   bogen,
                                   bogencoo,
                                   plant_bo,
                                   plant_bo_bf.923,
                                   plant_gen,
                                   plant_gen.923,
                                   manualBogenEdit,
                                   manualBogencooEdit,
                                   Reported.Prime.Mover_page1,
                                   LAKE..OF..OC..RC.,
                                   RIVER..OF.,
                                   POND..OC..RC.,
                                   TOWER..RF..RI..RN.,
                                   DC,
                                   OS,
                                   sheet3_key,
                                   sheet4_key,
                                   bogencoo.key,
                                   combogencoo_cooly_type,
                                   combogencoo_cooly_type_nukes,
                                   YEAR)
 compileKey$YEAR<-rep(y,nrow(compileKey))
 if (y==2008){
   compileKey_all<-compileKey
 }else{
   compileKey_all<-rbind(compileKey_all,compileKey)
 }
  


}#end year loop
write.csv(compileKey_all,file=paste0(outputData_path,"compiledKeys_2008-2020.csv"),row.names = F)
write.csv(compileCross,file=paste0(outputData_path,"compileCrosswalks_2008-2020.csv"),row.names = F)