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
  # if(y==2014){
  #   updated<-"2022-09-19"
  # }else{
  #   updated<-"2022-09-05"
  # }
  
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
  
  #load manual bogens completed
  # if (y!=2015){
  # load(paste0(outputData_path2,"bogen.out.list.",eia_year,"_ManualComplete_updated_",updated))
  # 
  # bogencoo.key.list<-bocoo.associate(inputData.list$bocoo,bogen.out.list$sheet3_key,
  #                                    inputData.list$gen_fuel_data,inputData.list$boilerFuelData,
  #                                    inputData.list$cooling,inputData_path, save_image = FALSE)
  # 
  # combogencoo_cooly_type_nukes<-bogencoo.key.list$combogencoo_cooly_type_nukes
  # 
  # #load manual bogencoos complete
  # load(paste0(outputData_path2,"bogencoo.key.list.",eia_year,"_ManualComplete_updated_",updated))
  # 
  # #load final bogen.out.list
  # load(paste0(outputData_path2,"bogen.out.list.",eia_year,"_FinalForCD_updated_",updated))
  # bogen.out.list$combogencoo_cooly_type_nukes<-combogencoo_cooly_type_nukes
  # }else{#2015
    # load(paste0("C:/Users/lgorman/OneDrive - DOI/Repos/Thermoelectric/cd/Output/bogencoo.key.list.",eia_year,"_ManualComplete_updated_",updated))
    # load(paste0("C:/Users/lgorman/OneDrive - DOI/Repos/Thermoelectric/cd/Output/bogen.out.list.",eia_year,"_FinalForCD_updated_",updated))
     load(paste0(outputData_path2,"bogen.out.list.",eia_year,"_FinalForCD_updated_",updated))
    
 # }
    runCondenserDuty(inputData_path, outputData_path, inputData.list, bogen.out.list)
    
    postProcessVars$YEAR<-rep(y,nrow(postProcessVars))
  if (y==2008){
    postProcess_all<-postProcessVars
  }else{
    postProcess_all<-rbind(postProcess_all,postProcessVars)
  }
}#end year loop
write.csv(postProcess_all,file=paste0(outputData_path,"postProcessVars_2008-2020.csv"),row.names = F)