years<-seq(2008,2020,1)

for (y in years){

  inputData_path<-"./cd/Data/"
  outputData_path<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledManualBogens_2008-2020/"
  pathWrite<-"./plant_association/Output/"
  pathData<-"./cd/Data"
  
  #ImportModule
  #ImportModule
  eia_year<-y
  dest<-paste0("D:/EIA_",eia_year)
  path_InputData_Metafile<-paste0("C:/Users/lgorman/OneDrive - DOI/WBEEP/EIA_crosswalks_complete/UserControlCrosswalk",
                                  eia_year,".xlsx")
  path_EIAInputData<-paste0(dest,"/",eia_year)
  outputCSV<-TRUE
  path_outputCSV<-dest
  updateComment<-"Recent updates include processing non-nuclear units of plants with nuclear units"
  updated<-"2022-12-04"
  
  if (y!=2008){
    print(y)
    invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),   # Unload add-on packages
                     detach,
                     character.only = TRUE, unload = TRUE))
  }
  
  #########DO NOT EDIT BELOW THIS LINE#########
  devtools::install_deps("./cd", upgrade = "never")
  devtools::load_all("./cd",recompile = FALSE)
  devtools::install_deps("./plant_association", upgrade = "never")
  devtools::load_all("./plant_association",recompile = FALSE)
  
  
  plantList<-openxlsx::read.xlsx("C:/Users/lgorman/OneDrive - DOI/WBEEP/TE_MPL_allyears.xlsx",sheet="plant years vert")
  plantList$Plant.Code<-plantList$EIA_PLANT_ID
  plantList<-plantList[plantList$year==eia_year,]
  
  
  #run import module
  #eia_webpull(eia_year,dest)
  inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,path_outputCSV)
  
  inputData.list$plantList<-plantList
  #load bogens with manual edits
  load(paste0(outputData_path,"bogen.out.list.",eia_year,"_ManualComplete_updated_",updated))
  
  
  bogencoo.key.list<-bocoo.associate(inputData.list,bogen.out.list,inputData_path,save_image=FALSE)
  testbogencoo.key.list<-bogencoo.key.list
print("old code complete")
  invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),   # Unload add-on packages
                   detach,
                   character.only = TRUE, unload = TRUE))
  ########################################
  #close Rstudio and load importThermoEIA and EIAplantAssociation
  library(importThermoEIA)
  library(EIAplantAssociation)
  
  outputData_path<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledManualBogens_2008-2020/"
  updated<-"2022-12-04"
  
  
  #load MasterPlantList
  utils::data("masterPlantList")
  plantList<-masterPlantList[masterPlantList$year==eia_year,]
  inputData.list<-inputData.list[which(names(inputData.list)!="plantList")]
  
  #load bogens with manual edits
  load(paste0(outputData_path,"bogen.out.list.",eia_year,"_ManualComplete_updated_",updated))
  
  #run boiler-generator-cooling associations
  bogencoo.key.list<-bocoo.associate(inputData.list,bogen.out.list,plantList=plantList)
  

  testBogencoo<-testbogencoo.key.list$bogencoo.key
  testCombogen<-testbogencoo.key.list$combogencoo_cooly_type
  testCombogenNuke<-testbogencoo.key.list$combogencoo_cooly_type_nukes
 library(dplyr) 
  testBogencoo<-testBogencoo %>% select(all_of(names(bogencoo.key.list$bogencoo.key)))
testCombogen<-testCombogen %>% select(all_of(names(bogencoo.key.list$combogencoo_cooly_type)))
testCombogenNuke<-testCombogenNuke %>% select(all_of(names(bogencoo.key.list$combogencoo_cooly_type_nukes)))

if (y==2008){
  outID<-data.frame(YEAR=y,bogencoo.key = identical(testBogencoo,bogencoo.key.list$bogencoo.key), 
                    combogencoo_cooly_type = identical(testCombogen,bogencoo.key.list$combogencoo_cooly_type),
                    combogencoo_cooly_type_nukes = identical(testCombogenNuke,bogencoo.key.list$combogencoo_cooly_type_nukes) )
}else{outID<-rbind(outID,data.frame(YEAR=y,bogencoo.key = identical(testBogencoo,bogencoo.key.list$bogencoo.key), 
                                    combogencoo_cooly_type = identical(testCombogen,bogencoo.key.list$combogencoo_cooly_type),
                                    combogencoo_cooly_type_nukes = identical(testCombogenNuke,bogencoo.key.list$combogencoo_cooly_type_nukes) ))
}

  
}