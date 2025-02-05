years<-seq(2008,2020,1)

for (y in years){

  inputData_path<-"./cd/Data/"
  pathWrite<-"D:/plant_association/Output/"
  
  #ImportModule
  eia_year<-y
  dest<-paste0("D:/EIA_",eia_year)
  path_InputData_Metafile<-paste0("D:/WBEEPtest/EIA_crosswalks_complete/UserControlCrosswalk",
                                  eia_year,".xlsx")
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
  
  
  plantList<-openxlsx::read.xlsx("D:/WBEEPtest/TE_MPL_allyears.xlsx",sheet="plant years vert")
  plantList$Plant.Code<-plantList$EIA_PLANT_ID
  plantList<-plantList[plantList$year==eia_year,]
  
  
  #run import module
  #eia_webpull(eia_year,dest)
  inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,path_outputCSV)
  
  inputData.list$plantList<-plantList
  
  bogen.out.list<-executeBogenAssoc(inputData_path,pathWrite,inputData.list,select_RPM=c("CA", "CS", "CT", "ST",NA), eia_year)
  
  testbogen.out.list<-bogen.out.list$bogen.key

 
 invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),   # Unload add-on packages
                  detach,
                  character.only = TRUE, unload = TRUE))
  ####new package
  library(importThermoEIA)
  library(EIAplantAssociation)
  
  
  #select Reported.Prime.Movers to include
  select_RPM<-c("CA", "CS", "CT", "ST",NA)
  
  #load MasterPlantList
  utils::data("masterPlantList")
  plantList<-masterPlantList[masterPlantList$year==eia_year,]
  
inputData.list<-inputData.list[which(names(inputData.list)!="plantList")]
  #run boiler-generator associations
  bogen.key<-bogen_associate(analysisYear = eia_year,inputData.list,
                             plantList,select_RPM)
  
  
  
  testBogen<-testbogen.out.list$bogen.key %>% dplyr::select(dplyr::all_of(names(bogen.key$bogen.key)))
 
  if (y==2008){
    outID<-data.frame(year=y,identical=identical(testBogen,bogen.key$bogen.key))
  }else{
    outID<-rbind(outID,data.frame(year=y,identical=identical(testBogen,bogen.key$bogen.key)))
  }

  
}
write.csv(outID,file="D:/outID.csv",row.names = F)