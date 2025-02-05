#load CD
system(paste0("Rscript ","../../cd/loadCD.R"), wait = TRUE)
devtools::load_all("../../cd", recompile = FALSE)

pathWrite<-tempdir()
pathData<-"../../cd/Data"

#ImportModule
dest<-tempdir()
eia_year<-2015
path_InputData_Metafile<-"../../importModule/userControlFiles/UserControlCrosswalk_2015.xlsx"
path_EIAInputData<-paste0(dest,"/",eia_year)
outputCSV<-TRUE
path_outputCSV<-dest



#########DO NOT EDIT BELOW THIS LINE#########
devtools::install_deps("../../cd", upgrade = "never")
devtools::load_all("../../cd",recompile = FALSE)
devtools::install_deps("../../plant_association", upgrade = "never")
devtools::load_all("../../plant_association",recompile = FALSE)



plantList<-openxlsx::read.xlsx(paste0(pathData,"/TE_MPL_allyears.xlsx"),sheet="plant years vert")
plantList$Plant.Code<-plantList$EIA_PLANT_ID
plantList<-plantList[plantList$year==eia_year,]

nukePlants<-read.csv(paste0(pathData,'/nuke_combogencoo_key.csv'))

#run import module
eia_webpull(eia_year,dest)
inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,path_outputCSV)

inputData.list$plantList<-plantList
inputData.list$nukePlants<-nukePlants

#run bogen associations
bogen.out.list<-executeBogenAssoc(inputData_path=pathData,pathWrite,inputData.list,select_RPM=c("CA", "CS", "CT", "ST",NA), eia_year)

#read test_bogen.key
load("../../plant_association/test/test_bogen.key")

# compare to test output data
outStatus<-identical(bogen.out.list,test_bogen.key)
outStatus<-ifelse(outStatus,0,1)

message(paste0("\n\nexit status = ", outStatus))
if (outStatus == 0) {
  message("bogenTest passed")
} else {
  message("bogenTest FAILED non-zero exit status")
}
q(status = outStatus)