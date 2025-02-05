#find path to test script
fileNAME<-"bogenTest.R"
cmd_args <- commandArgs(trailingOnly = FALSE)
cmd_args_trailing <- commandArgs(trailingOnly = TRUE)
leading_idx <- seq.int(from = 1, length.out = length(cmd_args) - 
                         length(cmd_args_trailing))
cmd_args <- cmd_args[leading_idx]
res <- gsub("^(?:--file=(.*)|.*)$", "\\1", cmd_args)
res <- tail(res[res != ""], 1)

#get data/script/model paths
testDataPath<-gsub(fileNAME,"",res,ignore.case=TRUE)
scriptPath<-gsub(paste0("test","\\\\",fileNAME),paste0("R","\\\\"),res,ignore.case=TRUE)
CDmodelPath<-gsub(paste0("associationScripts","\\\\","test","\\\\",fileNAME),
                  paste0("condenserDuty"),res,ignore.case=TRUE)

#check if devtools is installed
if (!"devtools" %in% rownames(installed.packages())){
  install.packages("devtools")
}

# #load condenserDuty path
devtools::install_deps(CDmodelPath, upgrade = "never")
devtools::load_all(CDmodelPath,recompile = FALSE)

#source association scripts
source(paste0(scriptPath,"import_bogen_data.R"))
source(paste0(scriptPath,"import_bocoo_data.R"))
source(paste0(scriptPath,"import_generator_data.R"))
source(paste0(scriptPath,"associate.R"))
source(paste0(scriptPath,"bogen_assocv_v2.R"))
source(paste0(scriptPath,"bocoo_associate.R"))
source(paste0(scriptPath,"compareKeys_2D.R"))
source(paste0(scriptPath,"compilePlantInfo_excel.R"))

#load data
bogen<-import_bogen_data(paste0(testDataPath,.Platform$file.sep,'2015_bogen.csv'))
bocoo<-import_bocoo_data(paste0(testDataPath,.Platform$file.sep,'2015_bocoo.csv'))
generator.data<-import_generator_data(paste0(testDataPath,.Platform$file.sep,'2015_GeneratorData.csv'))
sheet1GenFuelData<-import_sheet1_gen_and_fuel_data(paste0(testDataPath,.Platform$file.sep,'2015_GenerationAndFuel.csv'))
sheet4GenFuelData<-import_sheet4_gen_and_fuel_data(paste0(testDataPath,.Platform$file.sep,'2015_GenerationData.csv'))
boilerDesignData<-import_boiler_design_data(paste0(testDataPath,.Platform$file.sep,'2015_BoilerDesignInfo.csv'))
boilerFuelData<-import_boiler_fuel_data(paste0(testDataPath,.Platform$file.sep,'2015_BoilerFuelData.csv'))
plantList<-read.csv(paste0(testDataPath,.Platform$file.sep,'sheet3_key.csv'))
nukePlants<-read.csv(paste0(testDataPath,.Platform$file.sep,'nuke_combogencoo_key.csv'))
retiredGenerators<-openxlsx::read.xlsx(paste0(testDataPath,.Platform$file.sep,"3_1_Generator_Y2015.xlsx"),sheet="Retired and Canceled",startRow=2)
load(paste0(testDataPath,.Platform$file.sep,'test_bogen.key'))

#set good RPMs and get plantList
select_RPM<-c("CA", "CS", "CT", "ST",NA)
plantList$Plant.Code<-sapply(plantList$plant_bo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))

#run associations
bogen.key<-bogen_assocv.v2(analysisYear = 2015,bogen,generator.data,sheet4GenFuelData,boilerFuelData,boilerDesignData,retiredGenerators,
                           plantList$Plant.Code,select_RPM,vis_out=F,Fill.Gen.ID=T)
#get only key
bogen.key<-bogen.key$bogen.key

#compare to test output data
outStatus<-identical(bogen.key,test_bogen.key)
outStatus<-ifelse(outStatus,0,1)

message(paste0("\n\nexit status = ",outStatus))
if (outStatus==0){
message("bogenTest passed")
}else{
message("bogenTest FAILED non-zero exit status")
}
q(status=outStatus)




