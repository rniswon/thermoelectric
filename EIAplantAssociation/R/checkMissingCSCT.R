#'@title checkMissingCSCT
#'@description Finds natural gas combined cycle (NGCC) units that partially report Fuel Consumption
#' and/or Net Generation in the EIA Form-923 "Page 3 Boiler Fuel Data" and "Page 4 Generator Data"
#' but report all Fuel Consumption and Net Generation for the NGCC unit on EIA Form-923
#'"Page 1 Generation and Fuel Data".  This function looks for entire rows of missing Fuel Consumption and
#'Net Generation. Plant-Reported.Prime.Mover level bogens will be assigned
#'in the form Plant.Code^NGCC. \cr \cr
#'@param generation.data data.frame with Plant.Code, Reported.Prime.Mover_923 
#'(Reported.Prime.Mover from the `generation.data` table in `inputData.list`) and 
#'Net.Generation(month) from EIA Form-923 "Page 4 Generator Data"
#'@param boilerFuelData data.frame with Plant.Code, Reported.Prime.Mover_bf.923 
#'(Reported.Prime.Mover from the `boilerFuelData` table in `inputData.list`)
#'from EIA Form-923 "Page 3 Boiler Fuel Data"
#'@param gen_fuel_data data.frame stored in `inputData.list$gen_fuel_data` from EIA Form-923
#'"Page 1 Generation and Fuel Data"
#'@param analysisYear numeric value indicating the year of EIA data present in the `inputData.list`.
#'@return data.frame of Natural Gas Combined Cycle plants with inconsitent data reporting
#'in the EIA Form-923 across "Page 3 Boiler Fuel Data", "Page 4 Generator Data", and "Page 1 
#'Generation and Fuel Data".  If data is present on "Page 1 Generation and Fuel Data" and not in
#'"Page 3 Boiler Fuel Data" and/or "Page 4 Generator Data", the boiler-generator association will take
#'the form Plant.Code^NGCC and  the `condenserDuty` package will calculate condenser duty using 
#'"Page 1 Generation and Fuel Data" 
#'@examples
#'#set arguments
#'dest<-tempdir()
#'eia_year<-2015
#'path_InputData_Metafile<-paste0(dest,.Platform$file.sep,"UserControlCrosswalk2015.xlsx")
#'path_EIAInputData<-paste0(dest,.Platform$file.sep,eia_year)
#'outputCSV<-FALSE
#'path_outputCSV<-dest
#'
#'#use crosswalk2015 object
#'useStandardCrosswalk<-TRUE
#'prepCondenserDuty<-TRUE
#'
#'#pull data from web
#'importThermoEIA::eia_webpull(eia_year,dest)
#'
#'
#'#run import using crosswalk2015
#'inputData.list<-importThermoEIA::import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,
#'                               path_outputCSV,prepCondenserDuty = TRUE,eia_year=eia_year,
#'                               useStandardCrosswalk=useStandardCrosswalk)
#'
#'#extract elements of the inputData.list
#'list2env(inputData.list,envir = parent.frame())
#'
#'#define months
#'months<-c("January","February","March","April","May",
#'           "June","July","August","September","October","November","December")
#'           
#'
#'#add required columns Reported.Prime.Mover_bf.923 and Reported.Prime.Mover_923
#'boilerFuelData$Reported.Prime.Mover_bf.923<-boilerFuelData$Reported.Prime.Mover
#'generation.data$Reported.Prime.Mover_923<-generation.data$Reported.Prime.Mover
#'
#'#set analysis Year
#'analysisYear<-2015
#'
#'checkMissingCSCT(generation.data, boilerFuelData, gen_fuel_data,analysisYear)
#'@export
#'


checkMissingCSCT<-function(generation.data, boilerFuelData, gen_fuel_data,analysisYear){
  #get all CSCT plants
  gen_fuel_data<-gen_fuel_data %>% dplyr::filter(Reported.Prime.Mover %in% c("CS","CT"))
  gen_fuel_data<-gen_fuel_data %>% dplyr::select(Plant.Code,Reported.Prime.Mover)
  gen_fuel_data<-gen_fuel_data[!duplicated(gen_fuel_data),]
  gen_fuel_data$IncludedPage1<-rep(1,nrow(gen_fuel_data))

  boilerFuelData<-boilerFuelData %>% dplyr::filter(Reported.Prime.Mover_bf.923 %in% c("CS","CT"))
  boilerFuelData<-boilerFuelData %>% dplyr::select(Plant.Code,Reported.Prime.Mover_bf.923)
  boilerFuelData<-boilerFuelData[!duplicated(boilerFuelData),]
  boilerFuelData$IncludedPage3<-rep(1,nrow(boilerFuelData))
  
  generation.data<-generation.data %>% dplyr::filter(Reported.Prime.Mover_923 %in% c("CS","CT"))
  generation.data<-generation.data %>% dplyr::select(Plant.Code,Reported.Prime.Mover_923)
  generation.data<-generation.data[!duplicated(generation.data),]
  generation.data$IncludedPage4<-rep(1,nrow(generation.data))
  
  #join page 3
  gen_fuel_data<-dplyr::left_join(gen_fuel_data,boilerFuelData,by=c("Plant.Code"="Plant.Code",
                                                             "Reported.Prime.Mover"="Reported.Prime.Mover_bf.923"))
  gen_fuel_data$IncludedPage3<-ifelse(is.na(gen_fuel_data$IncludedPage3),0,gen_fuel_data$IncludedPage3)
  
  #join page 4
  gen_fuel_data<-dplyr::left_join(gen_fuel_data,generation.data,by=c("Plant.Code"="Plant.Code",
                                                              "Reported.Prime.Mover"="Reported.Prime.Mover_923"))
  gen_fuel_data$IncludedPage3<-ifelse(is.na(gen_fuel_data$IncludedPage3),0,gen_fuel_data$IncludedPage3)
  gen_fuel_data$IncludedPage4<-ifelse(is.na(gen_fuel_data$IncludedPage4),0,gen_fuel_data$IncludedPage4)
  
  gen_fuel_data$IncludePage3AndPage4<-ifelse(gen_fuel_data$IncludedPage3==1 & gen_fuel_data$IncludedPage4==1,1,0)
  
  #add year
  gen_fuel_data$YEAR<-rep(analysisYear,nrow(gen_fuel_data))
  
  #filter only missing
  gen_fuel_data<-gen_fuel_data %>% dplyr::filter(IncludePage3AndPage4!=1)
  
  return(gen_fuel_data)
}