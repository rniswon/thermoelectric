#'@title checkANYmissingNGCC
#'@description Finds natural gas combined cycle (NGCC) units that partially report Fuel Consumption
#' and/or Net Generation in the EIA Form-923 "Page 3 Boiler Fuel Data" and "Page 4 Generator Data"
#' but report all Fuel Consumption and Net Generation for the NGCC unit on EIA Form-923
#'"Page 1 Generation and Fuel Data".  \cr \cr 
#'This function looks for any 1 month of missing Fuel Consumption 
#'and Net Generation data which distiguishes it from `checkMissingNGCC()`.  In the year 2014, 
#'Fuel Consumption and Net Generation data reporting standards shifted mid-year resulting in all monthly
#'data being reported on EIA Form-923 "Page 3 Boiler Fuel Data" and "Page 4 Generator Data" for the
#'last few months of the year, but only partial reporting on EIA Form-923 "Page 3 Boiler Fuel Data" 
#'and "Page 4 Generator Data" in the earlier months with full reporting on EIA Form-923
#'"Page 1 Generation and Fuel Data". This function addresses this data reporting anomaly.\cr \cr  
#'Plant-Reported.Prime.Mover level bogens will be assigned in the form Plant.Code^NGCC. \cr \cr
#'@param generation.data data.frame with Plant.Code, Reported.Prime.Mover_923 
#'(Reported.Prime.Mover from the `generation.data` table in `inputData.list`) and 
#'Net.Generation(month) from EIA Form-923 "Page 4 Generator Data"
#'@param boilerFuelData data.frame with Plant.Code, Reported.Prime.Mover_bf.923 
#'(Reported.Prime.Mover from the `boilerFuelData` table in `inputData.list`) and 
#'Quantity.Of.Fuel.Consumed.(month) from EIA Form-923 "Page 3 Boiler Fuel Data"
#'@param gen_fuel_data data.frame stored in `inputData.list$gen_fuel_data` from EIA Form-923
#'"Page 1 Generation and Fuel Data"
#'@param analysisYear numeric value indicating the year of EIA data present in the `inputData.list`.
#'@return numeric vector of Natural Gas Combined Cycle Plant.Codes with inconsitent data reporting
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
#'analysisYear<-eia_year
#'
#'checkANYmissingNGCC(generation.data, boilerFuelData, gen_fuel_data,analysisYear)
#'@export
#'


checkANYmissingNGCC<-function(generation.data, boilerFuelData, gen_fuel_data,analysisYear){

  #define months vector
  months<-c("January","February","March","April","May",
            "June","July","August","September","October","November","December")
  
  #checkmissing function
  missFunc<-function(page3.4,page1){
    if (page3.4<=0 & page1>0){
      out<-1
    }else{
      out<-0
    }
  }
  
  #get all CSCTCA plants
  gen_fuel_data<-gen_fuel_data %>% dplyr::filter(Reported.Prime.Mover %in% c("CS","CT","CA"))
  generation.data<-generation.data %>% dplyr::filter(Reported.Prime.Mover %in% c("CS","CT","CA"))
   boilerFuelData<-boilerFuelData %>% dplyr::filter(Reported.Prime.Mover %in% c("CS","CT","CA"))

  #check quantity fuel consumption
  gen_fuel_dataQ<-gen_fuel_data %>% dplyr::select(Plant.Code,Reported.Prime.Mover,dplyr::starts_with("Quantity"))
  boilerFuelDataQ<-boilerFuelData %>% dplyr::select(Plant.Code,Reported.Prime.Mover,dplyr::starts_with("Quantity"))
  names(boilerFuelDataQ)[2]<-"Reported.Prime.Mover"
  
  names(gen_fuel_dataQ)[3:length(gen_fuel_dataQ)]<-months
  names(boilerFuelDataQ)[3:length(boilerFuelDataQ)]<-months
  
  gen_fuel_dataQ<-reshape2::melt(gen_fuel_dataQ,id.vars=c("Plant.Code","Reported.Prime.Mover"))
  boilerFuelDataQ<-reshape2::melt(boilerFuelDataQ,id.vars=c("Plant.Code","Reported.Prime.Mover"))
  
  names(gen_fuel_dataQ)[3]<-"month"
  names(boilerFuelDataQ)[3]<-"month"
  
  #sum by RPM
  gen_fuel_dataQ<-gen_fuel_dataQ %>% dplyr::ungroup() %>%
  dplyr::group_by(Plant.Code,Reported.Prime.Mover,month) %>%
    dplyr::summarise(sumQpage1 = sum(value,na.rm = T))
  boilerFuelDataQ<-boilerFuelDataQ %>% dplyr::ungroup() %>%
    dplyr::group_by(Plant.Code,Reported.Prime.Mover,month) %>%
    dplyr::summarise(sumQpage3 = sum(value,na.rm = T))
  
  #join page 3 and page 1
  boilerFuelDataQ<-dplyr::left_join(gen_fuel_dataQ,boilerFuelDataQ,
                                    by=c("Plant.Code", "Reported.Prime.Mover", "month"))
  boilerFuelDataQ$sumQpage1<-ifelse(is.na(boilerFuelDataQ$sumQpage1),0,boilerFuelDataQ$sumQpage1)
  boilerFuelDataQ$sumQpage3<-ifelse(is.na(boilerFuelDataQ$sumQpage3),0,boilerFuelDataQ$sumQpage3)
  
  #flag missing page 3 data
  boilerFuelDataQ$flag_NGCC<-ifelse(boilerFuelDataQ$sumQpage3<=0 & boilerFuelDataQ$sumQpage1>0,1,0)
  boilerFuelDataQ<-boilerFuelDataQ %>% dplyr::filter(flag_NGCC==1)
  NGCCplants<-unique(boilerFuelDataQ$Plant.Code)
  
  
  #check generation
  gen_fuel_dataNG<-gen_fuel_data %>% dplyr::select(Plant.Code,Reported.Prime.Mover,dplyr::starts_with("Netgen"))
  generation.dataNG<-generation.data %>% dplyr::select(Plant.Code,Reported.Prime.Mover,dplyr::starts_with("Net.Generation."),
                                                -Net.Generation.Year.To.Date)

  names(generation.dataNG)[2]<-"Reported.Prime.Mover"
  
  names(gen_fuel_dataNG)[3:length(gen_fuel_dataNG)]<-months
  names(generation.dataNG)[3:length(generation.dataNG)]<-months
  
  gen_fuel_dataNG<-reshape2::melt(gen_fuel_dataNG,id.vars=c("Plant.Code","Reported.Prime.Mover"))
  generation.dataNG<-reshape2::melt(generation.dataNG,id.vars=c("Plant.Code","Reported.Prime.Mover"))
  
  names(gen_fuel_dataNG)[3]<-"month"
  names(generation.dataNG)[3]<-"month"
  
  #sum by RPM
  gen_fuel_dataNG<-gen_fuel_dataNG %>% dplyr::ungroup() %>%
    dplyr::group_by(Plant.Code,Reported.Prime.Mover,month) %>%
    dplyr::summarise(sumNGpage1 = sum(value,na.rm = T))
  generation.dataNG<-generation.dataNG %>% dplyr::ungroup() %>%
    dplyr::group_by(Plant.Code,Reported.Prime.Mover,month) %>%
    dplyr::summarise(sumNGpage4 = sum(value,na.rm = T))
  
  #join page 4 and page 1
  generation.dataNG<-dplyr::left_join(gen_fuel_dataNG,generation.dataNG,
                                      by=c("Plant.Code", "Reported.Prime.Mover", "month"))
  generation.dataNG$sumNGpage1<-ifelse(is.na(generation.dataNG$sumNGpage1),0,generation.dataNG$sumNGpage1)
  generation.dataNG$sumNGpage4<-ifelse(is.na(generation.dataNG$sumNGpage4),0,generation.dataNG$sumNGpage4)
  
  
  
  #flag missing page 4 data
  generation.dataNG$flag_NGCC<-ifelse(generation.dataNG$sumNGpage4<=0 & generation.dataNG$sumNGpage1>0,1,0)
  generation.dataNG<-generation.dataNG %>% dplyr::filter(flag_NGCC==1)
  NGCCplants<-unique(NGCCplants,generation.dataNG$Plant.Code)

  
  return(NGCCplants)
  
  
  }#end function