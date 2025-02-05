#'@title sumGenFuelData
#'@description Calculates Fuel Heat and Net Generation at the Plant.Code-Reported.Prime.Mover
#'level using EIA Form-923 "Page 1 Generation and Fuel Data" and appends it to `fuelHeatData`
#'and `generation.data` \cr \cr
#'@param fuelHeatData data.frame with Plant.Code, Reported.Prime.Mover_bf.923 
#'(Reported.Prime.Mover from the `boilerFuelData` table in `inputData.list`) and Fuel.Heat.(month)
#'from EIA Form-923 "Page 3 Boiler Fuel Data"
#'@param generation.data data.frame with Plant.Code, Reported.Prime.Mover 
#'(Reported.Prime.Mover from the `generation.data` table in `inputData.list`) and 
#'Net.Generation(month) from EIA Form-923 "Page 4 Generator Data"
#'@param gen_fuel_data data.frame stored in `inputData.list$gen_fuel_data` from EIA Form-923
#'"Page 1 Generation and Fuel Data" subset to only include plants that will be assigned a 
#'Plant.Code-Reported.Prime.Mover level bogen indicating that EIA Form-923 "Page 1 Generation and 
#'Fuel Data" should be used to calculate condenser duty.
#'@return list of data.frames including:
#'#'\tabular{ll}{`gen_fuel_data` \tab data.frame of Fuel Heat and Net Generation at the 
#'Plant.Code-Reported.Prime.Mover level.  \cr \cr
#'`bf.923.ungrouped` \tab data.frame of Fuel Heat at the Plant.Code-boiler level for units not
#'selected in the `gen_fuel_data` argument and Fuel Heat at the Plant.Code-Reported.Prime.Mover
#'level for units selected in the `gen_fuel_data` argument.\cr \cr
#'`generation.data.line124` \tab data.frame of Net Generation at the Plant.Code-generator level
#'for units not selected in the `gen_fuel_data` argument and Net Generation at the 
#'Plant.Code-Reported.Prime.Mover level for units selected in the `gen_fuel_data` argument.\cr \cr
#'}
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
#'#calculate Fuel Heat at the Boiler.ID level
#'fuelHeatData<-boilerFuelData
#'for(m in months){
#'fuelHeatstr<-paste0("fuelHeatData<-fuelHeatData %>% dplyr::mutate(Fuel.Heat.",m,"=Quantity.Of.Fuel.Consumed.",m,"*MMbtu.Per.Unit.",m,")")
#'eval(parse(text=fuelHeatstr))
#'}
#'
#'#add required column Reported.Prime.Mover_bf.923
#'fuelHeatData$Reported.Prime.Mover_bf.923<-fuelHeatData$Reported.Prime.Mover
#'
#'#set analysis Year
#'analysisYear<-2015
#'
#'sumGenFuelData(gen_fuel_data,fuelHeatData, generation.data,analysisYear)
#'@export
#'

sumGenFuelData<-function(gen_fuel_data,fuelHeatData, generation.data,analysisYear){
  
  months<-c("January","February","March","April","May",
             "June","July","August","September","October","November","December")
  
  #calculate Fuel Heat and sum Fuel Heat and Net Generation
  #at the Plant.Code-Reported.Prime.Mover level in gen_fuel_data
  for(m in months){
    fuelHeatstr<-paste0("gen_fuel_data<-gen_fuel_data %>% dplyr::group_by(Plant.Code,Reported.Prime.Mover) %>% 
                        dplyr::mutate(Fuel.Heat.",m,"=Quantity.",m,"*MMBtuPer_Unit.",m,")")
    eval(parse(text=fuelHeatstr))
    fuelHeatstr<-paste0("genFuelsumPlant<-gen_fuel_data %>% dplyr::group_by(Plant.Code,Reported.Prime.Mover) %>% 
                        dplyr::summarize(PlantLine.Total.Fuel.Heat.",m,"=sum(Fuel.Heat.",m,",na.rm=TRUE))")
    eval(parse(text=fuelHeatstr))
    
    netGenstr<-paste0("genFuelsumPlantNG<-gen_fuel_data %>% dplyr::group_by(Plant.Code,Reported.Prime.Mover) %>%
                        dplyr::summarize(PlantLine.Total.Net.Generation.",m,"=sum(Netgen.",m,",na.rm=T))")
    eval(parse(text=netGenstr))
    
    fuelHeatstr<-paste0("genFuelsumPlant2<-gen_fuel_data %>% dplyr::group_by(Plant.Code,Reported.Prime.Mover,Reported.Fuel.Type.Code) %>% 
                        dplyr::summarize(Fuel.Heat.",m,"=sum(Fuel.Heat.",m,",na.rm=TRUE))")
    eval(parse(text=fuelHeatstr))
    
    if (m=="January"){
      sumgenFuel<-genFuelsumPlant
      sumgenFuelLine<-genFuelsumPlant2
      sumgenFuel_netGen<-genFuelsumPlantNG
      
    }else{
      sumgenFuel<-merge(sumgenFuel,genFuelsumPlant, by=c("Plant.Code","Reported.Prime.Mover"),all=T)
      sumgenFuelLine<-merge(sumgenFuelLine,genFuelsumPlant2, by=c("Plant.Code","Reported.Prime.Mover",
                                                                  "Reported.Fuel.Type.Code"),all=T)
      sumgenFuel_netGen<-merge(sumgenFuel_netGen,genFuelsumPlantNG, by=c("Plant.Code","Reported.Prime.Mover"),all=T)
    }
    
    
  } 
  sumTotNGgen_fuel_data<-gen_fuel_data %>% dplyr::group_by(Plant.Code,Reported.Prime.Mover) %>%
    dplyr::summarize(Net.Generation.mwh=sum(Net.Generation..Megawatthours.,na.rm=T))
  
  gen_fuel_data<-merge(sumgenFuel,sumTotNGgen_fuel_data,by=c("Plant.Code","Reported.Prime.Mover"))
  gen_fuel_data<-merge(gen_fuel_data,sumgenFuel_netGen,by=c("Plant.Code","Reported.Prime.Mover"))
  names(gen_fuel_data)[names(gen_fuel_data)=="Reported.Prime.Mover"]<-"Reported.Prime.Mover_page1"
  
  #add gen_fuel lines to fuelHeatData and generation.data
  sumgenFuelLine$Boiler.ID<-rep(as.character(NA),nrow(sumgenFuelLine))
  sumgenFuelLine$YEAR<-rep(as.integer(analysisYear),nrow(sumgenFuelLine))
  
  
  if (analysisYear>2005){
    fuelHeatData<-fuelHeatData %>% dplyr::select(Plant.Code,Boiler.ID,Reported.Prime.Mover_bf.923,
                                                  Reported.Fuel.Type.Code,dplyr::contains("Fuel.Heat"),YEAR) 
    sumgenFuelLine$Reported.Prime.Mover_bf.923<-sumgenFuelLine$Reported.Prime.Mover
  }else{
    fuelHeatData<-fuelHeatData %>% dplyr::select(Plant.Code,Boiler.ID,
                                                  Reported.Fuel.Type.Code,dplyr::contains("Fuel.Heat"),YEAR) 
  }
  sumgenFuelLine<-sumgenFuelLine %>% dplyr::select(names(fuelHeatData),-Reported.Prime.Mover)
  
  fuelHeatData<-rbind(fuelHeatData,sumgenFuelLine)
  fuelHeatData<-fuelHeatData[order(as.numeric(fuelHeatData$Plant.Code),fuelHeatData$Boiler.ID),]
  bf.923.ungrouped<-fuelHeatData
  
  sumTotNGgen_fuel_data$Net.Generation.Year.To.Date<-sumTotNGgen_fuel_data$Net.Generation.mwh
  sumgenFuel_netGen<-merge(sumgenFuel_netGen,sumTotNGgen_fuel_data %>% 
                             dplyr::select(Plant.Code,Reported.Prime.Mover,Net.Generation.Year.To.Date),
                           by=c("Plant.Code","Reported.Prime.Mover"))
  names(sumgenFuel_netGen)[regexpr("PlantLine.Total.Net.Generation.",
                                   names(sumgenFuel_netGen))>0]<-names(generation.data)[(regexpr("Net.Generation.",
                                                                                                        names(generation.data))>0) & 
                                                                                                  names(generation.data)!="Net.Generation.Year.To.Date"]
  if (analysisYear>2005){
    generation.data<-generation.data %>% dplyr::select(Plant.Code,Generator.ID,Reported.Prime.Mover,
                                                                dplyr::contains("Net.Generation"),YEAR)
  }else{
    generation.data<-generation.data %>% dplyr::select(Plant.Code,Generator.ID,
                                                                dplyr::contains("Net.Generation"),YEAR)
  }
  sumgenFuel_netGen$Generator.ID<-rep(as.character(NA),nrow(sumgenFuel_netGen))
  sumgenFuel_netGen$YEAR<-rep(as.integer(analysisYear),nrow(sumgenFuel_netGen))
  
  sumgenFuel_netGen<-sumgenFuel_netGen %>% dplyr::select(names(generation.data))
  
  generation.data<-rbind(generation.data,sumgenFuel_netGen)
  generation.data<-generation.data[order(as.numeric(generation.data$Plant.Code),
                                                         generation.data$Generator.ID),]
  generation.data.line124<-generation.data
  
  #compile output into a named.list
  out.list<-named.list(gen_fuel_data,bf.923.ungrouped,generation.data.line124)
  
  return(out.list)
}