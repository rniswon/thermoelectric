#'@title checkDecember
#'@description Finds plants that report all Fuel Consumption and/or Net Generation for the
#'year in the month of December.  Monthly data for these plants can be found on EIA Form-923
#'"Page 1 Generation and Fuel Data".  Plant-Reported.Prime.Mover level bogens will be assigned
#'in the form Plant.Code^NGCC for natural gas combined cycle units or Plant.Code^ST for
#'steam units. \cr \cr
#'@param FuelHeatByLine data.frame with Plant.Code, Reported.Prime.Mover_bf.923 
#'(Reported.Prime.Mover from the `boilerFuelData` table in `inputData.list`) and Fuel.Heat.(month)
#'from EIA Form-923 "Page 3 Boiler Fuel Data"
#'@param NetGenByLine data.frame with Plant.Code, Reported.Prime.Mover 
#'(Reported.Prime.Mover from the `generation.data` table in `inputData.list`) and 
#'Net.Generation(month) from EIA Form-923 "Page 4 Generator Data"
#'@param plantList numeric vector of Plant.Codes to consider
#'@param gen_fuel_data data.frame stored in `inputData.list$gen_fuel_data` from EIA Form-923
#'"Page 1 Generation and Fuel Data"
#'@param select_RPM vector of character strings indicating which Reported.Prime.Mover types should be
#'included in the boiler-generator association output.
#'@return numeric vector of Plant.Codes that report annual Fuel Consumption and/or Net Generation
#'in the month of December.
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
#'FuelHeatByLine<-boilerFuelData
#'for(m in months){
#'fuelHeatstr<-paste0("FuelHeatByLine<-FuelHeatByLine %>% dplyr::mutate(Fuel.Heat.",m,"=Quantity.Of.Fuel.Consumed.",m,"*MMbtu.Per.Unit.",m,")")
#'eval(parse(text=fuelHeatstr))
#'}
#'
#'#add required column Reported.Prime.Mover_bf.923
#'FuelHeatByLine$Reported.Prime.Mover_bf.923<-FuelHeatByLine$Reported.Prime.Mover
#'
#'#get masterPlantList
#'data("masterPlantList")
#'plantList<-masterPlantList[masterPlantList$year==2015,]$Plant.Code
#'
#'#define NetGenByLine
#'NetGenByLine<-generation.data
#'
#'checkDecember(FuelHeatByLine,NetGenByLine,gen_fuel_data,
#'              plantList,select_RPM=c("CA", "CS", "CT", "ST",NA))
#'@export
#'

checkDecember<-function(FuelHeatByLine,NetGenByLine,gen_fuel_data,plantList,select_RPM){

  #get only Reported.Prime.Movers selected for analysis
FuelHeatByLine<-FuelHeatByLine %>% dplyr::filter(Reported.Prime.Mover_bf.923 %in% select_RPM)
NetGenByLine<-NetGenByLine %>% dplyr::filter(Reported.Prime.Mover %in% select_RPM)
gen_fuel_data<-gen_fuel_data %>% dplyr::filter(Reported.Prime.Mover %in% select_RPM)

#set months vector
months<-c("January","February","March","April","May",
          "June","July","August","September","October","November","December")

#sum Fuel Heat and Net Gen by Plant
decPlant<-numeric(0)
#plant level decembrist
for (m in months){
  #sum plant
  fuelHeatstr<-paste0("tempPlantFH<-FuelHeatByLine %>% dplyr::group_by(Plant.Code) %>% 
                        dplyr::summarize(Fuel.Heat.",m,"=sum(Fuel.Heat.",m,",na.rm=TRUE))")
  eval(parse(text=fuelHeatstr))
  netGenstr<-paste0("tempPlantNG<-NetGenByLine %>% dplyr::group_by(Plant.Code) %>% 
                        dplyr::summarize(Net.Generation.",m,"=sum(Net.Generation.",m,",na.rm=TRUE))")
  eval(parse(text=netGenstr))
  
  
  if (m=="January"){
    sumPlantFH<-tempPlantFH 
    sumPlantNG<-tempPlantNG

  }else{
    sumPlantFH<-merge(sumPlantFH,tempPlantFH,by="Plant.Code",all=T)
    sumPlantNG<-merge(sumPlantNG,tempPlantNG, by=c("Plant.Code"),all=T)

  }
  
}#end months

# find plants with only December FuelHeat and NetGen
for (p in plantList){
  #plant
  subPlantFH<-sumPlantFH %>% dplyr::filter(Plant.Code==p) %>%
    dplyr::select(dplyr::contains("Fuel.Heat"))
  decPlantFH<-subPlantFH$Fuel.Heat.December
  subPlantFH<-subPlantFH %>% dplyr::select(-Fuel.Heat.December)
  if (nrow(subPlantFH)==0){
    subPlantFH<-0
  }else{
    subPlantFH<-ifelse(any(abs(subPlantFH)>0),1,0) 
  }
  
  
  subPlantNG<-sumPlantNG %>% dplyr::filter(Plant.Code==p) %>%
    dplyr::select(dplyr::contains("Net.Generation."))
  decPlantNG<-subPlantNG$Net.Generation.December
  subPlantNG<-subPlantNG %>% dplyr::select(-Net.Generation.December)
  if (nrow(subPlantNG)==0){
    subPlantNG<-0
  }else{
    subPlantNG<-ifelse(any(abs(subPlantNG)>0),1,0) 
  }   
  
  
  if(length(decPlantFH)!=0){
    if (subPlantFH==0 & decPlantFH>0){
      if (length(decPlantNG)!=0){
        if (subPlantNG==0 & decPlantNG>0){
          decPlant<-c(decPlant,p)
        }
      }else{
        decPlant<-c(decPlant,p)
      }
      
      
    }
  }else if (length(decPlantNG)!=0){
    if (subPlantNG==0 & decPlantNG>0){
      decPlant<-c(decPlant,p)
    }
  }
  
}#each plant

decPlant<-unique(decPlant)

return(decPlant)
}#end func