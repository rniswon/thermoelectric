checkMissingCSCT<-function(generation.data, boilerFuelData, gen_fuel_data,analysisYear){
  #get all CSCT plants
  gen_fuel_data<-gen_fuel_data %>% filter(Reported.Prime.Mover %in% c("CS","CT"))
  gen_fuel_data<-gen_fuel_data %>% select(Plant.Code,Reported.Prime.Mover)
  gen_fuel_data<-gen_fuel_data[!duplicated(gen_fuel_data),]
  gen_fuel_data$IncludedPage1<-rep(1,nrow(gen_fuel_data))

  boilerFuelData<-boilerFuelData %>% filter(Reported.Prime.Mover_bf.923 %in% c("CS","CT"))
  boilerFuelData<-boilerFuelData %>% select(Plant.Code,Reported.Prime.Mover_bf.923)
  boilerFuelData<-boilerFuelData[!duplicated(boilerFuelData),]
  boilerFuelData$IncludedPage3<-rep(1,nrow(boilerFuelData))
  
  generation.data<-generation.data %>% filter(Reported.Prime.Mover_923 %in% c("CS","CT"))
  generation.data<-generation.data %>% select(Plant.Code,Reported.Prime.Mover_923)
  generation.data<-generation.data[!duplicated(generation.data),]
  generation.data$IncludedPage4<-rep(1,nrow(generation.data))
  
  #join page 3
  gen_fuel_data<-left_join(gen_fuel_data,boilerFuelData,by=c("Plant.Code"="Plant.Code",
                                                             "Reported.Prime.Mover"="Reported.Prime.Mover_bf.923"))
  gen_fuel_data$IncludedPage3<-ifelse(is.na(gen_fuel_data$IncludedPage3),0,gen_fuel_data$IncludedPage3)
  
  #join page 4
  gen_fuel_data<-left_join(gen_fuel_data,generation.data,by=c("Plant.Code"="Plant.Code",
                                                              "Reported.Prime.Mover"="Reported.Prime.Mover_923"))
  gen_fuel_data$IncludedPage3<-ifelse(is.na(gen_fuel_data$IncludedPage3),0,gen_fuel_data$IncludedPage3)
  gen_fuel_data$IncludedPage4<-ifelse(is.na(gen_fuel_data$IncludedPage4),0,gen_fuel_data$IncludedPage4)
  
  gen_fuel_data$IncludePage3AndPage4<-ifelse(gen_fuel_data$IncludedPage3==1 & gen_fuel_data$IncludedPage4==1,1,0)
  
  #add year
  gen_fuel_data$YEAR<-rep(analysisYear,nrow(gen_fuel_data))
  
  #filter only missing
  gen_fuel_data<-gen_fuel_data %>% filter(IncludePage3AndPage4!=1)
  
  return(gen_fuel_data)
}