checkANYmissingNGCC<-function(generation.data, boilerFuelData, gen_fuel_data,analysisYear){

  months<-c("January","February","March","April","May",
            "June","July","August","September","October","November","December")
  
  #checkmissnig 
  missFunc<-function(page3.4,page1){
    if (page3.4<=0 & page1>0){
      out<-1
    }else{
      out<-0
    }
  }
  
  #get all CSCTCA plants
  gen_fuel_data<-gen_fuel_data %>% filter(Reported.Prime.Mover %in% c("CS","CT","CA"))
  #boilerFuelData<-boilerFuelData %>% filter(Reported.Prime.Mover_bf.923 %in% c("CS","CT","CA"))
  generation.data<-generation.data %>% filter(Reported.Prime.Mover %in% c("CS","CT","CA"))
   boilerFuelData<-boilerFuelData %>% filter(Reported.Prime.Mover %in% c("CS","CT","CA"))
  # generation.data<-generation.data %>% filter(Reported.Prime.Mover %in% c("CS","CT","CA"))
  
  #check quantity
  gen_fuel_dataQ<-gen_fuel_data %>% select(Plant.Code,Reported.Prime.Mover,starts_with("Quantity"))
  #boilerFuelDataQ<-boilerFuelData %>% select(Plant.Code,Reported.Prime.Mover_bf.923,starts_with("Quantity"))
  boilerFuelDataQ<-boilerFuelData %>% select(Plant.Code,Reported.Prime.Mover,starts_with("Quantity"))
  names(boilerFuelDataQ)[2]<-"Reported.Prime.Mover"
  
  names(gen_fuel_dataQ)[3:length(gen_fuel_dataQ)]<-months
  names(boilerFuelDataQ)[3:length(boilerFuelDataQ)]<-months
  
  gen_fuel_dataQ<-melt(gen_fuel_dataQ,id.vars=c("Plant.Code","Reported.Prime.Mover"))
  boilerFuelDataQ<-melt(boilerFuelDataQ,id.vars=c("Plant.Code","Reported.Prime.Mover"))
  
  names(gen_fuel_dataQ)[3]<-"month"
  names(boilerFuelDataQ)[3]<-"month"
  
  #sum by RPM
  gen_fuel_dataQ<-gen_fuel_dataQ %>% ungroup() %>%
  dplyr::group_by(Plant.Code,Reported.Prime.Mover,month) %>%
    dplyr::summarise(sumQpage1 = sum(value,na.rm = T))
  boilerFuelDataQ<-boilerFuelDataQ %>% ungroup() %>%
    dplyr::group_by(Plant.Code,Reported.Prime.Mover,month) %>%
    dplyr::summarise(sumQpage3 = sum(value,na.rm = T))
  
  #join page 3 and page 1
  #boilerFuelDataQ<-left_join(boilerFuelDataQ,gen_fuel_dataQ)
  boilerFuelDataQ<-left_join(gen_fuel_dataQ,boilerFuelDataQ)
  boilerFuelDataQ$sumQpage1<-ifelse(is.na(boilerFuelDataQ$sumQpage1),0,boilerFuelDataQ$sumQpage1)
  boilerFuelDataQ$sumQpage3<-ifelse(is.na(boilerFuelDataQ$sumQpage3),0,boilerFuelDataQ$sumQpage3)
  
  #flag missing page 3 data
  boilerFuelDataQ$flag_NGCC<-ifelse(boilerFuelDataQ$sumQpage3<=0 & boilerFuelDataQ$sumQpage1>0,1,0)
  boilerFuelDataQ<-boilerFuelDataQ %>% filter(flag_NGCC==1)
  NGCCplants<-unique(boilerFuelDataQ$Plant.Code)
  
  
  #check generation
  gen_fuel_dataNG<-gen_fuel_data %>% select(Plant.Code,Reported.Prime.Mover,starts_with("Netgen"))
  generation.dataNG<-generation.data %>% select(Plant.Code,Reported.Prime.Mover,starts_with("Net.Generation."),
                                                -Net.Generation.Year.To.Date)
  # generation.dataNG<-generation.data %>% select(Plant.Code,Reported.Prime.Mover,starts_with("Net.Generation."),
  #                                               -Net.Generation.Year.To.Date)
  names(generation.dataNG)[2]<-"Reported.Prime.Mover"
  
  names(gen_fuel_dataNG)[3:length(gen_fuel_dataNG)]<-months
  names(generation.dataNG)[3:length(generation.dataNG)]<-months
  
  gen_fuel_dataNG<-melt(gen_fuel_dataNG,id.vars=c("Plant.Code","Reported.Prime.Mover"))
  generation.dataNG<-melt(generation.dataNG,id.vars=c("Plant.Code","Reported.Prime.Mover"))
  
  names(gen_fuel_dataNG)[3]<-"month"
  names(generation.dataNG)[3]<-"month"
  
  #sum by RPM
  gen_fuel_dataNG<-gen_fuel_dataNG %>% ungroup() %>%
    dplyr::group_by(Plant.Code,Reported.Prime.Mover,month) %>%
    dplyr::summarise(sumNGpage1 = sum(value,na.rm = T))
  generation.dataNG<-generation.dataNG %>% ungroup() %>%
    dplyr::group_by(Plant.Code,Reported.Prime.Mover,month) %>%
    dplyr::summarise(sumNGpage4 = sum(value,na.rm = T))
  
  #join page 4 and page 1
 # generation.dataNG<-left_join(generation.dataNG,gen_fuel_dataNG)
  generation.dataNG<-left_join(gen_fuel_dataNG,generation.dataNG)
  generation.dataNG$sumNGpage1<-ifelse(is.na(generation.dataNG$sumNGpage1),0,generation.dataNG$sumNGpage1)
  generation.dataNG$sumNGpage4<-ifelse(is.na(generation.dataNG$sumNGpage4),0,generation.dataNG$sumNGpage4)
  
  
  
  #flag missing page 4 data
  generation.dataNG$flag_NGCC<-ifelse(generation.dataNG$sumNGpage4<=0 & generation.dataNG$sumNGpage1>0,1,0)
  generation.dataNG<-generation.dataNG %>% filter(flag_NGCC==1)
  NGCCplants<-unique(NGCCplants,generation.dataNG$Plant.Code)

  
  return(NGCCplants)
  
  
  }#end function