sumGenFuelData<-function(gen_fuel_data,bf.923.ungrouped, generation.data.line124,months,analysisYear){
  
  for(m in months){
    fuelHeatstr<-paste0("gen_fuel_data<-gen_fuel_data %>% group_by(Plant.Code,Reported.Prime.Mover) %>% 
                        mutate(Fuel.Heat.",m,"=Quantity.",m,"*MMBtuPer_Unit.",m,")")
    eval(parse(text=fuelHeatstr))
    fuelHeatstr<-paste0("genFuelsumPlant<-gen_fuel_data %>% group_by(Plant.Code,Reported.Prime.Mover) %>% 
                        summarize(PlantLine.Total.Fuel.Heat.",m,"=sum(Fuel.Heat.",m,",na.rm=TRUE))")
    eval(parse(text=fuelHeatstr))
    
    netGenstr<-paste0("genFuelsumPlantNG<-gen_fuel_data %>% group_by(Plant.Code,Reported.Prime.Mover) %>%
                        summarize(PlantLine.Total.Net.Generation.",m,"=sum(Netgen.",m,",na.rm=T))")
    eval(parse(text=netGenstr))
    
    fuelHeatstr<-paste0("genFuelsumPlant2<-gen_fuel_data %>% group_by(Plant.Code,Reported.Prime.Mover,Reported.Fuel.Type.Code) %>% 
                        summarize(Fuel.Heat.",m,"=sum(Fuel.Heat.",m,",na.rm=TRUE))")
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
  sumTotNGgen_fuel_data<-gen_fuel_data %>% group_by(Plant.Code,Reported.Prime.Mover) %>%
    summarize(Net.Generation.mwh=sum(Net.Generation..Megawatthours.,na.rm=T))
  
  gen_fuel_data<-merge(sumgenFuel,sumTotNGgen_fuel_data,by=c("Plant.Code","Reported.Prime.Mover"))
  gen_fuel_data<-merge(gen_fuel_data,sumgenFuel_netGen,by=c("Plant.Code","Reported.Prime.Mover"))
  names(gen_fuel_data)[names(gen_fuel_data)=="Reported.Prime.Mover"]<-"Reported.Prime.Mover_page1"
  
  #add gen_fuel lines to bf.923.ungrouped and generation.data.line124
  # names(sumgenFuel)[regexpr("Fuel.Heat",
  #                           names(sumgenFuel))>0]<-names(bf.923.ungrouped)[regexpr("Fuel.Heat",
  #                           names(bf.923.ungrouped))>0]
  sumgenFuelLine$Boiler.ID<-rep(as.character(NA),nrow(sumgenFuelLine))
  sumgenFuelLine$YEAR<-rep(as.integer(analysisYear),nrow(sumgenFuelLine))
  
  
  if (eia_year>2005){
    bf.923.ungrouped<-bf.923.ungrouped %>% select(Plant.Code,Boiler.ID,Reported.Prime.Mover_bf.923,
                                                  Reported.Fuel.Type.Code,contains("Fuel.Heat"),YEAR) 
    sumgenFuelLine$Reported.Prime.Mover_bf.923<-sumgenFuelLine$Reported.Prime.Mover
  }else{
    bf.923.ungrouped<-bf.923.ungrouped %>% select(Plant.Code,Boiler.ID,
                                                  Reported.Fuel.Type.Code,contains("Fuel.Heat"),YEAR) 
  }
  sumgenFuelLine<-sumgenFuelLine %>% select(names(bf.923.ungrouped),-Reported.Prime.Mover)
  
  bf.923.ungrouped<-rbind(bf.923.ungrouped,sumgenFuelLine)
  bf.923.ungrouped<-bf.923.ungrouped[order(as.numeric(bf.923.ungrouped$Plant.Code),bf.923.ungrouped$Boiler.ID),]
  assign("bf.923.ungrouped",bf.923.ungrouped,envir = .GlobalEnv)
  
  sumTotNGgen_fuel_data$Net.Generation.Year.To.Date<-sumTotNGgen_fuel_data$Net.Generation.mwh
  sumgenFuel_netGen<-merge(sumgenFuel_netGen,sumTotNGgen_fuel_data %>% 
                             select(Plant.Code,Reported.Prime.Mover,Net.Generation.Year.To.Date),
                           by=c("Plant.Code","Reported.Prime.Mover"))
  names(sumgenFuel_netGen)[regexpr("PlantLine.Total.Net.Generation.",
                                   names(sumgenFuel_netGen))>0]<-names(generation.data.line124)[(regexpr("Net.Generation.",
                                                                                                        names(generation.data.line124))>0) & 
                                                                                                  names(generation.data.line124)!="Net.Generation.Year.To.Date"]
  if (eia_year>2005){
    generation.data.line124<-generation.data.line124 %>% select(Plant.Code,Generator.ID,Reported.Prime.Mover,
                                                                contains("Net.Generation"),YEAR)
  }else{
    generation.data.line124<-generation.data.line124 %>% select(Plant.Code,Generator.ID,
                                                                contains("Net.Generation"),YEAR)
  }
  sumgenFuel_netGen$Generator.ID<-rep(as.character(NA),nrow(sumgenFuel_netGen))
  sumgenFuel_netGen$YEAR<-rep(as.integer(analysisYear),nrow(sumgenFuel_netGen))
  
  sumgenFuel_netGen<-sumgenFuel_netGen %>% select(names(generation.data.line124))
  
  generation.data.line124<-rbind(generation.data.line124,sumgenFuel_netGen)
  generation.data.line124<-generation.data.line124[order(as.numeric(generation.data.line124$Plant.Code),
                                                         generation.data.line124$Generator.ID),]
  assign("generation.data.line124",generation.data.line124,envir = .GlobalEnv)
  
  return(gen_fuel_data)
}