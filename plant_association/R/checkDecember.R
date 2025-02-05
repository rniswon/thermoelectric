checkDecember<-function(FuelHeatByLine,NetGenByLine,gen_fuel_data,plantList,select_RPM,months){

FuelHeatByLine<-FuelHeatByLine %>% filter(Reported.Prime.Mover_bf.923 %in% select_RPM)
NetGenByLine<-NetGenByLine %>% filter(Reported.Prime.Mover %in% select_RPM)
gen_fuel_data<-gen_fuel_data %>% filter(Reported.Prime.Mover %in% select_RPM)

months<-c("January","February","March","April","May",
          "June","July","August","September","October","November","December")

decPlant<-numeric(0)
#plant level decembrist
for (m in months){
  #sum plant
  fuelHeatstr<-paste0("tempPlantFH<-FuelHeatByLine %>% group_by(Plant.Code) %>% 
                        summarize(Fuel.Heat.",m,"=sum(Fuel.Heat.",m,",na.rm=TRUE))")
  eval(parse(text=fuelHeatstr))
  netGenstr<-paste0("tempPlantNG<-NetGenByLine %>% group_by(Plant.Code) %>% 
                        summarize(Net.Generation.",m,"=sum(Net.Generation.",m,",na.rm=TRUE))")
  eval(parse(text=netGenstr))
  
  
  if (m=="January"){
    sumPlantFH<-tempPlantFH 
    sumPlantNG<-tempPlantNG

  }else{
    sumPlantFH<-merge(sumPlantFH,tempPlantFH,by="Plant.Code",all=T)
    sumPlantNG<-merge(sumPlantNG,tempPlantNG, by=c("Plant.Code"),all=T)

  }
  
}#end months

for (p in plantList){
  #plant
  subPlantFH<-sumPlantFH %>% filter(Plant.Code==p) %>%
    select(contains("Fuel.Heat"))
  decPlantFH<-subPlantFH$Fuel.Heat.December
  subPlantFH<-subPlantFH %>% select(-Fuel.Heat.December)
  if (nrow(subPlantFH)==0){
    subPlantFH<-0
  }else{
    subPlantFH<-ifelse(any(abs(subPlantFH)>0),1,0) 
  }
  
  
  subPlantNG<-sumPlantNG %>% filter(Plant.Code==p) %>%
    select(contains("Net.Generation."))
  decPlantNG<-subPlantNG$Net.Generation.December
  subPlantNG<-subPlantNG %>% select(-Net.Generation.December)
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
      
    # }else if (length(decPlantNG)!=0){
    #   if (subPlantNG==0 & decPlantNG>0){
    #     decPlant<-c(decPlant,p)
    #   }
      
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