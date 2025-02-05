sumNetGen_LinesAndPlant<-function(months,generation.data){
#sum all lines by plant and Generator.ID
for(m in months){
  netGenstr<-paste0("sumbyGen<-generation.data %>% group_by(Plant.Code,Generator.ID,orig_Generator.ID_923,flagGenerator.ID_923) %>%
                        summarize(Generator.ID.Total.Net.Generation.",m,"=sum(Net.Generation.",m,",na.rm=T))")
  eval(parse(text=netGenstr))
  if (m=="January"){
    generation.data.gb<-sumbyGen
  }else{
    
    generation.data.gb<-merge(generation.data.gb,sumbyGen,by=c("Plant.Code","Generator.ID","orig_Generator.ID_923","flagGenerator.ID_923"),all=T)
  }
  
}
generation.data.gb2 <- generation.data %>% group_by(.,Plant.Code,Generator.ID) %>% summarise(Net.Generation.mwh=sum(Net.Generation.Year.To.Date,na.rm=T))
generation.data.gb<-merge(generation.data.gb,generation.data.gb2,by=c("Plant.Code","Generator.ID"))
assign("generation.data.gb",generation.data.gb,envir = .GlobalEnv)

#sum all lines by plant
for(m in months){
  netGenstr<-paste0("sumbyPlant<-generation.data.gb %>% group_by(Plant.Code) %>% 
                        summarize(PlantLine.Total.Net.Generation.",m,"=sum(Generator.ID.Total.Net.Generation.",m,",na.rm=T))")
  eval(parse(text=netGenstr))
  if (m=="January"){
    sumNetGenLinesByPlant<-sumbyPlant  
  }else{
    sumNetGenLinesByPlant<-merge(sumNetGenLinesByPlant,sumbyPlant,by="Plant.Code",all=T)
  }
  
}
assign("sumNetGenLinesByPlant",sumNetGenLinesByPlant,envir = .GlobalEnv)

gen_923<-generation.data.gb

outlist<-named.list(generation.data.gb2,gen_923,sumNetGenLinesByPlant)
return(outlist)
}