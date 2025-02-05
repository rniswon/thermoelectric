#'@title thermalEfficiencyByBogen
#'@description 
#'@param bogen_fuel_heat
#'@param bogen_netgen
#'@param gen_fuel_data
#'@importFrom reshape2 melt dcast
#'@export

thermalEfficiencyByBogen<-function(bogen_fuel_heat,bogen_netgen, gen_fuel_data){
  months<-tolower(c("January","February","March","April","May",
            "June","July","August","September","October","November","December"))

#FuelHeat by Bogen    
bogen_fuel_heat<-bogen_fuel_heat %>% select(Plant.Code,bogen,all_of(months))
bogen_fuel_heat<-reshape2::melt(bogen_fuel_heat,id.vars = c("Plant.Code","bogen"),value.name = "FuelHeat")
#netgen by bogen
bogen_netgen<-bogen_netgen %>% select(Plant.Code, bogen, contains("Net.Generation"),-Net.Generation.Year.To.Date)
names(bogen_netgen)[3:length(bogen_netgen)]<-months
bogen_netgen<-reshape2::melt(bogen_netgen,id.vars = c("Plant.Code","bogen"),value.name = "NetGen")

#nukes
gen_fuel_data <- gen_fuel_data %>% subset(Nuclear.Unit.Id!="." & !is.na(Nuclear.Unit.Id))

nuke_fuel_heat <- gen_fuel_data %>% select(c("Plant.Code", "Nuclear.Unit.Id", contains("Tot_MMBtu")))
nuke_fuel_heat$bogen<-paste(nuke_fuel_heat$Plant.Code,nuke_fuel_heat$Nuclear.Unit.Id,sep="^")
nuke_fuel_heat<-nuke_fuel_heat %>% select(-Nuclear.Unit.Id)
nuke_fuel_heat<-nuke_fuel_heat %>% select(Plant.Code,bogen,contains("Tot_MMBtu"))
names(nuke_fuel_heat)[3:length(nuke_fuel_heat)]<-months
nuke_fuel_heat<-reshape2::melt(nuke_fuel_heat,id.vars = c("Plant.Code","bogen"),value.name = "FuelHeat")


nuke_netgen <- gen_fuel_data %>% select(c("Plant.Code", "Nuclear.Unit.Id", contains("Netgen")))
nuke_netgen$bogen<-paste(nuke_netgen$Plant.Code,nuke_netgen$Nuclear.Unit.Id,sep="^")
nuke_netgen<-nuke_netgen %>% select(-Nuclear.Unit.Id)
nuke_netgen<-nuke_netgen %>% select(Plant.Code,bogen,contains("Netgen"))
names(nuke_netgen)[3:length(nuke_netgen)]<-months
nuke_netgen<-reshape2::melt(nuke_netgen,id.vars = c("Plant.Code","bogen"),value.name = "NetGen")

#combine nukes and nonnukes
bogen_fuel_heat<-rbind(bogen_fuel_heat,nuke_fuel_heat)
bogen_netgen<-rbind(bogen_netgen,nuke_netgen)

#sumBybogen
bogen_fuel_heat<-bogen_fuel_heat %>% dplyr::group_by(Plant.Code,bogen,variable) %>% 
  dplyr::summarise(sumFuelHeat = sum(FuelHeat,na.rm = T))
bogen_netgen<-bogen_netgen %>% dplyr::group_by(Plant.Code,bogen,variable) %>% 
  dplyr::summarise(sumNetGen = sum(NetGen,na.rm = T))
                                                                                         


#sum by bogen
bogenData<-inner_join(bogen_fuel_heat,bogen_netgen,by=c("Plant.Code","bogen","variable"))

#calc thermeff
bogenData<-bogenData %>% dplyr::mutate(ThermalEfficiency = 3.41214*sumNetGen/sumFuelHeat)
bogenData<-bogenData %>% dplyr::mutate(ThermalEfficiency = ifelse(sumNetGen<=0 & sumFuelHeat<=0,0,
                                       ifelse(sumNetGen>0 & sumFuelHeat<=0,9999,ThermalEfficiency)))

bogenData<-bogenData %>% select(Plant.Code,bogen,variable,ThermalEfficiency)


bogenData<-reshape2::dcast(bogenData,Plant.Code+bogen~variable,value.var = "ThermalEfficiency")
names(bogenData)[3:length(bogenData)]<-paste0("ThermalEfficiency_",names(bogenData)[3:length(bogenData)])
return(bogenData)
}