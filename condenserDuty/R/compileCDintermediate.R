#'@title compileCDintermediate
#'@description 
#'@param bogen_fuel_heat
#'@param bogen_netgen
#'@param bogen_nom_loss
#'@param bogen_steam_heat
#'@param gen_fuel_data
#'@param non_nuke_CD_summary
#'@param nuke_CD_summary
#'@param plantList
#'@param plant_exports
#'
#'@importFrom reshape2 melt dcast
#'@export

compileCDintermediate<-function(bogen_fuel_heat,bogen_netgen,bogen_nom_loss,bogen_steam_heat, gen_fuel_data,
                                non_nuke_CD_summary,nuke_CD_summary,plantList,plant_exports){
months<-tolower(c("January","February","March","April","May",
                  "June","July","August","September","October","November","December"))
#saveRPM
bogen_RPM<-bogen_fuel_heat %>% select(bogen,Reported.Prime.Mover)
nuke_RPM<-gen_fuel_data %>% filter(Nuclear.Unit.Id!="." & !is.na(Nuclear.Unit.Id))
nuke_RPM$bogen<-paste(nuke_RPM$Plant.Code,nuke_RPM$Nuclear.Unit.Id,sep="^")
nuke_RPM<-nuke_RPM %>% select(bogen,Reported.Prime.Mover)
all_RPM<-rbind(bogen_RPM,nuke_RPM)
all_RPM$Reported.Prime.Mover<-ifelse(all_RPM$Reported.Prime.Mover %in% c("CS","CA","CT"),"NGCC",all_RPM$Reported.Prime.Mover)
all_RPM<-all_RPM[!duplicated(all_RPM),]
all_ST<-all_RPM %>% filter(Reported.Prime.Mover=="ST")
all_NGCC<-all_RPM %>% filter(Reported.Prime.Mover=="NGCC")

#FuelHeat by Bogen    
bogen_fuel_heat<-bogen_fuel_heat %>% select(Plant.Code,bogen,all_of(months))
bogen_fuel_heat<-reshape2::melt(bogen_fuel_heat,id.vars = c("Plant.Code","bogen"),value.name = "FuelHeat")
#netgen by bogen
bogen_netgen<-bogen_netgen %>% select(Plant.Code, bogen, contains("Net.Generation"),-Net.Generation.Year.To.Date)
names(bogen_netgen)[3:length(bogen_netgen)]<-months
bogen_netgen<-reshape2::melt(bogen_netgen,id.vars = c("Plant.Code","bogen"),value.name = "NetGen")
#steamHeat
bogen_steam_heat<-bogen_steam_heat %>% select(Plant.Code, bogen, starts_with("steam_heat"))
names(bogen_steam_heat)[3:length(bogen_steam_heat)]<-months
bogen_steam_heat<-reshape2::melt(bogen_steam_heat,id.vars = c("Plant.Code","bogen"),value.name = "steamHeat")
#nom_loss
bogen_nom_loss<-bogen_nom_loss %>% select(Plant.Code, bogen, all_of(months))
names(bogen_nom_loss)[3:length(bogen_nom_loss)]<-months
bogen_nom_loss<-reshape2::melt(bogen_nom_loss,id.vars = c("Plant.Code","bogen"),value.name = "nomLoss")

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
bogen_steam_heat<-bogen_steam_heat %>% dplyr::group_by(Plant.Code,bogen,variable) %>% 
  dplyr::summarise(sumSteamHeat = sum(steamHeat,na.rm = T))
bogen_nom_loss<-bogen_nom_loss %>% dplyr::group_by(Plant.Code,bogen,variable) %>% 
  dplyr::summarise(sumNomLoss = sum(nomLoss,na.rm = T))






#sum by bogen
bogenData<-inner_join(bogen_fuel_heat,bogen_netgen,by=c("Plant.Code","bogen","variable"))

#calc thermeff
bogenData<-bogenData %>% dplyr::mutate(ThermalEfficiency = 3.41214*sumNetGen/sumFuelHeat)
bogenData<-bogenData %>% dplyr::mutate(ThermalEfficiency = ifelse(sumNetGen<=0 & sumFuelHeat<=0,0,
                                                                  ifelse(sumNetGen>0 & sumFuelHeat<=0,9999,ThermalEfficiency)))
#thermalEff flags
#bogenData<-merge(bogenData,all_RPM,by="bogen",all.x=T)
bogenData<-bogenData %>% dplyr::mutate(flag_HighTE = ifelse(ThermalEfficiency>0.58 & bogen %in% all_NGCC$bogen,1,
                                                            ifelse(ThermalEfficiency>0.44 & bogen %in% all_ST$bogen,1,0)),
                                       flag_ZeroFH_ZeroNetGen = ifelse(sumNetGen<=0 & sumFuelHeat<=0,1,0),
                                       flag_ZeroFH = ifelse(ThermalEfficiency==9999,1,0),
                                       flag_ZeroNetGen = ifelse(sumNetGen<=0 & sumFuelHeat>0,1,0))

#separate tables
FH<-bogenData %>% select(Plant.Code,bogen,variable,sumFuelHeat)
FH<-dcast(FH,Plant.Code+bogen~variable,value.var = "sumFuelHeat")
names(FH)[3:length(FH)]<-paste0("FuelHeat_",names(FH)[3:length(FH)])

NG<-bogenData %>% select(Plant.Code,bogen,variable,sumNetGen)
NG<-dcast(NG,Plant.Code+bogen~variable,value.var = "sumNetGen")
names(NG)[3:length(NG)]<-paste0("NetGen_",names(NG)[3:length(NG)])

TE<-bogenData %>% select(Plant.Code,bogen,variable,ThermalEfficiency)
TE<-dcast(TE,Plant.Code+bogen~variable,value.var = "ThermalEfficiency")
names(TE)[3:length(TE)]<-paste0("ThermalEfficiency_",names(TE)[3:length(TE)])

HighTE<-bogenData %>% select(Plant.Code,bogen,variable,flag_HighTE)
HighTE<-dcast(HighTE,Plant.Code+bogen~variable,value.var = "flag_HighTE")
names(HighTE)[3:length(HighTE)]<-paste0("flag_HighTE_",names(HighTE)[3:length(HighTE)])

ZeroFH_ZeroNetGen<-bogenData %>% select(Plant.Code,bogen,variable,flag_ZeroFH_ZeroNetGen)
ZeroFH_ZeroNetGen<-dcast(ZeroFH_ZeroNetGen,Plant.Code+bogen~variable,value.var = "flag_ZeroFH_ZeroNetGen")
names(ZeroFH_ZeroNetGen)[3:length(ZeroFH_ZeroNetGen)]<-paste0("flag_ZeroFH_ZeroNetGen_",
                                                              names(ZeroFH_ZeroNetGen)[3:length(ZeroFH_ZeroNetGen)])

ZeroFH<-bogenData %>% select(Plant.Code,bogen,variable,flag_ZeroFH)
ZeroFH<-dcast(ZeroFH,Plant.Code+bogen~variable,value.var = "flag_ZeroFH")
names(ZeroFH)[3:length(ZeroFH)]<-paste0("Flag_gen_no_fuel_",names(ZeroFH)[3:length(ZeroFH)])

ZeroNetGen<-bogenData %>% select(Plant.Code,bogen,variable,flag_ZeroNetGen)
ZeroNetGen<-dcast(ZeroNetGen,Plant.Code+bogen~variable,value.var = "flag_ZeroNetGen")
names(ZeroNetGen)[3:length(ZeroNetGen)]<-paste0("Flag_fuel_no_gen_",names(ZeroNetGen)[3:length(ZeroNetGen)])

SH<-bogen_steam_heat
SH<-dcast(SH,Plant.Code+bogen~variable,value.var = "sumSteamHeat")
names(SH)[3:length(SH)]<-paste0("steamHeat_",names(SH)[3:length(SH)])

NL<-bogen_nom_loss
NL<-dcast(NL,Plant.Code+bogen~variable,value.var = "sumNomLoss")
names(NL)[3:length(NL)]<-paste0("nomLoss_",names(NL)[3:length(NL)])

#Cd results by bogen
nuke_CD_summary<-nuke_CD_summary %>% filter(Nuclear.Unit.Id!="." & !is.na(Nuclear.Unit.Id))
nuke_CD_summary<-nuke_CD_summary %>% select(Plant.Code,Nuclear.Unit.Id,starts_with("CD_"))
nuke_CD_summary$bogen<-paste(nuke_CD_summary$Plant.Code,nuke_CD_summary$Nuclear.Unit.Id,sep="^")
nuke_CD_summary<-nuke_CD_summary %>% select(Plant.Code,bogen,starts_with("CD_"))
names(nuke_CD_summary)<-names(non_nuke_CD_summary)
cd_by_bogen<-rbind(non_nuke_CD_summary,nuke_CD_summary)

#EXPORTS
plant_exports <- ungroup(plant_exports)

#get monthly exports >0  
monthly_exports <- plant_exports %>% 
  filter(Plant.Code %in% plantList) %>% 
  filter(plant_total > 0)

firstBogen<-cd_by_bogen %>% dplyr::group_by(Plant.Code) %>% dplyr::mutate(bogen=first(bogen))
firstBogen<-firstBogen %>% select(Plant.Code,bogen)
monthly_exports<-inner_join(monthly_exports,firstBogen,by="Plant.Code")
monthly_exports<-monthly_exports %>% select(Plant.Code,bogen,contains("Exported"))


allData<-merge(cd_by_bogen,FH,by= c("Plant.Code","bogen"),all=T)
allData<-merge(allData,NG,by= c("Plant.Code","bogen"),all=T)
allData<-merge(allData,TE,by= c("Plant.Code","bogen"),all=T)
allData<-merge(allData,SH,by= c("Plant.Code","bogen"),all=T)
allData<-merge(allData,NL,by= c("Plant.Code","bogen"),all=T)
allData<-merge(allData,monthly_exports,by= c("Plant.Code","bogen"),all=T)
allData<-merge(allData,HighTE,by= c("Plant.Code","bogen"),all=T)
allData<-merge(allData,ZeroFH_ZeroNetGen,by= c("Plant.Code","bogen"),all=T)
allData<-merge(allData,ZeroFH,by= c("Plant.Code","bogen"),all=T)
allData<-merge(allData,ZeroNetGen,by= c("Plant.Code","bogen"),all=T)

allData$general_mover<-ifelse(allData$bogen %in% all_NGCC$bogen,"NGCC",ifelse(allData$bogen %in% all_ST$bogen,"ST",NA))

allData<-allData %>% select(Plant.Code,bogen,general_mover,
                            names(allData)[!names(allData) %in% c("Plant.Code","bogen","general_mover")])

return(allData)
}#end func