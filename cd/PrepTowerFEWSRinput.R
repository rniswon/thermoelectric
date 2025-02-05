loadCondenserDutyFunctions("data_prep_funs")
plant_information <- read.csv(here('CondenserDutyModel', 'Data', 'eia860_plant_info.csv'), header = T, stringsAsFactors = T)
climate_wbdb <- read.csv(here('CondenserDutyModel', 'Data', 'Towers_3design2.csv'), header = T, stringsAsFactors = F)
plant_pond_info <- read.csv(here('CondenserDutyModel', 'Data', 'plant_elev_pond_info.csv'), header = T, stringsAsFactors = F)
CD_data <- read.csv(here('CondenserDutyModel', 'Output', 'rawInput_FEWSR_TOWER.csv'), header = T, stringsAsFactors = F)

plant_details <- plant_information %>% select(Plant.Code, Plant.Name, County, State, 
                                              Latitude, Longitude, Name.of.Water.Source)
#For tower model
Tower.design.char.header<-read.csv(here('CondenserDutyModel', 'Data', 'TOWERmodelPlantChar.csv'), header=F, nrows = 7, stringsAsFactors = F)
Tower.design.char<-read.csv(here('CondenserDutyModel', 'Data', 'TOWERmodelPlantChar.csv'), header=T, skip = 7, stringsAsFactors = F)

#Select required columns
Tower.design.char<-Tower.design.char[1:6]

all_tower_design_char <- processenvDesvars(climate_wbdb, Tower.design.char)

CD_data <- CD_data %>% select(1, 14, 2:13, 15:50, 55:66)
CD_data2 <- CD_data[!duplicated(CD_data[c(2:14)]),]
CD_data2 <- CD_data2 %>% filter(!is.na(Plant.Code))


#Prepare tower model input

tower.input <- CD_data2 %>% filter(cooling == "tower")
tower.input <- left_join(tower.input, all_tower_design_char, by=c("Plant.Code"="plant_id"))
tower.input <- tower.input %>% mutate(nwT = 30.49)
colnames(tower.input)[names(tower.input)=="tdb"] <- "Tdb"
colnames(tower.input)[names(tower.input)=="twb"] <- "Twb"
final_tower_input <- left_join(tower.input, plant_pond_info)
final_tower_input <- final_tower_input %>% select(Plant.Code, elevation, pond_area, 
                                                  contains("CD_"), contains("_DB"), contains("_WB"),
                                                  contains("WT_"), contains("_WS"), Tdb, Twb, nwT)
final_tower_input <- final_tower_input[!duplicated(final_tower_input),]

#Rename a columns
names(final_tower_input)[4:15]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
names(final_tower_input)[16:27]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
names(final_tower_input)[28:39]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
names(final_tower_input)[40:51]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
names(final_tower_input)[52:63]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

final_tower_input2 <- final_tower_input[complete.cases(final_tower_input), ]
colnames(final_tower_input2)[colnames(final_tower_input2)=="elevation"]<- "Elevation"
#Store TOWER model input files to TOWER model data directory
write.csv(Tower.design.char.header, here('Tower_model', 'Data', 'Tower_input_header_data.csv'), row.names = F)
write.csv(final_tower_input2, here('Tower_model', 'Data', 'Tower_input.csv'), row.names = F)

#For FEWSR model
FEWSR.biglakes.DC.header<-read.csv(here('CondenserDutyModel', 'Data', 'FEWS_BIG_Lake_plants_designChars.csv'), header=F,nrows = 5,stringsAsFactors = F)
FEWSR.ponds.DC.header<-read.csv(here('CondenserDutyModel', 'Data', 'FEWS_Pond_plants_designChars.csv'), header=F,nrows = 5,stringsAsFactors = F)
FEWSR.river.DC.header<-read.csv(here('CondenserDutyModel', 'Data', 'FEWS_River_plants_designChar.csv'), header=F,nrows = 5,stringsAsFactors = F)


fewsr_prep <- function(con_duty, pond_info, fewsr_type){
  con_duty <- con_duty %>% filter(cooling == fewsr_type) 
  con_duty <- left_join(con_duty, pond_info)
  con_duty <- con_duty %>% select(Plant.Code, elevation, pond_area, 
                                  contains("CD_"), contains("_DB"), contains("_WB"),
                                  contains("WT_"), contains("_WS"))
  
  names(con_duty)[4:15]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  names(con_duty)[16:27]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  names(con_duty)[28:39]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  names(con_duty)[40:51]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  names(con_duty)[52:63]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  
  return(con_duty)
}

####Merge for FEWSR input with waterbody characteristics
cd_lakes <- fewsr_prep(CD_data, plant_pond_info, "lake")
cd_pond <- fewsr_prep(CD_data, plant_pond_info, "pond")
cd_river <- fewsr_prep(CD_data, plant_pond_info, "river")





#Store FEWSR model input to FEWSR data directory
write.csv(FEWSR.biglakes.DC.header, here('FEWSR', 'Data', 'FEW_BIG_Lake_plants_input_header.csv'), row.names = F)
write.csv(cd_lakes,here('FEWSR', 'Data', 'FEW_BIG_Lake_plants_input.csv'), row.names = F)

write.csv(FEWSR.ponds.DC.header,here('FEWSR', 'Data', 'FEWS_Pond_plants_input_header.csv'),row.names = F)
write.csv(cd_pond, here('FEWSR', 'Data', 'FEWS_Pond_plants_input.csv'), row.names = F)

write.csv(FEWSR.river.DC.header, here('FEWSR', 'Data', 'FEWS_River_plants_input_header.csv'),row.names = F)
write.csv(cd_river, here('FEWSR', 'Data', 'FEWS_River_plants_input.csv'),row.names = F)

