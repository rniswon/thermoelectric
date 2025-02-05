#### based on original code in file named PrepTowerFEWSRinput_loop.R
#### purpose is to prepare input files for the tower model from the Condenser duty model outputs that have been joined to the environmental variables.
### modified Oct. 2022 by Melissa Lombard to run in a loop over files for each year
library(dplyr)

#setwd("C:/Users/mlombard/OneDrive - DOI/TE_Wateruse/Thermoelectric") #top of repo, change as needed
setwd("G:/WU/Data/Thermo/GitHub_Clone/Thermoelectric-master/tower")

#pond information file - not necessary
#plant_pond_info <- read.csv("./cd/Data/plant_elev_pond_info.csv", header = T, stringsAsFactors = F)

#read in the tower design info again and keep the data
Tower.design.char<-read.csv("../cd/Data/TOWERmodelPlantChar.csv", header=T, skip = 7, stringsAsFactors = F)
names(Tower.design.char)

#Select required columns - will this file change for every year or is there one static file?  Assuming the one used here is for 2015
Tower.design.char<-Tower.design.char[c("Plant_ID","Elevation","Pond_Area", "Tdb","Twb")]

### loop over the condenser duty data files for each year
years <- 2008:2020

#i=8 #test loop for 2015
months<-c("January","February","March","April","May",
          "June","July","August","September","October","November","December")

for (i in 1:length(years)) {
  year.i <- years[i]
# read in condenser duty and environmental data
CD_data.i <- read.csv(paste("./Data/",years[i],"_fewsr_tower_input_final_winterized.csv", sep =""), header = T, stringsAsFactors = F)
  #CD_data2.i <- CD_data.i[!duplicated(CD_data.i[c(2:14)]),] #not sure this is necesary and column indexes are from original, I think percent allocation and CD numbers
  #CD_data2.i <- CD_data.i %>% filter(!is.na(Plant.Code))
  tower.input <- CD_data.i %>% filter(cooling == "tower") #check that column name is cooling 
      tower.input1 <- left_join(tower.input, Tower.design.char, by=c("Plant.Code"="Plant_ID")) 
  tower.input1 <- tower.input1 %>% mutate(nwT = 30.49)
 
  #final_tower_input <- left_join(tower.input, plant_pond_info) # This seems unnecessary - the same info is in the Tower.design.char file
  final_tower_input <- tower.input1 %>% select(Plant.Code, Elevation, Pond_Area, percentAllocation,
                                                    contains("CD_"), contains("DB_"), contains("WB_"),
                                                    contains("WT_"), contains("WS_"), Tdb, Twb, nwT)
  
  #double check no columns without first three letters of month (i.e WT_multsrc)
  towerData<-final_tower_input %>% select(contains("CD_"), contains("DB_"), contains("WB_"),
                              contains("WT_"), contains("WS_"))
  towerData<-towerData %>% select(matches(substr(tolower(months),1,3)))
  #split non data columns
  final_tower_input<-final_tower_input %>% select(Plant.Code, Elevation, Pond_Area,percentAllocation, Tdb, Twb, nwT)
  #put it back together
  final_tower_input<-cbind(final_tower_input,towerData)
  #organize in correct order
  final_tower_input <- final_tower_input %>% select(Plant.Code, Elevation, Pond_Area, percentAllocation,
                              contains("CD_"), contains("DB_"), contains("WB_"),
                              contains("WT_"), contains("WS_"), Tdb, Twb, nwT)
  
  final_tower_input <- final_tower_input[!duplicated(final_tower_input),]
  #final_tower_input2 <- final_tower_input[complete.cases(final_tower_input), ]
  
  #save TOWER model input files to TOWER model data directory
#  write.csv(final_tower_input, paste("./targets_2015/model_archive/tower/Data/", years[i],"_Tower_input.csv", sep = ""), row.names = F)
  write.csv(final_tower_input, paste("./Data/", years[i],"_Tower_input.csv", sep = ""), row.names = F)
  
}

