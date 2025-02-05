#'@title getEnvVars
#'@description combines and format monthly environmental variables used by the TOWER and FEWSR models 
#'             (dry bulb, wet bulb, wind speed and natural water temperature) \cr \cr
#'Outputs the following files: environmental_variables.csv - monthly environmental variables used by the
#'                                                           TOWER and FEWSR models
#'Executed By: CondenserDutyModel_new.R \cr
#'@param plant_list vector of all plant codes from 2015, input file 2015_Plants.csv
#'@param dbwb_data_df NOAA dry-bulb and wet-bulb air temperature data for the nearest three weather 
#'                    stations to each power plant.  Used to calculate a distance-weighted average 
#'                    of the three nearest weather stations to determine DB and WB monthly air temps 
#'                    for each plant, from the 3sta_DBWB.csv input file
#'@param ws_data_df NOAA wind speed data for the nearest three weather stations to each power plant. 
#'                  Used to calculate a distance-weighted average of the three nearest weather stations 
#'                  to determine monthly wind speeds for each plant, from 3sta_WS.csv input file
#'@param wt_data_df Monthly kriged-estimated water temps for each plant, from the 2015_WT_Predictions.csv
#'                  input file
#'@param outputData_path character string indicating path for output files
#'@param getEnvVars TRUE/FALSE whether or not to execute getEnvVars() function, default value TRUE
#'@return `result` data.frame that combines and format monthly environmental variables used by the TOWER 
#'                 and FEWSR models (dry bulb, wet bulb, wind speed and natural water temperature)
#'@examples
#'getEnvVars(modeled_plants$Plant.Code, dbwb_data, ws_data, wt_data, getEnvVars=T)

getEnvVars<-function(plant_list, dbwb_data_df, ws_data_df, wt_data_df,outputData_path, getEnvVars = T){
if(getEnvVars){

####These lines of code are repeated for each environmental varaible required by the models. The last step is to combine these
#data into a single file and merge with plant level condenser duties
#Data clean up
dbwb_data_df <- dbwb_data_df %>% select(-c(WMO_num, NEAR_RANK, PLANT_ID_1))

#Convert near distance from meters to miles
dbwb_data_df$NEAR_DIST_mi <- dbwb_data_df$NEAR_DIST / 1609.34

#Create invert diastance variable
dbwb_data_df$Inv_dist <- 1 / dbwb_data_df$NEAR_DIST_mi

#Sort data in asceding order by Plant.Code
dbwb_data_df <- dbwb_data_df[order(dbwb_data_df$Plant_ID), ]

#Calculate inv-dist weighted avg for monthly WB and DB for each plant
wb.weighted.means <- dbwb_data_df %>% group_by(Plant_ID) %>% 
                     select(c(Plant_ID, 25:36, 38)) %>%  
                     summarize_each(funs(weighted.mean(., Inv_dist, na.rm = T)), -Inv_dist) %>% 
                     round(., 0)
#Rename columns
colnames(wb.weighted.means)[2:13] <- paste("weighted", colnames(wb.weighted.means)[2:13], sep = ".")

db.weighted.means <- dbwb_data_df %>% 
                     group_by(Plant_ID) %>% 
                     select(c(Plant_ID, 13:24, 38)) %>%  
                     summarize_each(funs(weighted.mean(., Inv_dist, na.rm = T)), -Inv_dist) %>% 
                     round(., 0)

#Rename columns
colnames(db.weighted.means)[2:13] <- paste("weighted", colnames(db.weighted.means)[2:13], sep = ".")

#Merge weighted wet bulb and dry bulb data
weighted.wb.db <- left_join(wb.weighted.means, db.weighted.means)

#merge with raw data set
dbwb_data_df.w.weights <- left_join(dbwb_data_df, weighted.wb.db, by='Plant_ID')

#List plants that don't use 3 weather stations 
no.w.sta <- table(dbwb_data_df.w.weights$Plant_ID)
plants.to.flag <- which(no.w.sta != 3)

names(dbwb_data_df.w.weights)[names(dbwb_data_df.w.weights) == "Plant_ID"] <- "Plant.Code"

#Wind speed
ws_data_df <- ws_data_df %>% select(-c(NEAR_RANK, State_1))

#Rename columns
names(ws_data_df)[names(ws_data_df) == "Plant_ID"] <- "Plant.Code"
names(ws_data_df)[names(ws_data_df) == "LAT"] <- "PLANT_LAT"
names(ws_data_df)[names(ws_data_df) == "LONG"] <- "PLANT_LONG"
names(ws_data_df)[names(ws_data_df) == "Lat_1"] <- "WBAN_LAT"
names(ws_data_df)[names(ws_data_df) == "Long_2"] <- "WBAN_LONG"

#Convert near distance from meters to miles
ws_data_df$NEAR_DIST_mi <- ws_data_df$NEAR_DIST / 1609.34

#Create invert diastance variable
ws_data_df$Inv_dist <- 1 / ws_data_df$NEAR_DIST_mi

#Sort data in asceding order by Plant.Code
ws_data_df <- ws_data_df[order(ws_data_df$Plant.Code), ]

#Calculate inv-dist weighted avg for monthly WS for each plant
WS.weighted.means <- ws_data_df %>% group_by(Plant.Code) %>% select(c(Plant.Code, 12:23,25)) %>%  
                     summarize_each(funs(weighted.mean(., Inv_dist, na.rm = T)), -Inv_dist) %>% 
                     round(., 0)
#Rename columns
colnames(WS.weighted.means)[2:13] <- paste("weighted", colnames(WS.weighted.means)[2:13], sep = ".")

#merge with raw data set
ws_data_df.w.weights<-left_join(ws_data_df, WS.weighted.means,by='Plant.Code')

#List plants that don't use 3 weather stations 
no.w.sta <- table(WS.weighted.means$Plant.Code)
plants.to.flag <- which(no.w.sta!=3)

names(ws_data_df.w.weights)[names(ws_data_df.w.weights) == "Plant_ID"] <- "Plant.Code"

wt_data_df <- wt_data_df %>% select(c(Plant_ID, Latitude, Longitude, State, County, contains("WT_pred")))
names(wt_data_df)[names(wt_data_df) == "Plant_ID"] <- "Plant.Code"

####Merge all environmental variable data


envVar_1 <- cbind(dbwb_data_df.w.weights[c(1,39:62)], ws_data_df.w.weights[c(26:37)])
envVar_1 <- envVar_1 %>%  group_by(Plant.Code) %>% filter(Plant.Code %in% plant_list) %>%  unique()

envVar_2 <- wt_data_df %>% filter(Plant.Code %in% plant_list) 

result <- (inner_join(envVar_1, envVar_2))

write.csv(result, paste0(outputData_path,'/environmental_variables.csv'), row.names = F)
return(result)
}
}



