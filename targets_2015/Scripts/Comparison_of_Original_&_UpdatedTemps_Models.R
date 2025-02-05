#Comparison of published withdrawal & consumption vs. updated temperature physical model predictions

#Remove "#" to download and install necessary R packages to run script (only need to do this once)
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("readxl")
install.packages("data.table")

#Functions to call necessary packages
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readxl)
library(Metrics)

################################################################################################################################
#Step 1: Manual setting of working directory
################################################################################################################################
##BEFORE RUNNING SCRIPT###
#Copy and paste location of downloaded downloaded Repository files and paste it
#to replace the path below to set the working directory.If path copies with backslash "\"
#replace all backslashes with a forward slash "/".
WorkingDirectory <- "C:/Users/kvalseth/Documents/Thermoelectric/targets_2015/"
setwd(WorkingDirectory)

################################################################################################################################
#Step 2: Import tower model outputs
################################################################################################################################
consumption_out_UpTemp <-read.csv("Model_archive/tower/Output/Tower_model_consumption_out_UpTemps_NoComplex.csv", stringsAsFactors = FALSE)
consumption_out_Orig <-read.csv("Model_archive/tower/Output/Tower_model_consumption_out_Orig.csv", stringsAsFactors = FALSE)

################################################################################################################################
#Step 3: Formatting oringinal and updated temperautre model outputs for monthly and annual datasets
################################################################################################################################
Med_Mod_Consump_UpTemp <- consumption_out_UpTemp[-1,c(1,15:26)]
colnames(Med_Mod_Consump_UpTemp) <- c("Plant_id", 1:12)
Monthly_Med_Mod_Consump_UpTemp <- pivot_longer(Med_Mod_Consump_UpTemp, cols = 2:13, names_to = 'Month', values_to = 'Model_Med_Consump_UpTemp')
Monthly_Med_Mod_Consump_UpTemp$Model_Med_Consump_UpTemp <- as.numeric(Monthly_Med_Mod_Consump_UpTemp$Model_Med_Consump_UpTemp)
Monthly_Med_Mod_Consump_UpTemp$Month <- as.numeric(Monthly_Med_Mod_Consump_UpTemp$Month)
Monthly_Med_Mod_Consump_UpTemp <- Monthly_Med_Mod_Consump_UpTemp %>% group_by(Plant_id, Month) %>% summarise(Model_Med_Consump_UpTemp = sum(Model_Med_Consump_UpTemp))
Annual_Med_Mod_Consump_UpTemp <- Monthly_Med_Mod_Consump_UpTemp %>% group_by(Plant_id) %>% summarise(Model_Med_Consump_UpTemp = mean(Model_Med_Consump_UpTemp))

Med_Mod_Consump_Orig <- consumption_out_Orig[-1,c(1,15:26)]
colnames(Med_Mod_Consump_Orig) <- c("Plant_id", 1:12)
Monthly_Med_Mod_Consump_Orig <- pivot_longer(Med_Mod_Consump_Orig, cols = 2:13, names_to = 'Month', values_to = 'Model_Med_Consump_Orig')
Monthly_Med_Mod_Consump_Orig$Model_Med_Consump_Orig <- as.numeric(Monthly_Med_Mod_Consump_Orig$Model_Med_Consump_Orig)
Monthly_Med_Mod_Consump_Orig$Month <- as.numeric(Monthly_Med_Mod_Consump_Orig$Month)
Monthly_Med_Mod_Consump_Orig <- Monthly_Med_Mod_Consump_Orig %>% group_by(Plant_id, Month) %>% summarise(Model_Med_Consump_Orig = sum(Model_Med_Consump_Orig))
Annual_Med_Mod_Consump_Orig <- Monthly_Med_Mod_Consump_Orig %>% group_by(Plant_id) %>% summarise(Model_Med_Consump_Orig = mean(Model_Med_Consump_Orig))

################################################################################################################################
#Step 4: Importing and formatting published consumption and withdrawal water use and plant location information
################################################################################################################################
#Create and set loctation for results
dir.create(path ="output/Model_Validation/" )
Model_pub_raw <- read_xlsx("input/tower/2015_monthly_TE_WD_CU.xlsx", skip = 4)
Model_pub_raw$`Plant ID` <- as.character(Model_pub_raw$`Plant ID`)
Plant_Loc <- read_xlsx("input/tower/2015_TE_Model_Estimates_lat.long_COMIDs.xlsx")
Plant_Loc <- select(Plant_Loc, EIA_PLANT_ID, LATITUDE, LONGITUDE)
Plant_Loc$EIA_PLANT_ID <- as.character(Plant_Loc$EIA_PLANT_ID)

Model_pub_Consump <- Model_pub_raw[,c(1,15:26)]
colnames(Model_pub_Consump) <- c("Plant_id", 1:12)
Model_pub_Consump_piv <- pivot_longer(Model_pub_Consump, cols = 2:13, names_to = 'Month', values_to = 'Model_pub_Consump')
Model_pub_Consump_piv$Month <- as.numeric(Model_pub_Consump_piv$Month)
Model_pub_Consump_piv <- Model_pub_Consump_piv %>% group_by(Plant_id, Month) %>% summarise(Model_pub_Consump = sum(Model_pub_Consump))

################################################################################################################################
#Step 5: Joining monthly published and updated temperature model output consumption datasets and generating withdrawal dataset
################################################################################################################################
Monthly_ModUpTemp_pub_Consump <- left_join( Monthly_Med_Mod_Consump_UpTemp, Model_pub_Consump_piv, by =  c("Plant_id", "Month"))
Monthly_ModUpTemp_pub_WD <-Monthly_ModUpTemp_pub_Consump
Monthly_ModUpTemp_pub_WD$Model_Med_WD_UpTemp <- Monthly_ModUpTemp_pub_WD$Model_Med_Consump_UpTemp*1.4
Monthly_ModUpTemp_pub_WD$Model_pub_WD <- Monthly_ModUpTemp_pub_WD$Model_pub_Consump*1.4

################################################################################################################################
#Step 6: Calculating percent error for monthly published vs Updated model output for consumption and withdrawal
################################################################################################################################
Monthly_ModUpTemp_pub_Consump$Error <- (Monthly_ModUpTemp_pub_Consump$Model_pub_Consump - Monthly_ModUpTemp_pub_Consump$Model_Med_Consump_UpTemp)
Monthly_ModUpTemp_pub_Consump$PercentError <- (Monthly_ModUpTemp_pub_Consump$Error/Monthly_ModUpTemp_pub_Consump$Model_pub_Consump)*100
Monthly_ModUpTemp_pub_WD$Error <- (Monthly_ModUpTemp_pub_WD$Model_pub_WD - Monthly_ModUpTemp_pub_WD$Model_Med_WD_UpTemp)
Monthly_ModUpTemp_pub_WD$PercentError <- (Monthly_ModUpTemp_pub_WD$Error/Monthly_ModUpTemp_pub_WD$Model_pub_WD)*100
Monthly_ModUptemp_pub_Consump_1PerError <-filter(Monthly_ModUpTemp_pub_Consump, abs(PercentError) > 1.0)
Plants_ModUptemp_pub_Consump_1PerError <- unique(Monthly_ModUptemp_pub_Consump_1PerError$Plant_id)
write.csv(Monthly_ModUpTemp_pub_Consump, "output/Model_Validation//ModUptemp_pub_Consump.csv")
write.csv(Monthly_ModUpTemp_pub_WD, "output/Model_Validation//ModUptemp_pub_WD.csv")
write.csv(Monthly_ModUptemp_pub_Consump_1PerError, "output/Model_Validation//ModUptemp_pub_Consump_1PerError.csv")

################################################################################################################################
#Step 7: Joining monthly published and original temperature model output consumption datasets and generating withdrawal dataset
################################################################################################################################
Monthly_ModOrig_pub_Consump <- left_join( Monthly_Med_Mod_Consump_Orig, Model_pub_Consump_piv, by =  c("Plant_id", "Month"))
Monthly_ModOrig_pub_WD <-Monthly_ModOrig_pub_Consump
Monthly_ModOrig_pub_WD$Model_Med_WD_Orig <- Monthly_ModOrig_pub_WD$Model_Med_Consump_Orig*1.4
Monthly_ModOrig_pub_WD$Model_pub_WD <- Monthly_ModOrig_pub_WD$Model_pub_Consump*1.4

################################################################################################################################
#Step 8: Calculating percent error for monthly published vs original model output for consumption and withdrawal
################################################################################################################################
Monthly_ModOrig_pub_Consump$Error <- (Monthly_ModOrig_pub_Consump$Model_pub_Consump - Monthly_ModOrig_pub_Consump$Model_Med_Consump_Orig)
Monthly_ModOrig_pub_Consump$PercentError <- (Monthly_ModOrig_pub_Consump$Error/Monthly_ModOrig_pub_Consump$Model_pub_Consump)*100
Monthly_ModOrig_pub_WD$Error <- (Monthly_ModOrig_pub_WD$Model_pub_WD - Monthly_ModOrig_pub_WD$Model_Med_WD_Orig)
Monthly_ModOrig_pub_WD$PercentError <- (Monthly_ModOrig_pub_WD$Error/Monthly_ModOrig_pub_WD$Model_pub_WD)*100
Monthly_ModOrig_pub_Consump_1PerError <-filter(Monthly_ModOrig_pub_Consump, abs(PercentError) > 1.0)
Plants_ModOrig_pub_Consump_1PerError <- unique(Monthly_ModOrig_pub_Consump_1PerError$Plant_id)
write.csv(Monthly_ModOrig_pub_Consump_1PerError, "output/Model_Validation//ModOrig_pub_Consump_1PerError.csv")

################################################################################################################################
#Step 9: Joining monthly original and updated temperature model output consumption datasets and generating withdrawal dataset
################################################################################################################################
Monthly_ModOrig_UpTemp_Consump <- left_join( Monthly_Med_Mod_Consump_Orig, Monthly_Med_Mod_Consump_UpTemp, by =  c("Plant_id", "Month"))
Monthly_ModOrig_UpTemp_WD <-Monthly_ModOrig_UpTemp_Consump
Monthly_ModOrig_UpTemp_WD$Model_Med_WD_Orig <- Monthly_ModOrig_UpTemp_WD$Model_Med_Consump_Orig*1.4
Monthly_ModOrig_UpTemp_WD$Model_Med_WD_UpTemp <- Monthly_ModOrig_UpTemp_WD$Model_Med_Consump_UpTemp*1.4

################################################################################################################################
#Step 10: Calculating percent error for monthly original vs updated model outputs for consumption and withdrawal
################################################################################################################################
Monthly_ModOrig_UpTemp_Consump$Error <- (Monthly_ModOrig_UpTemp_Consump$Model_Med_Consump_Orig - Monthly_ModOrig_UpTemp_Consump$Model_Med_Consump_UpTemp)
Monthly_ModOrig_UpTemp_Consump$PercentError <- (Monthly_ModOrig_UpTemp_Consump$Error/Monthly_ModOrig_UpTemp_Consump$Model_Med_Consump_Orig)*100
Monthly_ModOrig_UpTemp_WD$Error <- (Monthly_ModOrig_UpTemp_WD$Model_Med_WD_Orig - Monthly_ModOrig_UpTemp_WD$Model_Med_WD_UpTemp)
Monthly_ModOrig_UpTemp_WD$PercentError <- (Monthly_ModOrig_UpTemp_WD$Error/Monthly_ModOrig_UpTemp_WD$Model_Med_WD_Orig)*100
Monthly_ModOrig_UpTemp_Consump_1PerError <-filter(Monthly_ModOrig_UpTemp_Consump, abs(PercentError) > 1.0)
Plants_ModOrig_UpTemp_Consump_1PerError <- unique(Monthly_ModOrig_UpTemp_Consump_1PerError$Plant_id)
write.csv(Monthly_ModOrig_UpTemp_Consump, "output/Model_Validation//ModOrig_UpTemp_Consump.csv")
write.csv(Monthly_ModOrig_UpTemp_WD, "output/Model_Validation//ModOrig_UpTemp_WD.csv")
write.csv(Monthly_ModOrig_UpTemp_Consump_1PerError, "output/Model_Validation//ModOrig_UpTemp_Consump_1PerError.csv")

################################################################################################################################
#Step 11: Function for setting monthly plot formatting
################################################################################################################################
#Plotting
theme_USGS <-  function(base_size = 8){
  theme(
    plot.title = element_text (vjust = 3, size = 14, family="serif"),
    plot.subtitle = element_text(hjust = 1, size = 12,family="serif"),
    plot.margin = unit (c(5.5, 5, 5.5, 5), "lines"), 
    panel.border = element_rect (colour = "black", fill = F, size = 0.1),
    panel.grid.major = element_blank (),
    panel.grid.minor = element_blank (),
    panel.background = element_rect (fill = "white"),
    legend.background = element_blank(),
    legend.justification=c(0, 0),
    legend.position = "bottom",
    legend.key = element_blank (),
    legend.title = element_blank(),
    legend.text = element_text (size = 11),
    axis.title.x = element_text (size = 11, family="serif"),
    axis.title.y = element_text (vjust = 1, angle = 90, size = 11, family="serif"),
    axis.text.x = element_text (size = 11, vjust = -0.25, colour = "black", 
                                family="serif", margin=margin(10,5,20,5,"pt")),
    axis.text.y = element_text (size = 8, hjust = 1, colour = "black", 
                                family="serif", margin=margin(5,10,10,5,"pt")),
    axis.ticks = element_line (colour = "black", size = 0.1),
    axis.ticks.length = unit(-0.25 , "cm"),
    axis.ticks.margin = unit(0.5, "cm"),
    plot.caption = element_text(hjust = 0.5)
  )
}
################################################################################################################################
#Step 12: Percent error distribution plots for comparison of published values versus both model outputs
################################################################################################################################
pdf(file = paste0('output/Model_Validation//Distribution_ModUptemp_Pub_PerError.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModUpTemp_pub_Consump[,c(1,6)], aes(x = PercentError)) +
  geom_histogram(binwidth= 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-180, 20, by = 20), labels = as.character(seq(-180, 20, by = 20)))+
  scale_y_continuous(breaks = seq(0, 1200, by = 100), labels = as.character(seq(0, 1200, by = 100)))+
  labs(x = 'Water Consumption Percent Error (%)', y = 'Count',
       title = "Original Published vs. Updated Temperature Input Water Consumption" ) 
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Distribution_ModUptemp_Pub_PerError_WD.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModUpTemp_pub_WD[,c(1,8)], aes(x = PercentError)) +
  geom_histogram(binwidth= 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-180, 20, by = 20), labels = as.character(seq(-180, 20, by = 20)))+
  scale_y_continuous(breaks = seq(0, 1200, by = 100), labels = as.character(seq(0, 1200, by = 100)))+
  labs(x = 'Water Withdrawal Percent Error (%)', y = 'Count',
       title = "Original Published vs. Updated Temperature Input Water Withdrawal" ) 
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Distribution_ModOrig_Pub_PerError.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModOrig_pub_Consump[,c(1,6)], aes(x = PercentError)) +
  geom_histogram(binwidth= 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-1, 8, by = 1), labels = as.character(seq(-1, 8, by = 1)))+
  scale_y_continuous(breaks = seq(0, 8000, by = 1000), labels = as.character(seq(0, 8000, by = 1000)))+
  labs(x = 'Water Consumption Percent Error (%)', y = 'Count',
       title = "Original Published vs. Original Input Water Consumption" ) 
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Distribution_ModOrig_UpTemp_PerError.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModOrig_UpTemp_Consump[,c(1,6)], aes(x = PercentError)) +
  geom_histogram(binwidth= 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-180, 20, by = 20), labels = as.character(seq(-180, 20, by = 20)))+
  scale_y_continuous(breaks = seq(0, 1200, by = 100), labels = as.character(seq(0, 1200, by = 100)))+
  labs(x = 'Water Consumption Percent Error (%)', y = 'Count',
       title = "Original Input vs. Updated Temperature Input Water Consumption" ) 
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Distribution_ModOrig_UpTemp_PerError_WD.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModOrig_UpTemp_WD[,c(1,8)], aes(x = PercentError)) +
  geom_histogram(binwidth= 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-180, 20, by = 20), labels = as.character(seq(-180, 20, by = 20)))+
  scale_y_continuous(breaks = seq(0, 1200, by = 100), labels = as.character(seq(0, 1200, by = 100)))+
  labs(x = 'Water Withdrawal Percent Error (%)', y = 'Count',
       title = "Original Input vs. Updated Temperature Input Water Withdrawal" ) 
print(p)
dev.off()

################################################################################################################################
#Step 13: Error (actual value) distribution plots for comparison of published values versus both model outputs
################################################################################################################################
pdf(file = paste0('output/Model_Validation//Distribution_ModUptemp_Pub_Error.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModUpTemp_pub_Consump[,c(1,5)], aes(x = Error)) +
  geom_histogram(binwidth= 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-180, 20, by = 10), labels = as.character(seq(-180, 20, by = 10)))+
  scale_y_continuous(breaks = seq(0, 12000, by = 1000), labels = as.character(seq(0, 12000, by = 1000)))+
  labs(x = 'Water Consumption Error (Pub - UpTemp) in Million Gallons per Day', y = 'Count',
       title = "Original Published vs. Updated Temperature Input Water Consumption" ) 
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Distribution_ModUptemp_Pub_Error_WD.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModUpTemp_pub_WD[,c(1,7)], aes(x = Error)) +
  geom_histogram(binwidth= 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-180, 20, by = 10), labels = as.character(seq(-180, 20, by = 10)))+
  scale_y_continuous(breaks = seq(0, 12000, by = 1000), labels = as.character(seq(0, 12000, by = 1000)))+
  labs(x = 'Water Withdrawal Error (Pub - UpTemp) in Million Gallons per Day', y = 'Count',
       title = "Original Published vs. Updated Temperature Input Water Withdrawal" ) 
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Distribution_ModOrig_Pub_Error.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModOrig_pub_Consump[,c(1,5)], aes(x = Error)) +
  geom_histogram(binwidth= 0.25, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-3, 3, by = 0.25), labels = as.character(seq(-3, 3, by = 0.25)))+
  scale_y_continuous(breaks = seq(0, 8000, by = 1000), labels = as.character(seq(0, 8000, by = 1000)))+
  labs(x = 'Water Consumption Error (Pub - Orig) in Million Gallons per Day', y = 'Count',
       title = "Original Published vs. Original Input Water Consumption" ) 
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Distribution_ModOrig_Pub_Error_WD.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModOrig_pub_WD[,c(1,7)], aes(x = Error)) +
  geom_histogram(binwidth= 0.25, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-3, 3, by = 0.25), labels = as.character(seq(-3, 3, by = 0.25)))+
  scale_y_continuous(breaks = seq(0, 8000, by = 1000), labels = as.character(seq(0, 8000, by = 1000)))+
  labs(x = 'Water Withdrawal Error (Pub - Orig) in Million Gallons per Day', y = 'Count',
       title = "Original Published vs. Original Input Water Withdrawal" ) 
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Distribution_ModOrig_UpTemp_Error.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModOrig_UpTemp_Consump[,c(1,5)], aes(x = Error)) +
  geom_histogram(binwidth= 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-180, 20, by = 10), labels = as.character(seq(-180, 20, by = 10)))+
  scale_y_continuous(breaks = seq(0, 12000, by = 1000), labels = as.character(seq(0, 12000, by = 1000)))+
  labs(x = 'Water Consumption Error (Orig - UpTemp) in Million Gallons per Day', y = 'Count',
       title = "Original Input vs. Updated Temperature Input Water Consumption" ) 
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Distribution_ModOrig_UpTemp_Error_WD.pdf'), width = 10, height = 7)
p <- ggplot(Monthly_ModOrig_UpTemp_WD[,c(1,7)], aes(x = Error)) +
  geom_histogram(binwidth= 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_continuous(breaks = seq(-180, 20, by = 10), labels = as.character(seq(-180, 20, by = 10)))+
  scale_y_continuous(breaks = seq(0, 12000, by = 1000), labels = as.character(seq(0, 12000, by = 1000)))+
  labs(x = 'Water Withdrawal Error (Orig - UpTemp) in Million Gallons per Day', y = 'Count',
       title = "Original Input vs. Updated Temperature Input Water Withdrawal" ) 
print(p)
dev.off()

################################################################################################################################
#Step 14: Plots of months with lowest and highest percent error of consumption for published versus both model outputs
################################################################################################################################
Annual_Med_UpTemp_Pub_MinMonth <- data.frame(stringsAsFactors = FALSE)
for (i in unique(Monthly_ModUpTemp_pub_Consump$Plant_id)) {
  Mod_Pub <- filter(Monthly_ModUpTemp_pub_Consump , Plant_id == i)
  MinRec <- Mod_Pub[which.min(abs(Mod_Pub$PercentError)),]
  temp <- c(i, MinRec$Month)
  Annual_Med_UpTemp_Pub_MinMonth <- rbind(Annual_Med_UpTemp_Pub_MinMonth , temp, stringsAsFactors = FALSE)
}
colnames(Annual_Med_UpTemp_Pub_MinMonth) <- c("Plant_id", "Uptemp_Pub_MinMonth")
Annual_Med_UpTemp_Pub_MinMonth$Uptemp_Pub_MinMonth <- as.numeric(Annual_Med_UpTemp_Pub_MinMonth$Uptemp_Pub_MinMonth)
pdf(file = paste0('output/Model_Validation//Distribution_Uptemp_Pub_MinMonth.pdf'), width = 10, height = 7)
p <- ggplot(Annual_Med_UpTemp_Pub_MinMonth, aes(x = Uptemp_Pub_MinMonth)) +
  geom_bar(width = 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_discrete(limits = month.abb)+
  labs(x = 'Month with Lowest Percent Error for Water Consumption in Million Gallons per Day', y = 'Count',
       title = "Published vs. Updated Temperature Input Water Consumption" ) 
print(p)
dev.off()

Annual_Med_Orig_Pub_MinMonth <- data.frame(stringsAsFactors = FALSE)
for (i in unique(Monthly_ModOrig_pub_Consump$Plant_id)) {
  Mod_Pub <- filter(Monthly_ModOrig_pub_Consump , Plant_id == i)
  MinRec <- Mod_Pub[which.min(abs(Mod_Pub$PercentError)),]
  temp <- c(i, MinRec$Month)
  Annual_Med_Orig_Pub_MinMonth <- rbind(Annual_Med_Orig_Pub_MinMonth , temp, stringsAsFactors = FALSE)
}
colnames(Annual_Med_Orig_Pub_MinMonth) <- c("Plant_id", "Orig_Pub_MinMonth")
Annual_Med_Orig_Pub_MinMonth$Orig_Pub_MinMonth <- as.numeric(Annual_Med_Orig_Pub_MinMonth$Orig_Pub_MinMonth)
pdf(file = paste0('output/Model_Validation//Distribution_Orig_Pub_MinMonth.pdf'), width = 10, height = 7)
p <- ggplot(Annual_Med_Orig_Pub_MinMonth, aes(x = Orig_Pub_MinMonth)) +
  geom_bar(width = 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_discrete(limits = month.abb)+
  labs(x = 'Month with Lowest Percent Error for Water Consumption in Million Gallons per Day', y = 'Count',
       title = "Published vs. Original Temperature Input Water Consumption" ) 
print(p)
dev.off()

Annual_Med_Orig_UpTemp_MinMonth <- data.frame(stringsAsFactors = FALSE)
for (i in unique(Monthly_ModOrig_UpTemp_Consump$Plant_id)) {
  Mod_Pub <- filter(Monthly_ModOrig_UpTemp_Consump , Plant_id == i)
  MinRec <- Mod_Pub[which.min(abs(Mod_Pub$PercentError)),]
  temp <- c(i, MinRec$Month)
  Annual_Med_Orig_UpTemp_MinMonth <- rbind(Annual_Med_Orig_UpTemp_MinMonth , temp, stringsAsFactors = FALSE)
}
colnames(Annual_Med_Orig_UpTemp_MinMonth) <- c("Plant_id", "Orig_UpTemp_MinMonth")
Annual_Med_Orig_UpTemp_MinMonth$Orig_UpTemp_MinMonth <- as.numeric(Annual_Med_Orig_UpTemp_MinMonth$Orig_UpTemp_MinMonth)
pdf(file = paste0('output/Model_Validation//Distribution_Orig_UpTemp_MinMonth.pdf'), width = 10, height = 7)
p <- ggplot(Annual_Med_Orig_UpTemp_MinMonth, aes(x = Orig_UpTemp_MinMonth)) +
  geom_bar(width = 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_discrete(limits = month.abb)+
  labs(x = 'Month with Lowest Percent Error for Water Consumption in Million Gallons per Day', y = 'Count',
       title = "Updated vs. Original Temperature Input Water Consumption" ) 
print(p)
dev.off()

Annual_Med_UpTemp_Pub_MaxMonth <- data.frame(stringsAsFactors = FALSE)
for (i in unique(Monthly_ModUpTemp_pub_Consump$Plant_id)) {
  Mod_Pub <- filter(Monthly_ModUpTemp_pub_Consump , Plant_id == i)
  MaxRec <- Mod_Pub[which.max(abs(Mod_Pub$PercentError)),]
  temp <- c(i, MaxRec$Month)
  Annual_Med_UpTemp_Pub_MaxMonth <- rbind(Annual_Med_UpTemp_Pub_MaxMonth , temp, stringsAsFactors = FALSE)
}
colnames(Annual_Med_UpTemp_Pub_MaxMonth) <- c("Plant_id", "Uptemp_Pub_MaxMonth")
Annual_Med_UpTemp_Pub_MaxMonth$Uptemp_Pub_MaxMonth <- as.numeric(Annual_Med_UpTemp_Pub_MaxMonth$Uptemp_Pub_MaxMonth)
pdf(file = paste0('output/Model_Validation//Distribution_Uptemp_Pub_MaxMonth.pdf'), width = 10, height = 7)
p <- ggplot(Annual_Med_UpTemp_Pub_MaxMonth, aes(x = Uptemp_Pub_MaxMonth)) +
  geom_bar(width = 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_discrete(limits = month.abb)+
  labs(x = 'Month with Highest Percent Error for Water Consumption in Million Gallons per Day', y = 'Count',
       title = "Published vs. Updated Temperature Input Water Consumption" ) 
print(p)
dev.off()

Annual_Med_Orig_Pub_MaxMonth <- data.frame(stringsAsFactors = FALSE)
for (i in unique(Monthly_ModOrig_pub_Consump$Plant_id)) {
  Mod_Pub <- filter(Monthly_ModOrig_pub_Consump , Plant_id == i)
  MaxRec <- Mod_Pub[which.max(abs(Mod_Pub$PercentError)),]
  temp <- c(i, MaxRec$Month)
  Annual_Med_Orig_Pub_MaxMonth <- rbind(Annual_Med_Orig_Pub_MaxMonth , temp, stringsAsFactors = FALSE)
}
colnames(Annual_Med_Orig_Pub_MaxMonth) <- c("Plant_id", "Orig_Pub_MaxMonth")
Annual_Med_Orig_Pub_MaxMonth$Orig_Pub_MaxMonth <- as.numeric(Annual_Med_Orig_Pub_MaxMonth$Orig_Pub_MaxMonth)
pdf(file = paste0('output/Model_Validation//Distribution_Orig_Pub_MaxMonth.pdf'), width = 10, height = 7)
p <- ggplot(Annual_Med_Orig_Pub_MaxMonth, aes(x = Orig_Pub_MaxMonth)) +
  geom_bar(width = 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_discrete(limits = month.abb)+
  labs(x = 'Month with Highest Percent Error for Water Consumption in Million Gallons per Day', y = 'Count',
       title = "Published vs. Original Temperature Input Water Consumption" ) 
print(p)
dev.off()

Annual_Med_Orig_UpTemp_MaxMonth <- data.frame(stringsAsFactors = FALSE)
for (i in unique(Monthly_ModOrig_UpTemp_Consump$Plant_id)) {
  Mod_Pub <- filter(Monthly_ModOrig_UpTemp_Consump , Plant_id == i)
  MaxRec <- Mod_Pub[which.max(abs(Mod_Pub$PercentError)),]
  temp <- c(i, MaxRec$Month)
  Annual_Med_Orig_UpTemp_MaxMonth <- rbind(Annual_Med_Orig_UpTemp_MaxMonth , temp, stringsAsFactors = FALSE)
}
colnames(Annual_Med_Orig_UpTemp_MaxMonth) <- c("Plant_id", "Orig_UpTemp_MaxMonth")
Annual_Med_Orig_UpTemp_MaxMonth$Orig_UpTemp_MaxMonth <- as.numeric(Annual_Med_Orig_UpTemp_MaxMonth$Orig_UpTemp_MaxMonth)
pdf(file = paste0('output/Model_Validation//Distribution_Orig_UpTemp_MaxMonth.pdf'), width = 10, height = 7)
p <- ggplot(Annual_Med_Orig_UpTemp_MaxMonth, aes(x = Orig_UpTemp_MaxMonth)) +
  geom_bar(width = 1, colour="black", fill="white") +
  theme_USGS()+  
  scale_x_discrete(limits = month.abb)+
  labs(x = 'Month with Highest Percent Error for Water Consumption in Million Gallons per Day', y = 'Count',
       title = "Updated vs. Original Temperature Input Water Consumption" ) 
print(p)
dev.off()

################################################################################################################################
#Step 15: Individual plant comparison plots for published versus updated model output for consumption and withdrawal
################################################################################################################################

pdf(file = paste0('output/Model_Validation//Monthly_UpTemp_vs_pub_Consump.pdf'), width = 10, height = 7)
for (i in unique(Monthly_ModUpTemp_pub_Consump$Plant_id)) {
  Mod_Pub <- filter(Monthly_ModUpTemp_pub_Consump, Plant_id == i)
  Plant_AvgError <- round(mean(Mod_Pub$Error),2)
  Plant_MinError <- round(min(Mod_Pub$Error),2)
  Plant_MaxError <- round(max(Mod_Pub$Error),2)
  p <- ggplot(Mod_Pub) +
    geom_point(aes(x = Month, y = Model_Med_Consump_UpTemp, colour = "Updated Model Output")) +
    geom_point(aes(x = Month,  y = Model_pub_Consump, colour = "Published Model data" ))+
    theme_USGS() +  
    scale_colour_manual("", 
                        breaks = c("Updated Model Output", "Published Model data"),
                        values = c("Updated Model Output"="black", "Published Model data"="red")) +
    labs(x = 'Month', y = 'Consumption water use in Million Gallons per Day', 
         caption = paste0("Minimum Error = ",Plant_MinError, "     Average Error = ", Plant_AvgError, "     Maximum Error = ", Plant_MaxError) ,
         title = paste0("Updated monthly median model output & published consumption water use            Plant id = ", i )) 
  print(p)
}
dev.off()

pdf(file = paste0('output/Model_Validation//Monthly_UpTemp_vs_pub_WD.pdf'), width = 10, height = 7)
for (i in unique(Monthly_ModUpTemp_pub_WD$Plant_id)) {
  Mod_Pub <- filter(Monthly_ModUpTemp_pub_WD, Plant_id == i)
  Plant_AvgError <- round(mean(Mod_Pub$Error),2)
  Plant_MinError <- round(min(Mod_Pub$Error),2)
  Plant_MaxError <- round(max(Mod_Pub$Error),2)
  p <- ggplot(Mod_Pub) +
    geom_point(aes(x = Month, y = Model_Med_WD_UpTemp, colour = "Updated Model Output")) +
    geom_point(aes(x = Month,  y = Model_pub_WD, colour = "Published Model data" ))+
    theme_USGS() +  
    scale_colour_manual("", 
                        breaks = c("Updated Model Output", "Published Model data"),
                        values = c("Updated Model Output"="black", "Published Model data"="red")) +
    labs(x = 'Month', y = 'Withdrawal water use in Million Gallons per Day', 
         caption = paste0("Minimum Error = ",Plant_MinError, "     Average Error = ", Plant_AvgError, "     Maximum Error = ", Plant_MaxError) ,
         title = paste0("Updated monthly median model output & published withdrawal water use            Plant id = ", i )) 
  print(p)
}
dev.off()

################################################################################################################################
#Step 16: Function for setting formatting for annual plots
################################################################################################################################
theme_USGS <-  function(base_size = 8){
  theme(
    plot.title = element_text (vjust = 3, size = 14, family="serif"),
    plot.subtitle = element_text(hjust = 1, size = 12,family="serif"),
    plot.margin = unit (c(5.5, 5, 5.5, 5), "lines"), 
    panel.border = element_rect (colour = "black", fill = F, size = 0.1),
    panel.grid.major = element_blank (),
    panel.grid.minor = element_blank (),
    panel.background = element_rect (fill = "white"),
    legend.background = element_blank(),
    legend.justification=c(0.5, 0.5),
    legend.position = "bottom",
    legend.key = element_blank (),
    legend.title = element_blank(),
    legend.text = element_text (size = 11),
    axis.title.x = element_text (size = 11, family="serif"),
    axis.title.y = element_text (vjust = 1, angle = 90, size = 11, family="serif"),
    axis.text.x = element_text (size = 11, vjust = -0.25, colour = "white", 
                                family="serif", margin=margin(10,5,20,5,"pt")),
    axis.text.y = element_text (size = 8, hjust = 1, colour = "black", 
                                family="serif", margin=margin(5,10,10,5,"pt")),
    axis.ticks = element_line (colour = "black", size = 0.1),    
    axis.ticks.length.x = unit(0 , "cm"),
    axis.ticks.length.y = unit(-0.25 , "cm"),
    axis.ticks.margin = unit(0.5, "cm"),
    plot.caption = element_text(hjust = 0.5)
  )
}

################################################################################################################################
#Step 17: Joining annual original and updated model output datasets and calculating withdrawal dataset
################################################################################################################################
Annual_Med_UpTemp_Orig_Consump <- left_join(Annual_Med_Mod_Consump_UpTemp, Annual_Med_Mod_Consump_Orig, by = "Plant_id")
Annual_Med_UpTemp_Orig_WD <- left_join(Annual_Med_Mod_Consump_UpTemp, Annual_Med_Mod_Consump_Orig, by = "Plant_id")
Annual_Med_UpTemp_Orig_WD$Model_Med_WD_UpTemp <- Annual_Med_UpTemp_Orig_WD$Model_Med_Consump_UpTemp*1.4
Annual_Med_UpTemp_Orig_WD$Model_Med_WD_Orig <- Annual_Med_UpTemp_Orig_WD$Model_Med_Consump_Orig*1.4

################################################################################################################################
#Step 18: Calculating percent error for annual original vs updated model output consumption and withdrawal datasets
################################################################################################################################
Annual_Med_UpTemp_Orig_Consump$Error <- (Annual_Med_UpTemp_Orig_Consump$Model_Med_Consump_Orig-Annual_Med_UpTemp_Orig_Consump$Model_Med_Consump_UpTemp)
Annual_Med_UpTemp_Orig_Consump$PercentError <- (Annual_Med_UpTemp_Orig_Consump$Error/Annual_Med_UpTemp_Orig_Consump$Model_Med_Consump_Orig)*100
Annual_Med_UpTemp_Orig_WD$Error <- (Annual_Med_UpTemp_Orig_WD$Model_Med_WD_Orig-Annual_Med_UpTemp_Orig_WD$Model_Med_WD_UpTemp)
Annual_Med_UpTemp_Orig_WD$PercentError <- (Annual_Med_UpTemp_Orig_WD$Error/Annual_Med_UpTemp_Orig_WD$Model_Med_WD_Orig)*100

################################################################################################################################
#Step 19: Annual plots of original versus updated model output results for consumption and withdrawal
################################################################################################################################
pdf(file = paste0('output/Model_Validation//Annual_UpTemp_vs_orig_Consump.pdf'), width = 10, height = 7)
Plant_AvgError <- round(mean(Annual_Med_UpTemp_Orig_Consump$Error),2)
Plant_MinError <- round(min(Annual_Med_UpTemp_Orig_Consump$Error),2)
Plant_MaxError <- round(max(Annual_Med_UpTemp_Orig_Consump$Error),2)
Plant_AvgPerError <- round(mean(Annual_Med_UpTemp_Orig_Consump$PercentError),2)
Plant_MinPerError <- round(min(Annual_Med_UpTemp_Orig_Consump$PercentError),2)
Plant_MaxPerError <- round(max(Annual_Med_UpTemp_Orig_Consump$PercentError),2)
p <- ggplot(Annual_Med_UpTemp_Orig_Consump) +
  geom_point(aes(x = Plant_id, y = Model_Med_Consump_UpTemp, colour = "Updated Model Output")) +
  geom_point(aes(x = Plant_id,  y = Model_Med_Consump_Orig, colour = "Original Model data" ))+
  theme_USGS() +  
  scale_colour_manual("", 
                      breaks = c("Updated Model Output", "Original Model data"),
                      values = c("Updated Model Output"="black", "Original Model data"="red")) +
labs(x = 'Plant id', y = 'Consumption water use in Million Gallons per Day', 
  caption = paste0("Minimum Error = ",Plant_MinError, "        Average Error = ", Plant_AvgError, "         Maximum Error = ", Plant_MaxError,
                "\nMinimum % Error = ",Plant_MinPerError, "     Average % Error = ", Plant_AvgPerError, "     Maximum % Error = ", Plant_MaxPerError) ,
  title = "Annual median original & updated consumption model outputs")
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Annual_UpTemp_vs_orig_WD.pdf'), width = 10, height = 7)
Plant_AvgError <- round(mean(Annual_Med_UpTemp_Orig_WD$Error),2)
Plant_MinError <- round(min(Annual_Med_UpTemp_Orig_WD$Error),2)
Plant_MaxError <- round(max(Annual_Med_UpTemp_Orig_WD$Error),2)
Plant_AvgPerError <- round(mean(Annual_Med_UpTemp_Orig_WD$PercentError),2)
Plant_MinPerError <- round(min(Annual_Med_UpTemp_Orig_WD$PercentError),2)
Plant_MaxPerError <- round(max(Annual_Med_UpTemp_Orig_WD$PercentError),2)
p <- ggplot(Annual_Med_UpTemp_Orig_WD) +
  geom_point(aes(x = Plant_id, y = Model_Med_WD_UpTemp, colour = "Updated Model Output")) +
  geom_point(aes(x = Plant_id,  y = Model_Med_WD_Orig, colour = "Original Model data" ))+
  theme_USGS() +  
  scale_colour_manual("", 
                      breaks = c("Updated Model Output", "Original Model data"),
                      values = c("Updated Model Output"="black", "Original Model data"="red")) +
  labs(x = 'Plant id', y = 'Withdrawal water use in Million Gallons per Day', 
       caption = paste0("Minimum Error = ",Plant_MinError, "        Average Error = ", Plant_AvgError, "         Maximum Error = ", Plant_MaxError,
                        "\nMinimum % Error = ",Plant_MinPerError, "     Average % Error = ", Plant_AvgPerError, "     Maximum % Error = ", Plant_MaxPerError) ,
       title = "Annual median original & updated withdrawal model outputs")
print(p)
dev.off()

################################################################################################################################
#Step 20: Joining annual published and original model output datasets and calculating withdrawal dataset
################################################################################################################################
Annual_Med_Orig_Pub_Consump <- Monthly_ModOrig_pub_Consump[,c(1,3,4)] %>% group_by(Plant_id) %>% summarise(across(everything(), list(mean)))
colnames(Annual_Med_Orig_Pub_Consump) <- c("Plant_id", "Model_Med_Consump_Orig", "Model_Med_Consump_Pub")
Annual_Med_Orig_Pub_WD <- Monthly_ModOrig_pub_Consump[,c(1,3,4)] %>% group_by(Plant_id) %>% summarise(across(everything(), list(mean)))
colnames(Annual_Med_Orig_Pub_WD) <- c("Plant_id", "Model_Med_WD_Orig", "Model_Med_WD_Pub")
Annual_Med_Orig_Pub_WD$Model_Med_WD_Orig <- Annual_Med_Orig_Pub_WD$Model_Med_WD_Orig*1.4
Annual_Med_Orig_Pub_WD$Model_Med_WD_Pub <- Annual_Med_Orig_Pub_WD$Model_Med_WD_Pub*1.4

################################################################################################################################
#Step 21: Calculating percent error for annual published vs original model output consumption and withdrawal datasets
################################################################################################################################
Annual_Med_Orig_Pub_Consump$Error <- (Annual_Med_Orig_Pub_Consump$Model_Med_Consump_Orig-Annual_Med_Orig_Pub_Consump$Model_Med_Consump_Pub)
Annual_Med_Orig_Pub_Consump$PercentError <- (Annual_Med_Orig_Pub_Consump$Error/Annual_Med_Orig_Pub_Consump$Model_Med_Consump_Pub)*100
Annual_Med_Orig_Pub_WD$Error <- (Annual_Med_Orig_Pub_WD$Model_Med_WD_Pub-Annual_Med_Orig_Pub_WD$Model_Med_WD_Orig)
Annual_Med_Orig_Pub_WD$PercentError <- (Annual_Med_Orig_Pub_WD$Error/Annual_Med_Orig_Pub_WD$Model_Med_WD_Pub)*100

################################################################################################################################
#Step 22: Annual plots of published versus original model output results for consumption and withdrawal
################################################################################################################################
pdf(file = paste0('output/Model_Validation//Annual_Orig_vs_Pub_Consump.pdf'), width = 10, height = 7)
Plant_AvgError <- round(mean(Annual_Med_Orig_Pub_Consump$Error),2)
Plant_MinError <- round(min(Annual_Med_Orig_Pub_Consump$Error),2)
Plant_MaxError <- round(max(Annual_Med_Orig_Pub_Consump$Error),2)
Plant_AvgPerError <- round(mean(Annual_Med_Orig_Pub_Consump$PercentError),2)
Plant_MinPerError <- round(min(Annual_Med_Orig_Pub_Consump$PercentError),2)
Plant_MaxPerError <- round(max(Annual_Med_Orig_Pub_Consump$PercentError),2)
p <- ggplot(Annual_Med_Orig_Pub_Consump) +
  geom_point(aes(x = Plant_id, y = Model_Med_Consump_Orig, colour = "Updated Model Output")) +
  geom_point(aes(x = Plant_id,  y = Model_Med_Consump_Pub, colour = "Published data" ))+
  theme_USGS() +  
  scale_colour_manual("", 
                      breaks = c("Updated Model Output", "Published data"),
                      values = c("Updated Model Output"="black", "Published data"="red")) +
  labs(x = 'Plant id', y = 'Consumption water use in Million Gallons per Day', 
       caption = paste0("Minimum Error = ",Plant_MinError, "        Average Error = ", Plant_AvgError, "         Maximum Error = ", Plant_MaxError,
                        "\nMinimum % Error = ",Plant_MinPerError, "     Average % Error = ", Plant_AvgPerError, "     Maximum % Error = ", Plant_MaxPerError) ,
       title = "Annual median original consumption model output & published results")
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Annual_Orig_vs_Pub_WD.pdf'), width = 10, height = 7)
Plant_AvgError <- round(mean(Annual_Med_Orig_Pub_WD$Error),2)
Plant_MinError <- round(min(Annual_Med_Orig_Pub_WD$Error),2)
Plant_MaxError <- round(max(Annual_Med_Orig_Pub_WD$Error),2)
Plant_AvgPerError <- round(mean(Annual_Med_Orig_Pub_WD$PercentError),2)
Plant_MinPerError <- round(min(Annual_Med_Orig_Pub_WD$PercentError),2)
Plant_MaxPerError <- round(max(Annual_Med_Orig_Pub_WD$PercentError),2)
p <- ggplot(Annual_Med_Orig_Pub_WD) +
  geom_point(aes(x = Plant_id, y = Model_Med_WD_Orig, colour = "Updated Model Output")) +
  geom_point(aes(x = Plant_id,  y = Model_Med_WD_Pub, colour = "Published data" ))+
  theme_USGS() +  
  scale_colour_manual("", 
                      breaks = c("Updated Model Output", "Published data"),
                      values = c("Updated Model Output"="black", "Published data"="red")) +
  labs(x = 'Plant id', y = 'Withdrawal water use in Million Gallons per Day', 
       caption = paste0("Minimum Error = ",Plant_MinError, "        Average Error = ", Plant_AvgError, "         Maximum Error = ", Plant_MaxError,
                        "\nMinimum % Error = ",Plant_MinPerError, "     Average % Error = ", Plant_AvgPerError, "     Maximum % Error = ", Plant_MaxPerError) ,
       title = "Annual median updated withdrawal model output & published results")
print(p)
dev.off()

################################################################################################################################
#Step 23: Joining annual published and updated model output datasets and calculating withdrawal dataset
################################################################################################################################
Annual_Med_UpTemp_Pub_Consump <- Monthly_ModUpTemp_pub_Consump[,c(1,3,4)] %>% group_by(Plant_id) %>% summarise(across(everything(), list(mean)))
colnames(Annual_Med_UpTemp_Pub_Consump) <- c("Plant_id", "Model_Med_Consump_UpTemp", "Model_Med_Consump_Pub")
Annual_Med_UpTemp_Pub_WD <- Monthly_ModUpTemp_pub_Consump[,c(1,3,4)] %>% group_by(Plant_id) %>% summarise(across(everything(), list(mean)))
colnames(Annual_Med_UpTemp_Pub_WD) <- c("Plant_id", "Model_Med_WD_UpTemp", "Model_Med_WD_Pub")
Annual_Med_UpTemp_Pub_WD$Model_Med_WD_UpTemp <- Annual_Med_UpTemp_Pub_WD$Model_Med_WD_UpTemp*1.4
Annual_Med_UpTemp_Pub_WD$Model_Med_WD_Pub <- Annual_Med_UpTemp_Pub_WD$Model_Med_WD_Pub*1.4

################################################################################################################################
#Step 24: Calculating percent error for annual published versus updated model output consumption and withdrawal datasets
################################################################################################################################
Annual_Med_UpTemp_Pub_Consump$Error <- (Annual_Med_UpTemp_Pub_Consump$Model_Med_Consump_UpTemp-Annual_Med_UpTemp_Pub_Consump$Model_Med_Consump_Pub)
Annual_Med_UpTemp_Pub_Consump$PercentError <- (Annual_Med_UpTemp_Pub_Consump$Error/Annual_Med_UpTemp_Pub_Consump$Model_Med_Consump_Pub)*100
Annual_Med_UpTemp_Pub_WD$Error <- (Annual_Med_UpTemp_Pub_WD$Model_Med_WD_Pub-Annual_Med_UpTemp_Pub_WD$Model_Med_WD_UpTemp)
Annual_Med_UpTemp_Pub_WD$PercentError <- (Annual_Med_UpTemp_Pub_WD$Error/Annual_Med_UpTemp_Pub_WD$Model_Med_WD_Pub)*100

################################################################################################################################
#Step 25: Annual plots of published versus updated model output results for consumption and withdrawal
################################################################################################################################
pdf(file = paste0('output/Model_Validation//Annual_UpTemp_vs_Pub_Consump.pdf'), width = 10, height = 7)
Plant_AvgError <- round(mean(Annual_Med_UpTemp_Pub_Consump$Error),2)
Plant_MinError <- round(min(Annual_Med_UpTemp_Pub_Consump$Error),2)
Plant_MaxError <- round(max(Annual_Med_UpTemp_Pub_Consump$Error),2)
Plant_AvgPerError <- round(mean(Annual_Med_UpTemp_Pub_Consump$PercentError),2)
Plant_MinPerError <- round(min(Annual_Med_UpTemp_Pub_Consump$PercentError),2)
Plant_MaxPerError <- round(max(Annual_Med_UpTemp_Pub_Consump$PercentError),2)
p <- ggplot(Annual_Med_UpTemp_Pub_Consump) +
  geom_point(aes(x = Plant_id, y = Model_Med_Consump_UpTemp, colour = "Updated Model Output")) +
  geom_point(aes(x = Plant_id,  y = Model_Med_Consump_Pub, colour = "Published data" ))+
  theme_USGS() +  
  scale_colour_manual("", 
                      breaks = c("Updated Model Output", "Published data"),
                      values = c("Updated Model Output"="black", "Published data"="red")) +
  labs(x = 'Plant id', y = 'Consumption water use in Million Gallons per Day', 
       caption = paste0("Minimum Error = ",Plant_MinError, "        Average Error = ", Plant_AvgError, "         Maximum Error = ", Plant_MaxError,
                        "\nMinimum % Error = ",Plant_MinPerError, "     Average % Error = ", Plant_AvgPerError, "     Maximum % Error = ", Plant_MaxPerError) ,
       title = "Annual median updated consumption model output & published results")
print(p)
dev.off()

pdf(file = paste0('output/Model_Validation//Annual_UpTemp_vs_Pub_WD.pdf'), width = 10, height = 7)
Plant_AvgError <- round(mean(Annual_Med_UpTemp_Pub_WD$Error),2)
Plant_MinError <- round(min(Annual_Med_UpTemp_Pub_WD$Error),2)
Plant_MaxError <- round(max(Annual_Med_UpTemp_Pub_WD$Error),2)
Plant_AvgPerError <- round(mean(Annual_Med_UpTemp_Pub_WD$PercentError),2)
Plant_MinPerError <- round(min(Annual_Med_UpTemp_Pub_WD$PercentError),2)
Plant_MaxPerError <- round(max(Annual_Med_UpTemp_Pub_WD$PercentError),2)
p <- ggplot(Annual_Med_UpTemp_Pub_WD) +
  geom_point(aes(x = Plant_id, y = Model_Med_WD_UpTemp, colour = "Updated Model Output")) +
  geom_point(aes(x = Plant_id,  y = Model_Med_WD_Pub, colour = "Published data" ))+
  theme_USGS() +  
  scale_colour_manual("", 
                      breaks = c("Updated Model Output", "Published data"),
                      values = c("Updated Model Output"="black", "Published data"="red")) +
  labs(x = 'Plant id', y = 'Withdrawal water use in Million Gallons per Day', 
       caption = paste0("Minimum Error = ",Plant_MinError, "        Average Error = ", Plant_AvgError, "         Maximum Error = ", Plant_MaxError,
                        "\nMinimum % Error = ",Plant_MinPerError, "     Average % Error = ", Plant_AvgPerError, "     Maximum % Error = ", Plant_MaxPerError) ,
       title = "Annual median updated withdrawal model output & published results")
print(p)
dev.off()

################################################################################################################################
#Step 26: Maps of percent error at plant locations (looking for spatial patterns) for published vs both model outputs
################################################################################################################################
Annual_Med_UpTemp_Orig_Consump$PercentError <- abs(Annual_Med_UpTemp_Orig_Consump$PercentError)
temp_dat <- filter(Annual_Med_UpTemp_Orig_Consump, PercentError > 5)
temp_dat <- left_join(temp_dat[,c(1,5)], Plant_Loc, by = c("Plant_id" = "EIA_PLANT_ID"))
pdf(file = paste0("output/Model_Validation//Uptemp_vs_Orig_PerError_Map.pdf"), width = 10, height = 6)
states <- map_data("state")
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  guides(fill=FALSE) +
  geom_point(data = temp_dat, aes(x = LONGITUDE, y = LATITUDE, colour = cut(PercentError, c(0.0, 10.0, 20.0, 200.0))), size = 2) +
  scale_color_manual(name = "Absolute Percent Error Ranges",
                     values = c("(0,10]" = "black",
                                "(10,20]" = "yellow",
                                "(20,200]" = "red"),
                     labels = c("5 - 10", "10 - 20", "> 20")) +
  labs(title = "Contiguous United States", x= "Longitude", y = "Latitude",
       subtitle = "TE plants with annual consumption percent error greater than 5% of updated vs. original input data.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))
dev.off()

Annual_Med_UpTemp_Pub_Consump$PercentError <- abs(Annual_Med_UpTemp_Pub_Consump$PercentError)
temp_dat <- filter(Annual_Med_UpTemp_Pub_Consump, PercentError > 5)
temp_dat <- left_join(temp_dat[,c(1,5)], Plant_Loc, by = c("Plant_id" = "EIA_PLANT_ID"))
pdf(file = paste0("output/Model_Validation//Uptemp_vs_Pub_PerError_Map.pdf"), width = 10, height = 6)
states <- map_data("state")
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  guides(fill=FALSE) +
  geom_point(data = temp_dat, aes(x = LONGITUDE, y = LATITUDE, colour = cut(PercentError, c(0.0, 10.0, 20.0, 210.0))), size = 2) +
  scale_color_manual(name = "Absolute Percent Error Ranges",
                     values = c("(0,10]" = "black",
                                "(10,20]" = "yellow",
                                "(20,210]" = "red"),
                     labels = c("5 - 10", "10 - 20", "> 20")) +
  labs(title = "Contiguous United States", x= "Longitude", y = "Latitude",
       subtitle = "TE plants with annual consumption percent error greater than 5% of updated input vs. published data.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

dev.off()

Annual_Med_Orig_Pub_Consump$PercentError <- abs(Annual_Med_Orig_Pub_Consump$PercentError)
temp_dat <- filter(Annual_Med_Orig_Pub_Consump, PercentError > 5)
temp_dat <- left_join(temp_dat[,c(1,5)], Plant_Loc, by = c("Plant_id" = "EIA_PLANT_ID"))
pdf(file = paste0("output/Model_Validation//Orig_vs_Pub_PerError_Map.pdf"), width = 10, height = 6)
states <- map_data("state")
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  guides(fill=FALSE) +
  geom_point(data = temp_dat, aes(x = LONGITUDE, y = LATITUDE, colour = cut(PercentError, c(0.0, 10.0, 20.0, 210.0))), size = 2) +
  scale_color_manual(name = "Absolute Percent Error Ranges",
                     values = c("(0,10]" = "black",
                                "(10,20]" = "yellow",
                                "(20,210]" = "red"),
                     labels = c( "> 20")) +
  labs(title = "Contiguous United States", x= "Longitude", y = "Latitude",
       subtitle = "TE plants with annual consumption percent error greater than 5% of original input vs. published data.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

dev.off()

