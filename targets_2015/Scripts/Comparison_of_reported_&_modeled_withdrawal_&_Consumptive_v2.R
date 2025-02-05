#Comparison of EIA reported water withdrawal & consumption vs. physical model predictions

#Remove "#" to download and install necessary R packages to run script (only need to do this once)
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("smwrBase")
#install.packages("hydroGOF")
#install.packages("tidyverse")

#Functions to call necessary packages
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readxl)
library(Metrics)
library(smwrBase)
library(hydroGOF) #for "pbias" function
library(purrr) #for "reduce" function

##################################################################################################
#Step 1: Manual entry of model Repository
##################################################################################################
##BEFORE RUNNING SCRIPT###
#Copy and paste location of downloaded model Repository folder "targets_2015" and paste it
#to replace the path below to set the working directory.If path copies with backslash "\"
#replace all backslashes with a forward slash "/".
#Setting work directory.
WorkingDirectory <- "C:/Users/kvalseth/Documents/Thermoelectric/targets_2015/"
setwd(WorkingDirectory)
#Create and set loctation for results if it is not already generated
dir.create(path ="output/ModelResults_EIA" )
#Enter year to compare model and EIA data below:
Comp_Year <- 2015

#########################################################################################################
#Step 2: Import tower model results (will need to change location for new model results)
#########################################################################################################
#consumption_out <-read.csv("Model_archive/tower/Output/Tower_model_consumption_out_UpTemps_WithComplex.csv", stringsAsFactors = FALSE)
consumption_out <-read.csv("Model_archive/tower/Output/Tower_model_consumption_out_Orig_NoComplex.csv", stringsAsFactors = FALSE)

#########################################################################################################
#Step 3: Import and compile fews model outputs (will need to change location for new model results) 
#########################################################################################################
#fews model script acts as function and ouputs results automatically without creating R object.
lake_med_consump <- read_xlsx("output/fews/lakes_11_4_21.xlsx", sheet = "Best Consumption Estimates")
lake_med_WD <- read_xlsx("output/fews/lakes_11_4_21.xlsx", sheet = "Best Withdrawal Estimates")
pond_med_consump <- read_xlsx("output/fews/pond_11_4_21.xlsx", sheet = "Best Consumption Estimates")
pond_med_WD <- read_xlsx("output/fews/pond_11_4_21.xlsx", sheet = "Best Withdrawal Estimates")
river_med_consump <- read_xlsx("output/fews/river_11_4_21.xlsx", sheet = "Best Consumption Estimates")
river_med_WD <- read_xlsx("output/fews/river_11_4_21.xlsx", sheet = "Best Withdrawal Estimates")
Fews_Med_Consump <- bind_rows(lake_med_consump, pond_med_consump, river_med_consump)
Fews_Med_WD <- bind_rows(lake_med_WD, pond_med_WD, river_med_WD)

lake_Min_consump <- read_xlsx("output/fews/lakes_11_4_21.xlsx", sheet = "Min Consumpt with 22% cushion")
lake_Min_WD <- read_xlsx("output/fews/lakes_11_4_21.xlsx", sheet = "Estimated Min MGD Withdrawal")
pond_Min_consump <- read_xlsx("output/fews/pond_11_4_21.xlsx", sheet = "Min Consumpt with 22% cushion")
pond_Min_WD <- read_xlsx("output/fews/pond_11_4_21.xlsx", sheet = "Estimated Min MGD Withdrawal")
river_Min_consump <- read_xlsx("output/fews/river_11_4_21.xlsx", sheet = "Min Consumpt with 22% cushion")
river_Min_WD <- read_xlsx("output/fews/river_11_4_21.xlsx", sheet = "Estimated Min MGD Withdrawal")
Fews_Min_Consump <- bind_rows(lake_Min_consump, pond_Min_consump, river_Min_consump)
Fews_Min_WD <- bind_rows(lake_Min_WD, pond_Min_WD, river_Min_WD)

lake_Max_consump <- read_xlsx("output/fews/lakes_11_4_21.xlsx", sheet = "Max Consumpt with 22% cushion")
lake_Max_WD <- read_xlsx("output/fews/lakes_11_4_21.xlsx", sheet = "Estimated Max MGD Withdrawal")
pond_Max_consump <- read_xlsx("output/fews/pond_11_4_21.xlsx", sheet = "Max Consumpt with 22% cushion")
pond_Max_WD <- read_xlsx("output/fews/pond_11_4_21.xlsx", sheet = "Estimated Max MGD Withdrawal")
river_Max_consump <- read_xlsx("output/fews/river_11_4_21.xlsx", sheet = "Max Consumpt with 22% cushion")
river_Max_WD <- read_xlsx("output/fews/river_11_4_21.xlsx", sheet = "Estimated Max MGD Withdrawal")
Fews_Max_Consump <- bind_rows(lake_Max_consump, pond_Max_consump, river_Max_consump)
Fews_Max_WD <- bind_rows(lake_Max_WD, pond_Max_WD, river_Max_WD)

################################################################################################################################
#Step 4: Compile and format tower and fews model results (both consumption and withdrawal) into monthly and annual datasets
################################################################################################################################
Med_Mod_Consump <- as.data.frame(consumption_out[-1,c(1,15:26)])
colnames(Med_Mod_Consump) <- c("Plant_id", 1:12)
Med_Mod_Consump[,2:13] <- lapply(Med_Mod_Consump[,2:13], as.numeric)
colnames(Fews_Med_Consump) <- c("Plant_id", 1:12)
Med_Mod_Consump_UpTemp <- bind_rows(Med_Mod_Consump, Fews_Med_Consump)
Monthly_Med_Mod_Consump_UpTemp <- pivot_longer(Med_Mod_Consump_UpTemp, cols = 2:13, names_to = 'Month', values_to = 'Model_Med_Consump')
Monthly_Med_Mod_Consump_UpTemp$Month <- as.numeric(Monthly_Med_Mod_Consump_UpTemp$Month)
Monthly_Med_Mod_Consump_UpTemp <- Monthly_Med_Mod_Consump_UpTemp %>% group_by(Plant_id, Month) %>% summarise(Model_Med_Consump = sum(Model_Med_Consump))
Annual_Med_Mod_Consump_UpTemp <- Monthly_Med_Mod_Consump_UpTemp %>% group_by(Plant_id) %>% summarise(Model_Med_Consump = mean(Model_Med_Consump))

Min_Mod_Consump <- as.data.frame(consumption_out[-1,c(1:13)])
colnames(Min_Mod_Consump) <- c("Plant_id", 1:12)
Min_Mod_Consump[,2:13] <- lapply(Min_Mod_Consump[,2:13], as.numeric)
colnames(Fews_Min_Consump) <- c("Plant_id", 1:12)
Min_Mod_Consump_UpTemp <- bind_rows(Min_Mod_Consump, Fews_Min_Consump)
Monthly_Min_Mod_Consump_UpTemp <- pivot_longer(Min_Mod_Consump_UpTemp, cols = 2:13, names_to = 'Month', values_to = 'Model_Min_Consump')
Monthly_Min_Mod_Consump_UpTemp$Month <- as.numeric(Monthly_Min_Mod_Consump_UpTemp$Month)
Monthly_Min_Mod_Consump_UpTemp <- Monthly_Min_Mod_Consump_UpTemp %>% group_by(Plant_id, Month) %>% summarise(Model_Min_Consump = sum(Model_Min_Consump))
Annual_Min_Mod_Consump_UpTemp <- Monthly_Min_Mod_Consump_UpTemp %>% group_by(Plant_id) %>% summarise(Model_Min_Consump = mean(Model_Min_Consump))

Max_Mod_Consump <- as.data.frame(consumption_out[-1,c(1,28:39)])
colnames(Max_Mod_Consump) <- c("Plant_id", 1:12)
Max_Mod_Consump[,2:13] <- lapply(Max_Mod_Consump[,2:13], as.numeric)
colnames(Fews_Max_Consump) <- c("Plant_id", 1:12)
Max_Mod_Consump_UpTemp <- bind_rows(Max_Mod_Consump, Fews_Max_Consump)
Monthly_Max_Mod_Consump_UpTemp <- pivot_longer(Max_Mod_Consump_UpTemp, cols = 2:13, names_to = 'Month', values_to = 'Model_Max_Consump')
Monthly_Max_Mod_Consump_UpTemp$Month <- as.numeric(Monthly_Max_Mod_Consump_UpTemp$Month)
Monthly_Max_Mod_Consump_UpTemp <- Monthly_Max_Mod_Consump_UpTemp %>% group_by(Plant_id, Month) %>% summarise(Model_Max_Consump = sum(Model_Max_Consump))
Annual_Max_Mod_Consump_UpTemp <- Monthly_Max_Mod_Consump_UpTemp %>% group_by(Plant_id) %>% summarise(Model_Max_Consump = mean(Model_Max_Consump))

#Calculating modeled withdrawal by applying 1.4 factor
Med_Mod_WD <- pivot_longer(Med_Mod_Consump, cols = 2:13, names_to = 'Month', values_to = 'Model_Med_WD')
Med_Mod_WD$Model_Med_WD <-  Med_Mod_WD$Model_Med_WD*1.4
colnames(Fews_Med_WD) <- c("Plant_id", 1:12)
Fews_Med_WD <- pivot_longer(Fews_Med_WD, cols = 2:13, names_to = 'Month', values_to = 'Model_Med_WD')
Med_Mod_WD_UpTemp <- bind_rows(Med_Mod_WD, Fews_Med_WD)
Med_Mod_WD_UpTemp$Month <- as.numeric(Med_Mod_WD_UpTemp$Month)
Monthly_Med_Mod_WD_UpTemp <- Med_Mod_WD_UpTemp %>% group_by(Plant_id, Month) %>% summarise(Model_Med_WD = sum(Model_Med_WD))
Annual_Med_Mod_WD_UpTemp <- Monthly_Med_Mod_WD_UpTemp %>% group_by(Plant_id) %>% summarise(Model_Med_WD = mean(Model_Med_WD))

Min_Mod_WD <- pivot_longer(Min_Mod_Consump, cols = 2:13, names_to = 'Month', values_to = 'Model_Min_WD')
Min_Mod_WD$Model_Min_WD <-  Min_Mod_WD$Model_Min_WD*1.125
colnames(Fews_Min_WD) <- c("Plant_id", 1:12)
Fews_Min_WD <- pivot_longer(Fews_Min_WD, cols = 2:13, names_to = 'Month', values_to = 'Model_Min_WD')
Min_Mod_WD_UpTemp <- bind_rows(Min_Mod_WD, Fews_Min_WD)
Min_Mod_WD_UpTemp$Month <- as.numeric(Min_Mod_WD_UpTemp$Month)
Monthly_Min_Mod_WD_UpTemp <- Min_Mod_WD_UpTemp %>% group_by(Plant_id, Month) %>% summarise(Model_Min_WD = sum(Model_Min_WD))
Annual_Min_Mod_WD_UpTemp <- Monthly_Min_Mod_WD_UpTemp %>% group_by(Plant_id) %>% summarise(Model_Min_WD = mean(Model_Min_WD))

Max_Mod_WD <- pivot_longer(Max_Mod_Consump, cols = 2:13, names_to = 'Month', values_to = 'Model_Max_WD')
Max_Mod_WD$Model_Max_WD <-  Max_Mod_WD$Model_Max_WD*2
colnames(Fews_Max_WD) <- c("Plant_id", 1:12)
Fews_Max_WD <- pivot_longer(Fews_Max_WD, cols = 2:13, names_to = 'Month', values_to = 'Model_Max_WD')
Max_Mod_WD_UpTemp <- bind_rows(Max_Mod_WD, Fews_Max_WD)
Max_Mod_WD_UpTemp$Month <- as.numeric(Max_Mod_WD_UpTemp$Month)
Monthly_Max_Mod_WD_UpTemp <- Max_Mod_WD_UpTemp %>% group_by(Plant_id, Month) %>% summarise(Model_Max_WD = sum(Model_Max_WD))
Annual_Max_Mod_WD_UpTemp <- Monthly_Max_Mod_WD_UpTemp %>% group_by(Plant_id) %>% summarise(Model_Max_WD = mean(Model_Max_WD))

#Combining medium, minimum, and maximum values into one dataset so that above, below range values can be flagged
df_list <- list(Monthly_Med_Mod_Consump_UpTemp, Monthly_Min_Mod_Consump_UpTemp, Monthly_Max_Mod_Consump_UpTemp)
Monthly_Consump_EST <- df_list %>% reduce(full_join, by= c('Plant_id', "Month"))
df_list <- list(Monthly_Med_Mod_WD_UpTemp, Monthly_Min_Mod_WD_UpTemp, Monthly_Max_Mod_WD_UpTemp)
Monthly_WD_EST <- df_list %>% reduce(full_join, by= c('Plant_id', "Month"))
df_list <- list(Annual_Med_Mod_Consump_UpTemp, Annual_Min_Mod_Consump_UpTemp, Annual_Max_Mod_Consump_UpTemp)
Annual_Consump_EST <- df_list %>% reduce(full_join, by='Plant_id')
df_list <- list(Annual_Med_Mod_WD_UpTemp, Annual_Min_Mod_WD_UpTemp, Annual_Max_Mod_WD_UpTemp)
Annual_WD_EST <- df_list %>% reduce(full_join, by='Plant_id')

##############################################################################################################################
#Step 5: Import and format reported EIA data
##############################################################################################################################
#The EIA reported withdrawals and consumptive water use file is from: 
#https://teams.microsoft.com/_#/files/Water%20Use?threadId=19%3A332e5db8ab564769afe80baf3f49c416%40thread.skype&ctx=channel&context=EIA_data&rootfolder=%252Fsites%252FWBEEP%252FShared%2520Documents%252FWater%2520Use%2520Models%252FThermoelectric%252FEIA_data
Reported_raw <- read_xlsx("input/tower/EIA923_Schedule_8_Annual_Environmental_Information_2015_Final.xlsx", sheet = '8D Cooling System Information', skip = 4, guess_max = 18000)
Rep_WaterType <- read_xlsx("input/tower/cooling_detail_2015.xlsx",  skip = 2, guess_max = 100000)
Rep_WaterType <- unique(select(Rep_WaterType, "Plant Code", "Water Type", "860 Cooling Type 1"))
Rep_WaterType$`Water Type` <- ifelse(Rep_WaterType$`Water Type` == "Brackish", "Saline", Rep_WaterType$`Water Type`)
Rep_WaterType <- filter(Rep_WaterType, `Water Type` == "Saline" & str_sub(Rep_WaterType$`860 Cooling Type 1`, 6, 9) == "Once")
#Creating Cooling system dataset to symbolize by cooling system type on plots
Cool_Sys <- unique(select(Reported_raw, `Plant ID`, `Type of Cooling System`))
colnames(Cool_Sys) <- c("Plant_id", "CoolSystem")
for (i in Rep_WaterType$`Plant Code`) {
  Cool_Sys$CoolSystem <- ifelse(Cool_Sys$Plant_id == i & str_sub(Cool_Sys$CoolSystem,1,4) == "Once", paste0(Cool_Sys$CoolSystem, " -", " Saline"), Cool_Sys$CoolSystem)
}
Cool_Sys$Plant_id <- as.character(Cool_Sys$Plant_id)
Cool_Sys$CoolSystem[duplicated(Cool_Sys$Plant_id) | duplicated(Cool_Sys$Plant_id, fromLast = TRUE)] <- "Complex Cooling"
Cool_Sys <- unique(Cool_Sys)
Cool_Sys <- Cool_Sys %>% mutate(CoolSys_TP = case_when(CoolSystem == "Once through without cooling pond(s) or canal(s)" ~ "Once Through - Fresh", 
                                                       CoolSystem == "Recirculating with Induced Draft Cooling Tower" ~ "Recirculating Tower",
                                                       CoolSystem == "Recirculating with Natural Draft Cooling Tower" ~ "Recirculating Tower",
                                                       CoolSystem == "Recirculating with Cooling Ponds" ~ "Recirculating Pond",
                                                       CoolSystem == "Hybrid: recirculating with forced draft cooling tower(s) with dry cooling" ~ "Complex Cooling",
                                                       CoolSystem == "Once through with Cooling Ponds" ~ "Once Through  - Fresh",
                                                       CoolSystem == "Dry (air) cooling System" ~ "Dry Air cooling",
                                                       CoolSystem == "Recirculating with Forced Draft Cooling Tower" ~ "Recirculating Tower",
                                                       CoolSystem == "Hybrid: recirculating with induced draft cooling tower(s) with dry cooling" ~ "Complex Cooling",
                                                       CoolSystem == "Complex Cooling" ~ "Complex Cooling",
                                                       CoolSystem == "Once through without cooling pond(s) or canal(s) - Saline" ~ "Once Through - Saline",
                                                       CoolSystem == "Once through with Cooling Ponds - Saline " ~ "Once Through - Saline"))
Reported <- Reported_raw[, c(1:7,9:12,19:22)]
colnames(Reported) <- c("Year", "Month", "Plant_id", "Cooling_id", "Cooling_sys", "Status", "Service_hrs", "Diversion_GPM",
                         "Withdrawal_GPM","Discharge_GPM", "Consumption_GPM", "Diversion_MG","Withdrawal_MG", "Discharge_MG", "Consumptive_MG")
Reported[Reported == "."] <- NA
Reported <- filter(Reported, Year == Comp_Year)
Reported[,7:15] <- sapply(Reported[,7:15],as.numeric)
Reported$Plant_id <- as.character(Reported$Plant_id)
#Converting reported data units to match model result units
Reported$Calc_Withdrawal_MG <- ((Reported$Withdrawal_GPM*60)* Reported$Service_hrs)/1000000
Reported$Calc_Consump_MG <- ((Reported$Consumption_GPM*60)* Reported$Service_hrs)/1000000
Reported$Calc_Withdrawal_MGD <- (Reported$Calc_Withdrawal_MG/daysInMonth(Reported$Month, Reported$Year))
Reported$Calc_Consump_MGD <- (Reported$Calc_Consump_MG/daysInMonth(Reported$Month, Reported$Year))
MonthlyReported <- Reported[,c(1:3, 18, 19)]
#Aggregatating by Plant_id and Month as there are multiple cooling systems for some plants.
MonthlyReported_Consump <- MonthlyReported %>% group_by(Plant_id, Month) %>% summarise(Rep_Consump = sum(Calc_Consump_MGD))
MonthlyReported_Consump <- MonthlyReported_Consump[!is.na(MonthlyReported_Consump$Rep_Consump),]
AnnualReported_Consump <- MonthlyReported_Consump %>% group_by(Plant_id) %>% summarise(Rep_Consump = mean(Rep_Consump))
MonthlyReported_WD <- MonthlyReported %>% group_by(Plant_id, Month) %>% summarise(Rep_WD = sum(Calc_Withdrawal_MGD))
MonthlyReported_WD <- MonthlyReported_WD[!is.na(MonthlyReported_WD$Rep_WD),]
AnnualReported_WD <- MonthlyReported_WD %>% group_by(Plant_id) %>% summarise(Rep_WD = mean(Rep_WD))

#######################################################################################################################
#Step 6: Join reported and modeled values into monthly dataset for both consumption withdrawal datasets for plotting.
#######################################################################################################################
Monthly_ModUpTemp_Rep_Consump <- left_join(MonthlyReported_Consump, Monthly_Consump_EST, by =  c("Plant_id", "Month"))
Monthly_ModUpTemp_Rep_WD <- left_join(MonthlyReported_WD, Monthly_WD_EST, by =  c("Plant_id", "Month"))

########################################################################################################################
#Step 7: Calculate error (reported-modeled) and flag reported values above or below calculated ranges for monthly datasets
########################################################################################################################
Monthly_ModUpTemp_Rep_Consump$Error <- (Monthly_ModUpTemp_Rep_Consump$Rep_Consump - Monthly_ModUpTemp_Rep_Consump$Model_Med_Consump)
Monthly_ModUpTemp_Rep_Consump$PercentError <- (Monthly_ModUpTemp_Rep_Consump$Error/Monthly_ModUpTemp_Rep_Consump$Rep_Consump)*100
Monthly_ModUpTemp_Rep_Consump$InRange <- ifelse(Monthly_ModUpTemp_Rep_Consump$Rep_Consump > Monthly_ModUpTemp_Rep_Consump$Model_Min_Consump 
                                                & Monthly_ModUpTemp_Rep_Consump$Rep_Consump < Monthly_ModUpTemp_Rep_Consump$Model_Max_Consump, "Within estimated range", "Outside estimated range")
Monthly_ModUpTemp_Rep_Consump <- Monthly_ModUpTemp_Rep_Consump %>% mutate(AboveBelow = case_when(Rep_Consump < Model_Min_Consump ~ "Below Range",
                                                                                       Rep_Consump > Model_Max_Consump ~ "Above Range",
                                                                                       Rep_Consump >= Model_Min_Consump & Rep_Consump <= Model_Max_Consump ~ "In Range"))
Monthly_ModUpTemp_Rep_WD$Error <- (Monthly_ModUpTemp_Rep_WD$Rep_WD - Monthly_ModUpTemp_Rep_WD$Model_Med_WD)
Monthly_ModUpTemp_Rep_WD$PercentError <- (Monthly_ModUpTemp_Rep_WD$Error/Monthly_ModUpTemp_Rep_WD$Rep_WD)*100
Monthly_ModUpTemp_Rep_WD$InRange <- ifelse(Monthly_ModUpTemp_Rep_WD$Rep_WD > Monthly_ModUpTemp_Rep_WD$Model_Min_WD 
                                           & Monthly_ModUpTemp_Rep_WD$Rep_WD < Monthly_ModUpTemp_Rep_WD$Model_Max_WD, "Within estimated range", "Outside estimated range")
Monthly_ModUpTemp_Rep_WD <- Monthly_ModUpTemp_Rep_WD %>% mutate(AboveBelow = case_when(Rep_WD < Model_Min_WD ~ "Below Range",
                                                                                     Rep_WD > Model_Max_WD ~ "Above Range",
                                                                                     Rep_WD >= Model_Min_WD & Rep_WD <= Model_Max_WD ~ "In Range"))
Monthly_ModUptemp_Rep_Consump_5PerError <-filter(Monthly_ModUpTemp_Rep_Consump, abs(PercentError) > 5.0)
Monthly_ModUptemp_Rep_WD_5PerError <-filter(Monthly_ModUpTemp_Rep_WD, abs(PercentError) > 5.0)

Plants_ModUptemp_Rep_Consump_5PerError <- unique(Monthly_ModUptemp_Rep_Consump_5PerError$Plant_id)
write.csv(Monthly_ModUpTemp_Rep_Consump, "output/ModelResults_EIA/ModUptemp_Rep_Consump.csv")
write.csv(Monthly_ModUpTemp_Rep_WD, "output/ModelResults_EIA/ModUptemp_Rep_WD.csv")
write.csv(Monthly_ModUptemp_Rep_Consump_5PerError, "output/ModelResults_EIA/ModUptemp_Rep_Consump_1PerError.csv")

#Remove rows where modeled value is NA
Monthly_ModUpTemp_Rep_Consump <- Monthly_ModUpTemp_Rep_Consump[!is.na(Monthly_ModUpTemp_Rep_Consump$Model_Med_Consump),]
Monthly_ModUpTemp_Rep_WD <- Monthly_ModUpTemp_Rep_WD[!is.na(Monthly_ModUpTemp_Rep_WD$Model_Med_WD),]

#######################################################################################################################
#Step 8: Join reported and modeled values into annual dataset for both consumption withdrawal datasets for plotting.
#######################################################################################################################
Annual_ModUpTemp_Rep_Consump <- left_join(AnnualReported_Consump, Annual_Consump_EST, by = "Plant_id")
Annual_ModUpTemp_Rep_WD <- left_join(AnnualReported_WD, Annual_WD_EST, by = "Plant_id")

########################################################################################################################
#Step 9: Calculate error (reported-modeled) and flag reported values above or below calculated ranges for annual datasets
########################################################################################################################
Annual_ModUpTemp_Rep_Consump$Error <- (Annual_ModUpTemp_Rep_Consump$Rep_Consump - Annual_ModUpTemp_Rep_Consump$Model_Med_Consump)
Annual_ModUpTemp_Rep_Consump$PercentError <- (Annual_ModUpTemp_Rep_Consump$Error/Annual_ModUpTemp_Rep_Consump$Rep_Consump)*100
Annual_ModUpTemp_Rep_Consump$InRange <- ifelse(Annual_ModUpTemp_Rep_Consump$Rep_Consump > Annual_ModUpTemp_Rep_Consump$Model_Min_Consump 
                                               & Annual_ModUpTemp_Rep_Consump$Rep_Consump < Annual_ModUpTemp_Rep_Consump$Model_Max_Consump, "Within estimated range", "Outside estimated range")
Annual_ModUpTemp_Rep_Consump <- Annual_ModUpTemp_Rep_Consump %>% mutate(AboveBelow = case_when(Rep_Consump < Model_Min_Consump ~ "Below Range",
                                                                                     Rep_Consump > Model_Max_Consump ~ "Above Range",
                                                                                     Rep_Consump >= Model_Min_Consump & Rep_Consump <= Model_Max_Consump ~ "In Range"))
Annual_ModUpTemp_Rep_WD$Error <- (Annual_ModUpTemp_Rep_WD$Rep_WD - Annual_ModUpTemp_Rep_WD$Model_Med_WD)
Annual_ModUpTemp_Rep_WD$PercentError <- (Annual_ModUpTemp_Rep_WD$Error/Annual_ModUpTemp_Rep_WD$Rep_WD)*100
Annual_ModUpTemp_Rep_WD$InRange <- ifelse(Annual_ModUpTemp_Rep_WD$Rep_WD > Annual_ModUpTemp_Rep_WD$Model_Min_WD 
                                          & Annual_ModUpTemp_Rep_WD$Rep_WD < Annual_ModUpTemp_Rep_WD$Model_Max_WD, "Within estimated range", "Outside estimated range")
Annual_ModUpTemp_Rep_WD <- Annual_ModUpTemp_Rep_WD %>% mutate(AboveBelow = case_when(Rep_WD < Model_Min_WD ~ "Below Range",
                                                                                     Rep_WD > Model_Max_WD ~ "Above Range",
                                                                                     Rep_WD >= Model_Min_WD & Rep_WD <= Model_Max_WD ~ "In Range"))
Annual_ModUptemp_Rep_Consump_5PerError <-filter(Annual_ModUpTemp_Rep_Consump, abs(PercentError) > 5.0)
Annual_ModUptemp_Rep_WD_5PerError <-filter(Annual_ModUpTemp_Rep_WD, abs(PercentError) > 5.0)

Annual_ModUpTemp_Rep_Consump <- Annual_ModUpTemp_Rep_Consump[!is.na(Annual_ModUpTemp_Rep_Consump$Model_Med_Consump),]
Annual_ModUpTemp_Rep_WD <- Annual_ModUpTemp_Rep_WD[!is.na(Annual_ModUpTemp_Rep_WD$Model_Med_WD),]

Unmatched_Consump <- setdiff(AnnualReported_Consump$Plant_id, Annual_ModUpTemp_Rep_Consump$Plant_id)
Unmatched_WD <- setdiff(AnnualReported_WD$Plant_id, Annual_ModUpTemp_Rep_WD$Plant_id)

########################################################################################################################
#Step 10: Joining cooling system type dataset to monthly and annual withdrawal and consumption datasets
########################################################################################################################
Monthly_ModUpTemp_Rep_Consump_CS <- left_join(Monthly_ModUpTemp_Rep_Consump, Cool_Sys, by = "Plant_id")
Monthly_ModUpTemp_Rep_Consump_CS <- Monthly_ModUpTemp_Rep_Consump_CS[!is.na(Monthly_ModUpTemp_Rep_Consump_CS$CoolSys_TP),] 
Monthly_ModUpTemp_Rep_WD_CS <- left_join(Monthly_ModUpTemp_Rep_WD, Cool_Sys, by = "Plant_id")
Monthly_ModUpTemp_Rep_WD_CS <- Monthly_ModUpTemp_Rep_WD_CS[!is.na(Monthly_ModUpTemp_Rep_WD_CS$CoolSys_TP),]
Annual_ModUpTemp_Rep_Consump_CS <- left_join(Annual_ModUpTemp_Rep_Consump, Cool_Sys, by = "Plant_id")
Annual_ModUpTemp_Rep_Consump_CS <- Annual_ModUpTemp_Rep_Consump_CS[!is.na(Annual_ModUpTemp_Rep_Consump_CS$CoolSys_TP),] 
Annual_ModUpTemp_Rep_WD_CS <- left_join(Annual_ModUpTemp_Rep_WD, Cool_Sys, by = "Plant_id")
Annual_ModUpTemp_Rep_WD_CS <- Annual_ModUpTemp_Rep_WD_CS[!is.na(Annual_ModUpTemp_Rep_WD_CS$CoolSys_TP),]


########################################################################################################################
#Step 11: Format settings for plots
########################################################################################################################
theme_USGS <-  function(base_size = 8){
  theme(
    plot.title = element_text (vjust = 3, hjust = 0.5, size = 14, family="serif"),
    plot.subtitle = element_text(hjust = 1, size = 12,family="serif"),
    plot.caption = element_text(hjust = c(0,1), size = 12,family="serif"),
    plot.margin = unit (c(5.5, 5, 5.5, 5), "lines"), 
    panel.border = element_rect (colour = "black", fill = F, size = 0.1),
    panel.grid.major = element_line (colour = "black", size = 0.1),
    panel.grid.minor = element_line (colour = "grey", size = 0.07),
    panel.background = element_rect (fill = "white"),
    legend.background = element_blank(),
    legend.justification= "center",
    legend.position = "bottom",
    legend.key = element_blank (),
    legend.title = element_blank(),
    legend.text = element_text (size = 11),
    axis.title.x = element_text (size = 11, family="serif"),
    axis.title.y = element_text (vjust = 1, angle = 90, size = 11, family="serif"),
    axis.text.x = element_text (size = 11, vjust = -0.25, colour = "black", 
                                family="serif", margin=margin(10,5,20,5,"pt")),
    axis.text.y = element_text (size = 11, hjust = 1, colour = "black", 
                                family="serif", margin=margin(5,10,10,5,"pt")),
    axis.ticks = element_line (colour = "black", size = 0.1),
    axis.ticks.length = unit(-0.25 , "cm"),
    axis.ticks.margin = unit(0.5, "cm"),
  )
}

########################################################################################################################
#Step 12: Function to generate minor breaks on log plots
########################################################################################################################
minor_breaks_log <- function(base) {
  # Prevents lazy evaluation
  force(base) 
  # Wrap calculation in a function that the outer function returns
  function(limits) {
    ggplot2:::calc_logticks(
      base = base, 
      minpow = floor(log(limits[1], base = base)), 
      maxpow = ceiling(log(limits[2], base = base))
    )$value
  }
}

########################################################################################################################
#Step 13: Set major plot breaks and labels for consumption and withdrawal
########################################################################################################################
Consump_breaks <- c(0.000001, 0.0001, 0.01, 1.0, 100, 10000)
Consump_labels <- function(Consump_breaks) ifelse(Consump_breaks == 0, "0", Consump_breaks)
WD_breaks <- c(0.000001, 0.0001, 0.01, 1.0, 100, 10000)
WD_labels <- function(WD_breaks) ifelse(WD_breaks == 0, "0", WD_breaks)

########################################################################################################################
#Step 14: Plot monthly  modeled vs reported consumption and withdrawal on log scale with cooling system type symbolized
########################################################################################################################
PerBias <- pbias(Monthly_ModUpTemp_Rep_Consump_CS$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_CS$Model_Med_Consump)
rsq <- round(cor(Monthly_ModUpTemp_Rep_Consump_CS$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_CS$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_CoolSys_vs_Rep_Consump_log.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_Consump_CS) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = CoolSys_TP, shape = CoolSys_TP)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_USGS() +  
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Reported vs Modeled Monthly Average Daily Consumption Water \nUse in Million Gallons per Day"))
print(p)
dev.off()


PerBias <- pbias(Monthly_ModUpTemp_Rep_WD_CS$Rep_WD, Monthly_ModUpTemp_Rep_WD_CS$Model_Med_WD)
rsq <- round(cor(Monthly_ModUpTemp_Rep_WD_CS$Rep_WD, Monthly_ModUpTemp_Rep_WD_CS$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_CoolSys_vs_Rep_WD_log.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_WD_CS) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = CoolSys_TP, shape = CoolSys_TP)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_USGS() +  
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Reported vs Modeled Monthly Average Daily Withdrawal Water \nUse in Million Gallons per Day"))
print(p)
dev.off()

########################################################################################################################
#Step 15: Plot monthly modeled vs reported consumption and withdrawal on log scale with above, below, or in range symbolized
#########################################################################################################################
PerBias <- pbias(Monthly_ModUpTemp_Rep_Consump$Rep_Consump, Monthly_ModUpTemp_Rep_Consump$Model_Med_Consump)
rsq <- round(cor(Monthly_ModUpTemp_Rep_Consump$Rep_Consump, Monthly_ModUpTemp_Rep_Consump$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_Consump_log.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_Consump) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Reported vs Modeled Monthly Average Daily Consumption Water \nUse in Million Gallons per Day"))
print(p)
dev.off()

PerBias <- pbias(Monthly_ModUpTemp_Rep_WD$Rep_WD, Monthly_ModUpTemp_Rep_WD$Model_Med_WD)
rsq <- round(cor(Monthly_ModUpTemp_Rep_WD$Rep_WD, Monthly_ModUpTemp_Rep_WD$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_WD_log.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_WD) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Reported vs Modeled Monthly Average Daily Withdrawal Water \nUse in Million Gallons per Day"))
print(p)
dev.off()
########################################################################################################################
#Step 16: Plot by cooling system type monthly modeled vs reported consumption on log scale with cooling system type symbolized
########################################################################################################################
Monthly_ModUpTemp_Rep_Consump_CC <- filter(Monthly_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Complex Cooling")
PerBias <- pbias(Monthly_ModUpTemp_Rep_Consump_CC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_CC$Model_Med_Consump)
rsq <- round(cor(Monthly_ModUpTemp_Rep_Consump_CC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_CC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_Consump_log_ComplexCool.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_Consump_CC) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Complex Reported vs Modeled Monthly Average Daily Consumption \nWater Use in Million Gallons per Day"))
print(p)
dev.off()

Monthly_ModUpTemp_Rep_Consump_TC <- filter(Monthly_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Recirculating Tower")
PerBias <- pbias(Monthly_ModUpTemp_Rep_Consump_TC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_TC$Model_Med_Consump)
rsq <- round(cor(Monthly_ModUpTemp_Rep_Consump_TC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_TC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_Consump_log_TowerCool.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_Consump_TC) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Tower Reported vs Modeled Monthly Average Daily Consumption \nWater Use in Million Gallons per Day"))
print(p)
dev.off()

Monthly_ModUpTemp_Rep_Consump_OTF <- filter(Monthly_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Once Through - Fresh")
PerBias <- pbias(Monthly_ModUpTemp_Rep_Consump_OTC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_OTC$Model_Med_Consump)
rsq <- round(cor(Monthly_ModUpTemp_Rep_Consump_OTC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_OTC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_Consump_log_OneThruCoolFre.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_Consump_OTF) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Once Through - Fresh Reported vs Modeled Monthly Average Daily \nConsumption Water Use in Million Gallons per Day"))
print(p)
dev.off()

Monthly_ModUpTemp_Rep_Consump_OTS <- filter(Monthly_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Once Through - Saline")
PerBias <- pbias(Monthly_ModUpTemp_Rep_Consump_OTC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_OTC$Model_Med_Consump)
rsq <- round(cor(Monthly_ModUpTemp_Rep_Consump_OTC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_OTC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_Consump_log_OneThruCoolSal.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_Consump_OTS) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Once Through - Saline Reported vs Modeled Monthly Average Daily \nConsumption Water Use in Million Gallons per Day"))
print(p)
dev.off()

Monthly_ModUpTemp_Rep_Consump_RPC <- filter(Monthly_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Recirculating Pond")
PerBias <- pbias(Monthly_ModUpTemp_Rep_Consump_RPC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_RPC$Model_Med_Consump)
rsq <- round(cor(Monthly_ModUpTemp_Rep_Consump_RPC$Rep_Consump, Monthly_ModUpTemp_Rep_Consump_RPC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_Consump_log_RecircPondCool.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_Consump_RPC) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Recirculating Pond Reported vs Modeled Monthly Average \nDaily Consumption Water Use in Million Gallons per Day"))
print(p)
dev.off()

########################################################################################################################
#Step 17: Plot by cooling system type monthly modeled vs reported withdrawal on log scale with cooling system type symbolized
########################################################################################################################
Monthly_ModUpTemp_Rep_WD_CC <- filter(Monthly_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Complex Cooling")
PerBias <- pbias(Monthly_ModUpTemp_Rep_WD_CC$Rep_WD, Monthly_ModUpTemp_Rep_WD_CC$Model_Med_WD)
rsq <- round(cor(Monthly_ModUpTemp_Rep_WD_CC$Rep_WD, Monthly_ModUpTemp_Rep_WD_CC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_WD_log_ComplexCool.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_WD_CC) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Complex Reported vs Modeled Monthly Average Daily Withdrawal \nWater Use in Million Gallons per Day"))
print(p)
dev.off()

Monthly_ModUpTemp_Rep_WD_TC <- filter(Monthly_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Recirculating Tower")
PerBias <- pbias(Monthly_ModUpTemp_Rep_WD_TC$Rep_WD, Monthly_ModUpTemp_Rep_WD_TC$Model_Med_WD)
rsq <- round(cor(Monthly_ModUpTemp_Rep_WD_TC$Rep_WD, Monthly_ModUpTemp_Rep_WD_TC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_WD_log_TowerCool.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_WD_TC) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Tower Reported vs Modeled Monthly Average Daily Withdrawal \nWater Use in Million Gallons per Day"))
print(p)
dev.off()

Monthly_ModUpTemp_Rep_WD_OTF <- filter(Monthly_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Once Through - Fresh")
PerBias <- pbias(Monthly_ModUpTemp_Rep_WD_OTC$Rep_WD, Monthly_ModUpTemp_Rep_WD_OTC$Model_Med_WD)
rsq <- round(cor(Monthly_ModUpTemp_Rep_WD_OTC$Rep_WD, Monthly_ModUpTemp_Rep_WD_OTC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_WD_log_OneThruCoolFre.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_WD_OTF) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Once Through - Fresh Reported vs Modeled Monthly Average Daily \nWithdrawal Water Use in Million Gallons per Day"))
print(p)
dev.off()

Monthly_ModUpTemp_Rep_WD_OTS <- filter(Monthly_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Once Through - Saline")
PerBias <- pbias(Monthly_ModUpTemp_Rep_WD_OTC$Rep_WD, Monthly_ModUpTemp_Rep_WD_OTC$Model_Med_WD)
rsq <- round(cor(Monthly_ModUpTemp_Rep_WD_OTC$Rep_WD, Monthly_ModUpTemp_Rep_WD_OTC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_WD_log_OneThruCoolSal.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_WD_OTS) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Once Through - Saline Reported vs Modeled Monthly Average Daily \nWithdrawal Water Use in Million Gallons per Day"))
print(p)
dev.off()


Monthly_ModUpTemp_Rep_WD_RPC <- filter(Monthly_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Recirculating Pond")
PerBias <- pbias(Monthly_ModUpTemp_Rep_WD_RPC$Rep_WD, Monthly_ModUpTemp_Rep_WD_RPC$Model_Med_WD)
rsq <- round(cor(Monthly_ModUpTemp_Rep_WD_RPC$Rep_WD, Monthly_ModUpTemp_Rep_WD_RPC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Monthly_Model_vs_Rep_WD_log_RecircPondCool.pdf'), width = 8, height = 8)
p <- ggplot(Monthly_ModUpTemp_Rep_WD_RPC) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Recirculating Pond Reported vs Modeled Monthly Average \nDaily Withdrawal Water Use in Million Gallons per Day"))
print(p)
dev.off()

#############################################################################################################
#Step 18: Plot annual modeled vs reported consumption and withdrawal on log scale with cooling system type symbolized
#############################################################################################################
PerBias <- pbias(Annual_ModUpTemp_Rep_Consump_CS$Rep_Consump, Annual_ModUpTemp_Rep_Consump_CS$Model_Med_Consump)
rsq <- round(cor(Annual_ModUpTemp_Rep_Consump_CS$Rep_Consump, Annual_ModUpTemp_Rep_Consump_CS$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_CoolSys_vs_Rep_Consump_log.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_Consump_CS) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = CoolSys_TP, shape = CoolSys_TP)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_USGS() +  
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Reported vs Modeled Annual Average Daily Consumption Water \nUse in Million Gallons per Day"))
print(p)
dev.off()

PerBias <- pbias(Annual_ModUpTemp_Rep_WD_CS$Rep_WD, Annual_ModUpTemp_Rep_WD_CS$Model_Med_WD)
rsq <- round(cor(Annual_ModUpTemp_Rep_WD_CS$Rep_WD, Annual_ModUpTemp_Rep_WD_CS$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_CoolSys_vs_Rep_WD_log.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_WD_CS) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = CoolSys_TP, shape = CoolSys_TP)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_USGS() +  
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Reported vs Modeled Annual Average Daily Withdrawal Water \nUse in Million Gallons per Day"))
print(p)
dev.off()

##########################################################################################################################
#Step 19: Plot annual modeled vs reported consumption and withdrawal on log scale with above, below, or in range symbolized
##########################################################################################################################
PerBias <- pbias(Annual_ModUpTemp_Rep_Consump$Rep_Consump, Annual_ModUpTemp_Rep_Consump$Model_Med_Consump)
rsq <- round(cor(Annual_ModUpTemp_Rep_Consump$Rep_Consump, Annual_ModUpTemp_Rep_Consump$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_Consump_log.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_Consump) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Reported vs Modeled Annual Average Daily Consumption Water \nUse in Million Gallons per Day"))
print(p)
dev.off()

PerBias <- pbias(Annual_ModUpTemp_Rep_WD$Rep_WD, Annual_ModUpTemp_Rep_WD$Model_Med_WD)
rsq <- round(cor(Annual_ModUpTemp_Rep_WD$Rep_WD, Annual_ModUpTemp_Rep_WD$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_WD_log.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_WD) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Reported vs Modeled Annual Average Daily Withdrawal Water \nUse in Million Gallons per Day"))
print(p)
dev.off()
##########################################################################################################################
#Step 20: Plot by cooling system type annual modeled vs reported consumption on log scale with above, below, or in range symbolized
##########################################################################################################################
Annual_ModUpTemp_Rep_Consump_CC <- filter(Annual_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Complex Cooling")
PerBias <- pbias(Annual_ModUpTemp_Rep_Consump_CC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_CC$Model_Med_Consump)
rsq <- round(cor(Annual_ModUpTemp_Rep_Consump_CC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_CC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_Consump_log_ComplexCool.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_Consump_CC) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Complex Reported vs Modeled Annual Average Daily Consumption \nWater Use in Million Gallons per Day"))
print(p)
dev.off()

Annual_ModUpTemp_Rep_Consump_TC <- filter(Annual_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Recirculating Tower")
PerBias <- pbias(Annual_ModUpTemp_Rep_Consump_TC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_TC$Model_Med_Consump)
rsq <- round(cor(Annual_ModUpTemp_Rep_Consump_TC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_TC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_Consump_log_TowerCool.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_Consump_TC) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Tower Reported vs Modeled Annual Average Daily Consumption \nWater Use in Million Gallons per Day"))
print(p)
dev.off()

Annual_ModUpTemp_Rep_Consump_OTF <- filter(Annual_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Once Through - Fresh")
PerBias <- pbias(Annual_ModUpTemp_Rep_Consump_OTC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_OTC$Model_Med_Consump)
rsq <- round(cor(Annual_ModUpTemp_Rep_Consump_OTC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_OTC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_Consump_log_OneThruCoolFre.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_Consump_OTF) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Once Through - Fresh Reported vs Modeled Annual Average Daily \nConsumption Water Use in Million Gallons per Day"))
print(p)
dev.off()

Annual_ModUpTemp_Rep_Consump_OTS <- filter(Annual_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Once Through - Saline")
PerBias <- pbias(Annual_ModUpTemp_Rep_Consump_OTC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_OTC$Model_Med_Consump)
rsq <- round(cor(Annual_ModUpTemp_Rep_Consump_OTC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_OTC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_Consump_log_OneThruCoolSal.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_Consump_OTS) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Once Through - Saline Reported vs Modeled Annual Average Daily \nConsumption Water Use in Million Gallons per Day"))
print(p)
dev.off()

Annual_ModUpTemp_Rep_Consump_RPC <- filter(Annual_ModUpTemp_Rep_Consump_CS, CoolSys_TP == "Recirculating Pond")
PerBias <- pbias(Annual_ModUpTemp_Rep_Consump_RPC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_RPC$Model_Med_Consump)
rsq <- round(cor(Annual_ModUpTemp_Rep_Consump_RPC$Rep_Consump, Annual_ModUpTemp_Rep_Consump_RPC$Model_Med_Consump)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_Consump_log_RecircPondCool.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_Consump_RPC) +
  geom_point(aes(x = Model_Med_Consump,  y = Rep_Consump, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = Consump_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = Consump_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Recirculating Pond Reported vs Modeled Annual Average \nDaily Consumption Water Use in Million Gallons per Day"))
print(p)
dev.off()

##########################################################################################################################
#Step 21: Plot annual modeled vs reported withdrawal on log scale with above, below, or in range symbolized
##########################################################################################################################
Annual_ModUpTemp_Rep_WD_CC <- filter(Annual_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Complex Cooling")
PerBias <- pbias(Annual_ModUpTemp_Rep_WD_CC$Rep_WD, Annual_ModUpTemp_Rep_WD_CC$Model_Med_WD)
rsq <- round(cor(Annual_ModUpTemp_Rep_WD_CC$Rep_WD, Annual_ModUpTemp_Rep_WD_CC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_WD_log_ComplexCool.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_WD_CC) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Complex Reported vs Modeled Annual Average Daily Withdrawal \nWater Use in Million Gallons per Day"))
print(p)
dev.off()

Annual_ModUpTemp_Rep_WD_TC <- filter(Annual_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Recirculating Tower")
PerBias <- pbias(Annual_ModUpTemp_Rep_WD_TC$Rep_WD, Annual_ModUpTemp_Rep_WD_TC$Model_Med_WD)
rsq <- round(cor(Annual_ModUpTemp_Rep_WD_TC$Rep_WD, Annual_ModUpTemp_Rep_WD_TC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_WD_log_TowerCool.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_WD_TC) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Tower Reported vs Modeled Annual Average Daily Withdrawal \nWater Use in Million Gallons per Day"))
print(p)
dev.off()

Annual_ModUpTemp_Rep_WD_OTF <- filter(Annual_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Once Through - Fresh")
PerBias <- pbias(Annual_ModUpTemp_Rep_WD_OTC$Rep_WD, Annual_ModUpTemp_Rep_WD_OTC$Model_Med_WD)
rsq <- round(cor(Annual_ModUpTemp_Rep_WD_OTC$Rep_WD, Annual_ModUpTemp_Rep_WD_OTC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_WD_log_OneThruCoolFre.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_WD_OTF) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Once Through - Fresh Reported vs Modeled Annual Average Daily \nWithdrawal Water Use in Million Gallons per Day"))
print(p)
dev.off()

Annual_ModUpTemp_Rep_WD_OTS <- filter(Annual_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Once Through - Saline")
PerBias <- pbias(Annual_ModUpTemp_Rep_WD_OTC$Rep_WD, Annual_ModUpTemp_Rep_WD_OTC$Model_Med_WD)
rsq <- round(cor(Annual_ModUpTemp_Rep_WD_OTC$Rep_WD, Annual_ModUpTemp_Rep_WD_OTC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_WD_log_OneThruCoolSal.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_WD_OTS) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Once Through - Saline Reported vs Modeled Annual Average Daily \nWithdrawal Water Use in Million Gallons per Day"))
print(p)
dev.off()

Annual_ModUpTemp_Rep_WD_RPC <- filter(Annual_ModUpTemp_Rep_WD_CS, CoolSys_TP == "Recirculating Pond")
PerBias <- pbias(Annual_ModUpTemp_Rep_WD_RPC$Rep_WD, Annual_ModUpTemp_Rep_WD_RPC$Model_Med_WD)
rsq <- round(cor(Annual_ModUpTemp_Rep_WD_RPC$Rep_WD, Annual_ModUpTemp_Rep_WD_RPC$Model_Med_WD)^2,3)
pdf(file = paste0('output/ModelResults_EIA/Annual_Model_vs_Rep_WD_log_RecircPondCool.pdf'), width = 8, height = 8)
p <- ggplot(Annual_ModUpTemp_Rep_WD_RPC) +
  geom_point(aes(x = Model_Med_WD, y = Rep_WD, color = AboveBelow, shape = AboveBelow)) +
  theme_USGS() +  
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+ 
  scale_y_continuous(trans = "log10", breaks = WD_breaks, minor_breaks = minor_breaks_log(10), limits = c(0.000001, 10000), labels = WD_labels, expand =c(0,0))+
  labs(x = 'Reported', y = 'Modeled', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
       title = paste(Comp_Year, " Recirculating Pond Reported vs Modeled Annual Average \nDaily Withdrawal Water Use in Million Gallons per Day"))
print(p)
dev.off()

########################################################################################################
#Plant locations in case you need it
#Plant_Loc <- read_xlsx("input/tower/2015_TE_Model_Estimates_lat.long_COMIDs.xlsx")
#Plant_Loc <- select(Plant_Loc, EIA_PLANT_ID, LATITUDE, LONGITUDE)
#Plant_Loc$EIA_PLANT_ID <- as.character(Plant_Loc$EIA_PLANT_ID)