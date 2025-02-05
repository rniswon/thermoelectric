#Fomatting of SNTemp data

#Remove "#" to download and install necessary R packages to run script (only need to do this once)
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages(smwrBase)

#Functions to call necessary packages

library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(smwrBase)


##BEFORE RUNNING SCRIPT###
#Step 1: Create a new folder where you want all files saved that are generated from this script. Copy the location
#of that folder and paste it to replace the path below to set the working directory.If path copies with
#backslash "\" replace all backslashes with a forward slash "/".
#Step 2: Download seg_tave_water_20201030.csv.gz from ftp://ftpint.cr.usgs.gov/pub/cr/co/denver/BRR-CR/pub/markstro/NHM/gfv1.1/
#Unzip the file and copy the csv to the file path you pasted above in a new folder called Data.
#Step 3: Download TE_Locations.gpkg file from https://www.sciencebase.gov/catalog/item/5dc2e57fe4b06957975230c2
#and bring into ArcGIS as a layer file (gpkg file acts as a geodatabase file). Export the attribute table to a csv file and save it to the Data file you created for the previous file.


#Setting work directory.
WorkingDirectory <- here("environmental", "data")
sntemp_dir <- "C:/Users/galanter/OneDrive - DOI/1-Projects/TEWU/"
setwd(WorkingDirectory)


#Importing necessary files
#SNTemp file will take several minutes to read into R
SNTemp_Raw <- read.csv(paste0(sntemp_dir, "seg_tave_water_070722.csv"), header = TRUE, stringsAsFactors = FALSE)
Plant_StreamSeg <-read.csv("all_plants_all_years_1498.csv", header = TRUE, stringsAsFactors = FALSE)
Plant_StreamSeg <- filter(Plant_StreamSeg,!(is.na(Plant_StreamSeg$v11seg_id)))
#There are 1007 EIA plants that have an assigned stream segment.
#There are 859 unique stream segment numbers assigned to a plant id.

#Creating empty dataframe to append data from loop
SNTemp <- data.frame()

#Formating SNtemp data
SNTemp_Raw$Date <- as.Date(SNTemp_Raw$Date)
#filtering SNTemp for 2015, delete if you want to work with entire dataset
SNTemp_Raw <- filter(SNTemp_Raw, year(Date) == 2015)
#Creating list and limiting it stream segments with linked EIA plants for loop

#Segment_No <- Plant_StreamSeg$v11seg_id[!is.na(Plant_StreamSeg$v11seg_id)]
#Segment_No <- Segment_No[lengths(Segment_No)!="NA"]
#Segment_No <- Segment_No %>% discard(is.null)
Segment_No <- paste0("X", Plant_StreamSeg$v11seg_id)
Segment_No <- unique(Segment_No)
#Segment_No <-as.numeric(Segment_No)
#Loop for creating stacked dataframe
for (Seg_i in Segment_No) {
  df <- data.frame(SNTemp_Raw$Date, as.numeric(str_extract_all(Seg_i, "[0-9]+")[[1]]), SNTemp_Raw[,Seg_i])
  colnames(df) <- c("Date", "segment_no", "SNTemp_C")
  SNTemp <- rbind(SNTemp, df)
}

#make it a monthly df
SNTemp$month <- month(SNTemp$Date)
#SNTemp$mean_month <- mean(SNTemp$SNTemp_C)
monthly<- SNTemp %>% group_by_at(vars(segment_no,`month`)) %>%
  mutate (wt_avg_c = mean(SNTemp_C, na.rm = T))

monthly_df <- monthly %>% distinct(wt_avg_c, .keep_all = TRUE)
monthly_df <- select(monthly_df,-SNTemp_C)
monthly_df$Date <- as.yearmon(monthly_df$Date, "%m/%Y")
#Matching EIA plant id's to SNTemp estimates by stream segment number
#Checked EIA_plants for duplicates and found none so did not need to add state to make id's unique.
#Removed plant ids that did not have an assigned stream segment number.
Plant_StreamSeg <- filter(Plant_StreamSeg, !is.na(v11segID))
EIAplants_SNTemp <- select(Plant_StreamSeg, EIA_PLANT_, PLANT_NAME, STATE, v11segID)
EIAplants_SNTemp <- left_join(SNTemp, EIAplants_SNTemp, by = c("Segment_no" = "v11segID"))
EIAplants_SNTemp <- EIAplants_SNTemp[,c(4,5,6,2,1,3)]
EIAplants_SNTemp <- rename(EIAplants_SNTemp,"EIA_plant_id"="EIA_PLANT_")

#Exporting File
exportCSV(SNTemp, file.name = "Data/SNTemp_KG_plant_matches.csv")
exportCSV(EIAplants_SNTemp, file.name = "Data/EIAplants_SNTemp_Daily.csv")
