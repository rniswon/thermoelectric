# install the needed pacakges if needed
# install.packages("assertthat")
# install.packages("data.table")
# install.packages("fastmatch")
# install.packages("fpCompare")
# install.packages("gWidgets2")
# install.packages("gWidgets2tcltk")
# install.packages("openxlsx")
# install.packages("rio")


# Call the libraries
library(here)
library(assertthat)
library(data.table)
library(fastmatch)
library(fpCompare)
library(gWidgets2)
library(gWidgets2tcltk)
library(ggplot2)
library(grDevices)
library(openxlsx)
library(readxl)
library(rio)
library(stringi)
library(tcltk)
library(utils)
library(stringr)
library(lubridate)
library(dplyr)

#Run fewsr function, line 121 of fewsr.R

#run for each Lake, River, Pond, downloaded these

# 10/12/22 run strategy: for all the files in the fewsrDataPath, run the model
fewsrDataPath<-"C:/Users/galanter/DOI/GS-WMA-WBEEP - Thermoelectric/FEWSR_inputFilesPrepped/"
fewsroutpath <- "C:/Users/galanter/DOI/GS-WMA-WBEEP - Thermoelectric/fewsr_output/"
#Lily's paths
#fewsrDataPath<-"D:/FEWSRinput/highWTremoved/updated_fewsr_tower_input/FEWSR_input/"
#fewsroutpath <- "D:/UpdatedFewsrOutput/"


# list of the files within the directory
input_files <-list.files(fewsrDataPath)
# dictionary to link part of the file with the type of model
model_d <- (c("Big_" = "lake", "Rive" = "river", "Pond" = "pond", "Sali" = "lake"))
# Loop through the files
# run the appropriate model (lake, river, pond, saline [lake])
# output the file as the same input file name but replace 'input' as 'output'
# what about the dialog box thing?

# file_names<-c("Big_Lake","Pond","River","Saline")
# types<-c("lake","pond","river","OS")
#for (i in 1:2){
for (i in 1:length(input_files)) {
#for (i in 1:2) {
  name  <- input_files[i]
  model <- as.character(model_d[substr(name, 11,14)]) # get the 11th through 14th char of the file name to determine the model type
  file <- paste0(fewsrDataPath, name)

  year<-as.numeric(substr(basename(file),1,4))

  #output file
  output_f <- sub('input', 'output', name)
  output_f <- as.character(str_sub(output_f, 1, -5))
  fewsr(file, sheet = 1, type = model, output = "xlsx",year)
}

# file <- paste0(fewsrDataPath, '2009_FEWS_Big_Lake_plants_input_A.csv')
# #file = here('targets_2015','input','fews','FEWS_BIG_Lake_plants_input_test_complex.csv')
# fewsr(file, sheet = 1, type ="lake", output = "xlsx")
#
file <- paste0(fewsrDataPath, '2008_FEWS_Pond_plants_input_C.csv')
fewsr(file, sheet = 1, type ="pond", output = "xlsx")
#
# file = paste0(fewsrDataPath, '2008_FEWS_River_plants_input_D.csv')
# fewsr(file, sheet = 1, type ="river", output = "xlsx")
#
# file <- paste0(fewsrDataPath, '2008_FEWS_Saline_plants_input_A.csv')
# #file = here('targets_2015','input','fews','FEWS_BIG_Lake_plants_input_test_complex.csv')
# fewsr(file, sheet = 1, type ="lake", output = "xlsx")
