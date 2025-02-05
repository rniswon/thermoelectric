#Defines the Function for pulling in Thermoelectric data from ScienceBase
SciBase_webpull <- function(SciBase_year, path_SciBase){
  #Define variables and create destination
  dir.create(file.path(path_SciBase), recursive=TRUE)
  SciBase_url <- "Path to SciBase"
  SciBase_file <- paste(Scibase_url, SciBase_year, ".zip", sep="")
  
  #Download data file
  download.file(SciBase_file, paste(path_SciBase,"/eia860", SciBase_year, ".zip", sep=""))
  
  #Unzip only EIA files needed for analysis. The files used in this analysis are 
  #"3_1_Generator_Y{year}.xlsx" and "6_1_EnviroAssoc_Y{year}.xlsx", with "2___Plant_Y{year}.xlsx" to get lat/long.
  SciBase_gen <- paste("3_1_Generator_Y", SciBase_year, ".xlsx", sep="")
  SciBase_enviro <- paste("6_1_EnviroAssoc_Y", SciBase_year, ".xlsx", sep="")
  SciBase_plant <- paste("2___Plant_Y", SciBase_year, ".xlsx", sep="")
  SciBase_list <- c(SciBase_plant, SciBase_gen, SciBase_enviro)
  unzip(
    paste(path_SciBase,"/eia860", SciBase_year, ".zip", sep=""),
    files = SciBase_list,
    exdir = paste("tempdata/", SciBase_year, sep="")
  )
}