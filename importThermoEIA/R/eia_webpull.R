#'@title eia_webpull
#'@description pull EIA thermoelectric plant data including Form EIA-923 (2008-2020), 
#'             Form EIA-860 (2000-2020), Form EIA-906/920 (2001-2007), 
#'             Form EIA-767 (2000-2005), and Form EIA-759 (2000 only)
#'@param eia_year year of data to pull from https://www.eia.gov/electricity
#'@param dest character string indicating directory where data should be saved. A
#'            subdirectory named for `eia_year` will be created within the directory 
#'            specified and will contain unzipped EIA data files.
#'@examples
#'#set arguments
#'dest<-tempdir()
#'eia_year<-2015
#'
#'#pull data from web
#'eia_webpull(eia_year,dest)
#'@export

#Defines the Function for pulling in Thermoelectric data from the EIA website
eia_webpull <- function(eia_year, dest){
  #Create destination directory
  if (!dir.exists(dest)){
   dir.create(file.path(dest), recursive=TRUE) 
  }
  
  #create year subfolder
  suppressWarnings(dir.create(paste0(dest,.Platform$file.sep,eia_year)))
  
  
  #pull Form EIA-860
  if (eia_year<2001){
    eia_url<-"https://www.eia.gov/electricity/data/eia860/eia860a/eia860a"
 
  }else if (eia_year<2022){
    eia_url <- "https://www.eia.gov/electricity/data/eia860/archive/xls/eia860"
  }else{
    eia_url <- "https://www.eia.gov/electricity/data/eia860/xls/eia860"
  }

  #save zip file from EIA
  eia_file <- paste(eia_url, eia_year, ".zip", sep="")
  
  #Download data file
  options(timeout=3600)
  utils::download.file(eia_file, paste(dest,.Platform$file.sep,"eia860", eia_year, ".zip", sep=""))
  
  #unzip EIA data
  utils::unzip(
    paste(dest,.Platform$file.sep,"eia860", eia_year, ".zip", sep=""),
    exdir = paste(dest,.Platform$file.sep,eia_year, sep="")
  )

  #get Form EIA-767 data
  if (eia_year<2006){
  eia_url<-"https://www.eia.gov/electricity/data/eia767/zip/f767_"
  #save zip file from EIA
  eia_file<-paste(eia_url, eia_year, ".zip", sep="")
  #Download data file
  utils::download.file(eia_file, paste(dest,.Platform$file.sep,"eia767", eia_year, ".zip", sep=""))
  #unzip EIA data
  utils::unzip(
    paste(dest,.Platform$file.sep,"eia767", eia_year, ".zip", sep=""),
    exdir = paste(dest,.Platform$file.sep,eia_year, sep="")
  )
  }
  
  #get Form EIA-923 and Form EIA-906/920 data
if (eia_year>=2008){
  eia_url <- "https://www.eia.gov/electricity/data/eia923/archive/xls/f923"
}else if (eia_year<2008 & eia_year>=2001){
  eia_url <- "https://www.eia.gov/electricity/data/eia923/archive/xls/f906920" 
}else{
  eia_url<-"https://www.eia.gov/electricity/data/eia923/archive/xls/utility/f7592000mu.xls"
}
if (eia_year!=2000){
  eia_file <- paste(eia_url, "_" ,eia_year, ".zip", sep="") 
}else{
  eia_file<-eia_url
}
  #Download data file
  if (eia_year==2000){
utils::download.file(eia_file, paste(dest,.Platform$file.sep,eia_year,.Platform$file.sep,"f7592000mu.xls", sep=""),mode="wb")
  }else{
utils::download.file(eia_file, paste(dest,.Platform$file.sep,"eia923", eia_year, ".zip", sep=""))
}

  #unzip EIA data
  if (eia_year!=2000){
utils::unzip(
paste(dest,.Platform$file.sep,"eia923", eia_year, ".zip", sep=""),
exdir = paste(dest,.Platform$file.sep,eia_year, sep="")
)
}

#unpack recursive directories
if(eia_year<2007){
 extraDir<-list.dirs(paste(dest,.Platform$file.sep,eia_year, sep=""),recursive = F) 
 files<-list.files(extraDir,full.names = T)
 file.copy(files,paste0(dest,.Platform$file.sep,eia_year,.Platform$file.sep,basename(files)),overwrite = T)
}

}
