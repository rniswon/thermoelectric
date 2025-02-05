#Defines the Function for pulling in Thermoelectric data from the EIA website
eia_webpull <- function(eia_year, dest){
  #Define variables and create destination
  dir.create(file.path(dest), recursive=TRUE)
  if (eia_year<2001){
    eia_url<-"https://www.eia.gov/electricity/data/eia860/eia860a/eia860a"
 
  }else if (eia_year<2021){
    eia_url <- "https://www.eia.gov/electricity/data/eia860/archive/xls/eia860"
  }else{
    eia_url <- "https://www.eia.gov/electricity/data/eia860/xls/eia860"
  }

  eia_file <- paste(eia_url, eia_year, ".zip", sep="")
  
  #Download data file
  options(timeout=3600)
  download.file(eia_file, paste(dest,"/eia860", eia_year, ".zip", sep=""))
  
  unzip(
    paste(dest,"/eia860", eia_year, ".zip", sep=""),
    exdir = paste(dest,"/",eia_year, sep="")
  )

  #get 767 data
  if (eia_year<2007){
    eia_url<-"https://www.eia.gov/electricity/data/eia767/zip/f767_"
  
  eia_file<-paste(eia_url, eia_year, ".zip", sep="")
  #Download data file
  download.file(eia_file, paste(dest,"/eia767", eia_year, ".zip", sep=""))
  unzip(
    paste(dest,"/eia767", eia_year, ".zip", sep=""),
    exdir = paste(dest,"/",eia_year, sep="")
  )
  }
  

if (eia_year>=2008){
  eia_url <- "https://www.eia.gov/electricity/data/eia923/archive/xls/f923"
}else if (eia_year<2008 & eia_year>=2001){
  eia_url <- "https://www.eia.gov/electricity/data/eia923/archive/xls/f906920" 
}else{
  eia_url<-"https://www.eia.gov/electricity/data/eia923/archive/xls/utility/f7592000mu.xls"
  #eia_url <-"https://www.eia.gov/electricity/data/eia923/archive/xls/f906nonutil"
}
if (eia_year!=2000){
  eia_file <- paste(eia_url, "_" ,eia_year, ".zip", sep="") 
}else{
 # eia_file <- paste(eia_url, eia_year, ".zip", sep="")   
  eia_file<-eia_url
}
  if (eia_year==2000){
#Download data file
download.file(eia_file, paste(dest,"/",eia_year,"/f7592000mu.xls", sep=""),mode="wb")
  }else{
download.file(eia_file, paste(dest,"/eia923", eia_year, ".zip", sep=""))
}

  if (eia_year!=2000){
unzip(
paste(dest,"/eia923", eia_year, ".zip", sep=""),
exdir = paste(dest,"/",eia_year, sep="")
)
}

#unpack recursive directories
if(eia_year<2007){
 extraDir<-list.dirs(paste(dest,"/",eia_year, sep=""),recursive = F) 
 files<-list.files(extraDir,full.names = T)
 file.copy(files,paste0(dest,"/",eia_year,"/",basename(files)),overwrite = T)
}

}
