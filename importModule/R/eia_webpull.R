#Defines the Function for pulling in Thermoelectric data from the EIA website
eia_webpull <- function(eia_year, dest){
  #Define variables and create destination
  dir.create(file.path(dest), recursive=TRUE)
  eia_url <- "https://www.eia.gov/electricity/data/eia860/archive/xls/eia860"
  eia_file <- paste(eia_url, eia_year, ".zip", sep="")
  
  #Download data file
  options(timeout=3600)
  download.file(eia_file, paste(dest,"/eia860", eia_year, ".zip", sep=""))
  
  # unzip(
  #   paste(dest,"/eia860", eia_year, ".zip", sep=""),
  #   exdir = paste(dest,"/", eia_year, sep="")
  #     )
  # 
  unzip(
    paste(dest,"/eia860", eia_year, ".zip", sep=""),
    exdir = paste(dest,"/",eia_year, sep="")
  )


if (eia_year>=2008){
  eia_url <- "https://www.eia.gov/electricity/data/eia923/archive/xls/f923"
}else if (eia_year<2008 & eia_year>=2001){
  eia_url <- "https://www.eia.gov/electricity/data/eia923/archive/xls/f906920" 
}else{
  eia_url <-"https://www.eia.gov/electricity/data/eia923/archive/xls/f906nonutil"
}
if (eia_year!=2000){
  eia_file <- paste(eia_url, "_" ,eia_year, ".zip", sep="") 
}else{
  eia_file <- paste(eia_url, eia_year, ".zip", sep="")   
}
#Download data file
download.file(eia_file, paste(dest,"/eia923", eia_year, ".zip", sep=""))

# unzip(
#   paste(dest,"/eia923", eia_year, ".zip", sep=""),
#   exdir = paste(dest,"/", eia_year, sep="")
# )

unzip(
paste(dest,"/eia923", eia_year, ".zip", sep=""),
exdir = paste(dest,"/",eia_year, sep="")
)


}
