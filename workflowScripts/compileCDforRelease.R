devtools::install_deps("./cd", upgrade = "never")
devtools::load_all("./cd",recompile = FALSE)
devtools::install_deps("./plant_association", upgrade = "never")
devtools::load_all("./plant_association",recompile = FALSE)
inputPath<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledmanualBogens_2008-2020test/"
outputData_path<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledConsumpWD_dataRelease/"
years<-seq(2008,2020,1)
months<-c("January","February","March","April","May",
          "June","July","August","September","October","November","December")

for (y in years){
  eia_year<-y
  print(y)
 
  #read CD results
  cd<-read.csv(paste0(inputPath,"CD_results",eia_year,".csv"))
  
  #melt data
  cd<-reshape2::melt(cd,id.vars=c("Plant.Code","percentAllocation","cooling"),
                     variable.name=c("Month"),value.name="condenser_duty")
  cd<-cd %>% dplyr::mutate(Month = gsub("CD_","",Month))
  cd<-cd %>% rowwise() %>% dplyr::mutate(Month = which(tolower(months)==Month))
  cd<-cd[order(cd$Plant.Code,cd$cooling, cd$percentAllocation,cd$Month),]
  
  #add year
  cd$YEAR<-rep(y,nrow(cd))
  
  #compile
  if (y==2008){
    compileCD<-cd
  }else{
    compileCD<-rbind(compileCD,cd)
  }
}

write.csv(compileCD,file = paste0(outputData_path,"compiledCDresults_2008-2020.csv"),row.names = F)