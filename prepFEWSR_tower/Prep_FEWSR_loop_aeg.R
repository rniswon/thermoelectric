#### based on original code in file named PrepTowerFEWSRinput_loop.R
#### purpose is to prepare input files for the fewsr model from the Condenser duty model outputs that have been joined to the environmental variables.
### modified Sept. 2022 by Melissa Lombard to run in a loop over files for each year
library(dplyr)

setwd("C:/Users/WU/Thermoelectric/") #top of repo, change as needed
repo<- getwd()

#path to fewsr_tower_input files
fewsrDataPath<-"C:/Users/galanter/DOI/GS-WMA-WBEEP - Thermoelectric/"
#outputpath for prepped fewsr files
outputData_path<-paste0(fewsrDataPath,"FEWSR_inputFilesPrepped")

#read in plant pond elevation info to join to CD data
plant_pond_info <- read.csv(paste0(repo,"/cd/Data/plant_elev_pond_info_aeg.csv"), header = T, stringsAsFactors = F)
plant_pond_info<-plant_pond_info[!duplicated(plant_pond_info),]

### the fewsr prep function, selects by cooling type, joins to pond info, and changes column names
fewsr_prep <- function(dfout){
  dfout<-dfout %>% filter(!is.na(Plant.Code))
  #reformat Plant.Code to concat PLant.Code^cooling^percentAllocation
  dfout$Plant.Code<-paste(dfout$Plant.Code,dfout$cooling,dfout$percentAllocation,sep="^")
  #get columns for FEWSR
  dfout <- dfout %>% select(Plant.Code, elevation, pond_area,
                            contains("CD_"), contains("DB_"), contains("WB_"),
                            contains("WT_"), contains("WS_"))
  #double check no columns without first three letters of month (i.e WT_multsrc)
  dfoutData<-dfout %>% select(contains("CD_"), contains("DB_"), contains("WB_"),
                              contains("WT_"), contains("WS_"))
  dfoutData<-dfoutData %>% select(matches(substr(tolower(months),1,3)))
  #split non data columns
  dfout<-dfout %>% select(Plant.Code,elevation,pond_area)
  #put it back together
  dfout<-cbind(dfout,dfoutData)
  #organize in correct order
  dfout <- dfout %>% select(Plant.Code, elevation, pond_area,
                            contains("CD_"), contains("DB_"), contains("WB_"),
                            contains("WT_"), contains("WS_"))
  #remove NA rows
  dfout<-dfout %>% filter(!is.na(Plant.Code))
  names(dfout)[1]<-"Plant ID"
  return(dfout)
}

writeCSVheader<-function(dfout,type,outPath){
      line1<-t(matrix(c("User description of input file for big lakes",rep(NA,4),
                    "Note: The minimum heat loading and input wind coefficient values should be on Row 4")))
      line2<-t(matrix(c(NA,"Input wind function coefficients",rep(NA,3),
                      "Note: The input data (Plant ID, Elevation, etc.) should  begin on Row 7")))
      line3<-t(matrix(c("Minimum heat loading","a","b","c")))
      line5<-t(matrix(c("Plant Characteristics",NA,NA,"Added heat load (MMBtu)",rep(NA,11),
                    "Dry bulb air temperature Ta (oC)",rep(NA,11),
                    "Wet bulb air temperature Twb (oC)",rep(NA,11),
                    "Natural water temperature  T (oC)",rep(NA,11),
                    "Wind speed at 2m W (mph)")))
      if (type=="lake" | type=="OS"){
        line4<-t(matrix(c(0.1,1.04,1.05,0,"lake","Webster and Sherman, 1995")))
      }else if (type=="river"){
        line4<-t(matrix(c(0.2,2.96,0.64,0,"stream","Gulliver and Stefan, 1986")))
      }else{#pond
        line4<-t(matrix(c(0.1,2.465993817,0,0.123395469,"cooling ponds","Brady and others, 1969")))
      }

    write.table(line1,outPath,na="",quote=F,col.names = F,row.names = F,sep=",")
    write.table(line2,outPath,na="",quote=F,col.names = F,row.names = F,sep=",",append=T)
    write.table(line3,outPath,na="",quote=F,col.names = F,row.names = F,sep=",",append=T)
    write.table(line4,outPath,na="",quote=T,col.names = F,row.names = F,sep=",",append=T)
    write.table(line5,outPath,na="",quote=T,col.names = F,row.names = F,sep=",",append=T)
    write.table(dfout,outPath,na="",col.names=T,row.names=F,sep=",",append=T)

}


#set it up to loop the fewsr function over a file for each year and save a file for each year
years <- (2008:2020)
#i = 8 test for year 2015
months<-c("January","February","March","April","May",
          "June","July","August","September","October","November","December")

#loop through years
for (i in 1:length(years)) {
  year.i <- years[i]
  #read in the condenser duty data, run the fewsr_prep function, and save to files for each year
  CD_data.i <- read.csv(paste(fewsrDataPath,"Water_Temps/",years[i],"_fewsr_tower_input_final.csv", sep =""), header = T, stringsAsFactors = F)

  #set filter/naming variables
  fileNames<-c("Big_Lake","Pond","River","Saline")
  types<-c("lake","pond","river","OS")

  #loop through types
  for (t in types){
    #subset by cooling
    eval(parse(text = paste0("df<-CD_data.i %>% filter(cooling == '",t,"')")))
    #add pond_info
    df <- left_join(df, plant_pond_info, by = "Plant.Code")

    #find multiple rows
    dfMulti<-df %>% group_by(Plant.Code,cooling) %>% summarise(countRows = length(percentAllocation))
    dfMulti<-dfMulti %>% filter(countRows>1)
    dfsingle<-anti_join(df,dfMulti, by=c("Plant.Code","cooling"))
    dfMulti<-inner_join(df,dfMulti,  by=c("Plant.Code","cooling"))

    #create dataframes for each row
    for (f in 1:max(dfMulti$countRows)){
      eval(parse(text=paste0("cd_",t,".",f,"<-dfMulti[0,]")))
    }

    #loop through plants
    for (p in unique(dfMulti$Plant.Code)){
      dfsub<-dfMulti %>% filter(Plant.Code==p) %>% select(-countRows)
      #loop through cooling
      for (c in unique(dfsub$cooling)){
         dfsubC<-dfsub %>% filter(cooling==c)
         #loop through max rows
         for (r in 1:max(dfMulti$countRows)){
           dfsubR<-dfsubC[r,]
           if (nrow(dfsubR)!=0){
             #bind to type specific object name
             eval(parse(text=paste0("cd_",t,".",r,"<-rbind(cd_",t,".",r,",dfsubR)")))
           }#if nrow!=0
           }#for r
         }#for c
       }#for p

    #loop thorugh max rows
    for (f in 2:(max(dfMulti$countRows)+1)){
      eval(parse(text=paste0("dfout<-cd_",t,".",f-1)))

      #subset and order columns
      dfout <- fewsr_prep(dfout)

      #write to csv
      outPath<-paste0(outputData_path,years[i],"_FEWS_",fileNames[which(types==t)],"_plants_input_",toupper(letters[f]),
                      ".csv")
      writeCSVheader(dfout,t,outPath)
      # eval(parse(text=paste0("write.csv(dfout, file='",outputData_path,years[i],"_FEWS_",fileNames[which(types==t)],"_plants_input_",toupper(letters[f]),
      #                        ".csv', row.names = F)")))
    }#each file

    #for plants with single rows
    dfout<-dfsingle
    dfout <- fewsr_prep(dfout)
    outPath<-paste0(outputData_path,years[i],"_FEWS_",fileNames[which(types==t)],
                    "_plants_input_A.csv")
    writeCSVheader(dfout,t,outPath)
    # eval(parse(text=paste0("write.csv(dfout, file='",outputData_path,years[i],"_FEWS_",fileNames[which(types==t)],
    #                        "_plants_input_A.csv', row.names = F)")))

}#for type (pond,lake,river)






}
