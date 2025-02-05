#This software is preliminary or provisional and is subject to revision. It is being provided to meet 
#the need for timely best science. The software has not received final approval by the U.S. Geological 
#Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the 
#functionality of the software and related material nor shall the fact of release constitute any such 
#warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall 
#be held liable for any damages resulting from the authorized or unauthorized use of the software.

##################################################
## Script purpose: Compile and format raw FEWSR and Tower output files into long format monthly and 
##                 annual min, median and max consumption and withdrawal with plant attributes.  
##                 Complex plants (plants with multiple cooling types at a cooling single unit)  will
##                 be processed as in [Diehl and others, 2013](https://doi.org/10.3133/sir20135188).  
##                 Annual values are a weighted mean based on service hours and monthly values.
## RequiredLibraries : importThermoEIA, EIAplantAssociation, condenserDuty, dplyr
##################################################

#run these lines first time only to install packages
# install.packages("remotes")
# remotes::install_local("./Rpackages/importThermoEIA_1.0.tar.gz")
# remotes::install_local("./Rpackages/EIAplantAssociation_1.0.tar.gz",upgrade="never")
# remotes::install_local("./Rpackages/condenserDuty_1.0.tar.gz",upgrade="never")

#these paths need changed for release based on Amy's clean room
FEWSRinputData_path<-"D:/UpdatedFewsrOutput/"
compileOutFEWSRpath<-"D:/UpdatedFewsrOutput/FEWSRoutputCompiled/"
TowerinputData_path<-"D:/towerOutput/"
#outputData_path<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledConsumpWD_dataRelease/"
outputData_path<-"D:/"
postProcessVars_path<-"D:/compiledmanualBogens_2008-2020/postProcessVars_2008-2020.csv"

#########DO NOT EDIT BELOW THIS LINE#########
#load required libraries
library(importThermoEIA)
library(EIAplantAssociation)
library(condenserDuty)
library(dplyr)

years<-seq(2008,2020,1)
months<-c("January","February","March","April","May",
          "June","July","August","September","October","November","December")

for (y in years){
  eia_year<-y
  print(y)
  

  
  #compile A-D FEWSR output
  compileFEWSRoutput(FEWSRinputData_path,compileOutFEWSRpath,eia_year)
  
  
  #read and format Tower and FEWSR results
  FEWSRtower.list<-readFormatTowerFEWSRresults(TowerinputData_path,FEWSRinputData_path=compileOutFEWSRpath,eia_year)
  list2env(FEWSRtower.list, .GlobalEnv)
  
  #loop through FEWSRtower.list
  for (r in 1:length(FEWSRtower.list)){
    #split PLant_id into components
    df<-splitPlant_id(FEWSRtower.list[[r]])
    df$YEAR<-rep(y,nrow(df))
    #process Complex plants
    df<-processComplexCUWD(df)
    #save as names(FEWSRtower.list)[r]
    str<-paste0(names(FEWSRtower.list)[r],"<-df")
    eval(parse(text=str))
    #annual per cooling
    annual_df<-calcAnnual(df %>% dplyr::select(-flag_minMax),searchStr=c("Med","Min","Max"),
                          groupCols=c("Plant.Code","cooling","YEAR"))
    str<-paste0(gsub("Monthly","Annual",names(FEWSRtower.list)[r]),"<-annual_df")
    eval(parse(text=str))
    
    dfTeam<-df %>% dplyr::select(-flag_minMax,-percentAllocation)
    dfTeam<-dfTeam %>% dplyr::mutate(Month=substr(months[Month],1,3))
    annual_dfTeam<-annual_df
    annual_dfTeam$Month<-rep("annual",nrow(annual_dfTeam))
    annual_dfTeam<-annual_dfTeam %>% dplyr::select(dplyr::all_of(names(dfTeam)))
    dfTeam<-rbind(dfTeam,annual_dfTeam)
    
    str<-paste0("dfTeam<-dfTeam %>% tidyr::pivot_wider(names_from = Month, id_cols=c('Plant.Code','cooling','YEAR'),
                                  values_from=c(",
                paste(names(dfTeam)[!names(dfTeam) %in% c("Plant.Code","cooling","YEAR","Month")],collapse = ","),
                "))")
    eval(parse(text=str))
    
    if (r==1){
      dfTeam_all<-dfTeam
    }else{
      dfTeam_all<-merge(dfTeam_all,dfTeam,by=c("Plant.Code","cooling","YEAR"),all=T)
    }
    
    
    #for output to DataViz
    #aggregate to Plant-level
    df<-df %>% dplyr::select(-cooling,-percentAllocation,-flag_minMax)
    df<-df %>% dplyr::group_by(Plant.Code,Month,YEAR) %>%
      dplyr::summarise(across(everything(),sum,na.rm=TRUE),.groups="drop")
    
    #annual per plant
    annual_df<-calcAnnual(df,searchStr=c("Med","Min","Max"),
                          groupCols=c("Plant.Code","YEAR"))
    
    #format plant-level for Team
    dfTeamPlant<-df
    dfTeamPlant<-dfTeamPlant %>% dplyr::mutate(Month=substr(months[Month],1,3))
    annual_dfTeamPlant<-annual_df
    annual_dfTeamPlant$Month<-rep("annual",nrow(annual_dfTeamPlant))
    annual_dfTeamPlant<-annual_dfTeamPlant %>% dplyr::select(dplyr::all_of(names(dfTeamPlant)))
    dfTeamPlant<-rbind(dfTeamPlant,annual_dfTeamPlant)

    str<-paste0("dfTeamPlant<-dfTeamPlant %>% tidyr::pivot_wider(names_from = Month, id_cols=c('Plant.Code','YEAR'),
                                  values_from=c(",
                paste(names(dfTeamPlant)[!names(dfTeamPlant) %in% c("Plant.Code","YEAR","Month")],collapse = ","),
                "))")
    eval(parse(text=str))
    
    if (r==1){
      dfTeamPlant_all<-dfTeamPlant
    }else{
      dfTeamPlant_all<-dplyr::inner_join(dfTeamPlant_all,dfTeamPlant,by=c("Plant.Code","YEAR"))
    }
    
    if(y==2008){
      str<-paste0("compile_",names(FEWSRtower.list)[r],"<-df")
      eval(parse(text=str))
      str<-paste0("compile_",gsub("Monthly","Annual",names(FEWSRtower.list)[r]),"<-annual_df")
      eval(parse(text=str))
    }else{
      str<-paste0("compile_",names(FEWSRtower.list)[r],"<-rbind(compile_",names(FEWSRtower.list)[r],",df)")
      eval(parse(text=str))
      str<-paste0("compile_",gsub("Monthly","Annual",names(FEWSRtower.list)[r]),
                  "<-rbind(compile_",gsub("Monthly","Annual",names(FEWSRtower.list)[r]),",annual_df)")
      eval(parse(text=str))
    }
    
    
  }# for r
}#year


#add in postProcessVars
monthData<-dplyr::inner_join(compile_Monthly_Consump_EST,compile_Monthly_WD_EST)
annualData<-dplyr::inner_join(compile_Annual_Consump_EST,compile_Annual_WD_EST)

#postProcessVars
ppVars<-read.csv(postProcessVars_path)
ppVars<-ppVars %>% dplyr::select(-percentAllocation)

ppVars<-replaceMultiples(ppVars,
                             groupCols=c("Plant.Code","YEAR"),
                             countVar="general_mover",
                             repStr="complex")

ppVars<-replaceMultiples(ppVars,
                         groupCols=c("Plant.Code","YEAR"),
                         countVar="cooling",
                         repStr="complex")



ppVars<-ppVars %>% dplyr::group_by(Plant.Code,
                                   Plant.Name,
                                   County,
                                   State,
                                   Name.of.Water.Source,
                                   cooling,
                                   Plant.level_dom_fuel,
                                   general_mover,
                                   YEAR) %>%
  dplyr::summarise(Net.Generation.Year.To.Date = sum(Net.Generation.Year.To.Date,na.rm=T))

monthData<-dplyr::left_join(monthData,ppVars %>% dplyr::select(-Net.Generation.Year.To.Date),by=c("Plant.Code","YEAR"))
annualData<-dplyr::left_join(annualData,ppVars,by=c("Plant.Code","YEAR"))



#add coolingType and ModelType
names(monthData)[names(monthData)=="cooling"]<-"ModelType"
names(annualData)[names(annualData)=="cooling"]<-"ModelType"
crossCoolMod<-data.frame(cool=c("river","lake","OS","pond","tower","complex"),
                         coolingType=c("Once-through fresh",
                               "Once-through fresh",
                               "Once-through saline",
                               "Recirculating pond",
                               "Recirculating Tower",
                               "Complex"))
monthData<-dplyr::inner_join(monthData,crossCoolMod,by=c("ModelType"="cool"))
annualData<-dplyr::inner_join(annualData,crossCoolMod,by=c("ModelType"="cool"))

monthData<-monthData %>% dplyr::select(Plant.Code,
                                       coolingType,
                                       ModelType,
                                Plant.level_dom_fuel,
                                general_mover,
                                Month,
                                YEAR,
                                dplyr::contains("Consump"),
                                dplyr::contains("WD"))

annualData<-annualData %>% dplyr::select(Plant.Code,
                                YEAR,
                                Plant.Name,
                                County,
                                State,
                                Name.of.Water.Source,
                                coolingType,
                                ModelType,
                                Plant.level_dom_fuel,
                                general_mover,
                                Net.Generation.Year.To.Date,
                                dplyr::contains("Consump"),
                                dplyr::contains("WD"))

names(monthData)[regexpr("Consump",names(monthData))>0]<-c("cu_mgd","cu_lower_mgd","cu_upper_mgd")
names(monthData)[regexpr("WD",names(monthData))>0]<-c("wd_mgd","wd_lower_mgd","wd_upper_mgd")
names(annualData)[regexpr("Consump",names(annualData))>0]<-c("cu_mgd","cu_lower_mgd","cu_upper_mgd")
names(annualData)[regexpr("WD",names(annualData))>0]<-c("wd_mgd","wd_lower_mgd","wd_upper_mgd")

write.csv(monthData,file=paste0(outputData_path,"monthlyConsumpWD.csv"),row.names=F)
write.csv(annualData,file=paste0(outputData_path,"annualConsumpWD.csv"),row.names=F)