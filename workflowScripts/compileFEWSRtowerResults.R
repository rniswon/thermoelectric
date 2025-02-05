devtools::install_deps("./cd", upgrade = "never")
devtools::load_all("./cd",recompile = FALSE)
devtools::install_deps("./plant_association", upgrade = "never")
devtools::load_all("./plant_association",recompile = FALSE)
FEWSRinputData_path<-"D:/FEWSRoutput/"
compileOutFEWSRpath<-"D:/FEWSRoutput/FEWSRoutputCompiled/"
TowerinputData_path<-"./tower/Output/"
outputData_path<-"D:/compiledConsumpWD/updated/"
postProcessVars_path<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledmanualBogens_2008-2020/postProcessVars_2008-2020.csv"
years<-seq(2008,2020,1)
months<-c("January","February","March","April","May",
                        "June","July","August","September","October","November","December")

for (y in years){
  eia_year<-y
  print(y)
  
  #postProcessVars
  ppVars<-read.csv(postProcessVars_path)
  ppVars<-ppVars %>% filter(YEAR==y)
  ppVars<-ppVars %>% select(-percentAllocation)
  
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
  annual_df<-calcAnnual(df %>% select(-flag_minMax),searchStr=c("Med","Min","Max"),
             groupCols=c("Plant.Code","cooling","YEAR"))
  str<-paste0(gsub("Monthly","Annual",names(FEWSRtower.list)[r]),"<-annual_df")
  eval(parse(text=str))
  
  dfTeam<-df %>% select(-flag_minMax,-percentAllocation)
  dfTeam<-dfTeam %>% dplyr::mutate(Month=substr(months[Month],1,3))
  annual_dfTeam<-annual_df
  annual_dfTeam$Month<-rep("annual",nrow(annual_dfTeam))
  annual_dfTeam<-annual_dfTeam %>% select(all_of(names(dfTeam)))
  dfTeam<-rbind(dfTeam,annual_dfTeam)
  
  str<-paste0("dfTeam<-dfTeam %>% pivot_wider(names_from = Month, id_cols=c('Plant.Code','cooling','YEAR'),
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
  df<-df %>% select(-cooling,-percentAllocation,-flag_minMax)
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
  annual_dfTeamPlant<-annual_dfTeamPlant %>% select(all_of(names(dfTeamPlant)))
  dfTeamPlant<-rbind(dfTeamPlant,annual_dfTeamPlant)
  
  str<-paste0("dfTeamPlant<-dfTeamPlant %>% pivot_wider(names_from = Month, id_cols=c('Plant.Code','YEAR'),
                                  values_from=c(",
              paste(names(dfTeamPlant)[!names(dfTeamPlant) %in% c("Plant.Code","YEAR","Month")],collapse = ","),
              "))")
  eval(parse(text=str))
  
  if (r==1){
    dfTeamPlant_all<-dfTeamPlant
  }else{
    dfTeamPlant_all<-inner_join(dfTeamPlant_all,dfTeamPlant,by=c("Plant.Code","YEAR"))
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

#bind team data with ppVars
dfTeam_all<-left_join(dfTeam_all,ppVars,by=c("Plant.Code","cooling","YEAR"))
dfTeam_all<-dfTeam_all %>% select(all_of(names(ppVars)),
                                  contains("Med"),contains("Min"),contains("Max"))
names(dfTeam_all)<-gsub("Model_","",names(dfTeam_all))


#get plant-level ppVars
ppVarsPlant<-ppVars %>% select(-cooling,-dom_fuel)
ppVarsPlant_Char<-ppVarsPlant %>% select(-Net.Generation.Year.To.Date,-YEAR)
ppVarsPlant<-ppVarsPlant %>% select(Plant.Code,YEAR,Net.Generation.Year.To.Date)
ppVarsPlant<-ppVarsPlant %>% dplyr::group_by(Plant.Code,YEAR) %>%
  dplyr::summarise(Net.Generation.Year.To.Date=sum(Net.Generation.Year.To.Date,na.rm = T),.groups="drop")
ppVarsPlant_Char<-replaceMultiples(ppVarsPlant_Char,groupCols = c("Plant.Code"),
                                   countVar = "general_mover",repStr = "NGCC-ST")
ppVarsPlant_Char<-replaceMultiples(ppVarsPlant_Char,groupCols = c("Plant.Code"),
                                   countVar = "Plant.level_dom_fuel",repStr = "complex")
ppVarsPlant<-inner_join(ppVarsPlant_Char,ppVarsPlant,by=c("Plant.Code"))
dfTeamPlant_all<-left_join(dfTeamPlant_all,ppVarsPlant,by=c("Plant.Code","YEAR"))
dfTeamPlant_all<-dfTeamPlant_all %>% select(all_of(names(ppVarsPlant)),
                                            contains("Med"),contains("Min"),contains("Max"))
names(dfTeamPlant_all)<-gsub("Model_","",names(dfTeamPlant_all))

write.csv(dfTeam_all,file=paste0(outputData_path,y,"_resultsTeamByCooling.csv"),row.names = F)
write.csv(dfTeamPlant_all,file=paste0(outputData_path,y,"_resultsTeamByPlant.csv"),row.names = F)

}#for y

write.csv(compile_Monthly_Consump_EST,file=paste0(outputData_path,"compile_Monthly_Consump_EST.csv"),row.names = F)
write.csv(compile_Monthly_WD_EST,file=paste0(outputData_path,"compile_Monthly_WD_EST.csv"),row.names = F)
write.csv(compile_Annual_Consump_EST,file=paste0(outputData_path,"compile_Annual_Consump_EST.csv"),row.names = F)
write.csv(compile_Annual_WD_EST,file=paste0(outputData_path,"compile_Annual_WD_EST.csv"),row.names = F)

# write.csv(Monthly_Consump_EST,file=paste0(outputData_path,"Monthly_Consump_EST.csv"),row.names = F)
# write.csv(Monthly_WD_EST,file=paste0(outputData_path,"Monthly_WD_EST.csv"),row.names = F)
# write.csv(Annual_Consump_EST,file=paste0(outputData_path,"Annual_Consump_EST.csv"),row.names = F)
# write.csv(Annual_WD_EST,file=paste0(outputData_path,"Annual_WD_EST.csv"),row.names = F)



