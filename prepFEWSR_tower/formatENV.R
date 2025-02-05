#This software is preliminary or provisional and is subject to revision. It is being provided to meet 
#the need for timely best science. The software has not received final approval by the U.S. Geological 
#Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the 
#functionality of the software and related material nor shall the fact of release constitute any such 
#warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall 
#be held liable for any damages resulting from the authorized or unauthorized use of the software.

##################################################
## Script purpose: Format CD and Environmental variables for input into FEWSR and Tower
## RequiredLibraries : importThermoEIA, EIAplantAssociation, condenserDuty, dplyr
##################################################

#run these lines first time only to install packages
# install.packages("remotes")
# remotes::install_local("./Rpackages/importThermoEIA_1.0.tar.gz")
# remotes::install_local("./Rpackages/EIAplantAssociation_1.0.tar.gz",upgrade="never")
# remotes::install_local("./Rpackages/condenserDuty_1.0.tar.gz",upgrade="never")

#paths will need to be updated for release based on Amy's clean room
dataPath<-"D:/FEWSRinput/lowWTremoved/"
missingWT<-read.csv(paste0(dataPath,"missingWTdata_mah.csv"))
outputPath<-"D:/"

#########DO NOT EDIT BELOW THIS LINE#########
#load required libraries
library(importThermoEIA)
library(EIAplantAssociation)
library(condenserDuty)
library(dplyr)

years<-seq(2008,2020,1)

remove("extraDupsALL")
missingEIA_id<-data.frame(Plant.Code=numeric(0),YEAR=numeric(0))
for (y in years){
  print(y)
  
  df<-read.csv(paste0(dataPath,y,"_fewsr_tower_input.csv"))
  
  #get rid of WT_month_x columns
  df<-df %>% select(-(starts_with("WT_") & ends_with("_x")))
  names(df)[regexpr("WT_",names(df))>0]<-gsub("_y","",names(df)[regexpr("WT_",names(df))>0])
  
  #multiple CDs
  testCount<-df %>% select(Plant.Code,cooling,percentAllocation,starts_with("CD_"))
  testCount<-testCount[!duplicated(testCount),]
  testCount<-testCount %>% select(Plant.Code,cooling,percentAllocation,starts_with("CD_")) %>% 
    dplyr::group_by(Plant.Code,cooling,percentAllocation) %>%
    dplyr::summarise(countJan = length(CD_january))
  badCount<-testCount %>% filter(countJan>1)
  
  #multiples t_source
  testCount2<-df %>% dplyr::group_by(Plant.Code,cooling,percentAllocation) %>%
    dplyr::summarise(countT = length(unique(t_source)))
  badCount2<-testCount2 %>% filter(countT>1)
  
  dfDups<-inner_join(df,badCount2 %>% select(Plant.Code,cooling,percentAllocation), by=c("Plant.Code","cooling","percentAllocation"))
  
  dfNOdup<-anti_join(df,badCount2, by=c("Plant.Code","cooling","percentAllocation"))
  
  dfDups<-dfDups %>% filter(t_source=="sntemp")
  
  df.out<-rbind(dfNOdup,dfDups)
  df.out<-df.out[order(df.out$Plant.Code),]
  
  #check all dups removed
  testCount<-df.out %>% select(Plant.Code,cooling,percentAllocation,starts_with("CD_"))
  testCount<-testCount[!duplicated(testCount),]
  testCount<-testCount %>% select(Plant.Code,cooling,percentAllocation,starts_with("CD_")) %>% 
    dplyr::group_by(Plant.Code,cooling,percentAllocation) %>%
    dplyr::summarise(countJan = length(CD_january))
  badCount3<-testCount %>% filter(countJan>1)
  
  subMissing<-missingWT %>% filter(YEAR==y)
  subMissingGW<-subMissing %>% filter(gw_municipal==1)
  subMissingPond<-subMissing %>% filter(pond==1)
  
  #replace gw and municipal with annual average DB
  gwM<-df.out %>% filter(water_source_aeg %in% c("gw","gw_municipal","municipal","sw_municipal") | Plant.Code %in% subMissingGW$Plant.Code)
  df.out<-df.out %>% filter(!water_source_aeg %in% c("gw","gw_municipal","municipal","sw_municipal") & !Plant.Code %in% subMissingGW$Plant.Code) 
  gwM<-gwM %>% dplyr::mutate(WT_jan = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_feb = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_mar = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_apr = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_may = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_jun = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_jul = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_aug = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_sep = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_oct = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_nov = rowSums(select(., starts_with("DB_")), na.rm = T)/12,
                             WT_dec = rowSums(select(., starts_with("DB_")), na.rm = T)/12)
  df.out<-rbind(df.out,gwM)
  
  # --> change to monthly DB
  pond<-df.out %>% filter(cooling=="pond" | Plant.Code %in% subMissingPond$Plant.Code)
  df.out<-df.out %>% filter(cooling!="pond"  & !Plant.Code %in% subMissingPond$Plant.Code)
  pond<-pond %>% dplyr::mutate(WT_jan = DB_jan,
                               WT_feb = DB_feb,
                               WT_mar = DB_mar,
                               WT_apr = DB_apr,
                               WT_may = DB_may,
                               WT_jun = DB_jun,
                               WT_jul = DB_jul,
                               WT_aug = DB_aug,
                               WT_sep = DB_sep,
                               WT_oct = DB_oct,
                               WT_nov = DB_nov,
                               WT_dec = DB_dec)
  df.out<-rbind(df.out,pond)
  
  #replace high WTs with annual average DB
  #monthly WB  --> change to monthly DB
  df.out<-df.out %>% rowwise() %>% dplyr::mutate(WT_jan = ifelse(WT_jan==-99.9 | (!is.na(t_source) & is.na(WT_jan)),
                                                                 DB_jan,WT_jan),
                                                 WT_feb = ifelse(WT_feb==-99.9 | (!is.na(t_source) & is.na(WT_feb)),
                                                                 DB_feb,WT_feb),
                                                 WT_mar = ifelse(WT_mar==-99.9 | (!is.na(t_source) & is.na(WT_mar)),
                                                                 DB_mar,WT_mar),
                                                 WT_apr = ifelse(WT_apr==-99.9 | (!is.na(t_source) & is.na(WT_apr)),
                                                                 DB_apr,WT_apr),
                                                 WT_may = ifelse(WT_may==-99.9 | (!is.na(t_source) & is.na(WT_may)),
                                                                 DB_may,WT_may),
                                                 WT_jun = ifelse(WT_jun==-99.9 | (!is.na(t_source) & is.na(WT_jun)),
                                                                 DB_jun,WT_jun),
                                                 WT_jul = ifelse(WT_jul==-99.9 | (!is.na(t_source) & is.na(WT_jul)),
                                                                 DB_jul,WT_jul),
                                                 WT_aug = ifelse(WT_aug==-99.9 | (!is.na(t_source) & is.na(WT_aug)),
                                                                 DB_aug,WT_aug),
                                                 WT_sep = ifelse(WT_sep==-99.9 | (!is.na(t_source) & is.na(WT_sep)),
                                                                 DB_sep,WT_sep),
                                                 WT_oct = ifelse(WT_oct==-99.9 | (!is.na(t_source) & is.na(WT_oct)),
                                                                 DB_oct,WT_oct),
                                                 WT_nov = ifelse(WT_nov==-99.9 | (!is.na(t_source) & is.na(WT_nov)),
                                                                 DB_nov,WT_nov),
                                                 WT_dec = ifelse(WT_dec==-99.9 | (!is.na(t_source) & is.na(WT_dec)),
                                                                 DB_dec,WT_dec))
  
  df.out<-df.out %>% rowwise() %>% dplyr::mutate(ifelse(WT_jan==-999 & DB_jan>=0,
                                                        DB_jan,
                                                        ifelse(WT_jan==-999 & DB_jan<0,
                                                               0,WT_jan)),
                                                 ifelse(WT_feb==-999 & DB_feb>=0,
                                                        DB_feb,
                                                        ifelse(WT_feb==-999 & DB_feb<0,
                                                               0,WT_feb)),
                                                 ifelse(WT_mar==-999 & DB_mar>=0,
                                                        DB_mar,
                                                        ifelse(WT_mar==-999 & DB_mar<0,
                                                               0,WT_mar)),
                                                 ifelse(WT_apr==-999 & DB_apr>=0,
                                                        DB_apr,
                                                        ifelse(WT_apr==-999 & DB_apr<0,
                                                               0,WT_apr)),
                                                 ifelse(WT_may==-999 & DB_may>=0,
                                                        DB_may,
                                                        ifelse(WT_may==-999 & DB_may<0,
                                                               0,WT_may)),
                                                 ifelse(WT_jun==-999 & DB_jun>=0,
                                                        DB_jun,
                                                        ifelse(WT_jun==-999 & DB_jun<0,
                                                               0,WT_jun)),
                                                 ifelse(WT_jul==-999 & DB_jul>=0,
                                                        DB_jul,
                                                        ifelse(WT_jul==-999 & DB_jul<0,
                                                               0,WT_jul)),
                                                 ifelse(WT_aug==-999 & DB_aug>=0,
                                                        DB_aug,
                                                        ifelse(WT_aug==-999 & DB_aug<0,
                                                               0,WT_aug)),
                                                 ifelse(WT_sep==-999 & DB_sep>=0,
                                                        DB_sep,
                                                        ifelse(WT_sep==-999 & DB_sep<0,
                                                               0,WT_sep)),
                                                 ifelse(WT_oct==-999 & DB_oct>=0,
                                                        DB_oct,
                                                        ifelse(WT_oct==-999 & DB_oct<0,
                                                               0,WT_oct)),
                                                 ifelse(WT_nov==-999 & DB_nov>=0,
                                                        DB_nov,
                                                        ifelse(WT_nov==-999 & DB_nov<0,
                                                               0,WT_nov)),
                                                 ifelse(WT_dec==-999 & DB_dec>=0,
                                                        DB_dec,
                                                        ifelse(WT_dec==-999 & DB_dec<0,
                                                               0,WT_dec)))
  #replace cooling=pond with monthly WB

  df.out<-df.out[order(df.out$Plant.Code),]
  
  
  if (any(is.na(df.out$eia_id))){
    missing<-df.out %>% filter(is.na(eia_id))
    missing<-missing %>% select(Plant.Code)
    missing$YEAR<-rep(y,nrow(missing)) 
    missing<-missing[!duplicated(missing),]
    missingEIA_id<-rbind(missingEIA_id,missing)
  }
  
  df.out<-df.out %>% select(all_of(names(df)[regexpr("EV_",names(df))<0]),starts_with("EV_"))
  df.out.winter<-df.out %>% select(-starts_with("EV_"))
  
  #create winterized file for tower
  df.out.winter<-df.out.winter %>% dplyr::mutate(DB_jan = ifelse(DB_jan<4.44,4.44,DB_jan),
                                          DB_feb = ifelse(DB_feb<4.44,4.44,DB_feb),
                                          DB_mar = ifelse(DB_mar<4.44,4.44,DB_mar),
                                          DB_apr = ifelse(DB_apr<4.44,4.44,DB_apr),
                                          DB_may = ifelse(DB_may<4.44,4.44,DB_may),
                                          DB_jun = ifelse(DB_jun<4.44,4.44,DB_jun),
                                          DB_jul = ifelse(DB_jul<4.44,4.44,DB_jul),
                                          DB_aug = ifelse(DB_aug<4.44,4.44,DB_aug),
                                          DB_sep = ifelse(DB_sep<4.44,4.44,DB_sep),
                                          DB_oct = ifelse(DB_oct<4.44,4.44,DB_oct),
                                          DB_nov = ifelse(DB_nov<4.44,4.44,DB_nov),
                                          DB_dec = ifelse(DB_dec<4.44,4.44,DB_dec),
                                          WB_jan = ifelse(WB_jan<4.44,4.44,WB_jan),
                                          WB_feb = ifelse(WB_feb<4.44,4.44,WB_feb),
                                          WB_mar = ifelse(WB_mar<4.44,4.44,WB_mar),
                                          WB_apr = ifelse(WB_apr<4.44,4.44,WB_apr),
                                          WB_may = ifelse(WB_may<4.44,4.44,WB_may),
                                          WB_jun = ifelse(WB_jun<4.44,4.44,WB_jun),
                                          WB_jul = ifelse(WB_jul<4.44,4.44,WB_jul),
                                          WB_aug = ifelse(WB_aug<4.44,4.44,WB_aug),
                                          WB_sep = ifelse(WB_sep<4.44,4.44,WB_sep),
                                          WB_oct = ifelse(WB_oct<4.44,4.44,WB_oct),
                                          WB_nov = ifelse(WB_nov<4.44,4.44,WB_nov),
                                          WB_dec = ifelse(WB_dec<4.44,4.44,WB_dec),
                                          WT_jan = ifelse(WT_jan<0,0,WT_jan),
                                          WT_feb = ifelse(WT_feb<0,0,WT_feb),
                                          WT_mar = ifelse(WT_mar<0,0,WT_mar),
                                          WT_apr = ifelse(WT_apr<0,0,WT_apr),
                                          WT_may = ifelse(WT_may<0,0,WT_may),
                                          WT_jun = ifelse(WT_jun<0,0,WT_jun),
                                          WT_jul = ifelse(WT_jul<0,0,WT_jul),
                                          WT_aug = ifelse(WT_aug<0,0,WT_aug),
                                          WT_sep = ifelse(WT_sep<0,0,WT_sep),
                                          WT_oct = ifelse(WT_oct<0,0,WT_oct),
                                          WT_nov = ifelse(WT_nov<0,0,WT_nov),
                                          WT_dec = ifelse(WT_dec<0,0,WT_dec))
  
  months<-c("January","February","March","April","May",
            "June","July","August","September","October","November","December")
  dfHighWT<-df.out
  dfHighWT<-dfHighWT %>% filter(WT_jan>40 |
                                  WT_feb>40 |
                                  WT_mar>40 |
                                  WT_apr>40 |
                                  WT_may>40 |
                                  WT_jun>40 |
                                  WT_jul>40 |
                                  WT_aug>40 |
                                  WT_sep>40 |
                                  WT_oct>40 |
                                  WT_nov>40 |
                                  WT_dec>40)
  if (nrow(dfHighWT)!=0){
    dfHighWT$YEAR<-rep(y,nrow(dfHighWT))
    if (!exists("dfHighWT_all")){
      dfHighWT_all<-dfHighWT
    }else{
      dfHighWT_all<-rbind(dfHighWT_all,dfHighWT)
    }
  }
  
  if (identical(badCount,badCount3)){
    write.csv(df.out,file=paste0(dataPath,"updated_fewsr_tower_input/",y,"_fewsr_tower_input_final.csv"),row.names = F)
    write.csv(df.out.winter,file=paste0(dataPath,"updated_fewsr_tower_input/",y,"_fewsr_tower_input_final_winterized.csv"),row.names = F)
    
    }else{
    extraDups<-anti_join(badCount3,badCount)
    extraDups$YEAR<-rep(y,nrow(extraDups))
    if (!exists("extraDupsALL")){
      extraDupsALL<-extraDups
    }else{
      extraDupsALL<-rbind(extraDupsALL,extraDups)
    }
  }
}

write.csv(missingEIA_id,file=paste0(outputPath,"/missingWTdata2.csv"),row.names=F)
