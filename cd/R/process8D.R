process8D<-function(cooling8D,crossCool,eia_year){
    if (eia_year<2013){#0.1ft^3/second to gallons/minute
    cooling8D<-cooling8D %>% dplyr::mutate(Withdrawal_GPM = Withdrawal_GPM*60*7.4805194805)
    cooling8D<-cooling8D %>% dplyr::mutate(Consumption_GPM = Consumption_GPM*60*7.4805194805)
    }
  
  if (eia_year>=2010){
  cooling8D$Calc_Withdrawal_MG <- ((cooling8D$Withdrawal_GPM*60)* cooling8D$Service_hrs)/1000000
  cooling8D$Calc_Consump_MG <- ((cooling8D$Consumption_GPM*60)* cooling8D$Service_hrs)/1000000
  cooling8D$Calc_Withdrawal_MGD <- (cooling8D$Calc_Withdrawal_MG/daysInMonth(cooling8D$Month, rep(eia_year,nrow(cooling8D))))
  cooling8D$Calc_Consump_MGD <- (cooling8D$Calc_Consump_MG/daysInMonth(cooling8D$Month, rep(eia_year,nrow(cooling8D))))
  Monthlycooling8D <- cooling8D %>% select(Plant.Code,Cooling.ID,Month,Calc_Withdrawal_MGD,Calc_Consump_MGD)
 
  #add daysInMonth and daysInYear
  Monthlycooling8D$YEAR<-rep(eia_year,nrow(Monthlycooling8D))
  Monthlycooling8D$daysInMonth<-as.numeric(lubridate::days_in_month(as.POSIXct(paste0(Monthlycooling8D$Month,
                                                                                              "-1-",Monthlycooling8D$YEAR),
                                                                                       format="%m-%d-%Y")))
  Monthlycooling8D$daysInYear<-ifelse(lubridate::leap_year(as.POSIXct(paste0(Monthlycooling8D$Month,
                                                                        "-1-",Monthlycooling8D$YEAR),
                                                                 format="%m-%d-%Y")),366,365)
  
  #format boilerID in crossCool
  crossCool$flagCooling.ID_crossCool<-sapply(crossCool$Cooling.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                        ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  crossCool$orig_Cooling.ID_crossCool<-crossCool$Cooling.ID
  
  crossCool$Cooling.ID <- gsub("(?<![0-9])", "",crossCool$Cooling.ID , perl = TRUE)
  crossCool$Cooling.ID <- gsub("[[:space:]]", "",crossCool$Cooling.ID ) 
  
  crossCool<-removeLeadZero(crossCool,"Cooling.ID","flagCooling.ID_crossCool")
  
  #format boilerID in Monthlycooling8D
  Monthlycooling8D$flagCooling.ID_Monthlycooling8D<-sapply(Monthlycooling8D$Cooling.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                                    ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  Monthlycooling8D$orig_Cooling.ID_Monthlycooling8D<-Monthlycooling8D$Cooling.ID
  
  Monthlycooling8D$Cooling.ID <- gsub("(?<![0-9])", "",Monthlycooling8D$Cooling.ID , perl = TRUE)
  Monthlycooling8D$Cooling.ID <- gsub("[[:space:]]", "",Monthlycooling8D$Cooling.ID ) 
  
  Monthlycooling8D<-removeLeadZero(Monthlycooling8D,"Cooling.ID","flagCooling.ID_Monthlycooling8D")
  
  #test Cooling.ID matches
  noMatch<-anti_join(Monthlycooling8D,crossCool,by=c("Plant.Code","Cooling.ID"))
  noMatch<-noMatch %>% filter(!is.na(Calc_Consump_MGD) |!is.na(Calc_Withdrawal_MGD))
  noMatch<-noMatch %>% filter(Plant.Code %in% crossCool$Plant.Code)
  #find single Cooling.IDs in Monthlycool8D match with single cooling types
  singleMonthly8D<-Monthlycooling8D %>% dplyr::group_by(Plant.Code,Month) %>%
    dplyr::summarise(countCool=length(unique(Cooling.ID)))
  singleMonthly8D<-singleMonthly8D %>% filter(countCool==1 & Plant.Code %in% noMatch$Plant.Code)
  singleMonthly8D<-Monthlycooling8D %>% filter(Plant.Code %in% singleMonthly8D$Plant.Code)
  singleMonthly8D<-singleMonthly8D %>% select(Plant.Code,Cooling.ID)
  singleMonthly8D<-singleMonthly8D[!duplicated(singleMonthly8D),]
  singleCrossCool<-crossCool %>% dplyr::group_by(Plant.Code) %>%
    dplyr::summarise(countCool=length(unique(cooling)))
  singleCrossCool<-singleCrossCool %>% filter(countCool==1 & Plant.Code %in% noMatch$Plant.Code)
  singleCrossCool<-crossCool %>% filter(Plant.Code %in% singleCrossCool$Plant.Code)
  singleCrossCool<-singleCrossCool %>% select(-Cooling.ID, -flagCooling.ID_crossCool,-orig_Cooling.ID_crossCool)
  singleCrossCool<-singleCrossCool[!duplicated(singleCrossCool),]
  singleCrossCool<-inner_join(singleCrossCool,singleMonthly8D,by="Plant.Code")
  crossCool<-crossCool %>% filter(!Plant.Code %in% singleCrossCool$Plant.Code)
  crossCool<-rbind.fill(crossCool,singleCrossCool)
  
  #add model cooling types and aggregate by cooling/month
  Monthlycooling8D<-inner_join(Monthlycooling8D,crossCool,by=c("Plant.Code","Cooling.ID"))
  Monthly_Consump_EST.8D<-Monthlycooling8D %>% 
    dplyr::group_by(Plant.Code, Month,cooling,daysInMonth,daysInYear) %>% 
    dplyr::summarise(Rep_Consump = sum(Calc_Consump_MGD,na.rm = T))
  Monthly_WD_EST.8D<-Monthlycooling8D %>% 
    dplyr::group_by(Plant.Code, Month,cooling,daysInMonth,daysInYear) %>% 
    dplyr::summarise(Rep_WD = sum(Calc_Withdrawal_MGD,na.rm=T))
  

  
  #get annual by cooling
  Annual_Consump_EST.8D<-Monthly_Consump_EST.8D %>% 
    dplyr::mutate(Rep_Consump=Rep_Consump*daysInMonth,.groups='drop')
  Annual_Consump_EST.8D<-Annual_Consump_EST.8D %>% dplyr::group_by(Plant.Code,cooling) %>% 
    dplyr::summarise(Rep_Consump = sum(Rep_Consump,na.rm = T)/as.numeric(unique(daysInYear)))
  Annual_WD_EST.8D<-Monthly_WD_EST.8D %>% 
    dplyr::mutate(Rep_WD=Rep_WD*daysInMonth,.groups='drop')
  Annual_WD_EST.8D<-Annual_WD_EST.8D %>% dplyr::group_by(Plant.Code,cooling) %>% 
    dplyr::summarise(Rep_WD = sum(Rep_WD,na.rm = T)/as.numeric(unique(daysInYear)))
  
  #Aggregatating by Plant.Code and Month as there are multiple cooling systems for some plants.
  Monthly_Consump_EST_plantLevel.8D <- Monthlycooling8D %>% dplyr::group_by(Plant.Code, Month,daysInMonth,daysInYear) %>% 
    dplyr::summarise(Rep_Consump = sum(Calc_Consump_MGD,na.rm = T))
  Monthly_Consump_EST_plantLevel.8D <- Monthly_Consump_EST_plantLevel.8D[!is.na(Monthly_Consump_EST_plantLevel.8D$Rep_Consump),]
  Annual_Consump_EST_plantLevel.8D<-Monthly_Consump_EST_plantLevel.8D %>% 
    dplyr::mutate(Rep_Consump=Rep_Consump*daysInMonth,.groups='drop')
  Annual_Consump_EST_plantLevel.8D<-Annual_Consump_EST_plantLevel.8D %>% dplyr::group_by(Plant.Code) %>% 
    dplyr::summarise(Rep_Consump = sum(Rep_Consump,na.rm = T)/as.numeric(unique(daysInYear)))
  
  Monthly_WD_EST_plantLevel.8D <- Monthlycooling8D %>% dplyr::group_by(Plant.Code, Month,daysInMonth,daysInYear) %>% 
    dplyr::summarise(Rep_WD = sum(Calc_Withdrawal_MGD,na.rm=T))
  Monthly_WD_EST_plantLevel.8D <- Monthly_WD_EST_plantLevel.8D[!is.na(Monthly_WD_EST_plantLevel.8D$Rep_WD),]
  Annual_WD_EST_plantLevel.8D<-Monthly_WD_EST_plantLevel.8D %>% 
    dplyr::mutate(Rep_WD=Rep_WD*daysInMonth,.groups='drop')
  Annual_WD_EST_plantLevel.8D<-Annual_WD_EST_plantLevel.8D %>% dplyr::group_by(Plant.Code) %>% 
    dplyr::summarise(Rep_WD = sum(Rep_WD,na.rm = T)/as.numeric(unique(daysInYear)))
  
  #remove daysInYear, daysInMonth
  
  
out.list<-named.list(Monthly_Consump_EST.8D,Monthly_WD_EST.8D,
                     Annual_Consump_EST.8D,Annual_WD_EST.8D,
                     Monthly_Consump_EST_plantLevel.8D,Monthly_WD_EST_plantLevel.8D,
                     Annual_Consump_EST_plantLevel.8D,Annual_WD_EST_plantLevel.8D)
  }else{#annual analysis only prior 2010
    cooling8D$Calc_Withdrawal_MGD <- cooling8D$Withdrawal_GPM*60*24/1000000
    cooling8D$Calc_Consump_MGD <- cooling8D$Consumption_GPM*60*24/1000000
    Annual_Consump_EST.8D<-inner_join(cooling8D,crossCool,by=c("Plant.Code","Cooling.ID"))
    Annual_Consump_EST.8D<-Annual_Consump_EST.8D %>% 
      dplyr::group_by(Plant.Code,cooling) %>% 
      dplyr::summarise(Rep_Consump = sum(Calc_Consump_MGD,na.rm = T))
    Annual_WD_EST.8D<-inner_join(cooling8D,crossCool,by=c("Plant.Code","Cooling.ID"))
    Annual_WD_EST.8D<-Annual_WD_EST.8D %>% 
      dplyr::group_by(Plant.Code,cooling) %>% 
      dplyr::summarise(Rep_WD = sum(Calc_Withdrawal_MGD,na.rm = T))
    
    #plant-level
    Annual_Consump_EST_plantLevel.8D<-Annual_Consump_EST.8D %>% 
      dplyr::group_by(Plant.Code) %>% 
      dplyr::summarise(Rep_Consump = sum(Rep_Consump,na.rm = T))
    Annual_WD_EST_plantLevel.8D<-Annual_WD_EST.8D %>% 
      dplyr::group_by(Plant.Code) %>% 
      dplyr::summarise(Rep_WD = sum(Rep_WD,na.rm = T))
    
    out.list<-named.list(Annual_Consump_EST.8D,Annual_WD_EST.8D,
                         Annual_Consump_EST_plantLevel.8D,Annual_WD_EST_plantLevel.8D)
    
  }

  #remove daysInYear, daysInMonth
for (t in 1:length(out.list)){
  df<-out.list[[t]]
  if ("daysInYear" %in% names(df)){
  df<-df %>% ungroup() %>% select(-daysInYear,-daysInMonth)
  out.list[[t]]<-df 
   }
 
  
}

return(out.list)
  
  
  
}