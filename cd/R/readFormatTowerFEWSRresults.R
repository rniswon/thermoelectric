readFormatTowerFEWSRresults<-function(TowerinputData_path,FEWSRinputData_path,eia_year,
                                      types=c("Big_Lake","Pond","River","Saline")){
#read tower results
  consumption_out <-read.csv(paste0(TowerinputData_path,eia_year,"_Tower_model_consumption_out.csv"),
                                           stringsAsFactors = FALSE)
  WD_out <-read.csv(paste0(TowerinputData_path,eia_year,"_Tower_model_withdrawal_out.csv"),
                                  stringsAsFactors = FALSE)
  #create aggregated Plant.ID column
    consumption_out$Plant_ID<-paste(consumption_out$Plant_ID,rep("tower",nrow(consumption_out)),consumption_out$percentAllocation,sep="^")
  WD_out$Plant_ID<-paste(WD_out$Plant_ID,rep("tower",nrow(WD_out)),WD_out$percentAllocation,sep="^")
  #remove percentAllocation column
    consumption_out<-consumption_out %>% select(-percentAllocation)
  WD_out<-WD_out %>% select(-percentAllocation)
  listFEWSR<-list(Med_Consump="Best Consumption Estimates",
                                Med_WD="Best Withdrawal Estimates",
                                Min_Consump="Min Consumpt with 22% cushion",
                                Min_WD="Estimated Min MGD Withdrawal",
                                Max_Consump="Max Consumpt with 22% cushion",
                                Max_WD ="Estimated Max MGD Withdrawal")
  #typeFEWSR<-c("lake","pond","river","saline")
    typeFEWSR<-types
  FEWSRfiles<-list.files(FEWSRinputData_path,pattern = "xlsx")
  FEWSRfiles<-FEWSRfiles[regexpr(eia_year,FEWSRfiles)>0]
  #read FEWSR results
    for (s in names(listFEWSR)){
      for (t in 1:length(typeFEWSR)){
        #read data
          strFileName<-FEWSRfiles[regexpr(typeFEWSR[t],FEWSRfiles)>0]
          tstr<-paste0(typeFEWSR[t],"_",s,"<-read_xlsx(paste0(FEWSRinputData_path,strFileName),sheet=listFEWSR$",s,")")
          eval(parse(text=tstr))
          #change colnames
            tstr<-paste0("colnames(",typeFEWSR[t],"_",s,") <- c('Plant_id', 1:12)")
          eval(parse(text=tstr))
         #bind by s
            if(t==1){
              sStr<-paste0("Fews_",s,"<-",typeFEWSR[t],"_",s)
              }else{
                sStr<-paste0("Fews_",s,"<-rbind(Fews_",s,",",typeFEWSR[t],"_",s,")")
                }
          eval(parse(text=sStr))
          }#for type t
      }#for s FEWSR result sheet
  
  listAgg<-c("Med","Min","Max")
  for (t in listAgg){
    #comsump
      df<-as.data.frame(consumption_out)
      # df<-df %>% select(Plant_ID,starts_with(tolower(t)))
        # df<-df[,names(df)!=tolower(t)]
        df<-df %>% select(Plant_ID,starts_with(tolower(t)),-ends_with("design"))
      colnames(df) <- c("Plant_id", 1:12)
      df[,2:13] <- lapply(df[,2:13], as.numeric)
      #withdrawl
        sumVarWD<-paste0("Model_",t,"_WD")
      df_WD<-as.data.frame(WD_out)
      df_WD<-df_WD %>% select(Plant_ID,starts_with(tolower(t)),-ends_with("design"))
      colnames(df_WD) <- c("Plant_id", 1:12)
      df_WD[,2:13] <- lapply(df_WD[,2:13], as.numeric)
      #flag min/max for complex plants
      df_WD<-findComplexPlants2(df_WD)
      
      df_WD<-pivot_longer(df_WD, cols = 2:13, names_to = 'Month', values_to = "sumVar")
      # df_WD$sumVar <-  df_WD$sumVar*1.4
      str<-paste0("Fews_WD<-Fews_",t,"_WD")
      eval(parse(text=str))
      Fews_WD <- pivot_longer(Fews_WD, cols = 2:13, names_to = 'Month', values_to = 'sumVar')
      df_WD_UpTemp<-rbind(df_WD,Fews_WD)
      df_WD_UpTemp$Month <- as.numeric(df_WD_UpTemp$Month)
      #join tower consump with fewsr
        str<-paste0("df_upTemp<-rbind(df,Fews_",t,"_Consump)")
      eval(parse(text=str))
  
      #flag min/max for complex plants
      df_upTemp<-findComplexPlants2(df_upTemp)
      #df_WD_UpTemp<-findComplexPlants2(df_WD_UpTemp)
      
      #monthly
        Monthly_df_upTemp<-pivot_longer(df_upTemp, cols = 2:13, names_to = 'Month',
                                                      values_to = "sumVar")
      Monthly_df_upTemp$Month <- as.numeric(Monthly_df_upTemp$Month)
      Monthly_df_upTemp <- Monthly_df_upTemp %>% dplyr::group_by(Plant_id, Month) %>% dplyr::summarise(sumVar = sum(sumVar,na.rm=T))
      
      #if saline consumption==NA
      Monthly_df_upTemp<-Monthly_df_upTemp %>% 
        dplyr::mutate(sumVar = ifelse(regexpr("OS",Plant_id)>0,NA,sumVar))
      
      names(Monthly_df_upTemp)[names(Monthly_df_upTemp)=="sumVar"]<-paste0("Model_",t,"_Consump")
      # str<-paste0("Monthly_",t,"_Mod_Consump_UpTemp<-Monthly_df_upTemp")
      # eval(parse(text=str))
      
      #withdrawl
        Monthly_df_upTempWD <- df_WD_UpTemp %>% dplyr::group_by(Plant_id, Month) %>% dplyr::summarise(sumVar = sum(sumVar,na.rm=T))
      names(Monthly_df_upTempWD)[names(Monthly_df_upTempWD)=="sumVar"]<-sumVarWD
      # str<-paste0("Monthly_",t,"_Mod_WD_UpTemp<-Monthly_df_upTempWD")
      # eval(parse(text=str))
      
      #annual
      #   names(Monthly_df_upTemp)[names(Monthly_df_upTemp)==paste0("Model_",t,"_Consump")]<-"sumVar"
      # #Question: should the mean be used here?
      #   Annual_df_upTemp<-Monthly_df_upTemp %>% dplyr::group_by(Plant_id) %>% dplyr::summarise(sumVar = mean(sumVar,na.rm=T))
      # names(Annual_df_upTemp)[names(Annual_df_upTemp)=="sumVar"]<-paste0("Model_",t,"_Consump")

      
      #withdrawl
     #    names(Monthly_df_upTempWD)[names(Monthly_df_upTempWD)==paste0("Model_",t,"_WD")]<-"sumVar"
     #  Annual_df_WD_UpTemp <- Monthly_df_upTempWD %>% dplyr::group_by(Plant_id) %>% dplyr::summarise(sumVar = mean(sumVar,na.rm=T))
     # names(Annual_df_WD_UpTemp)[names(Annual_df_WD_UpTemp)=="sumVar"]<-sumVarWD
     #  #fix names
 
      
        names(df_WD_UpTemp)[names(df_WD_UpTemp)=="sumVar"]<-sumVarWD
      names(df_WD)[names(df_WD)=="sumVar"]<-sumVarWD

    
      
      names(Monthly_df_upTemp)[names(Monthly_df_upTemp)=="sumVar"]<-paste0("Model_",t,"_Consump")
      names(Monthly_df_upTempWD)[names(Monthly_df_upTempWD)=="sumVar"]<-sumVarWD
      if (t==listAgg[1]){
        df_list_consump <- list(Monthly_df_upTemp)
        names(df_list_consump)<-paste0("Monthly_",t,"_Mod_Consump_UpTemp")
        df_list_WD <- list(Monthly_df_upTempWD)
        names(df_list_WD)<-paste0("Monthly_",t,"_Mod_WD_UpTemp")
        # df_list_consumpAnnual <- list(Annual_df_upTemp)
        # names(df_list_consumpAnnual)<-paste0("Annual_",t,"_Mod_Consump_UpTemp")
        # df_list_WDannual <- list(Annual_df_WD_UpTemp)
        # names(df_list_WDannual)<-paste0("Annual_",t,"_Mod_WD_UpTemp")
        }else{
          str<-paste0("df_list_consump$Monthly_",t,"_Mod_Consump_UpTemp<-Monthly_df_upTemp")
          eval(parse(text=str))
          str<-paste0("df_list_WD$Monthly_",t,"_Mod_WD_UpTemp<-Monthly_df_upTempWD")
          eval(parse(text=str))
          # str<-paste0("df_list_consumpAnnual$Annual_",t,"_Mod_Consump_UpTemp<-Annual_df_upTemp")
          # eval(parse(text=str))
          # str<-paste0("df_list_WDannual$Annual_",t,"_Mod_WD_UpTemp<-Annual_df_WD_UpTemp")
          # eval(parse(text=str))
          }
      }# for t in listAgg
  
  Monthly_Consump_EST <- df_list_consump %>% reduce(full_join, by= c('Plant_id', "Month"))
  Monthly_WD_EST <- df_list_WD %>% reduce(full_join, by= c('Plant_id', "Month"))
  # Annual_Consump_EST <- df_list_consumpAnnual %>% reduce(full_join, by='Plant_id')
  # Annual_WD_EST <- df_list_WDannual %>% reduce(full_join, by='Plant_id')
  
  out.list<-named.list(Monthly_Consump_EST,Monthly_WD_EST)
  return(out.list)
  }
  