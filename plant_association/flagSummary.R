flagList<-openxlsx::read.xlsx("E:/Corona_VPN/WBEEP/testAssoc/ColumnLegend_metafile.xlsx",sheet="Flags")
flagList<-flagList[1:24,1]

flagSummary<-data.frame(flag=flagList,sheet3_Mismatches=rep(NA,length(flagList)),sheet3_Matches=rep(NA,length(flagList)),
                        sheet4_Mismatches=rep(NA,length(flagList)),sheet4_Matches=rep(NA,length(flagList)),
                        total_Mismatches=rep(NA,length(flagList)),total_matches=rep(NA,length(flagList)))
flagSummaryBogen<-flagSummary
#add countFlags column
countFlagfunc<-function(f){
  if (!is.na(f)){
    if(f==TRUE | f==1 | f %in% c("OS","OA","RE")){
      c<-1
    }else{#no flag
      c<-0
    }
  }else{#missing flag
    c<-0
  }
  return(c)
}

for (f in flagList){
  eval(parse(text=paste0("subsheet3MisMatch<-onlyAutoBogens %>% rowwise() %>% mutate(countFlag = sum(countFlagfunc(",f,")))")))
  subsheet3MisMatch<-subsheet3MisMatch %>% filter(countFlag==1)
  flagSummary[flagSummary$flag==f,]$sheet3_Mismatches<-length(unique(subsheet3MisMatch$Plant.Code))
  flagSummaryBogen[flagSummaryBogen$flag==f,]$sheet3_Mismatches<-length(unique(subsheet3MisMatch$bogen))
  
  subsheet3Match<-auto_sheet3_key[!auto_sheet3_key$Plant.Code %in% onlyAutoBogens & !auto_sheet3_key$Plant.Code %in% onlyOrigBogens,]
  eval(parse(text=paste0("subsheet3Match<-subsheet3Match %>% rowwise() %>% mutate(countFlag = sum(countFlagfunc(",f,")))")))
  subsheet3Match<-subsheet3Match %>% filter(countFlag==1)
  flagSummary[flagSummary$flag==f,]$sheet3_Matches<-length(unique(subsheet3Match$Plant.Code))
  flagSummaryBogen[flagSummaryBogen$flag==f,]$sheet3_Matches<-length(unique(subsheet3Match$bogen))
  
  
  eval(parse(text=paste0("subsheet4MisMatch<-onlyAutoBogens2 %>% rowwise() %>% mutate(countFlag = sum(countFlagfunc(",f,")))")))
  subsheet4MisMatch<-subsheet4MisMatch %>% filter(countFlag==1)
  flagSummary[flagSummary$flag==f,]$sheet4_Mismatches<-length(unique(subsheet4MisMatch$Plant.Code))
  flagSummaryBogen[flagSummaryBogen$flag==f,]$sheet4_Mismatches<-length(unique(subsheet4MisMatch$bogen))
  
  subsheet4Match<-auto_sheet4_key[!auto_sheet4_key$Plant.Code %in% onlyAutoBogens2 & !auto_sheet4_key$Plant.Code %in% onlyOrigBogens2,]
  eval(parse(text=paste0("subsheet4Match<-subsheet4Match %>% rowwise() %>% mutate(countFlag = sum(countFlagfunc(",f,")))")))
  subsheet4Match<-subsheet4Match %>% filter(countFlag==1)
  flagSummary[flagSummary$flag==f,]$sheet4_Matches<-length(unique(subsheet4Match$Plant.Code))
  flagSummaryBogen[flagSummaryBogen$flag==f,]$sheet4_Matches<-length(unique(subsheet4Match$bogen))
  
  
  flagSummary[flagSummary$flag==f,]$total_Mismatches<-flagSummary[flagSummary$flag==f,]$sheet3_Mismatches+flagSummary[flagSummary$flag==f,]$sheet4_Mismatches
  flagSummary[flagSummary$flag==f,]$total_matches<-flagSummary[flagSummary$flag==f,]$sheet3_Matches+flagSummary[flagSummary$flag==f,]$sheet4_Matches
  flagSummaryBogen[flagSummaryBogen$flag==f,]$total_Mismatches<-flagSummaryBogen[flagSummaryBogen$flag==f,]$sheet3_Mismatches+flagSummaryBogen[flagSummaryBogen$flag==f,]$sheet4_Mismatches
  flagSummaryBogen[flagSummaryBogen$flag==f,]$total_matches<-flagSummaryBogen[flagSummaryBogen$flag==f,]$sheet3_Matches+flagSummaryBogen[flagSummaryBogen$flag==f,]$sheet4_Matches

  }
write.csv(flagSummary,paste0(pathWrite,"flagSummary_byPlant.csv"),row.names = F)
write.csv(flagSummaryBogen,paste0(pathWrite,"flagSummary_byBogen.csv"),row.names = F)


auto_sheet3_key$anyFlag<-ifelse(auto_sheet3_key$countFlags>=1,1,0)
test<-auto_sheet3_key %>% filter(anyFlag==1)
test2<-anti_join(auto_sheet3_key,test,by="Plant.Code")

test3<-auto_sheet3_key %>% filter(Boiler.Status %in% c("OS","OA","RE") & countFlags==1)
nrow(test3)
onlyAutoBogens %>% filter(Boiler.ID.NotIn.Assoc==TRUE & countFlags==1)
onlyAutoBogens2 %>% filter(Boiler.ID.NotIn.Assoc==TRUE & countFlags==1)


