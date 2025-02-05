replaceMultiples<-function(plantChar,groupCols,countVar,repStr){
#find multiples
  str<-paste0("multi<-plantChar %>% dplyr::group_by(",paste(groupCols,collapse = ","),") %>%
              dplyr::summarise(countVar=length(unique(",countVar,")))")
  eval(parse(text=str))
  

  multi<-multi %>% filter(countVar>1)
  if(nrow(multi)!=0){
    multi<-inner_join(multi %>% select(-countVar),plantChar, by =groupCols)
    plantChar<-anti_join(plantChar,multi,by =groupCols)
    str<-paste0("multi$",countVar,"<-rep(repStr,nrow(multi))")
    eval(parse(text=str))
    plantChar<-rbind(plantChar,multi)
    }
  plantChar<-plantChar[!duplicated(plantChar),]
  
  return(plantChar)
  }