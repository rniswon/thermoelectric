#'@title calcAnnual
#'@description
#'@param monthlyData
#'@param searchStr
#'@param groupCols
#'@export
#'


calcAnnual<-function(monthlyData,searchStr=c("Med","Min","Max"),groupCols){

  monthlyData$daysInMonth<-as.numeric(lubridate::days_in_month(as.POSIXct(paste0(monthlyData$Month,
                                                                                 "-1-",monthlyData$YEAR),
                                                                          format="%m-%d-%Y")))
  monthlyData$daysInYear<-ifelse(lubridate::leap_year(as.POSIXct(paste0(monthlyData$Month,
                                                                            "-1-",monthlyData$YEAR),
                                                                     format="%m-%d-%Y")),366,365)
  for (c in searchStr){
    subdata<-monthlyData %>% select(all_of(groupCols),"Month","daysInMonth","daysInYear",contains(c))
    NAME<-names(subdata)[regexpr(c,names(subdata))>0]
    names(subdata)[regexpr(c,names(subdata))>0]<-"sumVar"
    str<-paste0("subdata<-subdata %>% dplyr::group_by(",
    paste(c(groupCols,"Month","daysInMonth","daysInYear"),collapse=","),") %>%
      dplyr::summarise(sumVar=sum(sumVar,na.rm = T),.groups='drop')")
    eval(parse(text=str))
    
    subdata<-subdata %>% dplyr::mutate(sumVar=sumVar*daysInMonth,.groups='drop')
    str<-paste0("subdata<-subdata %>% dplyr::group_by(",paste(c(groupCols),collapse=","),") %>%
      dplyr::summarise(sumVar=sum(sumVar,na.rm = T)/as.numeric(unique(daysInYear)),.groups='drop')")
    
    eval(parse(text=str))
    names(subdata)[names(subdata)=="sumVar"]<-NAME
    
    if (c==searchStr[1]){
      annualData<-subdata
    }else{
      annualData<-inner_join(annualData,subdata,by=groupCols)
    }
  }
  
  return(annualData)
  
}