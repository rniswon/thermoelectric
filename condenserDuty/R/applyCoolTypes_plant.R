#'@title applyCoolTypes_plant
#'@description
#'@param coolType_df
#'@param plantData
#'@param coolTypes
#'@param colNames
#'@param keepCols_cool
#'@param keepCols_data
#'@param joinCol
#'@param sumCols
#'@export


applyCoolTypes_plant<-function(coolType_df,plantData,coolTypes,colNames, 
                     keepCols_cool,keepCols_data,joinCol,sumCols=NA){
  for (c in 1:length(coolTypes)){
    
    #select columns from coolType_df
    cooling_combogencoo <- coolType_df %>%
      select(c(keepCols_cool,colNames[c])) %>%
      dplyr::rename(filterCol = colNames[c]) %>%
      filter(!is.na(filterCol))
    
    
    
    #join with condenser duties
    outdata <- left_join(cooling_combogencoo,plantData,by=joinCol)
    outdata<- outdata %>%
      select(c(keepCols_cool,keepCols_data,filterCol,"Plant.Code"))
    outdata_all<-outdata
    
    #apply percent allocations
    outdata <- dplyr::mutate(outdata, percentAllocation = filterCol*100)
    outdata_2 <- outdata[!duplicated(outdata),]
    if (!is.na(sumCols)){
      outdata_2 <- outdata_2 %>% dplyr::mutate_each(funs(.*filterCol), sumCols)
      outdata_2 <- outdata_2 %>% dplyr::group_by(Plant.Code,percentAllocation) %>%
       dplyr::summarize_at(names(outdata_2)[names(outdata_2)==sumCols], sum, na.rm = T) 
    }else{
     outdata_2 <- subset(outdata_2, select=-c(filterCol)) 
    }
    outdata_2 <- outdata_2[!duplicated(outdata_2),]
    outdata_2 <- dplyr::mutate(outdata_2, cooling = coolTypes[c])
    
    
    
    #get 100% allocations
    hundredPercents<- outdata_all %>%
      filter(filterCol!=1)
    hundredPercents <- dplyr::mutate(hundredPercents, percentAllocation = 100)
    hundredPercents<-subset(hundredPercents, select=-c(filterCol))
    hundredPercents <- hundredPercents[!duplicated(hundredPercents),]
    if (!is.na(sumCols)){
    hundredPercents <- hundredPercents %>% dplyr::group_by(Plant.Code,percentAllocation) %>%
      dplyr::summarize_at(names(hundredPercents)[names(hundredPercents)==sumCols], sum, na.rm = T)
    }
    #hundredPercents <- dplyr::mutate(hundredPercents, cooling = "all")
    hundredPercents <- dplyr::mutate(hundredPercents, cooling = coolTypes[c])
    
    #flag complex
    hundredPercents$flagComplex<-rep(TRUE,nrow(hundredPercents))
    outdata_2$flagComplex<-ifelse(outdata_2$percentAllocation!=100,TRUE,FALSE)
    
    outdata_2<-rbind(outdata_2,hundredPercents)
    outdata_2<- outdata_2 %>% select(Plant.Code,percentAllocation,cooling,flagComplex,keepCols_data)
    
    if (c==1){
      all_plantData<-outdata_2
    }else{
      all_plantData<-rbind(all_plantData,outdata_2)
    } 
  }#for each cooltype 
  
  #remove NA Plant.Codes
  all_plantData<-all_plantData %>% filter(!is.na(Plant.Code))
  
  return(all_plantData)
  
}