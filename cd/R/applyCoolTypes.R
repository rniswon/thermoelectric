#'@title applyCoolTypes
#'@description applies percent allocations of cooling types to condenser duty values.  For 
#'             complex cooling plants both percentage and 100 percent values are given \cr \cr
#'Executed By: assignCooling.R \cr
#'@param coolType_df data.frame with cooling types and percent allocations by combogencoo
#'@param cd_df data.frame with the condenser duty values
#'@param coolTypes character vector of cooling types to apply
#'@param colNames character vector of column names from `coolType_df` that correspond to `coolTypes`
#'@param keepCols character vector of column names from the `coolType_df` to keep
#'@param joinCol character string indicating column to join `coolType_df` and `cd_df`
#'@return `all_final_duties` data.frame that combines condenser dutes for all `coolTypes` 
#'@examples
#'final_duties<-applyCoolTypes(coolType_df = combogencoo_paired_cooling, 
#'                             cd_df = non_nuke_CD_summary_df, 
#'                             coolTypes = c("tower","lake","pond","river","DC","OS"), 
#'                             colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","DC","OS"),
#'                             keepCols = c("combogen","combogencoo","bogen","X2015.corrected.cooling.type"),
#'                             joinCol = "bogen")
#'              


applyCoolTypes<-function(coolType_df, cd_df, coolTypes, colNames, keepCols, joinCol){ 
   
   for (c in 1:length(coolTypes)){

      #select columns from coolType_df
      cooling_combogencoo <- coolType_df %>%
         select(c(keepCols,colNames[c])) %>%
         dplyr::rename(filterCol = colNames[c]) %>%
         filter(!is.na(filterCol))
   

      
      #join with condenser duties
      condenser_duty <- left_join(cooling_combogencoo,cd_df,by=joinCol)
      condenser_duty<- condenser_duty %>%
         select(c(keepCols,filterCol,"Plant.Code",contains("CD")))
      
      
      #apply percent allocations
      final_duties <- condenser_duty %>% mutate_each(funs(.*filterCol), contains("CD"))
      final_duties <- dplyr::mutate(final_duties, percentAllocation = filterCol*100)
      final_duties <- subset(final_duties, select=-c(filterCol))
      final_duties_2 <- final_duties[!duplicated(final_duties),]
      final_duties_2 <- final_duties_2 %>% group_by(Plant.Code,percentAllocation) %>%
         summarize_at(names(.)[(length(keepCols)+2):(length(final_duties_2)-1)], sum, na.rm = T)
      final_duties_2 <- dplyr::mutate(final_duties_2, cooling = coolTypes[c])
      

      
      #get 100% allocations
      hundredPercents<- condenser_duty %>%
         filter(filterCol!=1)
      hundredPercents <- dplyr::mutate(hundredPercents, percentAllocation = 100)
      hundredPercents<-subset(hundredPercents, select=-c(filterCol))
      hundredPercents <- hundredPercents[!duplicated(hundredPercents),]
      hundredPercents <- hundredPercents %>% group_by(Plant.Code,percentAllocation) %>%
         summarize_at(names(.)[(length(keepCols)+2):(length(hundredPercents)-1)], sum, na.rm = T)
      
      #hundredPercents <- dplyr::mutate(hundredPercents, cooling = "all")
      hundredPercents <- dplyr::mutate(hundredPercents, cooling = coolTypes[c])
      
      
      final_duties_2<-rbind(final_duties_2,hundredPercents)
      final_duties_2<- final_duties_2 %>% select(Plant.Code,contains("CD"),percentAllocation,cooling)
      
      if (c==1){
         all_final_duties<-final_duties_2
      }else{
         all_final_duties<-rbind(all_final_duties,final_duties_2)
      } 
   }#for each cooltype 

   #remove NA Plant.Codes
   all_final_duties<-all_final_duties %>% filter(!is.na(Plant.Code))
   
   return(all_final_duties)
   
}