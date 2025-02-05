#'@title assignCooling
#'@description combines Steam Condenser Duties, NGCC Condenser Duties, Nuclear Conenser Duties, 
#'             and Cooling System Data. \cr \cr
#'Outputs the following files: \itemize{\item non_nuke_duties_w_cooling.csv 
#'                                        \item nuke_duties_w_cooling.csv
#'                                        \item final_CD_w_nukes_cooling.csv
#'                                        \item no_dupe_final_CD_w_nukes_cooling.csv} \cr
#'Executed By: CondenserDutyModel_new.R \cr
#'@param sheet4_key plant_gen to bogen associations
#'@param bogencoo.key associates boilers-generators-cooling systems
#'@param combogencoo_cooly_type List of bogencoo's cooling types for 2015 and 2010
#'@param nuke_combogencoo_key nuclear plant associate boilers-generators-cooling systems
#'@param non_nuke_CD_summary condenser duty model execution for non nuclear plants, output
#'                              of the non_nuke_condenser_duty() function
#'@param nuke_CD_summary condenser duty model execution for nuclear plants, output of the 
#'                          NukeCDcal() function
#'@param outputData_path character string indicating path for output files
#'@return `results` data.frame that combines Steam Condenser Duties, NGCC Condenser Duties, Nuclear Conenser Duties, 
#'                  and Cooling System Data 
#'@export


assignCooling <- function(sheet4_key, bogencoo.key, combogencoo_cooly_type, nuke_combogencoo_key, 
                           non_nuke_CD_summary, nuke_CD_summary,
                          outputData_path){
# for_cooling_bgk4 <- sheet4_key
# for_cooling_bgk4$combogen <- paste("combogen",for_cooling_bgk4$bogen,sep="^")
# 

# 
# 
# #names(bogencoo.key) <- c("combogen","combogencoo")
# final_combogencoo_key <- full_join_track(bogencoo.key,for_cooling_bgk4, .merge = T)
# final_combogencoo_key_2 <- final_combogencoo_key[which(final_combogencoo_key$.merge=="matched"),]
# final_combogencoo_key_2 <- select(final_combogencoo_key_2, -".merge")

# 
# combogencoo_paired_cooling <- unique(full_join_track(final_combogencoo_key_2,combogencoo_cooly_type,by=c("combogencoo"="bogencoo"),.merge = T))

# 
# left_only <- combogencoo_paired_cooling[combogencoo_paired_cooling$.merge=="left_only",]

# 
# combogencoo_paired_cooling <- combogencoo_paired_cooling[combogencoo_paired_cooling$.merge=="matched",]
  
  #make negative condenser duties zero
  non_nuke_CD_summary[,which(regexpr("CD",names(non_nuke_CD_summary))>0)]<-
    sapply(non_nuke_CD_summary[,which(regexpr("CD",names(non_nuke_CD_summary))>0)], 
           function(x) ifelse(x<0,0,x))
  nuke_CD_summary[,which(regexpr("CD",names(nuke_CD_summary))>0)]<-
    sapply(nuke_CD_summary[,which(regexpr("CD",names(nuke_CD_summary))>0)], 
           function(x) ifelse(x<0,0,x))
  
  
  combogencoo_paired_cooling<-prepCoolType(sheet4_key, bogencoo.key, combogencoo_cooly_type)

nuke_coo_type<-inner_join(nuke_combogencoo_key,combogencoo_cooly_type, by= c("combogencoo" ="bogencoo"))
# i<-i+1
# assign("i",i,envir = .GlobalEnv)
# print("line 43")

final_duties<-applyCoolTypes(coolType_df = combogencoo_paired_cooling, 
                             cd_df = non_nuke_CD_summary, 
                             coolTypes = c("tower","lake","pond","river","OS"), 
                             colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS"),
                             keepCols = c("combogen","combogencoo","bogen"),
                             joinCol = "bogen")


nuke_duties<-applyCoolTypes(coolType_df = nuke_coo_type,
                             cd_df = nuke_CD_summary,
                             coolTypes = c("tower","lake","pond","river","OS"),
                             colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS"),
                             keepCols = c("combogencoo","plant_unit"),
                            joinCol = "plant_unit")



names(nuke_duties) <- names(final_duties)

final_CD_w_nukes <- rbind(final_duties, nuke_duties)
no_dup_final_CD_w_nukes <- final_CD_w_nukes[!duplicated(final_CD_w_nukes),]

# #make negative condenser duties zero
# final_CD_w_nukes[2:13]<-sapply(final_CD_w_nukes[2:13], function(x) ifelse(x<0,0,x))
# no_dup_final_CD_w_nukes[2:13]<-sapply(no_dup_final_CD_w_nukes[2:13], function(x) ifelse(x<0,0,x))
# nuke_duties[2:13]<-sapply(nuke_duties[2:13], function(x) ifelse(x<0,0,x))
# final_duties[2:13]<-sapply(final_duties[2:13], function(x) ifelse(x<0,0,x))

results <- list(non_nuke_duties_w_cooling = final_duties, 
                nuke_duties_w_cooling = nuke_duties,
                final_CD_w_nukes_cooling = final_CD_w_nukes,
                no_dupe_final_CD_w_nukes_cooling = no_dup_final_CD_w_nukes
                )

eia_year<-unique(na.omit(sheet4_key$YEAR))
write.csv(final_duties ,paste0(outputData_path,'/non_nuke_duties_w_cooling.csv'), row.names = F)
write.csv(nuke_duties ,paste0(outputData_path,'/nuke_duties_w_cooling.csv'), row.names = F)
write.csv(final_CD_w_nukes ,paste0(outputData_path,'/final_CD_w_nukes_cooling.csv'), row.names = F)
write.csv(final_CD_w_nukes ,paste0(outputData_path,'/CD_results',eia_year,'.csv'), row.names = F)
write.csv(no_dup_final_CD_w_nukes ,paste0(outputData_path,'/no_dupe_final_CD_w_nukes_cooling.csv'), row.names = F)

return(results)
}
