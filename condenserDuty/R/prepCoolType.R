#'@title prepCoolType
#'@description reformat cooling type and join with bogen and combogencoo key\cr \cr
#'Executed By: \itemize{\item processExports.R 
#'                                        \item assignCooling.R} \cr
#'@param sheet4_key plant_gen to bogen associations
#'@param combogencoo_key associates boilers-generators-cooling systems
#'@param combogencoo_cooling_type_df List of bogencoo's cooling types for 2015 and 2010
#'combogencoo_paired_cooling<-prepCoolType(sheet4_key, combogencoo_key, combogencoo_cooling_type_df)
#'@export

prepCoolType<-function(sheet4_key, combogencoo_key, combogencoo_cooling_type_df){
for_cooling_bgk4 <- sheet4_key[c("plant_gen","bogen")]
for_cooling_bgk4$combogen <- paste("combogen",for_cooling_bgk4$bogen,sep="^")

final_combogencoo_key <- full_join_track(combogencoo_key[c("combogen","combogencoo")],for_cooling_bgk4, .merge = T)
final_combogencoo_key_2 <- final_combogencoo_key[which(final_combogencoo_key$.merge=="matched"),]
final_combogencoo_key_2 <- select(final_combogencoo_key_2, -".merge")

combogencoo_paired_cooling <- unique(full_join_track(final_combogencoo_key_2,combogencoo_cooling_type_df,by=c("combogencoo"="bogencoo"),.merge = T))

left_only <- combogencoo_paired_cooling[combogencoo_paired_cooling$.merge=="left_only",]

combogencoo_paired_cooling <- combogencoo_paired_cooling[combogencoo_paired_cooling$.merge=="matched",]
return(combogencoo_paired_cooling)

}