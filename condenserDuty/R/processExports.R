#'@title processExports
#'@description calculates monthly net condenser duty heat exports for non-nuclear plants \cr \cr
#'Executed By: non_nuke_condenser_duty.R \cr
#'@param plantList vector of all plant codes from 2015, input file 2015_Plants.csv
#'@param plant_exports monthly heat exports by plant, output of the calHeatExports() function 
#'                        in the heatExport_fun.R
#'@param non_nuke_cd combined steam and NGCC condenser duty output of analyzeSteamCD() and analyzeNGCCCD()
#'@param sheet4_key plant_gen to bogen associations
#'@param bogencoo.key associates boilers-generators-cooling systems
#'@param combogencoo_cooly_type List of bogencoo's cooling types for 2015 and 2010
#'@param outputData_path character string indicating path for output files
#'@return `non_nuke_cd_exports` data.frame containing monthly net condenser duty heat exports for 
#'                              non-nuclear plants, output of the processExports() function
#'@export

#processPlantExports
processExports <- function(plantList, plant_exports, non_nuke_cd,
                           sheet4_key, bogencoo.key, combogencoo_cooly_type,
                           outputData_path)
{
  if(!nrow(non_nuke_cd) > 0L){stop("problem with non nuke condenser duty table in processing exports.")}
 
  plant_exports <- ungroup(plant_exports)
  non_nuke_cd <- ungroup(non_nuke_cd)

   #get monthly exports >0  
monthly_exports <- plant_exports %>% 
                   filter(Plant.Code %in% plantList) %>% 
                   filter(plant_total > 0)


#check for multiple cooling types per plant
#get cooling types
combogencoo_paired_cooling<-prepCoolType(sheet4_key, bogencoo.key, combogencoo_cooly_type)
#select columns from coolType_df
colNames <- c("bogen","TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS")
cooling_combogencoo<-combogencoo_paired_cooling[,colNames]
#add Plant.Code
cooling_combogencoo<-merge(cooling_combogencoo,non_nuke_cd[,names(non_nuke_cd) %in% c("Plant.Code","bogen")])

#save bogen with cooling
cooling_bogen<-cooling_combogencoo[,names(cooling_combogencoo) %in% c("Plant.Code","bogen")]
#get first bogen with cooling
cooling_bogen <- cooling_bogen[!duplicated(cooling_bogen$Plant.Code),]

cooling_combogencoo<-cooling_combogencoo[,!names(cooling_combogencoo)=="bogen"]


# cooling_combogencoo<-rbind(cooling_combogencoo,cooling_combogencoo[1,])
# cooling_combogencoo[nrow(cooling_combogencoo),]$TOWER..RF..RI..RN.<-1000
countNames<-names(cooling_combogencoo)[names(cooling_combogencoo)!="Plant.Code"]
countCoolTypes<-eval(parse(text = paste0("cooling_combogencoo %>%
                                              dplyr::group_by(Plant.Code) %>%
                                              dplyr::summarise(count = n_distinct(",
                                              paste0(names(cooling_combogencoo)[names(cooling_combogencoo)!="Plant.Code"],collapse=","),"))")))



#cooling types of plants with monthly exports by Plant.Code
countCoolTypes <- countCoolTypes %>%
  filter(Plant.Code %in% monthly_exports$Plant.Code)


#plants with >1 unique cooling type allocation and subset out
multiCoolType<-countCoolTypes[which(countCoolTypes$count>1),]
cooling_bogen<-cooling_bogen[which(!cooling_bogen$Plant.Code %in% multiCoolType$Plant.Code),]
#output flag file of plants with multiple cooling type allocations and exports
flag_non_nuke_cd_multiCool_w_Export<-non_nuke_cd[which(non_nuke_cd$Plant.Code %in% 
                                                            multiCoolType$Plant.Code),]
write.csv(flag_non_nuke_cd_multiCool_w_Export, 
          paste0(outputData_path,'/flag_non_nuke_cd_multiCool_w_Export.csv'), 
          row.names = F)
#remove plants from CDs with multiple cooling type allocations and exports
non_nuke_cd<-non_nuke_cd[which(!non_nuke_cd$Plant.Code %in% multiCoolType$Plant.Code),]

#this is the exporter_plant_cd.csv file 
#it is condenser duties of plants with monthly exports by Plant.Code
#sum CD by plant for plants with monthly exports
exporter_plant_cd <- non_nuke_cd %>% 
                     filter(Plant.Code %in% monthly_exports$Plant.Code) %>% 
                     group_by(Plant.Code) %>% 
                     summarize_at(names(.)[3:14], sum, na.rm = T)

monthly_exports<-monthly_exports %>% filter(Plant.Code %in% exporter_plant_cd$Plant.Code)

#Now the exporter_plant_cd file of condenser duties with monthly exports 
#by Plant.Code are merged by Plant.Code with cooling_bogen which contains
#the first bogen for each Plant without plants with multiple cooling type 
#allocations
exporter_plant_cd <- left_join(exporter_plant_cd, cooling_bogen)



#cd_after_exports <- cbind(cd_after_exports[1], cd_after_exports[14], cd_after_exports[2:13] - cd_after_exports[(13+2):(13+13)])


#combine plants with exports and plants without exports
cd_after_exports <- cbind(monthly_exports[1], exporter_plant_cd[14], exporter_plant_cd[2:13] - monthly_exports[2:13])
# cd_after_exports<-cbind(monthly_exports[1], exporter_plant_cd[14])
# for (c in 2:13){
#   monthCDafter<-data.frame(exporter_plant_cd[c],monthly_exports[c])
#   monthCDafter$dif<-ifelse(monthCDafter[[1]]-monthCDafter[[2]]<0,
#                            monthCDafter[[1]],
#                            monthCDafter[[1]]-monthCDafter[[2]])
#   cd_after_exports<-cbind(cd_after_exports,monthCDafter$dif)
#   names(cd_after_exports)[length(cd_after_exports)]<-names(exporter_plant_cd)[c]
# }


overExport<-cd_after_exports[apply(cd_after_exports[3:14], 1, function(row) any(row < 0)),]


non_nuke_cd_exports <- as.data.frame(non_nuke_cd[!non_nuke_cd$Plant.Code %in% cd_after_exports$Plant.Code,])
non_nuke_cd_exports <- rbind(non_nuke_cd_exports[1:14], cd_after_exports)

return(non_nuke_cd_exports)
#the output of this function becomes the primary output for non_nuclear condenser duties
}
