#'@title crossCoolingID_cooling
#'@param bocoo
#'@param bogencoo.key
#'@param combogencoo_cooly_type
#'@param combogencoo_cooly_type_nukes
#'@param sheet4_key
#'@param nuke_combogencoo_key
#'@param boilerFuelData
#'@param gen_fuel_data
#'@export

crossCoolingID_cooling<-function(bocoo,bogencoo.key,combogencoo_cooly_type,
                                 combogencoo_cooly_type_nukes,sheet4_key,
                                 nuke_combogencoo_key,boilerFuelData,gen_fuel_data){
  
  #format boilerID in bocoo
  bocoo$flagBoiler.ID_bocoo<-sapply(bocoo$Boiler.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                      ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  bocoo$orig_Boiler.ID_bocoo<-bocoo$Boiler.ID
  
  bocoo$Boiler.ID <- gsub("(?<![0-9])", "",bocoo$Boiler.ID , perl = TRUE)
  bocoo$Boiler.ID <- gsub("[[:space:]]", "",bocoo$Boiler.ID ) 
  
  bocoo<-removeLeadZero(bocoo,"Boiler.ID","flagBoiler.ID_bocoo")
  
  #get combogencoo from bogencoo.key
  bogencoo.key<-bogencoo.key %>% select(Plant.Code,Boiler.ID,Bogen,combogencoo,combogen)
  names(bogencoo.key)[names(bogencoo.key)=="Bogen"]<-"bogen"
  bogencoo.keyCount<-bogencoo.key %>% dplyr::group_by(Plant.Code,bogen) %>% 
    dplyr::summarise(countBogen = length(unique(bogen)))
  bogencoo.keyCount<-bogencoo.keyCount %>% filter(countBogen==1)
  
  #fill missing boiler.ID for Plant.Code^NGCC and Plant.Code^ST bogens
  page1_bogencoo.key<-bogencoo.key %>% filter((regexpr("NGCC",bogen)>0 | regexpr("ST",bogen)>0) & is.na(Boiler.ID))
  bogencoo.key<-bogencoo.key %>% filter(!regexpr("NGCC",bogen)>0 & !regexpr("ST",bogen)>0 |!is.na(Boiler.ID))
  boilerFuelDataIDs<-boilerFuelData %>% select(Plant.Code,Boiler.ID,Reported.Prime.Mover)
  boilerFuelDataIDs$general_mover<-ifelse(boilerFuelDataIDs$Reported.Prime.Mover %in% c("CS","CA","CT"),"NGCC",boilerFuelDataIDs$Reported.Prime.Mover)
  page1_bogencoo.key$general_mover<-ifelse(regexpr("NGCC",page1_bogencoo.key$bogen)>0,"NGCC","ST")
  page1_bogencoo.key<-left_join(page1_bogencoo.key %>% select(-Boiler.ID),boilerFuelDataIDs,by=c("Plant.Code","general_mover"))
  #format boilerID in page1_bogencoo.key
  page1_bogencoo.key$flagBoiler.ID_page1_bogencoo.key<-sapply(page1_bogencoo.key$Boiler.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                        ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  page1_bogencoo.key$orig_Boiler.ID_page1_bogencoo.key<-page1_bogencoo.key$Boiler.ID
  
  page1_bogencoo.key$Boiler.ID <- gsub("(?<![0-9])", "",page1_bogencoo.key$Boiler.ID , perl = TRUE)
  page1_bogencoo.key$Boiler.ID <- gsub("[[:space:]]", "",page1_bogencoo.key$Boiler.ID ) 
  
  page1_bogencoo.key<-removeLeadZero(page1_bogencoo.key,"Boiler.ID","flagBoiler.ID_page1_bogencoo.key")
  page1_bogencoo.key<-page1_bogencoo.key %>% select(all_of(names(bogencoo.key)))
  #get boiler.IDs from EIA bogen association table
  page1_filled<-page1_bogencoo.key %>% filter(!is.na(Boiler.ID))
  page1_bogencoo.key<-page1_bogencoo.key %>% filter(is.na(Boiler.ID) & 
                                                      Plant.Code %in% bogencoo.keyCount$Plant.Code)
  bogenIDs<-bogen %>% select(Plant.Code,Boiler.ID)
   page1_bogencoo.key<-inner_join(page1_bogencoo.key %>% select(-Boiler.ID),bogenIDs,by=c("Plant.Code"))
  #format boilerID in page1_bogencoo.key
  page1_bogencoo.key$flagBoiler.ID_page1_bogencoo.key<-sapply(page1_bogencoo.key$Boiler.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                                                               ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  page1_bogencoo.key$orig_Boiler.ID_page1_bogencoo.key<-page1_bogencoo.key$Boiler.ID
  
  page1_bogencoo.key$Boiler.ID <- gsub("(?<![0-9])", "",page1_bogencoo.key$Boiler.ID , perl = TRUE)
  page1_bogencoo.key$Boiler.ID <- gsub("[[:space:]]", "",page1_bogencoo.key$Boiler.ID ) 
  
  page1_bogencoo.key<-removeLeadZero(page1_bogencoo.key,"Boiler.ID","flagBoiler.ID_page1_bogencoo.key")
  page1_bogencoo.key<-page1_bogencoo.key %>% select(all_of(names(bogencoo.key)))
  
  
  
  bogencoo.key<-rbind(bogencoo.key,page1_bogencoo.key,page1_filled)
  
  
  bocoo_all<-bocoo
  bocoo<-inner_join(bocoo,bogencoo.key,by=c("Plant.Code","Boiler.ID"))
  bocoo<-bocoo %>% select(-combogencoo,-combogen)
  #add single cooling type bocoos
  single.bocoo<-anti_join(bogencoo.key,bocoo,by=c("Plant.Code","Boiler.ID"))
  single.bocoo$Cooling.ID<-rep(NA,nrow(single.bocoo))
  single.bocoo<-single.bocoo %>% select(-combogencoo,-combogen)
  
  #format cooling from combogencoo_cool_type.keys
  combogencoo_cooly_type<-rbind.fill(combogencoo_cooly_type,combogencoo_cooly_type_nukes)
  combogencoo_paired_cooling<-prepCoolType(sheet4_key, bogencoo.key, combogencoo_cooly_type)
  bocoo<-applyCoolTypes_plant(coolType_df = combogencoo_paired_cooling,
                                  plantData = bocoo,
                                  coolTypes = c("tower","lake","pond","river","OS"),
                                  colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS"),
                                  keepCols_cool = c("combogencoo","bogen"),
                                  keepCols_data<-c("Cooling.ID"),
                                  joinCol = "bogen")
  
  single.bocoo<-applyCoolTypes_plant(coolType_df = combogencoo_paired_cooling,
                              plantData = single.bocoo,
                              coolTypes = c("tower","lake","pond","river","OS"),
                              colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS"),
                              keepCols_cool = c("combogencoo","bogen"),
                              keepCols_data<-c("Cooling.ID"),
                              joinCol = "bogen")
  
  countSingle.bocoo<-single.bocoo %>% dplyr::group_by(Plant.Code) %>%
    dplyr::summarise(countCool=length(na.omit(unique(cooling))))
  countSingle.bocoo<-countSingle.bocoo %>% filter(countCool==1)
  single.bocoo<-single.bocoo %>% filter(Plant.Code %in% countSingle.bocoo$Plant.Code)
  bocoo<-rbind(bocoo,single.bocoo)
  
  nuke_coo_type<-inner_join(nuke_combogencoo_key,combogencoo_cooly_type_nukes, by= c("combogencoo" ="bogencoo"))
  nuke_coo_type$Plant.Code<-sapply(nuke_coo_type$combogencoo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][2]))
  test<-nuke_coo_type %>% group_by(Plant.Code) %>% summarise(count=length(unique(combogencoo)))
  multiCool.nuke<-test %>% filter(count>1)
  oneCoolType.nuke<-nuke_coo_type[0,]
  for (p in unique(multiCool.nuke$Plant.Code)){
    subdata<-nuke_coo_type %>% filter(Plant.Code==p)
    subdata2<-subdata %>% select(-combogencoo,-plant_unit,-manualEdit)
    subdata2<-subdata2[!duplicated(subdata2),]
    if (nrow(subdata2)==1){
      oneCoolType.nuke<-rbind(oneCoolType.nuke,subdata[1,])
    }
  }
  test<-test %>% filter(count==1)
  
  nuke_coo_type<-nuke_coo_type %>% filter(Plant.Code %in% test$Plant.Code)
  nuke_coo_type<-rbind(nuke_coo_type,oneCoolType.nuke)
  nuke_coo_type<-nuke_coo_type %>% select(-plant_unit,-manualEdit)
  nuke_coo_type<-nuke_coo_type[!duplicated(nuke_coo_type),]
  nukes<-gen_fuel_data %>% filter(Nuclear.Unit.Id!=".")
  nuke.bfd<-boilerFuelData %>% filter(Plant.Code %in% nukes$Plant.Code) %>% select(Plant.Code,Boiler.ID)
  nuke.bogen<-bogen %>% filter(Plant.Code %in% nukes$Plant.Code) %>% select(Plant.Code,Boiler.ID)
  nuke.bocoo<-bocoo_all %>% filter(Plant.Code %in% nukes$Plant.Code) %>% select(Plant.Code,Boiler.ID)
  nuke.Boiler<-rbind(nuke.bfd,nuke.bogen,nuke.bocoo)
  nuke.Boiler<-nuke.Boiler[!duplicated(nuke.Boiler),]
  nuke_coo_type<-inner_join(nuke_coo_type,nuke.Boiler,by="Plant.Code")
  #format boilerID in nuke_coo_type
  nuke_coo_type$flagBoiler.ID_nuke_coo_type<-sapply(nuke_coo_type$Boiler.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                                                               ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  nuke_coo_type$orig_Boiler.ID_nuke_coo_type<-nuke_coo_type$Boiler.ID
  
  nuke_coo_type$Boiler.ID <- gsub("(?<![0-9])", "",nuke_coo_type$Boiler.ID , perl = TRUE)
  nuke_coo_type$Boiler.ID <- gsub("[[:space:]]", "",nuke_coo_type$Boiler.ID ) 
  
  nuke_coo_type<-removeLeadZero(nuke_coo_type,"Boiler.ID","flagBoiler.ID_nuke_coo_type")
  
  
  bocoo.nukes<-inner_join(bocoo_all,nuke_coo_type,by=c("Plant.Code","Boiler.ID"))
  bocoo.nukes<-bocoo.nukes %>% select(Plant.Code,Boiler.ID,Cooling.ID,combogencoo)
  
  bocoo.nukes<-applyCoolTypes_plant(coolType_df = nuke_coo_type,
                              plantData = bocoo.nukes,
                              coolTypes = c("tower","lake","pond","river","OS"),
                              colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS"),
                              keepCols_cool = c("combogencoo"),
                              keepCols_data<-c("Cooling.ID"),
                              joinCol = "combogencoo")
  bocoo.nukes<-bocoo.nukes %>% select(all_of(names(bocoo)))
  bocoo<-rbind(bocoo,bocoo.nukes)

  
  bocoo<-bocoo %>% mutate(cooling =ifelse(percentAllocation!=100 | flagComplex==TRUE,"complex",cooling))
  bocoo<-bocoo %>% filter(percentAllocation==100)
  bocoo<-bocoo[!duplicated(bocoo),]
  bocoo<-bocoo %>% select(-flagComplex,-percentAllocation)
  
  
  
  return(bocoo)
}