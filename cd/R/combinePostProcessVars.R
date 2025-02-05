combinePostProcessVars<-function(plantDom,plantDom_page1,bogen_netgen,bogen_fuel_heat,gen_fuel_data,
                       combogencoo_key, combogencoo_cooly_type,combogencoo_cooly_type_nukes,sheet4_key,
                       nuke_combogencoo_key,plantInfo,plantList){
  #saveRPM
  bogen_RPM<-bogen_fuel_heat %>% select(bogen,Reported.Prime.Mover)
  nuke_RPM<-gen_fuel_data %>% filter(Nuclear.Unit.Id!="." & !is.na(Nuclear.Unit.Id))
  nuke_RPM$bogen<-paste(nuke_RPM$Plant.Code,nuke_RPM$Nuclear.Unit.Id,sep="^")
  nuke_RPM<-nuke_RPM %>% select(bogen,Reported.Prime.Mover)
  all_RPM<-rbind(bogen_RPM,nuke_RPM)
  all_RPM$Reported.Prime.Mover<-ifelse(all_RPM$Reported.Prime.Mover %in% c("CS","CA","CT"),"NGCC",all_RPM$Reported.Prime.Mover)
  all_RPM<-all_RPM[!duplicated(all_RPM),]
  all_ST<-all_RPM %>% filter(Reported.Prime.Mover=="ST")
  all_NGCC<-all_RPM %>% filter(Reported.Prime.Mover=="NGCC")
  
  #put page3.4 and page 1 together
  plantChar<-rbind(plantDom,plantDom_page1)
  
  #add NetGen
  combogencoo_paired_cooling<-prepCoolType(sheet4_key, combogencoo_key, combogencoo_cooly_type)
  netGen<-applyCoolTypes_plant(coolType_df = combogencoo_paired_cooling,
                       plantData = bogen_netgen,
                       coolTypes = c("tower","lake","pond","river","OS"),
                       colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS"),
                       keepCols_cool = c("combogencoo","bogen"),
                       keepCols_data<-c("Net.Generation.Year.To.Date"),
                       joinCol = "bogen",
                       sumCols=c("Net.Generation.Year.To.Date"))
  netGen<-netGen %>% select(-flagComplex)
  netGen$Plant_id<-paste(netGen$Plant.Code,netGen$cooling,netGen$percentAllocation,sep="^")
  netGen<-findComplexPlants2(netGen,results=F)
  netGen<-splitPlant_id(netGen)
  netGen$flagComplex<-ifelse(netGen$percentAllocation!=100 | netGen$flag_minMax==TRUE,TRUE,FALSE)
  netGenComplex<-netGen %>% dplyr::group_by(Plant.Code,flagComplex) %>%
    dplyr::summarise(Net.Generation.Year.To.Date=max(Net.Generation.Year.To.Date,na.rm = T))
  netGenComplex<-netGenComplex %>% filter(flagComplex==TRUE)
  netGenComplex$cooling<-rep("complex",nrow(netGenComplex))
  netGenComplex$percentAllocation<-rep(NA,nrow(netGenComplex))
  netGen<-netGen %>% filter(flagComplex==FALSE) %>% select(-flag_minMax)
  netGenComplex<-netGenComplex %>% select(all_of(names(netGen)))
  
  plantCharComplex<-plantChar %>% filter(flagComplex==TRUE)
  plantCharComplex$cooling<-rep("complex",nrow(plantCharComplex))
  plantCharComplex$percentAllocation<-rep(NA,nrow(plantCharComplex))
  plantCharComplex<-replaceMultiples(plantCharComplex,
                                     groupCols=c("Plant.Code","cooling","percentAllocation","flagComplex"),
                                     countVar="general_mover",
                                     repStr="complex")
  plantCharComplex<-replaceMultiples(plantCharComplex,
                                     groupCols=c("Plant.Code","cooling","percentAllocation","flagComplex"),
                                     countVar="dom_fuel",
                                     repStr="complex")
  plantCharComplex<-replaceMultiples(plantCharComplex,
                                     groupCols=c("Plant.Code","cooling","percentAllocation","flagComplex"),
                                     countVar="Plant.level_dom_fuel",
                                     repStr="complex")
  plantCharComplex<-plantCharComplex[!duplicated(plantCharComplex),]
  plantCharComplex_justChar<-plantCharComplex
  plantCharComplex<-left_join(plantCharComplex,netGenComplex,
                              by=c("Plant.Code","cooling","percentAllocation","flagComplex"))
 
  
  plantChar2<-inner_join(plantChar,netGen,by=c("Plant.Code","cooling","percentAllocation","flagComplex"))
  plantChar2<-rbind(plantChar2,plantCharComplex)
  plantChar2<-plantChar2 %>% ungroup() %>% select(-flagComplex)
  
  #nukes
  combogencoo_cooly_type<-rbind.fill(combogencoo_cooly_type,combogencoo_cooly_type_nukes)
  nukes<-gen_fuel_data %>% filter(Nuclear.Unit.Id!="." & !is.na(Nuclear.Unit.Id))
  nukes$bogen<-paste(nukes$Plant.Code,nukes$Nuclear.Unit.Id,sep="^")
  nukes<-nukes %>% select(Plant.Code,bogen,Reported.Prime.Mover,starts_with("Netgen"))
  #get annual netgen
  nukes <- data.frame(nukes[,names(nukes)[names(nukes) %in% 
                                                c("Plant.Code","bogen","Reported.Prime.Mover")]], 
                                sapply(nukes[,names(nukes)[regexpr("Netgen",names(nukes))>0]],
                                       function(x) {y<-ifelse(x > 0, x, 0)}))
  nukes$Net.Generation.Year.To.Date<-rowSums(nukes[,names(nukes)[regexpr("Netgen",names(nukes))>0]],na.rm = TRUE)
  nukes<-nukes %>% select(Plant.Code,bogen,Reported.Prime.Mover,Net.Generation.Year.To.Date)
  #get cooltypes
  nuke_coo_type<-inner_join(nuke_combogencoo_key,combogencoo_cooly_type, by= c("combogencoo" ="bogencoo"))
  nuke_coo_type$bogen<-gsub("gen\\^","",nuke_coo_type$plant_unit)
  if (nrow(nuke_coo_type)!=0){
  nukesChar<-nukes %>% select(Plant.Code,bogen,Reported.Prime.Mover)
  nukesChar$general_mover<-ifelse(nukesChar$Reported.Prime.Mover %in% c("CS","CA","CT"),"NGCC",
                                  nukesChar$Reported.Prime.Mover)

  nukes<-applyCoolTypes_plant(coolType_df = nuke_coo_type,
                               plantData = nukes,
                               coolTypes = c("tower","lake","pond","river","OS"),
                               colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS"),
                               keepCols_cool = c("combogencoo","bogen"),
                               keepCols_data<-c("Net.Generation.Year.To.Date"),
                               joinCol = "bogen",
                               sumCols=c("Net.Generation.Year.To.Date"))
  nukesChar<-applyCoolTypes_plant(coolType_df = nuke_coo_type,
                              plantData = nukesChar,
                              coolTypes = c("tower","lake","pond","river","OS"),
                              colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS"),
                              keepCols_cool = c("combogencoo","bogen"),
                              keepCols_data<-c("general_mover"),
                              joinCol = "bogen")
  nukesChar<-nukesChar[!duplicated(nukesChar),]
  # nukes<-inner_join(nukesChar %>% select(-flagComplex),
  #                   nukes %>% select(-flagComplex),
  #                   by=c("Plant.Code","cooling","percentAllocation"))
  
  nukes$dom_fuel<-rep("nuclear",nrow(nukes))
  nukes$Plant.level_dom_fuel<-rep("nuclear",nrow(nukes))
  
  #add nukes to non-nukes
  #nukes<-nukes %>% select(all_of(names(plantChar2)))
  nukes$Plant_id<-paste(nukes$Plant.Code,nukes$cooling,nukes$percentAllocation,sep="^")
  nukes<-nukes %>% select(Plant_id,Net.Generation.Year.To.Date)
  nukes<-findComplexPlants2(nukes,results=F)
  nukes<-splitPlant_id(nukes)
  nukes$flagComplex<-ifelse(nukes$percentAllocation!=100 | nukes$flag_minMax==TRUE,TRUE,FALSE)
  if (nrow(nukes %>% filter(flagComplex==TRUE))!=0){
  nukesComplex<-nukes %>% dplyr::group_by(Plant.Code,flagComplex) %>%
    dplyr::summarise(Net.Generation.Year.To.Date=max(Net.Generation.Year.To.Date,na.rm = T),.groups="drop")
  nukesComplex<-nukesComplex %>% filter(flagComplex==TRUE)
  nukesComplex$cooling<-rep("complex",nrow(nukesComplex))
  nukesComplex$percentAllocation<-rep(NA,nrow(nukesComplex))
  nukes<-nukes %>% filter(flagComplex==FALSE) %>% select(-flag_minMax)
  nukesComplex<-nukesComplex %>% select(all_of(names(nukes)))
  
  nukesCharComplex<-nukesChar %>% filter(flagComplex==TRUE)
  nukesCharComplex$cooling<-rep("complex",nrow(nukesCharComplex))
  nukesCharComplex$percentAllocation<-rep(NA,nrow(nukesCharComplex))
  nukesCharComplex<-replaceMultiples(nukesCharComplex,
                                     groupCols=c("Plant.Code","cooling","percentAllocation","flagComplex"),
                                     countVar="general_mover",
                                     repStr="complex")
  nukesCharComplex<-nukesCharComplex[!duplicated(nukesCharComplex),]
  nukesCharComplex<-inner_join(nukesCharComplex,nukesComplex,
                               by=c("Plant.Code","cooling","percentAllocation","flagComplex"))
  
  nukes2<-inner_join(nukesChar,nukes,
                    by=c("Plant.Code","cooling","percentAllocation","flagComplex"))
  nukes2<-nukes2 %>% select(all_of(names(nukesCharComplex)))
  nukes2<-rbind(nukes2,nukesCharComplex)
  nukes2$dom_fuel<-rep("nuclear",nrow(nukes2))
  nukes2$Plant.level_dom_fuel<-rep("nuclear",nrow(nukes2))
  nukes2<-nukes2 %>% ungroup() %>% select(-flagComplex)
  nukes2<-nukes2 %>% select(all_of(names(plantChar2)))
  }else{#no complex nukes
    nukes<-inner_join(nukesChar %>% select(-flagComplex),
                      nukes %>% select(-flagComplex,-flag_minMax),
                      by=c("Plant.Code","cooling","percentAllocation"))
    nukes$dom_fuel<-rep("nuclear",nrow(nukes))
    nukes$Plant.level_dom_fuel<-rep("nuclear",nrow(nukes))
    nukes2<-nukes %>% select(all_of(names(plantChar2)))
    }
  
  plantChar2<-rbind(plantChar2,nukes2)
  }#if nuke in cool type
  
  plantChar2<-replaceMultiples(plantChar2,
                                     groupCols=c("Plant.Code"),
                                     countVar="Plant.level_dom_fuel",
                                     repStr="complex")
  plantChar2<-replaceMultiples(plantChar2,
                                     groupCols=c("Plant.Code","cooling","percentAllocation"),
                                     countVar="general_mover",
                                     repStr="complex")
  
  #add plant_info vars
  plantInfo<-plantInfo %>% select(Plant.Code,Plant.Name,County,State,Name.of.Water.Source)
  plantChar3<-left_join(plantChar2,plantInfo,by="Plant.Code")
  plantChar3<-plantChar3 %>% select(all_of(names(plantInfo)),all_of(names(plantChar2)))

  plantChar3<-plantChar3 %>% filter(Plant.Code %in% plantList)
  return(plantChar3)
}