#'@title analyzeFuelHeat
#'@description Calculates annual fuel-specific fuel heat, and boiler total fuel heat at each boiler, 
#'             flags dominant fuels, majority of boiler fuel, and fuels with zero consmption, 
#'             assigns dominant fuel codes.  Returns object for output files fuel_heat_table.csv and 
#'             fuel_heat_ST_only.csv. \cr \cr
#'Executed By: non_nuke_condenser_duty.R \cr
#'@param plantList vector of all plant codes from 2015, input file 2015_Plants.csv
#'@param data data.frame with monthly Quantity.of.Fuel.Consumed and MMBTu_per_unit, names may vary
#'@param dom_thrsh numeric threshold for dominant fuels
#'@param maj_thrsh numeric threshold for boiler majority fuel
#'@param fuel.codes Fuel generalization list defined as 
#'                  `list(biomass = c("AB",'BLQ','MSB','MSN','OBL','OBS','SLW','TDF','WDS'),
#'                        coal = c("ANT","BIT","LIG","PC","SUB","WC"),
#'                        gas = c("BFG","LFG","NG","OBG","OG","PG","SGC","SGP"),
#'                        oil = c("DFO","JF","KER","RFO","WO"),
#'                        other = c("OTH"))`
#'@param page3 logical indicating whether page 3 boilerFuelData is used as input, if FALSE page 1 gen_fuel_data is used                       
#'@return `raw_fuel_heat_df_2` annual fuel-specific fuel heat, and boiler total fuel heat at each boiler, 
#'                             flags dominant fuels, majority of boiler fuel, and fuels with zero consmption, 
#'                             assigns dominant fuel codes
#'@examples
#'analyzeFuelHeat(plantList, boilerFuelData, dom_thrsh, maj_thrsh, fuel.codes)


#Functions for calculating fuel_heat

# #Fuel generalization list
# fuel.codes <- list(
#   biomass = c("AB",'BLQ','MSB','MSN','OBL','OBS','SLW','TDF','WDS'),
#      coal = c("ANT","BIT","LIG","PC","SUB","WC"),
#       gas = c("BFG","LFG","NG","OBG","OG","PG","SGC","SGP"),
#       oil = c("DFO","JF","KER","RFO","WO"),
#     other = c("OTH"))


analyzeFuelHeat <- function(plantList, data, dom_thrsh, maj_thrsh,
                            fuel.codes,sheet3_key,sheet4_key,combogencoo_key, 
                            combogencoo_cooling_type_df,page3=FALSE){
  select_RPM=c("CA", "CS", "CT", "ST",NA)
  months <- c("january","february","march","april",
            "may", "june", "july", "august",
            "september", "october", "november", "december")  
  
fuel_heat_id_cols <- c("Plant.Code", "Boiler.ID", "Reported.Prime.Mover", "Reported.Fuel.Type.Code")
Qnames<-c("Quantity.Of.Fuel.Consumed.January",
          "Quantity.Of.Fuel.Consumed.February",
          "Quantity.Of.Fuel.Consumed.March",
          "Quantity.Of.Fuel.Consumed.April",
          "Quantity.Of.Fuel.Consumed.May",
          "Quantity.Of.Fuel.Consumed.June",
          "Quantity.Of.Fuel.Consumed.July",
          "Quantity.Of.Fuel.Consumed.August",
          "Quantity.Of.Fuel.Consumed.September",
          "Quantity.Of.Fuel.Consumed.October",
          "Quantity.Of.Fuel.Consumed.November",
          "Quantity.Of.Fuel.Consumed.December")
Mnames<-c("MMbtu.Per.Unit.January",
          "MMbtu.Per.Unit.February",
          "MMbtu.Per.Unit.March",
          "MMbtu.Per.Unit.April",
          "MMbtu.Per.Unit.May",
          "MMbtu.Per.Unit.June",
          "MMbtu.Per.Unit.July",
          "MMbtu.Per.Unit.August",
          "MMbtu.Per.Unit.September",
          "MMbtu.Per.Unit.October",
          "MMbtu.Per.Unit.November",
          "MMbtu.Per.Unit.December")

#change names if Page 1 data
if (!page3){
  Qnames<-gsub("Of.Fuel.Consumed.","",Qnames)
  Mnames<-gsub("btu.Per.","BtuPer_",Mnames)
  fuel_heat_id_cols<-fuel_heat_id_cols[fuel_heat_id_cols!="Boiler.ID"]
}

#subset data by PLantList
data <- data %>% subset(Plant.Code %in% plantList)

#calculate Fuel Heat
fuel_heat_df <- as.data.frame(mapply(calFuelHeat, data[Qnames], 
                                     data[Mnames]), stringsAsFactors = F)
#change colnames to months
colnames(fuel_heat_df) <- months 

#combine ID cols and FuelHeat
raw_fuel_heat_df <- cbind(data[fuel_heat_id_cols], fuel_heat_df)

if (page3){
#add plant^bo column
raw_fuel_heat_df$plant_bo <- paste(raw_fuel_heat_df$Plant.Code, raw_fuel_heat_df$Boiler.ID, sep="^")
}else{
  #sub Reported.Prime.Mover in for Boiler.ID in page 1 data
  raw_fuel_heat_df$plant_bo <- paste(raw_fuel_heat_df$Plant.Code, raw_fuel_heat_df$Reported.Prime.Mover, sep="^")
}

#Calculate annual fuel-specific fuel heat, and boiler total fuel heat at each boiler
#if page 1 sub Reported.Prime.Mover for boiler
raw_fuel_heat_df_2 <- raw_fuel_heat_df %>% 
  dplyr::mutate(fuel_specific_total = rowSums(select(., months), na.rm = T))

if(page3){
raw_fuel_heat_df_3<-joinBogen(tbl=raw_fuel_heat_df_2,
                         key=sheet3_key %>% filter(regexpr("NGCC",bogen)<0 & regexpr("ST",bogen)<0),
                         joinCols=c("plant_bo","plant_bo_bf.923"))
}else{
  key_page1<-sheet3_key %>% filter(regexpr("NGCC",bogen)>0 | regexpr("ST",bogen)>0)
  key_page1$plant_RPM<-paste(key_page1$Plant.Code,key_page1$Reported.Prime.Mover_page1,sep="^")
  #key_page1$plant_RPM.mod<-paste(key_page1$Plant.Code,key_page1$Reported.Prime.Mover_page1,sep="^")
  key_page1$plant_RPM.mod<-ifelse(key_page1$Reported.Prime.Mover_page1 %in%
           c("CS","CT","CA","NGCC"),paste(key_page1$Plant.Code,"NGCC",sep="^"),
           paste(key_page1$Plant.Code,"ST",sep="^"))
  page1<-raw_fuel_heat_df_2 %>% filter(Reported.Prime.Mover %in% select_RPM)
  page1$plant_RPM<-paste(page1$Plant.Code,page1$Reported.Prime.Mover,sep="^")
  page1$mod.Reported.Prime.Mover<-ifelse(page1$Reported.Prime.Mover %in%
                                           c("CS","CT","CA"),"NGCC",
                                         page1$Reported.Prime.Mover)
  page1$plant_RPM.mod<-paste(page1$Plant.Code,page1$mod.Reported.Prime.Mover,sep="^")
  
  raw_fuel_heat_df_3<-joinBogen(tbl=page1 %>% filter(Plant.Code %in% key_page1$Plant.Code),
                         key=key_page1,
                         joinCols=c("plant_RPM","plant_RPM.mod"))
  raw_fuel_heat_df_3<-raw_fuel_heat_df_3 %>% filter(!is.na(bogen))
  
}#end page 1

bogen_FuelHeat<-raw_fuel_heat_df_3
#get Fuel Category
raw_fuel_heat_df_3 <- raw_fuel_heat_df_3 %>%
  dplyr::mutate(Fuel.Category = lookup(Reported.Fuel.Type.Code, fuel.codes ))


raw_fuel_heat_df_3 <- raw_fuel_heat_df_3 %>% 
  dplyr::mutate(Fuel.Category = lookup(Reported.Fuel.Type.Code, fuel.codes ))
#assign fuel Dominance by bogen
raw_fuel_heat_df_3<-groupedDomFuel(raw_fuel_heat_df_3,groupCols=c("Plant.Code","bogen"),
                                   domGroup="bogen",
                                   naString="other",dom_thrsh,maj_thrsh,
                                   nameTotal="bogen_total",fuel.codes)



# raw_fuel_heat_df_2<-raw_fuel_heat_df_3
  # dplyr::group_by(Plant.Code, plant_bo) %>% 
  # dplyr::mutate(boiler_total = round(sum(fuel_specific_total, na.rm=T),0)) %>% 
  # dplyr::mutate(fuel_pct = fuel_specific_total / boiler_total)
#get plant-cooling-level dominant fuel
plantDom<-bogen_FuelHeat
plantDom <- bogen_FuelHeat %>%
  dplyr::mutate(Fuel.Category = lookup(Reported.Fuel.Type.Code, fuel.codes ))
#join with cooling_type
combogencoo_paired_cooling<-prepCoolType(sheet4_key, combogencoo_key, 
                                         combogencoo_cooling_type_df)
plantDom<-applyCoolTypes_plant(coolType_df = combogencoo_paired_cooling,
                               plantData = plantDom,
                               coolTypes = c("tower","lake","pond","river","OS"),
                               colNames = c("TOWER..RF..RI..RN.","LAKE..OF..OC..RC.","POND..OC..RC.","RIVER..OF.","OS"),
                               keepCols_cool = c("combogen","combogencoo","bogen"),
                               keepCols_data<-c("Fuel.Category","fuel_specific_total","Reported.Fuel.Type.Code","Reported.Prime.Mover"),
                               joinCol = "bogen")
#assign fuel dominance by cooling
plantDom<-groupedDomFuel(plantDom,groupCols=c("Plant.Code","cooling","percentAllocation","flagComplex"),
                         domGroup=c("Plant.Code","cooling","percentAllocation","flagComplex"),
                         naString="complex",dom_thrsh,maj_thrsh,
                         nameTotal="cooling_total",fuel.codes)
#assign general mover
plantDom$general_mover<-ifelse(plantDom$Reported.Prime.Mover %in% c("CS","CA","CT"),"NGCC",
                               plantDom$Reported.Prime.Mover)
#get plantLevel dom_fuel
plantDomtotal<-groupedDomFuel(plantDom,groupCols=c("Plant.Code"),
                              domGroup=c("Plant.Code"),
                              naString="complex",dom_thrsh,maj_thrsh,
                              nameTotal="plant_total",fuel.codes)
plantDomtotal$Plant.level_dom_fuel<-plantDomtotal$dom_fuel
plantDomtotal<-plantDomtotal %>% select(Plant.Code,Plant.level_dom_fuel)

plantDom<-left_join(plantDom,plantDomtotal, by="Plant.Code")

plantDom<-plantDom %>% select(Plant.Code,cooling,percentAllocation,dom_fuel,Plant.level_dom_fuel,general_mover)

plantDom<-plantDom[!duplicated(plantDom),]

if (!page3){
  #rename boiler specific columns
  raw_fuel_heat_df_3<-raw_fuel_heat_df_3[,!names(raw_fuel_heat_df_3) %in% c("plant_bo","mod.Reported.Prime.Mover","plant_RPM.mod")]
  assign("plantDom_page1",plantDom,envir = .GlobalEnv)
  }else{
  assign("plantDom",plantDom,envir = .GlobalEnv)
}

return(raw_fuel_heat_df_3)
}




