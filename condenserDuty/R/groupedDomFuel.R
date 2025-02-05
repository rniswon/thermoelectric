#'@title groupedDomFuel
#'@description 
#'@param df
#'@param groupCols
#'@param domGroup
#'@param naString
#'@param dom_thrsh
#'@param maj_thrsh
#'@param nameTotal
#'@param fuel.codes
#'
#'@export


groupedDomFuel<-function(df,groupCols,domGroup,naString="other",
                         dom_thrsh,maj_thrsh,nameTotal,fuel.codes){

  #fuel_specific_total per fuel category
  str<-paste0(" df <- df %>% dplyr::group_by(",paste(groupCols,collapse=","),",Fuel.Category) %>%
              dplyr::mutate(fuel_specific_total_category = sum(fuel_specific_total, na.rm = T))")
  eval(parse(text=str))
  #get nameTotal
  str<-paste0(" df <- df %>% dplyr::group_by(",paste(groupCols,collapse=","),") %>%
              dplyr::mutate(",nameTotal," = sum(fuel_specific_total, na.rm = T)) %>%
    dplyr::mutate(fuel_pct = fuel_specific_total_category / ",nameTotal,")")
  eval(parse(text=str))
  
  #Flag dominant fuels, majority of boiler fuel, and fuels with zero consmption
  df$fuel_dominance <- sapply(df$fuel_pct, calFuelDominance, dom_thrsh = dom_thrsh, maj_thrsh = maj_thrsh)
  
  #Assign dominant fuel codes
  df$dom_fuel <- as.character(length(df$Plant.Code))
  for(x in 1:length(df$fuel_dominance)){
    df$dom_fuel[x] <- assignDominance(df$Reported.Fuel.Type.Code[x], df$fuel_dominance[x], fuel_code_list = fuel.codes)
  }
  
  df$dom_fuel[is.na(df$dom_fuel)] <- lookup(df$Reported.Fuel.Type.Code[is.na(df$dom_fuel)], fuel.codes )
  
  #replace dom_fuel with dominant fuel type where fuel_dominance=="dominant" else other
  str<-paste0("domFuels<-df %>% ungroup() %>% select(",paste(domGroup,collapse=","),
  ",dom_fuel,fuel_dominance)")
  eval(parse(text=str))
  
  domFuels<-domFuels %>% filter(fuel_dominance=="dominant")
  domFuels<-domFuels[!duplicated(domFuels),]
  df<-left_join(df %>% select(-dom_fuel),
                                domFuels %>% select(-fuel_dominance), by=domGroup)
  df$dom_fuel<-ifelse(is.na(df$dom_fuel),naString,df$dom_fuel)
  
  return(df)
  
}