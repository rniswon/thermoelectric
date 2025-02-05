#'@title assignDominance
#'@description Assigns dominant fuel codes \cr \cr
#'Executed By: \itemize{\item analyzeFuelHeat.R 
#'                        \item fuel_heat_analysis.R} \cr
#'@param fuel_code Fuel generalization code from list defined as 
#'                  `list(biomass = c("AB",'BLQ','MSB','MSN','OBL','OBS','SLW','TDF','WDS'),
#'                        coal = c("ANT","BIT","LIG","PC","SUB","WC"),
#'                        gas = c("BFG","LFG","NG","OBG","OG","PG","SGC","SGP"),
#'                        oil = c("DFO","JF","KER","RFO","WO"),
#'                        other = c("OTH"))`
#'@param fuel_dominance flagged dominant fuel, majority of boiler fuel, and fuels 
#'                      with zero consmption, assigns dominant fuel codes
#'@param fuel_code_list Fuel generalization list defined as 
#'                      `list(biomass = c("AB",'BLQ','MSB','MSN','OBL','OBS','SLW','TDF','WDS'),
#'                        coal = c("ANT","BIT","LIG","PC","SUB","WC"),
#'                        gas = c("BFG","LFG","NG","OBG","OG","PG","SGC","SGP"),
#'                        oil = c("DFO","JF","KER","RFO","WO"),
#'                        other = c("OTH"))`
#'@return `result` character string assigning the dominant fuel code
#'@examples
#'assignDominance(raw_fuel_heat_df_2$Reported.Fuel.Type.Code[x], 
#'                raw_fuel_heat_df_2$fuel_dominance[x], fuel_code_list = fuel_codes)

assignDominance <- function(fuel_code, fuel_dominance, fuel_code_list){
  if(is.na(fuel_dominance)){
    result <- NA
  } else if(fuel_dominance == "dominant"){
    result <- lookup(fuel_code, fuel_code_list)
  } else if(fuel_dominance == "boiler_majority"){
    result <- lookup(fuel_code, fuel_code_list)
  } else {
    result <- NA
  }
  return(result)
}