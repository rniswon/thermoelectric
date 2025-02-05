#'@title estEfficiency
#'@description get reported boiler efficiencies and estimate boiler efficiencies \cr \cr
#'Executed By: steamHeatAnalysis.R \cr
#'@param ST_fuel_heat_df data.frame subset of fuel_heat_table.csv, with only 
#'                    Reported.Prime.Mover=="ST"
#'@param boilerDesignData data.frame 2015 Form EIA-860 Data - Schedule 6C, 
#'                             'Boiler Information - Design Parameters', input file 2015_BoilerDesignInfo.csv
#'@param page3 logical indicating whether page 3 boilerFuelData is used as input, if FALSE page 1 gen_fuel_data is used                       
#'@return `results` list of estimated boiler efficiencies and reported steam heat efficiencies
#'@export

estEfficiency <- function(ST_fuel_heat_df, boilerDesignData,page3){
if (page3){
  bygroup<-c("Plant.Code", "Boiler.ID")
  monthindex<-c(5:16)
}else{
  bygroup<-c("Plant.Code", "Reported.Prime.Mover")
  monthindex<-c(4:15)
}
  
  #get reported boiler efficiencies
  reported_eff <- boilerDesignData %>% 
    select(Plant.Code, Boiler.ID, Efficiency.100..Load) %>% 
    group_by(Plant.Code, Boiler.ID) %>% 
    filter(!is.na(Efficiency.100..Load))

  
  colnames(reported_eff)[colnames(reported_eff) == "Efficiency.100..Load"] <- "boiler_efficiency"
  
  if (page3){
  steam_heat_rpt_eff <- as.data.frame(left_join(ST_fuel_heat_df, reported_eff, by=c("Plant.Code", "Boiler.ID")))
  }else{#page 1
    steam_heat_rpt_eff<-ST_fuel_heat_df
    steam_heat_rpt_eff$boiler_efficiency<-as.numeric(rep(NA,nrow(steam_heat_rpt_eff)))
  }
  

  months <- c("january", "february", "march", "april",
             "may", "june", "july", "august",
             "september", "october", "november", "december")
  
  
  eval(parse(text=paste0("steam_heat_rpt_eff <- steam_heat_rpt_eff %>% 
    ungroup() %>% 
    dplyr::mutate(fuel_specific_total_fuel_heat = rowSums(select(., months), na.rm = T)) %>% 
    dplyr::group_by(",paste(bygroup,collapse=","),") %>% 
    dplyr::mutate(boiler_total_fuel_heat = sum(fuel_specific_total_fuel_heat, na.rm=T)) %>% 
    dplyr::mutate(fuel_pct_total_fuel_heat = fuel_specific_total_fuel_heat / boiler_total_fuel_heat) %>% 
    ungroup()")))
  
  names(steam_heat_rpt_eff)[monthindex] <- gsub('^([a-z])', '\\fuel_heat_\\1', names(steam_heat_rpt_eff)[monthindex])
  
  
  steam_heat_vals <- as.data.frame(apply(steam_heat_rpt_eff[monthindex], 2, calSteamHeat, boiler_efficiency = steam_heat_rpt_eff$boiler_efficiency))
  names(steam_heat_vals) <- gsub('fuel_heat_', 'steam_heat_', names(steam_heat_vals))
  
  reported_eff_steam_heat <- cbind(steam_heat_rpt_eff, steam_heat_vals)
  
  eval(parse(text=paste0("reported_eff_steam_heat <- reported_eff_steam_heat %>% 
    ungroup() %>% 
    dplyr::mutate(fuel_specific_total_steam_heat = rowSums(select(., contains('steam_heat_')), na.rm = T)) %>% 
    dplyr::group_by(",paste(bygroup,collapse=","),") %>% 
    dplyr::mutate(boiler_total_steam_heat = sum(fuel_specific_total_steam_heat, na.rm=T)) %>% 
    dplyr::mutate(fuel_pct_total_steam_heat = fuel_specific_total_steam_heat / boiler_total_steam_heat) %>% 
    ungroup()")))
  
  
  steam_heat_summary_by_fuel <- reported_eff_steam_heat %>% group_by(dom_fuel) %>%  summarize_at(.vars = "fuel_specific_total_steam_heat",sum)
  fuel_heat_summary_by_fuel <- reported_eff_steam_heat %>%  group_by(dom_fuel) %>% summarize_at(.vars = "fuel_specific_total_fuel_heat",sum)
  
  coal_est_eff <- steam_heat_summary_by_fuel$fuel_specific_total_steam_heat[steam_heat_summary_by_fuel$dom_fuel == "coal"] /
    fuel_heat_summary_by_fuel$fuel_specific_total_fuel_heat[fuel_heat_summary_by_fuel$dom_fuel == "coal"]
  
  gas_est_eff <- steam_heat_summary_by_fuel$fuel_specific_total_steam_heat[steam_heat_summary_by_fuel$dom_fuel == "gas"] /
    fuel_heat_summary_by_fuel$fuel_specific_total_fuel_heat[fuel_heat_summary_by_fuel$dom_fuel == "gas"]
  
  oil_est_eff <- steam_heat_summary_by_fuel$fuel_specific_total_steam_heat[steam_heat_summary_by_fuel$dom_fuel == "oil"] /
    fuel_heat_summary_by_fuel$fuel_specific_total_fuel_heat[fuel_heat_summary_by_fuel$dom_fuel == "oil"]
  
  biomass_est_eff <- steam_heat_summary_by_fuel$fuel_specific_total_steam_heat[steam_heat_summary_by_fuel$dom_fuel == "biomass"] /
    fuel_heat_summary_by_fuel$fuel_specific_total_fuel_heat[fuel_heat_summary_by_fuel$dom_fuel == "biomass"]
  
  est_boiler_effs <- list(biomass_est_eff = "biomass", coal_est_eff = "coal", gas_est_eff = "gas", oil_est_eff = "oil")
  
  results <- list(est_efficiencies = est_boiler_effs, temp_steam_heat = reported_eff_steam_heat)
  return(results)
}
