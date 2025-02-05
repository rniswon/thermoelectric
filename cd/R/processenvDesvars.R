
processenvDesvars <- function(climate_wbdb_df, tower_design_param){
colnames(climate_wbdb_df)[colnames(climate_wbdb_df) == "WeaSta"]  <- "station"

names(climate_wbdb_df) <- tolower(names(climate_wbdb_df))

wbdbDes_dist <- climate_wbdb_df %>% 
  select(-near_rank, -plant_id_1, -latitude, -longitude, -lat, -lon) %>% 
  mutate(dist_mi = near_dist / 1609.34) %>% 
  mutate(inv_dist = 1 / dist_mi)

names(wbdbDes_dist)

wbdbDes_dist <- wbdbDes_dist[order(wbdbDes_dist$plant_id),]
wbdbDES_est <- wbdbDes_dist %>% 
               group_by(plant_id) %>% 
               mutate(tdbw = weighted.mean(tdb,inv_dist)) %>% 
               mutate(twbw = weighted.mean(twb, inv_dist)) %>% 
               select(plant_id, station, near_dist, model_type,
                      tdb, twb, dist_mi, inv_dist, tdbw, twbw) %>% 
               ungroup()

names(tower_design_param) <- tolower(names(tower_design_param))
tower_params_1 <- tower_design_param[c(1,4,5)]
tower_params_2 <- wbdbDES_est[c(1, 9, 10)]
names(tower_params_2) <- names(tower_params_1)
results <- rbind(tower_params_2, tower_params_1)
return(results)
}
