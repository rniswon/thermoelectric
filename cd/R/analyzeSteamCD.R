#' @title analyzeSteamCD
#' @description calculates monthly condenser duty for stream plants by
#'              bogen
#' @param bogen_netgen_df monthly net generation by bogen, output of
#'                        the assignBogen() function
#' @param bogen_steam_heat_df monthly steam heat by bogen, output of
#'                            the assignBogen() function
#' @param bogen_nom_loss_df monthly nominal losses by bogen, output of
#'                          the assignBogen()
#' @return `final_st_cd` data.frame containing monthly condenser duty
#'                       for stream plants by bogen
#' @examples
#' analyzeSteamCD(
#'   data_by_bogen$bogen_netgen,
#'   data_by_bogen$bogen_steam_heat,
#'   data_by_bogen$bogen_nom_loss
#' )

analyzeSteamCD <- function(
  bogen_netgen_df, bogen_steam_heat_df, bogen_nom_loss_df
  ) {
  st_bogen_netgen <- bogen_netgen_df %>%
    filter(Reported.Prime.Mover == "ST") %>%
    group_by(Plant.Code, bogen, Reported.Prime.Mover) %>%
    summarize_at(names(.)[7:18], sum, na.rm = T)

  bogen_sh <- bogen_steam_heat_df %>%
    group_by(Plant.Code, bogen, Reported.Prime.Mover) %>%
    summarize_at(names(.)[30:41], sum, na.rm = T)

  st_nom_los <- bogen_nom_loss_df %>%
    filter(Reported.Prime.Mover == "ST") %>%
    group_by(Plant.Code, bogen, Reported.Prime.Mover) %>%
    summarize_at(names(.)[5:16], sum, na.rm = T)

  cd_cal_table <- left_join(bogen_sh, st_nom_los)

  cd_cal_table_2 <- inner_join(cd_cal_table, st_bogen_netgen)
  cd_cal_table_2 <- cd_cal_table_2[!duplicated(cd_cal_table_2), ]

  st_cd <- calSTCondenserDuty(
    cd_cal_table_2[4:15], cd_cal_table_2[28:39], cd_cal_table_2[16:27]
  )

  cd_cal_table_2 <- ungroup(cd_cal_table_2)
  final_st_cd <- as.data.frame(cbind(cd_cal_table_2[1:3], st_cd))
  final_st_cd$bogen_total <- rowSums(final_st_cd[4:15], na.rm = T)
  final_st_cd[4:16] <- round(final_st_cd[4:16], 0)
  final_st_cd <- final_st_cd %>%
    group_by(Plant.Code, bogen) %>%
    summarize_at(names(.)[4:16], sum, na.rm = T)

  return(final_st_cd)
}
