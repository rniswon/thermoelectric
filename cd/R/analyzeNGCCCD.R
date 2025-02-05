#' @title analyzeNGCCCD
#' @description calculates monthly condenser duty for general_mover =
#'     NGCC by bogen Executed By: non_nuke_condenser_duty.R
#' @param bogen_netgen_df monthly net generation by bogen, output of
#'     the assignBogen() function
#' @param bogen_fuel_heat_df monthly fuel heat by bogen, output from
#'     assignBogen() function
#' @param bogen_nom_loss_df monthly nominal losses by bogen, output of
#'     the assignBogen()
#' @return `final_ngcc_cd` data.frame containing monthly condenser
#'     duty for general_mover = NGCC by bogen
#' @examples analyzeNGCCCD(
#'             data_by_bogen$bogen_netgen,
#'             data_by_bogen$bogen_fuel_heat,
#'             data_by_bogen$bogen_nom_loss
#'           )

# Function for analyzing NGCC bogen CD
analyzeNGCCCD <- function(
  bogen_netgen_df, bogen_fuel_heat_df, bogen_nom_loss_df
  ) {
  ngcc_bogen_netgen <- bogen_netgen_df %>%
    filter(Reported.Prime.Mover != "ST") %>%
    group_by(Plant.Code, bogen) %>%
    summarize_at(names(.)[7:18], sum, na.rm = T)

  bogen_fh <- bogen_fuel_heat_df %>%
    filter(Reported.Prime.Mover != "ST") %>%
    group_by(Plant.Code, bogen) %>%
    summarize_at(names(.)[5:16], sum, na.rm = T)

  colnames(bogen_fh)[3:14] <-
    paste("fuel_heat", colnames(bogen_fh)[3:14], sep = "_")

  ngcc_nom_los <- bogen_nom_loss_df %>%
    filter(Reported.Prime.Mover != "ST") %>%
    group_by(Plant.Code, bogen) %>%
    summarize_at(names(.)[5:16], sum, na.rm = T)

  colnames(ngcc_nom_los)[3:14] <-
    paste("nom_loss", colnames(ngcc_nom_los)[3:14], sep = "_")

  cd_cal_table <- left_join(bogen_fh, ngcc_nom_los)

  cd_cal_table_2 <- inner_join(cd_cal_table, ngcc_bogen_netgen)
  cd_cal_table_2 <- cd_cal_table_2[!duplicated(cd_cal_table_2), ]

  ngcc_cd <-
    calNGCCCondenserDuty(
      cd_cal_table_2[3:14], cd_cal_table_2[27:38], cd_cal_table_2[15:26]
    )

  cd_cal_table_2 <- ungroup(cd_cal_table_2)
  final_ngcc_cd <- as.data.frame(cbind(cd_cal_table_2[1:2], ngcc_cd))
  final_ngcc_cd$bogen_total <- rowSums(final_ngcc_cd[3:14], na.rm = T)
  final_ngcc_cd[3:15] <- round(final_ngcc_cd[3:15], 0)
  final_ngcc_cd <- final_ngcc_cd %>%
    group_by(Plant.Code, bogen) %>%
    summarize_at(names(.)[3:15], sum, na.rm = T)
  return(final_ngcc_cd)
}
