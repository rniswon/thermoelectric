#' @title SteamHeatanalysis
#' @author Lillian Gorman Sanisaca
#' @author Andrew Halper
#' @description calculates monthly steam heat by plant and boiler, for
#'              output file `final_steam_heat.csv`
#' Executed By: non_nuke_condenser_duty.R
#' @param ST_fuel_heat_df data.frame subset of fuel_heat_table.csv,
#'                        with only Reported.Prime.Mover == "ST"
#' @param boilerDesignData data.frame 2015 Form EIA-860 Data -
#'                         Schedule 6C, "Boiler Information - Design
#'                         Parameters", input file
#'                         `2015_BoilerDesignInfo.csv`
#' @param pub_efficiency list of published weighted boiler
#'                       efficiencies defined as
#'                         list(
#'                           "0.781596394425121" = "biomass",
#'                           "0.873790282218364" = "coal",
#'                           "0.861342667396523" = "gas",
#'                           "0.886779729944024" = "oil"
#'                         )
#' @param use_published_efficiencies TRUE/FALSE indicating whether or
#'                                   not to use published
#'                                   efficiencies; default value TRUE
#' @param page3 a logical indicating whether page 3 boilerFuelData is
#'              used as input; if FALSE page 1 gen_fuel_data is used
#' @return `steam_heat_results` data.frame monthly steam heat by plant
#'                              and boiler
#' @examples
#' SteamHeatanalysis(
#'   ST_fuel_heat_df,
#'   boiler_design_data,
#'   pub_efficency_table,
#'   use_published_efficiencies
#' )

SteamHeatanalysis <- function(ST_fuel_heat_df, boilerDesignData, pub_efficiency,
                              use_published_efficiencies = T, page3) {
  init_steam_heat_results <-
    estEfficiency(ST_fuel_heat_df, boilerDesignData, page3)
  list2env(init_steam_heat_results, environment())

  if (page3) {
    monthindex <- c(5:16)
    outIndex <- c(30:41)
    bygroup <- c("Plant.Code", "Boiler.ID")
  } else {
    monthindex <- c(4:15)
    outIndex <- c(29:40)
    bygroup <- c("Plant.Code", "Reported.Prime.Mover")
  }

  temp_steam_heat$boiler_efficiency <-
    ifelse(!is.na(temp_steam_heat$boiler_efficiency) &
      (temp_steam_heat$boiler_efficiency < 0.75 |
        temp_steam_heat$boiler_efficiency > 0.94),
    NA, temp_steam_heat$boiler_efficiency
    )

  if (use_published_efficiencies) {
    missing_efficiency <- temp_steam_heat %>%
      filter(is.na(boiler_efficiency))
    missing_efficiency$boiler_efficiency[is.na(
      missing_efficiency$boiler_efficiency
    )] <-
      as.numeric(
        as.character(
          lookup(missing_efficiency$dom_fuel, pub_efficiency)
        )
      )
    missing_efficiency[outIndex] <-
      as.data.frame(
        apply(
          missing_efficiency[monthindex],
          2,
          calSteamHeat,
          boiler_efficiency = missing_efficiency$boiler_efficiency
        )
      )
    steam_heat_results <-
      rbind(
        temp_steam_heat[!is.na(temp_steam_heat$boiler_efficiency), ],
        missing_efficiency
      )

    eval(
      parse(
        text = paste0(
          "steam_heat_results <- steam_heat_results %>%\n",
          "ungroup() %>%\n",
          "dplyr::mutate(fuel_specific_total_steam_heat = ",
          "rowSums(select(., contains('steam_heat_')), na.rm = T)) %>%\n",
          "dplyr::group_by(", paste(bygroup, collapse = ","), ") %>%\n",
          "dplyr::mutate(boiler_total_steam_heat = ",
          "  sum(fuel_specific_total_steam_heat, na.rm=T)) %>%\n",
          "dplyr::mutate(fuel_pct_total_steam_heat = ",
          "  fuel_specific_total_steam_heat / boiler_total_steam_heat) %>%\n",
          "ungroup()"
        )
      )
    )
  } else if (use_published_efficiencies == F) {
    missing_efficiency <- temp_steam_heat %>%
      filter(is.na(boiler_efficiency))
    missing_efficiency$boiler_efficiency[is.na(
      missing_efficiency$boiler_efficiency
      )] <-
      as.numeric(
        as.character(lookup(missing_efficiency$dom_fuel, est.efficiencies))
      )
    missing_efficiency[outIndex] <-
      as.data.frame(
        apply(
          missing_efficiency[monthindex],
          2,
          calSteamHeat,
          boiler_efficiency = missing_efficiency$boiler_efficiency
        )
      )
    steam_heat_results <-
      rbind(
        temp_steam_heat[!is.na(temp_steam_heat$boiler_efficiency), ],
        missing_efficiency
      )

    eval(
      parse(
        text = paste0(
          "steam_heat_results <- steam_heat_results %>%\n",
          "ungroup() %>%\n",
          "dplyr::mutate(fuel_specific_total_steam_heat = ",
          "  rowSums(select(., contains('steam_heat_')), na.rm = T)) %>%\n",
          "dplyr::group_by(\", paste(bygroup, collapse = \",\"), \") %>%\n",
          "dplyr::mutate(boiler_total_steam_heat = ",
          "  sum(fuel_specific_total_steam_heat, na.rm=T)) %>%\n",
          "dplyr::mutate(fuel_pct_total_steam_heat = ",
          "  fuel_specific_total_steam_heat / boiler_total_steam_heat) %>%\n",
          "ungroup()"
        )
      )
    )
  }

  if (!page3) {
    names(steam_heat_results)[names(steam_heat_results) %in% c(
      "boiler_efficiency",
      "boiler_total_fuel_heat",
      "boiler_total_steam_heat"
    )] <- c(
      "efficiency",
      "RPM_total_fuel_heat",
      "RPM_total_steam_heat"
    )
  }
  return(steam_heat_results)
}