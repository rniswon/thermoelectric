### Make monthly Gridmet data for 2010 - 2015
### Melissa Lombard March 2021

setwd("C:/Workspace/Documents/ThermoElectric/Climate_data")

options(scipen = 999) #gets rid of scientific notation

library(readr)
library(dplyr)
library(purrr)
library(lubridate)

#read one file to get column headings and format
#d2010 <- read_csv("gridMET_at_TE_locations_2010.csv") #look at one file
#head(d2010)
#rm(d2010)

#import all files into one dataframe 
#make sure working directory only has csv files that you want to combine
all<-list.files(pattern = "*.csv") %>%
  map_df(~read_csv(., col_types = cols(
    EIA_P_DATE = col_character(),
    EIA_PLANT_ = col_character(),
    date = col_date(format = "['%Y', '%m', '%d']"),
    rh_avg = col_double(),
    air_tmp_C_avg = col_double(),
    open_wtr_et_mm = col_double(),
    wb_tmp_C = col_double(),
    rh_max = col_double(),
    rh_min = col_double(),
    sp_h_kg_kg = col_double(),
    air_tmn_K = col_double(),
    air_tmx_K = col_double(),
    precip_mm = col_double(),
    wnd_spd_m_s = col_double(),
    wnd_dir_dg_clk_frm_N = col_double(),
    ref_et_gr_mm = col_double(),
    ref_et_alf_mm = col_double(),
    sur_rad_Wm2 = col_double(),
    vpdef_kPa = col_double()
  )))
  
head(all)
tail(all$EIA_P_DATE)

all$month <- month(all$date, label = T, abbr = T)
names(all)

monthly <- all %>% group_by_at(vars(EIA_PLANT_,`month`)) %>% mutate (rh_avg = mean(rh_avg, na.rm = T)) %>%
  mutate(air_tmp_C_avg = mean(air_tmp_C_avg, na.rm = T)) %>% mutate(open_wtr_et_mm = sum(open_wtr_et_mm, na.rm = T)) %>%
  mutate(wb_tmp_C = mean(wb_tmp_C, na.rm = T)) %>% mutate(rh_max = mean(rh_max, na.rm = T)) %>% 
  mutate(rh_min = mean(rh_min, na.rm = T)) %>% mutate(sp_h_kg_kg = mean(sp_h_kg_kg, na.rm = T)) %>% 
  mutate(air_tmn_K = mean(air_tmn_K, na.rm = T)) %>% mutate(air_tmx_K = mean(air_tmx_K, na.rm = T)) %>% 
  mutate(precip_mm = sum(precip_mm, na.rm = T)) %>% mutate(wnd_spd_m_s = mean(wnd_spd_m_s, na.rm = T)) %>%
  mutate(wnd_dir_dg_clk_frm_N = mean(wnd_dir_dg_clk_frm_N, na.rm = T)) %>% mutate(ref_et_gr_mm = sum(ref_et_gr_mm, na.rm = T)) %>%
  mutate(ref_et_alf_mm = sum(ref_et_alf_mm, na.rm = T)) %>% mutate(sur_rad_Wm2 = mean(sur_rad_Wm2, na.rm = T)) %>% 
  mutate(vpdef_kPa = mean(vpdef_kPa), na.rm = T) %>%
  distinct(`month`, .keep_all = T)

write_csv(monthly, "Gridmet_monthly_2010-2015.csv")
