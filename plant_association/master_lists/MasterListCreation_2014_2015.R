# Kenneth Skinner
#R script to get EIA data and extract and format for master list creation.
#  Script works for years 2012 - 2015
#
#
library(tidyverse)
library(stringi)
library(lubridate) # Make working with dates easier
library(httr) # Perform HTTP requests (in this case used to get data from FACT API)
library(tidyjson) # Work with json objects in a tidy way. Useful for highly nested objects and "ragged" arrays and/or objects (varying lengths by document)
library(readxl) # Read data from xlsx files via read_excel()
library(openxlsx) # Create and write to formatted xlsx documents
library(dplyr)
library(purrr) # Use of partial and map functions
#
### Assign key values for data retrieval
eia_860_year <- 2014 # The  EIA data year (available at https://www.eia.gov/electricity/data/eia860/)
earliest_retirement_year <- 2000 # All units retired before this year are filtered out of the data
setwd("G:\\WU\\Data\\Thermo\\Crosswalk\\EIA\\EIA-860")
#eia_data_file <- str_glue("https://www.eia.gov/electricity/data/eia860/archive/xls/eia860{eia_860_year}.zip")
#
## Import EIA data
#This section downloads and imports data from EIA-860 for the year specified above. To manually download the data from EIA, visit the [EIA-860 data](https://www.eia.gov/electricity/data/eia860/). Select and download the latest year's ZIP file on the right-hand-side of the page. The files used in this analysis are "3_1_Generator_Y{year}.xlsx" and "6_1_EnviroAssoc_Y{year}.xlsx", with "2___Plant_Y{year}.xlsx" to get lat/long.
# Import plant, generator, and boiler (EnviroAssoc) data from EIA-860 using data year specified in eia_860_year
#download.file(
#  eia_data_file,
#  str_glue("data/eia860{eia_860_year}.zip")
#)
#unzip(zipfile = str_glue("data/eia860{eia_860_year}.zip"), exdir = "data")
#
#
#
# Get plant location data
eia_plant <-
read_excel(
  str_glue("{eia_860_year}/2___Plant_Y{eia_860_year}.xlsx"),
  sheet = "Plant",
  range = cell_cols("C:K"),
  skip = 1,
  trim_ws = TRUE
) %>%
  select(
    EIA_PLANT_ID = "Plant Code",
  ) %>%
  mutate("year" = eia_860_year)

# Get boiler ID
eia_boigen <-
  read_excel(
    str_glue("{eia_860_year}/6_1_EnviroAssoc_Y{eia_860_year}.xlsx"),
    sheet = "Boiler Generator",
    range = cell_cols("C:F"),
    skip = 1,
    trim_ws = TRUE
  ) %>%
  select(
    EIA_PLANT_ID = "Plant Code",
    EIA_GENERATOR_ID = "Generator ID",
    EIA_BOILER_ID = "Boiler ID",
  ) %>%
  inner_join(eia_plant, by = c("EIA_PLANT_ID"))

# Get Cooler ID
eia_boigencool <-
  read_excel(
    str_glue("{eia_860_year}/6_1_EnviroAssoc_Y{eia_860_year}.xlsx"),
    sheet = "Boiler Cooling",
    range = cell_cols("C:F"),
    skip = 1,
    trim_ws = TRUE
  ) %>%
  select(
    EIA_PLANT_ID = "Plant Code",
    EIA_COOLER_ID = "Cooling ID",
    EIA_BOILER_ID = "Boiler ID",
  ) %>%
  right_join(eia_boigen, by = c("EIA_PLANT_ID", "EIA_BOILER_ID"))


eia_boiler_par <-
  read_excel(
    str_glue("{eia_860_year}/6_2_EnviroEquip_Y{eia_860_year}.xlsx"),
    sheet = "Boiler Info & Design Parameters",
    range = cell_cols("C:X"),
    skip = 1,
    trim_ws = TRUE,
  ) %>%
  select(
    EIA_PLANT_ID = "Plant Code",
    EIA_BOILER_ID = "Boiler ID",
    BOIL_STATUS = "Boiler Status",
    BOIL_STARTMO = "Inservice Month", 
    BOIL_STARTYR = "Inservice Year",
    BOIL_RETMO = "Retirement Month", 
    BOIL_RETYR = "Retirement Year",
    BOIL_FUEL1 = "Primary Fuel 1",
    BOIL_FUEL2 = "Primary Fuel 2",
    BOIL_fuel3 = "Primary Fuel 3"
  ) 

# Create a consolidated list of all units (retired and operating)

eia_gen_opr <- # Operating units
  read_excel(
    str_glue("{eia_860_year}/3_1_Generator_Y{eia_860_year}.xlsx"),
    sheet = "Operable",
    range = cell_cols("C:AI"),
    skip = 1,
    trim_ws = TRUE
  ) %>%
  select(-"Planned Retirement Month", -"Planned Retirement Year") %>%
  mutate("Retirement Month" = 0) %>%
  mutate("Retirement Year" = 0)

eia_gen_ret <- # Retired units
  read_excel(
    str_glue("{eia_860_year}/3_1_Generator_Y{eia_860_year}.xlsx"),
    sheet = "Retired and Canceled",
    range = cell_cols("C:AI"),
    skip = 1,
    trim_ws = TRUE
  )

eia_generator <- rbind(eia_gen_opr, eia_gen_ret) %>%
  select(
    EIA_PLANT_ID = "Plant Code",
    EIA_PLANT_NAME = "Plant Name",
    EIA_STATE = "State",
    EIA_COUNTY = "County",
    EIA_GENERATOR_ID = "Generator ID",
    GEN_TECHNOLOGY = "Technology",
    GEN_PRIME_MOVER = "Prime Mover",
    GEN_UNIT_CODE = "Unit Code",
    GEN_NAMEPLATE_CAPACITY = "Nameplate Capacity (MW)",
    GEN_FUEL_TYPE1 = "Energy Source 1",
    GEN_FUEL_TYPE2 = "Energy Source 2",
    GEN_Operating_Month = "Operating Month",
    GEN_Operating_Year = "Operating Year",
    GEN_RETIRE_Month = "Retirement Month",
    GEN_RETIRE_YEAR = "Retirement Year",
    GEN_SECTOR_NAME = "Sector Name",
    GEN_SECTOR = "Sector"
  )

# Get Cooler Info
eia_cooler <-
  read_excel(
    str_glue("{eia_860_year}/6_2_EnviroEquip_Y{eia_860_year}.xlsx"),
    sheet = "Cooling",
    range = cell_cols("C:R"),
    skip = 1,
    trim_ws = TRUE,
    #n_max = Inf,
    #guess_max = min(1000, n_max)
    guess_max = 4000
  ) %>%
  select(
    EIA_PLANT_ID = "Plant Code",
    EIA_COOLER_ID = "Cooling ID",
    COOL_STATUS = "Cooling Status",
    COOL_TYPE1 = "Cooling Type 1", 
    COOL_TYPE2 = "Cooling Type 2",
    COOL_PER_DC = "Percent Dry Cooling",
    COOL_WS = "Cooling Water Source",
    COOL_WC = "Water Source Code",
    COOL_WD = "Cooling Water Discharge",
    COOL_WT = "Water Type Code"
  ) 
#
# Combine plant, generator, boiler, and cooler tables
eia_ids_boi <- left_join(eia_boigencool, eia_boiler_par, by = c("EIA_PLANT_ID", "EIA_BOILER_ID"))
eia_idsboigen <- full_join(eia_ids_boi, eia_generator, by = c("EIA_PLANT_ID", "EIA_GENERATOR_ID"))
eia_idboigencool <- left_join(eia_idsboigen, eia_cooler, by = c("EIA_PLANT_ID", "EIA_COOLER_ID"))

# need to remove leading zeros from generator ids to match with EIA FORM 923 PAGE 4
eia_idboigencooln <- mutate(eia_idboigencool, EIA_GENERATOR_IDNUM = tryCatch(as.numeric(EIA_GENERATOR_ID) * 1, error = EIA_GENERATOR_ID))
eia_idboigencool_n2 <- mutate(eia_idboigencooln, EIA_GENERATOR_IDN = coalesce(as.character(EIA_GENERATOR_IDNUM), EIA_GENERATOR_ID))


# get EIA 923 generation data by matching EIA_PLANT_ID and EIA_GENERATOR_ID -- power generated by generator (page 4) and power generated by prime mover (page 1)
eia_idboigencool_923_4 <-
  read_excel(
    str_glue("../EIA_923/{eia_860_year}/EIA923_Schedules_2_3_4_5_M_12_{eia_860_year}_Final_Revision.xlsx"),
    sheet = "Page 4 Generator Data",
    #    range = cell_cols("A:Z"),
    skip = 5,
    trim_ws = TRUE,
    .name_repair = "universal",
  ) %>%
  select(
    EIA_PLANT_ID = "Plant.Id",
    EIA_GENERATOR_IDN = "Generator.Id",
    PRIME_MOVER_923_1 = "Reported..Prime.Mover",
    NetGen_Jan4 = "Net.Generation..January", 
    NetGen_Feb4 = "Net.Generation..February", 
    NetGen_Mar4 = "Net.Generation..March", 
    NetGen_Apr4 = "Net.Generation..April", 
    NetGen_May4 = "Net.Generation..May", 
    NetGen_Jun4 = "Net.Generation..June", 
    NetGen_Jul4 = "Net.Generation..July", 
    NetGen_Aug4 = "Net.Generation..August", 
    NetGen_Sep4 = "Net.Generation..September", 
    NetGen_Oct4 = "Net.Generation..October", 
    NetGen_Nov4 = "Net.Generation..November", 
    NetGen_Dec4 = "Net.Generation..December", 
    NetGen_Yr4 = "Net.Generation..Year.To.Date",
  )  %>%
  full_join(eia_idboigencool_n2, by = c("EIA_PLANT_ID", "EIA_GENERATOR_IDN"))

# cleanup
rm(eia_plant)
rm(eia_boigen)
rm(eia_boiler_par)
rm(eia_cooler)
rm(eia_gen_opr)
rm(eia_gen_ret)
rm(eia_generator)
rm(eia_ids_boi)
rm(eia_idsboigen)
rm(eia_boigencool)
rm(eia_idboigencool)
rm(eia_idboigencooln)
rm(eia_idboigencool_n2)

# get EIA 923 generation data by Reported Prime Mover and Reported Fuel Type for plants the generated power in page 1

eia_idboigencool_923_1 <-
  read_excel(
    str_glue("../EIA_923/{eia_860_year}/EIA923_Schedules_2_3_4_5_M_12_{eia_860_year}_Final_Revision.xlsx"),
    sheet = "Page 1 Generation and Fuel Data",
    #    range = cell_cols("A:CS"),
    skip = 5,
    trim_ws = TRUE,
    .name_repair = "universal",
  ) %>%
  select(
    EIA_PLANT_ID = "Plant.Id",
    GEN_PRIME_MOVER = "Reported..Prime.Mover",
    GEN_FUEL_TYPE1 = "Reported..Fuel.Type.Code",
    NetGen_Jan1 = "Netgen..January", 
    NetGen_Feb1 = "Netgen..February", 
    NetGen_Mar1 = "Netgen..March", 
    NetGen_Apr1 = "Netgen..April", 
    NetGen_May1 = "Netgen..May", 
    NetGen_Jun1 = "Netgen..June", 
    NetGen_Jul1 = "Netgen..July", 
    NetGen_Aug1 = "Netgen..August", 
    NetGen_Sep1 = "Netgen..September", 
    NetGen_Oct1 = "Netgen..October", 
    NetGen_Nov1 = "Netgen..November", 
    NetGen_Dec1 = "Netgen..December", 
    NetGen_Yr1 = "Net.Generation...Megawatthours.",
  )  %>%
  #  filter(is.na(PRIME_MOVER_923_1) & is.na(PRIME_MOVER_923_2)) %>%
  right_join(eia_idboigencool_923_4, by = c("EIA_PLANT_ID", "GEN_PRIME_MOVER", "GEN_FUEL_TYPE1"))
#
# Combine generation data and move columns
eia_idboigencool_923_41 <- eia_idboigencool_923_1 %>% 
  mutate(NetGen_Jan = coalesce(NetGen_Jan4, NetGen_Jan1)) %>%
  relocate(NetGen_Jan, .before = NetGen_Jan1) %>%
  select(-NetGen_Jan4) %>% select(-NetGen_Jan1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Feb = coalesce(NetGen_Feb4, NetGen_Feb1)) %>%
  relocate(NetGen_Feb, .before = NetGen_Feb1)  %>%
  select(-NetGen_Feb4) %>% select(-NetGen_Feb1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Mar = coalesce(NetGen_Mar4, NetGen_Mar1)) %>%
  relocate(NetGen_Mar, .before = NetGen_Mar1)  %>%
  select(-NetGen_Mar4) %>% select(-NetGen_Mar1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Apr = coalesce(NetGen_Apr4, NetGen_Apr1)) %>%
  relocate(NetGen_Apr, .before = NetGen_Apr1) %>%
  select(-NetGen_Apr4) %>% select(-NetGen_Apr1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_May = coalesce(NetGen_May4, NetGen_May1)) %>%
  relocate(NetGen_May, .before = NetGen_May1) %>%
  select(-NetGen_May4) %>% select(-NetGen_May1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Jun = coalesce(NetGen_Jun4, NetGen_Jun1)) %>%
  relocate(NetGen_Jun, .before = NetGen_Jun1) %>%
  select(-NetGen_Jun4) %>% select(-NetGen_Jun1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Jul = coalesce(NetGen_Jul4, NetGen_Jul1)) %>%
  relocate(NetGen_Jul, .before = NetGen_Jul1) %>%
  select(-NetGen_Jul4) %>% select(-NetGen_Jul1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Aug = coalesce(NetGen_Aug4, NetGen_Aug1)) %>%
  relocate(NetGen_Aug, .before = NetGen_Aug1) %>%
  select(-NetGen_Aug4) %>% select(-NetGen_Aug1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Sep = coalesce(NetGen_Sep4, NetGen_Sep1)) %>%
  relocate(NetGen_Sep, .before = NetGen_Sep1) %>%
  select(-NetGen_Sep4) %>% select(-NetGen_Sep1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Oct = coalesce(NetGen_Oct4, NetGen_Oct1)) %>%
  relocate(NetGen_Oct, .before = NetGen_Oct1) %>%
  select(-NetGen_Oct4) %>% select(-NetGen_Oct1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Nov = coalesce(NetGen_Nov4, NetGen_Nov1)) %>%
  relocate(NetGen_Nov, .before = NetGen_Nov1) %>%
  select(-NetGen_Nov4) %>% select(-NetGen_Nov1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Dec = coalesce(NetGen_Dec4, NetGen_Dec1)) %>%
  relocate(NetGen_Dec, .before = NetGen_Dec1) %>%
  select(-NetGen_Dec4) %>% select(-NetGen_Dec1)
eia_idboigencool_923_41 <- eia_idboigencool_923_41 %>% 
  mutate(NetGen_Yr = coalesce(NetGen_Yr4, NetGen_Yr1)) %>%
  relocate(NetGen_Yr, .before = NetGen_Yr1) %>%
  select(-NetGen_Yr4) %>% select(-NetGen_Yr1)
#
# apply filters
eia_masterlist_f1 <- filter(eia_idboigencool_923_41,  !is.na(GEN_RETIRE_YEAR) & (GEN_RETIRE_YEAR == 0 | GEN_RETIRE_YEAR > ({eia_860_year} - 1)))
eia_masterlist_f2 <- filter(eia_masterlist_f1, GEN_PRIME_MOVER == "CT" | GEN_PRIME_MOVER == "CS" | GEN_PRIME_MOVER == "CA" | GEN_PRIME_MOVER == "ST")
eia_masterlist_f3 <- filter(eia_masterlist_f2, NetGen_Yr > 0)
eia_masterlist_f4 <- filter(eia_masterlist_f3, GEN_SECTOR < 4 | GEN_SECTOR > 7)
eia_masterlist_f5 <- filter(eia_masterlist_f4, GEN_FUEL_TYPE1 != "GEO" & GEN_FUEL_TYPE1 != "SUN" & GEN_FUEL_TYPE1 != "SGC" & GEN_FUEL_TYPE1 != "SGP")

# routine to identify all dry cooling types based on plantid + gen_unit_code and cool_type1
dclist <- filter(eia_masterlist_f5, (COOL_TYPE1 == "DC"))
dclist$plant_genunit <- paste(dclist$EIA_PLANT_ID, dclist$GEN_UNIT_CODE, sep = "_")
eia_masterlist_f5$Plant_genunit <- paste(eia_masterlist_f5$EIA_PLANT_ID, eia_masterlist_f5$GEN_UNIT_CODE, sep = "_")
eia_masterlist_f5b = within(eia_masterlist_f5, is_DC <- (eia_masterlist_f5$Plant_genunit %in% dclist$plant_genunit) & (eia_masterlist_f5$COOL_TYPE1 == "DC" | is.na(eia_masterlist_f5$COOL_TYPE1)))
eia_masterlist_f6 <- filter(eia_masterlist_f5b, !(is_DC)) 
eia_masterlist_f7 <- subset(eia_masterlist_f6, select = -c(Plant_genunit, is_DC, EIA_GENERATOR_IDN, GEN_Operating_Month, GEN_Operating_Year, BOIL_STARTMO, BOIL_STARTYR))
# moved lat/long addition to the end so plant data from 923 will have lat/longs
eia_masterlist <-
  read_excel(
    str_glue("{eia_860_year}/2___Plant_Y{eia_860_year}.xlsx"),
    sheet = "Plant",
    range = cell_cols("C:K"),
    skip = 1,
    trim_ws = TRUE
  ) %>%
  select(
    EIA_PLANT_ID = "Plant Code",
    EIA_LATITUDE = "Latitude",
    EIA_LONGITUDE = "Longitude"
  ) %>%
  right_join(eia_masterlist_f7, by = c("EIA_PLANT_ID"))

#
# rearrange columns
eia_masterlist <- eia_masterlist %>% relocate(EIA_PLANT_NAME, .after = EIA_PLANT_ID)
eia_masterlist <- eia_masterlist %>% relocate(EIA_STATE, .after = EIA_PLANT_NAME)
eia_masterlist <- eia_masterlist %>% relocate(EIA_COUNTY, .after = EIA_STATE)
eia_masterlist <- eia_masterlist %>% relocate(EIA_LATITUDE, .after = EIA_COUNTY)
eia_masterlist <- eia_masterlist %>% relocate(EIA_LONGITUDE, .after = EIA_LATITUDE)
eia_masterlist <- eia_masterlist %>% relocate(GEN_TECHNOLOGY, .after = EIA_LONGITUDE)
eia_masterlist <- eia_masterlist %>% relocate(EIA_GENERATOR_ID, .after = GEN_TECHNOLOGY) 
eia_masterlist <- eia_masterlist %>% relocate(GEN_UNIT_CODE, .after = EIA_GENERATOR_ID)
eia_masterlist <- eia_masterlist %>% relocate(PRIME_MOVER_923_1, .after = GEN_PRIME_MOVER)
eia_masterlist <- eia_masterlist %>% relocate(GEN_FUEL_TYPE2, .after = GEN_FUEL_TYPE1)
eia_masterlist <- eia_masterlist %>% relocate(GEN_NAMEPLATE_CAPACITY, .after = GEN_FUEL_TYPE2)
eia_masterlist <- eia_masterlist %>% relocate(GEN_RETIRE_Month, .after = NetGen_Yr)
eia_masterlist <- eia_masterlist %>% relocate(GEN_RETIRE_YEAR, .after = GEN_RETIRE_Month)
eia_masterlist <- eia_masterlist %>% relocate(GEN_SECTOR_NAME, .after = GEN_RETIRE_YEAR)
eia_masterlist <- eia_masterlist %>% relocate(GEN_SECTOR, .after = GEN_SECTOR_NAME)
#
# more cleanup
rm(dclist)
rm(eia_idboigencool_923_1)
rm(eia_idboigencool_923_4)
rm(eia_idboigencool_923_41)
rm(eia_masterlist_f1)
rm(eia_masterlist_f2)
rm(eia_masterlist_f3)
rm(eia_masterlist_f4)
rm(eia_masterlist_f5)
rm(eia_masterlist_f5b)
rm(eia_masterlist_f6)

write.xlsx(eia_masterlist, file = str_glue("eia_masterlist_{eia_860_year}.xlsx"),
           indicator = "all")

write_excel_csv(eia_masterlist,
                str_glue("eia_masterlist_{eia_860_year}.csv"),
                col_names = TRUE,
                na = ""
)
#
rm(eia_masterlist_f7)
rm(eia_masterlist)



