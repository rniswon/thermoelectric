#'@title import_sheet1_gen_and_fuel_data
#'@description import and format for use the 
#'             ~/thermoelectric-water-consumption-models/CondenserDutyModel/Data/2015_GenerationAndFuel.csv
#'              file \cr \cr
#'Executed By: CondenserDutyModel_new.R \cr
#'@param plant_Data_file_path path to ~/thermoelectric-water-consumption-models/
#'                                CondenserDutyModel/Data/2015_GenerationAndFuel.csv
#'@return `sheet1_gen_fuel_data` formatted plant generator fuel data
#'@examples
#'import_sheet1_gen_and_fuel_data(paste0(inputData_path,'/2015_GenerationAndFuel.csv'))

#import_sheet1_gen_and_fuel_data <- function(plant_Data_file_path)
import_sheet1_gen_and_fuel_data <- function(sheet1_gen_fuel_data)
    
{
  # if(!file.exists(plant_Data_file_path)) 
  # {
  #   stop("enter valid file path")
  # }
  # path  <-  plant_Data_file_path
  # sheet1_gen_fuel_data_header  <-  read.csv(path,header=T,stringsAsFactors = F,nrows=5)
  # sheet1_gen_fuel_data  <-  read.csv(path,header=T,stringsAsFactors = F,skip=5)
  if(!is.data.frame(sheet1_gen_fuel_data) || any(!c('Plant.Id', 'Netgen.January','Netgen.February','Netgen.March',
                                                    'Netgen.April','Netgen.May','Netgen.June','Netgen.July',
                                                    'Netgen.August','Netgen.September','Netgen.October','Netgen.November','Netgen.December') %in% names(sheet1_gen_fuel_data))) 
  {
    stop("data must be a data.frame with columns 'Plant.Id', 'Netgen.January','Netgen.February','Netgen.March',
         'Netgen.April','Netgen.May','Netgen.June','Netgen.July',
         'Netgen.August','Netgen.September','Netgen.October','Netgen.November','Netgen.December' for this function to continue")
  }
  names(sheet1_gen_fuel_data)[names(sheet1_gen_fuel_data) == 'Plant.Id']  <-  "Plant.Code"
  
  #missing data
  # sheet1_gen_fuel_data2<-sheet1_gen_fuel_data
  # sheet1_gen_fuel_data2[scriptnames][sheet1_gen_fuel_data2[scriptnames] == "."] <- NA
  # sheet1_gen_fuel_data$missingData<-rowSums(is.na(sheet1_gen_fuel_data2[scriptnames]))
  # sheet1_gen_fuel_data[scriptnames][sheet1_gen_fuel_data[scriptnames] == "."] <- 0

  scriptnames=c("Quantity.January",
                "Quantity.February",
                "Quantity.March",
                "Quantity.April",
                "Quantity.May",
                "Quantity.June",
                "Quantity.July",
                "Quantity.August",
                "Quantity.September",
                "Quantity.October",
                "Quantity.November",
                "Quantity.December",
                "Elec_Quantity.January",
                "Elec_Quantity.February",
                "Elec_Quantity.March",
                "Elec_Quantity.April",
                "Elec_Quantity.May",
                "Elec_Quantity.June",
                "Elec_Quantity.July",
                "Elec_Quantity.August",
                "Elec_Quantity.September",
                "Elec_Quantity.October",
                "Elec_Quantity.November",
                "Elec_Quantity.December",
                "MMBtuPer_Unit.January",
                "MMBtuPer_Unit.February",
                "MMBtuPer_Unit.March",
                "MMBtuPer_Unit.April",
                "MMBtuPer_Unit.May",
                "MMBtuPer_Unit.June",
                "MMBtuPer_Unit.July",
                "MMBtuPer_Unit.August",
                "MMBtuPer_Unit.September",
                "MMBtuPer_Unit.October",
                "MMBtuPer_Unit.November",
                "MMBtuPer_Unit.December",
                "Tot_MMBtu.January",
                "Tot_MMBtu.February",
                "Tot_MMBtu.March",
                "Tot_MMBtu.April",
                "Tot_MMBtu.May",
                "Tot_MMBtu.June",
                "Tot_MMBtu.July",
                "Tot_MMBtu.August",
                "Tot_MMBtu.September",
                "Tot_MMBtu.October",
                "Tot_MMBtu.November",
                "Tot_MMBtu.December",
                "Elec_MMBtu.January",
                "Elec_MMBtu.February",
                "Elec_MMBtu.March",
                "Elec_MMBtu.April",
                "Elec_MMBtu.May",
                "Elec_MMBtu.June",
                "Elec_MMBtu.July",
                "Elec_MMBtu.August",
                "Elec_MMBtu.September",
                "Elec_MMBtu.October",
                "Elec_MMBtu.November",
                "Elec_MMBtu.December",
                "Netgen.January",
                "Netgen.February",
                "Netgen.March",
                "Netgen.April",
                "Netgen.May",
                "Netgen.June",
                "Netgen.July",
                "Netgen.August",
                "Netgen.September",
                "Netgen.October",
                "Netgen.November",
                "Netgen.December",
                "Total.Fuel.Consumption.Quantity",
                "Electric.Fuel.Consumption.Quantity",
                "Total.Fuel.Consumption.MMBtu",
                "Elec.Fuel.Consumption.MMBtu",
                "Net.Generation..Megawatthours."
                )
  
  sheet1_gen_fuel_data<-removeMissing(sheet1_gen_fuel_data,cols = which(names(sheet1_gen_fuel_data) %in% scriptnames), missingFlag = ".")
  
  sheet1_gen_fuel_data[scriptnames] <- sapply(sheet1_gen_fuel_data[scriptnames], function(x){gsub("\\,","",x)})
  sheet1_gen_fuel_data[scriptnames] <- sapply(sheet1_gen_fuel_data[scriptnames],as.numeric)
  
  
  test_for_numeric_variables_data_frame<-function(x){all(sapply(x,is.numeric))}
  
  #Test variable types
  test_that("Sheet1 generation and fuel consumption data variable types are correct",
            {
              expect_true(test_for_numeric_variables_data_frame(sheet1_gen_fuel_data[scriptnames]), "Error wrong variable type. Generation and fuel consumptiona are numeric variables.")
              expect_true(is.numeric(sheet1_gen_fuel_data$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              expect_true(is.character(sheet1_gen_fuel_data$Plant.Name), "Error wrong variable type. Plant.Name is a character string.")
              expect_true(is.character(sheet1_gen_fuel_data$Reported.Prime.Mover), "Error wrong variable type. Reported.Prime.Mover is a character string.")
              expect_true(is.character(sheet1_gen_fuel_data$Reported.Fuel.Type.Code), "Error wrong variable type. Reported.Fuel.Type.Code is a character string.")
              
              
            }
  )
  
  return(sheet1_gen_fuel_data)
}
