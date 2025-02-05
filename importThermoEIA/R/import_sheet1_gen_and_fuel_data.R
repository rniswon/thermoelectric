#'@title import_sheet1_gen_and_fuel_data
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA Plant-Reported.Prime.Mover level Fuel Consumption and Generation Data table \cr \cr
#'@param gen_fuel_data EIA Plant-Reported.Prime.Mover level Fuel Consumption and Generation Data
#'                     table import with standardized column names found in \code{\link{crosswalk2015}} 
#'                     where scriptTableName = "gen_fuel_data"
#'@return `gen_fuel_data` formatted Plant-Reported.Prime.Mover level Fuel Consumption and Generation 
#'                        Data  table
#'@export

import_sheet1_gen_and_fuel_data <- function(gen_fuel_data)
    
{

  if(!is.data.frame(gen_fuel_data) || any(!c('Plant.Id', 'Netgen.January','Netgen.February','Netgen.March',
                                                    'Netgen.April','Netgen.May','Netgen.June','Netgen.July',
                                                    'Netgen.August','Netgen.September','Netgen.October','Netgen.November','Netgen.December') %in% names(gen_fuel_data))) 
  {
    stop("data must be a data.frame with columns 'Plant.Id', 'Netgen.January','Netgen.February','Netgen.March',
         'Netgen.April','Netgen.May','Netgen.June','Netgen.July',
         'Netgen.August','Netgen.September','Netgen.October','Netgen.November','Netgen.December' for this function to continue")
  }
  names(gen_fuel_data)[names(gen_fuel_data) == 'Plant.Id']  <-  "Plant.Code"
  

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
                "Elec.Fuel.Consumption.MMBtu",
                "Net.Generation..Megawatthours."
                )
  
  gen_fuel_data<-removeMissing(gen_fuel_data,cols = which(names(gen_fuel_data) %in% scriptnames), missingFlag = ".")
  
  gen_fuel_data[scriptnames] <- sapply(gen_fuel_data[scriptnames], function(x){gsub("\\,","",x)})
  gen_fuel_data[scriptnames] <- sapply(gen_fuel_data[scriptnames],as.numeric)
  
  
  test_for_numeric_variables_data_frame<-function(x){all(sapply(x,is.numeric))}
  
  #Test variable types
  testthat::test_that("Sheet1 generation and fuel consumption data variable types are correct",
            {
              testthat::expect_true(test_for_numeric_variables_data_frame(gen_fuel_data[scriptnames]), "Error wrong variable type. Generation and fuel consumptiona are numeric variables.")
              testthat::expect_true(is.numeric(gen_fuel_data$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(gen_fuel_data$Plant.Name), "Error wrong variable type. Plant.Name is a character string.")
              testthat::expect_true(is.character(gen_fuel_data$Reported.Fuel.Type.Code), "Error wrong variable type. Reported.Fuel.Type.Code is a character string.")
              
              
            }
  )
  
  return(gen_fuel_data)
}
