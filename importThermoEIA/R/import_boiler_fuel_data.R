#'@title import_boiler_fuel_data
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA Boiler Fuel Data table \cr \cr
#'@param boilerFuelData EIA Boiler Fuel Data table import with standardized column names found in 
#'                 \code{\link{crosswalk2015}} where scriptTableName = "boilerFuelData"
#'@return `boilerFuelData` formatted Boiler Fuel Data table
#'@export

import_boiler_fuel_data <- function(boilerFuelData)
{

  if(!is.data.frame(boilerFuelData) || any(!c('Plant.Id', 'Boiler.Id') %in% names(boilerFuelData))) 
  {
    stop("data must be a data.frame with columns 'Plant.Id', 'Boiler.Id' for this function to continue")
  }
  names(boilerFuelData)[names(boilerFuelData)=='Plant.Id'] <- "Plant.Code"
  names(boilerFuelData)[names(boilerFuelData)=='Boiler.Id'] <- "Boiler.ID"
  

  #missing data
  boilerFuelData<-removeMissing(boilerFuelData,cols = c(1:length(boilerFuelData)), missingFlag = ".")
  

  
   scriptnames=c("Quantity.Of.Fuel.Consumed.January",
                     "Quantity.Of.Fuel.Consumed.February",
                     "Quantity.Of.Fuel.Consumed.March",
                     "Quantity.Of.Fuel.Consumed.April",
                     "Quantity.Of.Fuel.Consumed.May",
                     "Quantity.Of.Fuel.Consumed.June",
                     "Quantity.Of.Fuel.Consumed.July",
                     "Quantity.Of.Fuel.Consumed.August",
                     "Quantity.Of.Fuel.Consumed.September",
                     "Quantity.Of.Fuel.Consumed.October",
                     "Quantity.Of.Fuel.Consumed.November",
                     "Quantity.Of.Fuel.Consumed.December",
                     "MMbtu.Per.Unit.January",
                     "MMbtu.Per.Unit.February",
                     "MMbtu.Per.Unit.March",
                     "MMbtu.Per.Unit.April",
                     "MMbtu.Per.Unit.May",
                     "MMbtu.Per.Unit.June",
                     "MMbtu.Per.Unit.July",
                     "MMbtu.Per.Unit.August",
                     "MMbtu.Per.Unit.September",
                     "MMbtu.Per.Unit.October",
                     "MMbtu.Per.Unit.November",
                     "MMbtu.Per.Unit.December",
                     "Sulfur.Content.January",
                     "Sulfur.Content.February",
                     "Sulfur.Content.March",
                     "Sulfur.Content.April",
                     "Sulfur.Content.May",
                     "Sulfur.Content.June",
                     "Sulfur.Content.July",
                     "Sulfur.Content.August",
                     "Sulfur.Content.September",
                     "Sulfur.Content.October",
                     "Sulfur.Content.November",
                     "Sulfur.Content.December",
                     "Ash.Content.January",
                     "Ash.Content.February",
                     "Ash.Content.March",
                     "Ash.Content.April",
                     "Ash.Content.May",
                     "Ash.Content.June",
                     "Ash.Content.July",
                     "Ash.Content.August",
                     "Ash.Content.September",
                     "Ash.Content.October",
                     "Ash.Content.November",
                     "Ash.Content.December",
                     "Total.Fuel.Consumption.Quantity") 
  
  boilerFuelData[scriptnames] <- sapply(boilerFuelData[scriptnames], function(x){gsub("\\,","",x)})
                     
  boilerFuelData[scriptnames] <- sapply(boilerFuelData[scriptnames],as.numeric)
                     
  
  test_for_numeric_variables_data_frame<-function(x){all(sapply(x,is.numeric))}
  
  #Test variable types
  testthat::test_that("Boiler fuel data variable types are correct",
            {
              testthat::expect_true(is.numeric(boilerFuelData$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(boilerFuelData$Boiler.ID), "Error wrong variable type. Boiler.ID is a character string.")
              testthat::expect_true(test_for_numeric_variables_data_frame(boilerFuelData[scriptnames]), "Error wrong variable type. Fuel consumption and fuel heat content are numeric variables.")
              testthat::expect_true(if("Reported.Prime.Mover" %in% names(boilerFuelData)){
                is.character(boilerFuelData$Reported.Prime.Mover)}else{TRUE}, "Error wrong variable type. Reported.Prime.Mover is a character string.")
              testthat::expect_true(is.character(boilerFuelData$Reported.Fuel.Type.Code), "Error wrong variable type. Reported.Fuel.Type.Code is a character string.")
              
            }
  )
  
  return(boilerFuelData)
}
