#'@title import_boiler_fuel_data
#'@description import and format for use the 
#'             ~/thermoelectric-water-consumption-models/CondenserDutyModel/Data/2015_BoilerFuelData.csv
#'              file \cr \cr
#'Executed By: CondenserDutyModel_new.R \cr
#'@param boiler_fuel_data path to ~/thermoelectric-water-consumption-models/
#'                                CondenserDutyModel/Data/2015_BoilerFuelData.csv
#'@return `boiler_fuel_data` formatted EIA-923 Monthly Boiler Fuel Consumption and Emissions Time Series 
#'                      File, 2015 Final Revision data
#'@examples
#'import_boiler_fuel_data(paste0(inputData_path,'/2015_BoilerFuelData.csv'))


import_boiler_fuel_data <- function(boiler_fuel_data)
{
  # if(!file.exists(boiler_fuel_data))
  # {
  #   stop("enter valid file path")
  # }
  # path <- boiler_fuel_data
  # boiler_fuel_data_header <- read.csv(path,header=T,stringsAsFactors = F,nrow=5)
  # boiler_fuel_data <- read.csv(path,header=T,stringsAsFactors = F,skip=5)
  if(!is.data.frame(boiler_fuel_data) || any(!c('Plant.Id', 'Boiler.Id') %in% names(boiler_fuel_data))) 
  {
    stop("data must be a data.frame with columns 'Plant.Id', 'Boiler.Id' for this function to continue")
  }
  names(boiler_fuel_data)[names(boiler_fuel_data)=='Plant.Id'] <- "Plant.Code"
  names(boiler_fuel_data)[names(boiler_fuel_data)=='Boiler.Id'] <- "Boiler.ID"
  
  #boiler_fuel_data[boiler_fuel_data=="."] <- NA
  #missing data
  boiler_fuel_data<-removeMissing(boiler_fuel_data,cols = c(1:length(boiler_fuel_data)), missingFlag = ".")
  
  #boiler_fuel_data[c("Quantity Of Fuel Consumed January", "Total Fuel Consumption Quantity")] <- sapply(boiler_fuel_data[c("Quantity Of Fuel Consumed January", "Total Fuel Consumption Quantity")], function(x){gsub("\\,","",x)})
      
  
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
  
  boiler_fuel_data[scriptnames] <- sapply(boiler_fuel_data[scriptnames], function(x){gsub("\\,","",x)})
                     
  boiler_fuel_data[scriptnames] <- sapply(boiler_fuel_data[scriptnames],as.numeric)
                     
  
  test_for_numeric_variables_data_frame<-function(x){all(sapply(x,is.numeric))}
  
  #Test variable types
  test_that("Boiler fuel data variable types are correct",
            {
              expect_true(is.character(boiler_fuel_data$Plant.Name), "Error wrong variable type. Plant.Name is a character string.")
              expect_true(is.numeric(boiler_fuel_data$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              expect_true(is.character(boiler_fuel_data$Boiler.ID), "Error wrong variable type. Boiler.ID is a character string.")
              expect_true(test_for_numeric_variables_data_frame(boiler_fuel_data[scriptnames]), "Error wrong variable type. Fuel consumption and fuel heat content are numeric variables.")
              expect_true(is.character(boiler_fuel_data$Reported.Prime.Mover), "Error wrong variable type. Reported.Prime.Mover is a character string.")
              expect_true(is.character(boiler_fuel_data$Reported.Fuel.Type.Code), "Error wrong variable type. Reported.Fuel.Type.Code is a character string.")
              
            }
  )
  
  return(boiler_fuel_data)
}
