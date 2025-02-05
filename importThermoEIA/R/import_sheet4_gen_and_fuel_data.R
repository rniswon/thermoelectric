#'@title import_sheet4_gen_and_fuel_data
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA Generation Data table\cr \cr
#'@param generation.data EIA Generation Data table import with standardized column names found in 
#'                       \code{\link{crosswalk2015}} where scriptTableName = "generation.data"
#'@return `generation.data` formatted Generation Data table
#'@export

import_sheet4_gen_and_fuel_data <- function(generation.data)
{

  
  if(!is.data.frame(generation.data) || any(!c('Plant.Id',
                                                    'Net.Generation.February','Net.Generation.March','Net.Generation.April','Net.Generation.May',
                                                    'Net.Generation.June','Net.Generation.July','Net.Generation.August','Net.Generation.October',
                                                    'Net.Generation.November','Net.Generation.December') %in% names(generation.data)))
  {
    stop("data must be a data.frame with columns 'Plant.Id',
         'Net.Generation.February','Net.Generation.March','Net.Generation.April','Net.Generation.May',
         'Net.Generation.June','Net.Generation.July','Net.Generation.August','Net.Generation.October',
         'Net.Generation.November','Net.Generation.December' for this function to continue")
  }
  
  names(generation.data)[names(generation.data)=='Plant.Id'] <- "Plant.Code"
  names(generation.data)[names(generation.data)=='Generator.Id'] <- "Generator.ID"
  

  
  scriptnames=c("Net.Generation.January",
                "Net.Generation.February",
                "Net.Generation.March",
                "Net.Generation.April",
                "Net.Generation.May",
                "Net.Generation.June",
                "Net.Generation.July",
                "Net.Generation.August",
                "Net.Generation.September",
                "Net.Generation.October",
                "Net.Generation.November",
                "Net.Generation.December",
                "Net.Generation.Year.To.Date")   
  
  #missing data
  generation.data<-removeMissing(generation.data, cols = which(names(generation.data) %in% scriptnames), missingFlag = ".")
  
  generation.data[scriptnames] <- sapply(generation.data[scriptnames], function(x){gsub("\\,","",x)})
  generation.data[scriptnames] <- sapply(generation.data[scriptnames],as.numeric)

  test_for_numeric_variables_data_frame<-function(x){all(sapply(x,is.numeric))}
  
  #Test variable types
  testthat::test_that("Sheet4 generation and fuel variable types are correct",
            {
              testthat::expect_true(is.numeric(generation.data$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(generation.data$Generator.ID), "Error wrong variable type. Generator.ID is a character string.")
              testthat::expect_true(test_for_numeric_variables_data_frame(generation.data[scriptnames]), "Error wrong variable type. Generation and fuel consumption are numeric variables.")
            }
  )
  
  return(generation.data)
}
