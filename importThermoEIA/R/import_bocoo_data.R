#'@title import_bocoo_data
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA Boiler-Cooling association table \cr \cr
#'@param bocoo EIA Boiler-Cooling association table import with standardized column names found in 
#'                 \code{\link{crosswalk2015}} where scriptTableName = "bocoo"
#'@return `bocoo` formatted Boiler-Cooling association table
#'@export

import_bocoo_data <- function(bocoo)
{
  
  if(!is.data.frame(bocoo) || any(!c('Plant.Code',  'Boiler.ID','Cooling.ID') %in% names(bocoo))) 
  {
    stop("data must be a data.frame with columns 'Plant.Code',  'Boiler.ID','Cooling.ID' for this function to continue")
  }
  
  #Test variable types
  testthat::test_that("Bocoo data variable types are correct",
            {
              testthat::expect_true(is.numeric(bocoo$Plant.Code),"Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(bocoo$Boiler.ID), "Error wrong variable type. Boiler.ID is a character string.")
              testthat::expect_true(is.character(bocoo$Cooling.ID), "Error wrong variable type. Cooling.ID is a character string.")
              
            }
  )
  #Return imported Bocoo Data
  return(bocoo)
}
