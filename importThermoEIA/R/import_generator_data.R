#'@title import_generator_data
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA-860 Generator Data table \cr \cr
#'Executed By: import_EIAData() \cr
#'@param gen860 EIA-860 Generator Data table import with standardized column names found in 
#'                 \code{\link{crosswalk2015}} where scriptTableName = "gen860"
#'@return `gen860` formatted Generator Data  table
#'@export

import_generator_data <- function(gen860)
{

  if(!is.data.frame(gen860) || any(!c('Utility.ID','Plant.Code','Generator.ID','Prime.Mover') %in% names(gen860))) 
  {
    stop("data must be a data.frame with columns 'Utility.ID','Plant.Code','Generator.ID','Prime.Mover' for this function to continue")
  }
  
  
  #Test variable types
  testthat::test_that("Generator data variable types are correct",
            {
              testthat::expect_true(is.numeric(gen860$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(gen860$Generator.ID), "Error wrong variable type. Generator.ID is a character string.")
              testthat::expect_true(is.character(gen860$Prime.Mover), "Error wrong variable type. Prime.Mover is a character string.")

              
            }
  )
  
  return(gen860)
}
