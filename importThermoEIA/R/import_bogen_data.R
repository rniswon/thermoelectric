#'@title import_bogen_data
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA Boiler-Generator association table \cr \cr
#'@param bogen EIA Boiler-Generator association table import with standardized column names found in 
#'                 \code{\link{crosswalk2015}} where scriptTableName = "bogen"
#'@return `bogen` formatted Boiler-Generator association table
#'@export

import_bogen_data <- function(bogen)
{
 
   if(!is.data.frame(bogen) || any(!c('Plant.Code','Boiler.ID','Generator.ID') %in% names(bogen))) 
  {
    stop("data must be a data.frame with columns 'Plant.Code','Boiler.ID','Generator.ID' for this function to continue")
  }
  
  #Test check variable types
  testthat::test_that("Bogen data variable types are correct",
            {
              testthat::expect_true(is.numeric(bogen$Plant.Code),"Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(bogen$Boiler.ID),"Error wrong variable type. Boiler.ID is a character string.")
              testthat::expect_true(is.character(bogen$Generator.ID),"Error wrong variable type. Generator.ID is a character string.")
              
            }
  )
  #Return imported bogen data
  return(bogen)
}
