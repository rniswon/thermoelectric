#'@title import_plantInfo
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA Plant Information table \cr \cr
#'@param plantInfo EIA Plant Information table import with standardized column names found in 
#'                 \code{\link{crosswalk2015}} where scriptTableName = "plantInfo"
#'@export

import_plantInfo <- function(plantInfo){
{
  

    if(!is.data.frame(plantInfo) || any(!c('Plant.Code','Plant.Name','County',
                                         'State','Name.of.Water.Source') %in% names(plantInfo))) 
    {
      stop("data must be a data.frame with columns 'Plant.Code','Plant.Name','County',
                                       'State','Name.of.Water.Source' for this function to continue")
    }
  

    
  }
  
  #Test variable types
  testthat::test_that("Cooling data variable types are correct",
            {
              testthat::expect_true(is.numeric(plantInfo$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(plantInfo$Plant.Name), "Error wrong variable type. Plant.Name is a character string.")
              testthat::expect_true(is.character(plantInfo$County), "Error wrong variable type. County is a character string.")
              testthat::expect_true(is.character(plantInfo$State), "Error wrong variable type. State is a character string.")
              testthat::expect_true(is.character(plantInfo$Name.of.Water.Source), "Error wrong variable type. Name.of.Water.Source is a character string.")
              
              
            }
  )
  
  return(plantInfo)
}
