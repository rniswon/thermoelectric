#'@title import_coolingSys
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA Cooling System table \cr \cr
#'@param cooling EIA Cooling System  table import with standardized column names found in 
#'                 \code{\link{crosswalk2015}} where scriptTableName = "cooling"
#'@return `cooling` formatted Cooling System  table
#'@export

import_coolingSys <- function(cooling)
{

  if ("Inservice.Month" %in% names(cooling)){
  if(!is.data.frame(cooling) || any(!c('Plant.Code','Cooling.ID','Cooling.Status',
                                       'Inservice.Month','Inservice.Year',
                                       'Cooling.Type.1','Cooling.Type.2') %in% names(cooling))) 
  {
    stop("data must be a data.frame with columns 'Plant.Code','Cooling.ID','Cooling.Status',
                                       'Inservice.Month','Inservice.Year',
                                       'Cooling.Type.1','Cooling.Type.2' for this function to continue")
  }
  }else{
    if(!is.data.frame(cooling) || any(!c('Plant.Code','Cooling.ID','Cooling.Status',
                                         'Inservice.Date',
                                         'Cooling.Type.1','Cooling.Type.2') %in% names(cooling))) 
    {
      stop("data must be a data.frame with columns 'Plant.Code','Cooling.ID','Cooling.Status',
                                       'Inservice.Date',
                                       'Cooling.Type.1','Cooling.Type.2' for this function to continue")
    }
    if (is.na(as.POSIXct(cooling$Inservice.Date[1],format="%Y-%m-%d"))){
      cooling$Inservice.Date<-as.POSIXct(cooling$Inservice.Date,format="%d-%b-%y")
      cooling$Inservice.Year<-as.integer(format(cooling$Inservice.Date,format="%Y"))
      cooling$Inservice.Month<-as.integer(format(cooling$Inservice.Date,format="%m"))
    }else{
     cooling$Inservice.Date<-as.POSIXct(cooling$Inservice.Date,format="%Y-%m-%d")
    cooling$Inservice.Year<-as.integer(format(cooling$Inservice.Date,format="%Y"))
    cooling$Inservice.Month<-as.integer(format(cooling$Inservice.Date,format="%m")) 
    }

  }
  
  #Test variable types
  testthat::test_that("Cooling data variable types are correct",
            {
              testthat::expect_true(is.numeric(cooling$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(cooling$Cooling.ID), "Error wrong variable type. Cooling.ID is a character string.")
              testthat::expect_true(is.character(cooling$Cooling.Type.1), "Error wrong variable type. Cooling.Type.1 is a character string.")
              testthat::expect_true(is.character(cooling$Cooling.Type.2), "Error wrong variable type. Cooling.Type.2 is a character string.")
              
              
            }
  )
  
  return(cooling)
}
