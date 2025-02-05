#'@title import_8D
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA Cooling Operations table \cr \cr
#'@param cooling8D EIA Cooling Operations table import with standardized column names found in 
#'                 \code{\link{crosswalk2015}} where scriptTableName = "cooling8D"
#'@return `cooling8D` formatted EIA Cooling Operations table
#'@export

import_8D<-function(cooling8D){
  if(!is.data.frame(cooling8D) || any(!c('Plant.Code','Cooling.ID',
                                         'Withdrawal_GPM','Discharge_GPM','Consumption_GPM') %in% names(cooling8D))) 
  {
    stop("data must be a data.frame with columns 'Plant.Code','Cooling.ID',
                                         'Withdrawal_GPM','Discharge_GPM','Consumption_GPM' for this function to continue")
  }
  
  if ("Service_hrs" %in% names(cooling8D)){
    scriptnames<-c('Withdrawal_GPM','Discharge_GPM','Consumption_GPM','Service_hrs') 
  }else{
  scriptnames<-c('Withdrawal_GPM','Discharge_GPM','Consumption_GPM')  
  }
  
  
  #missing data
  cooling8D<-removeMissing(cooling8D,cols = c(1:length(cooling8D)), missingFlag = ".")
  cooling8D[scriptnames] <- sapply(cooling8D[scriptnames], function(x){gsub("\\,","",x)})
  
  cooling8D[scriptnames] <- sapply(cooling8D[scriptnames],as.numeric)
  
  #Test variable types
  testthat::test_that("8D cooling data variable types are correct",
            {
              testthat::expect_true(is.numeric(cooling8D$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(cooling8D$Cooling.ID), "Error wrong variable type. Cooling.ID is a character string.")
              testthat::expect_true(is.numeric(cooling8D$Withdrawal_GPM), "Error wrong variable type. Withdrawal_GPM is a numeric variable.")
              testthat::expect_true(is.numeric(cooling8D$Consumption_GPM), "Error wrong variable type. Consumption_GPM is a numeric variable.")
    
              
            }
  )
  
  return(cooling8D)
}