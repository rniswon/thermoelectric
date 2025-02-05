#'@title import_boiler_design_data
#'@description import and format for use by the EIAplantAssociation and condenserDuty packages the 
#'             EIA Boiler Design table \cr \cr
#'@param boilerDesignData EIA Boiler Design table import with standardized column names found in 
#'                 \code{\link{crosswalk2015}} where scriptTableName = "boilerDesignData"
#'@return `boilerDesignData` formatted Boiler Design table
#'@export

import_boiler_design_data <- function(boilerDesignData)
{

  if(!is.data.frame(boilerDesignData) || any(!c('Plant.Code','Boiler.ID') %in% names(boilerDesignData))) 
  {
    stop("data must be a data.frame with columns 'Plant.Code', 'Boiler.ID' for this function to continue")
  }
  
  boilerDesignData<-removeMissing(boilerDesignData,cols =which(names(boilerDesignData) %in% c("Efficiency.100..Load","Efficiency.50..Load")), missingFlag = "")
  boilerDesignData[c("Efficiency.100..Load","Efficiency.50..Load")] <- sapply(boilerDesignData[c("Efficiency.100..Load", "Efficiency.50..Load")], function(x){as.numeric(gsub("%","",as.character(x)))})


  test_for_numeric_variables_data_frame<-function(x){all(sapply(x,is.numeric))}
  
  #Test variable types
  testthat::test_that("Boiler design data variable types are correct",
            {
              testthat::expect_true(is.numeric(boilerDesignData$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              testthat::expect_true(is.character(boilerDesignData$Boiler.ID), "Error wrong variable type. Boiler.ID is a character string.")
              testthat::expect_true(test_for_numeric_variables_data_frame(boilerDesignData[c("Efficiency.100..Load", "Efficiency.50..Load")]), "Error wrong variable type. Boiler 100% and 50% load efficiency are numeric variables.")
            }
  )
  
  
  
  return(boilerDesignData)
}
