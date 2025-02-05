#'@title import_boiler_design_data
#'@description import and format for use the 
#'             ~/thermoelectric-water-consumption-models/CondenserDutyModel/Data/2015_BoilerDesignInfo.csv
#'              file \cr \cr
#'Executed By: CondenserDutyModel_new.R \cr
#'@param boiler_design_data path to ~/thermoelectric-water-consumption-models/
#'                                CondenserDutyModel/Data/2015_BoilerDesignInfo.csv
#'@return `boiler_design_data` formatted 2015 Form EIA-860 Data - Schedule 6C, 
#'                      'Boiler Information - Design Parameters' data
#'@examples
#'import_boiler_design_data(paste0(inputData_path,'/2015_BoilerDesignInfo.csv'))


import_boiler_design_data <- function(boiler_design_data)
{
  # if(!file.exists(boiler_design_data)) 
  # {
  #   stop("enter valid file path")
  # }
  # path <- boiler_design_data
  # boiler_design_data_header <- read.csv(path,header=T,stringsAsFactors = F,nrow=5)
  # boiler_design_data <- read.csv(path,header=T,stringsAsFactors = F,skip=5)
  if(!is.data.frame(boiler_design_data) || any(!c('Plant.Code','Boiler.ID') %in% names(boiler_design_data))) 
  {
    stop("data must be a data.frame with columns 'Plant.Code', 'Boiler.ID' for this function to continue")
  }
  
  #boiler_design_data[,c("Efficiency.100..Load","Efficiency.50..Load")][boiler_design_data[,c("Efficiency.100..Load","Efficiency.50..Load")]==""] <- NA
  boiler_design_data<-removeMissing(boiler_design_data,cols =which(names(boiler_design_data) %in% c("Efficiency.100..Load","Efficiency.50..Load")), missingFlag = "")
  
  #boiler_design_data[c("Efficiency.100..Load", "Efficiency.50..Load")] <- sapply(boiler_design_data[c("Efficiency.100..Load", "Efficiency.50..Load")], function(x){as.numeric(gsub("%","",as.character(x)))})
  boiler_design_data[c("Efficiency.100..Load","Efficiency.50..Load")] <- sapply(boiler_design_data[c("Efficiency.100..Load", "Efficiency.50..Load")], function(x){as.numeric(gsub("%","",as.character(x)))})

 # boiler_design_data[c("Efficiency.100..Load", "Efficiency.50..Load")] <- sapply(boiler_design_data[c("Efficiency.100..Load", "Efficiency.50..Load")], function(x){x/100})

  test_for_numeric_variables_data_frame<-function(x){all(sapply(x,is.numeric))}
  
  #Test variable types
  test_that("Boiler design data variable types are correct",
            {
              expect_true(is.character(boiler_design_data$Plant.Name), "Error wrong variable type. Plant.Name is a character string.")
              expect_true(is.numeric(boiler_design_data$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              expect_true(is.character(boiler_design_data$Boiler.ID), "Error wrong variable type. Boiler.ID is a character string.")
              expect_true(test_for_numeric_variables_data_frame(boiler_design_data[c("Efficiency.100..Load", "Efficiency.50..Load")]), "Error wrong variable type. Boiler 100% and 50% load efficiency are numeric variables.")
            }
  )
  
  
  
  return(boiler_design_data)
}
