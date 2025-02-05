#'@title import_generator_data
#'@description import and format for use the 
#'             ~/thermoelectric-water-consumption-models/CondenserDutyModel/Data/2015_GeneratorData.csv
#'              file \\cr \\cr
#'Executed By: CondenserDutyModel_new.R \\cr
#'@param generator_data_file_path path to ~/thermoelectric-water-consumption-models/
#'                                CondenserDutyModel/Data/2015_GeneratorData.csv
#'@return `generator_data` formatted plant generator data
#'@examples
#'import_generator_data(here('CondenserDutyModel','Data','2015_GeneratorData.csv'))

import_generator_data <- function(generator_data)
{
  # if(!file.exists(generator_data_file_path)) 
  # {
  #   stop("enter valid file path")
  # }
  # path <- generator_data_file_path
  # generator_data_header <- read.csv(path,header=T,stringsAsFactors = F,nrows=5)
  # generator_data <- read.csv(path,header=T,stringsAsFactors = F,skip=5)
  if(!is.data.frame(generator_data) || any(!c('Utility.ID','Utility.Name','Plant.Code','Plant.Name','Generator.ID','Prime.Mover','Unit.Code') %in% names(generator_data))) 
  {
    stop("data must be a data.frame with columns 'Utility.ID','Utility.Name','Plant.Code','Plant.Name','Generator.ID','Prime.Mover','Unit.Code' for this function to continue")
  }
  
  
  #Test variable types
  test_that("Generator data variable types are correct",
            {
              expect_true(is.character(generator_data$Plant.Name), "Error wrong variable type. Plant.Name is a character string.")
              expect_true(is.numeric(generator_data$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              expect_true(is.character(generator_data$Generator.ID), "Error wrong variable type. Generator.ID is a character string.")
              expect_true(is.character(generator_data$Prime.Mover), "Error wrong variable type. Prime.Mover is a character string.")
              expect_true(is.character(generator_data$Unit.Code), "Error wrong variable type. Unit.Code is a character string.")
              
            }
  )
  
  return(generator_data)
}
