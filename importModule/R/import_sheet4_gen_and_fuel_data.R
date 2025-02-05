#'@title import_sheet4_gen_and_fuel_data
#'@description import and format for use the 
#'             ~/thermoelectric-water-consumption-models/CondenserDutyModel/Data/2015_GenerationData.csv
#'              file \cr \cr
#'Executed By: CondenserDutyModel_new.R \cr
#'@param sheet4_gen_fuel_data path to ~/thermoelectric-water-consumption-models/
#'                                CondenserDutyModel/Data/2015_GenerationData.csv
#'@return `sheet4_gen_fuel_data` formatted plant generator data
#'@examples
#'import_sheet4_gen_and_fuel_data(paste0(inputData_path,'/2015_GenerationData.csv'))


import_sheet4_gen_and_fuel_data <- function(sheet4_gen_fuel_data)
{
  # if(!file.exists(sheet4_gen_data)) 
  # {
  #   stop("enter valid file path")
  # }
  # 
  # path <- sheet4_gen_data
  # sheet4_gen_fuel_data_header <- read.csv(path,header=T,stringsAsFactors = F,nrow=5)
  # sheet4_gen_fuel_data <- read.csv(path,header=T,stringsAsFactors = F,skip=5)
  
  if(!is.data.frame(sheet4_gen_fuel_data) || any(!c('Plant.Id',
                                                    'Net.Generation.February','Net.Generation.March','Net.Generation.April','Net.Generation.May',
                                                    'Net.Generation.June','Net.Generation.July','Net.Generation.August','Net.Generation.October',
                                                    'Net.Generation.November','Net.Generation.December') %in% names(sheet4_gen_fuel_data)))
  {
    stop("data must be a data.frame with columns 'Plant.Id',
         'Net.Generation.February','Net.Generation.March','Net.Generation.April','Net.Generation.May',
         'Net.Generation.June','Net.Generation.July','Net.Generation.August','Net.Generation.October',
         'Net.Generation.November','Net.Generation.December' for this function to continue")
  }
  
  names(sheet4_gen_fuel_data)[names(sheet4_gen_fuel_data)=='Plant.Id'] <- "Plant.Code"
  names(sheet4_gen_fuel_data)[names(sheet4_gen_fuel_data)=='Generator.Id'] <- "Generator.ID"
  

  
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
  sheet4_gen_fuel_data<-removeMissing(sheet4_gen_fuel_data, cols = which(names(sheet4_gen_fuel_data) %in% scriptnames), missingFlag = ".")
  
  sheet4_gen_fuel_data[scriptnames] <- sapply(sheet4_gen_fuel_data[scriptnames], function(x){gsub("\\,","",x)})
  sheet4_gen_fuel_data[scriptnames] <- sapply(sheet4_gen_fuel_data[scriptnames],as.numeric)

  test_for_numeric_variables_data_frame<-function(x){all(sapply(x,is.numeric))}
  
  #Test variable types
  test_that("Sheet4 generation and fuel variable types are correct",
            {
              expect_true(is.character(sheet4_gen_fuel_data$Plant.Name), "Error wrong variable type. Plant.Name is a character string.")
              expect_true(is.numeric(sheet4_gen_fuel_data$Plant.Code), "Error wrong variable type. Plant.Code is a numeric variable.")
              expect_true(is.character(sheet4_gen_fuel_data$Generator.ID), "Error wrong variable type. Generator.ID is a character string.")
              expect_true(test_for_numeric_variables_data_frame(sheet4_gen_fuel_data[scriptnames]), "Error wrong variable type. Generation and fuel consumption are numeric variables.")
            }
  )
  
  return(sheet4_gen_fuel_data)
}
