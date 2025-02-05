import_bocoo_data <- function(bocoo)
{
  
  if(!is.data.frame(bocoo) || any(!c('Plant.Code', 'Plant.Name', 'Boiler.ID','Cooling.ID') %in% names(bocoo))) 
  {
    stop("data must be a data.frame with columns 'Plant.Code', 'Plant.Name', 'Boiler.ID','Cooling.ID' for this function to continue")
  }
  
  #Test variable types
  test_that("Bocoo data variable types are correct",
            {
              expect_true(is.character(bocoo$Plant.Name),"Error wrong variable type. Plant.Name is a character string.")
              expect_true(is.numeric(bocoo$Plant.Code),"Error wrong variable type. Plant.Code is a numeric variable.")
              expect_true(is.character(bocoo$Boiler.ID), "Error wrong variable type. Boiler.ID is a character string.")
              expect_true(is.character(bocoo$Cooling.ID), "Error wrong variable type. Cooling.ID is a character string.")
              
            }
  )
  #Return imported Bocoo Data
  return(bocoo)
}
