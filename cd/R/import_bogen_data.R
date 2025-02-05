import_bogen_data <- function(bogen)
{
 
   if(!is.data.frame(bogen) || any(!c('Plant.Code','Boiler.ID','Generator.ID') %in% names(bogen))) 
  {
    stop("data must be a data.frame with columns 'Plant.Code','Boiler.ID','Generator.ID' for this function to continue")
  }
  
  #Test check variable types
  test_that("Bogen data variable types are correct",
            {
              expect_true(is.numeric(bogen$Plant.Code),"Error wrong variable type. Plant.Code is a numeric variable.")
              expect_true(is.character(bogen$Boiler.ID),"Error wrong variable type. Boiler.ID is a character string.")
              expect_true(is.character(bogen$Generator.ID),"Error wrong variable type. Generator.ID is a character string.")
              
            }
  )
  #Return imported bogen data
  return(bogen)
}
