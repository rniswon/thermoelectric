context("Import Working Check")

testthat::test_that("Import working",{
  
  testthat::skip_on_cran()
  
  
  #set arguments
  dest<-tempdir()
  eia_year<-2015
  path_InputData_Metafile<-paste0(dest,.Platform$file.sep,"UserControlCrosswalk2015.xlsx")
  path_EIAInputData<-paste0(dest,.Platform$file.sep,eia_year)
  outputCSV<-FALSE
  path_outputCSV<-dest
  
  #use crosswalk2015 object
  useStandardCrosswalk<-T
  prepCondenserDuty<-T
  
  #pull data from web
  eia_webpull(eia_year,dest)
  
  
  #run import using crosswalk2015
  inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,
                                 path_outputCSV,prepCondenserDuty = T,eia_year=eia_year,
                                 useStandardCrosswalk=useStandardCrosswalk)
  
  
  #test identical
  testthat::expect_true(class(inputData.list)=="list")
  testthat::expect_true(length(inputData.list)==11)
  testthat::expect_true(identical(order(names(inputData.list)),order(c("bogen", "bocoo", "gen_860", "retiredGenerators", "generation.data", 
                                                 "boilerFuelData", "gen_fuel_data", "boilerDesignData", "cooling", 
                                                 "plantInfo", "cooling8D"))))
  testthat::expect_true(unique(lapply(inputData.list,class))=="data.frame")
  
  
    
  
})

