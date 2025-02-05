context("Import Error prepCondenserDuty")

testthat::test_that("Import needs Edit",{
  
  testthat::skip_on_cran()
  
  
  #set arguments
  dest<-tempdir()
  eia_year<-2015
  path_InputData_Metafile<-paste0(dest,.Platform$file.sep,"UserControlCrosswalk2015.xlsx")
  path_EIAInputData<-paste0(dest,.Platform$file.sep,eia_year)
  outputCSV<-FALSE
  path_outputCSV<-dest
  
  #use crosswalk2015 object
  useStandardCrosswalk<-F
  prepCondenserDuty<-T
  
  #create bad crosswalk file
  data("crosswalk2015")
  crosswalk2015<-crosswalk2015[3:nrow(crosswalk2015),]
  openxlsx::write.xlsx(crosswalk2015,file=path_InputData_Metafile,rowNames=F,overwrite = T)
  
  #pull data from web
  eia_webpull(eia_year,dest)
  
  #get test file
  data("test_prepCondenserDuty")
  
  #run import using crosswalk2015
  inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,
                                 path_outputCSV,prepCondenserDuty = T,eia_year=eia_year,
                                 useStandardCrosswalk=useStandardCrosswalk)
  
  #test identical
  testthat::expect_true(identical(inputData.list,test_prepCondenserDuty))
  
  
  
})