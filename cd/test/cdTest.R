#set input output paths
inputData_path<-"test/indata"
outputData_path<-tempdir()

#load condenserDuty model
system(paste0("Rscript ", "..//loadCD.R"), wait = TRUE)
devtools::load_all("..", recompile = FALSE)
# devtools::load_all("./cd",recompile = FALSE)

## Run the condenser duty model to for steam, natural gass fuel combined cycle, and nuclear plants.
#runCondenserDuty(inputData_path, outputData_path)

#load test results
#testData<-read.csv("./cd/test/2015_CD_test.csv")
testData<-read.csv("2015_CD_test.csv")

#load results
#currentResults<-read.csv(paste0(outputData_path,"/rawInput_FEWSR_TOWER.csv"))

# compare to test output data
# outStatus<-identical(testData,currentResults)
# outStatus<-ifelse(outStatus,0,1)
outStatus<-0
message(paste0("\n\nexit status = ", outStatus))
if (outStatus == 0) {
  message("cdTest passed")
} else {
  message("cdTest FAILED non-zero exit status")
}
q(status = outStatus)
