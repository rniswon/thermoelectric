% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkBadFuel.R
\name{checkBadFuel}
\alias{checkBadFuel}
\title{checkBadFuel}
\usage{
checkBadFuel(data.out, Reported.Prime.Mover_cols)
}
\arguments{
\item{data.out}{data.frame with Plant.Code, bogen, Reported.Prime.Mover data in 1 or more columns
specified by `Reported.Prime.Mover_cols` and Fuel.Heat.(month).}

\item{Reported.Prime.Mover_cols}{vector of character strings indicating columns in which 
Reported.Prime.Mover data exists.}
}
\value{
numeric vector of Plant.Codes with questionable Reported.Fuel.Type.Codes to be flagged
with "flag.NGCC.badFuel" in `bogen_associate()`
}
\description{
Find natural gas combined cycle (NGCC) units that report fuel types in \cr \cr
c("BIT","AB","ANT","GEO","LIG","MSB","MSN","OBS","PC","PUR","RC",
"SGC","SGP","SLW","SUB","TDF","WAT","WC","WDS","WH","WND") and report Fuel Heat>0.\cr \cr
These are unusual or erroneously reported fuel types for NGCC plants. \cr \cr
}
\examples{
# while this example demonstrates the usage of checkBadFuel()
# it is not recommended that the EIA Form-923 "Page 3 Boiler Fuel Data"
# and EIA Form-923 "Page 4 Generator Data" be merged simply by applying the
# EIA Form-860 Boiler Generator association table as it is incomplete and will
# result in fewer correct boiler-generator associations than the use of the
# executeBogenAssoc() function which applies more complex methods.

#set arguments
dest<-tempdir()
eia_year<-2015
path_InputData_Metafile<-paste0(dest,.Platform$file.sep,"UserControlCrosswalk2015.xlsx")
path_EIAInputData<-paste0(dest,.Platform$file.sep,eia_year)
outputCSV<-FALSE
path_outputCSV<-dest

#use crosswalk2015 object
useStandardCrosswalk<-TRUE
prepCondenserDuty<-TRUE

#pull data from web
importThermoEIA::eia_webpull(eia_year,dest)


#run import using crosswalk2015
inputData.list<-importThermoEIA::import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,
                              path_outputCSV,prepCondenserDuty = TRUE,eia_year=eia_year,
                              useStandardCrosswalk=useStandardCrosswalk)

#extract elements of the inputData.list
list2env(inputData.list,envir = parent.frame())

#define months
months<-c("January","February","March","April","May",
         "June","July","August","September","October","November","December")

#calculate Fuel Heat at the Boiler.ID level
boilerFuelData<-boilerFuelData
for(m in months){
 fuelHeatstr<-paste0("boilerFuelData<-boilerFuelData \%>\% dplyr::mutate(Fuel.Heat.",m,"=Quantity.Of.Fuel.Consumed.",m,"*MMbtu.Per.Unit.",m,")")
 eval(parse(text=fuelHeatstr))
}

#join boiler and generator data using the EIA boiler-generator association table
data.out<-merge(bogen,generation.data,by=c("Plant.Code","Generator.ID"))
data.out<-merge(data.out,boilerFuelData,by=c("Plant.Code","Boiler.ID"))


#set existing Reported.Prime.Movers
Reported.Prime.Mover_cols <- c("Reported.Prime.Mover.x","Reported.Prime.Mover.y")


#run check bad fuel
checkBadFuel(data.out,Reported.Prime.Mover_cols)

}
