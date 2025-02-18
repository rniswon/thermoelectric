% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkANYmissingNGCC.R
\name{checkANYmissingNGCC}
\alias{checkANYmissingNGCC}
\title{checkANYmissingNGCC}
\usage{
checkANYmissingNGCC(
  generation.data,
  boilerFuelData,
  gen_fuel_data,
  analysisYear
)
}
\arguments{
\item{generation.data}{data.frame with Plant.Code, Reported.Prime.Mover_923 
(Reported.Prime.Mover from the `generation.data` table in `inputData.list`) and 
Net.Generation(month) from EIA Form-923 "Page 4 Generator Data"}

\item{boilerFuelData}{data.frame with Plant.Code, Reported.Prime.Mover_bf.923 
(Reported.Prime.Mover from the `boilerFuelData` table in `inputData.list`) and 
Quantity.Of.Fuel.Consumed.(month) from EIA Form-923 "Page 3 Boiler Fuel Data"}

\item{gen_fuel_data}{data.frame stored in `inputData.list$gen_fuel_data` from EIA Form-923
"Page 1 Generation and Fuel Data"}

\item{analysisYear}{numeric value indicating the year of EIA data present in the `inputData.list`.}
}
\value{
numeric vector of Natural Gas Combined Cycle Plant.Codes with inconsitent data reporting
in the EIA Form-923 across "Page 3 Boiler Fuel Data", "Page 4 Generator Data", and "Page 1 
Generation and Fuel Data".  If data is present on "Page 1 Generation and Fuel Data" and not in
"Page 3 Boiler Fuel Data" and/or "Page 4 Generator Data", the boiler-generator association will take
the form Plant.Code^NGCC and  the `condenserDuty` package will calculate condenser duty using 
"Page 1 Generation and Fuel Data"
}
\description{
Finds natural gas combined cycle (NGCC) units that partially report Fuel Consumption
and/or Net Generation in the EIA Form-923 "Page 3 Boiler Fuel Data" and "Page 4 Generator Data"
but report all Fuel Consumption and Net Generation for the NGCC unit on EIA Form-923
"Page 1 Generation and Fuel Data".  \cr \cr 
This function looks for any 1 month of missing Fuel Consumption 
and Net Generation data which distiguishes it from `checkMissingNGCC()`.  In the year 2014, 
Fuel Consumption and Net Generation data reporting standards shifted mid-year resulting in all monthly
data being reported on EIA Form-923 "Page 3 Boiler Fuel Data" and "Page 4 Generator Data" for the
last few months of the year, but only partial reporting on EIA Form-923 "Page 3 Boiler Fuel Data" 
and "Page 4 Generator Data" in the earlier months with full reporting on EIA Form-923
"Page 1 Generation and Fuel Data". This function addresses this data reporting anomaly.\cr \cr  
Plant-Reported.Prime.Mover level bogens will be assigned in the form Plant.Code^NGCC. \cr \cr
}
\examples{
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
          

#add required columns Reported.Prime.Mover_bf.923 and Reported.Prime.Mover_923
boilerFuelData$Reported.Prime.Mover_bf.923<-boilerFuelData$Reported.Prime.Mover
generation.data$Reported.Prime.Mover_923<-generation.data$Reported.Prime.Mover

#set analysis Year
analysisYear<-eia_year

checkANYmissingNGCC(generation.data, boilerFuelData, gen_fuel_data,analysisYear)
}
