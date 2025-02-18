% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkDecember.R
\name{checkDecember}
\alias{checkDecember}
\title{checkDecember}
\usage{
checkDecember(
  FuelHeatByLine,
  NetGenByLine,
  gen_fuel_data,
  plantList,
  select_RPM
)
}
\arguments{
\item{FuelHeatByLine}{data.frame with Plant.Code, Reported.Prime.Mover_bf.923 
(Reported.Prime.Mover from the `boilerFuelData` table in `inputData.list`) and Fuel.Heat.(month)
from EIA Form-923 "Page 3 Boiler Fuel Data"}

\item{NetGenByLine}{data.frame with Plant.Code, Reported.Prime.Mover 
(Reported.Prime.Mover from the `generation.data` table in `inputData.list`) and 
Net.Generation(month) from EIA Form-923 "Page 4 Generator Data"}

\item{gen_fuel_data}{data.frame stored in `inputData.list$gen_fuel_data` from EIA Form-923
"Page 1 Generation and Fuel Data"}

\item{plantList}{numeric vector of Plant.Codes to consider}

\item{select_RPM}{vector of character strings indicating which Reported.Prime.Mover types should be
included in the boiler-generator association output.}
}
\value{
numeric vector of Plant.Codes that report annual Fuel Consumption and/or Net Generation
in the month of December.
}
\description{
Finds plants that report all Fuel Consumption and/or Net Generation for the
year in the month of December.  Monthly data for these plants can be found on EIA Form-923
"Page 1 Generation and Fuel Data".  Plant-Reported.Prime.Mover level bogens will be assigned
in the form Plant.Code^NGCC for natural gas combined cycle units or Plant.Code^ST for
steam units. \cr \cr
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
          
#calculate Fuel Heat at the Boiler.ID level
FuelHeatByLine<-boilerFuelData
for(m in months){
fuelHeatstr<-paste0("FuelHeatByLine<-FuelHeatByLine \%>\% dplyr::mutate(Fuel.Heat.",m,"=Quantity.Of.Fuel.Consumed.",m,"*MMbtu.Per.Unit.",m,")")
eval(parse(text=fuelHeatstr))
}

#add required column Reported.Prime.Mover_bf.923
FuelHeatByLine$Reported.Prime.Mover_bf.923<-FuelHeatByLine$Reported.Prime.Mover

#get masterPlantList
data("masterPlantList")
plantList<-masterPlantList[masterPlantList$year==2015,]$Plant.Code

#define NetGenByLine
NetGenByLine<-generation.data

checkDecember(FuelHeatByLine,NetGenByLine,gen_fuel_data,
             plantList,select_RPM=c("CA", "CS", "CT", "ST",NA))
}
