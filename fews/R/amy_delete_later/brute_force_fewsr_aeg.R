
library(rio)
library(readxl)
library(openxlsx)
library(ggplot2)
library(tcltk)
library(gWidgets2)
library(gWidgets2tcltk)
library(data.table)
library(grDevices)
library(utils)
library(stringi)
library(fpCompare)
library(fastmatch)
library(assertthat)
library(methods)


# Bring in the input data, do this for lake, pond, river
setwd('../../Data') #AEG
file = 'FEW_BIG_Lake_plants_input.csv'
sheet = 1

#define minheatload, MW/acre
minheadload <- 0.1

#define wind1, wind2, wind3

# convert elevation to meters
# calculate the pressure in mbar
Kelvin <- 273.15 # K
averagedcoefficients <- data.table(coefficient = c(4119.6015341, -48.7753617, -339.1569784, -359.7975614, 0.2536791, 12.5618961, 46.1229360), units = c("constant", "x T", "x W", "x MW/a", "x T^2" , "x W^2", "x (MW/a)^2"))

## Input environmental data and heat loading
pond <- fewsronly[, c(1, 5)] # subset of fewsronly with the Plant ID and Pond Area (acres) data only

duty <- fewsronly[, c(1, 6:17)] # subset of fewsronly with the Plant ID and Duty [Added heat load (MMBtu)] data only

db <- fewsronly[, c(1, 18:29)] # subset of fewsronly with the Plant ID and Dry bulb air temperature Ta (deg C) data only

wb <- fewsronly[, c(1, 30:41)] # subset of fewsronly with the Plant ID and Wet bulb air temperature Twb (deg C) data only

# The natural or upstream water temperature is a critical input but it is difficult to estimate accurately from existing data.
wt <- fewsronly[, c(1, 42:53)] # subset of fewsronly with the Plant ID and Natural water temperature T (deg C) data only

ws <- fewsronly[, c(1, 54:65)] # subset of fewsronly with the Plant ID and Wind speed at 2m W (mph) data only

# Wind Functions of the form f(W) = a + bW + cW2
tablewind <- data.table(type = c("cooling lake", "cooling lake", "cooling ponds", "big lake", "fewsronly channel", "stream", "lake"), reference = c("Harbeck 1964", "Ward 1980", "Brady et al. 1969", "Anderson 1954", "Fulford and Sturm 1984", "Gulliver & Stefan 1986", "Webster & Sherman 1995"), a = c(0.000000, 0.000000, 2.465994, 0.000000, 3.200000, 2.960000, 1.040000), b = c(1.461666, 1.352899, 0.000000, 0.920000, 0.800000, 0.640000, 1.050000), c = c(0.0000000, 0.0000000, 0.1233955, 0.0000000, 0.0000000, 0.0000000, 0.0000000), "maximum W (m/s)" = list("- -", "- -", "- -", 8.25000000000000000000, 3.75000000000000000000, 5.75000000000000000000, 5.00000000000000000000))

# Constants used in formulae			SOURCE for constant
source <- c("http://www.nist.gov/pml/wmd/metric/upload/SP1038.pdf", "ditto", "ditto", "ditto", "ditto", "calculated", "Ward 1980", "Ward 1980", "Ward 1980", "Ward 1980")
# also see Ward equation 1 2 3 14

constants <- c(252.164401, 3785.412, 453.59237, 4.184, 1055.056, 252.164435946463, 0.97, 0.00000011710000000000, 0.24, 0.622, 991.61663430384, 86400)
units <- c("calorie per Btu", "cc/gal", "grams per pound", "Joules per calorie", "Joules per Btu", "Calories per Btu", "per day", rep_len("unitless", 3), "mbar", "seconds per day")
names <- c(rep_len(" ", 6), "epsilonr", "sigma", "cp", "epsilon", "p", " ")
constants <- data.table(names, constants, units)

# This method was originally designed to be applied to monthly data.  Conversion to a shorter time step is probably not realistic.
# Days per month
days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthdays <- data.table(matrix(rep(days, each = nrow(fewsronly), times = nrow(fewsronly)), nrow = nrow(fewsronly), ncol = 12))
setnames(monthdays, c("jan_days per month", "feb_days per month", "mar_days per month", "apr_days per month", "may_days per month", "jun_days per month", "jul_days per month", "aug_days per month", "sep_days per month", "oct_days per month", "nov_days per month", "dec_days per month"))

# Added heat load deltaH (MW/acre)
# The monthly average heat loading is an important input. The added heat can be calculated by dividing the monthly fuel consumption in thermal megawatt-hours by the number of hours in the month, then multiplying by the percentage in cell S21.  For dedicated cooling ponds, the area in question is that of the pond; for plants that create a thermal plume in a fewsronly or large lake, the appropriate area is the area over which the average temperature increase is equal to the estimated heating in cells L12-L23
# Added heat loading is less than 0.5 MWt/acre for most lakes and fewsronlys, but could be as high as 2 for some dedicated cooling ponds.  A value in this cell will be copied into G13-G23. However, the user can provide monthly heat loadings. The added heat load is shown in different units in column U.
pond[, paste0("Pond Area (acres)", 2:12) := rep(pond[, 2], 11)]
addedheatload <- data.table(duty, pond, monthdays)
addedheatload[, 14 := NULL]
setnames(addedheatload, 2:length(addedheatload), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
setkey(addedheatload, Plant_ID)
# Sources 7 & 8 begin

