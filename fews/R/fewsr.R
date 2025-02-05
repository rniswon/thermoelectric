#' FEWS Multiplant plants -- lake, pond, river (version 3.104) -- non-
#' interactive file input
#'
#' Draft Model of Forced Evaporation from a Water Surface, with user choice of
#'     wind function
#' Multiplant Lake/Pond/River Version 3.104 - 2012/12/15
#'
#' @param file Path of input file chosen by the user [.xls(x) or .csv]
#' @param sheet Sheet number or sheet name for the input file
#' @param type The type of surface water feature (lake, pond, river)
#' @param output The output format of the resulting spreadsheet
#'          (.xlsx, .csv, or both)
#'
#' @return spreadsheet with the output parameters and a plot of the average
#'          evaporation per month
#'
#' @author Irucka Embry, Timothy H. Diehl (Microsoft Excel spreadsheets)
#'
#'
#' @references
#' \enumerate{
#'    \item Withdrawal and consumption of water by thermoelectric power plants in the United States, 2010: A product of the USGS National Water Census and the USGS National Streamflow Information Program By: Timothy H. Diehl and Melissa A. Harris, Scientific Investigations Report 2014-5184. See \url{http://pubs.er.usgs.gov/publication/sir20145184}.
#'    \item Methods for estimating water consumption for thermoelectric power plants in the United States: USGS National Water Census and National Streamflow Information Program By: Timothy H. Diehl, Melissa Harris, Jennifer C. Murphy, Susan S. Hutson, and David E. Ladd, Scientific Investigations Report 2013-5188. See \url{http://pubs.er.usgs.gov/publication/sir20135188}.
#'}
#'
#'
#' @source
#' \enumerate{
#'    \item r - How can I check if a file is empty? - Stack Overflow answered by Konrad Rudolph and edited by Geekuna Matata on Apr 23 2014. See \url{http://stackoverflow.com/questions/23254002/how-can-i-check-if-a-file-is-empty}.
#'    \item r - Better error message for stopifnot? - Stack Overflow answered by Andrie on Dec 1 2011. See \url{http://stackoverflow.com/questions/8343509/better-error-message-for-stopifnot}.
#'    \item RDocumentation: TclInterface {tcltk}. See \url{http://www.rdocumentation.org/packages/tcltk/versions/3.3.1/topics/TclInterface}.
#'    \item r - Convert column classes in data.table - Stack Overflow answered by Matt Dowle on Dec 27 2013. See \url{http://stackoverflow.com/questions/7813578/convert-column-classes-in-data-table}.
#'    \item r - removing columns with NA values only - Stack Overflow answered by David Arenburg on Sep 18 2014. See \url{http://stackoverflow.com/questions/25922385/removing-columns-with-na-values-only}.
#'    \item r - Reduce data.table by group with with different actions depending on column class? - Stack Overflow answered by Ananda Mahto on May 9 2014. See \url{http://stackoverflow.com/questions/23566343/reduce-data-table-by-group-with-with-different-actions-depending-on-column-class/23566488}.
#'    \item r - operations (+, -, /, *) on unequal-sized data.table - Stack Overflow asked and answered by iembry on Jul 24 2014 and Aug 14 2014, respectively. See \url{http://stackoverflow.com/questions/24940246/operations-on-unequal-sized-data-table}.
#'    \item R data.table operations with multiple groups in single data.table and outside function with lapply - Stack Overflow answered by eddi on Aug 12 2014. See \url{https://stackoverflow.com/questions/25273767/r-data-table-operations-with-multiple-groups-in-single-data-table-and-outside-fu}.
#'    \item r - ifelse data.table multiplication while keeping NA values as real - Stack Overflow answered by Mike.Gahan on Jul 25 2014. See \url{http://stackoverflow.com/questions/24965620/ifelse-data-table-multiplication-while-keeping-na-values-as-real}.
#'    \item r - How to replace NA values in a table *for selected columns*? data.frame, data.table - Stack Overflow edited BY eddi on Jan 19 2016 on Nov 17 2011. See \url{https://stackoverflow.com/questions/19379081/how-to-replace-na-values-in-a-table-for-selected-columns-data-frame-data-tab}.
#'    \item r - Summing across rows of a data.table for specifc columns - Stack Overflow answered by eddi on Feb 18 2014. See \url{http://stackoverflow.com/questions/21857679/summing-across-rows-of-a-data-table-for-specifc-columns/21859095}.
#'    \item r - Sum multiple columns - Stack Overflow answered by eddi on Jul 1 2013. See \url{http://stackoverflow.com/questions/17407506/sum-multiple-columns}.
#'    \item R summarizing multiple columns with data.table - Stack Overflow answered by Ricardo Saporta on May 13 2013. See \url{http://stackoverflow.com/questions/16513827/r-summarizing-multiple-columns-with-data-table/16513949}.
#'    \item performance Yearly comparison timeseries ggplot2 R - Stack Overflow answered and edited by Ruthger Righart on Mar 19 2015. See \url{http://stackoverflow.com/questions/29124724/yearly-comparison-timeseries-ggplot2-r/29147348}.
#'    \item r - ggplot2 line chart gives "geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?" - Stack Overflow answered by Mario B. on Mar 12 2015. See \url{http://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation}.
#'    \item James Wettenhall & Philippe Grosjean, File Open/Save dialogs in R tcltk, December 01, 2015. See \url{http://www.sciviews.org/recipes/tcltk/TclTk-file-open-save-dialogs/}.
#'    \item performance - R: need help to speed up creating new columns in a data.frame - Stack Overflow answered by Thell on Apr 26 2014 and edited by Thell on Apr 27 2014. See \url{http://stackoverflow.com/questions/23309300/r-need-help-to-speed-up-creating-new-columns-in-a-data-frame/23312458}.
#'    \item warnings - How do I get rid of the NOTE's generated by R CMD check when using for example ddply in my package? - Stack Overflow answered by shadow on Mar 4 2015 and edited by shadow on Mar 5 2015. See \url{https://stackoverflow.com/questions/28851812/how-do-i-get-rid-of-the-notes-generated-by-r-cmd-check-when-using-for-example-d}.
#'    \item How to check file extensions in R - Stack Overflow answered by lebatsnok on May 17 2014. See \url{http://stackoverflow.com/questions/23713284/how-to-check-file-extensions-in-r}.
#'    \item multiple output filenames in R - Stack Overflow asked and edited by Gabelins on Feb 1 2013. See \url{http://stackoverflow.com/questions/14651594/multiple-output-filenames-in-r}.
#'    \item r - Regex return file name, remove path and file extension - Stack Overflow answered and edited by Ananda Mahto on Feb 25 20134. See \url{http://stackoverflow.com/questions/15073753/regex-return-file-name-remove-path-and-file-extension/15073919}.
#'    \item r - Order data frame rows according to vector with specific order - Stack Overflow answered and edited by Edward on Aug 15 2012. See \url{https://stackoverflow.com/questions/11977102/order-data-frame-rows-according-to-vector-with-specific-order}.
#'    \item Global variables in packages in R - Stack Overflow answered by Greg Snow on Sep 26 2012. See \url{https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r/12605694}.
#' }
#'
#' @encoding UTF-8
#'
#'
#' @examples
#' \dontrun{
#' library("FEWSR")
#' # Examples to check the input file format
#'
#' # Copy and paste the following code into the R console if you
#' # wish to see the input file format for the Lake plants.
#' library("openxlsx")
#' openXL(file.path(file.path(system.file("extdata", "FEWS_BIG_Lake_plants_input.xlsx",
#' package = "FEWSR")))
#'   # opens the workbook using the default spreadsheet application
#'
#'
#' # Copy and paste the following code into the R console if you
#' # wish to see the input file format for the Pond plants.
#' library("openxlsx")
#' openXL(file.path(file.path(system.file("extdata", "FEWS_Pond_plants_input.xlsx",
#' package = "FEWSR")))
#'   # opens the workbook using the default spreadsheet application
#'
#'
#' # Copy and paste the following code into the R console if you
#' # wish to see the input file format for the River plants.
#' library("openxlsx")
#' openXL(file.path(file.path(system.file("extdata", "FEWS_River_plants_input.xlsx",
#' package = "FEWSR")))
#'   # opens the workbook using the default spreadsheet application
#'
#'
#'
#' # Examples to show different use cases
#' fewsr(file.path(system.file("extdata", "FEWS_Pond_plants_input.xlsx",
#' package = "FEWSR"), type = "pond", output = "csv"))
#' # The sheet number is 1 (default) and the surface water is pond
#'
#'
#' fewsr(file.path(system.file("extdata", "FEWS_River_plants_input.xlsx",
#' package = "FEWSR"), type = "river", output = "xlsx"))
#' # The sheet number is 1 (default) and the surface water is river
#'
#'
#' fewsr(file.path(system.file("extdata", "FEWS_BIG_Lake_plants_input.xlsx",
#' package = "FEWSR"), type = "lake", output = "both"))
#' # The sheet number is 1 (default) and the surface water is lake
#' }
#'
#'
#' @importFrom methods setPackageName
#' @import rio
#' @import readxl
#' @import openxlsx
#' @import ggplot2
#' @import tcltk
#' @import gWidgets2
#' @import gWidgets2tcltk
#' @import data.table
#' @import grDevices
#' @import utils
#' @import stringi
#' @import fpCompare
#' @import fastmatch
#' @import assertthat
#'
#' @export
fewsr <- function (file, sheet = 1, type = c("lake", "pond", "river"), output  = c("csv", "xlsx", "both")) {
# AEG-ponds-- diff input

# Check for file information
# assert_that(!(nchar(file) %==% 0 == TRUE), msg = "You did not choose a file. Please select a file again.")
# # Source 2 / provide a stop warning if no file was selected
#
# confirm <- gconfirm(toolkit = guiToolkit("tcltk"), msg = paste0("Do you want to select", " ", file, "?"), title = "Confirm", icon = "question")
#
# assert_that(!confirm == FALSE, msg = "You did not confirm the selection of the file. Please try again with a different file.")
# # Source 2 / provide a stop warning if the user wants to change the file
#
# assert_that(!file.info(file)$size %==% 0 == TRUE, msg = "Your file is empty. Please try again with a different file.")
# # Sources 1 & 2 / only process non-empty files and provide a stop warning if the input file is empty
#
# assert_that(any(has_extension(file, c("csv", "xlsx", "xls"))), msg = "Your file does not have a .csv or .xls(x) extension. Please try again with a different file.")
# # only process files that have a .csv, .xlsx, or .xls extension



# Input provides the following parameters: Plant ID, Elevation (feet), Pond Area (acres), Added heat load (MMBtu) Jan - Dec, Dry bulb air temperature Ta (deg C) Jan - Dec, Wet bulb air temperature Twb (deg C) Jan - Dec, Natural water temperature T (deg C) Jan - Dec, Wind speed at 2m W (mph) Jan - Dec.


# no visible binding for global variable NOTE
# Source 19 and 23 & data.table package begins
.global <- new.env()

setPackageName("FEWSR", .global)

.global$print = ""


Plant_ID <- a <- b <- Month <- Percent <- NULL

# as discussed in data.table package
# These are exported to prevent NOTEs from R CMD check, and checkUsage via compiler.
# But also exporting them makes it clear (to users and other packages) that data.table uses these as symbols.
# And NULL makes it clear (to the R's mask check on loading) that they're variables not functions.
# utils::globalVariables(c(".SD",".N")) was tried as well, but exporting seems better.
# So even though .BY doesn't appear in this file, it should still be NULL here and exported because it's
# defined in SDenv and can be used by users.
# Source 19 and 23 & data.table package ends


if (grepl("*.csv", file)) {
  fewsronly<-read.csv(file,header=F,col.names = seq(1,75,1))
  assertthat::assert_that(fewsronly[3, 1] == "Minimum heat loading", msg = "Please check that 'Minimum heat loading' is in Row 3, Column 1 in the input file. Please try again with a different file.")
  assertthat::assert_that(fewsronly[3, 1] == "Minimum heat loading", msg = "Please check that 'Minimum heat loading' is in Row 3, Column 1 in the input file. Please try again with a different file.")
  assertthat::assert_that(all(grepl("([0-9])", fewsronly[4, 1:4])), msg = "Please check that Row 4, Columns 1 - 4 are numeric in the input file. Please try again with a different file.")
  assertthat::assert_that(fewsronly[6, 1] == "Plant ID", msg = "Please check that 'Plant ID' is in Row 6, Column 1 in the input file. Please try again with a different file.")
  assertthat::assert_that(all(grepl("([0-9])", fewsronly[7, 2:63])), msg = "Please check that Row 7, Columns 2 - 63 are numeric in the input file. Please try again with a different file.")
  fewsronly <- fewsronly[-1, ]
  fewsronly <- data.table::setDT(fewsronly)




# fewsronly <- import(file, header = FALSE, na.strings = getOption("datatable.na.strings", c("na", "NA", "N/A", "#N/A", "-", "")))
#
# assert_that(fewsronly[3, 1] == "Minimum heat loading", msg = "Please check that 'Minimum heat loading' is in Row 3, Column 1 in the input file. Please try again with a different file.")
#
# assert_that(all(grepl("([0-9])", fewsronly[4, 1:4])), msg = "Please check that Row 4, Columns 1 - 4 are numeric in the input file. Please try again with a different file.")
#
# assert_that(fewsronly[6, 1] == "Plant ID", msg = "Please check that 'Plant ID' is in Row 6, Column 1 in the input file. Please try again with a different file.")
#
# assert_that(all(grepl("([0-9])", fewsronly[7, 2:63])), msg = "Please check that Row 7, Columns 2 - 63 are numeric in the input file. Please try again with a different file.")
#
# fewsronly <- fewsronly[-1, ]
#
# fewsronly <- setDT(fewsronly)

} else {

fewsronly <- import(file, which = sheet, col_names = FALSE, na = c("na", "NA", "N/A", "#N/A", "-", ""))

assert_that(fewsronly[3, 1] == "Minimum heat loading", msg = "Please check that 'Minimum heat loading' is in Row 3, Column 1 in the input file. Please try again with a different file.")

assert_that(all(grepl("([0-9])", fewsronly[4, 1:4])), msg = "Please check that Row 4, Columns 1 - 4 are numeric in the input file. Please try again with a different file.")

assert_that(fewsronly[6, 1] == "Plant ID", msg = "Please check that 'Plant ID' is in Row 6, Column 1 in the input file. Please try again with a different file.")

assert_that(all(grepl("([0-9])", fewsronly[7, 2:63])), msg = "Please check that Row 7, Columns 2 - 63 are numeric in the input file. Please try again with a different file.")

fewsronly <- fewsronly[-1, ]

fewsronly <- setDT(fewsronly)

}


## Input values
minheatload <- as.numeric(fewsronly[3, 1][[1]])
# input value for minimum heat loading, MW/acre

# The "wind function" has been used to calibrate energy budgets, and as such, it accumulates all the unexplained variation.  It's informative to look at the original studies to see how much scatter there is around these curves.
# I use the function of Brady and others for dedicated cooling ponds and lakes, Gulliver and Stefan for fewsronlys, and Webster and Sherman for large lakes.
# input value for Input wind function coefficient a, b, c in m/s
inputwind <- fewsronly[3, 2:4] # list wind1, wind2, and wind3 as a data.table
setnames(inputwind, c("wind1", "wind2", "wind3"))

# changing column to numeric class
# obtain the name of the column based on the column number
change_class <- names(inputwind)
for (col in change_class) set(inputwind, j = col, value = as.numeric(inputwind[[col]])) # Source 4


## Return to fewsronly

fewsronly <- fewsronly[!1:5, ] # remove rows 1:5

setnames(fewsronly, 1, "Plant_ID")

check <- copy(fewsronly)

order_check <- check$"Plant_ID"

setkey(fewsronly, Plant_ID)



# changing column to character class
# obtain the name of the column based on the column number
change_class1 <- "Plant_ID"
for (col in change_class1) set(fewsronly, j = col, value = as.character(fewsronly[[col]])) # Source 4
setkey(fewsronly, Plant_ID)


# change all columns to character class
# obtain the name of the column based on the column number
change_class2 <- names(fewsronly[, 2:ncol(fewsronly)]) # name of all columns except column 1
for (col in change_class2) set(fewsronly, j = col, value = as.character(fewsronly[[col]])) # Source 4


# change all possible stored values for NA to R's NA
for (col in change_class2) set(fewsronly, i = which(fewsronly[[col]] %fin% c("na", "NA", "N/A", "#N/A", "-", "")), j = col, value = NA_character_) # Source 4


# trim all white space surrounding numbers
for (col in change_class2) set(fewsronly, j = col, value = stri_trim_both(fewsronly[[col]], pattern = "\\P{Wspace}")) # Source 4


# replace all commas with nothing
for (col in change_class2) set(fewsronly, j = col, value = stri_replace_all_fixed(fewsronly[[col]], ",", ""))


# changing columns to numeric class
for (col in change_class2) set(fewsronly, j = col, value = as.numeric(fewsronly[[col]])) # Source 4
setkey(fewsronly, Plant_ID)



fewsronly <- setDF(fewsronly)
fewsronly <- fewsronly[!is.nan(colMeans(fewsronly[, 2:ncol(fewsronly)], na.rm = TRUE))] # Source 5 / remove columns with NA only
riverrow <- rowSums(fewsronly[, 2:ncol(fewsronly)], na.rm = TRUE) # obtain the rowSums
fewsronly <- fewsronly[which(riverrow != 0), ] # subset with the rows that do not have a sum of 0 (sum of 0 means that all rows are NA due to NA being removed)
fewsronly <- setDT(fewsronly)


# now return NA as zero (0) to avoid logical error
fewsronly[is.na(fewsronly)] <- 0 # Source 10
setkey(fewsronly, Plant_ID)


setnames(fewsronly, c("Plant_ID", "Elevation above sea level, feet", "Pond Area (acres)", "duty_jan (MMBtu)", "duty_feb (MMBtu)", "duty_mar (MMBtu)", "duty_apr (MMBtu)",	"duty_may (MMBtu)", "duty_jun (MMBtu)",	"duty_jul (MMBtu)", "duty_aug (MMBtu)",	"duty_sep (MMBtu)", "duty_oct (MMBtu)",	"duty_nov (MMBtu)", "duty_dec (MMBtu)",	"db_jan (deg C)", "db_feb (deg C)", "db_mar (deg C)", "db_apr (deg C)", "db_may (deg C)", "db_jun (deg C)", "db_jul (deg C)", "db_aug (deg C)", "db_sep (deg C)", "db_oct (deg C)", "db_nov (deg C)", "db_dec (deg C)", "wb_jan (deg C)", "wb_feb (deg C)", "wb_mar (deg C)", "wb_apr (deg C)", "wb_may (deg C)", "wb_jun (deg C)", "wb_jul (deg C)", "wb_aug (deg C)", "wb_sep (deg C)", "wb_oct (deg C)", "wb_nov (deg C)", "wb_dec (deg C)", "wt_jan (deg C)", "wt_feb (deg C)", "wt_mar (deg C)", "wt_apr (deg C)", "wt_may (deg C)", "wt_jun (deg C)", "wt_jul (deg C)", "wt_aug (deg C)", "wt_sep (deg C)", "wt_oct (deg C)", "wt_nov (deg C)", "wt_dec (deg C)", "ws_jan (mph)", "ws_feb (mph)", "ws_mar (mph)", "ws_apr (mph)", "ws_may (mph)", "ws_jun (mph)", "ws_jul (mph)", "ws_aug (mph)", "ws_sep (mph)", "ws_oct (mph)", "ws_nov (mph)", "ws_dec (mph)")) # set column names

# Source 18 begins
set(fewsronly, j = "elevation", value = fewsronly$"Elevation above sea level, feet" * 0.3048) # convert the elevation to meters
set(fewsronly, j = "Pressure", value = ((44331.514 - fewsronly$elevation) / 11880.516) ^ (1 / 0.1902632)) # calculate the pressure in mbar
# Source 18 ends

setnames(fewsronly, 64:65, c("Elevation above sea level, meters", "Pressure, mbar")) # set the column names
setcolorder(fewsronly, c(1:2, 64:65, 3:63)) # rearrange the columns
setkey(fewsronly, Plant_ID)


## Other Input values
Kelvin <- 273.15 # K

conversion_factor <- 0.000000115740740740741


# Initial Guess at Solution based on quadratic fits to ratio of temperature increase to heat loading
averagedcoefficients <- data.table(coefficient = c(4119.6015341, -48.7753617, -339.1569784, -359.7975614, 0.2536791, 12.5618961, 46.1229360), units = c("constant", "x T", "x W", "x MW/a", "x T^2" , "x W^2", "x (MW/a)^2"))


## Input environmental data and heat loading
pond <- fewsronly[, c(1, 5)] # subset of fewsronly with the Plant ID and Pond Area (acres) data only

duty <- fewsronly[, c(1, 6:17)] # subset of fewsronly with the Plant ID and Duty [Added heat load (MMBtu)] data only

db <- fewsronly[, c(1, 18:29)] # subset of fewsronly with the Plant ID and Dry bulb air temperature Ta (deg C) data only

wb <- fewsronly[, c(1, 30:41)] # subset of fewsronly with the Plant ID and Wet bulb air temperature Twb (deg C) data only

# The natural or upstream water temperature is a critical input but it is difficult to estimate accurately from existing data.
wt <- fewsronly[, c(1, 42:53)] # subset of fewsronly with the Plant ID and Natural water temperature T (deg C) data only

ws <- fewsronly[, c(1, 54:65)] # subset of fewsronly with the Plant ID and Wind speed at 2m W (mph) data only

#aeg
gm_evap <- fewsronly[, c(1,)]

# Wind Functions of the form f(W) = a + bW + cW2
tablewind <- data.table(type = c("cooling lake", "cooling lake", "cooling ponds", "big lake", "fewsronly channel", "stream", "lake"), reference = c("Harbeck 1964", "Ward 1980", "Brady et al. 1969", "Anderson 1954", "Fulford and Sturm 1984", "Gulliver & Stefan 1986", "Webster & Sherman 1995"), a = c(0.000000, 0.000000, 2.465994, 0.000000, 3.200000, 2.960000, 1.040000), b = c(1.461666, 1.352899, 0.000000, 0.920000, 0.800000, 0.640000, 1.050000), c = c(0.0000000, 0.0000000, 0.1233955, 0.0000000, 0.0000000, 0.0000000, 0.0000000), "maximum W (m/s)" = list("- -", "- -", "- -", 8.25000000000000000000, 3.75000000000000000000, 5.75000000000000000000, 5.00000000000000000000))
# These formulas use windspeed in m/s and give f(W) in mm/(day*kPa) following the format of McJannet et al. (2012).
# Timothy H. Diehl gratefully acknowledges the assistance of David I. Stannard


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
# Sources 7 & 8 begin -
# aeg- if (added heat load/3.412*monthdays*24*pond area) > 0, then put in
# addedheatload the max between that var and minheatload, else 0
#fun1 <- function(a, b, c) { ifelse(a / (3.412 * c * 24 * b) > 0, max(a / (3.412 * c * 24 * b), minheatload), 0)
#}
#this below 'fun1' was changed due to an error we believe having to do with zeros #aeg and legh
#hypothesis that this is the source of the issue of not running the second line with same plant id
fun1 <- function(a, b, c) { ifelse((a!=0 | b!=0) & a / (3.412 * c * 24 * b) > 0, max(a / (3.412 * c * 24 * b), minheatload), ifelse(a==0 & b==0,as.numeric(0),as.numeric(0)))}
addedheatload <- addedheatload[, lapply(1:12, function(i) fun1(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(addedheatload, 2:length(addedheatload), c("deltaH_jan (MW/acre)", "deltaH_feb (MW/acre)", "deltaH_mar (MW/acre)", "deltaH_apr (MW/acre)", "deltaH_may (MW/acre)", "deltaH_jun (MW/acre)", "deltaH_jul (MW/acre)", "deltaH_aug (MW/acre)", "deltaH_sep (MW/acre)", "deltaH_oct (MW/acre)", "deltaH_nov (MW/acre)", "deltaH_dec (MW/acre)"))


# input heat load, cal/(cm^2 sec) [deltaHf]
deltaHf <- copy(addedheatload)
deltaHf <- deltaHf[, lapply(.SD, function(x) {((x * 1000000 / 43560) / (0.3048 ^ 2)) / (4.184 * 10000)}), by = key(deltaHf)] # Source 6
setnames(deltaHf, 2:ncol(deltaHf), c("deltaHf_jan {cal/(cm^2 sec) [deltaHf]}", "deltaHf_feb {cal/(cm^2 sec) [deltaHf]}", "deltaHf_mar {cal/(cm^2 sec) [deltaHf]}", "deltaHf_apr {cal/(cm^2 sec) [deltaHf]}", "deltaHf_may {cal/(cm^2 sec) [deltaHf]}", "deltaHf_jun {cal/(cm^2 sec) [deltaHf]}", "deltaHf_jul {cal/(cm^2 sec) [deltaHf]}", "deltaHf_aug {cal/(cm^2 sec) [deltaHf]}", "deltaHf_sep {cal/(cm^2 sec) [deltaHf]}", "deltaHf_oct {cal/(cm^2 sec) [deltaHf]}", "deltaHf_nov {cal/(cm^2 sec) [deltaHf]}", "deltaHf_dec {cal/(cm^2 sec) [deltaHf]}"))


# Calculations for un-heated condition
# saturation vapor pressure calculated on the basis of wet bulb temperature
# e(Twb) in mbar
# See "Lowe 1977 e(T)" for supporting calculations.
e_Twb <- wb[, lapply(.SD, function(x) {
  ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))), -296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)
}), by = key(wb)] # Source 6
setnames(e_Twb, 2:length(e_Twb), c("e(Twb)_jan (mbar)", "e(Twb)_feb (mbar)", "e(Twb)_mar (mbar)", "e(Twb)_apr (mbar)", "e(Twb)_may (mbar)", "e(Twb)_jun (mbar)", "e(Twb)_jul (mbar)", "e(Twb)_aug (mbar)", "e(Twb)_sep (mbar)", "e(Twb)_oct (mbar)", "e(Twb)_nov (mbar)", "e(Twb)_dec (mbar)"))


# air vapor pressure
fewsronly2 <- fewsronly[, c(1, 4)]
fewsronly2[, paste0("Pressure, mbar", 2:12) := rep(fewsronly2[, 2], 11)]
ea <- data.table(e_Twb, db, wb, fewsronly2)
ea[, c(14, 27, 40) := NULL]
setnames(ea, 2:length(ea), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12"))
setkey(ea, Plant_ID)
# Sources 7 & 8 begin
fun2 <- function(a, b, c, d) a - d * (b - c) * 0.00066 * (1 + 0.00115 * c)

ea <- ea[, lapply(1:12, function(i) fun2(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(ea, 2:length(ea), c("ea_jan (mbar)", "ea_feb (mbar)", "ea_mar (mbar)", "ea_apr (mbar)", "ea_may (mbar)", "ea_jun (mbar)", "ea_jul (mbar)", "ea_aug (mbar)", "ea_sep (mbar)", "ea_oct (mbar)", "ea_nov (mbar)", "ea_dec (mbar)"))


# windspeed in m/s
windspeed <- ws[, lapply(.SD, function(x) {
  x * 1609.44 * (1 / 3600)
}), by = key(ws)] # Source 6
setnames(windspeed, 2:length(windspeed), c("ws_jan (m/s)", "ws_feb (m/s)", "ws_mar (m/s)", "ws_apr (m/s)", "ws_may (m/s)", "ws_jun (m/s)", "ws_jul (m/s)", "ws_aug (m/s)", "ws_sep (m/s)", "ws_oct (m/s)", "ws_nov (m/s)", "ws_dec (m/s)"))


# f(W) in mm/ (day*kPa)
f_W_mm <- windspeed[, lapply(.SD, function(x) {
  inputwind$wind1[1] + inputwind$wind2[1] * x + inputwind$wind3[1] * x ^ 2
}), by = key(windspeed)] # Source 6
setnames(f_W_mm, 2:length(f_W_mm), c("f(W)_jan [mm/(day*kPa)]", "f(W)_feb [mm/(day*kPa)]", "f(W)_mar [mm/(day*kPa)]", "f(W)_apr [mm/(day*kPa)]", "f(W)_may [mm/(day*kPa)]", "f(W)_jun [mm/(day*kPa)]", "f(W)_jul [mm/(day*kPa)]", "f(W)_aug [mm/(day*kPa)]", "f(W)_sep [mm/(day*kPa)]", "f(W)_oct [mm/(day*kPa)]", "f(W)_nov [mm/(day*kPa)]", "f(W)_dec [mm/(day*kPa)]"))


# f(W) in cm/(sec*mbar)
f_W_cm <- f_W_mm[, lapply(.SD, function(x) {
  conversion_factor * x
}), by = key(f_W_mm)] # Source 6
setnames(f_W_cm, 2:length(f_W_cm), c("f(W)_jan [cm/(sec*mbar)]", "f(W)_feb [cm/(sec*mbar)]", "f(W)_mar [cm/(sec*mbar)]", "f(W)_apr [cm/(sec*mbar)]", "f(W)_may [cm/(sec*mbar)]", "f(W)_jun [cm/(sec*mbar)]", "f(W)_jul [cm/(sec*mbar)]", "f(W)_aug [cm/(sec*mbar)]", "f(W)_sep [cm/(sec*mbar)]", "f(W)_oct [cm/(sec*mbar)]", "f(W)_nov [cm/(sec*mbar)]", "f(W)_dec [cm/(sec*mbar)]"))


# Initial \u03c1 (g/cm^3), water density
initial_rho <- wt[, lapply(.SD, function(x) {
  0.99983 + 0.00005 * x - 0.00000625 * x ^ 2
}), by = key(wt)] # Source 6
setnames(initial_rho, 2:length(initial_rho), c("\u03c1_jan (g/cm^3)", "\u03c1_feb (g/cm^3)", "\u03c1_mar (g/cm^3)", "\u03c1_apr (g/cm^3)", "\u03c1_may (g/cm^3)", "\u03c1_jun (g/cm^3)", "\u03c1_jul (g/cm^3)", "\u03c1_aug (g/cm^3)", "\u03c1_sep (g/cm^3)", "\u03c1_oct (g/cm^3)", "\u03c1_nov (g/cm^3)", "\u03c1_dec (g/cm^3)"))


# heat of vaporization
heat_of_vaporization <- wt[, lapply(.SD, function(x) {
  595.9 - 0.545 * x
}), by = key(wt)] # Source 6
setnames(heat_of_vaporization, 2:length(heat_of_vaporization), c("L_jan (cal/g)", "L_feb (cal/g)", "L_mar (cal/g)", "L_apr (cal/g)", "L_may (cal/g)", "L_jun (cal/g)", "L_jul (cal/g)", "L_aug (cal/g)", "L_sep (cal/g)", "L_oct (cal/g)", "L_nov (cal/g)", "L_dec (cal/g)"))


# saturation vapor pressure at water temperature
# Source 9 begins
e_Tfunction <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
e_T <- wt[, lapply(.SD, function(x) ifelse(!is.na(x), e_Tfunction(x), NA_real_)), by = key(wt)]
# Source 9 ends
setnames(e_T, 2:length(e_T), c("e(T)_jan (mbar)", "e(T)_feb (mbar)", "e(T)_mar (mbar)", "e(T)_apr (mbar)", "e(T)_may (mbar)", "e(T)_jun (mbar)", "e(T)_jul (mbar)", "e(T)_aug (mbar)", "e(T)_sep (mbar)", "e(T)_oct (mbar)", "e(T)_nov (mbar)", "e(T)_dec (mbar)"))


# Calculations for un-heated condition table
calc_unheated_condition <- data.table(e_Twb, ea, windspeed, f_W_mm, f_W_cm, initial_rho, heat_of_vaporization, e_T)
calc_unheated_condition[, c(14, 27, 40, 53, 66, 79, 92) := NULL]
setkey(calc_unheated_condition, Plant_ID)


# initial estimated ratio of delta T to heat loading
deltaT_heat_loading_ratio_initial <- data.table(wt, ws, addedheatload)
deltaT_heat_loading_ratio_initial[, c(14, 27) := NULL]
setkey(deltaT_heat_loading_ratio_initial, Plant_ID)
setnames(deltaT_heat_loading_ratio_initial, 2:length(deltaT_heat_loading_ratio_initial), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun3 <- function(a, b, c) (averagedcoefficients$coefficient[1] + averagedcoefficients$coefficient[2] * a + averagedcoefficients$coefficient[3] * b + averagedcoefficients$coefficient[4] * c + averagedcoefficients$coefficient[5] * a ^ 2 + averagedcoefficients$coefficient[6] * b ^ 2 + averagedcoefficients$coefficient[7] * c ^ 2)

deltaT_heat_loading_ratio_initial <- deltaT_heat_loading_ratio_initial[, lapply(1:12, function(i) fun3(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaT_heat_loading_ratio_initial, 2:length(deltaT_heat_loading_ratio_initial), c("deltaT_HL_ratio_jan", "deltaT_HL_ratio_feb", "deltaT_HL_ratio_mar", "deltaT_HL_ratio_apr", "deltaT_HL_ratio_may", "deltaT_HL_ratio_jun", "deltaT_HL_ratio_jul", "deltaT_HL_ratio_aug", "deltaT_HL_ratio_sep", "deltaT_HL_ratio_oct", "deltaT_HL_ratio_nov", "deltaT_HL_ratio_dec"))


# Initial guess at T' / T1
T1 <- data.table(wt, deltaT_heat_loading_ratio_initial, deltaHf)
T1[, c(14, 27) := NULL]
setkey(T1, Plant_ID)
setnames(T1, 2:length(T1), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun4 <- function(a, b, c) pmin(99, a + b * c)

T1 <- T1[, lapply(1:12, function(i) fun4(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(T1, 2:length(T1), c("T1_jan ()", "T1_feb ()", "T1_mar ()", "T1_apr ()", "T1_may ()", "T1_jun ()", "T1_jul ()", "T1_aug ()", "T1_sep ()", "T1_oct ()", "T1_nov ()", "T1_dec ()"))


# e(T')1
# Source 9 begins
eTprime1function <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
eTprime1 <- T1[, lapply(.SD, function(x) ifelse(!is.na(x), eTprime1function(x), NA_real_)), by = key(T1)]
# Source 9 ends
setnames(eTprime1, 2:length(eTprime1), c("e(T')1_jan ()", "e(T')1_feb ()", "e(T')1_mar ()", "e(T')1_apr ()", "e(T')1_may ()", "e(T')1_jun ()", "e(T')1_jul ()", "e(T')1_aug ()", "e(T')1_sep ()", "e(T')1_oct ()", "e(T')1_nov ()", "e(T')1_dec ()"))


# Increment in heat loss by mode
# deltaE1
deltaE1 <- data.table(f_W_cm, initial_rho, heat_of_vaporization, e_T, eTprime1)
deltaE1[, c(14, 27, 40, 53) := NULL]
setkey(deltaE1, Plant_ID)
setnames(deltaE1, 2:length(deltaE1), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
# Sources 7 & 8 begin
fun5 <- function(a, b, c, d, e) b * c * a * (e - d)

deltaE1 <- deltaE1[, lapply(1:12, function(i) fun5(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaE1, 2:length(deltaE1), c("deltaE1_jan (cal/(cm^2 sec))", "deltaE1_feb (cal/(cm^2 sec))", "deltaE1_mar (cal/(cm^2 sec))", "deltaE1_apr (cal/(cm^2 sec))", "deltaE1_may (cal/(cm^2 sec))", "deltaE1_jun (cal/(cm^2 sec))", "deltaE1_jul (cal/(cm^2 sec))", "deltaE1_aug (cal/(cm^2 sec))", "deltaE1_sep (cal/(cm^2 sec))", "deltaE1_oct (cal/(cm^2 sec))", "deltaE1_nov (cal/(cm^2 sec))", "deltaE1_dec (cal/(cm^2 sec))"))


# deltaC1
deltaC1 <- data.table(f_W_cm, initial_rho, T1, wt, fewsronly2)
deltaC1[, c(14, 27, 40, 53) := NULL]
setnames(deltaC1, 2:length(deltaC1), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(deltaC1, Plant_ID)
# Sources 7 & 8 begin
fun6 <- function(a, b, c, d, e) a * ((b * e * constants$constants[9]) / constants$constants[10]) * (c - d)

deltaC1 <- deltaC1[, lapply(1:12, function(i) fun6(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaC1, 2:length(deltaC1), c("deltaC1_jan (cal/(cm^2 sec))", "deltaC1_feb (cal/(cm^2 sec))", "deltaC1_mar (cal/(cm^2 sec))", "deltaC1_apr (cal/(cm^2 sec))", "deltaC1_may (cal/(cm^2 sec))", "deltaC1_jun (cal/(cm^2 sec))", "deltaC1_jul (cal/(cm^2 sec))", "deltaC1_aug (cal/(cm^2 sec))", "deltaC1_sep (cal/(cm^2 sec))", "deltaC1_oct (cal/(cm^2 sec))", "deltaC1_nov (cal/(cm^2 sec))", "deltaC1_dec (cal/(cm^2 sec))"))


# deltaR1
deltaR1 <- data.table(T1, wt)
deltaR1[, 14 := NULL]
setkey(deltaR1, Plant_ID)
setnames(deltaR1, 2:length(deltaR1), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun7 <- function(a, b) (constants$constants[7] * constants$constants[8] / constants$constants[12]) * ((a + Kelvin) ^ 4 - (b + Kelvin) ^ 4)

deltaR1 <- deltaR1[, lapply(1:12, function(i) fun7(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaR1, 2:length(deltaR1), c("deltaR1_jan (cal/(cm^2 sec))", "deltaR1_feb (cal/(cm^2 sec))", "deltaR1_mar (cal/(cm^2 sec))", "deltaR1_apr (cal/(cm^2 sec))", "deltaR1_may (cal/(cm^2 sec))", "deltaR1_jun (cal/(cm^2 sec))", "deltaR1_jul (cal/(cm^2 sec))", "deltaR1_aug (cal/(cm^2 sec))", "deltaR1_sep (cal/(cm^2 sec))", "deltaR1_oct (cal/(cm^2 sec))", "deltaR1_nov (cal/(cm^2 sec))", "deltaR1_dec (cal/(cm^2 sec))"))


# Increment in total heat loss deltaH / deltaH1
deltaH1 <- data.table(deltaE1, deltaC1, deltaR1)
deltaH1[, c(14, 27) := NULL]
setkey(deltaH1, Plant_ID)
setnames(deltaH1 , 2:length(deltaH1), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun8 <- function(a, b, c) a + b + c

deltaH1 <- deltaH1[, lapply(1:12, function(i) fun8(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaH1, 2:length(deltaH1), c("deltaH1_jan (cal/(cm^2 sec))", "deltaH1_feb (cal/(cm^2 sec))", "deltaH1_mar (cal/(cm^2 sec))", "deltaH1_apr (cal/(cm^2 sec))", "deltaH1_may (cal/(cm^2 sec))", "deltaH1_jun (cal/(cm^2 sec))", "deltaH1_jul (cal/(cm^2 sec))", "deltaH1_aug (cal/(cm^2 sec))", "deltaH1_sep (cal/(cm^2 sec))", "deltaH1_oct (cal/(cm^2 sec))", "deltaH1_nov (cal/(cm^2 sec))", "deltaH1_dec (cal/(cm^2 sec))"))


# iteration 1
iteration1 <- data.table(T1, eTprime1, deltaE1, deltaC1, deltaR1, deltaH1)
iteration1[, c(14, 27, 40, 53, 66) := NULL]


## iteration 2
# Estimated heated water temperature T' (deg C)1
T2 <- data.table(wt, T1, deltaH1, deltaHf)
T2[, c(14, 27, 40) := NULL]
setnames(T2, 2:length(T2), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12"))
setkey(T2, Plant_ID)
# Sources 7 & 8 begin
fun9 <- function(a, b, c, d) { (a + (b - a) * (d / c))
}

T2 <- T2[, lapply(1:12, function(i) fun9(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(T2, 2:length(T2), c("T2_jan ()", "T2_feb ()", "T2_mar ()", "T2_apr ()", "T2_may ()", "T2_jun ()", "T2_jul ()", "T2_aug ()", "T2_sep ()", "T2_oct ()", "T2_nov ()", "T2_dec ()"))


# Percent change in T' from previous iteration1
percentchangeTprime1 <- data.table(T1, T2)
percentchangeTprime1[, 14 := NULL]
setkey(percentchangeTprime1, Plant_ID)
setnames(percentchangeTprime1 , 2:length(percentchangeTprime1), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun10 <- function(a, b) ((b - a) / a) * 100

percentchangeTprime1 <- percentchangeTprime1[, lapply(1:12, function(i) fun10(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(percentchangeTprime1, 2:length(percentchangeTprime1), c("percent change in T'1_jan", "percent change in T'1_feb", "percent change in T'1_mar", "percent change in T'1_apr", "percent change in T'1_may", "percent change in T'1_jun", "percent change in T'1_jul", "percent change in T'1_aug", "percent change in T'1_sep", "percent change in T'1_oct", "percent change in T'1_nov", "percent change in T'1_dec"))


# eTprime2
# Source 9 begins
eTprime2function <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
eTprime2 <- T2[, lapply(.SD, function(x) ifelse(!is.na(x), eTprime2function(x), NA_real_)), by = key(T2)]
# Source 9 ends
setnames(eTprime2, 2:length(eTprime2), c("e(T')2_jan ()", "e(T')2_feb ()", "e(T')2_mar ()", "e(T')2_apr ()", "e(T')2_may ()", "e(T')2_jun ()", "e(T')2_jul ()", "e(T')2_aug ()", "e(T')2_sep ()", "e(T')2_oct ()", "e(T')2_nov ()", "e(T')2_dec ()"))


# deltaE2 / deltaE2
deltaE2 <- data.table(f_W_cm, initial_rho, heat_of_vaporization, e_T, eTprime2)
deltaE2[, c(14, 27, 40, 53) := NULL]
setkey(deltaE2, Plant_ID)
setnames(deltaE2, 2:length(deltaE2), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
# Sources 7 & 8 begin
fun11 <- function(a, b, c, d, e) b * c * a * (e - d)

deltaE2 <- deltaE2[, lapply(1:12, function(i) fun11(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaE2, 2:length(deltaE2), c("deltaE2_jan (cal/(cm^2 sec))", "deltaE2_feb (cal/(cm^2 sec))", "deltaE2_mar (cal/(cm^2 sec))", "deltaE2_apr (cal/(cm^2 sec))", "deltaE2_may (cal/(cm^2 sec))", "deltaE2_jun (cal/(cm^2 sec))", "deltaE2_jul (cal/(cm^2 sec))", "deltaE2_aug (cal/(cm^2 sec))", "deltaE2_sep (cal/(cm^2 sec))", "deltaE2_oct (cal/(cm^2 sec))", "deltaE2_nov (cal/(cm^2 sec))", "deltaE2_dec (cal/(cm^2 sec))"))


# deltaC2 / deltaC2
deltaC2 <- data.table(f_W_cm, initial_rho, T2, wt, fewsronly2)
deltaC2[, c(14, 27, 40, 53) := NULL]
setnames(deltaC2, 2:length(deltaC2), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(deltaC2, Plant_ID)
# Sources 7 & 8 begin
fun12 <- function(a, b, c, d, e) a * ((b * e * constants$constants[9]) / constants$constants[10]) * (c - d)

deltaC2 <- deltaC2[, lapply(1:12, function(i) fun12(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaC2, 2:length(deltaC2), c("deltaC2_jan (cal/(cm^2 sec))", "deltaC2_feb (cal/(cm^2 sec))", "deltaC2_mar (cal/(cm^2 sec))", "deltaC2_apr (cal/(cm^2 sec))", "deltaC2_may (cal/(cm^2 sec))", "deltaC2_jun (cal/(cm^2 sec))", "deltaC2_jul (cal/(cm^2 sec))", "deltaC2_aug (cal/(cm^2 sec))", "deltaC2_sep (cal/(cm^2 sec))", "deltaC2_oct (cal/(cm^2 sec))", "deltaC2_nov (cal/(cm^2 sec))", "deltaC2_dec (cal/(cm^2 sec))"))


# deltaR2 / deltaR2
deltaR2 <- data.table(T2, wt)
deltaR2[, 14 := NULL]
setkey(deltaR2, Plant_ID)
setnames(deltaR2, 2:length(deltaR2), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun13 <- function(a, b) (constants$constants[7] * constants$constants[8] / constants$constants[12]) * ((a + Kelvin) ^ 4 - (b + Kelvin) ^ 4)

deltaR2 <- deltaR2[, lapply(1:12, function(i) fun13(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaR2, 2:length(deltaR2), c("deltaR2_jan (cal/(cm^2 sec))", "deltaR2_feb (cal/(cm^2 sec))", "deltaR2_mar (cal/(cm^2 sec))", "deltaR2_apr (cal/(cm^2 sec))", "deltaR2_may (cal/(cm^2 sec))", "deltaR2_jun (cal/(cm^2 sec))", "deltaR2_jul (cal/(cm^2 sec))", "deltaR2_aug (cal/(cm^2 sec))", "deltaR2_sep (cal/(cm^2 sec))", "deltaR2_oct (cal/(cm^2 sec))", "deltaR2_nov (cal/(cm^2 sec))", "deltaR2_dec (cal/(cm^2 sec))"))


# Increment in total heat loss deltaH / deltaH2
deltaH2 <- data.table(deltaE2, deltaC2, deltaR2)
deltaH2[, c(14, 27) := NULL]
setkey(deltaH2, Plant_ID)
setnames(deltaH2 , 2:length(deltaH2), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun14 <- function(a, b, c) a + b + c

deltaH2 <- deltaH2[, lapply(1:12, function(i) fun14(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaH2, 2:length(deltaH2), c("deltaH2_jan (cal/(cm^2 sec))", "deltaH2_feb (cal/(cm^2 sec))", "deltaH2_mar (cal/(cm^2 sec))", "deltaH2_apr (cal/(cm^2 sec))", "deltaH2_may (cal/(cm^2 sec))", "deltaH2_jun (cal/(cm^2 sec))", "deltaH2_jul (cal/(cm^2 sec))", "deltaH2_aug (cal/(cm^2 sec))", "deltaH2_sep (cal/(cm^2 sec))", "deltaH2_oct (cal/(cm^2 sec))", "deltaH2_nov (cal/(cm^2 sec))", "deltaH2_dec (cal/(cm^2 sec))"))


iteration2 <- data.table(T2, percentchangeTprime1, eTprime2, deltaE2, deltaC2, deltaR2, deltaH2)
iteration2[, c(14, 27, 40, 53, 66, 79) := NULL]


## iteration 3
# Estimated heated water temperature T' (deg C)1
T3 <- data.table(T1, T2, deltaH1, deltaH2, deltaHf)
T3[, c(14, 27, 40, 53) := NULL]
setnames(T3, 2:length(T3), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(T3, Plant_ID)
# Sources 7 & 8 begin
fun15 <- function(a, b, c, d, e) { (a + (b - a) * ((e - c) / (d - c)))
}

T3 <- T3[, lapply(1:12, function(i) fun15(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(T3, 2:length(T3), c("T3_jan ()", "T3_feb ()", "T3_mar ()", "T3_apr ()", "T3_may ()", "T3_jun ()", "T3_jul ()", "T3_aug ()", "T3_sep ()", "T3_oct ()", "T3_nov ()", "T3_dec ()"))


# Percent change in T' from previous iteration1
percentchangeTprime2 <- data.table(T2, T3)
percentchangeTprime2[, 14 := NULL]
setkey(percentchangeTprime2, Plant_ID)
setnames(percentchangeTprime2 , 2:length(percentchangeTprime2), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun16 <- function(a, b) ((b - a) / a) * 100

percentchangeTprime2 <- percentchangeTprime2[, lapply(1:12, function(i) fun16(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(percentchangeTprime2, 2:length(percentchangeTprime2), c("percent change in T'2_jan", "percent change in T'2_feb", "percent change in T'2_mar", "percent change in T'2_apr", "percent change in T'2_may", "percent change in T'2_jun", "percent change in T'2_jul", "percent change in T'2_aug", "percent change in T'2_sep", "percent change in T'2_oct", "percent change in T'2_nov", "percent change in T'2_dec"))


# eTprime3
# Source 9 begins
eTprime3function <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
eTprime3 <- T3[, lapply(.SD, function(x) ifelse(!is.na(x), eTprime3function(x), NA_real_)), by = key(T3)]
# Source 9 ends
setnames(eTprime3, 2:length(eTprime3), c("e(T')3_jan ()", "e(T')3_feb ()", "e(T')3_mar ()", "e(T')3_apr ()", "e(T')3_may ()", "e(T')3_jun ()", "e(T')3_jul ()", "e(T')3_aug ()", "e(T')3_sep ()", "e(T')3_oct ()", "e(T')3_nov ()", "e(T')3_dec ()"))


# deltaE3 / deltaE3
deltaE3 <- data.table(f_W_cm, initial_rho, heat_of_vaporization, e_T, eTprime3)
deltaE3[, c(14, 27, 40, 53) := NULL]
setkey(deltaE3, Plant_ID)
setnames(deltaE3, 2:length(deltaE3), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
# Sources 7 & 8 begin
fun17 <- function(a, b, c, d, e) b * c * a * (e - d)

deltaE3 <- deltaE3[, lapply(1:12, function(i) fun17(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaE3, 2:length(deltaE3), c("deltaE3_jan (cal/(cm^2 sec))", "deltaE3_feb (cal/(cm^2 sec))", "deltaE3_mar (cal/(cm^2 sec))", "deltaE3_apr (cal/(cm^2 sec))", "deltaE3_may (cal/(cm^2 sec))", "deltaE3_jun (cal/(cm^2 sec))", "deltaE3_jul (cal/(cm^2 sec))", "deltaE3_aug (cal/(cm^2 sec))", "deltaE3_sep (cal/(cm^2 sec))", "deltaE3_oct (cal/(cm^2 sec))", "deltaE3_nov (cal/(cm^2 sec))", "deltaE3_dec (cal/(cm^2 sec))"))


# deltaC3 / deltaC3
deltaC3 <- data.table(f_W_cm, initial_rho, T3, wt, fewsronly2)
deltaC3[, c(14, 27, 40, 53) := NULL]
setnames(deltaC3, 2:length(deltaC3), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(deltaC3, Plant_ID)
# Sources 7 & 8 begin
fun18 <- function(a, b, c, d, e) a * ((b * e * constants$constants[9]) / constants$constants[10]) * (c - d)

deltaC3 <- deltaC3[, lapply(1:12, function(i) fun18(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaC3, 2:length(deltaC3), c("deltaC3_jan (cal/(cm^2 sec))", "deltaC3_feb (cal/(cm^2 sec))", "deltaC3_mar (cal/(cm^2 sec))", "deltaC3_apr (cal/(cm^2 sec))", "deltaC3_may (cal/(cm^2 sec))", "deltaC3_jun (cal/(cm^2 sec))", "deltaC3_jul (cal/(cm^2 sec))", "deltaC3_aug (cal/(cm^2 sec))", "deltaC3_sep (cal/(cm^2 sec))", "deltaC3_oct (cal/(cm^2 sec))", "deltaC3_nov (cal/(cm^2 sec))", "deltaC3_dec (cal/(cm^2 sec))"))


# deltaR3 / deltaR3
deltaR3 <- data.table(T3, wt)
deltaR3[, 14 := NULL]
setkey(deltaR3, Plant_ID)
setnames(deltaR3, 2:length(deltaR3), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun19 <- function(a, b) (constants$constants[7] * constants$constants[8] / constants$constants[12]) * ((a + Kelvin) ^ 4 - (b + Kelvin) ^ 4)

deltaR3 <- deltaR3[, lapply(1:12, function(i) fun19(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaR3, 2:length(deltaR3), c("deltaR3_jan (cal/(cm^2 sec))", "deltaR3_feb (cal/(cm^2 sec))", "deltaR3_mar (cal/(cm^2 sec))", "deltaR3_apr (cal/(cm^2 sec))", "deltaR3_may (cal/(cm^2 sec))", "deltaR3_jun (cal/(cm^2 sec))", "deltaR3_jul (cal/(cm^2 sec))", "deltaR3_aug (cal/(cm^2 sec))", "deltaR3_sep (cal/(cm^2 sec))", "deltaR3_oct (cal/(cm^2 sec))", "deltaR3_nov (cal/(cm^2 sec))", "deltaR3_dec (cal/(cm^2 sec))"))


# Increment in total heat loss deltaH / deltaH3
deltaH3 <- data.table(deltaE3, deltaC3, deltaR3)
deltaH3[, c(14, 27) := NULL]
setkey(deltaH3, Plant_ID)
setnames(deltaH3 , 2:length(deltaH3), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun20 <- function(a, b, c) a + b + c

deltaH3 <- deltaH3[, lapply(1:12, function(i) fun20(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaH3, 2:length(deltaH3), c("deltaH3_jan (cal/(cm^2 sec))", "deltaH3_feb (cal/(cm^2 sec))", "deltaH3_mar (cal/(cm^2 sec))", "deltaH3_apr (cal/(cm^2 sec))", "deltaH3_may (cal/(cm^2 sec))", "deltaH3_jun (cal/(cm^2 sec))", "deltaH3_jul (cal/(cm^2 sec))", "deltaH3_aug (cal/(cm^2 sec))", "deltaH3_sep (cal/(cm^2 sec))", "deltaH3_oct (cal/(cm^2 sec))", "deltaH3_nov (cal/(cm^2 sec))", "deltaH3_dec (cal/(cm^2 sec))"))


iteration3 <- data.table(T3, percentchangeTprime2, eTprime3, deltaE3, deltaC3, deltaR3, deltaH3)
iteration3[, c(14, 27, 40, 53, 66, 79) := NULL]



## iteration 4
# Estimated heated water temperature T' (deg C)1
T4 <- data.table(T2, T3, deltaH2, deltaH3, deltaHf)
T4[, c(14, 27, 40, 53) := NULL]
setnames(T4, 2:length(T4), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(T4, Plant_ID)
# Source 7 - 9 begins
fun21 <- function(a, b, c, d, e) { ifelse(d != c, (a + (b - a) * ((e - c) / (d - c))), b)
}

T4a <- T4[, lapply(1:12, function(i) ifelse(!is.na(c(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), fun21(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i))), NA_real_)), by = Plant_ID]
# Source 7 - 9 ends
T4 <- T4a[pmatch(T4$Plant_ID, T4a$Plant_ID), ]
setnames(T4, 2:length(T4), c("T4_jan ()", "T4_feb ()", "T4_mar ()", "T4_apr ()", "T4_may ()", "T4_jun ()", "T4_jul ()", "T4_aug ()", "T4_sep ()", "T4_oct ()", "T4_nov ()", "T4_dec ()"))
setkey(T4, Plant_ID)


# Percent change in T' from previous iteration1
percentchangeTprime3 <- data.table(T3, T4)
percentchangeTprime3[, 14 := NULL]
setkey(percentchangeTprime3, Plant_ID)
setnames(percentchangeTprime3 , 2:length(percentchangeTprime3), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun22 <- function(a, b) ((b - a) / a) * 100

percentchangeTprime3 <- percentchangeTprime3[, lapply(1:12, function(i) fun22(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(percentchangeTprime3, 2:length(percentchangeTprime3), c("percent change in T'3_jan", "percent change in T'3_feb", "percent change in T'3_mar", "percent change in T'3_apr", "percent change in T'3_may", "percent change in T'3_jun", "percent change in T'3_jul", "percent change in T'3_aug", "percent change in T'3_sep", "percent change in T'3_oct", "percent change in T'3_nov", "percent change in T'3_dec"))


# eTprime4
# Source 9 begins
eTprime4function <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
eTprime4 <- T4[, lapply(.SD, function(x) ifelse(!is.na(x), eTprime4function(x), NA_real_)), by = key(T4)]
# Source 9 ends
setnames(eTprime4, 2:length(eTprime4), c("e(T')4_jan ()", "e(T')4_feb ()", "e(T')4_mar ()", "e(T')4_apr ()", "e(T')4_may ()", "e(T')4_jun ()", "e(T')4_jul ()", "e(T')4_aug ()", "e(T')4_sep ()", "e(T')4_oct ()", "e(T')4_nov ()", "e(T')4_dec ()"))


# deltaE4 / deltaE4
deltaE4 <- data.table(f_W_cm, initial_rho, heat_of_vaporization, e_T, eTprime4)
deltaE4[, c(14, 27, 40, 53) := NULL]
setkey(deltaE4, Plant_ID)
setnames(deltaE4, 2:length(deltaE4), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
# Sources 7 & 8 begin
fun23 <- function(a, b, c, d, e) b * c * a * (e - d)

deltaE4 <- deltaE4[, lapply(1:12, function(i) fun23(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaE4, 2:length(deltaE4), c("deltaE4_jan (cal/(cm^2 sec))", "deltaE4_feb (cal/(cm^2 sec))", "deltaE4_mar (cal/(cm^2 sec))", "deltaE4_apr (cal/(cm^2 sec))", "deltaE4_may (cal/(cm^2 sec))", "deltaE4_jun (cal/(cm^2 sec))", "deltaE4_jul (cal/(cm^2 sec))", "deltaE4_aug (cal/(cm^2 sec))", "deltaE4_sep (cal/(cm^2 sec))", "deltaE4_oct (cal/(cm^2 sec))", "deltaE4_nov (cal/(cm^2 sec))", "deltaE4_dec (cal/(cm^2 sec))"))


# deltaC4 / deltaC4
deltaC4 <- data.table(f_W_cm, initial_rho, T4, wt, fewsronly2)
deltaC4[, c(14, 27, 40, 53) := NULL]
setnames(deltaC4, 2:length(deltaC4), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(deltaC4, Plant_ID)
# Sources 7 & 8 begin
fun24 <- function(a, b, c, d, e) a * ((b * e * constants$constants[9]) / constants$constants[10]) * (c - d)

deltaC4 <- deltaC4[, lapply(1:12, function(i) fun24(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaC4, 2:length(deltaC4), c("deltaC4_jan (cal/(cm^2 sec))", "deltaC4_feb (cal/(cm^2 sec))", "deltaC4_mar (cal/(cm^2 sec))", "deltaC4_apr (cal/(cm^2 sec))", "deltaC4_may (cal/(cm^2 sec))", "deltaC4_jun (cal/(cm^2 sec))", "deltaC4_jul (cal/(cm^2 sec))", "deltaC4_aug (cal/(cm^2 sec))", "deltaC4_sep (cal/(cm^2 sec))", "deltaC4_oct (cal/(cm^2 sec))", "deltaC4_nov (cal/(cm^2 sec))", "deltaC4_dec (cal/(cm^2 sec))"))


# deltaR4 / deltaR4
deltaR4 <- data.table(T4, wt)
deltaR4[, 14 := NULL]
setkey(deltaR4, Plant_ID)
setnames(deltaR4, 2:length(deltaR4), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun25 <- function(a, b) (constants$constants[7] * constants$constants[8] / constants$constants[12]) * ((a + Kelvin) ^ 4 - (b + Kelvin) ^ 4)

deltaR4 <- deltaR4[, lapply(1:12, function(i) fun25(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaR4, 2:length(deltaR4), c("deltaR4_jan (cal/(cm^2 sec))", "deltaR4_feb (cal/(cm^2 sec))", "deltaR4_mar (cal/(cm^2 sec))", "deltaR4_apr (cal/(cm^2 sec))", "deltaR4_may (cal/(cm^2 sec))", "deltaR4_jun (cal/(cm^2 sec))", "deltaR4_jul (cal/(cm^2 sec))", "deltaR4_aug (cal/(cm^2 sec))", "deltaR4_sep (cal/(cm^2 sec))", "deltaR4_oct (cal/(cm^2 sec))", "deltaR4_nov (cal/(cm^2 sec))", "deltaR4_dec (cal/(cm^2 sec))"))


# Increment in total heat loss deltaH / deltaH4
deltaH4 <- data.table(deltaE4, deltaC4, deltaR4)
deltaH4[, c(14, 27) := NULL]
setkey(deltaH4, Plant_ID)
setnames(deltaH4 , 2:length(deltaH4), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun26 <- function(a, b, c) a + b + c

deltaH4 <- deltaH4[, lapply(1:12, function(i) fun26(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaH4, 2:length(deltaH4), c("deltaH4_jan (cal/(cm^2 sec))", "deltaH4_feb (cal/(cm^2 sec))", "deltaH4_mar (cal/(cm^2 sec))", "deltaH4_apr (cal/(cm^2 sec))", "deltaH4_may (cal/(cm^2 sec))", "deltaH4_jun (cal/(cm^2 sec))", "deltaH4_jul (cal/(cm^2 sec))", "deltaH4_aug (cal/(cm^2 sec))", "deltaH4_sep (cal/(cm^2 sec))", "deltaH4_oct (cal/(cm^2 sec))", "deltaH4_nov (cal/(cm^2 sec))", "deltaH4_dec (cal/(cm^2 sec))"))


iteration4 <- data.table(T4, percentchangeTprime3, eTprime4, deltaE4, deltaC4, deltaR4, deltaH4)
iteration4[, c(14, 27, 40, 53, 66, 79) := NULL]


## iteration 5
# Estimated heated water temperature T' (deg C)1
T5 <- data.table(T3, T4, deltaH3, deltaH4, deltaHf)
T5[, c(14, 27, 40, 53) := NULL]
setnames(T5, 2:length(T5), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(T5, Plant_ID)
# Source 7 - 9 begins
fun27 <- function(a, b, c, d, e) { ifelse(d != c, (a + (b - a) * ((e - c) / (d - c))), b)
}

T5a <- T5[, lapply(1:12, function(i) ifelse(!is.na(c(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), fun27(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i))), NA_real_)), by = Plant_ID]
# Source 7 - 9 ends
T5 <- T5a[pmatch(T5$Plant_ID, T5a$Plant_ID), ]
setnames(T5, 2:length(T5), c("T5_jan ()", "T5_feb ()", "T5_mar ()", "T5_apr ()", "T5_may ()", "T5_jun ()", "T5_jul ()", "T5_aug ()", "T5_sep ()", "T5_oct ()", "T5_nov ()", "T5_dec ()"))
setkey(T5, Plant_ID)


# Percent change in T' from previous iteration1
percentchangeTprime4 <- data.table(T4, T5)
percentchangeTprime4[, 14 := NULL]
setkey(percentchangeTprime4, Plant_ID)
setnames(percentchangeTprime4 , 2:length(percentchangeTprime4), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun28 <- function(a, b) ((b - a) / a) * 100

percentchangeTprime4 <- percentchangeTprime4[, lapply(1:12, function(i) fun28(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(percentchangeTprime4, 2:length(percentchangeTprime4), c("percent change in T'4_jan", "percent change in T'4_feb", "percent change in T'4_mar", "percent change in T'4_apr", "percent change in T'4_may", "percent change in T'4_jun", "percent change in T'4_jul", "percent change in T'4_aug", "percent change in T'4_sep", "percent change in T'4_oct", "percent change in T'4_nov", "percent change in T'4_dec"))


# eTprime5
# Source 9 begins
eTprime5function <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
eTprime5 <- T5[, lapply(.SD, function(x) ifelse(!is.na(x), eTprime5function(x), NA_real_)), by = key(T5)]
# Source 9 ends
setnames(eTprime5, 2:length(eTprime5), c("e(T')5_jan ()", "e(T')5_feb ()", "e(T')5_mar ()", "e(T')5_apr ()", "e(T')5_may ()", "e(T')5_jun ()", "e(T')5_jul ()", "e(T')5_aug ()", "e(T')5_sep ()", "e(T')5_oct ()", "e(T')5_nov ()", "e(T')5_dec ()"))


# deltaE5 / deltaE5
deltaE5 <- data.table(f_W_cm, initial_rho, heat_of_vaporization, e_T, eTprime5)
deltaE5[, c(14, 27, 40, 53) := NULL]
setkey(deltaE5, Plant_ID)
setnames(deltaE5, 2:length(deltaE5), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
# Sources 7 & 8 begin
fun29 <- function(a, b, c, d, e) b * c * a * (e - d)

deltaE5 <- deltaE5[, lapply(1:12, function(i) fun29(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaE5, 2:length(deltaE5), c("deltaE5_jan (cal/(cm^2 sec))", "deltaE5_feb (cal/(cm^2 sec))", "deltaE5_mar (cal/(cm^2 sec))", "deltaE5_apr (cal/(cm^2 sec))", "deltaE5_may (cal/(cm^2 sec))", "deltaE5_jun (cal/(cm^2 sec))", "deltaE5_jul (cal/(cm^2 sec))", "deltaE5_aug (cal/(cm^2 sec))", "deltaE5_sep (cal/(cm^2 sec))", "deltaE5_oct (cal/(cm^2 sec))", "deltaE5_nov (cal/(cm^2 sec))", "deltaE5_dec (cal/(cm^2 sec))"))


# deltaC5 / deltaC5
deltaC5 <- data.table(f_W_cm, initial_rho, T5, wt, fewsronly2)
deltaC5[, c(14, 27, 40, 53) := NULL]
setnames(deltaC5, 2:length(deltaC5), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(deltaC5, Plant_ID)
# Sources 7 & 8 begin
fun30 <- function(a, b, c, d, e) a * ((b * e * constants$constants[9]) / constants$constants[10]) * (c - d)

deltaC5 <- deltaC5[, lapply(1:12, function(i) fun30(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaC5, 2:length(deltaC5), c("deltaC5_jan (cal/(cm^2 sec))", "deltaC5_feb (cal/(cm^2 sec))", "deltaC5_mar (cal/(cm^2 sec))", "deltaC5_apr (cal/(cm^2 sec))", "deltaC5_may (cal/(cm^2 sec))", "deltaC5_jun (cal/(cm^2 sec))", "deltaC5_jul (cal/(cm^2 sec))", "deltaC5_aug (cal/(cm^2 sec))", "deltaC5_sep (cal/(cm^2 sec))", "deltaC5_oct (cal/(cm^2 sec))", "deltaC5_nov (cal/(cm^2 sec))", "deltaC5_dec (cal/(cm^2 sec))"))


# deltaR5 / deltaR5
deltaR5 <- data.table(T5, wt)
deltaR5[, 14 := NULL]
setkey(deltaR5, Plant_ID)
setnames(deltaR5, 2:length(deltaR5), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun31 <- function(a, b) (constants$constants[7] * constants$constants[8] / constants$constants[12]) * ((a + Kelvin) ^ 4 - (b + Kelvin) ^ 4)

deltaR5 <- deltaR5[, lapply(1:12, function(i) fun31(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaR5, 2:length(deltaR5), c("deltaR5_jan (cal/(cm^2 sec))", "deltaR5_feb (cal/(cm^2 sec))", "deltaR5_mar (cal/(cm^2 sec))", "deltaR5_apr (cal/(cm^2 sec))", "deltaR5_may (cal/(cm^2 sec))", "deltaR5_jun (cal/(cm^2 sec))", "deltaR5_jul (cal/(cm^2 sec))", "deltaR5_aug (cal/(cm^2 sec))", "deltaR5_sep (cal/(cm^2 sec))", "deltaR5_oct (cal/(cm^2 sec))", "deltaR5_nov (cal/(cm^2 sec))", "deltaR5_dec (cal/(cm^2 sec))"))


# Increment in total heat loss deltaH / deltaH5
deltaH5 <- data.table(deltaE5, deltaC5, deltaR5)
deltaH5[, c(14, 27) := NULL]
setkey(deltaH5, Plant_ID)
setnames(deltaH5 , 2:length(deltaH5), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun32 <- function(a, b, c) a + b + c

deltaH5 <- deltaH5[, lapply(1:12, function(i) fun32(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaH5, 2:length(deltaH5), c("deltaH5_jan (cal/(cm^2 sec))", "deltaH5_feb (cal/(cm^2 sec))", "deltaH5_mar (cal/(cm^2 sec))", "deltaH5_apr (cal/(cm^2 sec))", "deltaH5_may (cal/(cm^2 sec))", "deltaH5_jun (cal/(cm^2 sec))", "deltaH5_jul (cal/(cm^2 sec))", "deltaH5_aug (cal/(cm^2 sec))", "deltaH5_sep (cal/(cm^2 sec))", "deltaH5_oct (cal/(cm^2 sec))", "deltaH5_nov (cal/(cm^2 sec))", "deltaH5_dec (cal/(cm^2 sec))"))


iteration5 <- data.table(T5, percentchangeTprime4, eTprime5, deltaE5, deltaC5, deltaR5, deltaH5)
iteration5[, c(14, 27, 40, 53, 66, 79) := NULL]



## iteration 6
# Estimated heated water temperature T' (deg C)1
T6 <- data.table(T4, T5, deltaH4, deltaH5, deltaHf)
T6[, c(14, 27, 40, 53) := NULL]
setnames(T6, 2:length(T6), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(T6, Plant_ID)
# Source 7 - 9 begins
fun33 <- function(a, b, c, d, e) { ifelse(d != c, (a + (b - a) * ((e - c) / (d - c))), b)
}

T6a <- T6[, lapply(1:12, function(i) ifelse(!is.na(c(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), fun33(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i))), NA_real_)), by = Plant_ID]
# Source 7 - 9 ends
T6 <- T6a[pmatch(T6$Plant_ID, T6a$Plant_ID), ]
setnames(T6, 2:length(T6), c("T6_jan ()", "T6_feb ()", "T6_mar ()", "T6_apr ()", "T6_may ()", "T6_jun ()", "T6_jul ()", "T6_aug ()", "T6_sep ()", "T6_oct ()", "T6_nov ()", "T6_dec ()"))
setkey(T6, Plant_ID)


# Percent change in T' from previous iteration1
percentchangeTprime5 <- data.table(T5, T6)
percentchangeTprime5[, 14 := NULL]
setkey(percentchangeTprime5, Plant_ID)
setnames(percentchangeTprime5 , 2:length(percentchangeTprime5), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun34 <- function(a, b) ((b - a) / a) * 100

percentchangeTprime5 <- percentchangeTprime5[, lapply(1:12, function(i) fun34(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(percentchangeTprime5, 2:length(percentchangeTprime5), c("percent change in T'5_jan", "percent change in T'5_feb", "percent change in T'5_mar", "percent change in T'5_apr", "percent change in T'5_may", "percent change in T'5_jun", "percent change in T'5_jul", "percent change in T'5_aug", "percent change in T'5_sep", "percent change in T'5_oct", "percent change in T'5_nov", "percent change in T'5_dec"))


# eTprime6
# Source 9 begins
eTprime6function <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
eTprime6 <- T6[, lapply(.SD, function(x) ifelse(!is.na(x), eTprime6function(x), NA_real_)), by = key(T6)]
# Source 9 ends
setnames(eTprime6, 2:length(eTprime6), c("e(T')6_jan ()", "e(T')6_feb ()", "e(T')6_mar ()", "e(T')6_apr ()", "e(T')6_may ()", "e(T')6_jun ()", "e(T')6_jul ()", "e(T')6_aug ()", "e(T')6_sep ()", "e(T')6_oct ()", "e(T')6_nov ()", "e(T')6_dec ()"))


# deltaE6 / deltaE6
deltaE6 <- data.table(f_W_cm, initial_rho, heat_of_vaporization, e_T, eTprime6)
deltaE6[, c(14, 27, 40, 53) := NULL]
setkey(deltaE6, Plant_ID)
setnames(deltaE6, 2:length(deltaE6), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
# Sources 7 & 8 begin
fun35 <- function(a, b, c, d, e) b * c * a * (e - d)

deltaE6 <- deltaE6[, lapply(1:12, function(i) fun35(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaE6, 2:length(deltaE6), c("deltaE6_jan (cal/(cm^2 sec))", "deltaE6_feb (cal/(cm^2 sec))", "deltaE6_mar (cal/(cm^2 sec))", "deltaE6_apr (cal/(cm^2 sec))", "deltaE6_may (cal/(cm^2 sec))", "deltaE6_jun (cal/(cm^2 sec))", "deltaE6_jul (cal/(cm^2 sec))", "deltaE6_aug (cal/(cm^2 sec))", "deltaE6_sep (cal/(cm^2 sec))", "deltaE6_oct (cal/(cm^2 sec))", "deltaE6_nov (cal/(cm^2 sec))", "deltaE6_dec (cal/(cm^2 sec))"))


# deltaC6 / deltaC6
deltaC6 <- data.table(f_W_cm, initial_rho, T6, wt, fewsronly2)
deltaC6[, c(14, 27, 40, 53) := NULL]
setnames(deltaC6, 2:length(deltaC6), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(deltaC6, Plant_ID)
# Sources 7 & 8 begin
fun36 <- function(a, b, c, d, e) a * ((b * e * constants$constants[9]) / constants$constants[10]) * (c - d)

deltaC6 <- deltaC6[, lapply(1:12, function(i) fun36(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaC6, 2:length(deltaC6), c("deltaC6_jan (cal/(cm^2 sec))", "deltaC6_feb (cal/(cm^2 sec))", "deltaC6_mar (cal/(cm^2 sec))", "deltaC6_apr (cal/(cm^2 sec))", "deltaC6_may (cal/(cm^2 sec))", "deltaC6_jun (cal/(cm^2 sec))", "deltaC6_jul (cal/(cm^2 sec))", "deltaC6_aug (cal/(cm^2 sec))", "deltaC6_sep (cal/(cm^2 sec))", "deltaC6_oct (cal/(cm^2 sec))", "deltaC6_nov (cal/(cm^2 sec))", "deltaC6_dec (cal/(cm^2 sec))"))


# deltaR6 / deltaR6
deltaR6 <- data.table(T6, wt)
deltaR6[, 14 := NULL]
setkey(deltaR6, Plant_ID)
setnames(deltaR6, 2:length(deltaR6), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun37 <- function(a, b) (constants$constants[7] * constants$constants[8] / constants$constants[12]) * ((a + Kelvin) ^ 4 - (b + Kelvin) ^ 4)

deltaR6 <- deltaR6[, lapply(1:12, function(i) fun37(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaR6, 2:length(deltaR6), c("deltaR6_jan (cal/(cm^2 sec))", "deltaR6_feb (cal/(cm^2 sec))", "deltaR6_mar (cal/(cm^2 sec))", "deltaR6_apr (cal/(cm^2 sec))", "deltaR6_may (cal/(cm^2 sec))", "deltaR6_jun (cal/(cm^2 sec))", "deltaR6_jul (cal/(cm^2 sec))", "deltaR6_aug (cal/(cm^2 sec))", "deltaR6_sep (cal/(cm^2 sec))", "deltaR6_oct (cal/(cm^2 sec))", "deltaR6_nov (cal/(cm^2 sec))", "deltaR6_dec (cal/(cm^2 sec))"))


# Increment in total heat loss deltaH / deltaH6
deltaH6 <- data.table(deltaE6, deltaC6, deltaR6)
deltaH6[, c(14, 27) := NULL]
setkey(deltaH6, Plant_ID)
setnames(deltaH6 , 2:length(deltaH6), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun38 <- function(a, b, c) a + b + c

deltaH6 <- deltaH6[, lapply(1:12, function(i) fun38(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaH6, 2:length(deltaH6), c("deltaH6_jan (cal/(cm^2 sec))", "deltaH6_feb (cal/(cm^2 sec))", "deltaH6_mar (cal/(cm^2 sec))", "deltaH6_apr (cal/(cm^2 sec))", "deltaH6_may (cal/(cm^2 sec))", "deltaH6_jun (cal/(cm^2 sec))", "deltaH6_jul (cal/(cm^2 sec))", "deltaH6_aug (cal/(cm^2 sec))", "deltaH6_sep (cal/(cm^2 sec))", "deltaH6_oct (cal/(cm^2 sec))", "deltaH6_nov (cal/(cm^2 sec))", "deltaH6_dec (cal/(cm^2 sec))"))


iteration6 <- data.table(T6, percentchangeTprime5, eTprime6, deltaE6, deltaC6, deltaR6, deltaH6)
iteration6[, c(14, 27, 40, 53, 66, 79) := NULL]


## iteration 7
# Estimated heated water temperature T' (deg C)1
T7 <- data.table(T5, T6, deltaH5, deltaH6, deltaHf)
T7[, c(14, 27, 40, 53) := NULL]
setnames(T7, 2:length(T7), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(T7, Plant_ID)
# Source 7 - 9 begins
fun39 <- function(a, b, c, d, e) { ifelse(d != c, (a + (b - a) * ((e - c) / (d - c))), b)
}

T7a <- T7[, lapply(1:12, function(i) ifelse(!is.na(c(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), fun39(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i))), NA_real_)), by = Plant_ID]
# Source 7 - 9 ends
T7 <- T7a[pmatch(T7$Plant_ID, T7a$Plant_ID), ]
setnames(T7, 2:length(T7), c("T7_jan ()", "T7_feb ()", "T7_mar ()", "T7_apr ()", "T7_may ()", "T7_jun ()", "T7_jul ()", "T7_aug ()", "T7_sep ()", "T7_oct ()", "T7_nov ()", "T7_dec ()"))
setkey(T7, Plant_ID)


# Percent change in T' from previous iteration1
percentchangeTprime6 <- data.table(T6, T7)
percentchangeTprime6[, 14 := NULL]
setkey(percentchangeTprime6, Plant_ID)
setnames(percentchangeTprime6 , 2:length(percentchangeTprime6), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun40 <- function(a, b) ((b - a) / a) * 100

percentchangeTprime6 <- percentchangeTprime6[, lapply(1:12, function(i) fun40(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(percentchangeTprime6, 2:length(percentchangeTprime6), c("percent change in T'6_jan", "percent change in T'6_feb", "percent change in T'6_mar", "percent change in T'6_apr", "percent change in T'6_may", "percent change in T'6_jun", "percent change in T'6_jul", "percent change in T'6_aug", "percent change in T'6_sep", "percent change in T'6_oct", "percent change in T'6_nov", "percent change in T'6_dec"))


# eTprime7
# Source 9 begins
eTprime7function <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
eTprime7 <- T7[, lapply(.SD, function(x) ifelse(!is.na(x), eTprime7function(x), NA_real_)), by = key(T7)]
# Source 9 ends
setnames(eTprime7, 2:length(eTprime7), c("e(T')7_jan ()", "e(T')7_feb ()", "e(T')7_mar ()", "e(T')7_apr ()", "e(T')7_may ()", "e(T')7_jun ()", "e(T')7_jul ()", "e(T')7_aug ()", "e(T')7_sep ()", "e(T')7_oct ()", "e(T')7_nov ()", "e(T')7_dec ()"))


# deltaE7 / deltaE7
deltaE7 <- data.table(f_W_cm, initial_rho, heat_of_vaporization, e_T, eTprime7)
deltaE7[, c(14, 27, 40, 53) := NULL]
setkey(deltaE7, Plant_ID)
setnames(deltaE7, 2:length(deltaE7), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
# Sources 7 & 8 begin
fun41 <- function(a, b, c, d, e) b * c * a * (e - d)

deltaE7 <- deltaE7[, lapply(1:12, function(i) fun41(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaE7, 2:length(deltaE7), c("deltaE7_jan (cal/(cm^2 sec))", "deltaE7_feb (cal/(cm^2 sec))", "deltaE7_mar (cal/(cm^2 sec))", "deltaE7_apr (cal/(cm^2 sec))", "deltaE7_may (cal/(cm^2 sec))", "deltaE7_jun (cal/(cm^2 sec))", "deltaE7_jul (cal/(cm^2 sec))", "deltaE7_aug (cal/(cm^2 sec))", "deltaE7_sep (cal/(cm^2 sec))", "deltaE7_oct (cal/(cm^2 sec))", "deltaE7_nov (cal/(cm^2 sec))", "deltaE7_dec (cal/(cm^2 sec))"))


# deltaC7 / deltaC7
deltaC7 <- data.table(f_W_cm, initial_rho, T7, wt, fewsronly2)
deltaC7[, c(14, 27, 40, 53) := NULL]
setnames(deltaC7, 2:length(deltaC7), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(deltaC7, Plant_ID)
# Sources 7 & 8 begin
fun42 <- function(a, b, c, d, e) a * ((b * e * constants$constants[9]) / constants$constants[10]) * (c - d)

deltaC7 <- deltaC7[, lapply(1:12, function(i) fun42(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaC7, 2:length(deltaC7), c("deltaC7_jan (cal/(cm^2 sec))", "deltaC7_feb (cal/(cm^2 sec))", "deltaC7_mar (cal/(cm^2 sec))", "deltaC7_apr (cal/(cm^2 sec))", "deltaC7_may (cal/(cm^2 sec))", "deltaC7_jun (cal/(cm^2 sec))", "deltaC7_jul (cal/(cm^2 sec))", "deltaC7_aug (cal/(cm^2 sec))", "deltaC7_sep (cal/(cm^2 sec))", "deltaC7_oct (cal/(cm^2 sec))", "deltaC7_nov (cal/(cm^2 sec))", "deltaC7_dec (cal/(cm^2 sec))"))


# deltaR7 / deltaR7
deltaR7 <- data.table(T7, wt)
deltaR7[, 14 := NULL]
setkey(deltaR7, Plant_ID)
setnames(deltaR7, 2:length(deltaR7), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun43 <- function(a, b) (constants$constants[7] * constants$constants[8] / constants$constants[12]) * ((a + Kelvin) ^ 4 - (b + Kelvin) ^ 4)

deltaR7 <- deltaR7[, lapply(1:12, function(i) fun43(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaR7, 2:length(deltaR7), c("deltaR7_jan (cal/(cm^2 sec))", "deltaR7_feb (cal/(cm^2 sec))", "deltaR7_mar (cal/(cm^2 sec))", "deltaR7_apr (cal/(cm^2 sec))", "deltaR7_may (cal/(cm^2 sec))", "deltaR7_jun (cal/(cm^2 sec))", "deltaR7_jul (cal/(cm^2 sec))", "deltaR7_aug (cal/(cm^2 sec))", "deltaR7_sep (cal/(cm^2 sec))", "deltaR7_oct (cal/(cm^2 sec))", "deltaR7_nov (cal/(cm^2 sec))", "deltaR7_dec (cal/(cm^2 sec))"))


# Increment in total heat loss deltaH / deltaH7
deltaH7 <- data.table(deltaE7, deltaC7, deltaR7)
deltaH7[, c(14, 27) := NULL]
setkey(deltaH7, Plant_ID)
setnames(deltaH7 , 2:length(deltaH7), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun44 <- function(a, b, c) a + b + c

deltaH7 <- deltaH7[, lapply(1:12, function(i) fun44(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaH7, 2:length(deltaH7), c("deltaH7_jan (cal/(cm^2 sec))", "deltaH7_feb (cal/(cm^2 sec))", "deltaH7_mar (cal/(cm^2 sec))", "deltaH7_apr (cal/(cm^2 sec))", "deltaH7_may (cal/(cm^2 sec))", "deltaH7_jun (cal/(cm^2 sec))", "deltaH7_jul (cal/(cm^2 sec))", "deltaH7_aug (cal/(cm^2 sec))", "deltaH7_sep (cal/(cm^2 sec))", "deltaH7_oct (cal/(cm^2 sec))", "deltaH7_nov (cal/(cm^2 sec))", "deltaH7_dec (cal/(cm^2 sec))"))


iteration7 <- data.table(T7, percentchangeTprime6, eTprime7, deltaE7, deltaC7, deltaR7, deltaH7)
iteration7[, c(14, 27, 40, 53, 66, 79) := NULL]




## iteration 8
# Estimated heated water temperature T' (deg C)1
T8 <- data.table(T6, T7, deltaH6, deltaH7, deltaHf)
T8[, c(14, 27, 40, 53) := NULL]
setnames(T8, 2:length(T8), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(T8, Plant_ID)
# Source 7 - 9 begins
fun45 <- function(a, b, c, d, e) { ifelse(d != c, (a + (b - a) * ((e - c) / (d - c))), b)
}

T8a <- T8[, lapply(1:12, function(i) ifelse(!is.na(c(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), fun45(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i))), NA_real_)), by = Plant_ID]
# Source 7 - 9 ends
T8 <- T8a[pmatch(T8$Plant_ID, T8a$Plant_ID), ]
setnames(T8, 2:length(T8), c("T8_jan ()", "T8_feb ()", "T8_mar ()", "T8_apr ()", "T8_may ()", "T8_jun ()", "T8_jul ()", "T8_aug ()", "T8_sep ()", "T8_oct ()", "T8_nov ()", "T8_dec ()"))
setkey(T8, Plant_ID)


# Percent change in T' from previous iteration1
percentchangeTprime7 <- data.table(T7, T8)
percentchangeTprime7[, 14 := NULL]
setkey(percentchangeTprime7, Plant_ID)
setnames(percentchangeTprime7 , 2:length(percentchangeTprime7), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun46 <- function(a, b) ((b - a) / a) * 100

percentchangeTprime7 <- percentchangeTprime7[, lapply(1:12, function(i) fun46(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(percentchangeTprime7, 2:length(percentchangeTprime7), c("percent change in T'7_jan", "percent change in T'7_feb", "percent change in T'7_mar", "percent change in T'7_apr", "percent change in T'7_may", "percent change in T'7_jun", "percent change in T'7_jul", "percent change in T'7_aug", "percent change in T'7_sep", "percent change in T'7_oct", "percent change in T'7_nov", "percent change in T'7_dec"))


# eTprime8
# Source 9 begins
eTprime8function <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
eTprime8 <- T8[, lapply(.SD, function(x) ifelse(!is.na(x), eTprime8function(x), NA_real_)), by = key(T8)]
# Source 9 ends
setnames(eTprime8, 2:length(eTprime8), c("e(T')8_jan ()", "e(T')8_feb ()", "e(T')8_mar ()", "e(T')8_apr ()", "e(T')8_may ()", "e(T')8_jun ()", "e(T')8_jul ()", "e(T')8_aug ()", "e(T')8_sep ()", "e(T')8_oct ()", "e(T')8_nov ()", "e(T')8_dec ()"))


# deltaE8 / deltaE8
deltaE8 <- data.table(f_W_cm, initial_rho, heat_of_vaporization, e_T, eTprime8)
deltaE8[, c(14, 27, 40, 53) := NULL]
setkey(deltaE8, Plant_ID)
setnames(deltaE8, 2:length(deltaE8), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
# Sources 7 & 8 begin
fun47 <- function(a, b, c, d, e) b * c * a * (e - d)

deltaE8 <- deltaE8[, lapply(1:12, function(i) fun47(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaE8, 2:length(deltaE8), c("deltaE8_jan (cal/(cm^2 sec))", "deltaE8_feb (cal/(cm^2 sec))", "deltaE8_mar (cal/(cm^2 sec))", "deltaE8_apr (cal/(cm^2 sec))", "deltaE8_may (cal/(cm^2 sec))", "deltaE8_jun (cal/(cm^2 sec))", "deltaE8_jul (cal/(cm^2 sec))", "deltaE8_aug (cal/(cm^2 sec))", "deltaE8_sep (cal/(cm^2 sec))", "deltaE8_oct (cal/(cm^2 sec))", "deltaE8_nov (cal/(cm^2 sec))", "deltaE8_dec (cal/(cm^2 sec))"))


# deltaC8 / deltaC8
deltaC8 <- data.table(f_W_cm, initial_rho, T8, wt, fewsronly2)
deltaC8[, c(14, 27, 40, 53) := NULL]
setnames(deltaC8, 2:length(deltaC8), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(deltaC8, Plant_ID)
# Sources 7 & 8 begin
fun48 <- function(a, b, c, d, e) a * ((b * e * constants$constants[9]) / constants$constants[10]) * (c - d)

deltaC8 <- deltaC8[, lapply(1:12, function(i) fun48(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaC8, 2:length(deltaC8), c("deltaC8_jan (cal/(cm^2 sec))", "deltaC8_feb (cal/(cm^2 sec))", "deltaC8_mar (cal/(cm^2 sec))", "deltaC8_apr (cal/(cm^2 sec))", "deltaC8_may (cal/(cm^2 sec))", "deltaC8_jun (cal/(cm^2 sec))", "deltaC8_jul (cal/(cm^2 sec))", "deltaC8_aug (cal/(cm^2 sec))", "deltaC8_sep (cal/(cm^2 sec))", "deltaC8_oct (cal/(cm^2 sec))", "deltaC8_nov (cal/(cm^2 sec))", "deltaC8_dec (cal/(cm^2 sec))"))


# deltaR8 / deltaR8
deltaR8 <- data.table(T8, wt)
deltaR8[, 14 := NULL]
setkey(deltaR8, Plant_ID)
setnames(deltaR8, 2:length(deltaR8), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun49 <- function(a, b) (constants$constants[7] * constants$constants[8] / constants$constants[12]) * ((a + Kelvin) ^ 4 - (b + Kelvin) ^ 4)

deltaR8 <- deltaR8[, lapply(1:12, function(i) fun49(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaR8, 2:length(deltaR8), c("deltaR8_jan (cal/(cm^2 sec))", "deltaR8_feb (cal/(cm^2 sec))", "deltaR8_mar (cal/(cm^2 sec))", "deltaR8_apr (cal/(cm^2 sec))", "deltaR8_may (cal/(cm^2 sec))", "deltaR8_jun (cal/(cm^2 sec))", "deltaR8_jul (cal/(cm^2 sec))", "deltaR8_aug (cal/(cm^2 sec))", "deltaR8_sep (cal/(cm^2 sec))", "deltaR8_oct (cal/(cm^2 sec))", "deltaR8_nov (cal/(cm^2 sec))", "deltaR8_dec (cal/(cm^2 sec))"))


# Increment in total heat loss deltaH / deltaH8
deltaH8 <- data.table(deltaE8, deltaC8, deltaR8)
deltaH8[, c(14, 27) := NULL]
setkey(deltaH8, Plant_ID)
setnames(deltaH8 , 2:length(deltaH8), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun50 <- function(a, b, c) a + b + c

deltaH8 <- deltaH8[, lapply(1:12, function(i) fun50(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaH8, 2:length(deltaH8), c("deltaH8_jan (cal/(cm^2 sec))", "deltaH8_feb (cal/(cm^2 sec))", "deltaH8_mar (cal/(cm^2 sec))", "deltaH8_apr (cal/(cm^2 sec))", "deltaH8_may (cal/(cm^2 sec))", "deltaH8_jun (cal/(cm^2 sec))", "deltaH8_jul (cal/(cm^2 sec))", "deltaH8_aug (cal/(cm^2 sec))", "deltaH8_sep (cal/(cm^2 sec))", "deltaH8_oct (cal/(cm^2 sec))", "deltaH8_nov (cal/(cm^2 sec))", "deltaH8_dec (cal/(cm^2 sec))"))


iteration8 <- data.table(T8, percentchangeTprime7, eTprime8, deltaE8, deltaC8, deltaR8, deltaH8)
iteration8[, c(14, 27, 40, 53, 66, 79) := NULL]



## iteration 9
## Output
# Estimated heated water temperature T' (deg C)
T9 <- data.table(T7, T8, deltaH7, deltaH8, deltaHf)
T9[, c(14, 27, 40, 53) := NULL]
setnames(T9, 2:length(T9), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(T9, Plant_ID)
# Source 7 - 9 begins
fun51 <- function(a, b, c, d, e) { ifelse(d != c, (a + (b - a) * ((e - c) / (d - c))), b)
}

T9a <- T9[, lapply(1:12, function(i) ifelse(!is.na(c(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), fun51(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i))), NA_real_)), by = Plant_ID]
# Source 7 - 9 ends
T9 <- T9a[pmatch(T9$Plant_ID, T9a$Plant_ID), ]
setnames(T9, 2:length(T9), c("T9_jan ()", "T9_feb ()", "T9_mar ()", "T9_apr ()", "T9_may ()", "T9_jun ()", "T9_jul ()", "T9_aug ()", "T9_sep ()", "T9_oct ()", "T9_nov ()", "T9_dec ()"))
setkey(T9, Plant_ID)


# Percent change in T' from previous iteration1
percentchangeTprime8 <- data.table(T8, T9)
percentchangeTprime8[, 14 := NULL]
setkey(percentchangeTprime8, Plant_ID)
setnames(percentchangeTprime8 , 2:length(percentchangeTprime8), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun52 <- function(a, b) ((b - a) / a) * 100

percentchangeTprime8 <- percentchangeTprime8[, lapply(1:12, function(i) fun52(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(percentchangeTprime8, 2:length(percentchangeTprime8), c("percent change in T'8_jan", "percent change in T'8_feb", "percent change in T'8_mar", "percent change in T'8_apr", "percent change in T'8_may", "percent change in T'8_jun", "percent change in T'8_jul", "percent change in T'8_aug", "percent change in T'8_sep", "percent change in T'8_oct", "percent change in T'8_nov", "percent change in T'8_dec"))


## Output values for evaporation depth
# Final \u03c1 (g/cm^3), water density
# based on sixth iteration (deltaR6)
final_rho <- copy(deltaR6)
# Source 9 begins
final_rhofunction <- function(x) {
    0.99983 + 0.00005 * x - 0.00000625 * x ^ 2}
final_rho <- final_rho[, lapply(.SD, function(x) ifelse(!is.na(x), final_rhofunction(x), NA_real_)), by = key(final_rho)]
# Source 9 ends
setnames(final_rho, 2:length(final_rho), c("\u03c1_jan (g/cm^3)", "\u03c1_feb (g/cm^3)", "\u03c1_mar (g/cm^3)", "\u03c1_apr (g/cm^3)", "\u03c1_may (g/cm^3)", "\u03c1_jun (g/cm^3)", "\u03c1_jul (g/cm^3)", "\u03c1_aug (g/cm^3)", "\u03c1_sep (g/cm^3)", "\u03c1_oct (g/cm^3)", "\u03c1_nov (g/cm^3)", "\u03c1_dec (g/cm^3)"))


# heat of vaporization [L (cal/g)]
# based on sixth iteration (deltaR6)
heat_vapor <- copy(deltaR6)
# Source 9 begins
heat_vaporfunction <- function(x) {
    595.9 - 0.545 * x}
heat_vapor <- heat_vapor[, lapply(.SD, function(x) ifelse(!is.na(x), heat_vaporfunction(x), NA_real_)), by = key(heat_vapor)]
# Source 9 ends
setnames(heat_vapor, 2:length(heat_vapor), c("L_jan (cal/g)", "L_feb (cal/g)", "L_mar (cal/g)", "L_apr (cal/g)", "L_may (cal/g)", "L_jun (cal/g)", "L_jul (cal/g)", "L_aug (cal/g)", "L_sep (cal/g)", "L_oct (cal/g)", "L_nov (cal/g)", "L_dec (cal/g)"))


# eTprime9
# Source 9 begins
eTprime9function <- function(x) {
    ifelse(x < 62.276, 6.107799961 + x * (0.4436518521 + x * (0.01428945805 + x * (0.0002650648471 + x * (0.000003031240396 + x * (0.00000002034080948 + x * 0.00000000006136820929))))),-296.901212123675 + 16.9015967001546 * x - 0.302242100380422 * x ^ 2 + 0.00264123776535373 * x ^ 3)}
eTprime9 <- T9[, lapply(.SD, function(x) ifelse(!is.na(x), eTprime9function(x), NA_real_)), by = key(T9)]
# Source 9 ends
setnames(eTprime9, 2:length(eTprime9), c("e(T')9_jan ()", "e(T')9_feb ()", "e(T')9_mar ()", "e(T')9_apr ()", "e(T')9_may ()", "e(T')9_jun ()", "e(T')9_jul ()", "e(T')9_aug ()", "e(T')9_sep ()", "e(T')9_oct ()", "e(T')9_nov ()", "e(T')9_dec ()"))


# deltaE9 / deltaE9
deltaE9 <- data.table(f_W_cm, initial_rho, heat_of_vaporization, e_T, eTprime9)
deltaE9[, c(14, 27, 40, 53) := NULL]
setkey(deltaE9, Plant_ID)
setnames(deltaE9, 2:length(deltaE9), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
# Sources 7 & 8 begin
fun53 <- function(a, b, c, d, e) b * c * a * (e - d)

deltaE9 <- deltaE9[, lapply(1:12, function(i) fun53(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaE9, 2:length(deltaE9), c("deltaE9_jan (cal/(cm^2 sec))", "deltaE9_feb (cal/(cm^2 sec))", "deltaE9_mar (cal/(cm^2 sec))", "deltaE9_apr (cal/(cm^2 sec))", "deltaE9_may (cal/(cm^2 sec))", "deltaE9_jun (cal/(cm^2 sec))", "deltaE9_jul (cal/(cm^2 sec))", "deltaE9_aug (cal/(cm^2 sec))", "deltaE9_sep (cal/(cm^2 sec))", "deltaE9_oct (cal/(cm^2 sec))", "deltaE9_nov (cal/(cm^2 sec))", "deltaE9_dec (cal/(cm^2 sec))"))


# deltaC9 / deltaC9
deltaC9 <- data.table(f_W_cm, initial_rho, T9, wt, fewsronly2)
deltaC9[, c(14, 27, 40, 53) := NULL]
setnames(deltaC9, 2:length(deltaC9), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11", "e12"))
setkey(deltaC9, Plant_ID)
# Sources 7 & 8 begin
fun54 <- function(a, b, c, d, e) a * ((b * e * constants$constants[9]) / constants$constants[10]) * (c - d)

deltaC9 <- deltaC9[, lapply(1:12, function(i) fun54(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)), get(paste0('e', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaC9, 2:length(deltaC9), c("deltaC9_jan (cal/(cm^2 sec))", "deltaC9_feb (cal/(cm^2 sec))", "deltaC9_mar (cal/(cm^2 sec))", "deltaC9_apr (cal/(cm^2 sec))", "deltaC9_may (cal/(cm^2 sec))", "deltaC9_jun (cal/(cm^2 sec))", "deltaC9_jul (cal/(cm^2 sec))", "deltaC9_aug (cal/(cm^2 sec))", "deltaC9_sep (cal/(cm^2 sec))", "deltaC9_oct (cal/(cm^2 sec))", "deltaC9_nov (cal/(cm^2 sec))", "deltaC9_dec (cal/(cm^2 sec))"))


# deltaR9 / deltaR9
deltaR9 <- data.table(T9, wt)
deltaR9[, 14 := NULL]
setkey(deltaR9, Plant_ID)
setnames(deltaR9, 2:length(deltaR9), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun55 <- function(a, b) (constants$constants[7] * constants$constants[8] / constants$constants[12]) * ((a + Kelvin) ^ 4 - (b + Kelvin) ^ 4)

deltaR9 <- deltaR9[, lapply(1:12, function(i) fun55(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaR9, 2:length(deltaR9), c("deltaR9_jan (cal/(cm^2 sec))", "deltaR9_feb (cal/(cm^2 sec))", "deltaR9_mar (cal/(cm^2 sec))", "deltaR9_apr (cal/(cm^2 sec))", "deltaR9_may (cal/(cm^2 sec))", "deltaR9_jun (cal/(cm^2 sec))", "deltaR9_jul (cal/(cm^2 sec))", "deltaR9_aug (cal/(cm^2 sec))", "deltaR9_sep (cal/(cm^2 sec))", "deltaR9_oct (cal/(cm^2 sec))", "deltaR9_nov (cal/(cm^2 sec))", "deltaR9_dec (cal/(cm^2 sec))"))


# Increment in total heat loss deltaH / deltaH9
deltaH9 <- data.table(deltaE9, deltaC9, deltaR9)
deltaH9[, c(14, 27) := NULL]
setkey(deltaH9, Plant_ID)
setnames(deltaH9 , 2:length(deltaH9), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun56 <- function(a, b, c) a + b + c

deltaH9 <- deltaH9[, lapply(1:12, function(i) fun56(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaH9, 2:length(deltaH9), c("deltaH9_jan (cal/(cm^2 sec))", "deltaH9_feb (cal/(cm^2 sec))", "deltaH9_mar (cal/(cm^2 sec))", "deltaH9_apr (cal/(cm^2 sec))", "deltaH9_may (cal/(cm^2 sec))", "deltaH9_jun (cal/(cm^2 sec))", "deltaH9_jul (cal/(cm^2 sec))", "deltaH9_aug (cal/(cm^2 sec))", "deltaH9_sep (cal/(cm^2 sec))", "deltaH9_oct (cal/(cm^2 sec))", "deltaH9_nov (cal/(cm^2 sec))", "deltaH9_dec (cal/(cm^2 sec))"))


iteration9 <- data.table(T9, percentchangeTprime8, eTprime9, deltaE9, deltaC9, deltaR9, deltaH9)
iteration9[, c(14, 27, 40, 53, 66, 79) := NULL]



## Output
# This section converts evaporation to gallons per million Btu of condenser duty for comparison to literature values
# Heat of vaporization (cal/cc)
heat_vaporization <- data.table(final_rho, heat_vapor)
heat_vaporization[, 14 := NULL]
setkey(heat_vaporization, Plant_ID)
setnames(heat_vaporization, 2:length(heat_vaporization), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun57 <- function(a, b) a * b

heat_vaporization <- heat_vaporization[, lapply(1:12, function(i) fun57(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(heat_vaporization, 2:length(heat_vaporization), c("L_jan (cal/(cal/cc))", "L_feb (cal/(cal/cc))", "L_mar (cal/(cal/cc))", "L_apr (cal/(cal/cc))", "L_may (cal/(cal/cc))", "L_jun (cal/(cal/cc))", "L_jul (cal/(cal/cc))", "L_aug (cal/(cal/cc))", "L_sep (cal/(cal/cc))", "L_oct (cal/(cal/cc))", "L_nov (cal/(cal/cc))", "L_dec (cal/(cal/cc))"))


# Volumetric heat of vaporization (Btu/gal)
vol_heat_vaporization <- copy(heat_vaporization)
setkey(vol_heat_vaporization, Plant_ID)
setnames(vol_heat_vaporization, 2:length(vol_heat_vaporization), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))
# Sources 7 & 8 begin
fun58 <- function(a, b) constants$constants[2] * a / constants$constants[1]

vol_heat_vaporization <- vol_heat_vaporization[, lapply(1:12, function(i) fun58(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(vol_heat_vaporization, 2:length(vol_heat_vaporization), c("Volumetric_L_jan (Btu/gal)", "Volumetric_L_feb (Btu/gal)", "Volumetric_L_mar (Btu/gal)", "Volumetric_L_apr (Btu/gal)", "Volumetric_L_may (Btu/gal)", "Volumetric_L_jun (Btu/gal)", "Volumetric_L_jul (Btu/gal)", "Volumetric_L_aug (Btu/gal)", "Volumetric_L_sep (Btu/gal)", "Volumetric_L_oct (Btu/gal)", "Volumetric_L_nov (Btu/gal)", "Volumetric_L_dec (Btu/gal)"))


iteration9length <- names(iteration9)[c(74:length(iteration9))]


# Percent forced evaporation
percent_forced_evap <- data.table(iteration9[, 1], iteration9[, c(38:49)], iteration9[, c(74:85)])
setkey(percent_forced_evap, Plant_ID)
setnames(percent_forced_evap, 2:length(percent_forced_evap), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun59 <- function(a, b) a / b

percent_forced_evap <- percent_forced_evap[, lapply(1:12, function(i) fun59(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(percent_forced_evap, 2:length(percent_forced_evap), c("percent forced evaporation_jan", "percent forced evaporation_feb", "percent forced evaporation_mar", "percent forced evaporation_apr", "percent forced evaporation_may", "percent forced evaporation_jun", "percent forced evaporation_jul", "percent forced evaporation_aug", "percent forced evaporation_sep", "percent forced evaporation_oct", "percent forced evaporation_nov", "percent forced evaporation_dec"))


# Condenser duty per evaporated volume (MMBtu/gal)
cond_duty_per_evap_vol <- data.table(vol_heat_vaporization, percent_forced_evap)
cond_duty_per_evap_vol[, 14 := NULL]
setkey(cond_duty_per_evap_vol, Plant_ID)
setnames(cond_duty_per_evap_vol, 2:length(cond_duty_per_evap_vol), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun60 <- function(a, b) a / (b * 1000000)

cond_duty_per_evap_vol <- cond_duty_per_evap_vol[, lapply(1:12, function(i) fun60(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(cond_duty_per_evap_vol, 2:length(cond_duty_per_evap_vol), c("Condenser cond_duty/evaporated volume_jan (MMBtu/gal)", "Condenser cond_duty/evaporated volume_feb (MMBtu/gal)", "Condenser cond_duty/evaporated volume_mar (MMBtu/gal)", "Condenser cond_duty/evaporated volume_apr (MMBtu/gal)", "Condenser cond_duty/evaporated volume_may (MMBtu/gal)", "Condenser cond_duty/evaporated volume_jun (MMBtu/gal)", "Condenser cond_duty/evaporated volume_jul (MMBtu/gal)", "Condenser cond_duty/evaporated volume_aug (MMBtu/gal)", "Condenser cond_duty/evaporated volume_sep (MMBtu/gal)", "Condenser cond_duty/evaporated volume_oct (MMBtu/gal)", "Condenser cond_duty/evaporated volume_nov (MMBtu/gal)", "Condenser cond_duty/evaporated volume_dec (MMBtu/gal)"))


# Evaporated volume per condenser duty (gal/MMBtu)
evap_vol_per_cond_duty_MMBtu <- copy(cond_duty_per_evap_vol)
setkey(evap_vol_per_cond_duty_MMBtu, Plant_ID)
setnames(evap_vol_per_cond_duty_MMBtu, 2:length(evap_vol_per_cond_duty_MMBtu), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))
# Sources 7 & 8 begin
fun61 <- function(a) 1 / a

evap_vol_per_cond_duty_MMBtu <- evap_vol_per_cond_duty_MMBtu[, lapply(1:12, function(i) fun61(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(evap_vol_per_cond_duty_MMBtu, 2:length(evap_vol_per_cond_duty_MMBtu), c("Evaporated volume/condenser duty_jan (gal/MMBtu)", "Evaporated volume/condenser duty_feb (gal/MMBtu)", "Evaporated volume/condenser duty_mar (gal/MMBtu)", "Evaporated volume/condenser duty_apr (gal/MMBtu)", "Evaporated volume/condenser duty_may (gal/MMBtu)", "Evaporated volume/condenser duty_jun (gal/MMBtu)", "Evaporated volume/condenser duty_jul (gal/MMBtu)", "Evaporated volume/condenser duty_aug (gal/MMBtu)", "Evaporated volume/condenser duty_sep (gal/MMBtu)", "Evaporated volume/condenser duty_oct (gal/MMBtu)", "Evaporated volume/condenser duty_nov (gal/MMBtu)", "Evaporated volume/condenser duty_dec (gal/MMBtu)"))


# Evaporated volume per condenser duty (gal/MWh thermal)
evap_vol_per_cond_duty_MWh_thermal <- copy(cond_duty_per_evap_vol)
setkey(evap_vol_per_cond_duty_MWh_thermal, Plant_ID)
setnames(evap_vol_per_cond_duty_MWh_thermal, 2:length(evap_vol_per_cond_duty_MWh_thermal), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))
# Sources 7 & 8 begin
fun62 <- function(a) 3412000 / (a * 1000000)

evap_vol_per_cond_duty_MWh_thermal <- evap_vol_per_cond_duty_MWh_thermal[, lapply(1:12, function(i) fun62(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(evap_vol_per_cond_duty_MWh_thermal, 2:length(evap_vol_per_cond_duty_MWh_thermal), c("Evaporated volume/condenser duty_jan (gal/MWh thermal)", "Evaporated volume/condenser duty_feb (gal/MWh thermal)", "Evaporated volume/condenser duty_mar (gal/MWh thermal)", "Evaporated volume/condenser duty_apr (gal/MWh thermal)", "Evaporated volume/condenser duty_may (gal/MWh thermal)", "Evaporated volume/condenser duty_jun (gal/MWh thermal)", "Evaporated volume/condenser duty_jul (gal/MWh thermal)", "Evaporated volume/condenser duty_aug (gal/MWh thermal)", "Evaporated volume/condenser duty_sep (gal/MWh thermal)", "Evaporated volume/condenser duty_oct (gal/MWh thermal)", "Evaporated volume/condenser duty_nov (gal/MWh thermal)", "Evaporated volume/condenser duty_dec (gal/MWh thermal)"))


# density at heated temperature (pounds per gallon)
density_heat_T <- copy(final_rho)
setkey(density_heat_T, Plant_ID)
setnames(density_heat_T, 2:length(density_heat_T), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))
# Sources 7 & 8 begin
fun63 <- function(a) a * constants$constants[2] / constants$constants[3]

density_heat_T <- density_heat_T[, lapply(1:12, function(i) fun63(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(density_heat_T, 2:length(density_heat_T), c("density at heated temperature_jan (lbs/gal)", "density at heated temperature_feb (lbs/gal)", "density at heated temperature_mar (lbs/gal)", "density at heated temperature_apr (lbs/gal)", "density at heated temperature_may (lbs/gal)", "density at heated temperature_jun (lbs/gal)", "density at heated temperature_jul (lbs/gal)", "density at heated temperature_aug (lbs/gal)", "density at heated temperature_sep (lbs/gal)", "density at heated temperature_oct (lbs/gal)", "density at heated temperature_nov (lbs/gal)", "density at heated temperature_dec (lbs/gal)"))


# Heat of vaporization (Btu/lb)
heat_vap <- data.table(vol_heat_vaporization, density_heat_T)
heat_vap[, 14 := NULL]
setkey(heat_vap, Plant_ID)
setnames(heat_vap, 2:length(heat_vap), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun64 <- function(a, b) a / b

heat_vap <- heat_vap[, lapply(1:12, function(i) fun64(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(heat_vap, 2:length(heat_vap), c("Heat of vaporization_jan (Btu/lb)", "Heat of vaporization_feb (Btu/lb)", "Heat of vaporization_mar (Btu/lb)", "Heat of vaporization_apr (Btu/lb)", "Heat of vaporization_may (Btu/lb)", "Heat of vaporization_jun (Btu/lb)", "Heat of vaporization_jul (Btu/lb)", "Heat of vaporization_aug (Btu/lb)", "Heat of vaporization_sep (Btu/lb)", "Heat of vaporization_oct (Btu/lb)", "Heat of vaporization_nov (Btu/lb)", "Heat of vaporization_dec (Btu/lb)"))


## Final groups
# output
outputfinal <- iteration9[, c(1:13)]


# Output values for evaporation depth
output_evap_depth <- data.table(final_rho, heat_vapor, iteration9[, c(26:37)])
output_evap_depth[, 14 := NULL]


# Output
# Increment in heat loss by mode
output_incr_heat_loss_mode <- iteration9[, c(1, 38:73)]


# compare literature values
output_compare_lit <- data.table(heat_vaporization, vol_heat_vaporization, percent_forced_evap, cond_duty_per_evap_vol, evap_vol_per_cond_duty_MMBtu, evap_vol_per_cond_duty_MWh_thermal, density_heat_T, heat_vap)


## Summary output section - heat loss rates are in cal/(cm^2 sec)
# Final values - Solution with added heat
# Heated water temperature T' (deg C)
# Estimated average water-surface temperature in the heated plume or cooling pond. Estimated temperatures above the boiling point won't be displayed.
heat_water_temp_Tprime <- iteration8[, c(1:13)]
setkey(heat_water_temp_Tprime, Plant_ID)

# Source 9 begins
#changed the function from "too hot" to "NA_real_"
heat_water_temp_Tprimefunction <- function(x) {
    ifelse(x < 99, x, NA_real_)}
heat_water_temp_Tprime <- heat_water_temp_Tprime[, lapply(.SD, function(x) ifelse(!is.na(x), heat_water_temp_Tprimefunction(x), NA_real_)), by = Plant_ID]
# changing for now to leave in high water temp values
#heat_water_temp_Tprime <- heat_water_temp_Tprime[, lapply(.SD, function(x) ifelse(!is.na(x),x, NA_real_)), by = Plant_ID]
# Source 9 ends

setnames(heat_water_temp_Tprime, 2:length(heat_water_temp_Tprime), c("Heated water temperature T'_jan (deg C)", "Heated water temperature T'_feb (deg C)", "Heated water temperature T'_mar (deg C)", "Heated water temperature T'_apr (deg C)", "Heated water temperature T'_may (deg C)", "Heated water temperature T'_jun (deg C)", "Heated water temperature T'_jul (deg C)", "Heated water temperature T'_aug (deg C)", "Heated water temperature T'_sep (deg C)", "Heated water temperature T'_oct (deg C)", "Heated water temperature T'_nov (deg C)", "Heated water temperature T'_dec (deg C)"))



## Move to Temperature comparison to obtain values needed for the remainder of Summary output section - heat loss rates are in cal/(cm^2 sec)
## Temperature comparison
# initial estimated ratio of delta T to heat loading
# deltaT_heat_loading_ratio_initial


# final ratio of delta T to heat loading
deltaT_heat_loading_ratio_final <- data.table(heat_water_temp_Tprime, wt, deltaHf)
deltaT_heat_loading_ratio_final[, c(14, 27) := NULL]
setkey(deltaT_heat_loading_ratio_final, Plant_ID)
setnames(deltaT_heat_loading_ratio_final, 2:length(deltaT_heat_loading_ratio_final), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun65 <- function(a, b, c) (a - b) / c

deltaT_heat_loading_ratio_final <- deltaT_heat_loading_ratio_final[, lapply(1:12, function(i) fun65(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(deltaT_heat_loading_ratio_final, 2:length(deltaT_heat_loading_ratio_final), c("final ratio of delta T to heat loading_jan", "final ratio of delta T to heat loading_feb", "final ratio of delta T to heat loading_mar", "final ratio of delta T to heat loading_apr", "final ratio of delta T to heat loading_may", "final ratio of delta T to heat loading_jun", "final ratio of delta T to heat loading_jul", "final ratio of delta T to heat loading_aug", "final ratio of delta T to heat loading_sep", "final ratio of delta T to heat loading_oct", "final ratio of delta T to heat loading_nov", "final ratio
of delta T to heat loading_dec"))


# Percent change in T' in last iteration
percent_change_Tprime_final_iteration <- data.table(iteration9[, c(1:13)], iteration8[, c(1:13)])
percent_change_Tprime_final_iteration[, 14 := NULL]
setkey(percent_change_Tprime_final_iteration, Plant_ID)
setnames(percent_change_Tprime_final_iteration, 2:length(percent_change_Tprime_final_iteration), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))

fun66 <- function(a, b) (abs(( a - b ) / a)) * 100

percent_change_Tprime_final_iteration <- percent_change_Tprime_final_iteration[, lapply(1:12, function(i) fun66(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
setnames(percent_change_Tprime_final_iteration, 2:length(percent_change_Tprime_final_iteration), c("percent change in T' in last iteration_jan", "percent change in T' in last iteration_feb", "percent change in T' in last iteration_mar", "percent change in T' in last iteration_apr", "percent change in T' in last iteration_may", "percent change in T' in last iteration_jun", "percent change in T' in last iteration_jul", "percent change in T' in last iteration_aug", "percent change in T' in last iteration_sep", "percent change in T' in last iteration_oct", "percent change in T' in last iteration_nov", "percent change in T' in last iteration_dec"))


# Maximum Percent change in T' in last iteration
max_percent_change_Tprime_final_iteration <- copy(percent_change_Tprime_final_iteration)
setkey(max_percent_change_Tprime_final_iteration, Plant_ID)
setnames(max_percent_change_Tprime_final_iteration, 2:length(max_percent_change_Tprime_final_iteration), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))
# Sources 7 & 8 begin
fun67 <- function(a) max(a, na.rm = TRUE)

max_percent_change_Tprime_final_iteration <- max_percent_change_Tprime_final_iteration[, lapply(1:12, function(i) fun67(get(paste0('a', i))))]
# Sources 7 & 8 end
setnames(max_percent_change_Tprime_final_iteration, c("Maximum percent change in T' in last iteration_jan", "Maximum percent change in T' in last iteration_feb", "Maximum percent change in T' in last iteration_mar", "Maximum percent change in T' in last iteration_apr", "Maximum percent change in T' in last iteration_may", "Maximum percent change in T' in last iteration_jun", "Maximum percent change in T' in last iteration_jul", "Maximum percent change in T' in last iteration_aug", "Maximum percent change in T' in last iteration_sep", "Maximum percent change in T' in last iteration_oct", "Maximum percent change in T' in last iteration_nov", "Maximum percent change in T' in last iteration_dec"))



## Back to Summary output section - heat loss rates are in cal/(cm^2 sec)
# Asterisk indicates declining precision at T' > 50
# An asterisk in this column indicates that the estimate should not be trusted. This column will remain blank until heated water temperature exceeds 50 degrees Celsius, which is an unrealistically high temperature for a cooling pond, and the temperature at which the accuracy of some of the fundamental functions in the method begins to decline.


# Evaporated gallons per million Btu of condenser duty
# Estimated amount of forced evaporation, in gallons, per million British thermal units (Btu) added to the water.  This estimate should be within 10-15 percent of the correct value if the natural water temperature and wind function are chosen correctly.
# evap_vol_per_cond_duty_MMBtu


# Evaporated gallons per MWh thermal condenser duty
# Estimated amount of forced evaporation, in gallons, per megawatt-hour of heat added to the water.
# evap_vol_per_cond_duty_MWh_thermal


# Increment from initial to final conditions
# Heating of water (deg C)
heat_water <- data.table(iteration9[, c(1:13)], wt)
heat_water[, 14 := NULL]
setkey(heat_water, Plant_ID)
setnames(heat_water, 2:length(heat_water), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun68 <- function(a, b) a - b

heat_water <- heat_water[, lapply(1:12, function(i) fun68(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(heat_water, 2:length(heat_water), c("Heating of water_jan (deg C)", "Heating of water_feb (deg C)", "Heating of water_mar (deg C)", "Heating of water_apr (deg C)", "Heating of water_may (deg C)", "Heating of water_jun (deg C)", "Heating of water_jul (deg C)", "Heating of water_aug (deg C)", "Heating of water_sep (deg C)", "Heating of water_oct (deg C)", "Heating of water_nov (deg C)", "Heating of water_dec (deg C)"))


# Added evaporation (cm/month)
# Estimated forced evaporation as a monthly depth.
add_evap <- data.table(iteration9[, c(1, 38:49)], heat_vaporization, monthdays)
add_evap[, 14 := NULL]
setkey(add_evap, Plant_ID)
setnames(add_evap, 2:length(add_evap), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun69 <- function(a, b, c) (a / b) * constants$constants[12] * c

add_evap <- add_evap[, lapply(1:12, function(i) fun69(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(add_evap, 2:length(add_evap), c("Added evaporation_jan (cm/month)", "Added evaporation_feb (cm/month)", "Added evaporation_mar (cm/month)", "Added evaporation_apr (cm/month)", "Added evaporation_may (cm/month)", "Added evaporation_jun (cm/month)", "Added evaporation_jul (cm/month)", "Added evaporation_aug (cm/month)", "Added evaporation_sep (cm/month)", "Added evaporation_oct (cm/month)", "Added evaporation_nov (cm/month)", "Added evaporation_dec (cm/month)"))


# Evaporative heat loss increment = final deltaE
# iteration9[, c(1, 38:49)]


# Total heat loss increment = final deltaH
# iteration9[, iteration9length, with = FALSE]


# increment percent
# Evaporative cooling as percent of added heat
# Estimated heat loss through forced evaporation as a percentage of heat added by power plant.
evap_cool_percent_add_heat <- data.table(addedheatload, iteration9[, c(38:49)], iteration9[, iteration9length, with = FALSE])
setkey(evap_cool_percent_add_heat, Plant_ID)
setnames(evap_cool_percent_add_heat, 2:length(evap_cool_percent_add_heat), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
# Sources 7 & 8 begin
fun70 <- function(a, b,  c) ifelse(a == 0, NA_real_, (b / c) * 100)

evap_cool_percent_add_heat <- evap_cool_percent_add_heat[, lapply(1:12, function(i) fun70(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(evap_cool_percent_add_heat, 2:length(evap_cool_percent_add_heat), c("Evaporative cooling as percent of added heat_jan", "Evaporative cooling as percent of added heat_feb", "Evaporative cooling as percent of added heat_mar", "Evaporative cooling as percent of added heat_apr", "Evaporative cooling as percent of added heat_may", "Evaporative cooling as percent of added heat_jun", "Evaporative cooling as percent of added heat_jul", "Evaporative cooling as percent of added heat_aug", "Evaporative cooling as percent of added heat_sep", "Evaporative cooling as percent of added heat_oct", "Evaporative cooling as percent of added heat_nov", "Evaporative cooling as percent of added heat_dec"))


# Average of Evaporative cooling as percent of added heat
mean_evap_cool_percent_add_heat <- data.table(evap_cool_percent_add_heat)
setkey(mean_evap_cool_percent_add_heat, Plant_ID)
setnames(mean_evap_cool_percent_add_heat, 2:length(mean_evap_cool_percent_add_heat), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))
# Sources 7 & 8 begin
fun71 <- function(a) mean(a, na.rm = TRUE)

mean_evap_cool_percent_add_heat <- mean_evap_cool_percent_add_heat[, lapply(1:12, function(i) fun71(get(paste0('a', i))))]
# Sources 7 & 8 end
setnames(mean_evap_cool_percent_add_heat, c("Mean Evaporative cooling as percent of added heat_jan", "Mean Evaporative cooling as percent of added heat_feb", "Mean Evaporative cooling as percent of added heat_mar", "Mean Evaporative cooling as percent of added heat_apr", "Mean Evaporative cooling as percent of added heat_may", "Mean Evaporative cooling as percent of added heat_jun", "Mean Evaporative cooling as percent of added heat_jul", "Mean Evaporative cooling as percent of added heat_aug", "Mean Evaporative cooling as percent of added heat_sep", "Mean Evaporative cooling as percent of added heat_oct", "Mean Evaporative cooling as percent of added heat_nov", "Mean Evaporative cooling as percent of added heat_dec"))


# Maximum
# max_percent_change_Tprime_final_iteration


# Percent change in T' in last iteration
# Anything but zero in this column is a sign of non-convergence in the iterative solution. It's hard to get this without very unrealistic input values.  Repeated in column AJ and calculated in column CX.
# percent_change_Tprime_final_iteration


# Gallons evaporated
# Evaporative cooling in million gallons per day for each month
evap_cool_MGD_month <- data.table(addedheatload, duty, evap_vol_per_cond_duty_MMBtu, monthdays)
evap_cool_MGD_month[, c(14, 27) := NULL]
setkey(evap_cool_MGD_month, Plant_ID)
setnames(evap_cool_MGD_month, 2:length(evap_cool_MGD_month), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12"))
# Sources 7 & 8 begin
fun72 <- function(a, b, c, d) ifelse(a == 0, NA_real_, b * c / (1000000 * d))

#aeg- this is where consumption is calculated
evap_cool_MGD_month <- evap_cool_MGD_month[, lapply(1:12, function(i) fun72(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)), get(paste0('d', i)))), by = Plant_ID]
# if model = "pond"
# evap_cool_MGD_month <- na sum(evap_cool_MGD_month, nat evap)
# Sources 7 & 8 end
setnames(evap_cool_MGD_month, 2:length(evap_cool_MGD_month), c("Evaporative cooling_jan (MGD/month)", "Evaporative cooling_feb (MGD/month)", "Evaporative cooling_mar (MGD/month)", "Evaporative cooling_apr (MGD/month)", "Evaporative cooling_may (MGD/month)", "Evaporative cooling_jun (MGD/month)", "Evaporative cooling_jul (MGD/month)", "Evaporative cooling_aug (MGD/month)", "Evaporative cooling_sep (MGD/month)", "Evaporative cooling_oct (MGD/month)", "Evaporative cooling_nov (MGD/month)", "Evaporative cooling_dec (MGD/month)"))
# add in the natural evap

#potential location of pond forced evap + natural evap. if pond:
# natural evap = pond acre*natural evap
# consumption = withdrawal = forced evap
## Final Summary output section
summary_output <- data.table(heat_water_temp_Tprime, evap_vol_per_cond_duty_MMBtu, evap_vol_per_cond_duty_MWh_thermal, heat_water, add_evap, iteration9[, c(38:49)], iteration9[, iteration9length, with = FALSE], evap_cool_percent_add_heat, evap_cool_MGD_month, max_percent_change_Tprime_final_iteration, percent_change_Tprime_final_iteration)


## Report these as our Best Consumption Estimates
# millions of gallons at all plants
evap_cool_all_plants_MG <- data.table(evap_cool_MGD_month, monthdays)
setkey(evap_cool_all_plants_MG, Plant_ID)
setnames(evap_cool_all_plants_MG, 2:length(evap_cool_all_plants_MG), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
# Sources 7 & 8 begin
fun73 <- function(a, b) sum(a, na.rm = TRUE) * 24 * b

evap_cool_all_plants_MG <- evap_cool_all_plants_MG[, lapply(1:12, function(i) fun73(get(paste0('a', i)), get(paste0('b', i))))]
# Sources 7 & 8 end
evap_cool_all_plants_MG <- evap_cool_all_plants_MG[1, ]
setnames(evap_cool_all_plants_MG, c("Millions gal evaporated at all plants_jan", "Millions gal evaporated at all plants_feb", "Millions gal evaporated at all plants_mar", "Millions gal evaporated at all plants_apr", "Millions gal evaporated at all plants_may", "Millions gal evaporated at all plants_jun", "Millions gal evaporated at all plants_jul", "Millions gal evaporated at all plants_aug", "Millions gal evaporated at all plants_sep", "Millions gal evaporated at all plants_oct", "Millions gal evaporated at all plants_nov", "Millions gal evaporated at all plants_dec"))


# Calculations for annual average MGD Evaporative cooling/Consumption
calc_annual_mean_MGD_consumpt <- data.table(evap_cool_MGD_month, monthdays)
setkey(calc_annual_mean_MGD_consumpt, Plant_ID)
setnames(calc_annual_mean_MGD_consumpt, 2:length(calc_annual_mean_MGD_consumpt), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
calc_annual_mean_MGD_consumpt[is.na(calc_annual_mean_MGD_consumpt)] <- 0 # Source 10

# Sources 7 & 8 begin
fun74 <- function(a, b) ifelse(a == 0, 0, a * b)

calc_annual_mean_MGD_consumpt <- calc_annual_mean_MGD_consumpt[, lapply(1:12, function(i) fun74(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end

setnames(calc_annual_mean_MGD_consumpt, 2:length(calc_annual_mean_MGD_consumpt), c("annual average MGD consumption_jan", "annual average MGD consumption_feb", "annual average MGD consumption_mar", "annual average MGD consumption_apr", "annual average MGD consumption_may", "annual average MGD consumption_jun", "annual average MGD consumption_jul", "annual average MGD consumption_aug", "annual average MGD consumption_sep", "annual average MGD consumption_oct", "annual average MGD consumption_nov", "annual average MGD consumption_dec"))


# mean MGD for year
mean_MGD_year_consumpt <- copy(calc_annual_mean_MGD_consumpt)
mean_MGD_year_consumpt[, "mean MGD for year consumption" := (rowSums(.SD) / 365), .SDcols = 2:13] # Source 11

sum_mean_MGD_year_consumpt <- sum(mean_MGD_year_consumpt[, 14], na.rm = TRUE)

# total MGD
# total consumption MG
total_MGD_consumpt <- sum_mean_MGD_year_consumpt * 365


## Condenser heat rise
# Estimated max condenser heat rise
est_max_condenser_heat_rise <- copy(wt)
setkey(est_max_condenser_heat_rise, Plant_ID)
setnames(est_max_condenser_heat_rise, 2:length(est_max_condenser_heat_rise), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))
# Sources 7 & 8 begin
fun75 <- function(a) 49.6775 + -0.2514 * a

est_max_condenser_heat_rise <- est_max_condenser_heat_rise[, lapply(1:12, function(i) fun75(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(est_max_condenser_heat_rise, 2:length(est_max_condenser_heat_rise), c("Estimated max condenser heat rise_jan ()", "Estimated max condenser heat rise_feb ()", "Estimated max condenser heat rise_mar ()", "Estimated max condenser heat rise_apr ()", "Estimated max condenser heat rise_may ()", "Estimated max condenser heat rise_jun ()", "Estimated max condenser heat rise_jul ()", "Estimated max condenser heat rise_aug ()", "Estimated max condenser heat rise_sep ()", "Estimated max condenser heat rise_oct ()", "Estimated max condenser heat rise_nov ()", "Estimated max condenser heat rise_dec ()"))


# Estimated condenser heat rise
est_condenser_heat_rise <- copy(wt)
setkey(est_condenser_heat_rise, Plant_ID)
setnames(est_condenser_heat_rise, 2:length(est_condenser_heat_rise), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))
# Sources 7 & 8 begin
fun76 <- function(a) 22.4645 + -0.1044 * a

est_condenser_heat_rise <- est_condenser_heat_rise[, lapply(1:12, function(i) fun76(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(est_condenser_heat_rise, 2:length(est_condenser_heat_rise), c("Estimated condenser heat rise_jan ()", "Estimated condenser heat rise_feb ()", "Estimated condenser heat rise_mar ()", "Estimated condenser heat rise_apr ()", "Estimated condenser heat rise_may ()", "Estimated condenser heat rise_jun ()", "Estimated condenser heat rise_jul ()", "Estimated condenser heat rise_aug ()", "Estimated condenser heat rise_sep ()", "Estimated condenser heat rise_oct ()", "Estimated condenser heat rise_nov ()", "Estimated condenser heat rise_dec ()"))


# Estimated minimum condenser heat rise
est_min_condenser_heat_rise <- copy(wt)
setkey(est_min_condenser_heat_rise, Plant_ID)
setnames(est_min_condenser_heat_rise, 2:length(est_min_condenser_heat_rise), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))
# Sources 7 & 8 begin
fun77 <- function(a) 2.9419 + -0.0093 * a

est_min_condenser_heat_rise <- est_min_condenser_heat_rise[, lapply(1:12, function(i) fun77(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(est_min_condenser_heat_rise, 2:length(est_min_condenser_heat_rise), c("Estimated min condenser heat rise_jan ()", "Estimated min condenser heat rise_feb ()", "Estimated min condenser heat rise_mar ()", "Estimated min condenser heat rise_apr ()", "Estimated min condenser heat rise_may ()", "Estimated min condenser heat rise_jun ()", "Estimated min condenser heat rise_jul ()", "Estimated min condenser heat rise_aug ()", "Estimated min condenser heat rise_sep ()", "Estimated min condenser heat rise_oct ()", "Estimated min condenser heat rise_nov ()", "Estimated min condenser heat rise_dec ()"))


## MGD withdrawal
# Estimated Minimum MGD Withdrawal
est_min_MGD_withdrawal <- data.table(duty, est_max_condenser_heat_rise, monthdays)
est_min_MGD_withdrawal[, 14 := NULL]
setnames(est_min_MGD_withdrawal, 2:length(est_min_MGD_withdrawal), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
setkey(est_min_MGD_withdrawal, Plant_ID)
# Sources 7 & 8 begin
fun78 <- function(a, b, c) a / (b * 8.33 * c)

est_min_MGD_withdrawal <- est_min_MGD_withdrawal[, lapply(1:12, function(i) fun78(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(est_min_MGD_withdrawal, 2:length(est_min_MGD_withdrawal), c("Estimated Minimum Withdrawal_jan (MGD)", "Estimated Minimum Withdrawal_feb (MGD)", "Estimated Minimum Withdrawal_mar (MGD)", "Estimated Minimum Withdrawal_apr (MGD)", "Estimated Minimum Withdrawal_may (MGD)", "Estimated Minimum Withdrawal_jun (MGD)", "Estimated Minimum Withdrawal_jul (MGD)", "Estimated Minimum Withdrawal_aug (MGD)", "Estimated Minimum Withdrawal_sep (MGD)", "Estimated Minimum Withdrawal_oct (MGD)", "Estimated Minimum Withdrawal_nov (MGD)", "Estimated Minimum Withdrawal_dec (MGD)"))



# Calculations for annual average MGD withdrawal
calc_annual_mean_MGD_withdr <- data.table(est_min_MGD_withdrawal, monthdays)
setkey(calc_annual_mean_MGD_withdr, Plant_ID)
setnames(calc_annual_mean_MGD_withdr, 2:length(calc_annual_mean_MGD_withdr), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
calc_annual_mean_MGD_withdr[is.na(calc_annual_mean_MGD_withdr)] <- 0 # Source 10

# Sources 7 & 8 begin
fun79 <- function(a, b) ifelse(a == 0, 0, a * b)

calc_annual_mean_MGD_withdr <- calc_annual_mean_MGD_withdr[, lapply(1:12, function(i) fun79(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end

setnames(calc_annual_mean_MGD_withdr, 2:length(calc_annual_mean_MGD_withdr), c("Millions gal evaporated at all plants_jan", "Millions gal evaporated at all plants_feb", "Millions gal evaporated at all plants_mar", "Millions gal evaporated at all plants_apr", "Millions gal evaporated at all plants_may", "Millions gal evaporated at all plants_jun", "Millions gal evaporated at all plants_jul", "Millions gal evaporated at all plants_aug", "Millions gal evaporated at all plants_sep", "Millions gal evaporated at all plants_oct", "Millions gal evaporated at all plants_nov", "Millions gal evaporated at all plants_dec"))


# mean MGD for year
mean_MGD_year_withdr <- copy(calc_annual_mean_MGD_withdr)
mean_MGD_year_withdr[, "mean MGD for year" := (rowSums(.SD) / 365), .SDcols = 2:13] # Source 11

# Estimated Minimum
mean_MGD_year1_withdr <- sum(mean_MGD_year_withdr[, 14], na.rm = TRUE)

# total MGD
# total consumption MG
total_MGD_withdr <- mean_MGD_year1_withdr * 365


# Report these as our Best Withdrawal Estimates
# Estimated MGD Withdrawal
est_MGD_withdrawal <- data.table(duty, est_condenser_heat_rise, monthdays)
est_MGD_withdrawal[, 14 := NULL]
setnames(est_MGD_withdrawal, 2:length(est_MGD_withdrawal), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
setkey(est_MGD_withdrawal, Plant_ID)
# Sources 7 & 8 begin
fun80 <- function(a, b, c) a / (b * 8.33 * c)

est_MGD_withdrawal <- est_MGD_withdrawal[, lapply(1:12, function(i) fun80(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(est_MGD_withdrawal, 2:length(est_MGD_withdrawal), c("Estimated MGD Withdrawal_jan (MGD)", "Estimated MGD Withdrawal_feb (MGD)", "Estimated MGD Withdrawal_mar (MGD)", "Estimated MGD Withdrawal_apr (MGD)", "Estimated MGD Withdrawal_may (MGD)", "Estimated MGD Withdrawal_jun (MGD)", "Estimated MGD Withdrawal_jul (MGD)", "Estimated MGD Withdrawal_aug (MGD)", "Estimated MGD Withdrawal_sep (MGD)", "Estimated MGD Withdrawal_oct (MGD)", "Estimated MGD Withdrawal_nov (MGD)", "Estimated MGD Withdrawal_dec (MGD)"))


# Calculations for annual average MGD 2
calc_annual_mean_MGD_withdr2 <- data.table(est_MGD_withdrawal, monthdays)
setkey(calc_annual_mean_MGD_withdr2, Plant_ID)
setnames(calc_annual_mean_MGD_withdr2, 2:length(calc_annual_mean_MGD_withdr2), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
calc_annual_mean_MGD_withdr2[is.na(calc_annual_mean_MGD_withdr2)] <- 0 # Source 10

# Sources 7 & 8 begin
fun81 <- function(a, b) ifelse(a == 0, 0, a * b)

calc_annual_mean_MGD_withdr2 <- calc_annual_mean_MGD_withdr2[, lapply(1:12, function(i) fun81(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end

setnames(calc_annual_mean_MGD_withdr2, 2:length(calc_annual_mean_MGD_withdr2), c("Millions gal evaporated at all plants_jan", "Millions gal evaporated at all plants_feb", "Millions gal evaporated at all plants_mar", "Millions gal evaporated at all plants_apr", "Millions gal evaporated at all plants_may", "Millions gal evaporated at all plants_jun", "Millions gal evaporated at all plants_jul", "Millions gal evaporated at all plants_aug", "Millions gal evaporated at all plants_sep", "Millions gal evaporated at all plants_oct", "Millions gal evaporated at all plants_nov", "Millions gal evaporated at all plants_dec"))


# mean MGD for year
mean_MGD_year_withdr2 <- copy(calc_annual_mean_MGD_withdr2)
mean_MGD_year_withdr2[, "mean MGD for year" := (rowSums(.SD) / 365), .SDcols = 2:13] # Source 11

# Estimated Withdrawal
mean_MGD_year1_withdr2 <- sum(mean_MGD_year_withdr2[, 14], na.rm = TRUE)

# total MGD
# total consumption MG
total_MGD_withdr2 <- mean_MGD_year1_withdr2 * 365


# Estimated Maximum MGD Withdrawal
est_max_MGD_withdrawal <- data.table(duty, est_min_condenser_heat_rise, monthdays)
est_max_MGD_withdrawal[, 14 := NULL]
setnames(est_max_MGD_withdrawal, 2:length(est_max_MGD_withdrawal), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12"))
setkey(est_max_MGD_withdrawal, Plant_ID)
# Sources 7 & 8 begin
fun82 <- function(a, b, c) a / (b * 8.33 * c)

est_max_MGD_withdrawal <- est_max_MGD_withdrawal[, lapply(1:12, function(i) fun82(get(paste0('a', i)), get(paste0('b', i)), get(paste0('c', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(est_max_MGD_withdrawal, 2:length(est_max_MGD_withdrawal), c("Estimated Maximum Withdrawal_jan (MGD)", "Estimated Maximum Withdrawal_feb (MGD)", "Estimated Maximum Withdrawal_mar (MGD)", "Estimated Maximum Withdrawal_apr (MGD)", "Estimated Maximum Withdrawal_may (MGD)", "Estimated Maximum Withdrawal_jun (MGD)", "Estimated Maximum Withdrawal_jul (MGD)", "Estimated Maximum Withdrawal_aug (MGD)", "Estimated Maximum Withdrawal_sep (MGD)", "Estimated Maximum Withdrawal_oct (MGD)", "Estimated Maximum Withdrawal_nov (MGD)", "Estimated Maximum Withdrawal_dec (MGD)"))


# Calculations for annual average MGD 3
calc_annual_mean_MGD_withdr3 <- data.table(est_max_MGD_withdrawal, monthdays)
setkey(calc_annual_mean_MGD_withdr3, Plant_ID)
setnames(calc_annual_mean_MGD_withdr3, 2:length(calc_annual_mean_MGD_withdr3), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))
calc_annual_mean_MGD_withdr3[is.na(calc_annual_mean_MGD_withdr3)] <- 0 # Source 10

# Sources 7 & 8 begin
fun83 <- function(a, b) ifelse(a == 0, 0, a * b)

calc_annual_mean_MGD_withdr3 <- calc_annual_mean_MGD_withdr3[, lapply(1:12, function(i) fun83(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end

setnames(calc_annual_mean_MGD_withdr3, 2:length(calc_annual_mean_MGD_withdr3), c("Millions gal evaporated at all plants_jan", "Millions gal evaporated at all plants_feb", "Millions gal evaporated at all plants_mar", "Millions gal evaporated at all plants_apr", "Millions gal evaporated at all plants_may", "Millions gal evaporated at all plants_jun", "Millions gal evaporated at all plants_jul", "Millions gal evaporated at all plants_aug", "Millions gal evaporated at all plants_sep", "Millions gal evaporated at all plants_oct", "Millions gal evaporated at all plants_nov", "Millions gal evaporated at all plants_dec"))


# mean MGD for year
mean_MGD_year_withdr3 <- copy(calc_annual_mean_MGD_withdr3)
mean_MGD_year_withdr3[, "mean MGD for year" := (rowSums(.SD) / 365), .SDcols = 2:13] # Source 11

# Estimated Withdrawal
mean_MGD_year1_withdr3 <- sum(mean_MGD_year_withdr3[, 14], na.rm = TRUE)

# total MGD
# total consumption MG
total_MGD_withdr3 <- mean_MGD_year1_withdr3 * 365


## Report block for cushioned Consumption minimums and maximums
# Minimum Consumption with 22% cushion
# Minimum consumption in million gallons per day for each month
cushion1 <- 0.22 # input

min_consumpt_cushion <- copy(evap_cool_MGD_month)
setkey(min_consumpt_cushion, Plant_ID)
setnames(min_consumpt_cushion, 2:length(min_consumpt_cushion), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))

min_consumpt_cushion[is.na(min_consumpt_cushion)] <- 0 # Source 10

# Sources 7 & 8 begin
fun84 <- function(a) ifelse(a == 0, 0, a * (1 - cushion1))

min_consumpt_cushion <- min_consumpt_cushion[, lapply(1:12, function(i) fun84(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(min_consumpt_cushion, 2:length(min_consumpt_cushion), c("Minimum Consumption with 22 pct cushion_jan (MGD)", "Minimum Consumption with 22 pct cushion_feb (MGD)", "Minimum Consumption with 22 pct cushion_mar (MGD)", "Minimum Consumption with 22 pct cushion_apr (MGD)", "Minimum Consumption with 22 pct cushion_may (MGD)", "Minimum Consumption with 22 pct cushion_jun (MGD)", "Minimum Consumption with 22 pct cushion_jul (MGD)", "Minimum Consumption with 22 pct cushion_aug (MGD)", "Minimum Consumption with 22 pct cushion_sep (MGD)", "Minimum Consumption with 22 pct cushion_oct (MGD)", "Minimum Consumption with 22 pct cushion_nov (MGD)", "Minimum Consumption with 22 pct cushion_dec (MGD)"))


# Maximum consumption
# Maximum consumption in million gallons per day for each month
max_consumpt_cushion <- copy(evap_cool_MGD_month)
setkey(max_consumpt_cushion, Plant_ID)
setnames(max_consumpt_cushion, 2:length(max_consumpt_cushion), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))

max_consumpt_cushion[is.na(max_consumpt_cushion)] <- 0 # Source 10

# Sources 7 & 8 begin
fun85 <- function(a) ifelse(a == 0, 0, a * (1 + cushion1))

max_consumpt_cushion <- max_consumpt_cushion[, lapply(1:12, function(i) fun85(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(max_consumpt_cushion, 2:length(max_consumpt_cushion), c("Maximum Consumption with 22 pct cushion_jan (MGD)", "Maximum Consumption with 22 pct cushion_feb (MGD)", "Maximum Consumption with 22 pct cushion_mar (MGD)", "Maximum Consumption with 22 pct cushion_apr (MGD)", "Maximum Consumption with 22 pct cushion_may (MGD)", "Maximum Consumption with 22 pct cushion_jun (MGD)", "Maximum Consumption with 22 pct cushion_jul (MGD)", "Maximum Consumption with 22 pct cushion_aug (MGD)", "Maximum Consumption with 22 pct cushion_sep (MGD)", "Maximum Consumption with 22 pct cushion_oct (MGD)", "Maximum Consumption with 22 pct cushion_nov (MGD)", "Maximum Consumption with 22 pct cushion_dec (MGD)"))


# Report block for cushioned Withdrawal minimums and maximums
# Minimum Withdrawal with 0% cushion
# Minimum withdrawal in million gallons per day for each month
cushion2 <- 0 # input

min_withdr_cushion <- copy(est_min_MGD_withdrawal)
setkey(min_withdr_cushion, Plant_ID)
setnames(min_withdr_cushion, 2:length(min_withdr_cushion), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))

min_withdr_cushion[is.na(min_withdr_cushion)] <- 0 # Source 10

# Sources 7 & 8 begin
fun86 <- function(a) ifelse(a == 0, 0, a * (1 - cushion2))

min_withdr_cushion <- min_withdr_cushion[, lapply(1:12, function(i) fun86(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(min_withdr_cushion, 2:length(min_withdr_cushion), c("Minimum Withdrawal with 0 pct cushion_jan (MGD)", "Minimum Withdrawal with 0 pct cushion_feb (MGD)", "Minimum Withdrawal with 0 pct cushion_mar (MGD)", "Minimum Withdrawal with 0 pct cushion_apr (MGD)", "Minimum Withdrawal with 0 pct cushion_may (MGD)", "Minimum Withdrawal with 0 pct cushion_jun (MGD)", "Minimum Withdrawal with 0 pct cushion_jul (MGD)", "Minimum Withdrawal with 0 pct cushion_aug (MGD)", "Minimum Withdrawal with 0 pct cushion_sep (MGD)", "Minimum Withdrawal with 0 pct cushion_oct (MGD)", "Minimum Withdrawal with 0 pct cushion_nov (MGD)", "Minimum Withdrawal with 0 pct cushion_dec (MGD)"))


# Maximum withdrawal
# Maximum withdrawal in million gallons per day for each month
max_withdr_cushion <- copy(est_max_MGD_withdrawal)
setkey(max_withdr_cushion, Plant_ID)
setnames(max_withdr_cushion, 2:length(max_withdr_cushion), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12"))

max_withdr_cushion[is.na(max_withdr_cushion)] <- 0 # Source 10

# Sources 7 & 8 begin
fun87 <- function(a) ifelse(a == 0, 0, a * (1 + cushion2))

max_withdr_cushion <- max_withdr_cushion[, lapply(1:12, function(i) fun87(get(paste0('a', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(max_withdr_cushion, 2:length(max_withdr_cushion), c("Maximum Withdrawal with 0 pct cushion_jan (MGD)", "Maximum Withdrawal with 0 pct cushion_feb (MGD)", "Maximum Withdrawal with 0 pct cushion_mar (MGD)", "Maximum Withdrawal with 0 pct cushion_apr (MGD)", "Maximum Withdrawal with 0 pct cushion_may (MGD)", "Maximum Withdrawal with 0 pct cushion_jun (MGD)", "Maximum Withdrawal with 0 pct cushion_jul (MGD)", "Maximum Withdrawal with 0 pct cushion_aug (MGD)", "Maximum Withdrawal with 0 pct cushion_sep (MGD)", "Maximum Withdrawal with 0 pct cushion_oct (MGD)", "Maximum Withdrawal with 0 pct cushion_nov (MGD)", "Maximum Withdrawal with 0 pct cushion_dec (MGD)"))


## duty-weighted forced evap
# Monthly duty totals
Total_duty_month <- copy(duty)
setkey(Total_duty_month, Plant_ID)

Total_duty_month <- Total_duty_month[, lapply(.SD, sum), .SDcols = 2:13] # Source 12

setnames(Total_duty_month, c("Monthly duty totals_jan (MGD)", "Monthly duty totals_feb (MGD)", "Monthly duty totals_mar (MGD)", "Monthly duty totals_apr (MGD)", "Monthly duty totals_may (MGD)", "Monthly duty totals_jun (MGD)", "Monthly duty totals_jul (MGD)", "Monthly duty totals_aug (MGD)", "Monthly duty totals_sep (MGD)", "Monthly duty totals_oct (MGD)", "Monthly duty totals_nov (MGD)", "Monthly duty totals_dec (MGD)"))


Total_duty_year <- sum(Total_duty_month)


Total_duty_year_ID <- copy(duty)
Total_duty_year_ID[, "Total duty for year by Plant ID" := rowSums(.SD), .SDcols = 2:13] # Source 11
Total_duty_year_ID <- Total_duty_year_ID[, c(1, 14)]

Total_duty_year_ID_check <- sum(Total_duty_year_ID[, 2])


# Evaporation energy - product of FEpercent and condenser duty
evap_energy <- data.table(duty, evap_cool_percent_add_heat)
evap_energy[, 14 := NULL]
setkey(evap_energy, Plant_ID)
setnames(evap_energy, 2:length(evap_energy), c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12"))

evap_energy[is.na(evap_energy)] <- 0 # Source 10

# Sources 7 & 8 begin
fun88 <- function(a, b) ifelse(a == 0, 0, (a * b) / 100)

evap_energy <- evap_energy[, lapply(1:12, function(i) fun88(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end

setnames(evap_energy, 2:length(evap_energy), c("Evaporation energy_jan", "Evaporation energy_feb", "Evaporation energy_mar", "Evaporation energy_apr", "Evaporation energy_may", "Evaporation energy_jun", "Evaporation energy_jul", "Evaporation energy_aug", "Evaporation energy_sep", "Evaporation energy_oct", "Evaporation energy_nov", "Evaporation energy_dec"))


# evap_energy_month_sum <- evap_energy[, lapply(.SD, sum, na.rm = TRUE), .SDcols = 2:13] # Source 13


evap_energy_year_ID <- copy(evap_energy)
evap_energy_year_ID[, "Evaporation energy for year by Plant ID" := rowSums(.SD), .SDcols = 2:13] # Source 11
evap_energy_year_ID <- evap_energy_year_ID[, c(1, 14)]


evap_energy_year <- sum(evap_energy_year_ID[, 2])


# percent evaporation
pct_evap <- data.table(evap_energy_year_ID, Total_duty_year_ID)
pct_evap[, 3 := NULL]
setkey(pct_evap, Plant_ID)
setnames(pct_evap, 2:length(pct_evap), c("a1", "b1"))

pct_evap[is.na(pct_evap)] <- 0 # Source 10

# Sources 7 & 8 begin
fun89 <- function(a, b) ifelse(a == 0, 0, (a / b) * 100)

pct_evap <- pct_evap[, lapply(1, function(i) fun89(get(paste0('a', i)), get(paste0('b', i)))), by = Plant_ID]
# Sources 7 & 8 end
setnames(pct_evap, 2:length(pct_evap), "percent evaporation")


# average percent evaporation
mean_pct_evap <- (evap_energy_year/Total_duty_year) * 100



## Output, river-only plants
# total MGD
total_MGD <- sum_mean_MGD_year_consumpt


# total consumption MG
total_consumption_MG <- total_MGD_consumpt


# total withdrawal MGD
total_withdrawal_MGD <- mean_MGD_year1_withdr2


# collect all objects to ensure that the order of Plant_ID matches what was available in the beginning
evap_cool_MGD_month <- evap_cool_MGD_month[fmatch(order_check, evap_cool_MGD_month$Plant_ID)] # Source 23

max_consumpt_cushion <- max_consumpt_cushion[fmatch(order_check, max_consumpt_cushion$Plant_ID)] # Source 23

min_consumpt_cushion <- min_consumpt_cushion[fmatch(order_check, min_consumpt_cushion$Plant_ID)] # Source 23

est_MGD_withdrawal <- est_MGD_withdrawal[fmatch(order_check, est_MGD_withdrawal$Plant_ID)] # Source 23

est_max_MGD_withdrawal <- est_max_MGD_withdrawal[fmatch(order_check, est_max_MGD_withdrawal$Plant_ID)] # Source 23

est_min_MGD_withdrawal <- est_min_MGD_withdrawal[fmatch(order_check, est_min_MGD_withdrawal$Plant_ID)] # Source 23

max_withdr_cushion <- max_withdr_cushion[fmatch(order_check, max_withdr_cushion$Plant_ID)] # Source 23

min_withdr_cushion <- min_withdr_cushion[fmatch(order_check, min_withdr_cushion$Plant_ID)] # Source 23



## output of results
if (output == "xlsx") {

## create Microsoft Excel workbook
wb <- createWorkbook() # use R package openxlsx to create the .xlsx spreadsheet
addWorksheet(wb, "Best Consumption All Plants") # adds the worksheet with the name of Best Consumption All Plants
writeDataTable(wb, "Best Consumption All Plants", evap_cool_all_plants_MG) # writes the data to the workbook
setColWidths(wb, sheet = 1, cols = 1:ncol(evap_cool_all_plants_MG), widths = "auto")
addWorksheet(wb, "Best Consumption Estimates") # adds the worksheet with the name of Best Consumption Estimates
writeDataTable(wb, "Best Consumption Estimates", evap_cool_MGD_month) # writes the data to the workbook
setColWidths(wb, sheet = 2, cols = 1:ncol(evap_cool_MGD_month), widths = "auto")
addWorksheet(wb, "Max Consumpt with 22% cushion") # adds the worksheet with the name of Max Consumpt with 22% cushion
writeDataTable(wb, "Max Consumpt with 22% cushion", max_consumpt_cushion) # writes the data to the workbook
setColWidths(wb, sheet = 3, cols = 1:ncol(max_consumpt_cushion), widths = "auto")
addWorksheet(wb, "Min Consumpt with 22% cushion") # adds the worksheet with the name of Min Consumpt with 22% cushion
writeDataTable(wb, "Min Consumpt with 22% cushion", min_consumpt_cushion) # writes the data to the workbook
setColWidths(wb, sheet = 4, cols = 1:ncol(min_consumpt_cushion), widths = "auto")
addWorksheet(wb, "Best Withdrawal Estimates") # adds the worksheet with the name of Best Withdrawal Estimates
writeDataTable(wb, "Best Withdrawal Estimates", est_MGD_withdrawal) # writes the data to the workbook
setColWidths(wb, sheet = 5, cols = 1:ncol(est_MGD_withdrawal), widths = "auto")
addWorksheet(wb, "Estimated Max MGD Withdrawal") # adds the worksheet with the name of Estimated Max MGD Withdrawal
writeDataTable(wb, "Estimated Max MGD Withdrawal", est_max_MGD_withdrawal) # writes the data to the workbook
setColWidths(wb, sheet = 6, cols = 1:ncol(est_max_MGD_withdrawal), widths = "auto")
addWorksheet(wb, "Estimated Min MGD Withdrawal") # adds the worksheet with the name of Estimated Min MGD Withdrawal
writeDataTable(wb, "Estimated Min MGD Withdrawal", est_min_MGD_withdrawal) # writes the data to the workbook
setColWidths(wb, sheet = 7, cols = 1:ncol(est_min_MGD_withdrawal), widths = "auto")
addWorksheet(wb, "Max Withdrawal with 0% cushion") # adds the worksheet with the name of Max Withdrawal with 0% cushion
writeDataTable(wb, "Max Withdrawal with 0% cushion", max_withdr_cushion) # writes the data to the workbook
setColWidths(wb, sheet = 8, cols = 1:ncol(max_withdr_cushion), widths = "auto")
addWorksheet(wb, "Min Withdrawal with 0% cushion") # adds the worksheet with the name of Min Withdrawal with 0% cushion
writeDataTable(wb, "Min Withdrawal with 0% cushion", min_withdr_cushion) # writes the data to the workbook
setColWidths(wb, sheet = 9, cols = 1:ncol(min_withdr_cushion), widths = "auto")
filesave <- paste0(fewsroutpath, output_f)
#filesave <- tclvalue(tkgetSaveFile(title = "Save file as", filetypes = "{{MS Excel file} .xlsx}")) # Sources 3 & 17 / GUI file dialog to save the spreadsheet
saveWorkbook(wb, paste0(filesave, ".xlsx"), overwrite = TRUE)


## plot of Average percent evaporation by month
mean_evap_cool_percent_add_heat_plot <- t(setDF(mean_evap_cool_percent_add_heat))
mean_evap_cool_percent_add_heat_plot <- setDT(data.frame(month.abb, mean_evap_cool_percent_add_heat_plot))
setnames(mean_evap_cool_percent_add_heat_plot, c("Month", "Mean Evaporative cooling as percent of added heat"))
mean_evap_cool_percent_add_heat_plot <- setDF(mean_evap_cool_percent_add_heat_plot)
ordmonth <- factor(mean_evap_cool_percent_add_heat_plot$Month, as.character(mean_evap_cool_percent_add_heat_plot$Month)) # Source 14
mean_evap_cool_percent_add_heat_plot <- data.frame(ordmonth, mean_evap_cool_percent_add_heat_plot$"Mean Evaporative cooling as percent of added heat")
names(mean_evap_cool_percent_add_heat_plot) <- c("Month", "Percent")

plotsave <- paste0(fewsroutpath, "images/", output_f)
#plotsave <- tclvalue(tkgetSaveFile(title = "Save image as", filetypes = "{{PNG} .png}")) # Sources 3 & 17 / GUI file dialog to save the plot
png(filename = paste0(plotsave, ".png"), width = 480, height = 400, units = "px")
p <- ggplot(mean_evap_cool_percent_add_heat_plot, aes(x = Month, y = Percent, group = 1)) + geom_point() + geom_line() + labs(x = "Month", y = "Percent", title = paste("Average percent evaporation by month,", type)) # Source 15
print(p)
dev.off()


} else if (output == "csv") {

## create csv file
csvout <- cbind(evap_cool_all_plants_MG, evap_cool_MGD_month, max_consumpt_cushion, min_consumpt_cushion, est_MGD_withdrawal, est_max_MGD_withdrawal, est_min_MGD_withdrawal, max_withdr_cushion, min_withdr_cushion)
filesave1 <- tclvalue(tkgetSaveFile(title = "Save csv file as", filetypes = "{{Comma-separated value file} .csv}")) # Sources 3 & 17 / GUI file dialog to save the spreadsheet
fwrite(csvout, file = paste0(filesave1, ".csv"), row.names = FALSE, na = "")


## plot of Average percent evaporation by month
mean_evap_cool_percent_add_heat_plot <- t(setDF(mean_evap_cool_percent_add_heat))
mean_evap_cool_percent_add_heat_plot <- setDT(data.frame(month.abb, mean_evap_cool_percent_add_heat_plot))
setnames(mean_evap_cool_percent_add_heat_plot, c("Month", "Mean Evaporative cooling as percent of added heat"))
mean_evap_cool_percent_add_heat_plot <- setDF(mean_evap_cool_percent_add_heat_plot)
ordmonth <- factor(mean_evap_cool_percent_add_heat_plot$Month, as.character(mean_evap_cool_percent_add_heat_plot$Month)) # Source 14
mean_evap_cool_percent_add_heat_plot <- data.frame(ordmonth, mean_evap_cool_percent_add_heat_plot$"Mean Evaporative cooling as percent of added heat")
names(mean_evap_cool_percent_add_heat_plot) <- c("Month", "Percent")

plotsave <- paste0(fewsroutpath, "images/", output_f)
#plotsave <- tclvalue(tkgetSaveFile(title = "Save image as", filetypes = "{{PNG} .png}")) # Sources 3 & 17 / GUI file dialog to save the plot
png(filename = paste0(plotsave, ".png"), width = 480, height = 400, units = "px")
p <- ggplot(mean_evap_cool_percent_add_heat_plot, aes(x = Month, y = Percent, group = 1)) + geom_point() + geom_line() + labs(x = "Month", y = "Percent", title = paste("Average percent evaporation by month,", type)) # Source 15
print(p)
dev.off()


} else if (output == "both") {

## create Microsoft Excel workbook
wb <- createWorkbook() # use R package openxlsx to create the .xlsx spreadsheet
addWorksheet(wb, "Best Consumption All Plants") # adds the worksheet with the name of Best Consumption All Plants
writeDataTable(wb, "Best Consumption All Plants", evap_cool_all_plants_MG) # writes the data to the workbook
setColWidths(wb, sheet = 1, cols = 1:ncol(evap_cool_all_plants_MG), widths = "auto")
addWorksheet(wb, "Best Consumption Estimates") # adds the worksheet with the name of Best Consumption Estimates
writeDataTable(wb, "Best Consumption Estimates", evap_cool_MGD_month) # writes the data to the workbook
setColWidths(wb, sheet = 2, cols = 1:ncol(evap_cool_MGD_month), widths = "auto")
addWorksheet(wb, "Max Consumpt with 22% cushion") # adds the worksheet with the name of Max Consumpt with 22% cushion
writeDataTable(wb, "Max Consumpt with 22% cushion", max_consumpt_cushion) # writes the data to the workbook
setColWidths(wb, sheet = 3, cols = 1:ncol(max_consumpt_cushion), widths = "auto")
addWorksheet(wb, "Min Consumpt with 22% cushion") # adds the worksheet with the name of Min Consumpt with 22% cushion
writeDataTable(wb, "Min Consumpt with 22% cushion", min_consumpt_cushion) # writes the data to the workbook
setColWidths(wb, sheet = 4, cols = 1:ncol(min_consumpt_cushion), widths = "auto")
addWorksheet(wb, "Best Withdrawal Estimates") # adds the worksheet with the name of Best Withdrawal Estimates
writeDataTable(wb, "Best Withdrawal Estimates", est_MGD_withdrawal) # writes the data to the workbook
setColWidths(wb, sheet = 5, cols = 1:ncol(est_MGD_withdrawal), widths = "auto")
addWorksheet(wb, "Estimated Max MGD Withdrawal") # adds the worksheet with the name of Estimated Max MGD Withdrawal
writeDataTable(wb, "Estimated Max MGD Withdrawal", est_max_MGD_withdrawal) # writes the data to the workbook
setColWidths(wb, sheet = 6, cols = 1:ncol(est_max_MGD_withdrawal), widths = "auto")
addWorksheet(wb, "Estimated Min MGD Withdrawal") # adds the worksheet with the name of Estimated Min MGD Withdrawal
writeDataTable(wb, "Estimated Min MGD Withdrawal", est_min_MGD_withdrawal) # writes the data to the workbook
setColWidths(wb, sheet = 7, cols = 1:ncol(est_min_MGD_withdrawal), widths = "auto")
addWorksheet(wb, "Max Withdrawal with 0% cushion") # adds the worksheet with the name of Max Withdrawal with 0% cushion
writeDataTable(wb, "Max Withdrawal with 0% cushion", max_withdr_cushion) # writes the data to the workbook
setColWidths(wb, sheet = 8, cols = 1:ncol(max_withdr_cushion), widths = "auto")
addWorksheet(wb, "Min Withdrawal with 0% cushion") # adds the worksheet with the name of Min Withdrawal with 0% cushion
writeDataTable(wb, "Min Withdrawal with 0% cushion", min_withdr_cushion) # writes the data to the workbook
setColWidths(wb, sheet = 9, cols = 1:ncol(min_withdr_cushion), widths = "auto")
filesave <- paste0(fewsroutpath, output_f)
#filesave <- tclvalue(tkgetSaveFile(title = "Save file as", filetypes = "{{MS Excel file} .xlsx}")) # Sources 3 & 17 / GUI file dialog to save the spreadsheet
saveWorkbook(wb, paste0(filesave, ".xlsx"), overwrite = TRUE)


## create csv file
csvout <- cbind(evap_cool_all_plants_MG, evap_cool_MGD_month, max_consumpt_cushion, min_consumpt_cushion, est_MGD_withdrawal, est_max_MGD_withdrawal, est_min_MGD_withdrawal, max_withdr_cushion, min_withdr_cushion)
filesave1 <- tclvalue(tkgetSaveFile(title = "Save csv file as", filetypes = "{{Comma-separated value file} .csv}")) # Sources 3 & 17 / GUI file dialog to save the spreadsheet
fwrite(csvout, file = paste0(filesave1, ".csv"), row.names = FALSE, na = "")


## plot of Average percent evaporation by month
mean_evap_cool_percent_add_heat_plot <- t(setDF(mean_evap_cool_percent_add_heat))
mean_evap_cool_percent_add_heat_plot <- setDT(data.frame(month.abb, mean_evap_cool_percent_add_heat_plot))
setnames(mean_evap_cool_percent_add_heat_plot, c("Month", "Mean Evaporative cooling as percent of added heat"))
mean_evap_cool_percent_add_heat_plot <- setDF(mean_evap_cool_percent_add_heat_plot)
ordmonth <- factor(mean_evap_cool_percent_add_heat_plot$Month, as.character(mean_evap_cool_percent_add_heat_plot$Month)) # Source 14
mean_evap_cool_percent_add_heat_plot <- data.frame(ordmonth, mean_evap_cool_percent_add_heat_plot$"Mean Evaporative cooling as percent of added heat")
names(mean_evap_cool_percent_add_heat_plot) <- c("Month", "Percent")

plotsave <- paste0(fewsroutpath, "images/", output_f)
#plotsave <- tclvalue(tkgetSaveFile(title = "Save image as", filetypes = "{{PNG} .png}")) # Sources 3 & 17 / GUI file dialog to save the plot
png(filename = paste0(plotsave, ".png"), width = 480, height = 400, units = "px")
p <- ggplot(mean_evap_cool_percent_add_heat_plot, aes(x = Month, y = Percent, group = 1)) + geom_point() + geom_line() + labs(x = "Month", y = "Percent", title = paste("Average percent evaporation by month,", type)) # Source 15
print(p)
dev.off()

}
}
