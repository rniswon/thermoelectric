#' Calculate condenser duty at thermoelectric plants
#' 
#' \code{condenserDuty} includes methods to calculate condenser duty (also known as heat 
#' load) using data reported to the EIA on thermoelectric plants data found here 
#' (https://www.eia.gov/electricity/).  Data used by the condenserDuty package comes from 
#' the following EIA forms: Form-923, Form-860, Form-767, Form-906920, and Form-759. 
#' Boiler-generator  and boiler-generator-cooling associations are critical to calculating 
#' condenser duty at thermoelectric plants since the data required to calculate fuel heat 
#' is reported by boiler and net generation is reported by generator, in order to calculate 
#' condenser duty The output of the EIAplantAssociation package is formatted to be used by 
#' the condenserDuty package. The condenserDuty package expects the input data used 
#' for calculations to be formatted according to the importThermoEIA package.
#' 
#'
#' 
#' \tabular{ll}{
#' Package: \tab condenserDuty \cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' https://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' Disclaimer: \tab This software is preliminary or provisional and is subject to revision. It is being 
#' provided to meet the need for timely best science. The software has not received final approval by the 
#' U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. 
#' Government as to the functionality of the software and related material nor shall the fact of release 
#' constitute any such warranty. The software is provided on the condition that neither the USGS nor the 
#' U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use 
#' of the software.\cr
#'
#' }
#'
#' \tabular{ll}{
#'
#'\code{\link{runCondenserDuty}} \tab calculates condenser duty at nuclear and non-nuclear plants 
#'modeled for water use estimation. Condenser duty calculations are based on  net power generation 
#'and fuel consumption at power plants, as well as associations for boiler, generators,and 
#'             cooling systems. \cr \cr
#'}
#'
#' @name condenserDuty-package
#' @docType package
#' @keywords Thermoelectric
#' @import importThermoEIA
#' @import EIAplantAssociation
#' @import methods  
#' @importFrom plyr rbind.fill
#' @import dplyr
#' @import purrr
#' @import qdapTools
#' @import sjmisc
#' @import reshape2
#' @import RColorBrewer
#' @import ggplot2
#' @import knitr
#' @import testthat
#' @import openxlsx
#' @import readxl
#' @import remotes
#' @import tidyr
#' @import stringr
#' @import lubridate
#' @import Metrics
#' @import hydroGOF
#' @import purrr

NULL