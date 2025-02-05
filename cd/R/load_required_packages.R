#'@title load_required_packages
#'@description loads required packages via library() calls \cr \cr
#'Executed By: ThermoElectricWaterConsumptionModels.R \cr
#'@examples
#'load_required_packages()

load_required_packages <- function(){

  library(here)
  library(dplyr)
  library(purrr)
  library(qdapTools)
  library(igraph)
  library(sjmisc)
  library(reshape2)
  library(RColorBrewer)
  library(ggmap)
  library(ggplot2)
  library(knitr)
  library(testthat)
  library(drake)
  library(devtools)
  
  #Test that all packages have been loaded
  packages_to_load  <-  c('devtools','drake', 'here', 'dplyr','purrr','qdaptools','igraph','sjmisc','reshape2','rcolorbrewer','ggmap','ggplot2','knitr','testthat')
  loaded_packages  <-  packages_to_load %in% tolower((.packages()))
  packages_not_loaded  <-  loaded_packages[which(loaded_packages)==FALSE]
  try(if(length(packages_not_loaded)) stop(paste("There was a problem loading the following packages:",packages_not_loaded,sep=" ")))

}