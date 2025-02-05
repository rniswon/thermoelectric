#'@title check_dependencies
#'@description checks if required packages are installed, and askes user if they want to 
#'             install and installs if Y \cr \cr
#'Executed By: ThermoElectricWaterConsumptionModels.R \cr
#'@examples
#'check_dependencies()

check_dependencies <- function(){
  #List of required packages
  req_packages  <-  c("drake", 
                      "devtools", 
                      "here", 
                      "dplyr", 
                      "purrr", 
                      "qdapTools", 
                      "igraph", 
                      "sjmisc", 
                      "reshape2",
                      "RColorBrewer",
                      "ggmap",
                      "ggplot2", 
                      "maps",
                      "knitr")
  
  #Check for required packages
  new_packages  <-  req_packages[!(req_packages %in% installed.packages()[,"Package"])]
  print(new_packages)
  #Test if user has required packages, stop execution and display error and missing packages
  missing_packages  <-  try(length(new_packages), stop(paste("The following packages are required:", new_packages, sep=" ")))
  
  if(missing_packages !=0){
    #Create variable for user input 
    to_install  <-  "a"
    
    while(to_install != "Y" | to_install != "y" | to_install != "N" | to_install != "n")
    {
      to_install  <-  readline(prompt = "Install packages? (Y/N):")
      
      permitted_input  <-  c("Y", "y", "N", "n")
      
      #Test if user input is valid
      try(if(!to_install %in% permitted_input) stop("Error invalid entry."))
      
      if(to_install == "Y" | to_install == "y")
      {
        install.packages(new_packages, dependencies = TRUE)
        break
      } else if(to_install == "N" | to_install == "n")
      {
        stop("Packages not installed, operation terminated")
        break
      }
    }
  } 

}
