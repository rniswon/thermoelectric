#'@title getHere
#'@description function to initiate a .here location for the thermoelectric-water-consumption-model
#'             scripts if no thermoelectric project is open.  If another Rproject is open, and error
#'             message will generated telling the user to switch or close the open project \cr \cr
#'Executed By: ThermoElectricWaterConsumptionModels.R \cr
#'@examples
#'getHere()

getHere<-function(){
  #check if any projects are open
  proj<-list.files(here(),pattern = ".Rproj",full.names = TRUE)
  
  #if a project is open tell user to close it
  if (length(proj)!=0){
    useProj  <-  readline(prompt = cat("Is this the thermoelectric-water-consumption-model project? (Y/N):\n", proj))
    permitted_input  <-  c("Y", "y", "N", "n")
    
    #Test if user input is valid
    try(if(!useProj %in% permitted_input)stop("Error invalid entry."))
    
    #tell user to close the open project
    if (tolower(useProj)=="n"){
      stop("You must close the current project before running the thermoelectric-water-consumption-model")
    }
    
  }else{#no project is open
    #refresh here() with current working directory
    detach("package:condenserDuty",unload = TRUE)
    detach("package:here", unload=TRUE)
    library(here)
    devtools::load_all("./condenserDuty/",recompile = FALSE)
  }
  
  
}