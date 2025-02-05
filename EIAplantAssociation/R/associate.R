#'@title associate
#'@description Generates concatenated boiler-generator (bogen) and 
#'boiler-generator-cooling (bogencoo) code applying the 
#'`igraph::graph_from_edgelist()` methods.  Bogen string used to calculate 
#'condenser duty at the boiler-generator level using the `condenserDuty` pacakge.
#'@param data data.frame containing EIA 'Plant.Code', 'Generator.ID' and 'Boiler.ID'
#'@param associationType character string 'bogen' indicating boiler-generator associations 
#'                       or 'bogencoo' indicating boiler-generator-cooling associations.
#'@return data.frame with 'Plant.Code', 'Generator.ID', 'Boiler.ID' and 'Bogen'.  
#''Bogen' is a concatenated string in the form of 'Plant.Code^boiler-generator-unit-number'
#'@examples
#'#set arguments
#'dest<-tempdir()
#'eia_year<-2015
#'path_InputData_Metafile<-paste0(dest,.Platform$file.sep,"UserControlCrosswalk2015.xlsx")
#'path_EIAInputData<-paste0(dest,.Platform$file.sep,eia_year)
#'outputCSV<-FALSE
#'path_outputCSV<-dest
#'
#'#use crosswalk2015 object
#'useStandardCrosswalk<-TRUE
#'prepCondenserDuty<-TRUE
#'
#'#pull data from web
#'importThermoEIA::eia_webpull(eia_year,dest)
#'
#'
#'#run import using crosswalk2015
#'inputData.list<-importThermoEIA::import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,
#'                               path_outputCSV,prepCondenserDuty = TRUE,eia_year=eia_year,
#'                               useStandardCrosswalk=useStandardCrosswalk)
#'                               
#'associate(inputData.list$bogen,associationType="bogen")
#'@export


associate <- function(data,associationType){

  if (associationType=="bogen"){
  # test: Check that data provided by user is correct
  if(!is.data.frame(data) || any(!c('Plant.Code', 'Generator.ID','Boiler.ID') %in% names(data))) {
    stop("data must be a data.frame with columns 'Plant.Code', 'Generator.ID', and 'Boiler.ID' for this function to continue")
  }
  }
  
  
  # extract vector of unique plant IDs
  plants  <-  unique(data$Plant.Code)
  
  # preallocated dataframe for output
  d_out  <-  data.frame()
  
  if (associationType=="bogen"){
  # for-loop that does the work
  for(i in 1:length(plants)){
    
    # grab plant[i]
    plant_i  <-  plants[i]

    # subset data by plant_i
    dsub  <-  dplyr::filter(data, Plant.Code==plant_i)
    
    # prepare 'edges' to pass to graph function
    edges  <-  cbind(dsub$Boiler.ID,dsub$Generator.ID)
    
    # build graph from edges
    g  <-  igraph::graph_from_edgelist(edges)
    
    # extract 'groups' from graph
    groups  <-  igraph::clusters(g)$membership
    
    # put everything together for plant_i
    d  <-  data.frame(Boiler.ID=names(groups), 
                      Bogen=paste0(plant_i,"^",groups), 
                      Plant.Code=plant_i,
                      row.names=NULL,
                      stringsAsFactors = F)
    
    # interatively append rows
    d_out  <-  rbind(d_out,d)
    
    
  } 
  }else{#bogencoo association
    # extract vector of unique plant IDs
    plants <- unique(data$Plant.Code)
    
    # preallocated dataframe for output
    d_out <- data.frame()
    
    # for-loop that does the work
    for(i in 1:length(plants)){
      
      # grab plant[i]
      plant_i <- plants[i]
      
      # subset data by plant_i
      dsub <- dplyr::filter(data, Plant.Code==plant_i)
      
      # prepare 'edges' to pass to graph function
      edges <- cbind(dsub$Boiler.ID,dsub$Cooling.ID)
      
      # build graph from edges
      g <- igraph::graph_from_edgelist(edges)
      
      # extract 'groups' from graph
      groups <- igraph::clusters(g)$membership
      
      
      if (!is.na(plant_i)){
        # put everything together for plant_i
        d <- data.frame(Cooling.ID=names(groups), 
                        result=paste0("combogencoo^",plant_i,"^",groups), 
                        Plant.Code=plant_i,
                        row.names=NULL,
                        stringsAsFactors = F)
        
        # interatively append rows
        d_out <- rbind(d_out,d)
        
        
        
      } 
    }
  }
  # return output data
  return(d_out)
}
