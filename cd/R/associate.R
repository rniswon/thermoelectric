associate <- function(data,save_image=FALSE){
  
  # test: Check that data provided by user is correct
  if(!is.data.frame(data) || any(!c('Plant.Code', 'Generator.ID','Boiler.ID') %in% names(data))) {
    stop("data must be a data.frame with columns 'Plant.Code', 'Generator.ID', and 'Boiler.ID' for this function to continue")
  }
  
  # extract vector of unique plant IDs
  plants  <-  unique(data$Plant.Code)
  
  # preallocated dataframe for output
  d_out  <-  data.frame()
  
  # for-loop that does the work
  for(i in 1:length(plants)){
    
    # grab plant[i]
    plant_i  <-  plants[i]
    
    # subset data by plant_i
    dsub  <-  dplyr::filter(data, Plant.Code==plant_i)
    
    # prepare 'edges' to pass to graph function
    edges  <-  cbind(dsub$Boiler.ID,dsub$Generator.ID)
    
    # build graph from edges
    g  <-  graph_from_edgelist(edges)
    
    # extract 'groups' from graph
    groups  <-  clusters(g)$membership
    
    # put everything together for plant_i
    d  <-  data.frame(Boiler.ID=names(groups), 
                      Bogen=paste0(plant_i,"^",groups), 
                      Plant.Code=plant_i,
                      row.names=NULL,
                      stringsAsFactors = F)
    
    # interatively append rows
    d_out  <-  rbind(d_out,d)
    
    # if save_image==TRUE, then save images
    if(save_image) { 
      
      png(file = paste("CondenserDutyModel\\Output\\","plant",plant_i, '.png', sep = ''))
      
      plot(g, vertex.size=5,
           vertex.label.dist=0.5,
           vertex.color="red",
           edge.arrow.size=0.7,
           main=paste0("plant ", plant_i))
      dev.off()
      
    }
    
  } 
  
  # return output data
  #return(d_out)
  return(d_out)
}
