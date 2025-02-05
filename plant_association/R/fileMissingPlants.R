fileMissingPlants<-function(plantList,bogen,generator.data,generation.data,boilerFuelData,
                            boilerDesignData,retiredGenerators){
  #flag Plant not in input table
  missingPlants<-data.frame(Plant.Code=unique(plantList),
                            missingPlant.assoc = rep(FALSE,length(unique(plantList))),
                            missingPlant.generator.data.860 = rep(FALSE,length(unique(plantList))),
                            missingPlant.boilerDesignData.860 = rep(FALSE,length(unique(plantList))),
                            missingPlant.generation.data.923 = rep(FALSE,length(unique(plantList))),
                            missingPlant.boilerFuelData.923 = rep(FALSE,length(unique(plantList))))
  for (p in plantList){
    subData<-bogen %>% filter(Plant.Code==p)
    if (nrow(subData)==0){
      missingPlants[missingPlants$Plant.Code==p,]$missingPlant.assoc<-TRUE
    }
    
    subData<-generator.data %>% filter(Plant.Code==p)
    subData2<-retiredGenerators %>% filter(Plant.Code==p)
    if (nrow(subData)==0 & nrow(subData2)==0){
      missingPlants[missingPlants$Plant.Code==p,]$missingPlant.generator.data.860<-TRUE
    }
    
    subData<-boilerDesignData %>% filter(Plant.Code==p)
    if (nrow(subData)==0){
      missingPlants[missingPlants$Plant.Code==p,]$missingPlant.boilerDesignData.860<-TRUE
    }
    
    subData<-generation.data %>% filter(Plant.Code==p)
    if (nrow(subData)==0){
      missingPlants[missingPlants$Plant.Code==p,]$missingPlant.generation.data.923<-TRUE
    }
    
    subData<-boilerFuelData %>% filter(Plant.Code==p)
    if (nrow(subData)==0){
      missingPlants[missingPlants$Plant.Code==p,]$missingPlant.boilerFuelData.923<-TRUE
    }
  }
  
  return(missingPlants)
}