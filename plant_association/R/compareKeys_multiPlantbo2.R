compareKeys_multiPlantbo2<-function(key1,key2,plant_bo_col1,plant_bo_col2){
  onlyKey1Bogens<-key1[0,]
  onlyKey2Bogens<-key2[0,]
  
  if (plant_bo_col1!="plant_bo"){
    key1$temp_plant_bo<-key1$plant_bo
  }
  if (plant_bo_col2!="plant_bo"){
    key2$temp_plant_bo<-key2$plant_bo
  }
  
  key1$plant_bo<-eval(parse(text = paste0("key1$",plant_bo_col1)))
  key2$plant_bo<-eval(parse(text = paste0("key2$",plant_bo_col2)))
  
  
  for (p in unique(key2$Plant.Code)){
    

    subKey1<-key1[key1$Plant.Code==p,]
    subKey2<-key2[key2$Plant.Code==p,]

    for (b in as.character(unique(subKey2$bogen))){
      sameBogenplant_bo<-as.character(subKey2[subKey2$bogen==b,]$plant_bo)
      #print(sameBogenplant_bo)
      if (length(sameBogenplant_bo)>1){
        
        for (s in 2:length(sameBogenplant_bo)){
          #print(subKey1[subKey1$plant_bo==sameBogenplant_bo[s],])
          #print(length(subKey1[subKey1$plant_bo==sameBogenplant_bo[s],]$bogen))
          if (length(subKey1[subKey1$plant_bo==sameBogenplant_bo[s],]$bogen)==0){#plant_bo not in auto
            onlyKey2Bogens<-rbind(onlyKey2Bogens,subKey2[subKey2$plant_bo==sameBogenplant_bo[s],])
          }else{
            #print(subKey1[subKey1$plant_bo==sameBogenplant_bo[1],]$bogen)
            if (length(subKey1[subKey1$plant_bo==sameBogenplant_bo[1],]$bogen)==0){
              onlyKey2Bogens<-rbind(onlyKey2Bogens,subKey2[subKey2$plant_bo==sameBogenplant_bo[1],])
            }else{
              if (subKey1[subKey1$plant_bo==sameBogenplant_bo[s],]$bogen==
                  subKey1[subKey1$plant_bo==sameBogenplant_bo[1],]$bogen){#good bogen

              }else{#badBogen
                onlyKey1Bogens<-rbind(onlyKey1Bogens,subKey1[subKey1$plant_bo==sameBogenplant_bo[s],])
              }#badBogen
            }#plant_bo[s] not in auto
          }#first plant_bo  in auto
        }#for s
        if (length(subKey1[subKey1$plant_bo==sameBogenplant_bo[1],]$bogen)==0){
          #print("line68")
          onlyKey2Bogens<-rbind(onlyKey2Bogens,subKey2[subKey2$plant_bo==sameBogenplant_bo[1],])
        }

      }else{#length plant_bo==1
        if (length(subKey1[subKey1$plant_bo==as.character(sameBogenplant_bo[1]),]$bogen)==0){
          #print("line85")
          onlyKey2Bogens<-rbind(onlyKey2Bogens,subKey2[subKey2$plant_bo==sameBogenplant_bo[1],])
        }

      }
    }
  }#for p
  
  if (plant_bo_col1!="plant_bo"){
    eval(parse(text=paste0("onlyKey1Bogens$",plant_bo_col1,"<-onlyKey1Bogens$plant_bo")))
    onlyKey1Bogens$plant_bo<-onlyKey1Bogens$temp_plant_bo
    onlyKey1Bogens<-onlyKey1Bogens[,names(onlyKey1Bogens)!="temp_plant_bo"]
  }
  if (plant_bo_col2!="plant_bo"){
    eval(parse(text=paste0("onlyKey2Bogens$",plant_bo_col2,"<-onlyKey2Bogens$plant_bo")))
    onlyKey2Bogens$plant_bo<-onlyKey2Bogens$temp_plant_bo
    onlyKey2Bogens<-onlyKey2Bogens[,names(onlyKey2Bogens)!="temp_plant_bo"]
  }
  
  
  out.list<-list(onlyKey1Bogens=onlyKey1Bogens,onlyKey2Bogens=onlyKey2Bogens,matched=matched)
  return(out.list)
}

