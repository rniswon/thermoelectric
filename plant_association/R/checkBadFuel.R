checkBadFuel<-function(data.out,analysisYear){
  badFuels<-c("BIT","AB","ANT","GEO","LIG","MSB","MSN","OBS","PC","PUR","RC",
              "SGC","SGP","SLW","SUB","TDF","WAT","WC","WDS","WH","WND")
  badPlants<-numeric(0)
  

  if (analysisYear>2005){
  data.out<-data.out %>% filter(Reported.Prime.Mover_860 %in% c("CS","CT","CS","NGCC") |
                                  Reported.Prime.Mover_923 %in% c("CS","CT","CS","NGCC") | 
                                  Reported.Prime.Mover_bf.923 %in% c("CS","CT","CS","NGCC") | 
                                  Reported.Prime.Mover_page1 %in% c("CS","CT","CS","NGCC"))
  }else{
    data.out<-data.out %>% filter(Reported.Prime.Mover_860 %in% c("CS","CT","CS","NGCC") |
                                    Reported.Prime.Mover_page1 %in% c("CS","CT","CS","NGCC"))
  }
  
  for (p in unique(data.out$Plant.Code)){
    subdata<-data.out %>% filter(Plant.Code==p)
    if (any(subdata$Reported.Fuel.Type.Code %in% badFuels)){
      subdata<-subdata %>% filter(Reported.Fuel.Type.Code %in% badFuels)
      
      if (nrow(subdata)!=0){
        subdata<-ungroup(subdata)
        subdata<-subdata %>% select(contains("Fuel.Heat"))
        if (any(subdata>0,na.rm=T)){
          badPlants<-c(badPlants,p)
        }
      }
      
      badPlants<-unique(badPlants)

  }#end if badFuels
}#end plant
  return(badPlants)
}#end func