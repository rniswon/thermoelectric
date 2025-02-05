origCol<-"plant_bo"
autoCols<-c("plant_bo","plant_bo_bf.923","plant_bo_assoc")
direction<-"right"
orig_key<-sheet3_key
auto_key<-auto_sheet3_key

compareKeys_1D<-function(orig_key,auto_key,origCol,autoCols,direction){
  if (direction=="right"){
   misMatch<-orig_key[0,] 
   plantList<-unique(orig_key$Plant.Code)
   bogenList<-orig_key %>% select(Plant.Code,bogen)

  }else{
    misMatch<-auto_key[0,]
    plantList<-unique(auto_key$Plant.Code)
    bogenList<-orig_key %>% select(Plant.Code,bogen)

  }
  
names(orig_key)[names(orig_key)==origCol]<-"tempId"
# names(auto_key)[names(auto_key) %in% autoCols]<-sapply(names(auto_key)[names(auto_key) %in% autoCols],
#                                                        function(x) paste0("tempId",
#                                                                           which(names(auto_key)[names(auto_key) %in%
#                                                                                                   autoCols]==x)))
  
for (p in plantList){
  
  subOrig<-orig_key %>% filter(Plant.Code==p)
  subAuto<-auto_key %>% filter(Plant.Code==p)
  subBogen<-bogenList %>% filter(Plant.Code==p)
  
  if (nrow(subOrig)==1 & nrow(subAuto)==1){
    gotMatch<-FALSE
    for (c in autoCols){
      eval(parse(text=paste0("subBo<-subAuto$",c)))
      if (subBo==subOrig$tempId){
        gotMatch<-TRUE
      }
    }
    
    if (gotMatch==TRUE){
      matched<-rbind(matched,subAuto)
    }else{
      onlyAutoBogens<-rbind(onlyAutoBogens,subAuto)
      onlyOrigBogens<-rbing(onlyOrigBogens,subOrig)
    }
    
  }else{#if nrow>1
    #find in Auto NOT orig
    for (b in as.character(unique(subOrig$bogen))){
    origPlantBos<-as.character(subOrig[as.character(subOrig$bogen)==b]$tempId)
    eval(parse(text = paste0("autoBogens<-subAuto %>% filter(",
                             paste0(autoCols," %in% origPlantBos",collapse = " | "),")")))

    if (length(origPlantBos)==1 & nrow(autoBogens)==1){#if only 1 row in autoBogens
      gotMatch<-FALSE
      for (c in autoCols){
        eval(parse(text=paste0("subBo<-autoBogens$",c)))
        if (subBo==origPlantBos){ 
          gotMatch<-TRUE
        }
      }#for c in autoCols 
      
      if (gotMatch){
        matched<-rbind(matched,autoBogens)
      }else{#no match found
        onlyAutoBogen<-rbind(onlyAutoBogen,autoBogens)
      }
      
    }else{#multiple rows in autoBogen, don't match plantCode^NA
      eval(parse(text = paste0("autoBogens<-subAuto %>% filter(",
                               paste0("(",autoCols," %in% origPlantBos & !grepl('\\^NA',",autoCols,"))",
                                      collapse = " | "),")")))

if(length(unique(autoBogens$bogen))==1){#if only 1 autobogen found for origPlantBos

  matchedOrigBo<-character(0)
   for (r in 1:nrow(autoBogen)){
     subAutoBogen<-autoBogen[r,]
   
     gotMatch<-FALSE
     for (c in autoCols){
    eval(parse(text=paste0("subBo<-subAutoBogen$",c)))
    if (subBo %in% origPlantBos & !grepl("\\^NA",subBo)){
      gotMatch<-TRUE
      matchedOrigBo<-c(matchedOrigBo,subBo)
    }
     }#c in autoCols
     
     if (gotMatch){
       matched<-rbind(matched,subAutoBogen)
     }else{#no match found
       onlyAutoBogen<-rbind(onlyAutoBogen,subAutoBogen)
     }
     
     }#r in autoBogen rows

  
}else{#extra bogens in auto
  onlyAutoBogen<-rbind(onlyAutoBogen,autoBogen %>% filter(bogen!=as.character(autoBogen$bogen[1])))
  autoBogen<-autoBogen %>% filter(bogen==as.character(autoBogen$bogen[1]))
  
  matchedOrigBo<-character(0)
  for (r in 1:nrow(autoBogen)){
    subAutoBogen<-autoBogen[r,]
    
    gotMatch<-FALSE
    for (c in autoCols){
      eval(parse(text=paste0("subBo<-subAutoBogen$",c)))
      if (subBo %in% origPlantBos & !grepl("\\^NA",subBo)){
        gotMatch<-TRUE
        matchedOrigBo<-c(matchedOrigBo,subBo)
      }
    }#c in autoCols
    
    if (gotMatch){
      matched<-rbind(matched,subAutoBogen)
    }else{#no match found
      onlyAutoBogen<-rbind(onlyAutoBogen,subAutoBogen)
    }
    
  }#r in autoBogen rows
 
                                  
  
}#extra bogens in auto

  

}#multiple rows in autoBogen, don't match plantCode^NA

if (length(origPlantBos[!origPlantBos %in% matchedOrigBo])!=0){
  onlyOrigBogens<-rbind(onlyOrigBogens,subOrig %>% filter(!tempId %in% matchedOrigBo))
}  
    
}#for b in unique(subOrig$bogen)
    
    #find in ORIG not AUTO
    for (b in as.character(unique(subAuto$bogen))){
      autoBogens<-subOrig %>% filter(as.character(bogen)==b)
      
      if (nrow(autoBogens)==1){
        eval(parse(text=paste0("origBogens<-subOrig %>% filter(",
                               paste0("tempId %in% autoBogens$",autoCols,collapse = " | "),")")))
        if(nrow(origBogens)==1){
          matched<-rbind(matched,autoBogens)
        }else if (nrow(origBogens)>1){
          eval(parse(text=paste0("origBogens<-subOrig %>% filter(",
                                 paste0("(tempId %in% autoBogens$",autoCols,"& grepl('\\^NA',",autoCols,"))",collapse = " | "),")")))
          if(nrow(origBogens)==1){
            matched<-rbind(matched,autoBogens)
          }else if (nrow(origBogens)==0){
            onlyAutoBogen<-rbind(onlyAutoBogen,autoBogens)
          }
        }else if (nrow(origBogens)==0){
          onlyAutoBogen<-rbind(onlyAutoBogen,autoBogens)
        }
      }else{#multiple rows in autoBogen
        eval(parse(text=paste0("autoPlantBos<-as.character(unique(",
                               paste0("as.character(autoBogens$",c,")",collapse = ","),"))")))
        autoPlantBos<-autoPlantBos %>% filter(!grepl("\\^NA",autoPlantBos))

          
          
          
      }
      

    }