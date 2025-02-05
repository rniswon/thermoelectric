compareKeys_2D<-function(orig_key,auto_key,origCol,autoCols){

  #give generic name to plant_bo or plant_gen from orig_key
  names(orig_key)[names(orig_key)==origCol]<-"tempId"
  
  #set up empty data.frames to bind to
  onlyOrigBogens<-orig_key[0,]
  onlyAutoBogens<-auto_key[0,]
  matched<-auto_key[0,]
  matched$bogen_orig<-character(0)

  
  for (p in unique(c(orig_key$Plant.Code,auto_key$Plant.Code))){
    subOrig<-orig_key %>% filter(Plant.Code==p)
    subAuto<-auto_key %>% filter(Plant.Code==p)
    matchedOrigBo<-character(0)
    
    if (nrow(subOrig)==1 & nrow(subAuto)==1){
      gotMatch<-FALSE
      for (c in autoCols){
        eval(parse(text=paste0("subBo<-subAuto$",c)))
        if (subBo==subOrig$tempId){
          gotMatch<-TRUE
        }
      }
      
      if (gotMatch==TRUE){
        subOrig2<-subOrig
        bindAuto<-subAuto
        bindAuto$bogen_orig<-unique(subOrig2$bogen)
        matched<-rbind(matched,bindAuto)

      }else{
        onlyAutoBogens<-rbind(onlyAutoBogens,subAuto)
        onlyOrigBogens<-rbind(onlyOrigBogens,subOrig)
      }
      
    }else if (nrow(subOrig)>1 | nrow(subAuto)>1){#if nrow>1
      #find in Auto NOT orig
      for (b in as.character(unique(subOrig$bogen))){
        origPlantIds<-as.character(subOrig[as.character(subOrig$bogen)==b,]$tempId)
        eval(parse(text = paste0("autoBogens<-subAuto %>% filter(",
                                 paste0(autoCols," %in% origPlantIds",collapse = " | "),")")))
        
       
        
        if (length(origPlantIds)==1 & nrow(autoBogens)==1){#if only 1 row in autoBogens
          
          autoBogens<-subAuto %>% filter(bogen==as.character(autoBogens$bogen[1]))
          
          for (r in 1:nrow(autoBogens)){
            subAutoBogen<-autoBogens[r,]
          gotMatch<-FALSE
          for (c in autoCols){
            eval(parse(text=paste0("subBo<-subAutoBogen$",c)))
            if (subBo==origPlantIds){ 
              gotMatch<-TRUE
              matchedOrigBo<-c(matchedOrigBo,as.character(subBo))
            }
          }#for c in autoCols 

          if (gotMatch){
            subOrig2<-subOrig %>% filter(subOrig$bogen==b)
            bindAuto<-autoBogens
            bindAuto$bogen_orig<-unique(subOrig2$bogen)
            matched<-rbind(matched,bindAuto)

          }else{#no match found
            onlyAutoBogens<-rbind(onlyAutoBogens,subAutoBogen)
          }
          
          }#r in autoBogens rows
          
        }else if (nrow(autoBogens)==0){#no bogens with NAs removed in auto
          onlyOrigBogens<-rbind(onlyOrigBogens,subOrig[as.character(subOrig$bogen)==b,])
          eval(parse(text = paste0("autoBogens<-subAuto %>% filter(",
                                   paste0(autoCols," %in% origPlantIds",collapse = " | "),")")))
          onlyAutoBogens<-rbind(onlyAutoBogens,autoBogens) 
        }else{#multiple rows in autoBogens, don't match plantCode^NA
          eval(parse(text = paste0("autoBogens<-subAuto %>% filter(",
                                   paste0("(",autoCols," %in% origPlantIds & !endsWith(as.character(",autoCols,"),'NA'))",
                                          collapse = " | "),")")))
          
          if(length(unique(autoBogens$bogen))==1){#if only 1 autobogen found for origPlantIds
            
            autoBogens<-subAuto %>% filter(bogen==as.character(autoBogens$bogen[1]))
            
            for (r in 1:nrow(autoBogens)){
              subAutoBogen<-autoBogens[r,]
              
              gotMatch<-FALSE
              for (c in autoCols){
                eval(parse(text=paste0("subBo<-subAutoBogen$",c)))
                if (subBo %in% origPlantIds & !endsWith(as.character(subBo),"NA")){
                  gotMatch<-TRUE
                  matchedOrigBo<-c(matchedOrigBo,as.character(subBo))
                }
              }#c in autoCols
              
              if (gotMatch){
                subOrig2<-subOrig %>% filter(subOrig$bogen==b)
                bindAuto<-subAutoBogen
                bindAuto$bogen_orig<-unique(subOrig2$bogen)
                matched<-rbind(matched,bindAuto)

              }else{#no match found
                onlyAutoBogens<-rbind(onlyAutoBogens,subAutoBogen)
              }
              
            }#r in autoBogens rows
          }else if (nrow(autoBogens)==0){#no bogens with NAs removed in auto
            onlyOrigBogens<-rbind(onlyOrigBogens,subOrig[as.character(subOrig$bogen)==b,])
            eval(parse(text = paste0("autoBogens<-subAuto %>% filter(",
                                     paste0(autoCols," %in% origPlantIds",collapse = " | "),")")))
            onlyAutoBogens<-rbind(onlyAutoBogens,autoBogens) 
          }else{#extra bogens in auto
            onlyAutoBogens<-rbind(onlyAutoBogens,autoBogens %>% filter(bogen!=as.character(autoBogens$bogen[1])))
            autoBogens<-autoBogens %>% filter(bogen==as.character(autoBogens$bogen[1]))
            
            for (r in 1:nrow(autoBogens)){
              subAutoBogen<-autoBogens[r,]
              
              gotMatch<-FALSE
              for (c in autoCols){
                eval(parse(text=paste0("subBo<-subAutoBogen$",c)))
                if (subBo %in% origPlantIds & !endsWith(as.character(subBo),"NA")){
                  gotMatch<-TRUE
                  matchedOrigBo<-c(matchedOrigBo,as.character(subBo))
                }
              }#c in autoCols
              
              if (gotMatch){
                subOrig2<-subOrig %>% filter(subOrig$bogen==b)
                bindAuto<-subAutoBogen
                bindAuto$bogen_orig<-unique(subOrig2$bogen)
                matched<-rbind(matched,bindAuto)


              }else{#no match found
                onlyAutoBogens<-rbind(onlyAutoBogens,subAutoBogen)
              }
              
            }#r in autoBogens rows
            
            
          }  

          
        }#multiple rows in autoBogens, don't match plantCode^NA

        if (length(origPlantIds[!origPlantIds %in% matchedOrigBo])!=0){
          onlyOrigBogens<-rbind(onlyOrigBogens,subOrig %>% filter(!tempId %in% matchedOrigBo))
        }
        
      }#for b in unique(subOrig$bogen)
      
      #find in ORIG not AUTO
      for (b in as.character(unique(subAuto$bogen))){
        autoBogens<-subAuto %>% filter(as.character(bogen)==b)
        eval(parse(text=paste0("origBogens<-subOrig %>% filter(",
                               paste0("tempId %in% autoBogens$",autoCols,collapse = " | "),")")))
        

        
        if (nrow(autoBogens)==1 & nrow(origBogens)==1){
          origBogens2<- subOrig %>% filter(bogen==origBogens$bogen)
          if (nrow(origBogens2)==1){
            subOrig2<-origBogens2
            bindAuto<-autoBogens
            bindAuto$bogen_orig<-unique(subOrig2$bogen)
            matched<-rbind(matched,bindAuto)


          }else{
            onlyOrigBogens<-rbind(onlyOrigBogens,origBogens2 %>% filter(tempId %in% origBogens$tempId))
            onlyAutoBogens<-rbind(onlyAutoBogens,autoBogens)
          }
        }else if (nrow(origBogens)==0){
          onlyAutoBogens<-rbind(onlyAutoBogens,autoBogens)
        }else if (nrow(autoBogens)>1 | nrow(origBogens)>1){
          eval(parse(text=paste0("origBogens<-subOrig %>% filter(",
                                 paste0("(tempId %in% autoBogens$",autoCols,
                                        " & !endsWith(as.character(tempId),'NA'))",collapse = " | "),")")))
          
          if (nrow(origBogens)==0){
            onlyAutoBogens<-rbind(onlyAutoBogens,autoBogens)
          }else if (nrow(autoBogens)==1 & nrow(origBogens)==1){
            origBogens2<- subOrig %>% filter(bogen==origBogens$bogen)
            if (nrow(origBogens2)==1){
              subOrig2<-origBogens2
              bindAuto<-autoBogens
              bindAuto$bogen_orig<-unique(subOrig2$bogen)
              matched<-rbind(matched,bindAuto)

            }else{
              onlyOrigBogens<-rbind(onlyOrigBogens,origBogens2 %>% filter(tempId %in% origBogens$tempId))
              onlyAutoBogens<-rbind(onlyAutoBogens,autoBogens)
            }
          }else{#nrow(autoBogens) | nrow(origBogens !=1)
            eval(parse(text = paste0("autoPlantIds<-unique(c(",
                                     paste0("as.character(unique(autoBogens$",autoCols,"))",collapse = " , "),"))")))
            autoPlantIds<-autoPlantIds[!endsWith(as.character(autoPlantIds),"NA")]
            
            if (length(unique(origBogens$bogen))==1){
              
              origBogens<-subOrig %>% filter(bogen==as.character(origBogens$bogen[1]))
              
              for (r in 1:nrow(origBogens)){
                subOrigBogens<-origBogens[r,]
                if (!subOrigBogens$tempId %in% autoPlantIds){#no match
                  onlyOrigBogens<-rbind(onlyOrigBogens,subOrigBogens)
                }
              }#for r in row(origBogens)
              
             
              
            }else{#extra bogens in orig
              misMatchorigBogens<-origBogens %>% filter(origBogens$tempId!=origBogens$tempId[1])
              onlyOrigBogens<-rbind(onlyOrigBogens,misMatchorigBogens)
              
              origBogens<-origBogens %>% filter(origBogens$tempId==origBogens$tempId[1])
              for (r in 1:nrow(origBogens)){
                subOrigBogens<-origBogens[r,]
                if (!subOrigBogens$tempId %in% autoPlantIds){#no match
                  onlyOrigBogens<-rbind(onlyOrigBogens,subOrigBogens)
                }
              }#for r in row(origBogens)
            }#extra bogens in orig
            
          }##nrow(autoBogens) | nrow(origBogens !=1) after remove Plant.Code^NA
        }#nrow(autoBogens)>1 | nrow(origBogens)>1 before remove Plant.Code^NA
        
        
      }# b in unique(subAuto$bogen)
    }else if (nrow(subOrig)==0 & nrow(subAuto)!=0){#plant not in orig
      onlyAutoBogens<-rbind(onlyAutoBogens,subAuto)
    }else if (nrow(subAuto)==0 & nrow(subOrig)!=0){#plant not in auto
      onlyOrigBogens<-rbind(onlyOrigBogens,subOrig)
    }

  }#for each plant p
  
  #remove dups
  onlyOrigBogens<-onlyOrigBogens[!duplicated(onlyOrigBogens),]
  onlyAutoBogens<-onlyAutoBogens[!duplicated(onlyAutoBogens),]
  matched<-matched[!duplicated(matched),]
  
  matched<-anti_join(matched,onlyAutoBogens,by=autoCols[1])
  names(matched)[length(matched)]<-"bogen_orig"
  names(onlyOrigBogens)[names(onlyOrigBogens)=="tempId"]<-origCol
  
out.list<-list(onlyAutoBogens=onlyAutoBogens,onlyOrigBogens=onlyOrigBogens,matched=matched)
return(out.list)
}#end func