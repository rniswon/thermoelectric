
bocoo.associate <- function(inputData.list,bogen.out.list,inputData_path,save_image=FALSE){
  
#bocoo.associate <- function(data,bogen.key,gen_fuel_data,boilerFuelData,cooling,inputData_path,save_image=FALSE){
  
  
  #unpack input data
  #unpack input data
  unPackList(lists = list(inputData.list = inputData.list,
                          bogen.out.list = bogen.out.list),
             parentObj = list(NA, NA))
  data<-bocoo
  
  names(sheet3_key)[names(sheet3_key)=="bogen"]<-"Bogen"
  # test: Check that data provided by user is correct
 # if(!is.data.frame(data) || any(names(data) != c('Plant.Code', 'Boiler.ID','Cooling.ID'))) {
  if(!is.data.frame(data) || 
     length(names(data)[which(names(data) %in% c('Plant.Code', 'Boiler.ID','Cooling.ID'))])<3) {
    stop("data must be a data.frame with columns 'Plant.Code', 'Boiler.ID', and 'Cooling.ID' for this function to continue")
  }
  
  data$flagBoiler.ID_bocoo<-sapply(data$Boiler.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                        ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  data$orig_Boiler.ID_bocoo<-data$Boiler.ID
  
  data$Boiler.ID <- gsub("(?<![0-9])", "",data$Boiler.ID , perl = TRUE)
  data$Boiler.ID <- gsub("[[:space:]]", "",data$Boiler.ID ) 
  
  data<-removeLeadZero(data,"Boiler.ID","flagBoiler.ID_bocoo")
  
  #format boiler.IDs for boilerFuelData
  boilerFuelData$flagBoiler.ID_bocoo<-sapply(boilerFuelData$Boiler.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                      ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  boilerFuelData$orig_Boiler.ID_bocoo<-boilerFuelData$Boiler.ID
  
  boilerFuelData$Boiler.ID <- gsub("(?<![0-9])", "",boilerFuelData$Boiler.ID , perl = TRUE)
  boilerFuelData$Boiler.ID <- gsub("[[:space:]]", "",boilerFuelData$Boiler.ID ) 
  
  boilerFuelData<-removeLeadZero(boilerFuelData,"Boiler.ID","flagBoiler.ID_bocoo")
  boilerFuelData<-boilerFuelData %>% filter(!is.na(Boiler.ID))
  
  #find NGCC bogens in boilerFuelData
  NGCCplants<-sheet3_key %>% filter(regexpr("NGCC",Bogen)>0)
  NGCCplants<-unique(NGCCplants$Plant.Code)
  boilerFuelData<-boilerFuelData %>% filter(Plant.Code %in% NGCCplants & !Reported.Prime.Mover %in% c("CT","ST"))
  
  #join with bocoo associate
  boilerFuelData<-boilerFuelData %>% select(Plant.Code,Boiler.ID)
  boilerFuelData<-inner_join(boilerFuelData,data,by=c("Plant.Code","Boiler.ID"))
  boilerFuelData<-boilerFuelData %>% select(Plant.Code,Boiler.ID,Cooling.ID)
  boilerFuelData<-boilerFuelData %>% group_by(Plant.Code) %>% summarise(countCooling = length(unique(Cooling.ID)),
                                                                         oneBoiler = first(Boiler.ID))
  boilerFuelData<-boilerFuelData %>% filter(countCooling==1)
  names(boilerFuelData)[3]<-"Boiler.ID"
  
  #replace empty boiler.ID in sheet3_key
  NGCCbogen<-sheet3_key %>% filter(regexpr("NGCC",Bogen)>0)
  sheet3_key<-sheet3_key %>% filter(regexpr("NGCC",Bogen)<0)
  NGCCbogen<-left_join(NGCCbogen %>% select(-Boiler.ID),boilerFuelData %>% select(Plant.Code,Boiler.ID),
                       by=c("Plant.Code"="Plant.Code"))
  NGCCbogen<-NGCCbogen %>% select(names(sheet3_key))
  sheet3_key<-rbind(sheet3_key,NGCCbogen)
  
  
  sheet3_key<-sheet3_key[order(sheet3_key$Bogen),]
  sortdata<-data[0,]
for (p in unique(data$Plant.Code)){
  subdata<-data %>% filter(Plant.Code==p)
  subBogen<-sheet3_key %>% filter(Plant.Code==p)
  subdata<-subdata[match(unique(subBogen$Boiler.ID),subdata$Boiler.ID),]
  sortdata<-rbind(sortdata,subdata)
}
  data<-sortdata
  

  
  assign("data",data,envir = .GlobalEnv)
  
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
    g <- graph_from_edgelist(edges)
    
    # extract 'groups' from graph
    groups <- clusters(g)$membership
    
    
    if (!is.na(plant_i)){
    # put everything together for plant_i
    d <- data.frame(Cooling.ID=names(groups), 
                    result=paste0("combogencoo^",plant_i,"^",groups), 
                    Plant.Code=plant_i,
                    row.names=NULL,
                    stringsAsFactors = F)
    
    # interatively append rows
    d_out <- rbind(d_out,d)
    
    
    # if save_image==TRUE, then save images
    if(save_image) { 
      
      jpeg(file = paste("figures/","plant",plant_i, '.jpeg', sep = ''))
      
      plot(g, vertex.size=5,
           vertex.label.dist=0.5,
           vertex.color="red",
           edge.arrow.size=0.7,
           main=paste0("plant ", plant_i))
      dev.off()
      
    }
    
  } 
  }
  
  bocoo.key <- left_join(data,d_out,by=c("Plant.Code","Cooling.ID"))
  bocoo.key$combogencoo<-bocoo.key$result
  bocoo.key<-bocoo.key[,-(which(names(bocoo.key)=="result"))]
  
  #join with sheet3_key
  bogencoo.key<-left_join(sheet3_key,bocoo.key)
  
  #add combogen
  bogencoo.key$combogen<-paste0("combogen^",bogencoo.key$Bogen)
  
  
  #fill missing if only 1 bogen
  oneBogenPlants<-ungroup(bogencoo.key) %>% select(Plant.Code,Bogen,combogencoo) %>% group_by(Plant.Code) %>% mutate(countBogens = length(unique(Bogen)))
  oneBogenPlants<-oneBogenPlants %>% filter(countBogens==1)
  oneBogenPlants<-oneBogenPlants[!duplicated(oneBogenPlants),]
  oneBogenPlants<-oneBogenPlants %>% filter(is.na(combogencoo))
  oneBogenPlants$combogencoo<-paste0("combogencoo^",oneBogenPlants$Plant.Code,"^1")


  bogencoo.key<-left_join(bogencoo.key,oneBogenPlants %>% select(Plant.Code,combogencoo),by="Plant.Code")
  bogencoo.key$combogencoo<-ifelse(is.na(bogencoo.key$combogencoo.x) & !is.na(bogencoo.key$combogencoo.y),bogencoo.key$combogencoo.y,bogencoo.key$combogencoo.x)
  bogencoo.key<-bogencoo.key %>% select(names(bogencoo.key)[!names(bogencoo.key) %in% c("combogencoo.x","combogencoo.y")])
  bogencoo.key<-bogencoo.key[!duplicated(bogencoo.key),]
  
  bogencoo.key$manualEdit<-rep(0,nrow(bogencoo.key))
  
  bogencoo.key<-bogencoo.key %>% select(manualEdit,manualBogenEdit,Plant.Code, Bogen, combogen,combogencoo,
                                        names(bogencoo.key)[!names(bogencoo.key) %in% 
                                                              c("manualEdit",
                                                                "manualBogenEdit",
                                                                "Plant.Code",
                                                                "Bogen",
                                                                "combogen",
                                                                "combogencoo",
                                                                "manualBogenEditFileName",
                                                                "manualBogenEditDateTime")],
                                        manualBogenEditFileName,manualBogenEditDateTime)
  
  #fill missing bogencoos if same bogen
  for (b in unique(bogencoo.key$combogen)){
    uniqueCombogenCoo<-unique(na.omit(bogencoo.key[bogencoo.key$combogen==b,]$combogencoo))
    if(length(uniqueCombogenCoo)==1){
      bogencoo.key[bogencoo.key$combogen==b,]$combogencoo<-rep(uniqueCombogenCoo,nrow(bogencoo.key[bogencoo.key$combogen==b,]))
    }
  }
  bogencoo.key<-bogencoo.key[order(bogencoo.key$Plant.Code),]
  
  
  #if all missing EIA data, use 2015 key IF bogens match
  combogencoo.key2015<-read.csv(paste0(inputData_path,'/combogencoo_key.csv'), header=T, stringsAsFactors = F)
  names(combogencoo.key2015)<-c("combogen2015","combogencoo2015")
  combogencoo.key2015$Plant.Code<-sapply(combogencoo.key2015$combogencoo2015, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][2]))
  
  missingBogencoo<-bogencoo.key %>% filter(is.na(combogencoo))
  okReplace<-numeric(0)
  for (p in unique(missingBogencoo$Plant.Code)){
    subMissing<-missingBogencoo %>% filter(Plant.Code==p)
    sub2015<-combogencoo.key2015 %>% filter(Plant.Code==p)
    
    if (nrow(subMissing)!=0 & nrow(sub2015)!=0){
    if (all(unique(subMissing$combogen) %in% unique(sub2015$combogen2015)) &
        all(unique(sub2015$combogen) %in% unique(subMissing$combogen2015))){
      okReplace<-c(okReplace,p)
    }
    }
    
  }
  bogencoo.key<-left_join(bogencoo.key,combogencoo.key2015,by=c("Plant.Code"="Plant.Code","combogen"="combogen2015"))
  bogencoo.key$okReplace<-ifelse(bogencoo.key$Plant.Code %in% okReplace,1,0)
  bogencoo.key$combogencoo<-ifelse(bogencoo.key$okReplace==1 & is.na(bogencoo.key$combogencoo) & 
                                     !is.na(bogencoo.key$combogencoo2015),
                                   bogencoo.key$combogencoo2015,bogencoo.key$combogencoo)
  bogencoo.key<-bogencoo.key %>% select(-combogencoo2015,-okReplace)
  
  
  ###read in combogencoo_cool_type_key and compare
  combogencoo_cooly_type <- read.csv(paste0(inputData_path,'/combogencoo_cool_type.csv'), header=T, stringsAsFactors = F)
  manualBogencoo<-anti_join(bogencoo.key,combogencoo_cooly_type,by=c("combogencoo"="bogencoo"))
  manualBogencoo<-manualBogencoo[!duplicated(manualBogencoo),]
  manualBogencoo<-manualBogencoo %>% select(Plant.Code)
  
  manualBogencoo2<-anti_join(combogencoo_cooly_type,bogencoo.key,by=c("bogencoo"="combogencoo"))
  manualBogencoo2$Plant.Code<-sapply(manualBogencoo2$bogencoo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][2]))
  manualBogencoo2<-manualBogencoo2 %>% filter(Plant.Code %in% sheet3_key$Plant.Code)
  manualBogencoo2<-manualBogencoo2[!duplicated(manualBogencoo2),]
  
 
  #don't flag mismatches from combogencoo_cooly_type to bogencoo.key IF only 1 cooling sys
  if (eia_year<2012){
    cooling$Cooling.Type.3<-rep(NA,nrow(cooling))
    cooling$Cooling.Type.4<-rep(NA,nrow(cooling))
  }
  cooling<-cooling %>% select(Plant.Code,Cooling.ID,Cooling.Type.1,Cooling.Type.2,
                              Cooling.Type.3, Cooling.Type.4)
  cooling<-cooling %>% group_by(Plant.Code) %>% summarise(c1 = length(na.omit(unique(Cooling.Type.1))),
                                                          c2 = length(na.omit(unique(Cooling.Type.2))),
                                                          c3 = length(na.omit(unique(Cooling.Type.3))),
                                                          c4 = length(na.omit(unique(Cooling.Type.4))))
  cooling<-cooling %>% filter(c1==1 & c2==0 & c3==0 & c4==0)
  singleMissing<-manualBogencoo2 %>% filter(endsWith(bogencoo,"^1"))
  manualBogencoo2<-manualBogencoo2 %>% filter(!endsWith(bogencoo,"^1"))
  manualBogencoo2<-manualBogencoo2 %>% filter(!Plant.Code %in% cooling$Plant.Code)
  manualBogencoo2<-rbind(manualBogencoo2,singleMissing)
  manualBogencoo2<-manualBogencoo2 %>% select(Plant.Code)
  
  manualBogencoo<-rbind(manualBogencoo,manualBogencoo2)
  manualBogencoo<-manualBogencoo[order(manualBogencoo$Plant.Code),]
 
  combogencoo_cooly_type$Plant.Code<-sapply(combogencoo_cooly_type$bogencoo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][2]))
  combogencoo_cooly_type$manualEdit<-rep(0,nrow(combogencoo_cooly_type))
  combogencoo_cooly_type<-combogencoo_cooly_type %>% select(manualEdit,Plant.Code,
                                                            names(combogencoo_cooly_type)[!names(combogencoo_cooly_type) %in% c("manualEdit","Plant.Code")])
  
    #remove nukes from manualBogencoos
  nukes<-gen_fuel_data
  # nukes$Nuclear.Unit.Id<-ifelse(nukes$Nuclear.Unit.Id==".",NA,nukes$Nuclear.Unit.Id)
  # nukes <- nukes %>% subset(!is.na(Nuclear.Unit.Id))
  # 
  nukes<-gen_fuel_data %>% filter(!(is.na(Nuclear.Unit.Id)) &
                                       (Nuclear.Unit.Id!="."))
  
  nonNukes<-gen_fuel_data %>% filter((is.na(Nuclear.Unit.Id)) |
                                                      (Nuclear.Unit.Id=="."))
  
  #complexNukes
  complexNukes<-nonNukes[nonNukes$Plant.Code %in% nukes$Plant.Code,]$Plant.Code
  
  #nukeOnly plants
  nukes<-nukes %>% filter(!Plant.Code %in% complexNukes)
  
  manualBogencoo<-manualBogencoo %>% filter(!Plant.Code %in% nukes$Plant.Code)
  
  #check for unreasonable percentages in combogencoo_cooly_type
  bigPercent<-combogencoo_cooly_type %>% 
    dplyr::filter_at(vars(all_of(names(combogencoo_cooly_type)[!names(combogencoo_cooly_type) %in%
                                   c("Plant.Code","manualEdit","bogencoo")])), 
                     any_vars(. >1 & !is.na(.)))
 bigPercent<-data.frame(Plant.Code = unique(bigPercent$Plant.Code))
 manualBogencoo<-rbind(manualBogencoo,bigPercent)
 manualBogencoo<-manualBogencoo[!duplicated(manualBogencoo),]
 manualBogencoo<-manualBogencoo[order(manualBogencoo$Plant.Code),]
  
  #save cool types for nukes
  combogencoo_cooly_type_nukes<-combogencoo_cooly_type %>% filter(Plant.Code %in% nukes$Plant.Code | Plant.Code %in% complexNukes)
  combogencoo_cooly_type<-combogencoo_cooly_type %>% filter(Plant.Code %in% sheet3_key$Plant.Code)
  

  #if complexNuke make sure combogencoos unique
  bogencoo.key<-bogencoo.key %>% mutate(combogencoo = ifelse(Plant.Code %in% 
                                                            complexNukes,paste0(combogencoo,".NonNuke"),
                                                            combogencoo))
  
  
  # bogencoo.key$combogencoo<-sapply(bogencoo.key$bocoo, function(x)  ifelse(!is.na(x),paste0("combogencoo^",bogencoo.key$bocoo),NA))
  
  #bogencoo.key<-left_join(sheet3_key,bocoo.key)
  
  
  #Output boiler-generator-cooling (BOGENCOO) association key#
  #write.csv(bogencoo.key,"CondenserDutyModel/Output/bogencoo_key.csv",row.names = F)
  

  
  
  
  
  bogencoo.key.list<-named.list(bogencoo.key,manualBogencoo,combogencoo_cooly_type,combogencoo_cooly_type_nukes)
  
  # return output data
  return(bogencoo.key.list)
}
