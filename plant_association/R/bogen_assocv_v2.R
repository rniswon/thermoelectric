bogen_assocv.v2 <- function(analysisYear,bogen,generator.data,generation.data,boilerFuelData,
                            boilerDesignData,retiredGenerators,gen_fuel_data,plantList,nukePlants,
                            select_RPM,vis_out=F,Fill.Gen.ID=T){
  months<-c("January","February","March","April","May",
            "June","July","August","September","October","November","December")
  #flag Plant not in input table
  missingPlants<-fileMissingPlants(plantList,bogen,generator.data,generation.data,boilerFuelData,
                                             boilerDesignData,retiredGenerators)
  #nukePlants$Plant.Code<-sapply(nukePlants$combogencoo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][2]))
  
  #select_RPM<-c("CA", "CS", "CT", "ST",NA)
  
  #These first few lines are only cleaning up the datasets and preparing them for the association process
  #Bogen table, set generator.id and boiler.id variables to character strings
  bogen<-bogen %>% filter(Plant.Code %in% plantList)
  bogen$Generator.ID <- as.character(bogen$Generator.ID)
  bogen$Boiler.ID <- as.character(bogen$Boiler.ID)
  #Drop utlity id variable from bogen table
  bogen <- select(bogen,-any_of(c("Utility.ID")))
  
  #added by Lily
  #flag changed IDS
  bogen<-prepIDcols(bogen,out_colNames=c("Generator.ID","Boiler.ID"),
                orig_colNames = c("orig_Generator.ID_assoc","orig_Boiler.ID_assoc"),
                      flagnames=c("flagGenerator.ID_assoc","flagBoiler.ID_assoc"))
    

  
  #Generator data
  #flag changed IDS
  gen_860 <- generator.data
  gen_860<-gen_860 %>% filter(Plant.Code %in% plantList)
  gen_860<- gen_860 %>% filter(!Plant.Code %in% nukePlants$Plant.Code)
  gen_860<-prepIDcols(gen_860,out_colNames="Generator.ID",
                    orig_colNames = "orig_Generator.ID_860",
                    flagnames="flagGenerator.ID_860")
  

  
  #solid fuel gas
  if (analysisYear>2003){
  solidGasPlants<-gen_860 %>% filter(Solid.Fuel.Gasification.System.=="Y")
  solidGasPlants<-solidGasPlants$Plant.Code
  assign("solidGasPlants",solidGasPlants,envir = .GlobalEnv)
  }
  
  #compareRPM added by Lily
  compare_RPM_860<-select(gen_860,c("Plant.Code","Generator.ID","flagGenerator.ID_860",
                                "orig_Generator.ID_860","Prime.Mover"))
  names(compare_RPM_860)[names(compare_RPM_860)=="Prime.Mover"]<-"Reported.Prime.Mover_860"
  
  
  #gen_860<-gen_860[gen_860$Prime.Mover %in% select_RPM,]
  names(gen_860)[names(gen_860)=="Prime.Mover"]<-"Reported.Prime.Mover_860"
  

  
  #Boiler generation data, set generator.id variable to character strings
  #flag changed IDS
  generation.data <- generation.data
  generation.data<-generation.data %>% filter(Plant.Code %in% plantList)
  generation.data<- generation.data %>% filter(!Plant.Code %in% nukePlants$Plant.Code)
  generation.data<-prepIDcols(generation.data,out_colNames="Generator.ID",
                      orig_colNames = "orig_Generator.ID_923",
                      flagnames="flagGenerator.ID_923")

  
  #added by Lily
  if (analysisYear>2005){
  compare_RPM_923<-select(generation.data,c("Plant.Code","Generator.ID","flagGenerator.ID_923",
                                "orig_Generator.ID_923","Reported.Prime.Mover"))
  names(compare_RPM_923)[names(compare_RPM_923)=="Reported.Prime.Mover"]<-"Reported.Prime.Mover_923"
  #compare_RPM_923<-compare_RPM_923[compare_RPM_923$Reported.Prime.Mover_923 %in% select_RPM,]
  compare_RPM <- full_join_track(compare_RPM_860,compare_RPM_923,by=c("Plant.Code","Generator.ID"),.merge=F,all=T)
  
  #generation.data<-generation.data[generation.data$Reported.Prime.Mover %in% select_RPM,]
  
  #added by Lily
  #save crosswalk flag ids
  generation.flagIDs<-generation.data %>% select(Plant.Code, Generator.ID,
                                                 Reported.Prime.Mover)
  }else{
    generation.flagIDs<-generation.data %>% select(Plant.Code, Generator.ID)
  }
  generation.flagIDs<-generation.flagIDs[!duplicated(generation.flagIDs),]
  
 
      assign("generation.data.line124",generation.data,envir = .GlobalEnv)
  #sum all lines by plant and Generator.ID
  outGeneration.list<-sumNetGen_LinesAndPlant(months,generation.data)    
  unPackList(lists = list(outGeneration.list = outGeneration.list),
             parentObj = list(NA))
  # for(m in months){
  #   netGenstr<-paste0("sumbyGen<-generation.data %>% group_by(Plant.Code,Generator.ID,orig_Generator.ID_923,flagGenerator.ID_923) %>%
  #                       summarize(Generator.ID.Total.Net.Generation.",m,"=sum(Net.Generation.",m,",na.rm=T))")
  #   eval(parse(text=netGenstr))
  #   if (m=="January"){
  #     generation.data.gb<-sumbyGen
  #   }else{
  # 
  #     generation.data.gb<-merge(generation.data.gb,sumbyGen,by=c("Plant.Code","Generator.ID","orig_Generator.ID_923","flagGenerator.ID_923"),all=T)
  #   }
  # 
  # }
  #     generation.data.gb2 <- generation.data %>% group_by(.,Plant.Code,Generator.ID) %>% summarise(Net.Generation.mwh=sum(Net.Generation.Year.To.Date,na.rm=T))
  #     generation.data.gb<-merge(generation.data.gb,generation.data.gb2,by=c("Plant.Code","Generator.ID"))
  # assign("generation.data.gb",generation.data.gb,envir = .GlobalEnv)
  # 
  # #sum all lines by plant
  # for(m in months){
  #   netGenstr<-paste0("sumbyPlant<-generation.data.gb %>% group_by(Plant.Code) %>% 
  #                       summarize(PlantLine.Total.Net.Generation.",m,"=sum(Generator.ID.Total.Net.Generation.",m,",na.rm=T))")
  #   eval(parse(text=netGenstr))
  #   if (m=="January"){
  #     sumNetGenLinesByPlant<-sumbyPlant  
  #   }else{
  #     sumNetGenLinesByPlant<-merge(sumNetGenLinesByPlant,sumbyPlant,by="Plant.Code",all=T)
  #   }
  #   
  # }
  # assign("sumNetGenLinesByPlant",sumNetGenLinesByPlant,envir = .GlobalEnv)
     
  #Aggregate total net generation by plant and generator.id
 # generation.data.gb <- generation.data %>% group_by(.,Plant.Code,Generator.ID)
 # gen_923 <- generation.data.gb %>% summarise(Net.Generation.mwh=sum(Net.Generation.Year.To.Date))
  #gen_923<-generation.data.gb
  #Create a variable to track which boilers are missing from the 923 report
  gen_923$missing.from.923 <- FALSE
  
  #added by Lily
  gen_923<-merge(gen_923,generation.flagIDs, by=c("Plant.Code","Generator.ID"))
  if (analysisYear>2005){
    names(gen_923)[names(gen_923)=="Reported.Prime.Mover"]<-"Reported.Prime.Mover_923"
  }
  
  
  
  #Merge generator data and generation data 
  merged <- full_join_track(gen_860,gen_923,by=c("Plant.Code","Generator.ID"),.merge=T,all=T)
  missing.from.860 <- merged[merged$.merge=='right_only',]
  
  
  #Compile list of all generators
  gens <- full_join_track(gen_923,gen_860,by=c("Plant.Code","Generator.ID"),.merge=F,all=T)
  
  #gen.status
  names(gens)[names(gens)=="Status"]<-"Generator.Status"


  
   #Select columns to keep after merge
  if("Technology" %in% names(gens)){
  gens <- select(gens,c("Plant.Code","Generator.ID","Unit.Code","missing.from.923",
                        "orig_Generator.ID_923","flagGenerator.ID_923","orig_Generator.ID_860","flagGenerator.ID_860",
                        "Reported.Prime.Mover_860","Reported.Prime.Mover_923","Technology",
                        "Energy.Source.1","Generator.Status",contains("Net.Generation")),)
  }else{
    if (analysisYear>2005){
     gens <- select(gens,c("Plant.Code","Generator.ID","Unit.Code","missing.from.923",
                          "orig_Generator.ID_923","flagGenerator.ID_923","orig_Generator.ID_860","flagGenerator.ID_860",
                          "Reported.Prime.Mover_860","Reported.Prime.Mover_923",
                          "Energy.Source.1","Generator.Status",contains("Net.Generation")),)  
    }else{
      if (analysisYear>2003){
      gens <- select(gens,c("Plant.Code","Generator.ID","Unit.Code","missing.from.923",
                            "orig_Generator.ID_923","flagGenerator.ID_923","orig_Generator.ID_860","flagGenerator.ID_860",
                            "Reported.Prime.Mover_860",
                            "Energy.Source.1","Generator.Status",contains("Net.Generation")),)
      }else{
        gens <- select(gens,c("Plant.Code","Generator.ID","missing.from.923",
                              "orig_Generator.ID_923","flagGenerator.ID_923","orig_Generator.ID_860","flagGenerator.ID_860",
                              "Reported.Prime.Mover_860",
                              "Energy.Source.1","Generator.Status",contains("Net.Generation")),)
      }
    }
    
  }
  gens$Generator.ID <- as.character(gens$Generator.ID)
  gens <- ungroup(gens)
  assign("gens_line189",gens,envir = .GlobalEnv)

  
  #Linked bogen associations

  bga.1 <- full_join_track(gens,bogen,by=c("Plant.Code","Generator.ID"),.merge=F,all=T)
  bga.1 <- select(bga.1,-any_of(c("Utility.Name","Plant.Name","Steam.Plant.Type")))

  
  
  bga_assn <- bga.1[!is.na(bga.1$Boiler.ID),]
  if (nrow(bga_assn)!=0){
  bga_assn$bga.source <- 'eia860.org'
  }else{
    bga_assn$bga.source <- character(0)
  }
  
  #Unlinked bogen associations
  bga_unassn <- bga.1[is.na(bga.1$Boiler.ID),]
  bga_unassn <- select(bga_unassn,-c(Boiler.ID))
  
  #String Matching unassigned boilers based on fuel data
  bf.923 <- boilerFuelData
  #print("line75")
  #print(unique(bf.923[bf.923$Plant.Code==96,]$Boiler.ID))
  
  #added by Lily
  #Drop Leading zeros in Boiler.ID
  bf.923<-bf.923 %>% filter(Plant.Code %in% plantList)
  bf.923<- bf.923 %>% filter(!Plant.Code %in% nukePlants$Plant.Code & Reported.Fuel.Type.Code!="NUC")
  bf.923$flagBoiler.ID_bf.923<-sapply(bf.923$Boiler.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                        ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  bf.923$orig_Boiler.ID_bf.923<-bf.923$Boiler.ID
  bf.923$Boiler.ID <- gsub("(?<![0-9])", "",bf.923$Boiler.ID , perl = TRUE)
  bf.923$Boiler.ID <- gsub("[[:space:]]", "", bf.923$Boiler.ID )
  bf.923$Boiler.ID <- as.character(bf.923$Boiler.ID)
  
  bf.923<-removeLeadZero(bf.923,"Boiler.ID","flagBoiler.ID_bf.923")
  
  bf.923$missing.bf.923 <- FALSE
  
  #9.21.21 make NA Boiler.IDs in bf.923 = "NA"
  #bf.923$Boiler.ID<-ifelse(is.na(bf.923$Boiler.ID),"NA",bf.923$Boiler.ID)
  print("line 180")
  print(unique(bf.923[bf.923$Plant.Code %in% c(7294,10805),]$Boiler.ID))
  #added by Lily
  if (analysisYear>2005){

  compare_RPM_bf.923<-select(bf.923,c("Plant.Code","Boiler.ID","flagBoiler.ID_bf.923",
                                            "orig_Boiler.ID_bf.923","Reported.Prime.Mover"))
  names(compare_RPM_bf.923)[names(compare_RPM_bf.923)=="Reported.Prime.Mover"]<-"Reported.Prime.Mover_bf.923"
  #compare_RPM_bf.923<-compare_RPM_bf.923[compare_RPM_bf.923$Reported.Prime.Mover_bf.923 %in% select_RPM,]
  compare_RPM <- full_join_track(compare_RPM,select(bga.1,c("Plant.Code","Generator.ID","Boiler.ID")),
                                                    by=c("Plant.Code","Generator.ID"),.merge=F,all=T)
  compare_RPM <- full_join_track(compare_RPM,compare_RPM_bf.923,by=c("Plant.Code","Boiler.ID"),.merge=F,all=T)
  compare_RPM<-compare_RPM %>% filter(Reported.Prime.Mover_860 %in% na.omit(select_RPM) |
                                        Reported.Prime.Mover_923 %in% na.omit(select_RPM) |
                                        Reported.Prime.Mover_bf.923 %in% na.omit(select_RPM))
  
  compare3cols<-function(x,y,z){
    if (x!=y & !is.na(x) & !is.na(y)){
      flag<-1
    }else if (x!=z & !is.na(x) & !is.na(z)){
      flag<-1
    }else if (z!=y & !is.na(z) & !is.na(y)){
      flag<-1
    }else if (is.na(x) & is.na(y) & is.na(z)){
      flag<-1
    }else{
      flag<-0
    }
    return(flag)
  }
  
  compare_RPM<-compare_RPM %>% rowwise() %>%mutate(flag_RPM = compare3cols(Reported.Prime.Mover_860,
                                                              Reported.Prime.Mover_923,
                                                              Reported.Prime.Mover_bf.923))
  compare_RPM<-compare_RPM %>% filter(flag_RPM==1)
  compare_RPM<-select(compare_RPM, c("Plant.Code","Generator.ID","Boiler.ID",
                                     "orig_Generator.ID_860","flagGenerator.ID_860",
                                     "orig_Generator.ID_923","flagGenerator.ID_923",
                                     "orig_Boiler.ID_bf.923","flagBoiler.ID_bf.923",
                                     "Reported.Prime.Mover_860",
                                     "Reported.Prime.Mover_923",
                                     "Reported.Prime.Mover_bf.923",
                                     "flag_RPM"))
  
}#analysisYear>2005
  
  #bf.923<-bf.923[bf.923$Reported.Prime.Mover %in% select_RPM,]
  orphanBoilerPlants<-bf.923 %>% filter(is.na(Boiler.ID))
  orphanBoilerPlants<-unique(orphanBoilerPlants$Plant.Code)
  assign("orphanBoilerPlants",orphanBoilerPlants,envir = .GlobalEnv)
  
  #print("line82")
  #print(unique(bf.923[bf.923$Plant.Code==96,]$Boiler.ID))
  
  #added by LIly
  #check if boiler.IDs NOT in association table
  #Boiler.ID_NotAssoc<-data.frame(Plant.Code=integer(0), Boiler.ID = character(0), Reported.Prime.Mover=character(0))
  Boiler.ID_NotAssoc<-bf.923[0,]
  for (p in unique(bf.923$Plant.Code)){
    if (analysisYear>2005){
      dfSub<-bf.923[bf.923$Plant.Code==p & bf.923$Reported.Prime.Mover %in% na.omit(select_RPM),]
    }else{
      dfSub<-bf.923[bf.923$Plant.Code==p,]
    }
    
    assSub<-bogen[bogen$Plant.Code==p,]
    testBoiler<-unique(dfSub$Boiler.ID)
    testBoiler<-testBoiler[!testBoiler %in% assSub$Boiler.ID]
    if (length(testBoiler)!=0){
      #testBoiler<-data.frame(Plant.Code=rep(p,length(testBoiler)),Boiler.ID = testBoiler)
      testBoiler<-dfSub %>% filter(Boiler.ID %in% testBoiler)
      Boiler.ID_NotAssoc<-rbind(Boiler.ID_NotAssoc,testBoiler %>% select(names(Boiler.ID_NotAssoc)))
    }
  }
  if (analysisYear>2005){
  names(Boiler.ID_NotAssoc)[names(Boiler.ID_NotAssoc)=="Reported.Prime.Mover"]<-"Reported.Prime.Mover_bf.923"
  names(bf.923)[names(bf.923)=="Reported.Prime.Mover"]<-"Reported.Prime.Mover_bf.923"
  }
  #New Fuel Heat Calculation added 9.29.21
  #calculate fuel heat on each line for each month by multiplying Quantity.Of.Fuel.Consumed*MMbtu.Per.Unit

  for(m in months){
    fuelHeatstr<-paste0("bf.923<-bf.923 %>% mutate(Fuel.Heat.",m,"=Quantity.Of.Fuel.Consumed.",m,"*MMbtu.Per.Unit.",m,")")
    eval(parse(text=fuelHeatstr))
  }
  bf.923.ungrouped<-bf.923 
  
  
  #sum all lines by plant
  for(m in months){
    fuelHeatstr<-paste0("sumbyPlant<-bf.923 %>% group_by(Plant.Code) %>% 
                        summarize(PlantLine.Total.Fuel.Heat.",m,"=sum(Fuel.Heat.",m,",na.rm=T))")
    eval(parse(text=fuelHeatstr))
    if (m=="January"){
    sumFuelHeatLinesByPlant<-sumbyPlant  
    }else{
      sumFuelHeatLinesByPlant<-merge(sumFuelHeatLinesByPlant,sumbyPlant,by="Plant.Code",all=T)
    }
    
  }
  assign("sumFuelHeatLinesByPlant",sumFuelHeatLinesByPlant,envir = .GlobalEnv)
  
  #sum all lines by plant and Boiler.ID
  
  for(m in months){
    fuelHeatstr<-paste0("sumbyBoiler<-bf.923 %>% group_by(Plant.Code,Boiler.ID,
                                                          orig_Boiler.ID_bf.923,flagBoiler.ID_bf.923,missing.bf.923) %>% 
                        summarize(Boiler.ID.Total.Fuel.Heat.",m,"=sum(Fuel.Heat.",m,",na.rm=T))")
    eval(parse(text=fuelHeatstr))
    if (m=="January"){
      bfd.gb<-sumbyBoiler  
    }else{

      bfd.gb<-merge(bfd.gb,sumbyBoiler,by=c("Plant.Code","Boiler.ID",
                                            "orig_Boiler.ID_bf.923","flagBoiler.ID_bf.923","missing.bf.923"),all=T)
    }
    
  }
  assign("bfd.gb",bfd.gb,envir = .GlobalEnv)
 
  #Get unique Boiler.IDs ???????????Question this step
  # bfd.gb <- select(bfd.gb,c(Plant.Code,Boiler.ID))
  # bfd.gb <- bfd.gb[!duplicated(bfd.gb[,c("Plant.Code","Boiler.ID")]),]
  bfd.923 <- ungroup(bfd.gb)
  
  # #Sum monthly fuel heat OLD
  # #bf.923$total.fuel.mmbtu.per.unit <- rowSums(bf.923[,c(28:39)])
  # bf.923$total.fuel.mmbtu.per.unit <- rowSums(bf.923 %>% select(contains("MMbtu.Per.Unit")))
  # #Calculate total fuel heat
  # bf.923$Total.Heat.Content.MMBTU <- bf.923$Total.Fuel.Consumption.Quantity*bf.923$total.fuel.mmbtu.per.unit
  # #Summarize heat content by plant and boiler id
  # bfd.gb <- bf.923 %>% group_by(Plant.Code,Boiler.ID) %>% summarize(Boiler.Total.Heat.Content.MMBTU=sum(Total.Heat.Content.MMBTU,na.rm=F))
  # bfd.gb <- select(bfd.gb,c(Plant.Code,Boiler.ID,Boiler.Total.Heat.Content.MMBTU))
  
  
  

  #added by LIly
  if (analysisYear>2005){
     bf_flagIDs<-bf.923 %>% select(Plant.Code, Boiler.ID, #orig_Boiler.ID_bf.923,flagBoiler.ID_bf.923,
                                Reported.Prime.Mover_bf.923,Reported.Fuel.Type.Code) 
  }else{
    bf_flagIDs<-bf.923 %>% select(Plant.Code, Boiler.ID) 
  }

  bf_flagIDs<-bf_flagIDs[!duplicated(bf_flagIDs),]
  bfd.923<-merge(bfd.923,bf_flagIDs,by=c("Plant.Code","Boiler.ID"))
  
  # print("line261")
  # print(bfd.923 %>% filter(Plant.Code==10805))
  #bfd.923 <- unique(bfd.923)
  #print("line95")
  #print(unique(bfd.923[bfd.923$Plant.Code==96,]$Boiler.ID))
  #print("line97")
  #print(unique(bga.1[bga.1$Plant.Code==96,]$Boiler.ID))
  
  #add bfd columns to bga_assn
  bga_assn <- merge(bfd.923,bga_assn,by=c("Plant.Code","Boiler.ID"),all.y=T)
  # print("276")
  # print(bga_assn %>% filter(Plant.Code==10805))
  #Create list of boilers not in bogen associations(bga) table
  bga.2 <- full_join_track(bfd.923,bga.1,by=c("Plant.Code","Boiler.ID"),.merge=T,all=T)
  bfd.not.in.bga <- bga.2[bga.2$.merge=="left_only",]
  bfd.not.in.bga <- select(bfd.not.in.bga,-c(.merge))
  bfd.not.in.bga$Boiler.ID.2 <- bfd.not.in.bga$Boiler.ID
  #bga_unassn <- ungroup(bga_unassn)
  #print("line104")
  ##print(unique(bga.2[bga.2$Plant.Code==96,]$Boiler.ID))
  #print(unique(bga.2 %>% filter(Plant.Code==96)))
  #Match unassociated generators and boilers
  # print("287")
  # print(unique(bga.2 %>% filter(Plant.Code==10805)))
  # ##added by Lily
  # #flag changed IDS
  # bga_unassn$Generator.ID <- gsub("(?<![0-9])0+", "", bga_unassn$Generator.ID , perl = TRUE)
  # bga_unassn$Generator.ID <- gsub("[[:space:]]", "", bga_unassn$Generator.ID )
  # 
  # bfd.not.in.bga$Generator.ID <- gsub("(?<![0-9])0+", "", bfd.not.in.bga$Generator.ID , perl = TRUE)
  # bfd.not.in.bga$Generator.ID <- gsub("[[:space:]]", "", bfd.not.in.bga$Generator.ID )
  # 
  # bfd.not.in.bga$Boiler.ID <- gsub("(?<![0-9])0+", "", bfd.not.in.bga$Boiler.ID , perl = TRUE)
  # bfd.not.in.bga$Boiler.ID <- gsub("[[:space:]]", "", bfd.not.in.bga$Boiler.ID)
  # 
  # bfd.not.in.bga$Boiler.ID2 <- gsub("(?<![0-9])0+", "", bfd.not.in.bga$Boiler.ID.2 , perl = TRUE)
  # bfd.not.in.bga$Boiler.ID2 <- gsub("[[:space:]]", "", bfd.not.in.bga$Boiler.ID.2)
  # 
  # print("line 303")
  # print(bga_unassn %>% filter(Plant.Code==10805))
  # print("line 305")
  # print(bfd.not.in.bga %>% filter(Plant.Code==10805))
  if (analysisYear>2005){
    bfd.not.in.bga.formerge<-bfd.not.in.bga %>% select(c("Plant.Code","Boiler.ID","Boiler.ID.2","Reported.Prime.Mover_bf.923","Reported.Fuel.Type.Code",
                                                       "orig_Boiler.ID_bf.923","flagBoiler.ID_bf.923","missing.bf.923"),contains("Fuel.Heat")) 
  }else{
    bfd.not.in.bga.formerge<-bfd.not.in.bga %>% select(c("Plant.Code","Boiler.ID","Boiler.ID.2",
                                                         "orig_Boiler.ID_bf.923","flagBoiler.ID_bf.923","missing.bf.923"),contains("Fuel.Heat")) 
  }
 
  assign("bfd.not.in.bga.formerge",bfd.not.in.bga.formerge,envir = .GlobalEnv)
  # bga_unassn.2 <- merge(bga_unassn,bfd.not.in.bga[,c("Plant.Code","Boiler.ID","Boiler.ID.2","Reported.Prime.Mover_bf.923",
  #                                                    "orig_Boiler.ID_bf.923","flagBoiler.ID_bf.923")],
  #                       by.x=c("Plant.Code","Generator.ID"),by.y=c("Plant.Code","Boiler.ID"),all.x=T)
  
  bga_unassn.2 <- merge(bga_unassn,bfd.not.in.bga.formerge,
                        by.x=c("Plant.Code","Generator.ID"),by.y=c("Plant.Code","Boiler.ID"),all.x=T)
  names(bga_unassn.2)[names(bga_unassn.2)=="Boiler.ID.2"] <- "Boiler.ID"
  bga_unassn.2 <- bga_unassn.2[order(bga_unassn.2$Plant.Code),]
  bga_unassn.2$bga.source[!is.na(bga_unassn.2$Boiler.ID)] <- "string_assn"
  
  # print("line 311")
  # print(bga_unassn.2 %>% filter(Plant.Code==10805))
  # print("line 313")
  # print(bga_assn %>% filter(Plant.Code==10805))
  # 
  #Collection of all Bogens assigned and unassigned with their sources and assignment method
  bfd.not.in.bga<-anti_join(bfd.not.in.bga,bga_unassn.2,by=c("Plant.Code","Boiler.ID"))
  

  
  Boiler.ID_NotAssoc<-left_join(Boiler.ID_NotAssoc,bfd.not.in.bga %>% select(c("Plant.Code","Boiler.ID"),contains("Fuel.Heat")),by=c("Plant.Code","Boiler.ID"))
  # print("line 322")
  # print(bfd.not.in.bga %>% filter(Plant.Code==10805))
  
  #9.21.21 add in "NA" Boiler IDs
  bga.2 <- bind_rows(bga_assn,bga_unassn.2)#,bfd.not.in.bga %>% filter(Plant.Code==2059))
    
  #repeat with orig_Generator.ID_860
  if (nrow(bfd.not.in.bga)!=0){
    if (analysisYear>2005){
      bfd.not.in.bga.formerge<-bfd.not.in.bga %>% select(c("Plant.Code","Boiler.ID","Boiler.ID.2","Reported.Prime.Mover_bf.923","Reported.Fuel.Type.Code",
                                                         "orig_Boiler.ID_bf.923","flagBoiler.ID_bf.923","missing.bf.923"),contains("Fuel.Heat"))
    }else{
      bfd.not.in.bga.formerge<-bfd.not.in.bga %>% select(c("Plant.Code","Boiler.ID","Boiler.ID.2",
                                                           "orig_Boiler.ID_bf.923","flagBoiler.ID_bf.923","missing.bf.923"),contains("Fuel.Heat"))
    }
    
    assign("bfd.not.in.bga.formerge",bfd.not.in.bga.formerge,envir = .GlobalEnv)
    # bga_unassn.2 <- merge(bga_unassn,bfd.not.in.bga[,c("Plant.Code","Boiler.ID","Boiler.ID.2","Reported.Prime.Mover_bf.923",
    #                                                    "orig_Boiler.ID_bf.923","flagBoiler.ID_bf.923")],
    #                       by.x=c("Plant.Code","Generator.ID"),by.y=c("Plant.Code","Boiler.ID"),all.x=T)
    
    bga_unassn.3 <- merge(bga_unassn,bfd.not.in.bga.formerge,
                          by.x=c("Plant.Code","orig_Generator.ID_860"),by.y=c("Plant.Code","Boiler.ID"),all.x=T)
    names(bga_unassn.3)[names(bga_unassn.3)=="Boiler.ID.2"] <- "Boiler.ID"
    bga_unassn.3 <- bga_unassn.3[order(bga_unassn.3$Plant.Code),]
    bga_unassn.3$bga.source[!is.na(bga_unassn.3$Boiler.ID)] <- "string_assn"
    
    bfd.not.in.bga<-anti_join(bfd.not.in.bga,bga_unassn.3,by=c("Plant.Code","Boiler.ID"))
    Boiler.ID_NotAssoc<-left_join(Boiler.ID_NotAssoc,bfd.not.in.bga %>% select(c("Plant.Code","Boiler.ID"),contains("Fuel.Heat")),by=c("Plant.Code","Boiler.ID"))
    
    #9.21.21 add in "NA" Boiler IDs
    bga.2 <- bind_rows(bga.2,bga_unassn.3)#,bfd.not.in.bga %>% filter(Plant.Code==2059))
    
  }
  
  bga.2 <- bga.2[order(bga.2$Plant.Code),]
  # print("line 319")
  # print(bga.2 %>% filter(Plant.Code==10805))
  
  #print("line132")
 # #print(unique(bga.2[bga.2$Plant.Code==96,]$Boiler.ID))
  #print(unique(bga.2 %>% filter(Plant.Code==96)))
  # print("321")
  # print(bga.2 %>% filter(Plant.Code==10805))
  bga.2$missing.from.923[is.na(bga.2$missing.from.923)] <- T
  bga.2$Boiler.ID <- as.character(as.factor(bga.2$Boiler.ID))

  ##added by Lily
  # bga.2$Boiler.ID <- gsub("(?<![0-9])0+", "", bga.2$Boiler.ID , perl = TRUE)
  # bga.2$Boiler.ID <- gsub("[[:space:]]", "", bga.2$Boiler.ID)
  # print("328")
  # print(unique(bga.2 %>% filter(Plant.Code==10805)))
  
  bga.2$Boiler.ID <- na_if(bga.2$Boiler.ID,"character(0)")
  if (analysisYear>2003){
  bga.2$Unit.Code <- na_if(bga.2$Unit.Code,"")  
  
  
  # print("331")
  # print(unique(bga.2 %>% filter(Plant.Code==10805)))
  

  #Connecting Bogens with Unit.Codes
  bga.2$Unit.Code <- na_if(bga.2$Unit.Code,"")
  bga.2.units <- bga.2[!is.na(bga.2$Unit.Code),]
  if (analysisYear>2005){
   bga.2.gen.units <- select(bga.2.units,-c(Boiler.ID, contains("Boiler"),Reported.Prime.Mover_bf.923,Reported.Fuel.Type.Code,missing.bf.923)) 
   bga.2.boil.units <- select(bga.2.units,c(Plant.Code,Boiler.ID,Unit.Code, contains("Boiler"),Reported.Prime.Mover_bf.923,Reported.Fuel.Type.Code,missing.bf.923))
  }else{
    bga.2.gen.units <- select(bga.2.units,-c(Boiler.ID, contains("Boiler"),missing.bf.923)) 
    bga.2.boil.units <- select(bga.2.units,c(Plant.Code,Boiler.ID,Unit.Code, contains("Boiler"),missing.bf.923))
  }
  

  # print("337")
  # print(unique(bga.2.boil.units %>% filter(Plant.Code==10805)))

  bga.2.boil.units <- subset(bga.2.boil.units,!is.na(Boiler.ID))
  #added by LIly 4.20.21
  na.Boiler.ID <- bga.2.units %>% subset(is.na(Boiler.ID))
 
  #print("line153")
  ##print(unique(bga.2[bga.2$Plant.Code==96,]$Boiler.ID))
  #print(unique(bga.2 %>% filter(Plant.Code==96)))
  #print("line177")
  #print(unique(bga.2.gen.units %>% filter(Plant.Code==96)))
  #print(unique(bga.2.boil.units %>% filter(Plant.Code==96)))
  #Merge the boilers with units
  bga.unit.compilation <- full_join_track(bga.2.gen.units,bga.2.boil.units,by=c("Plant.Code","Unit.Code"))
  
  bga.unit.compilation$bga.source[is.na(bga.unit.compilation$bga.source)] <- 'unit_connection'
  #print("line182")
  #print(unique(bga.unit.compilation %>% filter(Plant.Code==96) %>% select(Plant.Code,Boiler.ID,Generator.ID)))
  # print("line351")
  # print(bga.unit.compilation %>% filter(Plant.Code==10805))
  #List of boilers with no unit code
  bga2.non.units <- bga.2 %>% subset(is.na(Unit.Code))
  # print("line187")
  # print(unique(bga2.non.units %>% filter(Plant.Code==10805)))

  #changed by LIly
  #bga.3 <- rbind(bga2.non.units,bga.unit.compilation)
  bga.3 <- rbind(bga2.non.units,bga.unit.compilation)
  }else{#analysisYear>2003
  bga.3<-bga.2
  na.Boiler.ID <- bga.2 %>% subset(is.na(Boiler.ID))
    }
  bga.3 <- bga.3[order(bga.3$Plant.Code),]

  #changed by LIly
  if ("Technology" %in% names(bga.3)){
  bga.3.1 <- bga.3 %>% select(c(Plant.Code,Generator.ID,Boiler.ID,Unit.Code,bga.source,Net.Generation.mwh,missing.from.923,
                                orig_Generator.ID_923,flagGenerator.ID_923,orig_Generator.ID_860,flagGenerator.ID_860,
                                flagGenerator.ID_assoc, orig_Generator.ID_assoc,flagBoiler.ID_assoc,orig_Boiler.ID_assoc,
                                orig_Boiler.ID_bf.923,flagBoiler.ID_bf.923,missing.bf.923,
                                Reported.Prime.Mover_bf.923,Reported.Prime.Mover_923,Reported.Prime.Mover_860,Reported.Fuel.Type.Code,
                                Technology,Energy.Source.1,Generator.Status),contains("Fuel.Heat"),
                              contains("Generator.ID.Total.Net.Generation"))#,Boiler.Total.Heat.Content.MMBTU))
  }else{
    if (analysisYear>2005){
      bga.3.1 <- bga.3 %>% select(c(Plant.Code,Generator.ID,Boiler.ID,Unit.Code,bga.source,Net.Generation.mwh,missing.from.923,
                                    orig_Generator.ID_923,flagGenerator.ID_923,orig_Generator.ID_860,flagGenerator.ID_860,
                                    flagGenerator.ID_assoc, orig_Generator.ID_assoc,flagBoiler.ID_assoc,orig_Boiler.ID_assoc,
                                    orig_Boiler.ID_bf.923,flagBoiler.ID_bf.923,missing.bf.923,
                                    Reported.Prime.Mover_bf.923,Reported.Prime.Mover_923,Reported.Prime.Mover_860,Reported.Fuel.Type.Code,
                                    Energy.Source.1,Generator.Status),contains("Fuel.Heat"),
                                  contains("Generator.ID.Total.Net.Generation"))#,Boiler.Total.Heat.Content.MMBTU)) 
    }else{
      if (analysisYear>2003){
    bga.3.1 <- bga.3 %>% select(c(Plant.Code,Generator.ID,Boiler.ID,Unit.Code,bga.source,Net.Generation.mwh,missing.from.923,
                                  orig_Generator.ID_923,flagGenerator.ID_923,orig_Generator.ID_860,flagGenerator.ID_860,
                                  flagGenerator.ID_assoc, orig_Generator.ID_assoc,flagBoiler.ID_assoc,orig_Boiler.ID_assoc,
                                  orig_Boiler.ID_bf.923,flagBoiler.ID_bf.923,missing.bf.923,
                                  Reported.Prime.Mover_860,
                                  Energy.Source.1,Generator.Status),contains("Fuel.Heat"),
                                contains("Generator.ID.Total.Net.Generation"))#,Boiler.Total.Heat.Content.MMBTU))
      }else{
        bga.3.1 <- bga.3 %>% select(c(Plant.Code,Generator.ID,Boiler.ID,bga.source,Net.Generation.mwh,missing.from.923,
                                      orig_Generator.ID_923,flagGenerator.ID_923,orig_Generator.ID_860,flagGenerator.ID_860,
                                      flagGenerator.ID_assoc, orig_Generator.ID_assoc,flagBoiler.ID_assoc,orig_Boiler.ID_assoc,
                                      orig_Boiler.ID_bf.923,flagBoiler.ID_bf.923,missing.bf.923,
                                      Reported.Prime.Mover_860,
                                      Energy.Source.1,Generator.Status),contains("Fuel.Heat"),
                                    contains("Generator.ID.Total.Net.Generation"))#,Boiler.Total.Heat.Content.MMBTU))
      }
    }
  }
  
  # print("line370")

  #print("line167")
 # #print(unique(bga.3[bga.3$Plant.Code==96,]))
  #print(unique(bga.3 %>% filter(Plant.Code==96)))
  #Cleanup
  ##The next few lines address missing bogen associations that are listed in the bad plants list as either 
  #orphan boilers or orphan generators.
  #This line subsets the remaining bogen associations excluding any remaining missing Boiler.IDs. These boilers
  #are stored in the variable na.Boiler.ID
  #changed by LIly
  na.Boiler.ID<-na.Boiler.ID %>% select(names(bga.3.1))
  notNA.bga.3.1<-bga.3.1 %>% subset(!is.na(Boiler.ID))
  na.Boiler.ID<-anti_join(na.Boiler.ID,notNA.bga.3.1, by=c("Plant.Code","Generator.ID"))
  na.Boiler.ID <- rbind(na.Boiler.ID,bga.3.1 %>% subset(is.na(Boiler.ID)))
  na.Boiler.ID<-na.Boiler.ID[!duplicated(na.Boiler.ID),]
  
  #na.Boiler.ID <- bga.3.1 %>% subset(is.na(Boiler.ID))
  
  #print("NA.Boiler.ID")
  #print(bga.3.1[bga.3.1$Plant.Code==96,])
  #print(unique(bga.3.1 %>% filter(Plant.Code==96)))

  
  
  ##added by Lily
  # bocoo$flagBoiler.ID_bocoo<-sapply(bocoo$Boiler.ID, function(x) ifelse(grepl("(?<![0-9])0+",x,perl=TRUE),"removeChar",
  #                                                                       ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  # bocoo$orig_Boiler.ID_bocoo<-bocoo$Boiler.ID
  # bocoo$Boiler.ID <- gsub("(?<![0-9])0+", "", bocoo$Boiler.ID , perl = TRUE)
  # bocoo$Boiler.ID <- gsub("[[:space:]]", "", bocoo$Boiler.ID)
  # 
  # #Fill with boilers fom cooling associations where possible
  # #changed by Lily
  # #filler.bocoos <- select(bocoo,c(1,2))
  # filler.bocoos <- select(bocoo,c(1,3))
  # print("line268")
  # print(na.Boiler.ID %>% filter(Plant.Code==96))
  # fill.boiler.id <- left_join(na.Boiler.ID,filler.bocoos,by="Plant.Code")
  # print("line257")
  # print(unique(fill.boiler.id$bga.source))
  # fill.boiler.id$bga.source[is.na(fill.boiler.id$Boiler.ID.x) & !is.na(fill.boiler.id$Boiler.ID.y)]<-"fillBocoo"
  # fill.boiler.id$Boiler.ID.x[is.na(fill.boiler.id$Boiler.ID.x)] <- fill.boiler.id$Boiler.ID.y[is.na(fill.boiler.id$Boiler.ID.x)]
  # fill.boiler.id <- fill.boiler.id[-length(fill.boiler.id)]
  # names(fill.boiler.id) <- names(bga.3.1)
  # assign("fill.boiler.id",fill.boiler.id,envir = .GlobalEnv)
  # #Append to boiler associations list
  # bga.3.1 <- rbind(bga.3.1,fill.boiler.id)
  # print("line189")
  # print(bga.3.1 %>% filter(Plant.Code==96))
  # print(unique(bga.3.1[bga.3.1$Plant.Code==96,]$Boiler.ID))
  #Identify Plants with generation but no associated boilers
  genNoBoiler <- bga.3.1 %>% subset(is.na(Boiler.ID)&Net.Generation.mwh>0) 
  
  # write.csv(genNoBoiler,"CondenserDutyModel/Output/Orphan_Generators.csv",row.names = F)
  genNoBoiler <- as.data.frame(unique(genNoBoiler$Plant.Code))
  names(genNoBoiler) <- "Plant.Code"
  
  bga.3.2 <- full_join_track(genNoBoiler[1],bga.3.1,.merge = T)
  bga.3.2$Orphan.Generator[bga.3.2$.merge=="matched"] <- T
  bga.3.2$Orphan.Generator[is.na(bga.3.2$Orphan.Generator)] <- F
  #print("line201")
  #print(bga.3.2 %>% filter(Plant.Code==96))
  #print(unique(bga.3.2[bga.3.2$Plant.Code==96,]$Boiler.ID))
  
  #Identify generators that are in the 923 report but not mapped
  bga.3.2$unmapped_but_in_923 <- rep(NA,nrow(bga.3.2))
  #bga.3.2$unmapped_but_in_923[c(is.na(bga.3.2$Boiler.ID) & bga.3.2$missing.from.923==T & bga.3.2$Net.Generation.mwh==0)] <- T
  bga.3.2$unmapped_but_in_923[c(is.na(bga.3.2$Boiler.ID) & bga.3.2$missing.from.923==F & bga.3.2$Net.Generation.mwh==0)] <- T
  bga.3.2$unmapped_but_in_923[is.na(bga.3.2$unmapped_but_in_923)] <- F
  #print("line208")
  #print(bga.3.2 %>% filter(Plant.Code==96))
  #print(unique(bga.3.2[bga.3.2$Plant.Code==96,]$Boiler.ID))
  
  #Identify unmapped generators
  bga.3.2$unmapped <- rep(NA,nrow(bga.3.2))
  bga.3.2$unmapped[is.na(bga.3.2$Boiler.ID)] <- T
  bga.3.2$unmapped[!is.na(bga.3.2$Boiler.ID)] <- F
  #print("line215")
  #print(bga.3.2 %>% filter(Plant.Code==96))
  #print(unique(bga.3.2[bga.3.2$Plant.Code==96,]$Boiler.ID))
  
  if (analysisYear>2003){
  bga.3.2$Unit.Code[is.na(bga.3.2$Unit.Code)] <- "none"
}
  
  #changed by Lily
  Boiler.ID_NotAssoc<-anti_join(Boiler.ID_NotAssoc,bga.3.2, by=c("Plant.Code","Boiler.ID"))
  #bga.3.3 <- bga.3.2[-c(6,24)]
  bga.3.3 <- bga.3.2 %>% select(-c(".merge"))
  
  bga.out=bga.3.3

  
  #write.csv(bga.out,"CondenserDutyModel/Output/Boiler_Generator_Mapping_Table.csv",row.names = F)
  
  # #check if boiler.IDs NOT in association table
  # Boiler.ID_NotAssoc<-data.frame(Plant.Code=integer(0), Boiler.ID = character(0))
  # for (p in unique(bga.out$Plant.Code)){
  #   dfSub<-bga.out[bga.out$Plant.Code==p,]
  #   assSub<-bogen[bogen$Plant.Code==p,]
  #   testBoiler<-unique(dfSub$Boiler.ID)
  #   testBoiler<-testBoiler[!testBoiler %in% assSub$Boiler.ID]
  #   if (length(testBoiler)!=0){
  #     testBoiler<-data.frame(Plant.Code=rep(p,length(testBoiler)),Boiler.ID = testBoiler)
  #   Boiler.ID_NotAssoc<-rbind(Boiler.ID_NotAssoc,testBoiler)
  #   }
  # }
  
  #print(Boiler.ID_NotAssoc)
  Fill.Gen.ID=T
  
  #If option to fill 
  if(Fill.Gen.ID){
    #added by LIly
    bga.out$bga.source[is.na(bga.out$Boiler.ID)] <- "fillwithGenID"
    bga.out$Boiler.ID[is.na(bga.out$Boiler.ID)] <- bga.out$Generator.ID[is.na(bga.out$Boiler.ID)]
  }
  
  

  #orphan Boilers
  Boiler.ID_NotAssoc<-anti_join(Boiler.ID_NotAssoc,bga.out,by=c("Plant.Code","Boiler.ID"))

  bga.out$Orphan.Boiler<-FALSE
  orphanBoilers<-Boiler.ID_NotAssoc %>% filter(!is.na(Boiler.ID_NotAssoc$Boiler.ID))
  orphanBoilers2<-merge(orphanBoilers[,!names(orphanBoilers) %in% names(bfd.923)[!names(bfd.923) %in% c("Plant.Code","Boiler.ID")]],
                        bfd.923, by=c("Plant.Code","Boiler.ID"), all=TRUE)
  orphanBoilers<-anti_join(orphanBoilers,orphanBoilers2, by=c("Plant.Code","Boiler.ID"))
  orphanBoilers<-rbind(orphanBoilers,orphanBoilers2)
 
  if (analysisYear>2005){
  fillOrphanBoilers<-bga.out[0,]
  for (o in unique(orphanBoilers$Plant.Code)){
    subBGA<-bga.out %>% filter(Plant.Code==o)
    subOrphan<-orphanBoilers %>% filter(Plant.Code==o) 
    subOrphan<-subOrphan %>% select(names(subOrphan)[names(subOrphan) %in% names(subBGA)])
                            
    for (b in 1:length(subOrphan$Boiler.ID)){
      subBoil<-subOrphan[b,]
      if (!is.na(subBoil$Reported.Prime.Mover_bf.923)){
      if (subBoil$Reported.Prime.Mover_bf.923=="CS"){
        if (subBoil$Boiler.ID %in% na.omit(bga.out$Generator.ID)){
          namevector<-names(subBGA)[!names(subBGA) %in% names(subBoil)]
          fillOut <- do.call("cbind", list(subBoil, rep(list(NA),length(namevector))))
          colnames(fillOut)[-1*(1:(ncol(fillOut) - length(namevector)))] <- namevector
          fillOut$Generator.ID<-fillOut$Boiler.ID
          fillOut$Orphan.Boiler<-TRUE
          fillOut<-fillOut %>% select(names(subBGA))
          fillOrphanBoilers<-rbind(fillOrphanBoilers,fillOut)
          
        }#if Boiler.ID in Generator.IDs
        
      }#if CS Prime Mover
      }#if !is.n Prime Mover
    }#for each Boiler.ID
  }#for each Plant in orphan Boilers


  
  bga.out<-anti_join(bga.out,fillOrphanBoilers,by=c("Plant.Code","Boiler.ID"))
  bga.out<-rbind(bga.out,fillOrphanBoilers)
  }#analysisYear>2005
  
  Boiler.ID_NotAssoc<-anti_join(Boiler.ID_NotAssoc,bga.out,by=c("Plant.Code","Boiler.ID"))
  Boiler.ID_NotAssoc<- Boiler.ID_NotAssoc %>% select(names(Boiler.ID_NotAssoc)[names(Boiler.ID_NotAssoc) 
                                                                               %in% names(bga.out)])

  
  
  #add Boiler.Status column
  boilerDesignData$flagBoiler.ID_boilerDesignData<-sapply(boilerDesignData$Boiler.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                  ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  boilerDesignData$Boiler.ID <- gsub("(?<![0-9])", "",boilerDesignData$Boiler.ID , perl = TRUE)
  boilerDesignData$Boiler.ID <- gsub("[[:space:]]", "",boilerDesignData$Boiler.ID)
  boilerDesignData<-removeLeadZero(boilerDesignData,"Boiler.ID","flagBoiler.ID_boilerDesignData")
  
  if (analysisYear<=2005){
    boilerDesignData$Retirement.Date<-as.POSIXct(boilerDesignData$Retirement.Date,format="%Y-%m-%d")
    boilerDesignData$Retirement.Year<-as.integer(format(boilerDesignData$Retirement.Date,format="%Y"))
    boilerDesignData$Retirement.Month<-as.integer(format(boilerDesignData$Retirement.Date,format="%m"))
  }
  boilerDesignData<-boilerDesignData %>% select("Plant.Code","Boiler.ID","Boiler.Status","Retirement.Year","Retirement.Month")
  names(boilerDesignData)<-c("Plant.Code","Boiler.ID","Boiler.Status","Boiler.Retirement.Year","Boiler.Retirement.Month")
  bga.out<-merge(bga.out,boilerDesignData,by=c("Plant.Code","Boiler.ID"),all.x=T)
  bga.out<-bga.out %>% mutate(flag_BoilerRetired = ifelse(Boiler.Retirement.Year<analysisYear & Boiler.Retirement.Year!=0,1,0))
  bga.out<-bga.out %>% mutate(flag_Boiler.Status = ifelse(Boiler.Status %in% c("OS","OA","RE") & is.na(orig_Boiler.ID_bf.923),1,0))
  
  #add flag_Generator.Status
  bga.out<-bga.out %>% mutate(flag_Generator.Status = ifelse(Generator.Status %in% c("OS","OA") & 
                                                               (is.na(orig_Generator.ID_923) | Net.Generation.mwh==0),1,0))
  
  #flag retired Generators
  retiredGenerators$flagGenerator.ID_retiredGenerators<-sapply(retiredGenerators$Generator.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                                                         ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  retiredGenerators$Generator.ID <- gsub("(?<![0-9])", "",retiredGenerators$Generator.ID , perl = TRUE)
  retiredGenerators$Generator.ID <- gsub("[[:space:]]", "",retiredGenerators$Generator.ID)
  retiredGenerators<-removeLeadZero(retiredGenerators,"Generator.ID","flagGenerator.ID_retiredGenerators")
  
  retiredGenerators<-retiredGenerators %>% select("Plant.Code","Generator.ID","Retirement.Year","Retirement.Month")
  names(retiredGenerators)<-c("Plant.Code","Generator.ID","Generator.Retirement.Year","Generator.Retirement.Month")
  bga.out<-merge(bga.out,retiredGenerators,by=c("Plant.Code","Generator.ID"),all.x=T)
  bga.out<-bga.out %>% mutate(flag_GeneratorRetired = ifelse(Generator.Retirement.Year<analysisYear & Generator.Retirement.Year!=0,1,0))
  
  
  #add flag Boiler.ID not is association table
  bga.out$Boiler.ID.NotIn.Assoc<-ifelse(is.na(bga.out$orig_Boiler.ID_assoc),1,0)
  
  #add flag missing plant from table
  bga.out<-merge(bga.out,missingPlants, by="Plant.Code",all.x=T)

  
  bga.out.for.associate <- bga.out %>% filter(!is.na(Plant.Code))
 # bga.out.for.associate<- bga.out.for.associate[!bga.out.for.associate$missing.from.923,]
 #save(bga.out.for.associate,file="D:/bga.out")
 # print("line238")
 # print(bga.out.for.associate %>% filter(Plant.Code==96))
 # print(unique(bga.out.for.associate[bga.out.for.associate$Plant.Code==96,]$Boiler.ID))
 #add by LIly
 #subset out prime movers
#  keepNames<-names(bga.out.for.associate)
#  boilerFuelData_newID<-boilerFuelData
#  boilerFuelData_newID$Boiler.ID <- gsub("(?<![0-9])0+", "", boilerFuelData_newID$Boiler.ID , perl = TRUE)
#  boilerFuelData_newID$Boiler.ID  <- gsub("[[:space:]]", "",boilerFuelData_newID$Boiler.ID )
#  boilerFuelData_newID$plant_bo<-paste0(boilerFuelData_newID$Plant.Code,"^",boilerFuelData_newID$Boiler.ID)
#  boilerFuelData_newID<-boilerFuelData_newID[c("Reported.Prime.Mover","Plant.Code","Boiler.ID")]
#  bga.out.for.associate2<-full_join_track(bga.out.for.associate,boilerFuelData_newID, by = c("Plant.Code","Boiler.ID"))
#  removed_frombga.out<-bga.out.for.associate2[is.na(bga.out.for.associate2$Reported.Prime.Mover),]
#  bga.out.for.associate<-bga.out.for.associate2[!is.na(bga.out.for.associate2$Reported.Prime.Mover),]
#  bga.out.for.associate<-bga.out.for.associate[,keepNames] 
#  bga.out.for.associate<-bga.out.for.associate[!duplicated(bga.out.for.associate),]
  bga.out.for.associate$Boiler.ID<-sapply(bga.out.for.associate$Boiler.ID, function(x) ifelse(is.na(x),"NA",x))
# bga.out.for.associate$Generator.ID<-sapply(bga.out.for.associate$Generator.ID, function(x) ifelse(is.na(x),"NA",x))
#  save(bga.out.for.associate,file="D:/bga.out2")
 
 #add by Lily
 bga.out.for.associate.sort<-bga.out.for.associate[c(1:3)]
 bga.out.for.associate.sort<-bga.out.for.associate.sort %>%
 #  arrange(Plant.Code,nchar(Boiler.ID),Boiler.ID,nchar(Generator.ID),Generator.ID)
   arrange(Plant.Code,nchar(Boiler.ID),Boiler.ID,nchar(Generator.ID),Generator.ID)
  # arrange(Plant.Code,nchar(Boiler.ID),Boiler.ID)
 #save(bga.out.for.associate.sort,file="D:/bga.out.sort")
 # print("line264")
 # print(bga.out.for.associate.sort %>% filter(Plant.Code==96))
 # print(unique(bga.out.for.associate.sort[bga.out.for.associate.sort$Plant.Code==96,]$Boiler.ID))
 bga.out.for.associate.sort<-bga.out.for.associate.sort[!duplicated(bga.out.for.associate.sort),]
 
 #added by Lily
 #flag floating generators
 # if gen.id %in% boil.id AND no row exists with gen.id=boil.id
 checkPlants<-integer(0)
 for (p in unique(bga.out.for.associate.sort$Plant.Code)){
   dfSub<-bga.out.for.associate.sort[bga.out.for.associate.sort$Plant.Code==p,c("Boiler.ID","Generator.ID")]
   testGen<-unique(dfSub$Generator.ID)
   for (g in testGen){
     if (g %in% unique(dfSub$Boiler.ID)){
       bSub<-dfSub[dfSub$Boiler.ID==g & dfSub$Generator.ID==g,]
       if (nrow(bSub)==0){#floating generator
         checkPlants<-c(checkPlants,p)
         bga.out.for.associate.sort[bga.out.for.associate.sort$Plant.Code==p & 
                                      bga.out.for.associate.sort$Generator.ID==g,]$Generator.ID<-paste0(bga.out.for.associate.sort[bga.out.for.associate.sort$Plant.Code==p & 
                                                                                                                                     bga.out.for.associate.sort$Generator.ID==g,]$Generator.ID,"_floatingGen")
       }#if floating
     }#if g in Boiler.IDs
   }#for each gen
 }#for each plant
 checkPlants<-unique(checkPlants)
 

 
 bga.out.for.associate.sort<-bga.out.for.associate.sort[!is.na(bga.out.for.associate.sort$Generator.ID),]
 
  #bga.out.for.associate.sort<-bga.out.for.associate.sort[regexpr("GT",bga.out.for.associate.sort$Boiler.ID)>0,]
 # 
  #This argument is passed into the function giving the user the option to graph plant bogen network.
  vis_out=F
  # print(bga.out.for.associate.sort %>% filter(Plant.Code==688))
  #Generates concatenated bogen code used for summaries according to Bogen.
 #out <- associate(bga.out.for.associate[c(1:3)],save_image = vis_out)
  #added by Lily
  out <- associate(bga.out.for.associate.sort,save_image = vis_out)
  
  #Merge the Bogen code with the Bogen associations data frame to create Bogen key used in 
  #analysis
  data.out <- left_join(out,bga.out)
  #data.out <- na.omit(data.out[4:10])
  
  #added by LIly
  if (analysisYear>2005){
   data.out <- data.out %>% filter(Reported.Prime.Mover_860 %in% na.omit(select_RPM) |
                                  Reported.Prime.Mover_923 %in% na.omit(select_RPM) |
                                  Reported.Prime.Mover_bf.923 %in% na.omit(select_RPM))
  na.Boiler.ID <- na.Boiler.ID %>% filter(Reported.Prime.Mover_860 %in% na.omit(select_RPM) |
                                    Reported.Prime.Mover_923 %in% na.omit(select_RPM) |
                                    Reported.Prime.Mover_bf.923 %in% na.omit(select_RPM)) 
  }else{
    data.out <- data.out %>% filter(Reported.Prime.Mover_860 %in% na.omit(select_RPM))
    na.Boiler.ID <- na.Boiler.ID %>% filter(Reported.Prime.Mover_860 %in% na.omit(select_RPM)) 
  }
  assign("data.out882",data.out,envir = .GlobalEnv)
 
  #test for missing CS/CT data in Page3 or Page4
  gen_fuel_data<-gen_fuel_data %>% filter(Plant.Code %in% plantList)
  gen_fuel_data<-gen_fuel_data %>% filter(Reported.Prime.Mover %in% na.omit(select_RPM))
  #gen_fuel_data<- gen_fuel_data %>% filter(!Plant.Code %in% nukePlants$Plant.Code)
  gen_fuel_data<-gen_fuel_data %>% filter((is.na(Nuclear.Unit.Id) | Nuclear.Unit.Id=="."))
  decembrist<-checkDecember(FuelHeatByLine=bf.923.ungrouped,NetGenByLine=generation.data.line124,
                            gen_fuel_data,plantList,select_RPM,months)
  missingCSCT<-checkMissingCSCT(gen_923, bf.923.ungrouped,gen_fuel_data, analysisYear)
  #set NGCC bogen for plants missing data on Page1
  if (analysisYear==2014){
  anyMissingNGCC<-checkANYmissingNGCC(generation.data.line124, boilerFuelData,gen_fuel_data, analysisYear)
  }else{
    anyMissingNGCC<-numeric(0)
  }
  #remove if missingCSCT
  data.out<-data.out %>% filter(!(Plant.Code %in% missingCSCT$Plant.Code & 
                                    (Reported.Prime.Mover_860 %in% c("CS","CT","CA")| 
                                       Reported.Prime.Mover_923 %in% c("CS","CT","CA") | 
                                       Reported.Prime.Mover_bf.923 %in% c("CS","CT","CA"))))
  #save gen_fuel_data
  gen_fuel_data_all<-gen_fuel_data
  
  #Calculate Fuel.Heat and NetGen from Page 1 for missingCSCT
  gen_fuel_data<-gen_fuel_data %>% filter(Plant.Code %in% missingCSCT$Plant.Code)
  gen_fuel_data<-gen_fuel_data %>% filter(Reported.Prime.Mover %in% c("CS","CT","CA"))
  
  #get NGCC sums
  gen_fuel_data<-sumGenFuelData(gen_fuel_data,bf.923.ungrouped, generation.data.line124,months,analysisYear)
  
  #add plant-level Bogen
  gen_fuel_data$Bogen<-rep("",nrow(gen_fuel_data))
  for (p in unique(gen_fuel_data$Plant.Code)){
    gen_fuel_data[which(gen_fuel_data$Plant.Code==p),]$Bogen<-paste0(p,"^","NGCC")
  }
  
  data.out<-rbind.fill(data.out,gen_fuel_data)
  data.out<-data.out[order(data.out$Plant.Code,data.out$Bogen),]
  
  #decembrists sums
  #remove if decembrist
  data.out<-data.out %>% filter(!Plant.Code %in% decembrist)
  gen_fuel_data<-gen_fuel_data_all
  gen_fuel_data<-gen_fuel_data %>% filter(Plant.Code %in% decembrist)
  gen_fuel_data<-gen_fuel_data %>% filter(Reported.Prime.Mover %in% na.omit(select_RPM))
  gen_fuel_data$Reported.Prime.Mover<-ifelse(gen_fuel_data$Reported.Prime.Mover %in% c("CS","CT","CA"),"NGCC",
                                             gen_fuel_data$Reported.Prime.Mover)
  gen_fuel_data<-sumGenFuelData(gen_fuel_data,bf.923.ungrouped, generation.data.line124,months,analysisYear)
  
  #add RPM-level Bogen
  gen_fuel_data$Bogen<-rep("",nrow(gen_fuel_data))
  for (p in unique(gen_fuel_data$Plant.Code)){
    gen_fuel_data[which(gen_fuel_data$Plant.Code==p),]$Bogen<-paste0(p,"^",
                              gen_fuel_data[which(gen_fuel_data$Plant.Code==p),]$Reported.Prime.Mover_page1)
  }
  
  data.out<-rbind.fill(data.out,gen_fuel_data)
  data.out<-data.out[order(data.out$Plant.Code,data.out$Bogen),]
  
  

  #remove if anyMissingNGCC
  data.out<-data.out %>% filter(!(Plant.Code %in% anyMissingNGCC & 
                                    (Reported.Prime.Mover_860 %in% c("CS","CT","CA")| 
                                       Reported.Prime.Mover_923 %in% c("CS","CT","CA") | 
                                       Reported.Prime.Mover_bf.923 %in% c("CS","CT","CA"))))
  #save gen_fuel_data
  gen_fuel_data<-gen_fuel_data_all
  
  #Calculate Fuel.Heat and NetGen from Page 1 for missingCSCT
  gen_fuel_data<-gen_fuel_data %>% filter(Plant.Code %in% anyMissingNGCC)
  gen_fuel_data<-gen_fuel_data %>% filter(Reported.Prime.Mover %in% c("CS","CT","CA"))
  
  #get NGCC sums
  gen_fuel_data<-sumGenFuelData(gen_fuel_data,bf.923.ungrouped, generation.data.line124,months,analysisYear)
  
  #add plant-level Bogen
  gen_fuel_data$Bogen<-rep("",nrow(gen_fuel_data))
  for (p in unique(gen_fuel_data$Plant.Code)){
    gen_fuel_data[which(gen_fuel_data$Plant.Code==p),]$Bogen<-paste0(p,"^","NGCC")
  }
  
  data.out<-rbind.fill(data.out,gen_fuel_data)
  data.out<-data.out[order(data.out$Plant.Code,data.out$Bogen),]
  
  
  assign("data.out925",data.out,envir = .GlobalEnv)
  
  

  assign("data.out888",data.out,envir = .GlobalEnv)
  
  
  #flag missing from ST and BoilerFuelData.923
  data.out$missing.bf.923<-ifelse(is.na(data.out$orig_Boiler.ID_bf.923) & 
                                    (is.na(data.out$missing.bf.923) | data.out$missing.bf.923==TRUE) ,TRUE,FALSE)
  if (analysisYear>2005){
    data.out<-data.out %>% mutate(flag.ST.missing.bf.923=ifelse((Reported.Prime.Mover_923=="ST" | 
                                                                 Reported.Prime.Mover_860=="ST") &
                                                                missing.bf.923==TRUE,1,0))
  }else{
    data.out<-data.out %>% mutate(flag.ST.missing.bf.923=ifelse((Reported.Prime.Mover_860=="ST") &
                                                                  missing.bf.923==TRUE,1,0))
    }
  
  assign("data.out630",data.out,envir = .GlobalEnv)
 
   #remove if missing.from.923
  data.out<-data.out %>% filter((missing.from.923==FALSE | is.na(missing.from.923)) | regexpr("NGCC",Bogen)>0 | regexpr("ST",Bogen)<0)
  data.out$missing.from.923<-ifelse(is.na(data.out$missing.from.923) & regexpr("NGCC",data.out$Bogen)<0 & regexpr("ST",data.out$Bogen)<0,
                                    FALSE,data.out$missing.from.923)
  #remove if flag_Boiler.Status
  #data.out<-data.out %>% filter(flag_Boiler.Status!=1)
  #remove if flag.ST.missing.bf.923
  #data.out<-data.out %>% filter(!missing.bf.923)
  assign("data.out963",data.out,envir = .GlobalEnv)
  #check if any bogen CA without CT
  data.out$flag.CACT.missingPair<-FALSE
  data.out$flag_923bogenCTonly<-FALSE
  for (b in as.character(unique(data.out[regexpr("NGCC",data.out$Bogen)<0 & regexpr("ST",data.out$Bogen)<0,]$Bogen))){
    subData<-data.out %>% filter(Bogen==b)
    if (analysisYear>2005){
      allRPMs<-as.character(unique(c(subData$Reported.Prime.Mover_860,
                      subData$Reported.Prime.Mover_923,
                      subData$Reported.Prime.Mover_bf.923,
                      subData$Reported.Prime.Mover_page1)))
    }else{
      allRPMs<-as.character(unique(c(subData$Reported.Prime.Mover_860,
                                     subData$Reported.Prime.Mover_page1)))
    }
    
    if (analysisYear>2005){
      RPMs.923<-as.character(unique(c(subData$Reported.Prime.Mover_923,
                                   subData$Reported.Prime.Mover_bf.923,
                                   subData$Reported.Prime.Mover_page1)))
    }
   
    if ("CT" %in% allRPMs & !"CA" %in% allRPMs){
      data.out[data.out$Bogen==b,]$flag.CACT.missingPair<-rep(TRUE,nrow(data.out[data.out$Bogen==b,]))
    }else if ("CA" %in% allRPMs & !"CT" %in% allRPMs){
      data.out[data.out$Bogen==b,]$flag.CACT.missingPair<-rep(TRUE,nrow(data.out[data.out$Bogen==b,]))
    }
   
   #check if ONLY 'CT' in923 RPMS
    if(analysisYear>2005){
   if (length(unique(RPMs.923))==1 & !identical(unique(RPMs.923),NA)){
     if(identical(as.character(unique(RPMs.923)),"CT")){
       data.out[data.out$Bogen==b,]$flag_923bogenCTonly<-rep(TRUE,nrow(data.out[data.out$Bogen==b,]))
     }
   }
    }#analysisYear>2005
  }
 
 
  badFuels<-checkBadFuel(data.out,analysisYear)
   data.out$flag.NGCC.badFuel<-ifelse(data.out$Plant.Code %in% badFuels,1,0)
  
  #count rows per Plant
  countRows<-data.out %>% group_by(Plant.Code) %>% count(Plant.Code)
  data.out<-merge(data.out,countRows, by="Plant.Code")
  
  #flag for removal if flag.ST.missing.bf.923 & countRows!=1 | unmapped!=TRUE
  data.out$flag.ST.missing.bf.923.unmapped<-ifelse(data.out$flag.ST.missing.bf.923==1 & 
                                                     (data.out$n!=1 | data.out$unmapped!=TRUE) & 
                                                     regexpr("NGCC",data.out$Bogen)<0 & 
                                                     regexpr("ST",data.out$Bogen)<0,1,0)
  data.out<-data.out %>% select(-n)
  assign("data.out1011",data.out,envir = .GlobalEnv)
  #add inconsistent RPM flag
  if (analysisYear>2005){
  data.out<-merge(data.out, compare_RPM %>% select(Plant.Code,Generator.ID,flag_RPM), 
                  by=c("Plant.Code","Generator.ID"), all.x=TRUE)
  }
  
  #add floating generator flag
  data.out<-data.out %>% mutate(flag_floatingGen=ifelse(Plant.Code %in% checkPlants,1,0))
  
   #Additional Fuel Heat Calculations added 9.29.21
  #Sum by Bogen
  uniqueFuelHeat<-data.out %>% select(c("Plant.Code","Boiler.ID","Bogen"),contains("Fuel.Heat"))
  uniqueFuelHeat<-uniqueFuelHeat[!duplicated(uniqueFuelHeat),]
  
  for(m in months){
    fuelHeatstr<-paste0("uniqueFuelHeat<-uniqueFuelHeat %>% mutate(Boiler.ID.Total.Fuel.Heat.",m," = ifelse(regexpr('NGCC',Bogen)>0 | regexpr('ST',Bogen)>0,
                                                            PlantLine.Total.Fuel.Heat.",m,",Boiler.ID.Total.Fuel.Heat.",m,"))")
    eval(parse(text=fuelHeatstr))
    fuelHeatstr<-paste0("sumbyBogen<-uniqueFuelHeat %>% group_by(Plant.Code,Bogen) %>% 
                        summarize(Bogen.Total.Fuel.Heat.",m,"=sum(Boiler.ID.Total.Fuel.Heat.",m,",na.rm=T))")
    eval(parse(text=fuelHeatstr))
    if (m=="January"){
      sumFuelHeatByBogen<-sumbyBogen  
    }else{
      
      sumFuelHeatByBogen<-merge(sumFuelHeatByBogen,sumbyBogen,by=c("Plant.Code","Bogen"),all=T)
    }
    
  }
  assign("sumFuelHeatByBogen",sumFuelHeatByBogen,envir = .GlobalEnv)
  #Sum by Plant by summing Bogens
  for(m in months){
    fuelHeatstr<-paste0("sumbyPlant<-sumFuelHeatByBogen %>% group_by(Plant.Code) %>% 
                        summarize(Plant.Total.Fuel.Heat.",m,"=sum(Bogen.Total.Fuel.Heat.",m,",na.rm=T))")
    eval(parse(text=fuelHeatstr))
    if (m=="January"){
      sumFuelHeatBogensByPlant<-sumbyPlant  
    }else{
      
      sumFuelHeatBogensByPlant<-merge(sumFuelHeatBogensByPlant,sumbyPlant,by=c("Plant.Code"),all=T)
    }
    
  }
  assign("sumFuelHeatBogensByPlant",sumFuelHeatBogensByPlant,envir = .GlobalEnv)
  
  ###########################
  #Additional NetGen Calculations added 3.1.22
  #Sum by Bogen
  uniqueNetGen<-data.out %>% select(c("Plant.Code","Generator.ID","Bogen"),
                                    contains("Generator.ID.Total.Net.Generation"),
                                    contains("PlantLine.Total.Net.Generation."))
  uniqueNetGen<-uniqueNetGen[!duplicated(uniqueNetGen),]
  for(m in months){
    netGenstr<-paste0("uniqueNetGen<-uniqueNetGen %>% mutate(Generator.ID.Total.Net.Generation.",m," = ifelse(regexpr('NGCC',Bogen)>0 | regexpr('ST',Bogen)>0,
                                                            PlantLine.Total.Net.Generation.",m,",Generator.ID.Total.Net.Generation.",m,"))")
    eval(parse(text=netGenstr))
    netGenstr<-paste0("sumbyBogen<-uniqueNetGen %>% group_by(Plant.Code,Bogen) %>% 
                        summarize(Bogen.Total.Net.Generation.",m,"=sum(Generator.ID.Total.Net.Generation.",m,",na.rm=T))")
    eval(parse(text=netGenstr))
    if (m=="January"){
      sumNetGenByBogen<-sumbyBogen  
    }else{
      
      sumNetGenByBogen<-merge(sumNetGenByBogen,sumbyBogen,by=c("Plant.Code","Bogen"),all=T)
    }
    
  }
  assign("sumNetGenByBogen",sumNetGenByBogen,envir = .GlobalEnv)
  #Sum by Plant by summing Bogens
  for(m in months){
    netGenstr<-paste0("sumbyPlant<-sumNetGenByBogen %>% group_by(Plant.Code) %>% 
                        summarize(Plant.Total.Net.Generation.",m,"=sum(Bogen.Total.Net.Generation.",m,",na.rm=T))")
    eval(parse(text=netGenstr))
    if (m=="January"){
      sumNetGenBogensByPlant<-sumbyPlant  
    }else{
      
      sumNetGenBogensByPlant<-merge(sumNetGenBogensByPlant,sumbyPlant,by=c("Plant.Code"),all=T)
    }
    
  }
  assign("sumNetGenBogensByPlant",sumNetGenBogensByPlant,envir = .GlobalEnv)
  
  #thermal Effficiency
 thermalEffByBogen<-inner_join(sumNetGenByBogen,sumFuelHeatByBogen)
 for (m in months){
   eval(parse(text=paste0("thermalEffByBogen<-thermalEffByBogen %>% mutate(ThermalEfficiency.",m,"=3.41214*Bogen.Total.Net.Generation.",m,"/
                       Bogen.Total.Fuel.Heat.",m,")")))
   eval(parse(text=paste0("thermalEffByBogen<-thermalEffByBogen %>% mutate(ThermalEfficiency.",m,"=ifelse(Bogen.Total.Net.Generation.",m,"
                       <=0 & Bogen.Total.Fuel.Heat.",m,"<=0,0,ifelse(Bogen.Total.Net.Generation.",m,">0 & Bogen.Total.Fuel.Heat.",m,"<=0,
                          9999,ThermalEfficiency.",m,")))")))
   
 }
 assign("thermalEffByBogen",thermalEffByBogen,envir = .GlobalEnv)
 
  ############################
  
  #flag PLant Level NetGen>0 and FuelHet<=0 monthly
  compareNetGenFuelHeat<-merge(sumNetGenBogensByPlant,sumFuelHeatBogensByPlant,by="Plant.Code")
  compareNetGenFuelHeat$flagNetGenWithoutFuelHeat<-rep(0,nrow(compareNetGenFuelHeat))
  for(m in months){

    flagstr<-paste0("compareNetGenFuelHeat<-compareNetGenFuelHeat %>% mutate(flagNetGenWithoutFuelHeat=ifelse(
                        Plant.Total.Fuel.Heat.",m,"<=0 & Plant.Total.Net.Generation.",m,">0,
                    flagNetGenWithoutFuelHeat+1,flagNetGenWithoutFuelHeat))")
    eval(parse(text=flagstr))  
    flagstr<-paste0("compareNetGenFuelHeat<-compareNetGenFuelHeat %>%
                    mutate(flagNG_no_FH_",m,"=ifelse(Plant.Total.Fuel.Heat.",m,"<=0 & 
                    Plant.Total.Net.Generation.",m,">0,1,0))")
    eval(parse(text=flagstr))    
  }
  assign("compareNetGenFuelHeat2",compareNetGenFuelHeat,envir = .GlobalEnv)
  #add flagNetGenWithoutFuelHeat flag
  data.out<-merge(data.out, compareNetGenFuelHeat %>% select(Plant.Code,
                                                             flagNetGenWithoutFuelHeat,
                                                             contains("flagNG_no_FH_")), 
                  by=c("Plant.Code"), all.x=TRUE)
  
  #flag Bogen Level NetGen<0 and FuelHet>0 monthly
  compareNetGenFuelHeat<-merge(sumNetGenByBogen,sumFuelHeatByBogen,by=c("Plant.Code","Bogen"))
  compareNetGenFuelHeat$flagFuelHeatWithoutNetGen<-rep(0,nrow(compareNetGenFuelHeat))
  for(m in months){
    flagstr<-paste0("compareNetGenFuelHeat<-compareNetGenFuelHeat %>% mutate(flagFuelHeatWithoutNetGen=ifelse(
                        Bogen.Total.Fuel.Heat.",m,">0 & Bogen.Total.Net.Generation.",m,"<0,flagFuelHeatWithoutNetGen+1,flagFuelHeatWithoutNetGen))")
    eval(parse(text=flagstr))
    flagstr<-paste0("compareNetGenFuelHeat<-compareNetGenFuelHeat %>%
                    mutate(flagFH_no_NG_",m,"=ifelse(Bogen.Total.Fuel.Heat.",m,">0 & 
                    Bogen.Total.Net.Generation.",m,"<0,1,0))")
    eval(parse(text=flagstr)) 
  }
  assign("compareNetGenFuelHeat",compareNetGenFuelHeat,envir = .GlobalEnv)
  #add flagNetGenWithoutFuelHeat flag
  data.out<-merge(data.out, compareNetGenFuelHeat %>% select(Plant.Code,Bogen,
                                                             flagFuelHeatWithoutNetGen, 
                                                             contains("flagFH_no_NG_")), 
                  by=c("Plant.Code","Bogen"), all.x=TRUE)
  
  assign("data.out1149",data.out,envir = .GlobalEnv)
  #compare sumFuelHeatBogensByPlant and sumFuelHeatLinesByPlant -> Manual Flag
  compareTotFuelHeat<-merge(sumFuelHeatBogensByPlant,sumFuelHeatLinesByPlant, by="Plant.Code")
  compareTotFuelHeat$flagFuelHeat<-rep(0,nrow(compareTotFuelHeat))
  for(m in months){
    fuelHeatstr<-paste0("compareTotFuelHeat<-compareTotFuelHeat %>% mutate(flagFuelHeat=ifelse(
                        round(Plant.Total.Fuel.Heat.",m,",3)==round(PlantLine.Total.Fuel.Heat.",m,",3),
                        flagFuelHeat,flagFuelHeat+1))")
    eval(parse(text=fuelHeatstr))
    fuelHeatstr<-paste0("compareTotFuelHeat<-compareTotFuelHeat %>% mutate(flagFuelHeat_",m,"=ifelse(
                        round(Plant.Total.Fuel.Heat.",m,",3)==round(PlantLine.Total.Fuel.Heat.",m,",3),
                        0,1))")
    eval(parse(text=fuelHeatstr))
  }
  assign("compareTotFuelHeat",compareTotFuelHeat,envir = .GlobalEnv) 
  
  #add flagFuelHeat flag
 data.out<-merge(data.out, compareTotFuelHeat %>% select(Plant.Code,contains("flagFuelHeat")), 
                              by=c("Plant.Code"), all.x=TRUE)
 
 #flag ST associated with CT,CS,CA
 data.out$flag_STassoc_CT_CS_CA<-rep(0,nrow(data.out))
 dataFlag.out<-data.out[0,]
 for (b in unique(data.out$Bogen)){
   subdata.out<-data.out %>% filter(Bogen==b)
   if (analysisYear>2005){
     allRPMs<-as.character(unique(c(subdata.out$Reported.Prime.Mover_860,
                                    subdata.out$Reported.Prime.Mover_923,
                                    subdata.out$Reported.Prime.Mover_bf.923,
                                    subdata.out$Reported.Prime.Mover_page1)))
   }else{
     allRPMs<-c(unique(subdata.out$Reported.Prime.Mover_860,
                       subdata.out$Reported.Prime.Mover_page1))
   }
   if ("ST" %in% allRPMs &
       ("CT" %in% allRPMs |
        "CS" %in% allRPMs |
        "CA" %in% allRPMs
        )){
     subdata.out$flag_STassoc_CT_CS_CA<-rep(1,nrow(subdata.out))
     
   }
   dataFlag.out<-rbind(dataFlag.out,subdata.out)
 }
 data.out<-dataFlag.out
 assign("data.out1194",data.out,envir = .GlobalEnv)
 #flag CS associated anything
 data.out$flag_CSassociation<-rep(0,nrow(data.out))
 dataFlag.out<-data.out[0,]
 for (b in unique(data.out$Bogen)){
   subdata.out<-data.out %>% filter(Bogen==b)
   if (analysisYear>2005){
     allRPMs<-as.character(unique(c(subdata.out$Reported.Prime.Mover_860,
                       subdata.out$Reported.Prime.Mover_923,
                       subdata.out$Reported.Prime.Mover_bf.923,
                       subdata.out$Reported.Prime.Mover_page1)))
   }else{
     allRPMs<-c(unique(subdata.out$Reported.Prime.Mover_860,
                       subdata.out$Reported.Prime.Mover_page1))
   }
   if ("CS" %in% allRPMs &
       length(unique(na.omit(allRPMs)))!=1
       ){
     subdata.out$flag_CSassociation<-rep(1,nrow(subdata.out))
     
   }
   dataFlag.out<-rbind(dataFlag.out,subdata.out)
 }
 data.out<-dataFlag.out
 
 assign("data.out",data.out,envir = .GlobalEnv)
  #add countFlags column
  countFlagfunc<-function(f){
    if (!is.na(f)){
    if(f==TRUE | f>=1 | f %in% c("OS","OA","RE")){
      c<-1
    }else{#no flag
      c<-0
    }
    }else{#missing flag
      c<-0
    }
    return(c)
  }
  
  if (analysisYear>2005){
    countFlagstr<-"data.out<-data.out %>% rowwise() %>% mutate(countFlags = sum(countFlagfunc(missing.from.923),
                                                 countFlagfunc(Generator.Status),
                                                 countFlagfunc(Orphan.Generator),
                                                 countFlagfunc(unmapped_but_in_923),
                                                 countFlagfunc(unmapped),
                                                 countFlagfunc(Orphan.Boiler),
                                                 countFlagfunc(Boiler.Status),
                                                 countFlagfunc(Boiler.ID.NotIn.Assoc),
                                                 countFlagfunc(missingPlant.assoc),
                                                 countFlagfunc(missingPlant.generator.data.860),
                                                 countFlagfunc(missingPlant.boilerDesignData.860),
                                                 countFlagfunc(missingPlant.generation.data.923),
                                                 countFlagfunc(missingPlant.boilerFuelData.923),
                                                 countFlagfunc(flag.CACT.missingPair),
                                                 countFlagfunc(missing.bf.923),
                                                 countFlagfunc(flag.ST.missing.bf.923),
                                                 countFlagfunc(flag.ST.missing.bf.923.unmapped),
                                                 countFlagfunc(flag_RPM),
                                                 countFlagfunc(flag_floatingGen),
                                                 countFlagfunc(flag_BoilerRetired),
                                                 countFlagfunc(flag_Boiler.Status),
                                                 countFlagfunc(flag_GeneratorRetired),
                                                 countFlagfunc(flag_Generator.Status),
                                                 countFlagfunc(flag_923bogenCTonly),
                                                 countFlagfunc(flagFuelHeat),
                                                 countFlagfunc(flag_STassoc_CT_CS_CA),
                                                 countFlagfunc(flag_CSassociation),
                                                 countFlagfunc(flagNetGenWithoutFuelHeat),
                                                 countFlagfunc(flagFuelHeatWithoutNetGen),
                                                 countFlagfunc(flag.NGCC.badFuel),"
  }else{
  countFlagstr<-"data.out<-data.out %>% rowwise() %>% mutate(countFlags = sum(countFlagfunc(missing.from.923),
                                                 countFlagfunc(Generator.Status),
                                                 countFlagfunc(Orphan.Generator),
                                                 countFlagfunc(unmapped_but_in_923),
                                                 countFlagfunc(unmapped),
                                                 countFlagfunc(Orphan.Boiler),
                                                 countFlagfunc(Boiler.Status),
                                                 countFlagfunc(Boiler.ID.NotIn.Assoc),
                                                 countFlagfunc(missingPlant.assoc),
                                                 countFlagfunc(missingPlant.generator.data.860),
                                                 countFlagfunc(missingPlant.boilerDesignData.860),
                                                 countFlagfunc(missingPlant.generation.data.923),
                                                 countFlagfunc(missingPlant.boilerFuelData.923),
                                                 countFlagfunc(flag.CACT.missingPair),
                                                 countFlagfunc(missing.bf.923),
                                                 countFlagfunc(flag.ST.missing.bf.923),
                                                 countFlagfunc(flag.ST.missing.bf.923.unmapped),
                                                 countFlagfunc(flag_floatingGen),
                                                 countFlagfunc(flag_BoilerRetired),
                                                 countFlagfunc(flag_Boiler.Status),
                                                 countFlagfunc(flag_GeneratorRetired),
                                                 countFlagfunc(flag_Generator.Status),
                                                 countFlagfunc(flagFuelHeat),
                                                 countFlagfunc(flag_STassoc_CT_CS_CA),
                                                 countFlagfunc(flag_CSassociation),
                                                 countFlagfunc(flagNetGenWithoutFuelHeat),
                                                 countFlagfunc(flagFuelHeatWithoutNetGen),"
  }
  for (m in months){
    if (m!="December"){
    countFlagstr<-paste0(countFlagstr,"countFlagfunc(flagFH_no_NG_",m,"),
                                      countFlagfunc(flagNG_no_FH_",m,"),
                                      countFlagfunc(flagFuelHeat_",m,"),")
    }else{
      countFlagstr<-paste0(countFlagstr,"countFlagfunc(flagFH_no_NG_",m,"),
                                      countFlagfunc(flagNG_no_FH_",m,"),
                                      countFlagfunc(flagFuelHeat_",m,")))")
    }
  }
  eval(parse(text=countFlagstr))
    
  
                                                 
  
  assign("dataout708",data.out,envir = .GlobalEnv)
  #manual Plants
 # manualPlants<-data.frame(Plant.Code=checkPlants,floatingGenerator=rep(1,length(checkPlants)))
  flagCTCA_plants<-data.out %>% filter(flag.CACT.missingPair==1)
  flagCTCA_plants<-data.frame(Plant.Code = unique(flagCTCA_plants$Plant.Code),
                              flag.CACT.missingPair = rep(1,length(unique(flagCTCA_plants$Plant.Code))))
  #manualPlants<-merge(manualPlants,flagCTCA_plants,by="Plant.Code",all=T)
  manualPlants<-flagCTCA_plants
  
  # missing.bf.923_plants<-data.out %>% rowwise() %>% mutate( missing.bf.923 = countFlagfunc( missing.bf.923))
  # missing.bf.923_plants<-missing.bf.923_plants %>% filter( missing.bf.923==1)
  # missing.bf.923_plants<-missing.bf.923_plants %>% select(Plant.Code, missing.bf.923)
  # missing.bf.923_plants<-missing.bf.923_plants[!duplicated(missing.bf.923_plants),]
  # manualPlants<-merge(manualPlants,missing.bf.923_plants,by="Plant.Code",all=T)
  
  flag_923bogenCTonly_plants<-data.out %>% rowwise() %>% mutate( flag_923bogenCTonly = countFlagfunc( flag_923bogenCTonly))
  flag_923bogenCTonly_plants<-flag_923bogenCTonly_plants %>% filter( flag_923bogenCTonly==1)
  flag_923bogenCTonly_plants<-flag_923bogenCTonly_plants %>% select(Plant.Code, flag_923bogenCTonly)
  flag_923bogenCTonly_plants<-flag_923bogenCTonly_plants[!duplicated(flag_923bogenCTonly_plants),]
  manualPlants<-merge(manualPlants,flag_923bogenCTonly_plants,by="Plant.Code",all=T)
  
  flag_NG_no_FH_plants<-data.out %>% rowwise() %>% mutate( flagNetGenWithoutFuelHeat = countFlagfunc( flagNetGenWithoutFuelHeat))
  flag_NG_no_FH_plants<-flag_NG_no_FH_plants %>% filter( flagNetGenWithoutFuelHeat==1)
  flag_NG_no_FH_plants<-flag_NG_no_FH_plants %>% select(Plant.Code, flagNetGenWithoutFuelHeat)
  flag_NG_no_FH_plants<-flag_NG_no_FH_plants[!duplicated(flag_NG_no_FH_plants),]
  manualPlants<-merge(manualPlants,flag_NG_no_FH_plants,by="Plant.Code",all=T)
  
  # flag_FH_no_NG_plants<-data.out %>% rowwise() %>% mutate( flagFuelHeatWithoutNetGen = countFlagfunc( flagFuelHeatWithoutNetGen))
  # assign("flag_FH_no_NG_plants",flag_FH_no_NG_plants,envir = .GlobalEnv)
  # flag_FH_no_NG_plants<-flag_FH_no_NG_plants %>% filter( flagFuelHeatWithoutNetGen==1)
  # flag_FH_no_NG_plants<-flag_FH_no_NG_plants %>% select(Plant.Code, flagFuelHeatWithoutNetGen)
  # flag_FH_no_NG_plants<-flag_FH_no_NG_plants[!duplicated(flag_FH_no_NG_plants),]
  # manualPlants<-merge(manualPlants,flag_FH_no_NG_plants,by="Plant.Code",all=T)
  
  data.out<-data.out %>% rowwise() %>% mutate(flagFuelHeat = ifelse(regexpr("NGCC",Bogen)>0 | regexpr("ST",Bogen)>0,0,flagFuelHeat))
  flag_fuelheat_plants<-data.out %>% rowwise() %>% mutate( flagFuelHeat = countFlagfunc( flagFuelHeat))
  flag_fuelheat_plants<-flag_fuelheat_plants %>% filter( flagFuelHeat==1)
  flag_fuelheat_plants<-flag_fuelheat_plants %>% select(Plant.Code, flagFuelHeat)
  flag_fuelheat_plants<-flag_fuelheat_plants[!duplicated(flag_fuelheat_plants),]
  manualPlants<-merge(manualPlants,flag_fuelheat_plants,by="Plant.Code",all=T)
  
  # Boiler.Status_plants<-data.out %>% rowwise() %>% mutate(Boiler.Status = countFlagfunc(Boiler.Status))
  # Boiler.Status_plants<-Boiler.Status_plants %>% filter(Boiler.Status==1)
  # Boiler.Status_plants<-Boiler.Status_plants %>% select(Plant.Code,Boiler.Status)
  # Boiler.Status_plants<-Boiler.Status_plants[!duplicated(Boiler.Status_plants),]
  # manualPlants<-merge(manualPlants,Boiler.Status_plants,by="Plant.Code",all=T)
  # 
  # flag_RPM_plants<-data.out %>% rowwise() %>% mutate(flag_RPM = countFlagfunc(flag_RPM))
  # flag_RPM_plants<-flag_RPM_plants %>% filter(flag_RPM==1)
  # flag_RPM_plants<-flag_RPM_plants %>% select(Plant.Code,flag_RPM)
  # flag_RPM_plants<-flag_RPM_plants[!duplicated(flag_RPM_plants),]
  # manualPlants<-merge(manualPlants,flag_RPM_plants,by="Plant.Code",all=T)
  # 
  
  # missingPlant.Tables_plants<-data.out %>% rowwise() %>% mutate(missingPlant.Tables = sum(countFlagfunc(missingPlant.assoc),
  #                                                                                                           countFlagfunc(missingPlant.generator.data.860),
  #                                                                                                           countFlagfunc(missingPlant.generation.data.923),
  #                                                                                                           countFlagfunc(missingPlant.boilerDesignData.860),
  #                                                                                                           countFlagfunc(missingPlant.boilerFuelData.923)))
  # missingPlant.Tables_plants<-missingPlant.Tables_plants %>% filter(missingPlant.Tables>1)
  # missingPlant.Tables_plants<-missingPlant.Tables_plants %>% select(Plant.Code,missingPlant.Tables)
  # missingPlant.Tables_plants<-missingPlant.Tables_plants[!duplicated(missingPlant.Tables_plants),]
  # manualPlants<-merge(manualPlants,missingPlant.Tables_plants,by="Plant.Code",all=T)
  
  data.out<-data.out %>% rowwise() %>% mutate(missingPlant.assoc = ifelse(regexpr("NGCC",Bogen)>0 | regexpr("ST",Bogen)>0,
                                                                                           FALSE,missingPlant.assoc))
  missingPlant.assoc_plants<-data.out %>% rowwise() %>% mutate(missingPlant.assoc = countFlagfunc(missingPlant.assoc))
  missingPlant.assoc_plants<-missingPlant.assoc_plants %>% filter(missingPlant.assoc==1)
  missingPlant.assoc_plants<-missingPlant.assoc_plants %>% select(Plant.Code,missingPlant.assoc)
  missingPlant.assoc_plants<-missingPlant.assoc_plants[!duplicated(missingPlant.assoc_plants),]
  manualPlants<-merge(manualPlants,missingPlant.assoc_plants,by="Plant.Code",all=T)
  
  #flag ST associated with CT,CS,CA
  flag_STassoc_CT_CS_CA_plants<-data.out %>% rowwise() %>% mutate(flag_STassoc_CT_CS_CA = countFlagfunc(flag_STassoc_CT_CS_CA))
  flag_STassoc_CT_CS_CA_plants<-flag_STassoc_CT_CS_CA_plants %>% filter(flag_STassoc_CT_CS_CA==1)
  flag_STassoc_CT_CS_CA_plants<-flag_STassoc_CT_CS_CA_plants %>% select(Plant.Code,flag_STassoc_CT_CS_CA)
  flag_STassoc_CT_CS_CA_plants<-flag_STassoc_CT_CS_CA_plants[!duplicated(flag_STassoc_CT_CS_CA_plants),]
  manualPlants<-merge(manualPlants,flag_STassoc_CT_CS_CA_plants,by="Plant.Code",all=T)
  
  
  #flag CS associated with any other RPM
  flag_CSassociation_plants<-data.out %>% rowwise() %>% mutate(flag_CSassociation = countFlagfunc(flag_CSassociation))
  flag_CSassociation_plants<-flag_CSassociation_plants %>% filter(flag_CSassociation==1)
  flag_CSassociation_plants<-flag_CSassociation_plants %>% select(Plant.Code,flag_CSassociation)
  flag_CSassociation_plants<-flag_CSassociation_plants[!duplicated(flag_CSassociation_plants),]
  manualPlants<-merge(manualPlants,flag_CSassociation_plants,by="Plant.Code",all=T)
  
  flag.NGCC.badFuel_plants<-data.out %>% rowwise() %>% mutate(flag.NGCC.badFuel = countFlagfunc(flag.NGCC.badFuel))
  flag.NGCC.badFuel_plants<-flag.NGCC.badFuel_plants %>% filter(flag.NGCC.badFuel==1)
  flag.NGCC.badFuel_plants<-flag.NGCC.badFuel_plants %>% select(Plant.Code,flag.NGCC.badFuel)
  flag.NGCC.badFuel_plants<-flag.NGCC.badFuel_plants[!duplicated(flag.NGCC.badFuel_plants),]
  manualPlants<-merge(manualPlants,flag.NGCC.badFuel_plants,by="Plant.Code",all=T)
  
  # Generator.Status_plants<-data.out %>% rowwise() %>% mutate(Generator.Status = countFlagfunc(Generator.Status))
  # Generator.Status_plants<-Generator.Status_plants %>% filter(Generator.Status==1)
  # Generator.Status_plants<-Generator.Status_plants %>% select(Plant.Code,Generator.Status)
  # Generator.Status_plants<-Generator.Status_plants[!duplicated(Generator.Status_plants),]
  # manualPlants<-merge(manualPlants,Generator.Status_plants,by="Plant.Code",all=T)

  #flag as manual plants in MPL not in data.out
  # checkMissingPlant<-data.frame(Plant.Code = plantList[!plantList %in% data.out$Plant.Code])
  # checkMissingPlant$flag_missingPlant<-rep(1,nrow(checkMissingPlant))
  # checkMissingPlant<-checkMissingPlant %>% filter(!Plant.Code %in% nukePlants$Plant.Code)
  # manualPlants<-merge(manualPlants,checkMissingPlant,by="Plant.Code",all=T)
  # 
  
  #Fill.Gen.ID is an argument passed to the function giving the user the option to fill missing
  #boiler and generator IDs with the association boiler or generator ID when present.
  
  #group months together in data.out
  nonMonths<-data.out
  for (m in months){
    nonMonths<-nonMonths[,names(nonMonths)[regexpr(m,names(nonMonths))<0]]
    if (m=="January"){
      monthData<-data.out[,names(data.out)[regexpr(m,names(data.out))>0]]
    }else{
     monthData<-cbind(monthData,data.out[,names(data.out)[regexpr(m,names(data.out))>0]]) 
    }
  }
data.out<-cbind(nonMonths[1:7],monthData,nonMonths[8:length(nonMonths)])

data.out<-data.out[!duplicated(data.out),]  
data.out$YEAR<-rep(analysisYear,nrow(data.out))

  #write.csv(data.out,"CondenserDutyModel/Output/Bogenkey.csv",row.names=F)
  #write.csv(na.Boiler.ID,"CondenserDutyModel/Output/Bogenkey_NA_boiler.csv",row.names = F)
if (analysisYear>2005){
 data.out.list <- list(bogen.key=data.out,NA.BOILER.ID=na.Boiler.ID,bogen=bogen, 
                        checkPlants = checkPlants, Boiler.ID_NotAssoc = Boiler.ID_NotAssoc,
                        compare_RPM=compare_RPM,missingCSCT = missingCSCT,manualPlants=manualPlants,decembrist=decembrist) 
}else{
  data.out.list <- list(bogen.key=data.out,NA.BOILER.ID=na.Boiler.ID,bogen=bogen, 
                        checkPlants = checkPlants, Boiler.ID_NotAssoc = Boiler.ID_NotAssoc,
                        missingCSCT = missingCSCT,
                        manualPlants=manualPlants) 
}
  
  
  return(data.out.list)
}
