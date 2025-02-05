executeBogenAssoc<-function(inputData_path,pathWrite,inputData.list,select_RPM=c("CA", "CS", "CT", "ST",NA),eia_year){
  
  #unpack input data
  unPackList(lists = list(inputData.list = inputData.list),
             parentObj = list(NA))
  
  generator.data<-gen_860
  sheet1GenFuelData<-gen_fuel_data
  sheet4GenFuelData<-generation.data
  
  #format plantlist
  # if(eia_year==2015){
  # plantList$Plant.Code<-sapply(plantList$plant_bo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))
  # }
  
  #nukes
  nukes<-inputData.list$gen_fuel_data %>% filter((!is.na(Nuclear.Unit.Id) & Nuclear.Unit.Id!=".") & 
                                                   Plant.Code %in% plantList$Plant.Code)
  nonNukes<-inputData.list$gen_fuel_data %>% filter((is.na(Nuclear.Unit.Id) & 
                                                       Reported.Prime.Mover %in% select_RPM &
                                                       Plant.Code %in% plantList$Plant.Code) |
                                                      (Nuclear.Unit.Id=="." & 
                                                         Reported.Prime.Mover %in% select_RPM &
                                                         Plant.Code %in% plantList$Plant.Code))
  
  #complexNukes
  complexNukes<-nonNukes[nonNukes$Plant.Code %in% nukes$Plant.Code,]$Plant.Code
  
  #nukeOnly plants
  nukePlants<-nukes %>% filter(!Plant.Code %in% complexNukes)
  inputData.list$nukePlants<-nukePlants
  assign("inputData.list",inputData.list,envir = .GlobalEnv)
  
  #Generate boiler and generator associations# 
  bogen.key<-bogen_assocv.v2(analysisYear = eia_year,bogen,generator.data,sheet4GenFuelData,boilerFuelData,
                             boilerDesignData,retiredGenerators,gen_fuel_data,
                             plantList$Plant.Code,
                             nukePlants,
                             select_RPM,vis_out=F,Fill.Gen.ID=T)
  
  #get flags for changed IDS
  bogenFlag<-bogen.key$bogen
  
  #save NAs
  NA.BOILER.ID<-bogen.key$NA.BOILER.ID
  
  #possible problem plants
  checkPlants<-bogen.key$checkPlants
  
  #Boiler.IDs not in association table
  Boiler.ID_NotAssoc = bogen.key$Boiler.ID_NotAssoc
  
  #inconsistent PM
  compare_RPM<-bogen.key$compare_RPM
  
  #manualPLants
  manualPlants<-bogen.key$manualPlants
  
  missingCSCT<-bogen.key$missingCSCT
  
  #get only key
  bogen.key<-bogen.key$bogen.key
  
  #make complex nukes unique
  bogen.key<-bogen.key %>% mutate(Bogen = ifelse(Plant.Code %in% 
                                                      complexNukes,paste0(Bogen,".NonNuke"),Bogen))
  
  #test_bogen.key<-bogen.key
  #save(test_bogen.key,file="E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/associationScripts/test/test_bogen.key")
  
  
  #Select unique boilers and their Bogen code from Bogen key list (sheet3key)
  #take rows with max countFlags
  unique.boilers<-bogen.key %>% 
    group_by(Plant.Code,Boiler.ID,Reported.Prime.Mover_page1) %>%
    slice(which.min(missing.bf.923))
  
  
  assign("unique.boilers",unique.boilers,envir = .GlobalEnv)
  
  
  #Select unique generating units (sheet4key)
  #take rows with max countFlags
  unique.generators<-bogen.key %>% 
    group_by(Plant.Code,Generator.ID,Reported.Prime.Mover_page1) %>%
    slice(which.max(countFlags))
  
  #remove nuclear plants
  #nukePlants$Plant.Code<-sapply(nukePlants$combogencoo, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][2]))
  unique.boilers<-unique.boilers[!unique.boilers$Plant.Code %in% nukePlants$Plant.Code,]
  unique.generators<-unique.generators[!unique.generators$Plant.Code %in% nukePlants$Plant.Code,]
  
  
  sheet3_key<-unique.boilers
  
  #create plant_bo column
  sheet3_key$plant_bo<-paste0(sheet3_key$Plant.Code,"^",ifelse(!is.na(sheet3_key$Boiler.ID),sheet3_key$Boiler.ID,"NA"))
  sheet3_key$plant_bo_bf.923<-paste0(sheet3_key$Plant.Code,"^",ifelse(!is.na(sheet3_key$orig_Boiler.ID_bf.923),sheet3_key$orig_Boiler.ID_bf.923,"NA"))
  sheet3_key$plant_bo_assoc<-paste0(sheet3_key$Plant.Code,"^",ifelse(!is.na(sheet3_key$orig_Boiler.ID_assoc),sheet3_key$orig_Boiler.ID_assoc,"NA"))
  
  sheet4_key<-unique.generators
  
  #create plant_gen column
  sheet4_key$plant_gen<-paste0(sheet4_key$Plant.Code,"^",ifelse(!is.na(sheet4_key$Generator.ID),sheet4_key$Generator.ID,"NA"))
  sheet4_key$plant_gen.923<-paste0(sheet4_key$Plant.Code,"^",ifelse(!is.na(sheet4_key$orig_Generator.ID_923),sheet4_key$orig_Generator.ID_923,"NA"))
  sheet4_key$plant_gen.860<-paste0(sheet4_key$Plant.Code,"^",ifelse(!is.na(sheet4_key$orig_Generator.ID_860),sheet4_key$orig_Generator.ID_860,"NA"))
  sheet4_key$plant_gen_assoc<-paste0(sheet4_key$Plant.Code,"^",ifelse(!is.na(sheet4_key$orig_Generator.ID_assoc),sheet4_key$orig_Generator.ID_assoc,"NA"))
  assign("sheet3_key84",sheet3_key,envir = .GlobalEnv)
  #format columns
  names(sheet3_key)[names(sheet3_key)=="Bogen"]<-"bogen"
  names(sheet4_key)[names(sheet4_key)=="Bogen"]<-"bogen"
  
  #remove duplicates
  sheet3_key<-sheet3_key[!duplicated(sheet3_key),]
  sheet3_key<-sheet3_key[!is.na(sheet3_key$plant_bo),]
  sheet4_key<-sheet4_key[!duplicated(sheet4_key),]
  sheet4_key<-sheet4_key[!is.na(sheet4_key$plant_gen),]
  assign("sheet3_key94",sheet3_key,envir = .GlobalEnv)
  #remove Boilers with No boilerFuelData and Generators with No generation.data
  sheet3_key<-sheet3_key[sheet3_key$flag_Boiler.Status!=1 | regexpr("NGCC",sheet3_key$bogen)>0 | regexpr("ST",sheet3_key$bogen)>0,]
  sheet3_key<-sheet3_key %>% filter(missing.bf.923!=TRUE | regexpr("NGCC",bogen)>0 | regexpr("ST",bogen)>0)
  assign("sheet3_key98",sheet3_key,envir = .GlobalEnv)
  noGenData<-generation.data
  noGenData$Generator.ID <- as.character(noGenData$Generator.ID)
  noGenData<-noGenData %>% filter(Plant.Code %in% plantList$Plant.Code)
  noGenData$flagGenerator.ID_923<-sapply(noGenData$Generator.ID, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                                                ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  noGenData$orig_Generator.ID_923<-noGenData$Generator.ID
  noGenData$Generator.ID <- gsub("(?<![0-9])", "", noGenData$Generator.ID , perl = TRUE)
  noGenData$Generator.ID <- gsub("[[:space:]]", "", noGenData$Generator.ID )
  noGenData<-removeLeadZero(noGenData,"Generator.ID","flagGenerator.ID_923")
  noGenData<-anti_join(sheet4_key[,names(sheet4_key)!="Reported.Prime.Mover_page1"],noGenData,by=c("Plant.Code","Generator.ID"))
  noGenData$missing.generation.data<-rep(TRUE,nrow(noGenData))
  noGenData<-noGenData %>% select(Plant.Code,Generator.ID,missing.generation.data)
  sheet4_key<-merge(sheet4_key,noGenData,by=c("Plant.Code","Generator.ID"), all.x = TRUE)
  assign("sheet4_key112",sheet4_key,envir = .GlobalEnv)
  sheet4_key$missing.generation.data<-ifelse(is.na(sheet4_key$missing.generation.data) | regexpr("NGCC",sheet4_key$bogen)>0 | regexpr("ST",sheet4_key$bogen)>0,FALSE,TRUE)
  sheet4_key<-sheet4_key %>% filter(!missing.generation.data)
  sheet4_key<-sheet4_key[!duplicated(sheet4_key),]
  assign("sheet4_key115",sheet4_key,envir = .GlobalEnv)
  #format FH and NG tables for compiled file
  sheet3_key<-ungroup(sheet3_key)
  bf.923.ungrouped<-merge(bf.923.ungrouped,sheet3_key %>% select(Plant.Code,Boiler.ID,bogen,-Reported.Prime.Mover_page1),
                          by=c("Plant.Code","Boiler.ID"))
  bf.923.ungrouped<-bf.923.ungrouped[!duplicated(bf.923.ungrouped),]
  if (eia_year>2005){
   bf.923.ungrouped<-bf.923.ungrouped %>% select(Plant.Code,Boiler.ID,bogen,Reported.Prime.Mover_bf.923,
                                                Reported.Fuel.Type.Code,contains("Fuel.Heat"),YEAR) 
  }else{
    bf.923.ungrouped<-bf.923.ungrouped %>% select(Plant.Code,Boiler.ID,bogen,
                                                  Reported.Fuel.Type.Code,contains("Fuel.Heat"),YEAR) 
  }
  
  sheet4_key<-ungroup(sheet4_key)
  generation.data.line124<-merge(generation.data.line124,sheet4_key %>% select(Plant.Code,Generator.ID,bogen,-Reported.Prime.Mover_page1),
                          by=c("Plant.Code","Generator.ID"))
  generation.data.line124<-generation.data.line124[!duplicated(generation.data.line124),]
  if (eia_year>2005){
    generation.data.line124<-generation.data.line124 %>% select(Plant.Code,Generator.ID,bogen,Reported.Prime.Mover,
                                                              Net.Generation.Year.To.Date,contains("Net.Generation"),YEAR)
  }else{
    generation.data.line124<-generation.data.line124 %>% select(Plant.Code,Generator.ID,bogen,
                                                                Net.Generation.Year.To.Date,contains("Net.Generation"),YEAR)
  }
  
  if (eia_year>2005){
     bfd.gb<-merge(bfd.gb,sheet3_key %>% select(Plant.Code,Boiler.ID,bogen,Reported.Prime.Mover_bf.923,flagFuelHeat),
                by=c("Plant.Code","Boiler.ID")) 
  }else{
    bfd.gb<-merge(bfd.gb,sheet3_key %>% select(Plant.Code,Boiler.ID,bogen,flagFuelHeat),
                  by=c("Plant.Code","Boiler.ID")) 
  }

  noDupsYear<-bf.923.ungrouped %>% select(Plant.Code,Boiler.ID,YEAR)
  noDupsYear<-noDupsYear[!duplicated(noDupsYear),]
  bfd.gb<-merge(bfd.gb,noDupsYear,by=c("Plant.Code","Boiler.ID"))
  if (eia_year>2005){
   bfd.gb<-bfd.gb %>% select(Plant.Code,Boiler.ID,bogen,Reported.Prime.Mover_bf.923,flagFuelHeat,contains("Fuel.Heat"),YEAR)  
  }else{
    bfd.gb<-bfd.gb %>% select(Plant.Code,Boiler.ID,bogen,flagFuelHeat,contains("Fuel.Heat"),YEAR)  
  }
  
  noDupsYear<-generation.data.line124 %>% select(Plant.Code,Generator.ID,YEAR)
  noDupsYear<-noDupsYear[!duplicated(noDupsYear),]
  generation.data.gb<-merge(generation.data.gb,noDupsYear,by=c("Plant.Code","Generator.ID"))
  if (eia_year>2005){
    generation.data.gb<-merge(generation.data.gb,sheet4_key %>% select(Plant.Code,Generator.ID,bogen,
                                                                       Reported.Prime.Mover_923),
                              by=c("Plant.Code","Generator.ID"))
    generation.data.gb<-generation.data.gb %>% select(Plant.Code,Generator.ID,bogen,Reported.Prime.Mover_923,
                                                      Net.Generation.mwh,contains("Net.Generation"),YEAR)
    noDupsforFH<-bfd.gb %>% select(Plant.Code,bogen,Reported.Prime.Mover_bf.923,flagFuelHeat,YEAR)
  }else{
  generation.data.gb<-merge(generation.data.gb,sheet4_key %>% select(Plant.Code,Generator.ID,bogen),
                            by=c("Plant.Code","Generator.ID"))
  
  generation.data.gb<-generation.data.gb %>% select(Plant.Code,Generator.ID,bogen,
                                                    Net.Generation.mwh,contains("Net.Generation"),YEAR)
  
  noDupsforFH<-bfd.gb %>% select(Plant.Code,bogen,flagFuelHeat,YEAR)
  }
  
  noDupsforFH<-noDupsforFH[!duplicated(noDupsforFH),]
  names(sumFuelHeatByBogen)[which(names(sumFuelHeatByBogen)=="Bogen")]<-"bogen"
  sumFuelHeatByBogen<-merge(sumFuelHeatByBogen,noDupsforFH,by=c("Plant.Code","bogen"),all.x = TRUE)
  sumFuelHeatByBogen$blankCol<-rep("",nrow(sumFuelHeatByBogen))
  sumFuelHeatByBogen$YEAR<-rep(eia_year,nrow(sumFuelHeatByBogen))
  if(eia_year>2005){
    sumFuelHeatByBogen<-sumFuelHeatByBogen %>% select(Plant.Code,blankCol,bogen,Reported.Prime.Mover_bf.923,flagFuelHeat,
                                                      contains("Fuel.Heat"),YEAR) 
    noDupsforNG<-generation.data.gb %>% select(Plant.Code,bogen,Reported.Prime.Mover_923,Net.Generation.mwh,YEAR)

  }else{
  sumFuelHeatByBogen<-sumFuelHeatByBogen %>% select(Plant.Code,blankCol,bogen,flagFuelHeat,
                                                    contains("Fuel.Heat"),YEAR)

  noDupsforNG<-generation.data.gb %>% select(Plant.Code,bogen,Net.Generation.mwh,YEAR)

  }
  noDupsforNG2<-sheet4_key %>% select(Plant.Code,bogen,Net.Generation.mwh)
  noDupsforNG2<-noDupsforNG2 %>% group_by(Plant.Code,bogen) %>% summarize(Net.Generation.mwh = sum(Net.Generation.mwh,na.rm=TRUE))
  noDupsforNG2<-noDupsforNG2[!duplicated(noDupsforNG2),]
  noDupsforNG<-noDupsforNG[!duplicated(noDupsforNG),]
  names(sumNetGenByBogen)[which(names(sumNetGenByBogen)=="Bogen")]<-"bogen"
  sumNetGenByBogen<-merge(sumNetGenByBogen,noDupsforNG,by=c("Plant.Code","bogen"),all.x=TRUE)
  sumNetGenByBogen[regexpr("NGCC",sumNetGenByBogen$bogen)>0 | regexpr("ST",sumNetGenByBogen$bogen)>0,names(sumNetGenByBogen)!="YEAR"]<-merge(sumNetGenByBogen[regexpr("NGCC",sumNetGenByBogen$bogen)>0 | regexpr("ST",sumNetGenByBogen$bogen)>0,
                   !names(sumNetGenByBogen) %in% c("Net.Generation.mwh","YEAR")],noDupsforNG2 %>% filter(regexpr("NGCC",bogen)>0 | regexpr("ST",bogen)>0),
                   by=c("Plant.Code","bogen"), all.x=TRUE)
  sumNetGenByBogen$YEAR<-rep(eia_year,nrow(sumNetGenByBogen))
  sumNetGenByBogen$blankCol<-rep("",nrow(sumNetGenByBogen))
  if (eia_year>2005){
    sumNetGenByBogen<-sumNetGenByBogen %>% select(Plant.Code,blankCol,bogen,Reported.Prime.Mover_923,Net.Generation.mwh,
                                                  contains("Net.Generation"),YEAR) 
  }else{
  sumNetGenByBogen<-sumNetGenByBogen %>% select(Plant.Code,blankCol,bogen,Net.Generation.mwh,
                                                contains("Net.Generation"),YEAR)
  }
  names(thermalEffByBogen)[which(names(thermalEffByBogen)=="Bogen")]<-"bogen"
  thermalEffByBogen<-merge(thermalEffByBogen,noDupsforNG,by=c("Plant.Code","bogen"), all.x=TRUE)
  thermalEffByBogen[regexpr("NGCC",thermalEffByBogen$bogen)>0 | regexpr("ST",thermalEffByBogen$bogen)>0,names(thermalEffByBogen)!="YEAR"]<-merge(thermalEffByBogen[regexpr("NGCC",thermalEffByBogen$bogen)>0 | regexpr("ST",thermalEffByBogen$bogen)>0,
                                                                                                                     !names(thermalEffByBogen) %in% c("Net.Generation.mwh","YEAR")],
                                                                                                       noDupsforNG2 %>% filter(regexpr("NGCC",bogen)>0 | regexpr("ST",bogen)>0),
                                                                                                    by=c("Plant.Code","bogen"), all.x=TRUE)
  thermalEffByBogen$YEAR<-rep(eia_year,nrow(thermalEffByBogen))
  thermalEffByBogen$blankCol<-rep("",nrow(thermalEffByBogen))
  if (eia_year>2005){
    thermalEffByBogen<-thermalEffByBogen %>% select(Plant.Code,blankCol,bogen,Reported.Prime.Mover_923,Net.Generation.mwh,
                                                    contains("ThermalEfficiency"),YEAR)
  }else{
  thermalEffByBogen<-thermalEffByBogen %>% select(Plant.Code,blankCol,bogen,Net.Generation.mwh,
                                                contains("ThermalEfficiency"),YEAR)
  }
  sheet3_key<-sheet3_key %>% select(-Generator.ID,-contains("Net.Generation"),-contains("Generator"),-contains("NetGen"))
  sheet4_key<-sheet4_key %>% select(-Boiler.ID,-contains("Boiler"))
  
  sheet3_key$manualEdit<-integer(nrow(sheet3_key))
  sheet3_key<-sheet3_key %>% select(manualEdit,names(sheet3_key))
  sheet4_key$manualEdit<-integer(nrow(sheet4_key))
  sheet4_key<-sheet4_key %>% select(manualEdit,names(sheet4_key))
  
  #Binary flags for compiled file
  flagList<-openxlsx::read.xlsx(paste0(inputData_path, .Platform$file.sep, "ColumnLegend_metafile.xlsx"),sheet="Flags")
  flagList<-flagList[1:30,1]
  if (eia_year<=2005){
    flagList<-flagList[!flagList %in% c("flag_RPM","flag_923bogenCTonly")]
  }
  
  months<-c("January","February","March","April","May",
            "June","July","August","September","October","November","December")
  for (m in months){
    
    strflagList<-paste0("subflaglist<-c('flagFH_no_NG_",m,"','flagNG_no_FH_",m,"','flagFuelHeat_",m,"')")
    eval(parse(text=strflagList))
    flagList<-c(flagList,subflaglist)
    
  }
  
  #add countFlags column
  countFlagfunc<-function(f){
    if (!is.na(f)){
      if(f==TRUE){
        c<-1
      }else if(!class(f) %in% c("factor","character","logical")){
        if(f!=0){
          c<-1
        }else if(f==0){
          c<-0
        }
      }else if (f %in% c("OS","OA","RE")){
        c<-1
      }else if(f==FALSE){
        c<-0
      }else{#no flag
        c<-0
      }
    }else{#missing flag
      c<-0
    }
    return(c)
  }
  
  for (f in flagList){
    if (regexpr("generator",tolower(f))<0 & regexpr("net.generation",tolower(f))<0 & 
        regexpr("netgen",tolower(f))<0 & regexpr("missing.generation.data",tolower(f))<0){
      if (f==flagList[1]){
        
        eval(parse(text=paste0("binary_onlyAuto<-sheet3_key %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
        
      }else{
        if (tolower(f) %in% tolower(names(sheet3_key))){
        eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
        }
      }
    }
  }
  
  binary_Auto3<-binary_onlyAuto %>% select(Plant.Code,plant_bo,bogen,flagList[which(flagList %in% names(binary_onlyAuto))])

  
  for (f in flagList){
    if (regexpr("boiler",tolower(f))<0){
      if (f==flagList[1]){
        
        eval(parse(text=paste0("binary_onlyAuto<-sheet4_key %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
      }else{
        eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
      }
    }
  }
  
  binary_Auto4<-binary_onlyAuto %>% select(Plant.Code,plant_gen,bogen,flagList[which(flagList %in% names(binary_onlyAuto))])
  
  
  #outputKeys
  write.csv(sheet3_key,file=paste0(pathWrite,"auto_sheet3_bogen.key.csv"),row.names = F)
  write.csv(sheet4_key,file=paste0(pathWrite,"auto_sheet4_bogen.key.csv"),row.names = F)
  
  #output flags
  write.csv(manualPlants,paste0(pathWrite,"manualPlants.csv"),row.names = F)
  write.csv(NA.BOILER.ID,paste0(pathWrite,"NA.BOILER.ID.csv"),row.names = F)
  write.csv(data.frame(Plant.Code = checkPlants),paste0(pathWrite,"FloatingGenerators.csv"),row.names = F)
  write.csv(Boiler.ID_NotAssoc,paste0(pathWrite,"OrphanBoilers.csv"),row.names = F)
  write.csv(compare_RPM,paste0(pathWrite,"Inconsistent_RPM.csv"),row.names = F)
  
  

  bogen.out.list<-named.list(sheet3_key,sheet4_key,missingCSCT,manualPlants,NA.BOILER.ID,Boiler.ID_NotAssoc, 
                             bogen.key,binary_Auto3,binary_Auto4,bf.923.ungrouped,generation.data.line124,
                             bfd.gb,generation.data.gb,sumFuelHeatByBogen,sumNetGenByBogen,thermalEffByBogen)
  return(bogen.out.list)
}