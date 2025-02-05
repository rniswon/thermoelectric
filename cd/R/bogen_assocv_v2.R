bogen_assocv.v2 <- function(bogen,generator.data,generation.data,boilerFuelData,bocoo,vis_out=F,Fill.Gen.ID=T){
  #These first few lines are only cleaning up the datasets and preparing them for the association process
  #Bogen table, set generator.id and boiler.id variables to character strings
  bogen$Generator.ID <- as.character(bogen$Generator.ID)
  bogen$Boiler.ID <- as.character(bogen$Boiler.ID)
  #Drop utlity id variable from bogen table
  bogen <- select(bogen,-c("Utility.ID"))
  
  bogen$Generator.ID <- gsub("(?<![0-9])0+", "",bogen$Generator.ID , perl = TRUE)
  bogen$Boiler.ID <- gsub("(?<![0-9])0+", "",bogen$Boiler.ID , perl = TRUE)
  
  
  #Generator data
  gen_860 <- generator.data
  gen_860$Generator.ID <- as.character(gen_860$Generator.ID)
  #Boiler generation data, set generator.id variable to character strings
  generation.data <- generation.data
  generation.data$Generator.ID <- as.character(generation.data$Generator.ID)
  
  #Aggregate total net generation by plant and generator.id
  generation.data.gb <- generation.data %>% group_by(.,Plant.Code,Generator.ID)
  gen_923 <- generation.data.gb %>% summarise(Net.Generation.mwh=sum(Net.Generation.Year.To.Date))
  #Create a variable to track which boilers are missing from the 923 report
  gen_923$missing.from.923 <- FALSE
  
  
  #Merge generator data and generation data 
  merged <- full_join_track(gen_860,gen_923,by=c("Plant.Code","Generator.ID"),.merge=T,all=T)
  missing.from.860 <- merged[merged$.merge=='right_only',]
  
  
  #Compile list of all generators
  gens <- full_join_track(gen_923,gen_860,by=c("Plant.Code","Generator.ID"),.merge=F,all=T)
  
  #Select columns to keep after merge
  gens <- select(gens,c("Plant.Code","Generator.ID","Unit.Code","Net.Generation.mwh","missing.from.923"))
  gens$Generator.ID <- as.character(gens$Generator.ID)
  gens <- ungroup(gens)
  
  #Linked bogen associations
  
  bga.1 <- full_join_track(gens,bogen,by=c("Plant.Code","Generator.ID"),.merge=F,all=T)
  bga.1 <- select(bga.1,-c(Utility.Name,Plant.Name,Steam.Plant.Type))
  bga_assn <- bga.1[!is.na(bga.1$Boiler.ID),]
  bga_assn$bga.source <- 'eia860.org'
  
  #Unlinked bogen associations
  bga_unassn <- bga.1[is.na(bga.1$Boiler.ID),]
  bga_unassn <- select(bga_unassn,-c(Boiler.ID))
  
  #String Matching unassigned boilers based on fuel data
  bf.923 <- boilerFuelData
  
  #Drop Leading zeros in Boiler.ID
  bf.923$Boiler.ID <- gsub("(?<![0-9])0+", "",bf.923$Boiler.ID , perl = TRUE)
  bf.923$Boiler.ID <- as.character(bf.923$Boiler.ID)
  
  #Sum monthly fuel heat
  bf.923$total.fuel.mmbtu.per.unit <- rowSums(bf.923[,c(28:39)])
  #Calculate total fuel heat
  bf.923$Total.Heat.Content.MMBTU <- bf.923$Total.Fuel.Consumption.Quantity*bf.923$total.fuel.mmbtu.per.unit
  #Summarize heat content by plant and boiler id
  bfd.gb <- bf.923 %>% group_by(Plant.Code,Boiler.ID) %>% summarize(Boiler.Total.Heat.Content.MMBTU=sum(Total.Heat.Content.MMBTU,na.rm=F))
  bfd.gb <- select(bfd.gb,c(Plant.Code,Boiler.ID))
  bfd.gb <- bfd.gb[!duplicated(bfd.gb[,c("Plant.Code","Boiler.ID")]),]
  bfd.923 <- ungroup(bfd.gb)
  #bfd.923 <- unique(bfd.923)
  
  #Create list of boilers not in bogen associations(bga) table
  bga.2 <- full_join_track(bfd.923,bga.1,by=c("Plant.Code","Boiler.ID"),.merge=T,all=T)
  bfd.not.in.bga <- bga.2[bga.2$.merge=="left_only",]
  bfd.not.in.bga <- select(bfd.not.in.bga,-c(.merge))
  bfd.not.in.bga$Boiler.ID.2 <- bfd.not.in.bga$Boiler.ID
  #bga_unassn <- ungroup(bga_unassn)
  
  #Match unassociated generators and boilers
  
  bga_unassn.2 <- merge(bga_unassn,bfd.not.in.bga[,c("Plant.Code","Boiler.ID","Boiler.ID.2")],by.x=c("Plant.Code","Generator.ID"),by.y=c("Plant.Code","Boiler.ID"),all.x=T)
  names(bga_unassn.2)[names(bga_unassn.2)=="Boiler.ID.2"] <- "Boiler.ID"
  bga_unassn.2 <- bga_unassn.2[order(bga_unassn.2$Plant.Code),]
  bga_unassn.2$bga.source[!is.na(bga_unassn.2$Boiler.ID)] <- "string_assn"
  
  
  #Collection of all Bogens assigned and unassigned with their sources and assignment method
  bga.2 <- bind_rows(bga_assn,bga_unassn.2)
  bga.2 <- bga.2[order(bga.2$Plant.Code),]
  
  bga.2$missing.from.923[is.na(bga.2$missing.from.923)] <- T
  
  bga.2$Boiler.ID <- as.character(as.factor(bga.2$Boiler.ID))
  bga.2$Boiler.ID <- na_if(bga.2$Boiler.ID,"character(0)")
  bga.2$Unit.Code <- na_if(bga.2$Unit.Code,"")
  
  #Connecting Bogens with Unit.Codes
  bga.2$Unit.Code <- na_if(bga.2$Unit.Code,"")
  bga.2.units <- bga.2[!is.na(bga.2$Unit.Code),]
  bga.2.gen.units <- select(bga.2.units,-c(Boiler.ID))
  bga.2.boil.units <- select(bga.2.units,c(Plant.Code,Boiler.ID,Unit.Code))
  bga.2.boil.units <- subset(bga.2.boil.units,!is.na(Boiler.ID))
  
  
  #Merge the boilers with units
  bga.unit.compilation <- full_join_track(bga.2.gen.units,bga.2.boil.units,by=c("Plant.Code","Unit.Code"))
  
  bga.unit.compilation$bga.source[is.na(bga.unit.compilation$bga.source)] <- 'unit_connection'
  
  #List of boilers with no unit code
  bga2.non.units <- bga.2 %>% subset(is.na(Unit.Code))
  
  bga.3 <- rbind(bga2.non.units,bga.unit.compilation)
  bga.3 <- bga.3[order(bga.3$Plant.Code),]
  bga.3.1 <- bga.3 %>% select(c(Plant.Code,Generator.ID,Boiler.ID,Unit.Code,bga.source,Net.Generation.mwh,missing.from.923))
  
  #Cleanup
  ##The next few lines address missing bogen associations that are listed in the bad plants list as either 
  #orphan boilers or orphan generators.
  #This line subsets the remaining bogen associations excluding any remaining missing Boiler.IDs. These boilers
  #are stored in the variable na.Boiler.ID
  na.Boiler.ID <- bga.3.1 %>% subset(is.na(Boiler.ID))
  #Fill with boilers fom cooling associations where possible
  filler.bocoos <- select(bocoo,c(1,2))
  fill.boiler.id <- left_join(na.Boiler.ID,filler.bocoos,by="Plant.Code")
  fill.boiler.id$Boiler.ID.x[is.na(fill.boiler.id$Boiler.ID.x)] <- fill.boiler.id$Boiler.ID.y[is.na(fill.boiler.id$Boiler.ID.x)]
  fill.boiler.id <- fill.boiler.id[-8]
  names(fill.boiler.id) <- names(bga.3.1)
  #Append to boiler associations list
  bga.3.1 <- rbind(bga.3.1,fill.boiler.id)
  
  #Identify Plants with generation but no associated boilers
  bad.plants <- bga.3.1 %>% subset(is.na(Boiler.ID)&Net.Generation.mwh>0) 
  
  # write.csv(bad.plants,"CondenserDutyModel/Output/Orphan_Generators.csv",row.names = F)
  bad.plants <- as.data.frame(unique(bad.plants$Plant.Code))
  names(bad.plants) <- "Plant.Code"
  
  bga.3.2 <- full_join_track(bad.plants[1],bga.3.1,.merge = T)
  bga.3.2$Orphan.Generator[bga.3.2$.merge=="matched"] <- T
  bga.3.2$Orphan.Generator[is.na(bga.3.2$Orphan.Generator)] <- F
  
  #Identify generators that are in the 923 report but not mapped
  bga.3.2$unmapped_but_in_923 <- rep(NA,nrow(bga.3.2))
  bga.3.2$unmapped_but_in_923[c(is.na(bga.3.2$Boiler.ID) & bga.3.2$missing.from.923==T & bga.3.2$Net.Generation.mwh==0)] <- T
  bga.3.2$unmapped_but_in_923[is.na(bga.3.2$unmapped_but_in_923)] <- F
  
  #Identify unmapped generators
  bga.3.2$unmapped <- rep(NA,nrow(bga.3.2))
  bga.3.2$unmapped[is.na(bga.3.2$Boiler.ID)] <- T
  bga.3.2$unmapped[!is.na(bga.3.2$Boiler.ID)] <- F
  
  
  bga.3.2$Unit.Code[is.na(bga.3.2$Unit.Code)] <- "none"
  
  bga.3.3 <- bga.3.2[-c(6,8)]
  
  bga.out=bga.3.3
  
  #write.csv(bga.out,"CondenserDutyModel/Output/Boiler_Generator_Mapping_Table.csv",row.names = F)
  
  Fill.Gen.ID=T
  
  #If option to fill 
  if(Fill.Gen.ID){
    bga.out$Boiler.ID[is.na(bga.out$Boiler.ID)] <- bga.out$Generator.ID[is.na(bga.out$Boiler.ID)]
  }
  
  bga.out.for.associate <- bga.out %>% filter(!is.na(Plant.Code))
  
  
  
  #This argument is passed into the function giving the user the option to graph plant bogen network.
  vis_out=F
  #Generates concatenated bogen code used for summaries according to Bogen.
  out <- associate(bga.out.for.associate[c(1:3)],save_image = vis_out)
  
  #Merge the Bogen code with the Bogen associations data frame to create Bogen key used in 
  #analysis
  data.out <- left_join(out,bga.out)
  #data.out <- na.omit(data.out[4:10])
  
  #Fill.Gen.ID is an argument passed to the function giving the user the option to fill missing
  #boiler and generator IDs with the association boiler or generator ID when present.
  
  #write.csv(data.out,"CondenserDutyModel/Output/Bogenkey.csv",row.names=F)
  #write.csv(na.Boiler.ID,"CondenserDutyModel/Output/Bogenkey_NA_boiler.csv",row.names = F)
  data.out.list <- list(bogen.key=data.out,NA.BOILER.ID=na.Boiler.ID)
  
  return(data.out.list)
}
