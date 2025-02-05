
flagList<-openxlsx::read.xlsx("E:/Corona_VPN/WBEEP/testAssoc/ColumnLegend_metafile.xlsx",sheet="Flags")
flagList<-flagList[1:29,1]

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
    }else if(!class(f) %in% c("factor","character")){
      if(f!=0){
      c<-1
      }
    }else if (f %in% c("OS","OA","RE")){
      c<-1
    }else{#no flag
      c<-0
    }
  }else{#missing flag
    c<-0
  }
  return(c)
}

binary_onlyAuto<-auto_sheet3_key %>% filter(Plant.Code %in% testManual4$Plant.Code)
for (f in flagList){
 if (f==flagList[1]){
  eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }else{
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }
  }

binary_onlyAuto<-binary_onlyAuto %>% select(flagList)


library(data.table)
binary_onlyAuto<-as.data.table(binary_onlyAuto)
vals <- do.call(paste0, binary_onlyAuto)
#Get only unique values
unique_vals <- unique(vals)
#Initialise the columns to 0
binary_onlyAuto[, (unique_vals) := 0]
binary_onlyAuto[cbind(seq_len(nrow(binary_onlyAuto)), match(vals, unique_vals) + length(flagList))] <- 1

binary_onlyAuto<-data.frame(binary_onlyAuto)
summaryCombo<-data.frame(Number_MisMatch_Sheet3Bogens=numeric(0),Number_MisMatch_Sheet3Plants=numeric(0),
                         CompressedPattern=character(0))
for (n in names(binary_onlyAuto)[1:(length(flagList))]){
  eval(parse(text=paste0("summaryCombo$",n,"<-numeric(0)")))
}
j<-1

usedPlants<-character(0)
comboOrder<-data.frame(combo = as.character(names(binary_onlyAuto)[(length(flagList)+1):length(binary_onlyAuto)]))
comboOrder$countFlags<-sapply(comboOrder$combo,function(x) nchar(as.character(x)) - nchar(gsub("1","",as.character(x))))
comboOrder<-comboOrder[order(comboOrder$countFlags,decreasing=T),]
comboOrder<-as.character(comboOrder$combo)
#for (n in names(binary_onlyAuto)[(length(flagList)+1):length(binary_onlyAuto)][order(names(binary_onlyAuto)[(length(flagList)+1):length(binary_onlyAuto)],decreasing=TRUE)]){
for (n in comboOrder){ 
 summaryCombo[[j,1]]<-as.numeric(colSums(binary_onlyAuto[c(n)]))
  

  eval(parse(text=paste0("numPlants<-testManual4[which(binary_onlyAuto$",n,"==1),]")))
  numPlants<-numPlants %>% filter(!Plant.Code %in% usedPlants)
  if (n!="X000000000000000000000000000"){
    usedPlants<-c(usedPlants,unique(numPlants$Plant.Code))
  }
  
  summaryCombo[[j,2]]<-as.numeric(length(unique(numPlants$Plant.Code)))
  print(as.numeric(colSums(binary_onlyAuto[c(n)])))

  n<-gsub("X","",n)
  summaryCombo$CompressedPattern<-as.character(summaryCombo$CompressedPattern)
  summaryCombo[[j,3]]<-as.character(n)
  j<-j+1
  for (i in 1:nchar(n)){
    if (substr(n,i,i)==1){
      c<-names(binary_onlyAuto)[i]
      print(c)
      if (c %in% names(summaryCombo)){
        summaryCombo[nrow(summaryCombo),c]<-1
      }else{
        eval(parse(text=paste0("summaryCombo$",c,"<-NA")))
        summaryCombo[nrow(summaryCombo),c]<-1
      }
    }
  }

}
# ###round 2
# comboOrder<-summaryCombo[order(summaryCombo$Number_MisMatch_Sheet3Plants,summaryCombo$Number_MisMatch_Sheet3Plants,decreasing=TRUE),]
# comboOrder<-comboOrder$CompressedPattern
# comboOrder2<-comboOrder[which(nchar(gsub("1","",comboOrder))!=nchar(comboOrder))]
# if (length(comboOrder)>length(comboOrder2)){
#   comboOrder<-c(comboOrder2,comboOrder[which(nchar(gsub("1","",comboOrder))==nchar(comboOrder))])
# }
# comboOrder<-paste0("X",comboOrder)
# summaryCombo<-data.frame(Number_MisMatch_Sheet3Bogens=numeric(0),Number_MisMatch_Sheet3Plants=numeric(0),
#                          CompressedPattern=character(0))
# for (n in names(binary_onlyAuto)[1:(length(flagList))]){
#   eval(parse(text=paste0("summaryCombo$",n,"<-numeric(0)")))
# }
# j<-1
# 
# usedPlants<-character(0)
# for (n in comboOrder){
#   summaryCombo[[j,1]]<-as.numeric(colSums(binary_onlyAuto[c(n)]))
#   
#   
#   eval(parse(text=paste0("numPlants<-testManual4[which(binary_onlyAuto$",n,"==1),]")))
#   numPlants<-numPlants %>% filter(!Plant.Code %in% usedPlants)
#   if (n!="X000000000000000000000000000"){
#     usedPlants<-c(usedPlants,unique(numPlants$Plant.Code))
#   }
#   
#   summaryCombo[[j,2]]<-as.numeric(length(unique(numPlants$Plant.Code)))
#   print(as.numeric(colSums(binary_onlyAuto[c(n)])))
#   
#   n<-gsub("X","",n)
#   summaryCombo$CompressedPattern<-as.character(summaryCombo$CompressedPattern)
#   summaryCombo[[j,3]]<-as.character(n)
#   j<-j+1
#   for (i in 1:nchar(n)){
#     if (substr(n,i,i)==1){
#       c<-names(binary_onlyAuto)[i]
#       print(c)
#       if (c %in% names(summaryCombo)){
#         summaryCombo[nrow(summaryCombo),c]<-1
#       }else{
#         eval(parse(text=paste0("summaryCombo$",c,"<-NA")))
#         summaryCombo[nrow(summaryCombo),c]<-1
#       }
#     }
#   }
#   
# }
summaryCombo2<-summaryCombo[order(summaryCombo$Number_MisMatch_Sheet3Bogens, decreasing = T),]
write.csv(summaryCombo,file=paste0(pathWrite,"binaryPatternsSheet3BogenFlags.csv"),row.names=F, na="-",)

###############################binarybyPlant
binary_onlyAuto<-auto_sheet3_key %>% filter(Plant.Code %in% testManual4$Plant.Code)
for (f in flagList){
  if (f==flagList[1]){
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }else{
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }
}

binary_onlyAuto<-binary_onlyAuto %>% select(Plant.Code,flagList)
binary_onlyAuto<-aggregate(binary_onlyAuto[-c(1)],by=list(Plant.Code=binary_onlyAuto$Plant.Code),sum)
binary_onlyAuto[,-c(1)]<-sapply(binary_onlyAuto[,-c(1)],function(x) ifelse(x>=1,1,0))
testManual5<-unique(testManual4$Plant.Code)
binary_onlyAuto<-binary_onlyAuto %>% select(flagList)


library(data.table)
binary_onlyAuto<-as.data.table(binary_onlyAuto)
vals <- do.call(paste0, binary_onlyAuto)
#Get only unique values
unique_vals <- unique(vals)
#Initialise the columns to 0
binary_onlyAuto[, (unique_vals) := 0]
binary_onlyAuto[cbind(seq_len(nrow(binary_onlyAuto)), match(vals, unique_vals) + length(flagList))] <- 1

binary_onlyAuto<-data.frame(binary_onlyAuto)
summaryCombo<-data.frame(Number_MisMatch_Sheet3Plants=numeric(0),
                         CompressedPattern=character(0))
for (n in names(binary_onlyAuto)[1:(length(flagList))]){
  eval(parse(text=paste0("summaryCombo$",n,"<-numeric(0)")))
}
j<-1

usedPlants<-character(0)
unFlaggedPlants<-character(0)
comboOrder<-data.frame(combo = as.character(names(binary_onlyAuto)[(length(flagList)+1):length(binary_onlyAuto)]))
comboOrder$countFlags<-sapply(comboOrder$combo,function(x) nchar(as.character(x)) - nchar(gsub("1","",as.character(x))))
comboOrder<-comboOrder[order(comboOrder$countFlags,decreasing=T),]
comboOrder<-as.character(comboOrder$combo)
#for (n in names(binary_onlyAuto)[(length(flagList)+1):length(binary_onlyAuto)][order(names(binary_onlyAuto)[(length(flagList)+1):length(binary_onlyAuto)],decreasing=TRUE)]){
for (n in comboOrder){ 

  eval(parse(text=paste0("numPlants<-testManual5[which(binary_onlyAuto$",n,"==1)]")))
  numPlants<-numPlants[which(!numPlants %in% usedPlants)]
  if (n=="X000000010000000000000000000" | n=="X010000000000000000000010000"){
    unFlaggedPlants<-c(unFlaggedPlants,numPlants)
  }
  if (n!="X000000000000000000000000000"){
    usedPlants<-c(usedPlants,unique(numPlants))
  }
  
  summaryCombo[[j,1]]<-as.numeric(length(unique(numPlants)))
  print(as.numeric(colSums(binary_onlyAuto[c(n)])))
  
  n<-gsub("X","",n)
  summaryCombo$CompressedPattern<-as.character(summaryCombo$CompressedPattern)
  summaryCombo[[j,2]]<-as.character(n)
  j<-j+1
  for (i in 1:nchar(n)){
    if (substr(n,i,i)==1){
      c<-names(binary_onlyAuto)[i]
      print(c)
      if (c %in% names(summaryCombo)){
        summaryCombo[nrow(summaryCombo),c]<-1
      }else{
        eval(parse(text=paste0("summaryCombo$",c,"<-NA")))
        summaryCombo[nrow(summaryCombo),c]<-1
      }
    }
  }
  
}

summaryCombo2<-summaryCombo[order(summaryCombo$Number_MisMatch_Sheet3Plants, decreasing = T),]
write.csv(summaryCombo,file=paste0(pathWrite,"binaryPatternsSheet3BogenFlags.csv"),row.names=F, na="-",)


################################################
#onlyOrigBogens
 binary_onlyAuto<-auto_sheet3_key %>% filter(Plant.Code %in% onlyOrigBogens$Plant.Code)
for (f in flagList){
 
  if (f==flagList[1]){
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }else{
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }
}

binary_onlyAuto<-binary_onlyAuto %>% select(flagList)


library(data.table)
binary_onlyAuto<-as.data.table(binary_onlyAuto)
vals <- do.call(paste0, binary_onlyAuto)
#Get only unique values
unique_vals <- unique(vals)
#Initialise the columns to 0
binary_onlyAuto[, (unique_vals) := 0]
binary_onlyAuto[cbind(seq_len(nrow(binary_onlyAuto)), match(vals, unique_vals) + length(flagList))] <- 1

binary_onlyAuto<-data.frame(binary_onlyAuto)
summaryCombo<-data.frame(Number_MisMatch_Sheet3Bogens=numeric(0),Number_MisMatch_Sheet3Plants=numeric(0),
                         CompressedPattern=character(0))
for (n in names(binary_onlyAuto)[1:(length(flagList))]){
  eval(parse(text=paste0("summaryCombo$",n,"<-numeric(0)")))
}
j<-1
for (n in names(binary_onlyAuto)[(length(flagList)+1):length(binary_onlyAuto)]){
  summaryCombo[[j,1]]<-as.numeric(colSums(binary_onlyAuto[c(n)]))
  eval(parse(text=paste0("numPlants<-testManual4[which(binary_onlyAuto$",n,"==1),]")))
  summaryCombo[[j,2]]<-as.numeric(length(unique(numPlants$Plant.Code)))
  print(as.numeric(colSums(binary_onlyAuto[c(n)])))

  n<-gsub("X","",n)
  summaryCombo$CompressedPattern<-as.character(summaryCombo$CompressedPattern)
  summaryCombo[[j,3]]<-as.character(n)
  j<-j+1
  for (i in 1:nchar(n)){
    if (substr(n,i,i)==1){
      c<-names(binary_onlyAuto)[i]
      print(c)
      if (c %in% names(summaryCombo)){
        summaryCombo[nrow(summaryCombo),c]<-1
      }else{
        eval(parse(text=paste0("summaryCombo$",c,"<-NA")))
        summaryCombo[nrow(summaryCombo),c]<-1
      }
    }
  }
  
}

summaryCombo<-summaryCombo[order(summaryCombo$Number_MisMatch_Sheet3Bogens, decreasing = T),]
write.csv(summaryCombo,file=paste0(pathWrite,"binaryPatternsSheet3_OnlyInOrig_PlantFlags.csv"),row.names=F, na="-")

write.csv(test, file="D:/17NotFlaggedMisMatch.csv",row.names = F)


############################################
################################################
#46 false positive
binary_onlyAuto<-testManual3
for (f in flagList){
  
  if (f==flagList[1]){
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }else{
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }
}

binary_onlyAuto<-binary_onlyAuto %>% select(flagList)


library(data.table)
binary_onlyAuto<-as.data.table(binary_onlyAuto)
vals <- do.call(paste0, binary_onlyAuto)
#Get only unique values
unique_vals <- unique(vals)
#Initialise the columns to 0
binary_onlyAuto[, (unique_vals) := 0]
binary_onlyAuto[cbind(seq_len(nrow(binary_onlyAuto)), match(vals, unique_vals) + length(flagList))] <- 1

binary_onlyAuto<-data.frame(binary_onlyAuto)
summaryCombo<-data.frame(Number_Matched_Sheet3Bogens=numeric(0),Number_Matched_Sheet3Plants=numeric(0),
                         CompressedPattern=character(0))
for (n in names(binary_onlyAuto)[1:(length(flagList))]){
  eval(parse(text=paste0("summaryCombo$",n,"<-numeric(0)")))
}

j<-1
for (n in names(binary_onlyAuto)[(length(flagList)+1):length(binary_onlyAuto)]){
  summaryCombo[[j,1]]<-as.numeric(colSums(binary_onlyAuto[c(n)]))
  eval(parse(text=paste0("numPlants<-testManual3[which(binary_onlyAuto$",n,"==1),]")))
  summaryCombo[[j,2]]<-as.numeric(length(unique(numPlants$Plant.Code)))
  
  print(as.numeric(colSums(binary_onlyAuto[c(n)])))
  
  n<-gsub("X","",n)
  summaryCombo$CompressedPattern<-as.character(summaryCombo$CompressedPattern)
  summaryCombo[[j,3]]<-as.character(n)
  j<-j+1
  for (i in 1:nchar(n)){
    if (substr(n,i,i)==1){
      c<-names(binary_onlyAuto)[i]
      print(c)
      if (c %in% names(summaryCombo)){
        summaryCombo[nrow(summaryCombo),c]<-1
      }else{
        eval(parse(text=paste0("summaryCombo$",c,"<-NA")))
        summaryCombo[nrow(summaryCombo),c]<-1
      }
    }
  }
  
}

summaryCombo<-summaryCombo[order(summaryCombo$Number_Matched_Sheet3Bogens, decreasing = T),]
write.csv(summaryCombo,file=paste0(pathWrite,"binaryPatternsSheet3_46FalseFlags.csv"),row.names=F, na="-")

summaryCombo3<-merge(summaryCombo,summaryCombo2,,all = T)
write.csv(summaryCombo3,file=paste0(pathWrite,"binaryPatterns_Matched_vs_MisMatched.csv"),row.names=F, na="-")




#################################
################################################
#manualPLants
#add countFlags column
countFlagfunc2<-function(f){
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
binary_onlyAuto<-manualPlants2
flagList<-names(binary_onlyAuto[2:6])
for (f in flagList){
  
  if (f==flagList[1]){
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc2(",f,")))")))
  }else{
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc2(",f,")))")))
  }
}

binary_onlyAuto<-binary_onlyAuto %>% select(flagList)


library(data.table)
binary_onlyAuto<-as.data.table(binary_onlyAuto)
vals <- do.call(paste0, binary_onlyAuto)
#Get only unique values
unique_vals <- unique(vals)
#Initialise the columns to 0
binary_onlyAuto[, (unique_vals) := 0]
binary_onlyAuto[cbind(seq_len(nrow(binary_onlyAuto)), match(vals, unique_vals) + length(flagList))] <- 1

binary_onlyAuto<-data.frame(binary_onlyAuto)
summaryCombo<-data.frame(Number_Plants=numeric(0),
                         CompressedPattern=character(0))
for (n in names(binary_onlyAuto)[1:(length(flagList))]){
  eval(parse(text=paste0("summaryCombo$",n,"<-numeric(0)")))
}

j<-1
for (n in names(binary_onlyAuto)[(length(flagList)+1):length(binary_onlyAuto)]){
  summaryCombo[[j,1]]<-as.numeric(colSums(binary_onlyAuto[c(n)]))

  print(as.numeric(colSums(binary_onlyAuto[c(n)])))
  
  n<-gsub("X","",n)
  summaryCombo$CompressedPattern<-as.character(summaryCombo$CompressedPattern)
  summaryCombo[[j,2]]<-as.character(n)
  j<-j+1
  for (i in 1:nchar(n)){
    if (substr(n,i,i)==1){
      c<-names(binary_onlyAuto)[i]
      print(c)
      if (c %in% names(summaryCombo)){
        summaryCombo[nrow(summaryCombo),c]<-1
      }else{
        eval(parse(text=paste0("summaryCombo$",c,"<-NA")))
        summaryCombo[nrow(summaryCombo),c]<-1
      }
    }
  }
  
}

summaryCombo<-summaryCombo[order(summaryCombo$Number_Plants, decreasing = T),]
write.csv(summaryCombo,file=paste0(pathWrite,"binaryPatterns_ManualPlants2_8.16.21.csv"),row.names=F, na="-")


#####################################################

for (f in flagList){
   if (regexpr("generator",tolower(f))<0 & regexpr("net.generation",tolower(f))<0 & regexpr("netgen",tolower(f))<0){
  if (f==flagList[1]){
   
    eval(parse(text=paste0("binary_onlyAuto<-auto_sheet3_key %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
   
      }else{

    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
        }
          }
}

binary_Auto3<-binary_onlyAuto %>% select(Plant.Code,plant_bo,bogen,flagList[which(flagList %in% names(binary_onlyAuto))])
binary_Auto3$flag_ManualPlant<-ifelse(binary_Auto3$Plant.Code %in% manualPlants2$Plant.Code,1,0)
binary_Auto3$flag_MisMatch<-ifelse(binary_Auto3$Plant.Code %in% testManual3$Plant.Code,0,1)

for (f in flagList){
  if (regexpr("boiler",tolower(f))<0){
  if (f==flagList[1]){
    
    eval(parse(text=paste0("binary_onlyAuto<-auto_sheet4_key %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }else{
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }
  }
}

binary_Auto4<-binary_onlyAuto %>% select(Plant.Code,plant_gen,bogen,flagList[which(flagList %in% names(binary_onlyAuto))])
binary_Auto4$flag_ManualPlant<-ifelse(binary_Auto4$Plant.Code %in% manualPlants2$Plant.Code,1,0)
binary_Auto4$flag_MisMatch<-ifelse(binary_Auto4$Plant.Code %in% testManual3$Plant.Code,0,1)




#############################################
for (f in flagList){
  if (f==flagList[1]){
    eval(parse(text=paste0("binary_onlyAuto<-test3 %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }else{
    eval(parse(text=paste0("binary_onlyAuto<-binary_onlyAuto %>% rowwise() %>% mutate(",f," = sum(countFlagfunc(",f,")))")))
  }
}

test3_binary<-binary_onlyAuto %>% select(Plant.Code,plant_gen,bogen,flagList)