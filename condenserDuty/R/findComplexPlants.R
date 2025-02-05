#'@title findComplexPlant
#'@description 
#'@param data
#'@export


findComplexPlants<-function(data){
#get id vars
  data$Plant.Code<-sapply(data$Plant_id, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))
  data$cooling<-sapply(data$Plant_id, function(x) strsplit(as.character(x),"\\^")[[1]][2])
  data$percentAllocation<-sapply(data$Plant_id, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][3]))
  #get plants with multiples
    dataM<-data %>% dplyr::group_by(Plant.Code,cooling) %>%
   dplyr::summarise(countPlant=length(percentAllocation))
  dataM<-dataM %>% filter(countPlant>1)
  #subset multiples from other plants
    dataM<-data %>% filter(Plant.Code %in% dataM$Plant.Code)
  data<-data %>% filter(!Plant.Code %in% dataM$Plant.Code)
  data$flag_minMax<-rep(FALSE,nrow(data))
  dataM$flag_minMax<-rep(FALSE,nrow(dataM))
  #loop through multiples plants
    outMulti<-dataM[0,]
  for (p in unique(dataM$Plant.Code)){
    subMulti<-dataM %>% filter(Plant.Code==p)
    subMulti$cooling<-sapply(subMulti$Plant_id, function(x) strsplit(as.character(x),"\\^")[[1]][2])
    subMulti$percentAllocation<-sapply(subMulti$Plant_id, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][3]))
    for (c in unique(subMulti$cooling)){
      subCool<-subMulti %>% filter(cooling==c)
      #determine which 100 percentAllcoation is min/max
        subCoolPart<-subCool %>% filter(percentAllocation!=100)
      subCoolPart.out<-subCoolPart
      pA<-subCoolPart$percentAllocation/100
      subCoolPart<-subCoolPart %>% select(-Plant.Code,-Plant_id,-cooling,-percentAllocation,-flag_minMax)
      subCoolPart<-subCoolPart %>% dplyr::mutate_all(.,function(x) round(x/pA,4))
      subCoolWhole<-subCool %>% filter(percentAllocation==100)
      if (nrow(subCoolWhole)!=1){
        for (r in 1:nrow(subCoolWhole)){
          if(identical(c(round(subCoolWhole[r,names(subCoolWhole) %in% as.character(seq(1:12))],4)),
                                     c(subCoolPart))){
            subCoolWhole[r,]$flag_minMax<-TRUE
            }#if min/max columns
          }#for each r
        }else{
          subCoolWhole$flag_minMax<-TRUE
          }#regular complex
      outMulti<-rbind(outMulti,subCoolWhole,subCoolPart.out)
      }#for c in cooling
    }#for p in complex plants

  data<-rbind(data,outMulti)
  data$Plant_id<-paste(data$Plant.Code,data$cooling,data$percentAllocation,data$flag_minMax,sep="^")
  data<-data %>% select(-Plant.Code,-cooling,-percentAllocation,-flag_minMax)
  return(data)
}