#'@title processComplexCUWD
#'@description 
#'@param df
#'@export


processComplexCUWD<-function(df){
  
  complexAgg<-function(data,aggStr,group=NA){
    aggList<-c("Min","Med","Max")
    if(!all(is.na(unique(group)))){
      if (!identical(group,0)){
       data<-data %>% filter(percentAllocation==100 & cooling %in% group)  
      }else{
        data<-data %>% filter(percentAllocation==100)
      }
       
    }else{
      data<-data %>% filter(percentAllocation<100)
    }
    subStr<-paste0("-contains('",aggList[aggList!=aggStr],"')")
    str<-paste0("data<-data %>%  select(",paste(subStr,collapse=","),",-percentAllocation,-cooling,-flag_minMax)")
    eval(parse(text=str))
    sumName<-names(data)[regexpr(aggStr,names(data))>0]
    names(data)[regexpr(aggStr,names(data))>0]<-"sumVar"
    if ("Month" %in% names(data)){
      data<-data %>% ungroup()
      data<-data %>% dplyr::group_by(Plant.Code,Month,YEAR) %>%
        dplyr::summarise(sumVar=sum(sumVar,na.rm=T),.groups="drop")
    }else{
      data<-data %>% ungroup()
      data<-data %>% dplyr::group_by(Plant.Code,YEAR) %>%
        dplyr::summarise(sumVar=mean(sumVar,na.rm=T),.groups="drop")
    }
    if(!all(is.na(unique(group)))){
    if (identical(group,0)){
      data$sumVar<-0
    }
    }
    names(data)[names(data)=="sumVar"]<-sumName
    return(data)
  }
  
  #find complex plants
if ("Month" %in% names(df)){
  complexP<-df %>% group_by(Plant.Code,cooling,Month,YEAR) %>%
    dplyr::summarise(countPA = length(percentAllocation),.groups="drop")
}else{
  complexP<-df %>% group_by(Plant.Code,cooling,YEAR) %>%
    dplyr::summarise(countPA = length(percentAllocation),.groups="drop")
}
complexP<-complexP %>% filter(countPA>1)

complexP<-df %>% filter(Plant.Code %in% complexP$Plant.Code)
df<-df %>% filter(!Plant.Code %in% complexP$Plant.Code)


nonComplex<-complexP %>% filter(percentAllocation==100 & flag_minMax==FALSE)
df<-rbind(df,nonComplex)

complexP<-anti_join(complexP,nonComplex,by=c("Plant.Code","cooling","percentAllocation","flag_minMax"))

for (p in unique(complexP$Plant.Code)){
  subdata<-complexP %>% filter(Plant.Code==p)
  if (p==869){
    if (regexpr("Consump",names(FEWSRtower.list)[r])>0){
    mingroup<-"river"
    maxgroup<-"pond"
    }else{#WD
      mingroup<-"tower"
      maxgroup<-"river"
    }
    
  }else if (identical(unique(subdata$cooling)[order(unique(subdata$cooling))],c("pond","tower"))){
    if (regexpr("Consump",names(FEWSRtower.list)[r])>0){
      mingroup<-"tower"
      maxgroup<-"pond"
    }else{#wd
      mingroup<-"tower"
      maxgroup<-"pond"
    }
    
  }else if (sum(subdata %>% ungroup() %>% select(cooling,percentAllocation) %>% 
                dplyr::group_by(cooling,percentAllocation) %>% 
                filter(percentAllocation!=100) %>% 
                dplyr::summarise(percentAllocation=unique(percentAllocation),.groups="drop") %>% 
                select(percentAllocation),na.rm=T)!=100){#DC
    if (regexpr("Consump",names(FEWSRtower.list)[r])>0){
      mingroup<-0
      maxgroup<-as.character(unique(subdata$cooling))
    }else{#wd
      mingroup<-0
      maxgroup<-as.character(unique(subdata$cooling))
    }
  }else{#normal complex plant breakdown
    if (regexpr("Consump",names(FEWSRtower.list)[r])>0){
      maxgroup<-c("tower","pond")
      mingroup<-c("river","lake","OS")
    }else{
      maxgroup<-c("river","lake","OS")
      mingroup<-c("tower","pond")
    }
  }
  #get max data
  maxdata<-complexAgg(subdata,"Max",maxgroup)
  
  #get min data
  mindata<-complexAgg(subdata,"Min",mingroup)
  
  #get med data
  meddata<-complexAgg(subdata,"Med")
  
  outComplex<-inner_join(meddata,mindata,by=names(meddata)[regexpr("Med",names(meddata))<0])
  outComplex<-inner_join(outComplex,maxdata,by=names(meddata)[regexpr("Med",names(meddata))<0])
  outComplex$cooling<-rep("complex",nrow(outComplex))
  outComplex$percentAllocation<-rep(NA,nrow(outComplex))
  outComplex$flag_minMax<-rep(NA,nrow(outComplex))
  outComplex<-outComplex %>% select(all_of(names(subdata)))
  
  if (p==unique(complexP$Plant.Code)[1]){
    outComplex_all<-outComplex
  }else{
    outComplex_all<-rbind(outComplex_all,outComplex)
  }
  
}#each p

# #get min/max groups
# if (regexpr("Consump",names(FEWSRtower.list)[r])>0){
#   maxgroup<-c("tower","pond")
#   mingroup<-c("river","lake","OS")
# }else{
#   maxgroup<-c("river","lake","OS")
#   mingroup<-c("tower","pond")
# }
# #get max data
# maxdata<-complexAgg(complexP,"Max",maxgroup)
# 
# #get min data
# mindata<-complexAgg(complexP,"Min",mingroup)
# 
# #get med data
# meddata<-complexAgg(complexP,"Med")
# #combine min,med,mx by Plant.Code-cooling
# outComplex<-inner_join(meddata,mindata,by=names(meddata)[regexpr("Med",names(meddata))<0])
# outComplex<-inner_join(outComplex,maxdata,by=names(meddata)[regexpr("Med",names(meddata))<0])
# outComplex$cooling<-rep("complex",nrow(outComplex))
# outComplex$percentAllocation<-rep(NA,nrow(outComplex))
# outComplex<-outComplex %>% select(all_of(names(df)))
#add complex plants to non-complex plants
df<-rbind(df,outComplex_all)
df<-df[order(as.numeric(df$Plant.Code)),]
return(df)
}