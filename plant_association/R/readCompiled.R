readCompiled<-function(outputData_path,fileName,p,data,readDataName){
  #define functions
  convertClass<-function(df,mandf){
    for (c in 1:length(mandf)){
      if (class(df[[c]])=="numeric"){
        mandf[[c]]<-as.numeric(mandf[[c]])
      }else if (class(df[[c]])=="logical"){
        mandf[[c]]<-as.logical(mandf[[c]])
      }else if (class(df[[c]])=="integer"){
        mandf[[c]]<-as.integer(mandf[[c]])
      }else if (unique(class(df[[c]])==c("POSIXct","POSIXt"))){
        if (names(df)[c]=="manualBogenEditDateTime"){
          mandf[[c]]<-convertToDateTime(as.numeric(mandf[[c]]), origin = "1900-01-01")
        }else{
        mandf[[c]]<-as.POSIXct(mandf[[c]], origin="1960-01-01")
        }
      }
    }
    return(mandf)
  }
  
  getEditTime<-function(manualEdit,outputData_path,fileName,manualEditDateTime){
    if (!is.na(manualEdit)){
    if (manualEdit==1){
      editTime<-file.info(paste0(outputData_path,fileName))$mtime
    }else{
      editTime<-manualEditDateTime
    }
    }else{
      editTime<-manualEditDateTime
    }
  
    return(editTime)
  }
  
  eval(parse(text=paste0("outBogen<-openxlsx::read.xlsx(paste0(outputData_path,fileName),sheet='",p,"',colNames=FALSE)")))
  outBogen<-outBogen[which(outBogen[[1]]==readDataName):nrow(outBogen),]
  outBogen<-outBogen[3:which(is.na(outBogen[[2]]))[2]-1,]
  names(outBogen)<-outBogen[1,]
  outBogen<-outBogen[2:nrow(outBogen),]
  outBogen<-outBogen[,!is.na(names(outBogen))]
  
  outBogen<-suppressWarnings(convertClass(data,outBogen))
  
  #add fileName and lastEditedDate
  if (!"manualEditFileName" %in% names(data)){
    data$manualEditFileName<-character(nrow(data))
    data$manualEditDateTime<-as.POSIXct(rep("2014-10-27 18:11:36 PDT",nrow(data)))
    data$manualEditDateTime<-ifelse(data$manualEditDateTime=="2014-10-27 18:11:36 PDT",as.POSIXct(NA),data$manualEditDateTime)
    data$manualEditDateTime<-as.POSIXct(data$manualEditDateTime,origin = "1960-01-01")
  }
  if (!"manualEditFileName" %in% names(outBogen)){
    outBogen$manualEditFileName<-character(nrow(outBogen))
    outBogen$manualEditDateTime<-as.POSIXct(rep("2014-10-27 18:11:36 PDT",nrow(outBogen)))
    outBogen$manualEditDateTime<-ifelse(outBogen$manualEditDateTime=="2014-10-27 18:11:36 PDT",as.POSIXct(NA),outBogen$manualEditDateTime)
    outBogen$manualEditDateTime<-as.POSIXct(outBogen$manualEditDateTime,origin = "1960-01-01")
  }
  
  outBogen<-outBogen %>% mutate(manualEditFileName = ifelse(manualEdit==1,paste0(outputData_path,fileName),manualEditFileName))
  outBogen<-outBogen %>% rowwise() %>% mutate(manualEditDateTime = getEditTime(manualEdit,outputData_path,fileName,manualEditDateTime))
  
  
  #replace plants with manual edits
  if (!all(is.na(data[data$Plant.Code %in% outBogen$Plant.Code,][which(outBogen$manualEdit==1),]))){
    if (nrow(data[data$Plant.Code %in% outBogen$Plant.Code,])==nrow(outBogen)){
     data[data$Plant.Code %in% outBogen$Plant.Code,][which(outBogen$manualEdit==1),]<-outBogen[outBogen$manualEdit==1,] 
    }else{
      data<-data[!data$Plant.Code %in% outBogen$Plant.Code,]
      data<-rbind(data,outBogen)
    } 
    
    
  }else{
    data<-rbind(data,outBogen[outBogen$manualEdit==1,])
  }
  return(data)
}