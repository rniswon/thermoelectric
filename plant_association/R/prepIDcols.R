prepIDcols<-function(data,out_colNames,orig_colNames, flagnames){
  
  for (c in 1:length(out_colNames)){
    strOut<-paste0("data$",out_colNames[c]," <- as.character(data$",out_colNames[c],")")
    eval(parse(text=strOut))
    
    strFlag<-paste0("data$",flagnames[c],"<-sapply(data$",out_colNames[c],", function(x) 
                     ifelse(grepl('(?<![0-9])',x,perl=TRUE),'removeChar',ifelse(grepl(' ',x,perl=TRUE),
                    'removeSpace',NA)))")
    eval(parse(text=strFlag))
    
    strOrig<-paste0("data$",orig_colNames[c],"<-data$",out_colNames[c])
    eval(parse(text=strOrig))
    
    strOut<-paste0("data$",out_colNames[c]," <- gsub('(?<![0-9])', '',data$",out_colNames[c]," , perl = TRUE)")
    eval(parse(text = strOut))
    
    strOut<-paste0("data$",out_colNames[c]," <- gsub('[[:space:]]', '',data$",out_colNames[c],")")
    eval(parse(text = strOut))
    
    strOut<-paste0("data<-removeLeadZero(data,'",out_colNames[c],"','",flagnames[c],"')")
    eval(parse(text=strOut))
  }
  

  
return(data)
}