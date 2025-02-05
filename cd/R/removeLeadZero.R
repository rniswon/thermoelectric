#'@title removeLeadZero
#'@description removes leading zeros from character columns with IDs that do not match
#'            across EIA files with the zeros included.  Removes only leading zeros with 
#'            IDs that DO NOT have matching IDs in the same file without zeros. \cr \cr
#'Executed By: bogen_assocv_v2.R \cr
#'@param df data.frame with Plant.Code, ID, and flag columns
#'@param idCol character string indicating which column is the ID column from which leading
#'             zeros should be removed
#'@param flagCol character string indicating which column stores the flag information indicating
#'               that a zero was removed from the ID
#'@return `replaceDF` data.frame with IDs with leading zeros removed and a flag column indicating
#'                    when a removal has taken place
#'@examples
#'bf.923<-removeLeadZero(bf.923,"Boiler.ID","flagBoiler.ID_bf.923")

removeLeadZero<-function(df,idCol,flagCol){
  eval(parse(text=paste0("df$idCol<-df$",idCol)))
  eval(parse(text=paste0("df$flagCol<-df$",flagCol)))
  
  replaceDF<-df[0,]
  for (p in unique(df$Plant.Code)){
    subdf<-df[df$Plant.Code==p,]
    for (i in 1:length(subdf$idCol)){
      testReplace<-gsub("^0+", "",subdf$idCol[i] , perl = TRUE)
      if (!testReplace %in% subdf[subdf$flagCol!="removeZero",]$idCol & grepl("^0+",subdf$idCol[i],perl=TRUE)){
        subdf$idCol[i]<-testReplace
        subdf$flagCol[i]<-"removeZero"
      }
    }
     replaceDF<-rbind(replaceDF,subdf)
  }
  
  eval(parse(text=paste0("replaceDF$",idCol,"<-replaceDF$idCol")))
   eval(parse(text=paste0("replaceDF$",flagCol,"<-replaceDF$flagCol")))
  
  replaceDF<-replaceDF[,!names(replaceDF) %in% c("idCol","flagCol")]
  return(replaceDF)
}

