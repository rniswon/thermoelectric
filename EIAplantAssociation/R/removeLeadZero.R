#'@title removeLeadZero
#'@description removes leading zeros from character columns with IDs (i.e. Boiler.ID, 
#'Generator.ID, etc.).  This is necessary for boiler-generator associations because 
#'across EIA files there is a lack of consitency in ID names creating IDs that do not match
#'with the zeros included. \cr \cr
#'@param df data.frame with "Plant.Code" (required name), ID (name set by `idCol`), and flag 
#'(name set by `flagCol`) columns.
#'@param idCol character string indicating which column is the ID column from which leading
#'             zeros should be removed
#'@param flagCol character string indicating which column name in which to store the flag 
#'               information indicating that a zero was removed from the ID, can be output of 
#'               the `formatIDcol()` function.
#'@return data.frame with IDs with leading zeros removed and a flag column indicating
#'        when a removal has taken place by the string "removeZero"
#'@examples
#'df<-data.frame(Plant.Code=c(9999,9999,9999),ID=c("01","02","CT02"),flag=rep(NA,3))
#'removeLeadZero(df,idCol="ID",flagCol = "flag")
#'@export
#'
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
  
   if (idCol!="idCol"){
     replaceDF<-replaceDF[,!names(replaceDF) %in% c("idCol")] 
   }
   if (flagCol!="flagCol"){
    replaceDF<-replaceDF[,!names(replaceDF) %in% c("flagCol")] 
   }
  
  return(replaceDF)
}

