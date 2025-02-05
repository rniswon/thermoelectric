#'@title formatIDcol
#'@description removes special characters, spaces, and leading zeros from character columns 
#'with IDs (i.e. Boiler.ID, Generator.ID, etc.).  This is necessary for boiler-generator 
#'associations because across EIA files there is a lack of consistency in ID names creating 
#'IDs that do not match with the these included. \cr \cr
#'@param df data.frame with "Plant.Code" (required name), ID (name set by `idCol`), and flag 
#'(name set by `flagCol`) columns.
#'@param idCol character string indicating which column is the ID column from which leading
#'             zeros should be removed
#'@param flagColname character string indicating name of column in which to store the flag 
#'                  information indicating that the ID has been altered
#'@param originalColname character string indicating name of column in which to store the 
#'                       original un-altered EIA ID 
#'@return data.frame with IDs with special characters, spaces, and leading zeros removed and a 
#'        flag column indicating what type of removal has taken place ("removeSpace", 
#'        "removeChar" or "removeZero")
#'@examples
#'df<-data.frame(Plant.Code=c(9999,9999,9999,9999),
#'               ID=c("01","02","CT 02","GT-3"),
#'               flag=rep(NA,4))
#'formatIDcol(df,idCol="ID",flagColname = "flag",originalColname="orig_ID")
#'@export

formatIDcol<-function(df,idCol,flagColname,originalColname){
  #assign general name to ID column
  eval(parse(text=paste0("df$idCol<-df$",idCol)))
  
  #create flag column
   df$flagCol<-sapply(df$idCol, function(x) ifelse(grepl("(?<![0-9])",x,perl=TRUE),"removeChar",
                                                                      ifelse(grepl(" ",x,perl=TRUE),"removeSpace",NA)))
  #save original IDs
   eval(parse(text=paste0("df$",originalColname,"<-df$idCol")))
  
  #remove special characters, spaces, and/or leading zeros
  df$idCol <- gsub("(?<![0-9])", "",df$idCol , perl = TRUE)
  df$idCol <- gsub("[[:space:]]", "",df$idCol ) 
  
  #remove leading zeros
  df<-removeLeadZero(df,"idCol","flagCol")
  
  #assign user names
  eval(parse(text=paste0("df$",idCol,"<-df$idCol")))
  eval(parse(text=paste0("df$",flagColname,"<-df$flagCol")))
  
  #remove general names
  df<-df[,!names(df) %in% c("idCol","flagCol")]
  return(df)
}

