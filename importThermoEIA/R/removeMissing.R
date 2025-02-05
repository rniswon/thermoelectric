#'@title removeMissing
#'@description replace missing data with 0 and flag row as containing 
#'             missing data \cr \cr
#'@param df data.frame in which to flag missing data
#'@param cols numeric vector of column indices to check for missing data or 
#'            character vector of column names
#'@param missingFlag character string indicating missing data
#'@return `df` data.frame with missing values flagged and replaced with zeros
#'@examples
#'df<-data.frame(x = c(1,2,3),y=c(".",100,200))
#'removeMissing(df,cols=names(df),missingFlag = ".")
#'@export

removeMissing<-function(df,cols,missingFlag){
  #make missingFlag NA
  df2<-df
  
  if (class(cols)!="numeric"){
    cols<-which(names(df) %in% cols)
  }
  
  df2[cols][df2[cols] == missingFlag] <- NA
  
  #create flag column for missing data
  df$missingData<-rowSums(is.na(df2[cols]))
  
  #replace missing data with 0's
  df[cols][df[cols] == missingFlag] <- 0
  return(df)
}

