#'@title removeMissing
#'@description replace missing data with 0 and flag row as containing 
#'             missing data \cr \cr
#'Executed By: \itemize{\item import_sheet1_gen_and_fuel_data.R
#'                                        \item import_sheet4_gen_and_fuel_data.R} \cr
#'@param df data.frame in which to flag missing data
#'@param cols numeric vector of column indices to check for missing data
#'@param missingFlag character string indicating missing data
#'@return `df` data.frame with missing values flagged and replaced with zeros
#'@examples
#'removeMissing(sheet1_gen_fuel_data,cols = c(20:96))

removeMissing<-function(df,cols,missingFlag){
  #make missingFlag NA
  df2<-df
  df2[cols][df2[cols] == missingFlag] <- NA
  
  #create flag column for missing data
  df$missingData<-rowSums(is.na(df2[cols]))
  
  #replace missing data with 0's
  df[cols][df[cols] == missingFlag] <- 0
  return(df)
}

