#'@title splitPlant_id
#'@description 
#'@param df
#'@export


splitPlant_id<-function(df){
  df$Plant.Code<-sapply(df$Plant_id, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][1]))
  df$cooling<-sapply(df$Plant_id, function(x) strsplit(as.character(x),"\\^")[[1]][2])
  df$percentAllocation<-sapply(df$Plant_id, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][3]))
  df$flag_minMax<-sapply(df$Plant_id, function(x) as.logical(strsplit(as.character(x),"\\^")[[1]][4]))
  df<-df %>% ungroup()
  df<-df %>% select(Plant.Code,cooling,percentAllocation,flag_minMax,
                    all_of(names(df)[!names(df) %in% c("Plant.Code","Plant_id","cooling","percentAllocation","flag_minMax")]))
return(df)
}