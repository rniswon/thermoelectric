#'@title joinBogen
#'@description 
#'@param tbl
#'@param key
#'@param joinCols
#'@export


joinBogen<-function(tbl,key,joinCols){
  NAMES<-c(names(tbl),"bogen")
  tbl_bogen <- left_join(tbl, key[,which(!names(key) %in% names(tbl)[which(!names(tbl)==joinCols[1])])], by=joinCols[1])
  tbl_bogen <- tbl_bogen[!duplicated(tbl_bogen),]
  
  unMatch<-anti_join(tbl, key[,which(!names(key) %in% names(tbl)[which(!names(tbl)==joinCols[1])])], by=joinCols[1])
  if (nrow(unMatch)!=0 & length(joinCols)!=1){
    eval(parse(text=paste0("tbl_bogen2<-inner_join(unMatch, key[,which(!names(key) %in% 
                           names(tbl)[!names(tbl) %in% joinCols[2]])], by=c('",joinCols[1],"'='",joinCols[2],"'))")))
    tbl_bogen2 <- tbl_bogen2[!duplicated(tbl_bogen2),]
    
    eval(parse(text=paste0("unMatch2<-anti_join(unMatch, key[,which(!names(key) %in% 
                           names(tbl)[!names(tbl) %in% joinCols[2]])], by=c('",joinCols[1],"'='",joinCols[2],"'))")))

    if (joinCols[2] %in% names(key) & joinCols[2] %in% names(tbl) & nrow(unMatch2)!=0){
      tbl_bogen3<-inner_join(unMatch2, key[,which(!names(key) %in% 
                           names(tbl)[!names(tbl) %in% joinCols[2]])], by=joinCols[2])
      tbl_bogen3 <- tbl_bogen3[!duplicated(tbl_bogen3),] 
    }
    
    eval(parse(text=paste0("tbl_bogen2$",joinCols[2],"<-tbl_bogen2$",joinCols[1])))
    if (exists("tbl_bogen3")){
     eval(parse(text=paste0("tbl_bogen3$",joinCols[2],"<-tbl_bogen3$",joinCols[1]))) 
    }
    
    
    tbl_bogen2<-tbl_bogen2[names(tbl_bogen)]
    if (exists("tbl_bogen3")){
    tbl_bogen3<-tbl_bogen3[names(tbl_bogen)]
    }
    
    tbl_bogen<-anti_join(tbl_bogen,tbl_bogen2,by=joinCols[1])
    if (exists("tbl_bogen3")){
    tbl_bogen<-anti_join(tbl_bogen,tbl_bogen3,by=joinCols[1])
    }
    
    if (exists("tbl_bogen3")){
    tbl_bogen<-rbind(tbl_bogen,tbl_bogen2,tbl_bogen3)
    }else{
      tbl_bogen<-rbind(tbl_bogen,tbl_bogen2)
    }
    tbl_bogen <- tbl_bogen[!duplicated(tbl_bogen),]
    
  }
  tbl_bogen<-tbl_bogen[,names(tbl_bogen) %in% NAMES]
  return(tbl_bogen)
}