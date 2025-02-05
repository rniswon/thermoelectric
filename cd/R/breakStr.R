#'@title breakStr
#'@description Breaks long character strings at `nBreak` number of characters 
#'with the character string indicated by `strbreak` \\cr \\cr
#'@param longStr long character string
#'@param nBreak number of characters in between breaks
#'@param strBreak character string to place at break (i.e. `"\n"` to enter line break)
#'@return `newstr` character string with breaks added
#'@examples
#'longStr<-"This is a very, very, very long character string that is too long for my output format.  This string needs to be broken into parts."
#'newstr<-breakStr(longStr,50,'\n')
#'cat(newstr)

breakStr<-function(longStr,nBreak,strBreak){ 
  if (nchar(longStr)>nBreak){
    spaces<-lapply(strsplit(longStr, ''), function(longStr) which(longStr == ' '))[[1]]
    seqSplit<-seq(1,max(spaces),nBreak)
    start<-2
    if (max(seqSplit)!=1){
      start<-2
      newstr<-character(0)
      for (s in seqSplit[2:length(seqSplit)]){
        splitSpace<-max(spaces[which(spaces<s)])
        newstr<-c(newstr,substr(longStr,start,splitSpace))
        start<-splitSpace+1
      }
      newstr[1]<-paste0(substr(longStr,1,1),newstr[1])
      newstr<-paste(c(newstr,substr(longStr,start,nchar(longStr))),collapse = strBreak)
      
    }else{
      newstr<-c(substr(longStr,1,max(spaces)),substr(longStr,max(spaces)+1,nchar(longStr)))
      newstr<-paste0(newstr, collapse = strBreak)
    }
  }else{
    newstr<-longStr
  }
  return(newstr)
}