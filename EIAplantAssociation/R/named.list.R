#'@title named.list
#'@description Supports the creation of a list object by specifying unquoted object names in 
#'            the argument list. \\cr \\cr
#'@param ... unquoted object names to be compiled into a single list named with the object names.
#'@return list object with all ... objects named with object names
#'@examples
#'df<-data.frame(x=c(1,2,3),y=c(2,3,4))
#'myVector<-seq(1,200,1)
#'myList<-list(element1=data.frame(x=c(3,4,5)),setting="my custom list")
#'named.list(df,myVector,myList)
#'@export

named.list <- function(...) { 
  l <- setNames( list(...) , as.character( match.call()[-1]) ) 
  l
}
