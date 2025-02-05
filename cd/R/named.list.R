#'@title named.list
#'@description Supports the creation of a list object by specifying unquoted variable names in 
#'            the argument list. \\cr \\cr


named.list <- function(...) { 
  l <- setNames( list(...) , as.character( match.call()[-1]) ) 
  l
}
