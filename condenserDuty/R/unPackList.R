#'@title unPackList
#'@description creates individual variables from list objects and places them in the 
#'            `parent.frame()` environment \\cr \\cr
#'@param lists list of variables to unpack into the `parent.frame()`
#'@param parentObj list of parent objects that `lists` are part of, if NA no parent object
#'@export

unPackList<-function(lists,parentObj, env = parent.frame()){
  for (l in 1:length(lists)){
    sublist<-lists[[l]]
    
    
    if (length(parentObj[[l]])!=1){
      #variables from data frame
      parent<-parentObj[[l]]
      for (i in 1:length(sublist)) {
        dname <- paste0("parent$",sublist[i]) 
        x1name <- paste0(sublist[i])
        if((x1name %in% names(parent)) == TRUE) {
          assign(sublist[i],eval(parse(text=dname)),env = parent.frame())
        }
      }
      
    }else{
      #objects from list
      for(i in 1:length(sublist)){
        tempobj=sublist[[i]]
        assign(names(sublist)[[i]],tempobj,env = parent.frame())
        
      }
    }
    
    
  }
}
