


import_EIAData <- function(path_InputData_Metafile, path_EIAInputData, outputCSV, path_outputCSV){

  
  #Define variables and import CSV
  meta <- openxlsx::read.xlsx(path_InputData_Metafile)
  colNames <- meta$scriptColnames
  tableNames <- vector(mode = "list", length = 0)
  sheetColNames <- vector(mode = "list", length = 0)
  uniques <- vector(mode = "list", length = 0)
  uniques <- append(uniques, meta$InputFile[match(unique(na.omit(meta$InputFile)), 
                                                  na.omit(meta$InputFile))])
  
  
  sheetsUni <- vector(mode = "list", length = 0)
  sheetsUni <- append(sheetsUni, meta$InputSheetName[match(unique(meta$InputSheetName), 
                                                    meta$InputSheetName)])
  inputData.list <- vector(mode = "list", length = 0)
  
  #Create worksheet and style type in case of errors
  metaBook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(metaBook, sheetName = "Sheet1")
  openxlsx::writeData(metaBook,"Sheet1",meta,startRow = 1)
  highlight <- openxlsx::createStyle(fontColour = "#000000", fgFill = "#FFFF00")
  
  #Check for errors across the entirety of the crosswalk
  f <- 1
  s <- 1
  c <- 1
  rowCount <-  vector(mode = "list", length = 0)
  for (f in 1:length(uniques)){
    highlightCheck <- paste(path_EIAInputData, "/", uniques[f], sep="")
    highlightCheck <- file.access(highlightCheck)
    if (highlightCheck[[1]] != 0) {
      rowCount <- append(rowCount, (which(meta$InputFile == uniques[f]) + 1))
    } 
  }
  if (length(rowCount) < 1){
    f <- 1
    s <- 1
    for (f in 1:length(uniques)){
      sheetName <- openxlsx::getSheetNames(paste(path_EIAInputData, "/", uniques[f], sep=""))
      for (s in 1:length(meta$InputSheetName)){
        if ((meta$InputFile[s] == uniques[f]) && (any(toupper(sheetName) %in% toupper(meta$InputSheetName[s])) != TRUE)){
          rowCount <- append(rowCount, (s+1))
          c <- 2
        } 
      }
    }
  }
  if (length(rowCount) < 1){
    f <- 1
    s <- 1
    for (f in 1:length(sheetsUni)){
      for (s in 1:length(meta$InputColumnName)){
        if(meta$InputSheetName[s] == sheetsUni[f] && (any(sheetColNames %in% meta$InputSheetName[s])) != TRUE){
          temp <- openxlsx::read.xlsx(paste(path_EIAInputData, "/", meta$InputFile[s], sep=""), 
                                      meta$InputSheetName[s], colNames = FALSE, 
                                      skipEmptyRows = TRUE)
          temp <- temp[!(is.na(temp[,2]) | temp[,2]==""), ]
          rowHold <- sub("\n", " ", temp[1,])
          temp[1,] <- rowHold
          names(temp) <- as.character(unlist(temp[1,]))
          k <- 1
          for (k in 1:length(meta$InputColumnName)){
            if ((any(toupper(colnames(temp)) %in% toupper(meta$InputColumnName[k]))) != TRUE && meta$InputSheetName[k] == sheetsUni[f]){
              rowCount <- append(rowCount, (k+1))
              c <- 4
            }
          }
          sheetColNames <- append(sheetColNames, meta$InputSheetName[s])
        }  
      }
    }
  }
  #Check for incorrectly referenced xlsx data documents and Save
  if (length(rowCount) > 0){
    #openxlsx::addStyle(metaBook, sheet = 1, highlight, rows = rowCount, cols = c)
    openxlsx::addStyle(metaBook,"Sheet1",rows=rowCount,cols=c,style=openxlsx::createStyle(fgFill = "yellow"))
    openxlsx::saveWorkbook(metaBook, path_InputData_Metafile, overwrite = TRUE)
    print("Please review the Input Data Metafile for highlighted errors.")
    stopifnot(length(rowCount) > 0)
  } else {
    
    #Nested loop to extract data and place it into one R list of Data Frames
    f <- 1
    s <- 1
    rowCount <- vector(mode = "list", length = 0)
    tableNames <- vector(mode = "list", length = 0)
    for (f in 1:length(uniques)){
      sheetName <- openxlsx::getSheetNames(paste(path_EIAInputData, "/", uniques[f], sep=""))
      submeta <- meta[which(meta$InputFile == uniques[f]),]
      for (s in 1:length(unique(submeta$InputSheetName))){
       # if(meta$InputFile[s] == uniques[f] && (any(tableNames %in% meta$scriptTableName[s])) != TRUE){
          temp <- openxlsx::read.xlsx(paste(path_EIAInputData, "/", uniques[f], sep=""), 
                                      unique(submeta$InputSheetName)[s], colNames = FALSE, 
                                      skipEmptyRows = TRUE)
          temp <- temp[!(is.na(temp[,2]) | temp[,2]==""), ]
          rowHold <- sub("\n", " ", temp[1,])
          temp[1,] <- rowHold
          c <- 1
          submeta2 <- submeta[which(submeta$InputSheetName == unique(submeta$InputSheetName)[s]),]
          #replace column names
          for (c in 1:length(unique(submeta2$InputColumnName))) {
              temp[1,which(toupper(temp[1,]) == toupper(unique(submeta2$InputColumnName)[c]))] <- unique(submeta2$scriptColnames)[c]
          }
          names(temp) <- as.character(unlist(temp[1,]))
          temp <- temp[-1,]
          #convert classes
          temp<- lapply(temp, function(x) type.convert(x, as.is=TRUE))
          temp<-as.data.frame(temp,stringsAsFactors=FALSE)
          #apply import functions
          if (!is.na(unique(submeta$scriptImportFunc)[s])){
          eval(parse(text = paste0("temp<-",unique(submeta$scriptImportFunc)[s],"(temp)")))
            }
          inputData.list <- append(inputData.list, list(temp))
          tableNames <- append(tableNames, unique(submeta$scriptTableName)[s])
        } 
      #}
    }
    names(inputData.list) <- c(tableNames)
  
    #Check for saving the CSV or just returning the list  
    if (outputCSV == TRUE){
      if(!dir.exists(path_outputCSV)){
        dir.create(path_outputCSV)
      }
      x <- 1
      for(x in 1:length(inputData.list)){
        temp <- as.data.frame(inputData.list[x])
        names(temp)<-names(inputData.list[[x]])
        write.csv(temp, file = paste(path_outputCSV, "/", names(inputData.list[x]), ".csv", sep = ""), row.names = F)
      }
    }
  }
  return(inputData.list)
}
