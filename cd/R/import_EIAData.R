


import_EIAData <- function(path_InputData_Metafile, path_EIAInputData, outputCSV, path_outputCSV){

  
  #Define variables and import CSV
  meta <- openxlsx::read.xlsx(path_InputData_Metafile)
  meta$InputSheetName<-ifelse(is.na(meta$InputSheetName),"",meta$InputSheetName)
  #meta<-meta %>% filter(InputSheetName!="Generator")
  colNames <- meta$scriptColnames
  tableNames <- vector(mode = "list", length = 0)
  sheetColNames <- vector(mode = "list", length = 0)
  uniques <- vector(mode = "list", length = 0)
  uniques <- append(uniques, meta$InputFile[match(unique(na.omit(meta$InputFile)), 
                                                  na.omit(meta$InputFile))])
  #uniques<-list(uniques[[5]])
  
  sheetsUni <- vector(mode = "list", length = 0)
  sheetsUni <- append(sheetsUni, meta$InputSheetName[match(unique(meta$InputSheetName), 
                                                    meta$InputSheetName)])
  #sheetsUni<-list(sheetsUni[[3]])
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
    sheetColNames <- vector(mode = "list", length = 0)
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
      if (tolower(tools::file_ext(paste(path_EIAInputData, "/", uniques[f], sep=""))) %in% c("xls","xlsm")){
        sheetName <- readxl::excel_sheets(paste(path_EIAInputData, "/", uniques[f], sep=""))
      }else if(tolower(tools::file_ext(paste(path_EIAInputData, "/", uniques[f], sep="")))=="dbf"){
        sheetName<-""
      }else{
        sheetName <- openxlsx::getSheetNames(paste(path_EIAInputData, "/", uniques[f], sep=""))
      }
      
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
    # for (file in 1:length(uniques)){
    #   
    #   submeta<-meta %>% filter(InputFile==uniques[file])
    for (f in 1:length(unique(meta$scriptTableName))){
      submeta2 <- meta[which(meta$scriptTableName == unique(meta$scriptTableName)[f]),]

      for (s in 1:length(unique(submeta2$InputColumnName))){
        
        if(any(sheetColNames %in% submeta2$scriptTableName[s]) != TRUE){
          if (tolower(tools::file_ext(paste(path_EIAInputData, "/", submeta2$InputFile[s], sep=""))) %in% c("xls","xlsm")){
          temp<-suppressMessages(suppressWarnings(readxl::read_excel(paste(path_EIAInputData, "/", submeta2$InputFile[s], sep=""),
                                                    sheet=submeta2$InputSheetName[s])))
          if (names(temp)[2]=="...2"){
            skipRow<-which(complete.cases(temp))[1]
            temp<-suppressMessages(suppressWarnings(readxl::read_excel(paste(path_EIAInputData, "/", submeta2$InputFile[s], sep=""),
                                                      sheet=submeta2$InputSheetName[s],skip=skipRow)))
            
          }
          
          temp<-as.data.frame(temp)
          }else if(tolower(tools::file_ext(paste(path_EIAInputData, "/", submeta2$InputFile[s], sep="")))=="dbf"){
            temp<-read.dbf(paste(path_EIAInputData, "/", submeta2$InputFile[s], sep=""),as.is = TRUE)
            }else{
          temp <- openxlsx::read.xlsx(paste(path_EIAInputData, "/", submeta2$InputFile[s], sep=""), 
                                      submeta2$InputSheetName[s], colNames = FALSE, 
                                      skipEmptyRows = TRUE)
          temp <- temp[!(is.na(temp[,2]) | temp[,2]==""), ]
          rowHold <- sub("\n", " ", temp[1,])
          temp[1,] <- rowHold
          names(temp) <- gsub("\r","",as.character(unlist(temp[1,])))
          }
          k <- 1
          
          for (k in 1:length(meta$InputColumnName)){
            if ((any(toupper(colnames(temp)) %in% toupper(meta$InputColumnName[k]))) != TRUE &
                meta$scriptTableName[k]==unique(submeta2$scriptTableName)){
              rowCount <- append(rowCount, (k+1))
              c <- 4
            }

          }
          
          sheetColNames <- append(sheetColNames, submeta2$scriptTableName[s])
        }  
      }
    }
    #}
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
      if (tolower(tools::file_ext(paste(path_EIAInputData, "/", uniques[f], sep=""))) %in% c("xls","xlsm")){
        sheetName <- readxl::excel_sheets(paste(path_EIAInputData, "/", uniques[f], sep=""))
      }else if(tolower(tools::file_ext(paste(path_EIAInputData, "/", uniques[f], sep="")))=="dbf") {
       sheetName<-""
      }else{
        sheetName <- openxlsx::getSheetNames(paste(path_EIAInputData, "/", uniques[f], sep=""))
      }
      submeta <- meta[which(meta$InputFile == uniques[f]),]
      for (s in 1:length(unique(submeta$scriptTableName))){
        print(unique(submeta$scriptTableName)[s])
       # if(meta$InputFile[s] == uniques[f] && (any(tableNames %in% meta$scriptTableName[s])) != TRUE){
        submeta2 <- submeta[which(submeta$scriptTableName == unique(submeta$scriptTableName)[s]),]
        if (tolower(tools::file_ext(paste(path_EIAInputData, "/", uniques[f], sep=""))) %in% c("xls","xlsm")){
          temp<-suppressMessages(suppressWarnings(readxl::read_excel(paste(path_EIAInputData, "/", uniques[f], sep=""),
                                                    sheet=unique(submeta2$InputSheetName))))
          if (names(temp)[2]=="...2"){
            skipRow<-which(complete.cases(temp))[1]
            temp<-suppressMessages(suppressWarnings(readxl::read_excel(paste(path_EIAInputData, "/", uniques[f], sep=""),
                                                      sheet=unique(submeta2$InputSheetName),skip=skipRow)))
            
          }
          
          temp<-as.data.frame(temp)
          #replace column names
          for (c in 1:length(unique(submeta2$InputColumnName))) {
            names(temp)[toupper(names(temp))==toupper(unique(submeta2$InputColumnName)[c])]<-unique(submeta2$scriptColnames)[c]
          }
        }else if(tolower(tools::file_ext(paste(path_EIAInputData, "/", uniques[f], sep="")))=="dbf"){
          temp<-read.dbf(paste(path_EIAInputData, "/", uniques[f], sep=""),as.is = TRUE)
          for (c in 1:length(unique(submeta2$InputColumnName))) {
            names(temp)[toupper(names(temp))==toupper(unique(submeta2$InputColumnName)[c])]<-unique(submeta2$scriptColnames)[c]
          }
        }else{
          temp <- openxlsx::read.xlsx(paste(path_EIAInputData, "/", uniques[f], sep=""), 
                                      unique(submeta2$InputSheetName), colNames = FALSE, 
                                      skipEmptyRows = TRUE)
          temp <- temp[!(is.na(temp[,2]) | temp[,2]==""), ]
          rowHold <- sub("\n", " ", temp[1,])
          temp[1,] <- rowHold
          c <- 1
          
          #replace column names
          for (c in 1:length(unique(submeta2$InputColumnName))) {
              temp[1,which(toupper(gsub("\r","",temp[1,])) == toupper(unique(submeta2$InputColumnName)[c]))] <- unique(submeta2$scriptColnames)[c]
          }
          names(temp) <- as.character(unlist(temp[1,]))
          temp <- temp[-1,]
        }
        
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
  openxlsx::saveWorkbook(metaBook, path_InputData_Metafile, overwrite = TRUE)
  return(inputData.list)
}
