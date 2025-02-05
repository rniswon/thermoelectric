#'@importFrom magrittr %>%
#'@title import_EIAData
#'@description import and format EIA data using a user generated crosswalk or \code{\link{crosswalk2015}}.
#'             To format data for EIAplantAssociation and/or condenserDuty packages all 
#'             "scriptImportFunc", "scriptTableName" and "scriptColnames" present in 
#'             \code{\link{crosswalk2015}} must be present, and all column names of the 
#'             \code{\link{crosswalk2015}} table must be present.
#'@param path_InputData_Metafile Character string indicating path to crosswalk file must be in "xlsx" 
#'                               format. If `useStandardCrosswalk==TRUE` then this is the path to which 
#'                               the crosswalk will be saved for further edit by the user.
#'@param path_EIAInputData Character string indicating directory in which EIA data files have been
#'                         extracted from zip files.  If using the \code{\link{eia_webpull}} function, the 
#'                         directory will be specified by the `eia_year`
#'@param outputCSV TRUE/FALSE indicating whether or not to output the formatted EIA data tables as 
#'                 csv files
#'@param outputCSV TRUE/FALSE indicating whether or not to save formatted data as individual csv files
#'@param path_outputCSV Path to directory in which to save the formatted EIA data as csv files.
#'@param useStandardCrosswalk TRUE/FALSE indicating whether the \code{\link{crosswalk2015}} file should be
#'                            used to start the crosswalk development process.  After the first run the 
#'                            resulting crosswalk will be saved as `path_InputData_Metafile`.  Subsequent 
#'                            runs of import_EIAData should have `useStandardCrosswalk` as FALSE.
#'@param prepCondenserDuty TRUE/FALSE indicating whether checks should be made for data required to
#'                         execute the `EIAplantAssocation` and/or `condenserDuty` packages 
#'@param eia_year numeric value indicating the year of EIA data being formatted. ONLY required if 
#'                `prepCondenserDuty` is TRUE                                                    
#'@details If the crosswalk is complete and all tables have been imported and formatted, 
#'        `import_EIAData()` will return a list object containing all tables imported.  If the crosswalk
#'        is imcomplete/incompatible with the EIA data stored in `path_EIAInputData` the current crosswalk
#'        file will be modified highlighting missing data and a message will appear indicating that the
#'        crosswalk file should be edited.  The `import_EIAData()` should be run repeatedly until a list
#'        of tables is returned.  It is recommended that the resulting crosswalk be saved for future use.
#'@return `inputData.list` list of data.frames with standardized column names for columns found in the 
#'                         crosswalk.  If `prepCondenserDuty=TRUE` the inputData.list will be a list of 
#'                         the 11 data.frames shown below:\cr \cr
#'                        
#'\item{\code{bogen}}{data.frame of 7454 obs of 7 variables sourced from 2015 Form EIA-860 Data - Schedule 6A, 'Boiler / Generator Associations}
#'\item{\code{bocoo}}{data.frame of	3007 obs. of  7 variables sourced from 2015 Form EIA-860 Data - Schedule 6A, 'Boiler / Cooling System Associations}
#'\item{\code{gen_860}}{data.frame 20068 obs. of  72 variables sourced from 2015 Form EIA-860 Data - Schedule 3, 'Generator Data' (Operable Units Only)}
#'\item{\code{retiredGenerators}}{data.frame of 3629 obs. of  56 variables sourced from 2015 Form EIA-860 Data - Schedule 3, 'Generator Data' (Retired & Canceled Units Only)}
#'\item{\code{generation.data}}{data.frame of 4177 obs. of  28 variables sourced from 2015 Form EIA-923 Monthly Generating Unit Net Generation Time Series File}
#'\item{\code{boilerFuelData}}{data.frame	of 10001 obs. of  66 variables sourced from 2015 Form EIA-923 Monthly Boiler Fuel Consumption and Emissions Time Series File}
#'\item{\code{gen_fuel_data}}{data.frame of	12450 obs. of  98 variables sourced from 2015 Form EIA-923 Monthly Generation and Fuel Consumption Time Series File, 2015 Final Revision}  
#'\item{\code{boilerDesignData}}{data.frame of 4769 obs. of  33 variables sourced from 2015 Form EIA-860 Data - Schedule 6C, 'Boiler Information - Design Parameters}
#'\item{\code{cooling}}{data.frame of	1892 obs. of  42 variables sourced from 2015 Form EIA-860 Data - Schedule 6D, 'Cooling System Data}
#'\item{\code{plantInfo}}{data.frame of	8928 obs. of  36 variables sourced from 2015 EIA-860 Data - Plant Info" "Form EIA-860 Data - Plant Info}
#'\item{\code{cooling8D}}{data.frame of	17159 obs. of  23 variables sourced from 2015 EIA-923, Power Plant Operations Report}
#'
#'@examples
#'#set arguments
#'dest<-tempdir()
#'eia_year<-2015
#'path_InputData_Metafile<-paste0(dest,.Platform$file.sep,"UserControlCrosswalk2015.xlsx")
#'path_EIAInputData<-paste0(dest,.Platform$file.sep,eia_year)
#'outputCSV<-FALSE
#'path_outputCSV<-dest
#'
#'#use crosswalk2015 object
#'useStandardCrosswalk<-TRUE
#'prepCondenserDuty<-TRUE
#'
#'#pull data from web
#'eia_webpull(eia_year,dest)
#'
#'
#'#run import using crosswalk2015
#'inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,
#'                               path_outputCSV,prepCondenserDuty = TRUE,eia_year=eia_year,
#'                               useStandardCrosswalk=useStandardCrosswalk)
#'@export


import_EIAData <- function(path_InputData_Metafile, path_EIAInputData, outputCSV, path_outputCSV,
                           useStandardCrosswalk=F,prepCondenserDuty=F,eia_year=NA){

  
  #Import Crosswalk
  if (useStandardCrosswalk==F){
  meta <- openxlsx::read.xlsx(path_InputData_Metafile)  
  }else{
    utils::data("crosswalk2015")
    meta<-crosswalk2015
  }
  
  #make NA sheet names ""
  meta$InputSheetName<-ifelse(is.na(meta$InputSheetName),"",meta$InputSheetName)
  #get scriptColnames
  colNames <- meta$scriptColnames
  
  #create empty lists
  tableNames <- vector(mode = "list", length = 0)
  sheetColNames <- vector(mode = "list", length = 0)
  uniques <- vector(mode = "list", length = 0)
  
  # get vector of files to import
  uniques <- append(uniques, meta$InputFile[match(unique(stats::na.omit(meta$InputFile)), 
                                                  stats::na.omit(meta$InputFile))])

  sheetsUni <- vector(mode = "list", length = 0)
  #get vector of sheets to import
  sheetsUni <- append(sheetsUni, meta$InputSheetName[match(unique(meta$InputSheetName), 
                                                    meta$InputSheetName)])
  #create list for return
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
    highlightCheck <- paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep="")
    highlightCheck <- file.access(highlightCheck)
    if (highlightCheck[[1]] != 0) {
      rowCount <- append(rowCount, (which(meta$InputFile == uniques[f]) + 1))
    } 
  }
  if (length(rowCount) < 1){
    f <- 1
    s <- 1
    for (f in 1:length(uniques)){
      if (tolower(tools::file_ext(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""))) %in% c("xls","xlsm")){
        sheetName <- readxl::excel_sheets(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""))
      }else if(tolower(tools::file_ext(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep="")))=="dbf"){
        sheetName<-""
      }else{
        sheetName <- openxlsx::getSheetNames(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""))
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
    for (f in 1:length(unique(meta$scriptTableName))){
      submeta2 <- meta[which(meta$scriptTableName == unique(meta$scriptTableName)[f]),]

      for (s in 1:length(unique(submeta2$InputColumnName))){
        
        if(any(sheetColNames %in% submeta2$scriptTableName[s]) != TRUE){
          if (tolower(tools::file_ext(paste(path_EIAInputData, .Platform$file.sep, submeta2$InputFile[s], sep=""))) %in% c("xls","xlsm")){
          temp<-suppressMessages(suppressWarnings(readxl::read_excel(paste(path_EIAInputData, .Platform$file.sep, submeta2$InputFile[s], sep=""),
                                                    sheet=submeta2$InputSheetName[s])))
          if (names(temp)[2]=="...2"){
            skipRow<-which(stats::complete.cases(temp))[1]
            temp<-suppressMessages(suppressWarnings(readxl::read_excel(paste(path_EIAInputData, .Platform$file.sep, submeta2$InputFile[s], sep=""),
                                                      sheet=submeta2$InputSheetName[s],skip=skipRow)))
            
          }
          
          temp<-as.data.frame(temp)
          }else if(tolower(tools::file_ext(paste(path_EIAInputData, .Platform$file.sep, submeta2$InputFile[s], sep="")))=="dbf"){
            temp<-foreign::read.dbf(paste(path_EIAInputData, .Platform$file.sep, submeta2$InputFile[s], sep=""),as.is = TRUE)
            }else{
          temp <- openxlsx::read.xlsx(paste(path_EIAInputData, .Platform$file.sep, submeta2$InputFile[s], sep=""), 
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
    openxlsx::addStyle(metaBook,"Sheet1",rows=rowCount,cols=c,style=openxlsx::createStyle(fgFill = "yellow"))
    openxlsx::saveWorkbook(metaBook, path_InputData_Metafile, overwrite = TRUE)
    message("Please review the Input Data Metafile for highlighted errors.")
    #stopifnot(length(rowCount) > 0)
    return(meta)
  } else {
    #remove highlights from file
    openxlsx::saveWorkbook(metaBook, path_InputData_Metafile, overwrite = TRUE)
    #Nested loop to extract data and place it into one R list of Data Frames
    f <- 1
    s <- 1
    rowCount <- vector(mode = "list", length = 0)
    tableNames <- vector(mode = "list", length = 0)
    for (f in 1:length(uniques)){
      if (tolower(tools::file_ext(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""))) %in% c("xls","xlsm")){
        sheetName <- readxl::excel_sheets(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""))
      }else if(tolower(tools::file_ext(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep="")))=="dbf") {
       sheetName<-""
      }else{
        sheetName <- openxlsx::getSheetNames(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""))
      }
      submeta <- meta[which(meta$InputFile == uniques[f]),]
      for (s in 1:length(unique(submeta$scriptTableName))){
        print(unique(submeta$scriptTableName)[s])
        submeta2 <- submeta[which(submeta$scriptTableName == unique(submeta$scriptTableName)[s]),]
        if (tolower(tools::file_ext(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""))) %in% c("xls","xlsm")){
          temp<-suppressMessages(suppressWarnings(readxl::read_excel(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""),
                                                    sheet=unique(submeta2$InputSheetName))))
          if (names(temp)[2]=="...2"){
            skipRow<-which(stats::complete.cases(temp))[1]
            temp<-suppressMessages(suppressWarnings(readxl::read_excel(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""),
                                                      sheet=unique(submeta2$InputSheetName),skip=skipRow)))
            
          }
          
          temp<-as.data.frame(temp)
          #replace column names
          for (c in 1:length(unique(submeta2$InputColumnName))) {
            names(temp)[toupper(names(temp))==toupper(unique(submeta2$InputColumnName)[c])]<-unique(submeta2$scriptColnames)[c]
          }
        }else if(tolower(tools::file_ext(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep="")))=="dbf"){
          temp<-foreign::read.dbf(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""),as.is = TRUE)
          for (c in 1:length(unique(submeta2$InputColumnName))) {
            names(temp)[toupper(names(temp))==toupper(unique(submeta2$InputColumnName)[c])]<-unique(submeta2$scriptColnames)[c]
          }
        }else{
          temp <- openxlsx::read.xlsx(paste(path_EIAInputData, .Platform$file.sep, uniques[f], sep=""), 
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
          temp<- lapply(temp, function(x) utils::type.convert(x, as.is=TRUE))
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
        utils::write.csv(temp, file = paste(path_outputCSV, .Platform$file.sep, names(inputData.list[x]), ".csv", sep = ""), row.names = F)
      }
    }
  }
  openxlsx::saveWorkbook(metaBook, path_InputData_Metafile, overwrite = TRUE)
  
  
  if (prepCondenserDuty){
    utils::data("crosswalk2015")
    exceptions<-structure(list(scriptTableName = c("bogen", "bocoo", "gen_860", 
                                       "boilerDesignData", "cooling", "cooling", "cooling", "cooling8D", 
                                       "cooling8D", "cooling8D", "cooling8D", "cooling8D"), 
                               scriptColnames = c("Plant.Name", "Plant.Name", "Technology", "Plant.Name", 
                                                  "Plant.Name", "Water.Source.Code","Water.Type.Code", 
                                                  "Month", "Withdrawal_MG", "Discharge_MG", 
                                                  "Consumptive_MG", "Service_hrs")), 
                          row.names = c(NA, -12L), class = "data.frame")
    exceptions$FirstYearFound<-c(NA,NA,"2014",NA,NA,"2011","2011","2010","2012","2012","2012","2010")
    if (is.na(eia_year)){
      message("eia_year must be specified for prepCondenserDuty=TRUE")
      return(eia_year)
    }else{
     exceptions<-exceptions %>% dplyr::filter(FirstYearFound>=eia_year | is.na(FirstYearFound))
     test<-dplyr::anti_join(crosswalk2015,meta, by=c("scriptTableName","scriptColnames","scriptImportFunc"))
     test<-dplyr::anti_join(test,exceptions, by=c("scriptTableName","scriptColnames"))
     if (nrow(test)!=0){
     message(paste0("The following required tables/columns are NOT present in the ",path_InputData_Metafile))
     print(test %>% dplyr::select(InputSheetDescription,InputColumnDescription,scriptImportFunc,scriptTableName,scriptColnames))
     message("EIAplantAssociation and/or consenserDuty analysis CANNOT be completed")
     return(test %>% dplyr::select(InputSheetDescription,InputColumnDescription,scriptImportFunc,scriptTableName,scriptColnames))
     }
    }
    
    
  }
  
  return(inputData.list)
}
