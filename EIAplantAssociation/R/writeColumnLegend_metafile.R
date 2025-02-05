#'@title writeColumnLegend_metafile
#'@description creates a formatted Microsoft Excel® file with the flags described in the 
#'`data(ColumnLegend_metafile)` object associated with the creation of boiler-generator 
#'(bogen) associations using the `bogen_associate()` function. \cr \cr
#'@param outputDir character string indicating directory in which to save the 
#'"ColumnLegend_metafile.xlsx" formatted file.
#'@examples
#'outputDir<-tempDir()
#'writeColumnLegend_metafile(outputDir)
#'@export

writeColumnLegend_metafile<-function(outputDir){
  
  if (!dir.exists(outputDir)){
    dir.create(file.path(outputDir), recursive=TRUE) 
  }
  #get ColumnLegend_metafile object
  utils::data("ColumnLegend_metafile")
 
  #create new workbook
  wb<-openxlsx::createWorkbook() 
  
  flagData<-ColumnLegend_metafile
  #add formatted ColumnLegend_metafile
  flagDatatop<-flagData[1:35,]
  flagDatabottom<-flagData[36:nrow(flagData),]
  flagDatatop<-rbind(flagDatatop,rep("",length(flagData)))
  flagData<-rbind(flagDatatop,flagDatabottom)
  openxlsx::addWorksheet(wb,sheetName="ColumnLegend_metafile")
  openxlsx::writeData(wb,"ColumnLegend_metafile",flagData,startRow = 1,rowNames = F)
  
  #apply dark red fill to flags that indicate automatic removal from bogen key
  openxlsx::addStyle(wb,"ColumnLegend_metafile",rows=c(1,2,8,16,17),cols=c(3,1,1,1,1),
                     style=openxlsx::createStyle(fgFill = "#C00000",fontColour = "white"))
  
  #apply green fill to flags that indicate manual bogen checks
  openxlsx::addStyle(wb,"ColumnLegend_metafile",rows=c(1,10,15,26,27,28,29,30,32),cols=c(4,1,1,1,1,1,1,1,1),
                     style=openxlsx::createStyle(fgFill = "#92D050"))
  
  #apply grey to uncounted flags
  openxlsx::addStyle(wb,"ColumnLegend_metafile",rows=c(38,38),cols=c(1,2),
                     style=openxlsx::createStyle(fgFill = "#BFBFBF",textDecoration = "bold"))
  
  #save workbook
  openxlsx::saveWorkbook(wb,file = paste0(outputDir,.Platform$file.sep,"ColumnLegend_metafile.xlsx"),overwrite = TRUE)
  message(paste0("ColumnLegend_metafile.xlsx has been saved here:",paste0(outputDir,.Platform$file.sep,"ColumnLegend_metafile.xlsx")))
}