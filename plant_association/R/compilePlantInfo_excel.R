
compilePlantInfo_excel<-function(outputPath,plantList,outTables,shiftCols,inputData_path,fileComment="",years=NA,overwrite=T){
 
  if (overwrite==FALSE & file.exists(outputPath)){
    message(paste0("File Exists : ",outputPath,"\nChoose new file location OR set overwrite=TRUE"))
  }else{ 
    if (is.na(years)){
      years<-1
    }
  wb<-openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,sheetName="PlantList")
  openxlsx::writeData(wb,"PlantList",data.frame(PlantList = plantList),startRow = 1,rowNames = F)
  openxlsx::addStyle(wb,"PlantList",rows=1,cols=1,
           style=openxlsx::createStyle(fgFill = "lightskyblue1"))
  year<-unique(outTables$auto_sheet3_key$YEAR)
  openxlsx::writeData(wb,"PlantList",year,startRow = 1,startCol=2,rowNames = F)
  openxlsx::addStyle(wb,"PlantList",rows=1,cols=2,
                     style=openxlsx::createStyle(fgFill = "#00FF99",fontColour = "#FF0000",textDecoration="bold"))
  
  if (fileComment!=""){
    fileCommentOrig<-fileComment
   fileComment<-breakStr(fileComment,62,"/n")
   fileComment<-strsplit(fileComment,"/n")[[1]]
    fileComment<-data.frame(fileComment=fileComment)
    
    rows<-seq(3,nrow(fileComment)+3,1)
    cols<-seq(5,62/9+5,1)
    allRows<-numeric(0)
    for (r in rows){
        allRows<-c(allRows,rep(r,length(cols)))
    }
    allCols<-rep(cols,length(rows))
    
    names(fileComment)<-"FileComment"
   openxlsx::writeData(wb,"PlantList",fileComment,startRow = 2,startCol=5,rowNames = F) 
    openxlsx::addStyle(wb,"PlantList",rows=2,cols=seq(5,62/9+5,1),
                       style=openxlsx::createStyle(fgFill = "#FCD5B4"))
    openxlsx::addStyle(wb,"PlantList",rows=allRows,
                       cols=allCols,
                       style=openxlsx::createStyle(fgFill = "#FCD5B4"))
  }
  

  #add ColumnLegend_metafile
  flagData<-openxlsx::read.xlsx(paste0(inputData_path,"/ColumnLegend_metafile.xlsx"),sheet="Flags")
  flagDatatop<-flagData[1:33,]
  flagDatabottom<-flagData[34:nrow(flagData),]
  flagDatatop<-rbind(flagDatatop,rep("",length(flagData)))
  flagData<-rbind(flagDatatop,flagDatabottom)
  openxlsx::addWorksheet(wb,sheetName="ColumnLegend_metafile")
  openxlsx::writeData(wb,"ColumnLegend_metafile",flagData,startRow = 1,rowNames = F)
  openxlsx::addStyle(wb,"ColumnLegend_metafile",rows=c(1,2,8,16,17,32),cols=c(3,1,1,1,1,1),
                     style=openxlsx::createStyle(fgFill = "#C00000",fontColour = "white"))
  openxlsx::addStyle(wb,"ColumnLegend_metafile",rows=c(1,10,15,25,26,27,28,29,30),cols=c(4,1,1,1,1,1,1,1,1),
                     style=openxlsx::createStyle(fgFill = "#92D050"))
  openxlsx::addStyle(wb,"ColumnLegend_metafile",rows=c(36,36),cols=c(1,2),
                     style=openxlsx::createStyle(fgFill = "#BFBFBF",textDecoration = "bold"))
  
  outTables_orig<-outTables
  for (y in years){
    if (y!=1){
      outTables<-outTables_orig[[which(years==y)]]
      year<-y
    }
  for (p in plantList){
    startRow<-1
    if (y==1){
      sheetName<-as.character(p)
    }else{
      sheetName<-paste0(as.character(p),"_",y)
    }
    openxlsx::addWorksheet(wb,sheetName=sheetName) 
    #header
    headerSub<-data.frame(Plant.Code=p)
    for (t in 1:length(names(outTables))){
      tbl<-outTables[[t]] %>% filter(Plant.Code==p)
      countRows<-data.frame(temp = nrow(tbl))
      names(countRows)<-names(outTables)[t]
      headerSub<-cbind(headerSub,countRows)
    }
    rownames(headerSub)<-"#Rows"
    addStyle<-openxlsx::addStyle(wb,sheetName,rows=startRow,cols=1:(length(names(outTables))+2),
                       style=openxlsx::createStyle(fgFill = "yellow"))
    openxlsx::writeData(wb,sheetName,headerSub,startRow = startRow,rowNames = T)
    openxlsx::writeData(wb,sheetName,year,startRow = startRow,startCol=1,rowNames = F)
    openxlsx::addStyle(wb,sheetName,rows=startRow,cols=1,
                       style=openxlsx::createStyle(fgFill = "#00FF99",fontColour = "#FF0000",textDecoration="bold"))
    startRow<-startRow+2 
    

    for (t in 1:length(names(outTables))){
      openxlsx::writeData(wb,sheetName,names(outTables)[t],startRow = startRow)
      openxlsx::addStyle(wb,sheetName,rows=startRow,cols=1:100,
               style=openxlsx::createStyle(fgFill = "lightskyblue1"))
      openxlsx::writeData(wb,sheetName,year,startRow = startRow,startCol=5,rowNames = F)
      openxlsx::addStyle(wb,sheetName,rows=startRow,cols=5,
                         style=openxlsx::createStyle(fgFill = "#00FF99",fontColour = "#FF0000",textDecoration="bold"))
      if (!names(outTables)[t] %in% c("auto_sheet3_key","auto_sheet4_key","auto_bogencoo.key","combogencoo_cool_type_key")){
        openxlsx::writeData(wb,sheetName,outTables[[t]] %>% filter(Plant.Code==p),startRow = startRow+1,startCol = shiftCols+1) 
      }else{
      openxlsx::writeData(wb,sheetName,outTables[[t]] %>% filter(Plant.Code==p),startRow = startRow+1)
      }
      startRow<-startRow+1+nrow(outTables[[t]] %>% filter(Plant.Code==p))+1
      }#for each table
    
    
    openxlsx::addStyle(wb,sheetName,rows=startRow,cols=1:100,style=openxlsx::createStyle(fgFill = "black"))
    startRow<-startRow+1
    
  }#for each plant
  }#years
  
  openxlsx::saveWorkbook(wb,file = outputPath,overwrite = TRUE)
  }#if overwrite=TRUE or file does not exist
}#end function
