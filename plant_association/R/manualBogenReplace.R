manualBogenReplace<-function(outputData_path, fileName, data.list, replaceTables,lastReplace){
  
  #get data
  plantList<-openxlsx::read.xlsx(paste0(outputData_path,fileName),sheet="PlantList")
  plantList<-na.omit(plantList$PlantList)
  
  if("sheet3_key" %in% replaceTables | "sheet4_key" %in% replaceTables){
    #plantList<-unique(data.list$manualPlants$Plant.Code)  

    if("sheet3_key" %in% replaceTables){
    sheet3_key<-data.list$sheet3_key}
  if("sheet4_key" %in% replaceTables){
    sheet4_key<-data.list$sheet4_key}
  
    }else{#bogencoo
    #plantList<-unique(data.list$manualBogencoo$Plant.Code)
      
     if ("bogencoo.key" %in% replaceTables){
     bogencoo.key<-data.list$bogencoo.key  
    }
    if ("combogencoo_cooly_type" %in% replaceTables){
      combogencoo_cooly_type<-data.list$combogencoo_cooly_type
    }
  }

  
  
  #loop through plants
  for (p in plantList){
    if("sheet3_key" %in% replaceTables){
      sheet3_key<-readCompiled(outputData_path,fileName,p,data=sheet3_key,readDataName="auto_sheet3_key")
      
      
    }#end sheet3_key
    
    if("sheet4_key" %in% replaceTables){
      sheet4_key<-readCompiled(outputData_path,fileName,p,data=sheet4_key,readDataName="auto_sheet4_key")
      
      
    }#end sheet4_key 
    
    if ("bogencoo.key" %in% replaceTables){
      bogencoo.key<-readCompiled(outputData_path,fileName,p,data=bogencoo.key,readDataName="auto_bogencoo.key")
      
      
    }
    
    if ("combogencoo_cooly_type" %in% replaceTables){
      combogencoo_cooly_type<-readCompiled(outputData_path,fileName,p,data=combogencoo_cooly_type,
                                           readDataName="combogencoo_cool_type_key")
      
      
    }
    
  }#end for p
  
  if("sheet3_key" %in% replaceTables){
    if (lastReplace){
      names(sheet3_key)[names(sheet3_key)=="manualEdit"]<-"manualBogenEdit"
      names(sheet3_key)[names(sheet3_key)=="manualEditFileName"]<-"manualBogenEditFileName"
      names(sheet3_key)[names(sheet3_key)=="manualEditDateTime"]<-"manualBogenEditDateTime"
    }  
  }
  if("sheet4_key" %in% replaceTables){
    if (lastReplace){
      names(sheet4_key)[names(sheet4_key)=="manualEdit"]<-"manualBogenEdit"
      names(sheet4_key)[names(sheet4_key)=="manualEditFileName"]<-"manualBogenEditFileName"
      names(sheet4_key)[names(sheet4_key)=="manualEditDateTime"]<-"manualBogenEditDateTime"
    }  
  }
  if ("bogencoo.key" %in% replaceTables){
    if (lastReplace){
      names(bogencoo.key)[names(bogencoo.key)=="manualEdit"]<-"manualBogencooEdit"
      names(bogencoo.key)[names(bogencoo.key)=="manualEditFileName"]<-"manualBogencooEditFileName"
      names(bogencoo.key)[names(bogencoo.key)=="manualEditDateTime"]<-"manualBogencooEditDateTime"
    }
  }
  
  if ("combogencoo_cooly_type" %in% replaceTables){
    if (lastReplace){
      names(combogencoo_cooly_type)[names(combogencoo_cooly_type)=="manualEdit"]<-"manualBogencooEdit"
      names(combogencoo_cooly_type)[names(combogencoo_cooly_type)=="manualEditFileName"]<-"manualBogencooEditFileName"
      names(combogencoo_cooly_type)[names(combogencoo_cooly_type)=="manualEditDateTime"]<-"manualBogencooEditDateTime"
    }
  }
  
  inNamedList<-ifelse(length(replaceTables)>1,paste(replaceTables,collapse=","),replaceTables)
  eval(parse(text=paste0("out.keys.list<-named.list(",inNamedList,")")))
  return(out.keys.list)
}