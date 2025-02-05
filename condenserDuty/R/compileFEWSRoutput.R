#'@title compileFEWSRoutput
#'@description
#'@param FEWSRinputData_path
#'@param compileOutFEWSRpath
#'@param eia_year
#'@param types
#'
#'@export


compileFEWSRoutput<-function(FEWSRinputData_path,compileOutFEWSRpath,eia_year,
                             types=c("Big_Lake","Pond","River","Saline")){
#create output directory
  if (!exists(compileOutFEWSRpath)){
    dir.create(compileOutFEWSRpath)
    }
#get FEWSR output file names
  fileList<-list.files(FEWSRinputData_path,pattern = "xlsx",full.names = T)
fileList<-fileList[regexpr("FEWS",fileList)>0]
fileList<-fileList[regexpr(eia_year,fileList)>0]
#relavant data
  listFEWSR<-list(Med_Consump="Best Consumption Estimates",
                                Med_WD="Best Withdrawal Estimates",
                                Min_Consump="Min Consumpt with 22% cushion",
                                Min_WD="Estimated Min MGD Withdrawal",
                                Max_Consump="Max Consumpt with 22% cushion",
                                Max_WD ="Estimated Max MGD Withdrawal")
#types of FEWSR
  #types<-c("Big_Lake","Pond","River","Saline")
#read files
  for(t in types){
    subfileList<-fileList[regexpr(t,fileList)>0]
    for (f in subfileList){
      wb<-openxlsx::createWorkbook()
      for (s in 1:length(listFEWSR)){
        df<-openxlsx::read.xlsx(f,sheet=listFEWSR[[s]])
        if (f==subfileList[1]){#first file
          str<-paste0("df",s,"<-df")
          eval(parse(text=str))
          }else{#not first file
           str<-paste0("df",s,"<-rbind(df",s,",df)")
           eval(parse(text=str))
            }
        if (f==subfileList[length(subfileList)]){#last file
          openxlsx::addWorksheet(wb,sheetName=listFEWSR[[s]])
          str<-paste0("openxlsx::writeData(wb,listFEWSR[[s]],df",s,")")
          eval(parse(text=str))
          }
        }#for each sheet
      }#for each file in FEWSR type
    openxlsx::saveWorkbook(wb,file = paste0(compileOutFEWSRpath,gsub("_A","_all",basename(subfileList[1]))),overwrite = TRUE)
    }#for each type
}