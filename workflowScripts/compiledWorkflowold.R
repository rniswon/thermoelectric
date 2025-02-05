#path to compile keys file
pathKeys<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledConsumpWD_dataRelease/compiledKeys_2008-2020.csv"
#path to compiled crosswalks file
pathCrosswalks<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledConsumpWD_dataRelease/compileCrosswalks_2008-2020_updated_01.23.23.csv"
#path to other required input files
inputData_path<-"./cd/Data/"
#path for output of CD results
outputData_path<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledManualBogens_2008-2020test/"
#path for output of plant association results
pathWrite<-"./plant_association/Output/"
#path to save EIA data pulled from web
pathSaveEIA<-"D:/EIA_data/"

#########DO NOT EDIT BELOW THIS LINE#########
devtools::install_deps("./cd", upgrade = "never")
devtools::load_all("./cd",recompile = FALSE)
devtools::install_deps("./plant_association", upgrade = "never")
devtools::load_all("./plant_association",recompile = FALSE)
years<-seq(2008,2020,1)
for (y in years){
eia_year<-y
print(eia_year)

#ImportModule
#pull EIA data
eia_webpull(eia_year,pathSaveEIA)

#read in crosswalk
crosswalk<-read.csv(pathCrosswalks)
#filter for this year
crosswalk<-crosswalk %>% filter(YEAR==y)
#save to temp dir
tempPathCross<-paste0(tempdir(),"UserControlCrosswalk",eia_year,".xlsx")
openxlsx::write.xlsx(crosswalk,file=tempPathCross,row.names=F,overwrite = T)

path_InputData_Metafile<-tempPathCross
path_EIAInputData<-paste0(pathSaveEIA,"/",eia_year)
outputCSV<-F
path_outputCSV<-NA

#read in MPL
plantList<-openxlsx::read.xlsx(paste0(inputData_path,"/TE_MPL_allyears.xlsx"),sheet="plant years vert")
plantList$Plant.Code<-plantList$EIA_PLANT_ID
plantList<-plantList[plantList$year==eia_year,]


#run import module
inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,path_outputCSV)
#add MPL
inputData.list$plantList<-plantList
list2env(inputData.list, .GlobalEnv)


#get keys
allKeys<-read.csv(pathKeys)
allKeys<-allKeys %>% filter(YEAR==eia_year)

#format for input into condenserDuty model
sheet3_key<-allKeys %>% select(Plant.Code,
                                  Boiler.ID,
                                  bogen,
                                  contains("Reported.Prime.Mover"),
                                  YEAR,
                               plant_bo,
                               plant_bo_bf.923,
                               sheet3_key)
sheet3_key<-sheet3_key %>% filter(sheet3_key==1)
sheet3_key<-sheet3_key[!duplicated(sheet3_key),]
sheet3_key<-sheet3_key %>% filter(!is.na(bogen))


sheet4_key<-allKeys %>% select(Plant.Code,
                                  Generator.ID,
                                  bogen,
                               plant_gen,
                               plant_gen.923,
                               contains("Reported.Prime.Mover"),
                               YEAR,
                               sheet4_key)
sheet4_key<-sheet4_key %>% filter(sheet4_key==1)
sheet4_key<-sheet4_key[!duplicated(sheet4_key),]
sheet4_key<-sheet4_key %>% filter(!is.na(bogen))

bogencoo.key<-allKeys %>% select(Plant.Code,
                                 Boiler.ID,
                                      bogen,
                                      bogencoo,
                                 bogencoo.key)
names(bogencoo.key)[names(bogencoo.key)=="bogen"]<-"Bogen"
names(bogencoo.key)[names(bogencoo.key)=="bogencoo"]<-"combogencoo"
bogencoo.key$combogen<-paste0("combogen^",bogencoo.key$Bogen)
bogencoo.key<-bogencoo.key %>% filter(bogencoo.key==1)
bogencoo.key<-bogencoo.key[!duplicated(bogencoo.key),]

combogencoo_cooly_type<-allKeys %>% select(Plant.Code,
                                           bogencoo,
                                           LAKE..OF..OC..RC.,
                                           RIVER..OF.,
                                           POND..OC..RC.,
                                           TOWER..RF..RI..RN.,
                                           DC,
                                           OS,
                                           combogencoo_cooly_type)
combogencoo_cooly_type<-combogencoo_cooly_type %>% filter(combogencoo_cooly_type==1)

combogencoo_cooly_type_nukes<-allKeys %>% select(Plant.Code,
                                           bogencoo,
                                           LAKE..OF..OC..RC.,
                                           RIVER..OF.,
                                           POND..OC..RC.,
                                           TOWER..RF..RI..RN.,
                                           DC,
                                           OS,
                                           combogencoo_cooly_type_nukes)
combogencoo_cooly_type_nukes<-combogencoo_cooly_type_nukes %>% filter(combogencoo_cooly_type_nukes==1)
combogencoo_cooly_type_nukes<-combogencoo_cooly_type_nukes[!duplicated(combogencoo_cooly_type_nukes),]
combogencoo_cooly_type<-combogencoo_cooly_type[!duplicated(combogencoo_cooly_type),]

#remove rows with all missing cooling type
combogencoo_cooly_type<-aggregate(combogencoo_cooly_type[-c(1,2)],
                                  by=list(Plant.Code=combogencoo_cooly_type$Plant.Code,
                                          bogencoo=combogencoo_cooly_type$bogencoo),
                                  FUN=function(x) sum(x,na.rm=T))
combogencoo_cooly_type[-c(1,2)]<-sapply(combogencoo_cooly_type[-c(1,2)], function(x) ifelse(x==0,NA,x))
combogencoo_cooly_type_nukes<-aggregate(combogencoo_cooly_type_nukes[-c(1,2)],
                                  by=list(Plant.Code=combogencoo_cooly_type_nukes$Plant.Code,
                                          bogencoo=combogencoo_cooly_type_nukes$bogencoo),
                                  FUN=function(x) sum(x,na.rm=T))
combogencoo_cooly_type_nukes[-c(1,2)]<-sapply(combogencoo_cooly_type_nukes[-c(1,2)], function(x) ifelse(x==0,NA,x))



#compile keys into bogen.out.list object
bogen.out.list<-named.list(sheet3_key,sheet4_key,bogencoo.key,
                           combogencoo_cooly_type_nukes,combogencoo_cooly_type)

#run CD model
runCondenserDuty(inputData_path, outputData_path, inputData.list, bogen.out.list)
write.csv(final_CD_w_nukes_cooling,file=paste0(outputData_path,"CD_results",eia_year,".csv"),row.names = F)
#test same results
CD<-read.csv(paste0(outputData_path,"CD_results",eia_year,".csv"))
Cdtest<-read.csv(paste0("C:/Users/lgorman/OneDrive - DOI/WBEEP/compiledmanualBogens_2008-2020/CD_results",eia_year,".csv"))
message(eia_year)
message(identical(CD,Cdtest))
setdiff(CD,Cdtest)

}
