# setwd("./")
# inputData_path<-"./cd/Data/"
# outputData_path<-"./cd/Output/"
pathWrite<-"./plant_association/Output/"

#########DO NOT EDIT BELOW THIS LINE#########
devtools::install_deps("./cd", upgrade = "never")
devtools::load_all("./cd",recompile = FALSE)
devtools::install_deps("./plant_association", upgrade = "never")
devtools::load_all("./plant_association",recompile = FALSE)

# pathScripts<-"./plant_association/R/"
# source(paste0(pathScripts,"import_bogen_data.R"))
# source(paste0(pathScripts,"import_bocoo_data.R"))
# source(paste0(pathScripts,"import_generator_data.R"))
# source(paste0(pathScripts,"associate.R"))
# source(paste0(pathScripts,"bogen_assocv_v2.R"))
# source(paste0(pathScripts,"bocoo_associate.R"))
# source(paste0(pathScripts,"compareKeys_2D.R"))
# source(paste0(pathScripts,"compilePlantInfo_excel.R"))
# source(paste0(pathScripts,"executeBogenAssoc.R"))

pathData<-"./cd/Data"
plantList<-read.csv(paste0(pathData,'/sheet3_key.csv'))
nukePlants<-read.csv(paste0(pathData,'/nuke_combogencoo_key.csv'))


#ImportModule
dest<-tempdir()
eia_year<-2015


path_InputData_Metafile<-"./importModule/userControlFiles/UserControlCrosswalk_2015.xlsx"
path_EIAInputData<-paste0(dest,"/",eia_year)
outputCSV<-TRUE
path_outputCSV<-dest

path_scripts<-"./importModule/R/"

fileList<-list.files(path_scripts,full.names = T)
for (f in fileList){
  source(f)
}

eia_webpull(eia_year,dest)
inputData.list<-import_EIAData(path_InputData_Metafile,path_EIAInputData,outputCSV,path_outputCSV)

inputData.list$plantList<-plantList
inputData.list$nukePlants<-nukePlants

bogen.out.list<-executeBogenAssoc(pathWrite,inputData.list,select_RPM=c("CA", "CS", "CT", "ST",NA), eia_year)
sheet3_key<-bogen.out.list$sheet3_key



multiBogen<-data.table::setDT(bogen.key)[, .(count = data.table::uniqueN(Bogen)), by = Plant.Code]
multiBogen<-multiBogen %>% filter(count>1)
multiBogen2<-data.table::setDT(sheet3_key)[, .(count = data.table::uniqueN(bogen)), by = Plant.Code]
multiBogen2<-multiBogen2 %>% filter(count>1)
orphanBoilerPlants<-orphanBoilerPlants[orphanBoilerPlants %in% multiBogen$Plant.Code]
orphanBoilerPlants2<-orphanBoilerPlants[orphanBoilerPlants %in% multiBogen2$Plant.Code]
########################################################################################


#Generate BOGEN-cooling associations#
#Associate boiler units with cooling units
#names(sheet3_key)[names(sheet3_key)=="bogen"]<-"Bogen"
bogencoo.key<-bocoo.associate(inputData.list$bocoo,sheet3_key, save_image = FALSE)
names(sheet3_key)[names(sheet3_key)=="Bogen"]<-"bogen"

#run after compare.keys sheet3.key
sheet3_key<-auto_sheet3_key
matched_bocoo<-inner_join(bogencoo.key,matched %>% select(bogen,bogen_orig), by=c("Bogen"="bogen"))
matched_bocoo$combogen_orig<-paste0("combogen^",matched_bocoo$bogen_orig)
orig_bocoo_key<-read.csv("E:/Corona_VPN/Repos/Thermoelectric_github/2_physical_models/models/condenserDuty/Data/combogencoo_key.csv")

#format Plant.Code
orig_bocoo_key$Plant.Code<-sapply(orig_bocoo_key$combogen, function(x) as.numeric(strsplit(as.character(x),"\\^")[[1]][2]))

#subset unmached bogens
orig_bocoo_key$bogen<-sapply(orig_bocoo_key$combogen, function(x) gsub("combogen\\^","",as.character(x)))
orig_bocoo_key<-inner_join(orig_bocoo_key,matched %>% select(bogen_orig), by=c("bogen"="bogen_orig"))
orig_bocoo_key<-orig_bocoo_key[!duplicated(orig_bocoo_key),]

#add bogen column
matched_bocoo$bogen<-matched_bocoo$combogencoo
orig_bocoo_key$bogen<-orig_bocoo_key$combogencoo

#remove missing combogenscoos
matched_bocoo<-matched_bocoo %>% filter(!is.na(combogencoo))
matched_bocoo<-matched_bocoo[!duplicated(matched_bocoo),]

#compare original key and auto_key
origCol<-"combogen"
autoCols<-"combogen_orig"
orig_key<-orig_bocoo_key
auto_key<-matched_bocoo
compare.list<-compareKeys_2D(orig_key,auto_key,origCol,autoCols)
onlyAutoBogencoos<-compare.list$onlyAutoBogens
onlyOrigBogencoos<-compare.list$onlyOrigBogens
matchedBocoos<-compare.list$matched

#don't care about mismatches with Net.Gen<=0
onlyAutoBogencoos<-onlyAutoBogencoos %>% filter(Net.Generation.mwh>0)
#onlyOrigBogencoos<-onlyOrigBogencoos %>% filter(Net.Generation.mwh>0)

nrow(onlyAutoBogencoos)
nrow(onlyOrigBogencoos)
nrow(matchedBocoos)

length(unique(onlyAutoBogencoos$Plant.Code))
length(unique(onlyOrigBogencoos$Plant.Code))
#extra49auto<-anti_join(onlyAutoBogencoos,onlyAutoBogencoos_nofill,by="Plant.Code")

# test<-anti_join(onlyOrigBogencoos,bocoo, by=c("Plant.Code"))
# test2<-inner_join(onlyOrigBogencoos,bocoo, by=c("Plant.Code"))

outTables<-list(orig_combogencoo_key=orig_bocoo_key %>% select(Plant.Code,combogen,combogencoo),
                auto_combogencoo_key=matched_bocoo %>% select(Plant.Code,Bogen,bogen_orig,combogen,combogencoo),
                EIA_bocoo=bocoo,
                auto_sheet3_key=sheet3_key)

#compilePlantInfo_excel(outputPath="D:/49onlyAutoMismatchBocoo_OneBogen.xlsx",plantList=unique(extra49auto$Plant.Code)[order(as.numeric(as.character(unique(extra49auto$Plant.Code))))],outTables)

compilePlantInfo_excel(outputPath="D:/onlyInOrigBocoos.xlsx",plantList=unique(onlyOrigBogencoos$Plant.Code),outTables)
compilePlantInfo_excel(outputPath="D:/onlyInAutoBocoos.xlsx",plantList=unique(onlyAutoBogencoos$Plant.Code),outTables)

# matched_bocoo_orig<-inner_join(matched_bocoo,orig_bocoo_key,by=c("combogen_orig"="combogen","combogencoo"="combogencoo"))
# matched_bocoo_orig<-matched_bocoo_orig %>% select(Plant.Code,Generator.ID,Boiler.ID,Bogen,bogen_orig,combogen_orig,combogen,combogencoo)
# mismatched_bocoo_orig<-anti_join(matched_bocoo,orig_bocoo_key,by=c("combogen_orig"="combogen","combogencoo"="combogencoo"))
# mismatched_bocoo_orig<-mismatched_bocoo_orig %>% select(Plant.Code,Generator.ID,Boiler.ID,Bogen,bogen_orig,combogen_orig,combogen,combogencoo)

#Join BOCOO IDS with cooling units.
#bocoo.key <- left_join(bocoo,bocoo.key, by=c("Plant.Code","Cooling.ID","Boiler.ID","Plant.Name"))

#Create bogencoo key table#
#bogencoo.key<-full_join_track(bogen.key,bocoo.key)
#bogencoo.key$Bogencoo<-paste(bogencoo.key$Bogen,bogencoo.key$result,sep='^')

#Output boiler-generator-cooling (BOGENCOO) association key#
write.csv(bogencoo.key,"E:/Corona_VPN/WBEEP/testAssoc/output_4.27.21/bogencoo.key.csv",row.names = F)


