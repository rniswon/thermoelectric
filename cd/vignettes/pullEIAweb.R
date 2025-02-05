#path to repo
repoPath<-"C:/Users/lgorman/OneDrive - DOI/Repos/Thermoelectric/"

#directory to save EIA data import
EIAsaveOut<-"C:/Users/lgorman/OneDrive - DOI/WBEEP/eia_pull_11.28.22/"

#load development packages and dependencies
devtools::install_deps(paste0(repoPath,"/cd"), upgrade = "never")
devtools::load_all(paste0(repoPath,"/cd"),recompile = FALSE)
#####################################
#NO EDITS REQUIRED BELOW THIS LINE
####################################

years<-seq(2000,2020,1)
for(y in years){
  print(y)
  #ImportModule
  dest<-paste0(EIAsaveOut,"EIA_",y)
  eia_year<-y
  #run import module
  eia_webpull(eia_year,dest)
}

filelist<-list.files(EIAsaveOut,full.names = T,pattern = "*.zip",recursive = T)
for (f in filelist){unlink(f)}