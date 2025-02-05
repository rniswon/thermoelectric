path<-"E:/Corona_VPN/WBEEP/testAssoc/output_6.2.21/mismatches_missing.bf.923_removed/"
onlyAuto3<-read.csv(paste0(path,"onlyIn_autoSheet3_key.csv"))
onlyOrig3<-read.csv(paste0(path,"onlyIn_origSheet3_key.csv"))
onlyAuto4<-read.csv(paste0(path,"onlyIn_autoSheet4_key.csv"))
onlyOrig4<-read.csv(paste0(path,"onlyIn_origSheet4_key.csv"))

summaryBogen<-data.frame(var = "bogen",
                         onlyAutoSheet3 = length(unique(onlyAuto3$bogen)),
                         onlyOrigSheet3 = length(unique(onlyOrig3$bogen)),
                         totalSheet3 = length(unique(c(onlyAuto3$bogen,onlyOrig3$bogen))),
                         onlyAutoSheet4 = length(unique(onlyAuto4$bogen)),
                         onlyOrigSheet4 = length(unique(onlyOrig4$bogen)),
                         totalSheet4 = length(unique(c(onlyAuto4$bogen,onlyOrig4$bogen))),
                         totalBothSheets = length(unique(c(onlyAuto3$bogen,onlyOrig3$bogen,
                                                           onlyAuto4$bogen,onlyOrig4$bogen))))

summaryPlant<-data.frame(var = "Plant.Code",
                         onlyAutoSheet3 = length(unique(onlyAuto3$Plant.Code)),
                         onlyOrigSheet3 = length(unique(onlyOrig3$Plant.Code)),
                         totalSheet3 = length(unique(c(onlyAuto3$Plant.Code,onlyOrig3$Plant.Code))),
                         onlyAutoSheet4 = length(unique(onlyAuto4$Plant.Code)),
                         onlyOrigSheet4 = length(unique(onlyOrig4$Plant.Code)),
                         totalSheet4 = length(unique(c(onlyAuto4$Plant.Code,onlyOrig4$Plant.Code))),
                         totalBothSheets = length(unique(c(onlyAuto3$Plant.Code,onlyOrig3$Plant.Code,
                                                           onlyAuto4$Plant.Code,onlyOrig4$Plant.Code))))

summary<-rbind(summaryBogen,summaryPlant)

summary