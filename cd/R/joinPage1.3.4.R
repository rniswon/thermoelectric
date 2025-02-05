joinPage1.3.4<-function(page3.4,page1,key,joinCols,select_RPM){
  #page3
  bogen_page3.4<-joinBogen(tbl=page3.4,
                             key=key %>% filter(regexpr("NGCC",bogen)<0 & regexpr("ST",bogen)<0),
                             joinCols=joinCols)
  #page 1
  key_page1<-key %>% filter(regexpr("NGCC",bogen)>0 | regexpr("ST",bogen)>0)
  key_page1$plant_RPM<-paste(key_page1$Plant.Code,key_page1$Reported.Prime.Mover_page1,sep="^")
  key_page1$plant_RPM.mod<-paste(key_page1$Plant.Code,key_page1$Reported.Prime.Mover_page1,sep="^")
  
  page1<-page1 %>% filter(Reported.Prime.Mover %in% select_RPM)
  page1$plant_RPM<-paste(page1$Plant.Code,page1$Reported.Prime.Mover,sep="^")
  page1$mod.Reported.Prime.Mover<-ifelse(page1$Reported.Prime.Mover %in%
                                                           c("CS","CT","CA"),"NGCC",
                                                         page1$Reported.Prime.Mover)
  page1$plant_RPM.mod<-paste(page1$Plant.Code,page1$mod.Reported.Prime.Mover,sep="^")
  
  bogen_page1<-joinBogen(tbl=page1 %>% filter(Plant.Code %in% key_page1$Plant.Code),
                                   key=key_page1,
                                   joinCols=c("plant_RPM","plant_RPM.mod"))
  bogen_page1<-bogen_page1 %>% filter(!is.na(bogen))
  
  #combine
  bogen_all<-rbind.fill(bogen_page3.4,bogen_page1)
  bogen_all<-bogen_all %>% filter(!is.na(bogen))
  return(bogen_all)
}