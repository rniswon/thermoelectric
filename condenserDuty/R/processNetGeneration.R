#'@title processNetGeneration
#'@description calculates annual and montly process net generation by plant and generator, replaces 
#'             negative net generation months with zero, and filters out generators with negative net 
#'             generation for the year.  For output file pos_netgen.csv \cr \cr
#'Executed By: non_nuke_condenser_duty.R \cr
#'@param generation.data_df data.frame EIA-923 Monthly Generating Unit Net Generation Time 
#'                               Series File, 2015 Final Revision, input file 2015_GenerationData.csv
#'@param plantList vector of all plant codes from 2015, input file 2015_Plants.csv
#'@param page4 logical indicating whether page 4 generation.data is used as input, if FALSE page 1 gen_fuel_data is used                       
#'@return `results` data.frame annual and montly process net generation by plant and generator
#'
#'@export

#Collect net generation data. Function replaces negative net generation months with zero, and
#filters out generators with negative net generation for the year.


processNetGeneration <- function(generation.data_df, plantList, page4){
  months <- c("January", "February", "March", "April",
              "May", "June", "July", "August",
              "September", "October", "November", "December")
  if (page4){
    generation.data <- generation.data_df %>% 
    ungroup() %>% 
    subset(Plant.Code %in% plantList) %>% 
    select(c(Plant.Code, contains("Combined"), Generator.ID, contains("Reported"), contains("Net.Generation"), contains("Year.To"))) %>%
    dplyr::mutate(plant_gen = paste(Plant.Code, Generator.ID, sep = "^"))# %>% 
    #filter(select(., contains("Year.To.Date")) >= 0)
indexes<-c(1:4, 18, 17)
  }else{#page 1
    generation.data <- generation.data_df %>% 
      ungroup() %>% 
      subset(Plant.Code %in% plantList) %>% 
      select(c(Plant.Code, contains("Combined"), contains("Reported"), contains("Netgen."), contains("Net.Generation.")))
    names(generation.data)[names(generation.data)=="Net.Generation..Megawatthours."]<-"Net.Generation.Year.To.Date"
    names(generation.data)[5:16]<-paste0("Net.Generation.",months)
    indexes<-c(1:4,17)
    }
  
  #add flag for negative data
  for(m in months){
    netGenstr<-paste0("generation.data<-generation.data %>% 
                        dplyr::mutate(flag_NegNetGen.",m,"=ifelse(Net.Generation.",m,"<0,1,0))")
    eval(parse(text=netGenstr))
  }
  
  flagIndexes<-which(regexpr("flag",names(generation.data))>0)
  
  pos_netgen_only <- data.frame(generation.data[indexes], 
                                sapply(generation.data[5:16], function(x) {y<-ifelse(x > 0, x, 0)}),
                                generation.data[flagIndexes])
  pos_netgen_only<-pos_netgen_only[any(pos_netgen_only[,names(pos_netgen_only)[regexpr("Net.Generation",names(pos_netgen_only))>0 &
                                                                                names(pos_netgen_only)!="Net.Generation.Year.To.Date"]]>0),]
  pos_netgen_only$Net.Generation.Year.To.Date<-rowSums(pos_netgen_only[,names(pos_netgen_only)[regexpr("Net.Generation",names(pos_netgen_only))>0 &
                                                                                                 names(pos_netgen_only)!="Net.Generation.Year.To.Date"]],
                                                       na.rm = TRUE)
  pos_netgen_only<-pos_netgen_only[pos_netgen_only$Net.Generation.Year.To.Date>0,]
  
  if (page4){
  neg_netgen_only <- generation.data %>% 
    subset(Plant.Code %in% plantList) %>% 
    select(c(Plant.Code, contains("Combined"), Generator.ID, contains("Reported"), contains("Net.Generation"), contains("Year.To"))) %>% 
    dplyr::mutate(plant_gen = paste(Plant.Code, Generator.ID, sep = "^")) %>% 
    filter(select(., contains("Year.To.Date")) < 0)
  }else{#page 1
    neg_netgen_only <- generation.data %>% 
      subset(Plant.Code %in% plantList) %>% 
      select(c(Plant.Code, contains("Combined"), contains("Reported"), contains("Netgen."), contains("Net.Generation."))) 
    names(generation.data)[names(generation.data)=="Net.Generation..Megawatthours."]<-"Net.Generation.Year.To.Date"
    neg_netgen_only<-neg_netgen_only %>% filter(select(., contains("Year.To.Date")) < 0) 
  }
  
  results <- list(pos_netgen = pos_netgen_only, neg_netgen = neg_netgen_only)
  return(results)
}

