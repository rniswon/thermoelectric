fewsr_prep <- function(con_duty, pond_info, fewsr_type){
  con_duty <- con_duty %>% filter(cooling == fewsr_type) 
  con_duty <- left_join(con_duty, pond_info)
  con_duty <- con_duty %>% select(Plant.Code, elevation, pond_area, 
                                  contains("CD_"), contains("_DB"), contains("_WB"),
                                  contains("WT_"), contains("_WS"))
  
  names(con_duty)[4:15]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  names(con_duty)[16:27]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  names(con_duty)[28:39]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  names(con_duty)[40:51]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  names(con_duty)[52:63]<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  
  return(con_duty)
}
