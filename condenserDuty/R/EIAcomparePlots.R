#'@title EIAcomparePlots
#'@description 
#'@param FEWSRtower.list
#'@param list.8D
#'@param outputData_pathYear
#'
#'@export

EIAcomparePlots<-function(FEWSRtower.list,list.8D,outputData_pathYear){
  
  #Format settings for plots
  theme_USGS <-  function(base_size = 8){
    theme(
      plot.title = element_text (vjust = 3, hjust = 0.5, size = 14, family="serif"),
      plot.subtitle = element_text(hjust = 1, size = 12,family="serif"),
      plot.caption = element_text(hjust = c(0,1), size = 12,family="serif"),
      plot.margin = unit (c(5.5, 5, 5.5, 5), "lines"), 
      panel.border = element_rect (colour = "black", fill = F, size = 0.1),
      panel.grid.major = element_line (colour = "black", size = 0.1),
      panel.grid.minor = element_line (colour = "grey", size = 0.07),
      panel.background = element_rect (fill = "white"),
      legend.background = element_blank(),
      legend.justification= "center",
      legend.position = "bottom",
      legend.key = element_blank (),
      legend.title = element_blank(),
      legend.text = element_text (size = 11),
      axis.title.x = element_text (size = 11, family="serif"),
      axis.title.y = element_text (vjust = 1, angle = 90, size = 11, family="serif"),
      axis.text.x = element_text (size = 11, vjust = -0.25, colour = "black", 
                                  family="serif", margin=margin(10,5,20,5,"pt")),
      axis.text.y = element_text (size = 11, hjust = 1, colour = "black", 
                                  family="serif", margin=margin(5,10,10,5,"pt")),
      axis.ticks = element_line (colour = "black", size = 0.1),
      axis.ticks.length = unit(-0.25 , "cm"),
      axis.ticks.margin = unit(0.5, "cm"),
    )
  }
  
  #Function to generate minor breaks on log plots
  minor_breaks_log <- function(base) {
    # Prevents lazy evaluation
    force(base) 
    # Wrap calculation in a function that the outer function returns
    function(limits) {
      ggplot2:::calc_logticks(
        base = base, 
        minpow = floor(log(limits[1], base = base)), 
        maxpow = ceiling(log(limits[2], base = base))
      )$value
    }
  }
  
  #Set major plot breaks and labels for consumption and withdrawal
  Consump_breaks <- c(0.000001, 0.0001, 0.01, 1.0, 100, 10000)
  Consump_labels <- function(Consump_breaks) ifelse(Consump_breaks == 0, "0", Consump_breaks)
  WD_breaks <- c(0.000001, 0.0001, 0.01, 1.0, 100, 10000)
  WD_labels <- function(WD_breaks) ifelse(WD_breaks == 0, "0", WD_breaks)
  
  #plotly ticks
  minTickLoc = numeric()
  for (i in -5:5){
    bb = 1:10;
    minTickLoc = c(minTickLoc, (bb*10^(i-2)))
  }
  
  ttxt <- rep(" ",length(minTickLoc))  # no label at most of the ticks
  ttxt[seq(1,length(minTickLoc),10)] <- as.character(minTickLoc)[seq(1,length(minTickLoc),10)] # every 9th tick is labelled
  ttxt[length(minTickLoc)]<-as.character(minTickLoc[length(minTickLoc)])

  
  #get lists in same order
  FEWSRtower.list<-FEWSRtower.list[order(names(FEWSRtower.list))]
  list.8D<-list.8D[order(names(list.8D))]
  for (c in 1:length(list.8D)){
    df.8D<-list.8D[[c]]
    df.mod<-FEWSRtower.list[[c]]
    
    
    if (regexpr("plantLevel",names(list.8D)[c])>0){#plant level
      if(regexpr("Month",names(list.8D)[c])>0){
        df<-inner_join(df.mod,df.8D, by=c("Plant.Code","Month")) 
      }else{#annual
        df<-inner_join(df.mod,df.8D, by=c("Plant.Code"))
      }
      
    }else{#cooling level
      
      if(regexpr("Month",names(list.8D)[c])>0){
        df<-inner_join(df.mod,df.8D, by=c("Plant.Code","Month","cooling")) 
      }else{#annual
        df<-inner_join(df.mod,df.8D, by=c("Plant.Code","cooling")) 
      }
      
    }

    df<-df[,!names(df) %in% c("percentAllocation","flag_minMax")]
    
    #set names
    NAMES<-names(df)
    names(df)[regexpr("Med",names(df))>0]<-"Med"
    names(df)[regexpr("Min",names(df))>0]<-"Min"
    names(df)[regexpr("Max",names(df))>0]<-"Max"
    names(df)[regexpr("Rep",names(df))>0]<-"Rep"
    
    df<-df %>% filter(!is.na(Med))
    
    #calculated error
    df$Error <- (df$Rep - df$Med)
    df$PercentError <- (df$Error/df$Rep)*100
    df$InRange <- ifelse(df$Rep >= df$Min & df$Rep <= df$Max, "Within estimated range", "Outside estimated range")
    df <- df %>% mutate(AboveBelow = case_when(Rep < Min ~ "Below Range",
                                               Rep > Max ~ "Above Range",
                                               Rep >= Min & Rep <= Max ~ "In Range"))
    NAMES2<-names(df)
    #put original names for output
    names(df)<-c(NAMES,"Error","PercentError","InRange","AboveBelow")
    
    str<-paste0("list.8D$compare_",names(FEWSRtower.list)[c],"<-df")
    eval(parse(text=str))
    
    #get more than 5 percent Error
    df_5PerError <-filter(df, abs(PercentError) > 5.0)
    
    #output csv
    write.csv(df, file=paste0(outputData_pathYear,y,"_EIAcompare_",names(FEWSRtower.list)[c],".csv"),row.names = F)
    write.csv(df_5PerError, paste0(outputData_pathYear,y,"_EIAcompare_",names(FEWSRtower.list)[c],"5percentError.csv"),row.names = F)
    
    #reset general names for plotting
    names(df)<-NAMES2
    
    if (regexpr("Consump",names(list.8D)[c])>0){
      plotBreaks<-Consump_breaks
      plotLabels<-Consump_labels
      
      if (regexpr("Month",names(list.8D)[c])>0){
        plotTitle<-paste0(eia_year, " EIA vs Model Monthly Consumption")
        plotlyXlab<-"<b>USGS-modeled monthly consumption (million gallons per day)</b>"
        plotlyYlab<-"<b>EIA-reported monthly consumption (million gallons per day)</b>"
      }else{
        plotTitle<-paste0(eia_year, " EIA vs Model Annual Consumption")
        plotlyXlab<-"<b>USGS-modeled annual consumption (million gallons per day)</b>"
        plotlyYlab<-"<b>EIA-reported annual consumption (million gallons per day)</b>"
      }
    }else{#WD
      plotBreaks<-WD_breaks
      plotLabels<-WD_labels
      
      if (regexpr("Month",names(list.8D)[c])>0){
        plotTitle<-paste0(eia_year, " EIA vs Model Monthly Withdrawal")
        plotlyXlab<-"<b>USGS-modeled monthly withdrawals (million gallons per day)</b>"
        plotlyYlab<-"<b>EIA-reported monthly withdrawals (million gallons per day)</b>"
      }else{
        plotTitle<-paste0(eia_year, " EIA vs Model Annual Withdrawal")
        plotlyXlab<-"<b>USGS-modeled annual average withdrawals (million gallons per day)</b>"
        plotlyYlab<-"<b>EIA-reported annual average withdrawals (million gallons per day)</b>"
      }
      
    }
    
    
    
    #Plot modeled vs reported log scale with cooling system type symbolized
    if (regexpr("plantLevel",names(list.8D)[c])<0){#cooling
      PerBias <- pbias(df$Rep, df$Med)
      rsq <- round(cor(df$Rep, df$Med)^2,3)
      pdf(file = paste0(outputData_pathYear,y,"_EIAcompare_",names(FEWSRtower.list)[c],"_log.pdf"), width = 8, height = 8)
      p <- ggplot(df) +
        geom_point(aes(x = Med,  y = Rep, color = cooling, shape = cooling)) +
        geom_abline(intercept = 0, slope = 1) +
        theme_USGS() +  
        scale_x_continuous(trans = "log10", breaks = plotBreaks, 
                           minor_breaks = minor_breaks_log(10), 
                           limits = c(0.000001, 10000), 
                           labels = plotLabels, 
                           expand =c(0,0))+ 
        scale_y_continuous(trans = "log10", breaks = plotBreaks,
                           minor_breaks = minor_breaks_log(10),
                           limits = c(0.000001, 10000),
                           labels = plotLabels, expand =c(0,0))+
        labs(x = 'Modeled', y = 'Reported', caption = c(as.expression(bquote(R^2 == .(rsq))), 
                                                        as.expression(bquote(Percent~Bias == .(PerBias)))),
             title = plotTitle)
      print(p)
      dev.off()
      
      #plotly
      m <- list(
        l = 200,
        r = 50,
        b = 100,
        t = 100,
        pad = 4
      )
      dfZero<-df %>% filter(Rep==0 | Med==0)
      p<-plotly::plot_ly(colors=c(viridis::turbo(5,begin = 0.2),"#000064"),
                         symbols=c("square","cross","diamond","circle","diamond-wide","x-thin"))
      p <- p %>% plotly::layout(margin = m,
                                xaxis = list(title=" ",type = "log", tickvals = as.numeric(ttxt[ttxt!=" "]), 
                                              ticktext = paste0("<b>",as.character(ttxt[ttxt!=" "]),"</b>"),  
                                              zeroline=T, showline=T, ticks="outside", 
                                              ticklen=8, showgrid=T,rangemode= 'tozero',
                                              range=c(log10(0.000001), log10(10000)),
                                             tickfont = list(size = 14),
                                             overlaying="x4"),
                                xaxis4 = list(title=list(text = plotlyXlab,
                                                   standoff = 40L),
                                             type="log",
                                             showgrid=T,zeroline=T, showline=F, 
                                             ticks="outside",
                                             tickvals=minTickLoc,
                                             # ticktext=ttxt,
                                             ticktext=rep(" ", length(minTickLoc)),
                                             rangemode= 'tozero',
                                             ticklen=3, range=c(log10(0.000001), log10(10000)),
                                             gridcolor="#B2B2B2"),
                                
                                
                                yaxis = list(title=" ",type = "log", tickvals = as.numeric(ttxt[ttxt!=" "]), 
                                             ticktext = paste0("<b>",as.character(ttxt[ttxt!=" "]),"</b>"),  
                                             zeroline=T, showline=T, ticks="outside", 
                                             ticklen=8, showgrid=T,rangemode= 'tozero',
                                             range=c(log10(0.000001), log10(10000)),
                                             tickfont = list(size = 14),
                                             overlaying="y4"),
                                yaxis4 = list(title=list(text = plotlyYlab,
                                                         standoff = 60L),
                                              type="log",
                                              showgrid=T,zeroline=T, showline=F, 
                                              ticks="outside",
                                             tickvals=minTickLoc,
                                            #ticktext=ttxt,
                                            ticktext=rep(" ", length(minTickLoc)),
                                             rangemode= 'tozero',
                                             ticklen=3,
                                             range=c(log10(0.000001), log10(10000)),
                                            gridcolor="#B2B2B2"),
                                xaxis2 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                                              rangemode= 'tozero',overlaying="x4"),
                                
                                yaxis2 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                                              rangemode= 'tozero',overlaying="y4"),
                                xaxis3 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                                              rangemode= 'tozero',side="top",range=c(0,10000),overlaying="x4"),
                                
                                yaxis3 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                                              rangemode= 'tozero',side="right",range=c(0,10000),overlaying="y4"),
                                
                                showlegend =TRUE,title=paste0("<b>",plotTitle,"</b>"))
p<-p %>% plotly::add_trace(x=-1,y=-1,type="scatter",showlegend=F,
                                 xaxis="x",yaxis="y")
colors<-c(viridis::turbo(5,begin = 0.2),"#000064")
symbols<-c("square","cross","diamond","circle","diamond-wide","x-thin")
for (lg in 1:length(levels(as.factor(df$cooling)))){
  dfsub<-df %>% filter(cooling==levels(as.factor(df$cooling))[lg])
  dfZerosub<-dfZero %>% filter(cooling==levels(as.factor(df$cooling))[lg])
  p<-p %>% plotly::add_trace(data=dfsub, 
                             x=~Med,y=~Rep,mode = "markers",
                             text=paste0("Plant.Code : ",dfsub$Plant.Code,"</br> Modeled : ",dfsub$Med,"</br> Reported : ",dfsub$Rep),
                             hoverInfo='text',type="scatter",marker=list(size=13,color=colors[lg],symbol=symbols[lg],
                                                                         line=list(color="#000064",width=1)),
                             xaxis="x4",yaxis="y4",legendgroup=colors[lg], showlegend=TRUE,name=levels(as.factor(df$cooling))[lg])
  
  p<-p %>% plotly::add_trace(data=dfZerosub, 
                             x=~Med,y=~Rep,mode = "markers",
                             text=paste0("Plant.Code : ",dfZerosub$Plant.Code,"</br> Modeled : ",dfZerosub$Med,"</br> Reported : ",dfZerosub$Rep),
                             hoverInfo='text',type="scatter",marker=list(size=13,color=colors[lg],symbol=symbols[lg],
                                                                         line=list(color="#000064",width=1)),
                             xaxis="x2",yaxis="y2",showlegend=F,legendgroup=colors[lg],name=levels(as.factor(df$cooling))[lg])
}
      
      p<-p %>% plotly::add_trace(mode="line",x=c(0,10000),y=c(0,10000),name= paste0("Rsq=",rsq),color=I("black"),
                                 xaxis="x3",yaxis="y3")

      htmlwidgets::saveWidget(
        widget = p, #the plotly object
        file = paste0(outputData_pathYear,y,"_EIAcompare_",names(FEWSRtower.list)[c],"_log.html"), #the path & file name
        selfcontained = TRUE #creates a single html file,
      )
      
      #Plot by cooling system type monthly modeled vs reported consumption on log scale with cooling system type symbolized
      for (cool in unique(df$cooling)){
        df_cool <- filter(df, cooling == cool)
        PerBias <- pbias(df_cool$Rep, df_cool$Med)
        rsq <- round(cor(df_cool$Rep, df_cool$Med)^2,3)
        pdf(file =  paste0(outputData_pathYear,y,"_EIAcompare_",names(FEWSRtower.list)[c],"_log_",cool,"Cooling.pdf"), width = 8, height = 8)
        p <- ggplot(df_cool) +
          geom_point(aes(x = Med,  y = Rep, color = AboveBelow, shape = AboveBelow)) +
          theme_USGS() +  
          geom_abline(intercept = 0, slope = 1) +
          scale_x_continuous(trans = "log10", breaks = plotBreaks, 
                             minor_breaks = minor_breaks_log(10), 
                             limits = c(0.000001, 10000), 
                             labels = plotLabels, expand =c(0,0))+ 
          scale_y_continuous(trans = "log10", breaks = plotBreaks, 
                             minor_breaks = minor_breaks_log(10), 
                             limits = c(0.000001, 10000), 
                             labels = plotLabels, expand =c(0,0))+
          labs(x = 'Modeled', y = 'Reported', caption = c(as.expression(bquote(R^2 == .(rsq))), as.expression(bquote(Percent~Bias == .(PerBias)))),
               title = gsub("EIA",paste0(cool," EIA"),plotTitle))
        print(p)
        dev.off()
        
        #plotly
        dfZero_cool<-df_cool %>% filter(Rep==0 | Med==0)
        p<-plotly::plot_ly(colors=c(viridis::turbo(3,begin = 0.25,direction = -1))[c(1,3,2)],
                           symbols=c("triangle-up","triangle-down","cross"))
        p <- p %>% plotly::layout(margin = m,
          xaxis = list(title=" ",type = "log", tickvals = as.numeric(ttxt[ttxt!=" "]), 
                       ticktext = paste0("<b>",as.character(ttxt[ttxt!=" "]),"</b>"),  
                       zeroline=T, showline=T, ticks="outside", 
                       ticklen=8, showgrid=T,rangemode= 'tozero',
                       range=c(log10(0.000001), log10(10000)),
                       tickfont = list(size = 14),
                       overlaying="x4"),
          xaxis4 = list(title=list(text = plotlyXlab,
                                   standoff = 40L),
                        type="log",
                        showgrid=T,zeroline=T, showline=F, 
                        ticks="outside",
                        tickvals=minTickLoc,
                        # ticktext=ttxt,
                        ticktext=rep(" ", length(minTickLoc)),
                        rangemode= 'tozero',
                        ticklen=3, range=c(log10(0.000001), log10(10000)),
                        gridcolor="#B2B2B2"),
          
          
          yaxis = list(title=" ",type = "log", tickvals = as.numeric(ttxt[ttxt!=" "]), 
                       ticktext = paste0("<b>",as.character(ttxt[ttxt!=" "]),"</b>"),  
                       zeroline=T, showline=T, ticks="outside", 
                       ticklen=8, showgrid=T,rangemode= 'tozero',
                       range=c(log10(0.000001), log10(10000)),
                       tickfont = list(size = 14),
                       overlaying="y4"),
          yaxis4 = list(title=list(text = plotlyYlab,
                                   standoff = 60L),
                        type="log",
                        showgrid=T,zeroline=T, showline=F, 
                        ticks="outside",
                        tickvals=minTickLoc,
                        #ticktext=ttxt,
                        ticktext=rep(" ", length(minTickLoc)),
                        rangemode= 'tozero',
                        ticklen=3,
                        range=c(log10(0.000001), log10(10000)),
                        gridcolor="#B2B2B2"),
          xaxis2 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                        rangemode= 'tozero',overlaying="x4"),
          
          yaxis2 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                        rangemode= 'tozero',overlaying="y4"),
          xaxis3 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                        rangemode= 'tozero',side="top",range=c(0,10000),overlaying="x4"),
          
          yaxis3 = list(showgrid=F,zeroline=F, showline=F, showticklabels =F,
                        rangemode= 'tozero',side="right",range=c(0,10000),overlaying="y4"),
          
          showlegend =TRUE,title = gsub("EIA",paste0(cool," EIA"),paste0("<b>",plotTitle,"</b>")))
        
        p<-p %>% plotly::add_trace(x=-1,y=-1,type="scatter",showlegend=F,
                                   xaxis="x",yaxis="y")
        colors<-c(viridis::turbo(3,begin = 0.25,direction = -1))[c(1,3,2)]
        symbols<-c("triangle-up","triangle-down","cross")
        for (lg in 1:length(levels(as.factor(df_cool$AboveBelow)))){
          dfsub<-df_cool %>% filter(AboveBelow==levels(as.factor(df_cool$AboveBelow))[lg])
          dfZerosub<-dfZero_cool %>% filter(AboveBelow==levels(as.factor(df_cool$AboveBelow))[lg])
        p<-p %>% plotly::add_trace(data=dfsub, x=~Med,y=~Rep,mode = "markers",
                                   text=paste0("Plant.Code : ",dfsub$Plant.Code,"</br> Modeled : ",dfsub$Med,"</br> Reported : ",dfsub$Rep),
                                   hoverInfo='text',type="scatter",marker=list(size=13,color=colors[lg],symbol=symbols[lg],
                                                                              line=list(color="#000064",width=1)),
                                   showlegend=T,legendgroup=colors[lg],name=levels(as.factor(df$AboveBelow))[lg],
                                   xaxis="x4",yaxis="y4")
        p<-p %>% plotly::add_trace(data=dfZerosub, x=~Med,y=~Rep,mode = "markers",
                                   text=paste0("Plant.Code : ",dfZerosub$Plant.Code,"</br> Modeled : ",dfZerosub$Med,"</br> Reported : ",dfZerosub$Rep),
                                   hoverInfo='text',type="scatter",marker=list(size=13,color=colors[lg],symbol=symbols[lg],
                                                                               line=list(color="#000064",width=1)),
                                   showlegend=F,legendgroup=colors[lg],name=levels(as.factor(df$AboveBelow))[lg],
                                   xaxis="x2",yaxis="y2")
        
        }
        p<-p %>% plotly::add_trace(mode="line",x=c(0,10000),y=c(0,10000),
                                   name= paste0("Rsq=",rsq),color=I("black"),xaxis="x3",yaxis="y3")
        
        htmlwidgets::saveWidget(
          widget = p, #the plotly object
          file = paste0(outputData_pathYear,y,"_EIAcompare_",names(FEWSRtower.list)[c],"_log_",cool,"Cooling.html"), #the path & file name
          selfcontained = TRUE #creates a single html file,
        )
        
        
      }#for each cooling type
      
    }else{#plant-level
      
      #Plot modeled vs reported consumption and withdrawal on log scale with above, below, or in range symbolized
      PerBias <- pbias(df$Rep, df$Med)
      rsq <- round(cor(df$Rep, df$Med)^2,3)
      pdf(file = paste0(outputData_pathYear,y,"_EIAcompare_",names(FEWSRtower.list)[c],"aboveBelow_log.pdf"), width = 8, height = 8)
      p <- ggplot(df) +
        geom_point(aes(x = Med,  y = Rep, color = AboveBelow, shape = AboveBelow)) +
        theme_USGS() +  
        geom_abline(intercept = 0, slope = 1) +
        scale_x_continuous(trans = "log10", breaks = plotBreaks, 
                           minor_breaks = minor_breaks_log(10), 
                           limits = c(0.000001, 10000), 
                           labels = plotLabels, expand =c(0,0))+ 
        scale_y_continuous(trans = "log10", breaks = plotBreaks, 
                           minor_breaks = minor_breaks_log(10), 
                           limits = c(0.000001, 10000), 
                           labels = plotLabels, expand =c(0,0))+
        labs(x = 'Modeled', y = 'Reported', caption = c(as.expression(bquote(R^2 == .(rsq))), 
                                                        as.expression(bquote(Percent~Bias == .(PerBias)))),
             title = plotTitle)
      print(p)
      dev.off()
      
      #plotly
      dfZero_cool<-df_cool %>% filter(Rep==0 | Med==0)
      p<-plotly::plot_ly(colors=c(viridis::turbo(3,begin = 0.25,direction = -1))[c(1,3,2)],
                         symbols=c("triangle-up","triangle-down","cross"))
      p <- p %>% plotly::layout(margin = m,
        xaxis = list(title=" ",type = "log", tickvals = as.numeric(ttxt[ttxt!=" "]), 
                     ticktext = paste0("<b>",as.character(ttxt[ttxt!=" "]),"</b>"), 
                     zeroline=T, showline=T, ticks="outside", 
                     ticklen=8, showgrid=T,rangemode= 'tozero',
                     range=c(log10(0.000001), log10(10000)),
                     tickfont = list(size = 14),
                     overlaying="x4"),
        xaxis4 = list(title=list(text = plotlyXlab,
                                 standoff = 40L),
                      type="log",
                      showgrid=T,zeroline=T, showline=F, 
                      ticks="outside",
                      tickvals=minTickLoc,
                      # ticktext=ttxt,
                      ticktext=rep(" ", length(minTickLoc)),
                      rangemode= 'tozero',
                      ticklen=3, range=c(log10(0.000001), log10(10000)),
                      gridcolor="#B2B2B2"),
        
        
        yaxis = list(title=" ",type = "log", tickvals = as.numeric(ttxt[ttxt!=" "]), 
                     ticktext = paste0("<b>",as.character(ttxt[ttxt!=" "]),"</b>"),  
                     zeroline=T, showline=T, ticks="outside", 
                     ticklen=8, showgrid=T,rangemode= 'tozero',
                     range=c(log10(0.000001), log10(10000)),
                     tickfont = list(size = 14),
                     
                     overlaying="y4"),
        yaxis4 = list(title=list(text = plotlyYlab,
                                 standoff = 60L),
                      type="log",
                      showgrid=T,zeroline=T, showline=F, 
                      ticks="outside",
                      tickvals=minTickLoc,
                      #ticktext=ttxt,
                      ticktext=rep(" ", length(minTickLoc)),
                      rangemode= 'tozero',
                      ticklen=3,
                      range=c(log10(0.000001), log10(10000)),
                      gridcolor="#B2B2B2"),
        xaxis2 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                      rangemode= 'tozero',overlaying="x4"),
        
        yaxis2 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                      rangemode= 'tozero',overlaying="y4"),
        xaxis3 = list(title=" ",showgrid=F,zeroline=F, showline=F, showticklabels =F,
                      rangemode= 'tozero',side="top",range=c(0,10000),overlaying="x4"),
        
        yaxis3 = list(showgrid=F,zeroline=F, showline=F, showticklabels =F,
                      rangemode= 'tozero',side="right",range=c(0,10000),overlaying="y4"),
        
        showlegend =TRUE,title=gsub("EIA","Plant-level EIA",paste0("<b>",plotTitle,"</b>")))
      
      colors<-c(viridis::turbo(3,begin = 0.25,direction = -1))[c(1,3,2)]
      symbols<-c("triangle-up","triangle-down","cross")
      p<-p %>% plotly::add_trace(x=-1,y=-1,type="scatter",showlegend=F,
                                 xaxis="x",yaxis="y")
      for (lg in 1:length(levels(as.factor(df$AboveBelow)))){
        dfsub<-df %>% filter(AboveBelow==levels(as.factor(df$AboveBelow))[lg])
        dfZerosub<-dfZero_cool %>% filter(AboveBelow==levels(as.factor(df$AboveBelow))[lg])
        p<-p %>% plotly::add_trace(data=dfsub, x=~Med,y=~Rep,mode = "markers",
                                   text=paste0("Plant.Code : ",dfsub$Plant.Code,"</br> Modeled : ",dfsub$Med,"</br> Reported : ",dfsub$Rep),
                                   hoverInfo='text',type="scatter",marker=list(size=13,color=colors[lg],symbol=symbols[lg],
                                                                               line=list(color="#000064",width=1)),
                                   showlegend=T,legendgroup=colors[lg],name=levels(as.factor(df$AboveBelow))[lg],
                                   xaxis="x4",yaxis="y4")
        p<-p %>% plotly::add_trace(data=dfZerosub, x=~Med,y=~Rep,mode = "markers",
                                   text=paste0("Plant.Code : ",dfZerosub$Plant.Code,"</br> Modeled : ",dfZerosub$Med,"</br> Reported : ",dfZerosub$Rep),
                                   hoverInfo='text',type="scatter",marker=list(size=13,color=colors[lg],symbol=symbols[lg],
                                                                               line=list(color="#000064",width=1)),
                                   showlegend=F,legendgroup=colors[lg],name=levels(as.factor(df$AboveBelow))[lg],
                                   xaxis="x2",yaxis="y2")
        
      }
      p<-p %>% plotly::add_trace(mode="line",x=c(0,10000),y=c(0,10000),
                                 name= paste0("Rsq=",rsq),color=I("black"),xaxis="x3",yaxis="y3")
      
      htmlwidgets::saveWidget(
        widget = p, #the plotly object
        file = paste0(outputData_pathYear,y,"_EIAcompare_",names(FEWSRtower.list)[c],"aboveBelow_log.html"), #the path & file name
        selfcontained = TRUE #creates a single html file,
      )
    }
    
    
    
  }#for each names list.8D
  
}#end func