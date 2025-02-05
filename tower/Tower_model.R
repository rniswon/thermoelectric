# original script written by Kristen Valseth to calculate the tower and fewsr models and compare with EIA data and create plots.
# updated by Ken Skinner to just calculate the tower model and to add withdrawal estimates using code from the EIAvModel_WD_CU.R script.
## clear workspace environment
#rm(list=ls(all=T)) 

library(tidyverse)

#Setting work directory.
WorkingDirectory <- "G:/WU/Data/Thermo/GitHub_Clone/Thermoelectric-master/tower/"
setwd(WorkingDirectory)

leapyears <- list(2008, 2012, 2016, 2020) 

## enter names of input files
input_header = './Data/Tower_input_header_data.csv'
input_header_LY = 'Data/Tower_input_header_data_leapyear.csv'
CTI_input = 'Data/CTI_input.csv'
plant_char = read.csv("./Data/plant_char.csv")


for (yr in 2008:2020) {

  input_data = str_glue("Data/{yr}_Tower_input.csv")

  ## Set up workspace and import the data ----

  ## read from CSV file
  data = read.csv(input_data, header=T,na.strings="-")

  param = read.csv(input_header, header=T, skip=5, nrows=1,
                 colClasses = c(rep(NA, 3), rep("NULL", 10)))

  # Use the input_header file Tower_input_header_data_leapyear.csv for leap years, i.e. February = 29 days. Leap years in our timeframe are 2020, 2016, 2012, and 2008.
  if (yr %in% leapyears) {
    monthdays = read.csv(input_header_LY, header=T, skip=3, nrows=1,
                       colClasses = c(rep(NA, 13),rep("NULL", 0)))
  } else {
    monthdays = read.csv(input_header, header=T, skip=3, nrows=1,
                       colClasses = c(rep(NA, 13),rep("NULL", 0)))
  }

# update data file with correct plant characteristics
  data <- data %>% select(-Elevation, -Pond_Area, -Twb, -Tdb, -nwT)
  data <- left_join(data, plant_char, "Plant.Code")  
  data <- data %>% relocate(Elevation, .after = "Plant.Code") %>% relocate(Pond_Area, .after = "Elevation")

  min_T = param[1,1]
  min_approach = param[1,2]
  max_approach = param[1,3]

  ### Plant characteristics
  PlantChar = data[1:4]
  data <- data %>% relocate(percentAllocation,  .after = last_col())
  
  ### Design characteristics
  DesignChar = data[64:66]

  col_names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  ### Added heat load MMBtu
  HeatLoad = data[4:15]
#  col_names = colnames(HeatLoad)
  colnames(HeatLoad) = col_names
    
  ### Dry bulb air temperature Ta           					
  DryBulb = data[16:27]
  colnames(DryBulb) = col_names

  ### Wet bulb air temperature Twb    									
  WetBulb = data[28:39]
  colnames(WetBulb) = col_names

  ### Natural water temperature 										
  NaturalWater = data[40:51]
  colnames(NaturalWater) = col_names

  ### Wind speed at 2m W (mph)  										    									
  WindSpeed = data[52:63]
  colnames(WindSpeed) = col_names

  ## plant characteristics pre-calculations ----
  
  ## Create unique vector for plant ID # adding percentAllocation
  PlantID = data.frame(PlantChar[,1], PlantChar[,4])
  colnames(PlantID) = c("Plant_ID", "percentAllocation")

  ## convert elevation to mb to psia for all plants
  PlantChar$atm_mb = ((44331.514-(is.numeric(PlantChar$Elevation)*0.3048))/11880.516)^(1/0.1902632) 
  PlantChar$atm_psia = PlantChar$atm_mb/68.94757293 

  ## Month (and design characteristics) x plant calculations ----

  ## this section of codes creates matrices with columns 1-12 as Jan-Dec and
  ## column 13 is the design condition, and the rows are the plants
  
  ## Add design Twb, Tdb and nwT to WetBulb, DryBulb and NaturalWater matrices
  DryBulb$design = (is.numeric(DesignChar$Tdb)-32)*5/9
  WetBulb$design = (is.numeric(DesignChar$Twb)-32)*5/9
  NaturalWater$design = DesignChar$nwT

  ## set minimum T for wet and dry bulbs
  DryBulb[DryBulb<min_T] = min_T
  WetBulb[WetBulb<min_T] = min_T

  ## Calculate saturation vapor pressure at inlet air wet bulb temperature
  Pw_mb = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(WetBulb+273)))+
                     (-0.545/0.11)*log((WetBulb+273)/273)) 

  Pw_psia = Pw_mb/68.94757293 

  ## saturated vapor pressure from dry bulb temperature
  Ps_mb = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(DryBulb+273)))+
                     (-0.545/0.11)*log((DryBulb+273)/273))

  Ps_psia = Ps_mb/68.94757293

  ## Actual vapor pressure in inlet air
  vap_mb = Pw_mb - (PlantChar$atm_mb*0.00066*(DryBulb-WetBulb)*(1+(0.00115*WetBulb)))

  ## relative humidity of inlet air
  phi = vap_mb/Ps_mb

  ## Pounds of water vapor per pound of dry air in inlet air, calculated per L&M '71 eqn 3
  w1 = (0.622*phi*Ps_psia)/(PlantChar$atm_psia-(phi*Ps_psia))

  ## enthalpy of inlet air calculated per L&M '71 eqn 4
  Ha1=0.24*(DryBulb*(9/5)+32)+w1*(1061.8+0.44*(DryBulb*(9/5)+32))

  ## inlet air specific volume in cubic feet per pound - pertains to vapor/gas mixture
  sv = ((1+w1*1.585918)*286.9*((273.15+DryBulb)/(PlantChar$atm_psia*6894.757))
      /0.3048^3)/2.20462262

  ## specific volume of dry air ft3/lb
  svdry = sv*(1+w1)

  ## custom function for lookup table ----
  ### http://stackoverflow.com/questions/10160400/r-find-nearest-index
  nearest.vec <- function(x, vec)
  {
    smallCandidate <- findInterval(x, vec, all.inside=TRUE)
    largeCandidate <- smallCandidate + 1
    #nudge is TRUE if large candidate is nearer, FALSE otherwise
    nudge <- 2 * x > vec[smallCandidate] + vec[largeCandidate]
    return(smallCandidate + nudge)
  }

  ## Actual model ----

  ### Read in CTI file
  CTI = read.csv(file = CTI_input, header=T, skip=3)
  CTI_param = read.csv(CTI_input, header=T, skip=1, nrows=1,
                 colClasses = c(rep(NA, 5), rep("NULL", 1)))

  ### name parameters created by user
  min_app = CTI_param[1,1]
  max_app = CTI_param[1,2]
  cond_app = CTI_param[1,3]
  min_steam = CTI_param[1,4]
  steam_cushion = CTI_param[1,5]

  ### Create new LG values (1,1.33,1.667,2)
  CTI$LG2 = rep(c(1,1+(1/3),1+(2/3),2))

  ### preallocate matrices
  emin=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
  emed=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
  emax=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
  e25=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
  e75=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))

  cmin=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
  cmed=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
  cmax=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
  c25=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
  c75=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))


  for (i in 1:nrow(PlantChar)){

    ### typical steam
    typSteam = 92 + (DesignChar$Twb[i]-55)/40*28.5
    max_steam = typSteam + steam_cushion
  
    CTI$Approach_elev = (CTI[,5]-CTI[,4])/(3500-600)*(PlantChar$Elevation[i]-600) + CTI[,4]

    ### Create vectors needed for approach interpolation
    LGR = (CTI$LG2-1.2)/(1.8-1.2)
    diffA68 = rep(diff(CTI$Approach_elev)[seq(1, nrow(CTI), 4)], each=4)
    diffA78 = rep(diff(CTI$Approach_elev)[seq(3, nrow(CTI), 4)], each=4)
    APP68 = LGR*diffA68 + rep(CTI$Approach_elev[seq(1, nrow(CTI), 4)], each=4)
    APP78 = LGR*diffA78 + rep(CTI$Approach_elev[seq(3, nrow(CTI), 4)], each=4)

    ### Interpolate new approach for LG, elevation and design Twb
    CTI$Approach2 = ((DesignChar$Twb[i]-68)/10)*(APP78-APP68)+APP68

    ### add steam T
    CTI$SteamT = DesignChar$Twb[i] + CTI$Range + CTI$Approach2 + cond_app

    ### censor towers
    CTI2 = subset(CTI, Approach2 > min_app & Approach2 < max_app)
    CTI2 = subset(CTI2, SteamT > min_steam & SteamT < max_steam)
    #CTI2 = CTI; # do not censor

    ### some other parameters
    cHL = 1000000
    cRange = CTI2[,3]
    cQ = cHL/(60*8.3*cRange)

    ### first calculate the volume air flow for the design conditions
    LGDC = CTI2[,8]
    MaDC = cQ*8.3*60/LGDC
    VaDC = MaDC * svdry$design[i]

    ### add a design column of cHL to heatload
    HeatLoad$design <- cHL

    ### Use inputs + VaDC for monthly calculations
    for (j in 1:ncol(DryBulb)){
      Ma = VaDC/svdry[i,j] 
      LG = (cQ*8.33*60)/Ma
      MupWT = (NaturalWater[i,j]*(9/5)+32)
      gpm1 = 2.00803212851406
      gpm = gpm1
      gpm_old = rep(0,nrow(CTI2))
      dgpm = 1
      times = 0
      thold = 4e-6 

    ## Create lookup table 
      Tc = seq(0.00, 80, 0.01) 
      Tf = (Tc*(9/5))+32 
      mb = 6.1078*10^((Tc*7.5)/(Tc+237.3)) 
      psia = mb/68.94757293 
      W = (0.622*mb)/(PlantChar$atm_mb[i]-(0.378*mb))
      H = (0.24*Tf)+(W*(1061+0.444*Tf))
      SatH = data.frame(Tc,mb,psia,H,Tf,W)

    
      while(dgpm > thold) {
        DH = ((MupWT-32)*gpm*60*8.3+cHL)/Ma
        Ha2 = Ha1[i,j] + DH
        index = nearest.vec(Ha2,SatH$H)
        To = SatH$Tf[index]
        W2 = SatH$W[index]
        gpm = Ma*(W2-w1[i,j])/(8.3*60)
        dgpm = max(abs(gpm-gpm_old))
        gpm_old = gpm
        times = times + 1
        }
    
      cD = cHL/1000000

      Dutygpm = cD*(1000000*7.48051945564918/(60*(1000*(1-((NaturalWater[i,j])+
        288.9414)/(508929.2*((NaturalWater[i,j])+68.12963))*
        ((NaturalWater[i,j])-3.9863)^2))*0.0624*((((-0.0000614342)*
        (NaturalWater[i,j])^3 + (0.00158927)*(NaturalWater[i,j])^2 - (2.36418)*
        (NaturalWater[i,j]) + 2500.79)*0.947817/(2.2046)))))
    
      Evap = gpm/Dutygpm
    
      emin[i,j] = min(Evap)
      emed[i,j] = median(Evap)
      emax[i,j] = max(Evap)
      e25[i,j] = quantile(Evap,0.25,na.rm=T)
      e75[i,j] = quantile(Evap,0.75,na.rm=T)
    
     # consumption in MGD
      Consumption = (((HeatLoad[i,j]*1000000)/(monthdays[1,j]*24*cHL)) * gpm)/694.44
    
      cmin[i,j] = min(Consumption)
      cmed[i,j] = median(Consumption)
      cmax[i,j] = max(Consumption)
      c25[i,j] = quantile(Consumption,0.25,na.rm=T)
      c75[i,j] = quantile(Consumption,0.75,na.rm=T)
    }
  }
  

  ## Export to excel ----
  evap_out = data.frame(cbind(PlantID,emin,emed,emax,e25,e75))
  months = c("Plant_ID", "percentAllocation", rep(colnames(DryBulb),5))
#  evap_out = rbind(months,evap_out)
  type = c("Plant_ID","percentAllocation", rep("min",13),rep("med",13),rep("max",13),rep("25th",13),rep("75th",13))
  evap_cols <- paste(type, months, sep = "_")
  evap_cols[1] = "Plant_ID"
  evap_cols[2] = "percentAllocation"
  colnames(evap_out) = evap_cols
  
  write.csv(evap_out, str_glue("Output/{yr}_Tower_model_evap_out.csv"),row.names=F)
  
  consumption_out = data.frame(cbind(PlantID,cmin,cmed,cmax,c25,c75))
#  consumption_out = rbind(months,consumption_out)
  colnames(consumption_out) = evap_cols
  
  write.csv(consumption_out,str_glue("Output/{yr}_Tower_model_consumption_out.csv"), row.names=F)
  
  
  # copied from EIAvModel_WD_CU.R some lines changed to remove code related to fewsr 
  
  #dir.create(path ="Output/ModelResults_EIA" )
  
  Comp_Year <- yr
  
  #########################################################################################################
  #Step 2: Import tower model results (will need to change location for new model results)
  #########################################################################################################
  #consumption_out <-read.csv("Model_archive/tower/Output/Tower_model_consumption_out_Orig_NoComplex.csv", stringsAsFactors = FALSE)
  #not needed since consumption_out is created above.
  
  ################################################################################################################################
  #Step 4: Compile and format tower and fews model results (both consumption and withdrawal) into monthly and annual datasets
  ################################################################################################################################
  Med_Mod_Consump <- as.data.frame(consumption_out[-1,c(1,2,16:27)])
  colnames(Med_Mod_Consump) <- c("Plant_id", "percentAllocation", 1:12)
  Med_Mod_Consump[,3:14] <- lapply(Med_Mod_Consump[,3:14], as.numeric)
  #colnames(Fews_Med_Consump) <- c("Plant_id", 1:12)
  #Med_Mod_Consump_UpTemp <- bind_rows(Med_Mod_Consump, Fews_Med_Consump)
  # remove fewsr code -- Monthly_Med_Mod_Consump_UpTemp <- pivot_longer(Med_Mod_Consump_UpTemp, cols = 2:13, names_to = 'Month', values_to = 'Model_Med_Consump')
  Monthly_Med_Mod_Consump_UpTemp <- pivot_longer(Med_Mod_Consump, cols = 3:14, names_to = 'Month', values_to = 'Model_Med_Consump')
  Monthly_Med_Mod_Consump_UpTemp$Month <- as.numeric(Monthly_Med_Mod_Consump_UpTemp$Month)
  # next line doesn't appear to do anything anymore without the fewsr data in the dataframe.
  Monthly_Med_Mod_Consump_UpTemp <- Monthly_Med_Mod_Consump_UpTemp %>% group_by(Plant_id, percentAllocation, Month) %>% summarise(Model_Med_Consump = sum(Model_Med_Consump))  
  Annual_Med_Mod_Consump_UpTemp <- Monthly_Med_Mod_Consump_UpTemp %>% group_by(Plant_id, percentAllocation) %>% summarise(Model_Med_Consump = mean(Model_Med_Consump))
  
  Min_Mod_Consump <- as.data.frame(consumption_out[-1,c(1:14)])
  colnames(Min_Mod_Consump) <- c("Plant_id", "percentAllocation", 1:12)
  Min_Mod_Consump[,3:14] <- lapply(Min_Mod_Consump[,3:14], as.numeric)
  #colnames(Fews_Min_Consump) <- c("Plant_id", 1:12)
  #Min_Mod_Consump_UpTemp <- bind_rows(Min_Mod_Consump, Fews_Min_Consump)
  #Monthly_Min_Mod_Consump_UpTemp <- pivot_longer(Min_Mod_Consump_UpTemp, cols = 2:13, names_to = 'Month', values_to = 'Model_Min_Consump')
  Monthly_Min_Mod_Consump_UpTemp <- pivot_longer(Min_Mod_Consump, cols = 3:14, names_to = 'Month', values_to = 'Model_Min_Consump')
  Monthly_Min_Mod_Consump_UpTemp$Month <- as.numeric(Monthly_Min_Mod_Consump_UpTemp$Month)
  Monthly_Min_Mod_Consump_UpTemp <- Monthly_Min_Mod_Consump_UpTemp %>% group_by(Plant_id, percentAllocation, Month) %>% summarise(Model_Min_Consump = sum(Model_Min_Consump))
  Annual_Min_Mod_Consump_UpTemp <- Monthly_Min_Mod_Consump_UpTemp %>% group_by(Plant_id, percentAllocation) %>% summarise(Model_Min_Consump = mean(Model_Min_Consump))
  
  Max_Mod_Consump <- as.data.frame(consumption_out[-1,c(1,2,29:40)])
  colnames(Max_Mod_Consump) <- c("Plant_id", "percentAllocation", 1:12)
  Max_Mod_Consump[,3:14] <- lapply(Max_Mod_Consump[,3:14], as.numeric)
  #colnames(Fews_Max_Consump) <- c("Plant_id", 1:12)
  #Max_Mod_Consump_UpTemp <- bind_rows(Max_Mod_Consump, Fews_Max_Consump)
  #Monthly_Max_Mod_Consump_UpTemp <- pivot_longer(Max_Mod_Consump_UpTemp, cols = 2:13, names_to = 'Month', values_to = 'Model_Max_Consump')
  Monthly_Max_Mod_Consump_UpTemp <- pivot_longer(Max_Mod_Consump, cols = 3:14, names_to = 'Month', values_to = 'Model_Max_Consump')
  Monthly_Max_Mod_Consump_UpTemp$Month <- as.numeric(Monthly_Max_Mod_Consump_UpTemp$Month)
  Monthly_Max_Mod_Consump_UpTemp <- Monthly_Max_Mod_Consump_UpTemp %>% group_by(Plant_id,percentAllocation, Month) %>% summarise(Model_Max_Consump = sum(Model_Max_Consump))
  Annual_Max_Mod_Consump_UpTemp <- Monthly_Max_Mod_Consump_UpTemp %>% group_by(Plant_id, percentAllocation) %>% summarise(Model_Max_Consump = mean(Model_Max_Consump))
  
  #Calculating median modeled withdrawal by applying 1.4 factor for towers
  Med_Mod_ConsumpTf = Med_Mod_Consump
  for (i in 3:ncol(Med_Mod_ConsumpTf)) {
    Med_Mod_ConsumpTf[,i] = Med_Mod_ConsumpTf[,i] * 1.4
  }
  
  #Calculating median modeled withdrawal by applying 1.4 factor for towers
  #  Med_Mod_WD <- pivot_longer(Med_Mod_Consump, cols = 3:14, names_to = 'Month', values_to = 'Model_Med_WD')
  #  Med_Mod_WD$Model_Med_WD <-  Med_Mod_WD$Model_Med_WD*1.4

  #Calculating minimum modeled withdrawal by applying 1.125 factor for towers
  Min_Mod_ConsumpTf = Min_Mod_Consump
  for (i in 3:ncol(Min_Mod_ConsumpTf)) {
    Min_Mod_ConsumpTf[,i] = Min_Mod_ConsumpTf[,i] * 1.125
  }
  
  #Calculating minimum modeled withdrawal by applying 1.125 factor for towers
  #Min_Mod_WD <- pivot_longer(Min_Mod_Consump, cols = 3:14, names_to = 'Month', values_to = 'Model_Min_WD')
  #Min_Mod_WD$Model_Min_WD <-  Min_Mod_WD$Model_Min_WD*1.125
  
  #Calculating maximum modeled withdrawal by applying 2 factor for towers
  Max_Mod_ConsumpTf = Max_Mod_Consump
  for (i in 3:ncol(Max_Mod_ConsumpTf)) {
    Max_Mod_ConsumpTf[,i] = Max_Mod_ConsumpTf[,i] * 2
  }
  
  #Calculating maximum modeled withdrawal by applying 2 factor for towers
  #Max_Mod_WD <- pivot_longer(Max_Mod_Consump, cols = 3:14, names_to = 'Month', values_to = 'Model_Max_WD')
  #Max_Mod_WD$Model_Max_WD <-  Max_Mod_WD$Model_Max_WD*2
  
#  minmed_WD <- merge(Min_Mod_WD, Med_Mod_WD, by = c("Plant_id","percentAllocation", "Month"))
#  Tower_WDl <- merge(minmed_WD, Max_Mod_WD, by = c("Plant_id", "percentAllocation", "Month"))
#  Tower_WDls <- arrange(Tower_WDl, Plant_id, percentAllocation, as.numeric(Month))

  Tower_WD <- cbind(Min_Mod_ConsumpTf, Med_Mod_ConsumpTf, Max_Mod_ConsumpTf)
#  Tower_WD1 <- Tower_WD[,-15:-16]
  wdcnames = c("Plant_ID","percentAllocation", col_names, "pid2", "pa2", col_names, "pid3", "pa3", col_names)
  wdcnames2 = c("Plant_ID","percentAllocation", rep("min",12), rep("med", 14), rep("max", 14))
  wdcname3 = paste(wdcnames2, wdcnames, sep = "_")
  wdcname3[1] = "Plant_ID"
  wdcname3[2] = "percentAllocation"
  colnames(Tower_WD) = wdcname3  
  Tower_WD <- Tower_WD %>% select(-med_pid2) %>% select(-med_pa2) %>% select(-max_pid3) %>% select(-max_pa3)

  write.csv(Tower_WD,str_glue("Output/{yr}_Tower_model_withdrawal_out.csv"), row.names=F)
  
  # end of copy from EIAvModel_WD_CU.R with quite a few changes made now.
  
}
  
