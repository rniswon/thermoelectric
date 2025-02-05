# Section 1: load the libraries ------------------------------------------------
library(here)
library(dplyr)

--------------------------------------------------------------------------------
# Section 1.x-delete: extra stuff, don't need to run, just in case bits---------
  ## clear workspace environment
rm(list=ls(all=T)) 

#not needed
input_header = here('input', 'towers', 'Tower_input_header_data.csv')

param = read.csv(input_header, header=T, skip=5, nrows=1,
                 colClasses = c(rep(NA, 3), rep("NULL", 10)))
#min_T = param[1,1]
#min_approach = param[1,2]
#max_approach = param[1,3]

#monthdays = read.csv(input_header, header=T, skip=3, nrows=1,
#                colClasses = c(rep(NA, 13),rep("NULL", 0)))
#input_data = here('input', 'towers', 'Version_1.1_2015_TE_input_data.csv')

### Start the timer
#ptm <- proc.time()

#so which are in New and old? there are 5 that are in new but not old
# 5 plants in Hawaii

#results1 = setdiff(plant_char$EIA_PLANT_ID, PlantChar$Plant.Code)
--------------------------------------------------------------------------------
# Section 2: input files and variables------------------------------------------
# input straight from data release
data = read.csv(here('auto_test', 'input', 'Version_1.1_2015_TE_input_data.csv'))
# filter for recirculating towers (694)
tower_simple <-data[data$COOLING_TYPE == 'RECIRCULATING TOWER', ]
# old format (for checking)
data_old = read.csv(here('input', 'towers', 
                         'Tower_input_WBfirst_DBsecond_output_matches_published_output.csv'), 
                    header=T,na.strings="-")
#locations, merge this at the end for plotting, only need EIA, name, lat long
location <- read.csv(here('auto_test','input','2015_TE_Model_Estimates_lat.long_COMIDs.csv'))

# published output (for comparison)
pub_output = read.csv(here('auto_test','model_archive', '2015_TE_Model_Estimates.csv'))
tower_s_output <- pub_output[pub_output$COOLING_TYPE == 'RECIRCULATING TOWER', ]

# variables
# variables that were in param
mn_t = 4.44444 #unit, deg C?
mn_approach <-  5 #unit, deg C?
mx_approach <-  15 #unit, deg C?
## do we need the design? should modify this so it will work for any year (leap years)
monthdays<- data.frame(jan = 31, feb = 28, mar = 31, apr = 30, may = 31, jun = 30,
                       jul = 31, aug = 31, sep = 30, oct = 31, nov = 30, dec = 31, 
                       design = 30)
condenser_approach <- 5 # condenser approach, units?
mn_stm_tmp <- 79 # minimum steam temperature from manual, units, F?
stm_tmp_cush <- 15 # steam temperature cushion above wet bulb temperature, units F?
cush_exceed <- 5 # cushion by which the steam temperature can exceed the 
# "typical" steam temperature, units, F?


# CTI_input csv does not need to be updated, this is kind of a lookup table
CTI_input = here('input', 'towers', 'CTI_input.csv')
CTI = read.csv(file = CTI_input, header=T, skip=3)

#C, slope, range, approach_600, approach_3500, WBT, LG
#tower_input <- data[data$MODEL_TYPE == 'RECIRCULATING TOWER', ]

# In 2015, there are 829 plants with model type recirculating tower, 
# 694 with cooling type recirculating tower (non complex), compared to the 689 
# from the old dataset, so the new dataset includes Hawaii.
### Plant characteristics: [EIA_number, elev, pond area]
PlantChar = data_old[1:3]

#elev, pond area
plant_char <- tower_simple %>%
  select(EIA_PLANT_ID, ELEVATION, POND_AREA)




### Design characteristics, not sure if this is necessary, this is Tdb, Twb, and
### nWT, appears to be a max or something
DesignChar = data_old[64:66]

### Added heat load MMBtu, same as CD
HeatLoad = data_old[4:15]
col_names = colnames(HeatLoad)
heat_load = tower_simple %>%
  select(CD_Jan : CD_Dec)
### Dry bulb air temperature Ta  - order of input columns is Condenser duty, dry
# bulb, wetbulb, water temp, windspeed.
DryBulb = data_old[16:27]
colnames(DryBulb) = col_names

dry_bulb <- tower_simple %>%
  select(DB_Jan: DB_Dec)

### Wet bulb air temperature Twb    									
WetBulb = data_old[28:39]
colnames(WetBulb) = col_names
wet_bulb = tower_simple %>%
  select(WB_Jan: WB_Dec)

### Natural water temperature 										
NaturalWater = data_old[40:51]
colnames(NaturalWater) = col_names
nat_water = tower_simple %>%
  select(WT_Jan: WT_Dec)

### Wind speed at 2m W (mph)  										    									
WindSpeed = data[51:62]
colnames(WindSpeed) = col_names
wind_speed = tower_simple %>%
  select(WS_Jan: WS_Dec)

## plant characteristics pre-calculations ----
  
## Create unique vector for plant ID
PlantID = data.frame(PlantChar[,1])
colnames(PlantID) = "Plant_ID"

## convert elevation to mb to psia for all plants
#PlantChar$atm_mb = ((44331.514-(PlantChar$Elevation*0.3048))/11880.516)^(1/0.1902632) 
tower_simple$atm_mb = ((44331.514-(tower_simple$ELEVATION*0.3048))/11880.516)^(1/0.1902632) 
#PlantChar$atm_psia = PlantChar$atm_mb/68.94757293 # this looks to be psi, not psia
tower_simple$atm_psia = tower_simple$atm_mb/68.94757293

## Month (and design characteristics) x plant calculations ----

## this section of codes creates matrices with columns 1-12 as Jan-Dec and
## column 13 is the design condition, and the rows are the plants

## set minimum T for wet and dry bulbs
dry_bulb[dry_bulb<min_T] = min_T
WetBulb[WetBulb<min_T] = min_T

## Calculate saturation vapor pressure at inlet air wet bulb temperature, Ward 12
# could use Lowe 1977 if we want to
Pw_mb = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(WetBulb+273)))+
                     (-0.545/0.11)*log((WetBulb+273)/273)) 
# change this var 

Pw_psia = Pw_mb/68.94757293 

## saturated vapor pressure from dry bulb temperature- put dry bulb in kelvins
# https://sciencing.com/calculate-saturation-pressure-7834338.html
Ps_mb = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(DryBulb+273)))+
                     (-0.545/0.11)*log((DryBulb+273)/273))
## exp and log are base 10
Ps_psia = Ps_mb/68.94757293

## Actual vapor pressure in inlet air
vap_mb = Pw_mb - (PlantChar$atm_mb*0.00066*(DryBulb-WetBulb)*(1+(0.00115*WetBulb)))

## relative humidity of inlet air
phi = vap_mb/Ps_mb

## Pounds of water vapor per pound of dry air in inlet air, calculated per L&M '71 eqn 3
w1 = (0.622*phi*Ps_psia)/(PlantChar$atm_psia-(phi*Ps_psia))

## enthalpy of inlet air calculated per L&M '71 eqn 4 
## same as specific heat of inflow==
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

### Create new LG values (1,1.33,1.667,2) # not sure what this is
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

--------------------------------------------------------------------------------
#calculate typical steam, LG
# first loop, i loops through the plants (rows)
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

--------------------------------------------------------------------------------
#loop 2, loops through the months (columns)

### Use inputs + VaDC for monthly calculations
for (j in 1:ncol(DryBulb)){
    Ma = VaDC/svdry[i,j] #define, units
    LG = (cQ*8.33*60)/Ma #define, units
    MupWT = (NaturalWater[i,j]*(9/5)+32) #define, units
    gpm1 = 2.00803212851406 #define, units
    gpm = gpm1 #define, units
    gpm_old = rep(0,nrow(CTI2)) #define, units
    dgpm = 1 #define, units
    times = 0 #define, units
    thold = 4e-6 #define, units

    ## Create lookup table 
    Tc = seq(0.00, 80, 0.01) #define, units
    Tf = (Tc*(9/5))+32 #define, units
    mb = 6.1078*10^((Tc*7.5)/(Tc+237.3)) #define, units 
    psia = mb/68.94757293 #define, units
    W = (0.622*mb)/(PlantChar$atm_mb[i]-(0.378*mb)) #define, units
    H = (0.24*Tf)+(W*(1061+0.444*Tf)) #define, units
    SatH = data.frame(Tc,mb,psia,H,Tf,W) #define, units

    
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


--------------------------------------------------------------------------------
## Export to excel ----
evap_out = data.frame(cbind(PlantID,emin,emed,emax,e25,e75))
months = c("Plant_ID", rep(colnames(DryBulb),5))
evap_out = rbind(months,evap_out)
type = c("Plant_ID",rep("min",13),rep("med",13),rep("max",13),rep("25th",13),rep("75th",13))
colnames(evap_out) = type

write.csv(evap_out, here('2_physical_models','output','towers',
                         'wb_first_db_second_Tower_model_evap_out.csv'),row.names=F) #modify this line

consumption_out = data.frame(cbind(PlantID,cmin,cmed,cmax,c25,c75))
consumption_out = rbind(months,consumption_out)
colnames(consumption_out) = type

write.csv(consumption_out, here('2_physical_models','output','towers',
                                'wb_first_db_second_consumption_out.csv'), row.names=F)#modify this line
--------------------------------------------------------------------------------
## Plots ----
library(ggplot2)
library(reshape2)
library(RColorBrewer)

### melt wide frames into long format and combine
Twbm = melt(WetBulb[,1:13]) #theres an error with this
Tdbm = melt(DryBulb[,1:13])
emedm = melt(emed[,1:13])
CC = cbind(Twbm,Tdbm[,2],emedm[,2])
colnames(CC) = c("month","Twb","Tdb","medEvap")
CC$Plant_ID = rep(PlantChar$Plant.Code,13)
CC$elev = rep(PlantChar$Elevation,13)
CC2 = subset(CC, Twb > 25 & medEvap < 0.8)

# all points colored by month
p = ggplot(data=CC)
p = p + geom_point(aes(Twb,medEvap,color = month),size=4)
p = p + scale_color_manual(values=c("royalblue4", "royalblue3",
                                    "cornflowerblue", "darkgoldenrod",
                                    "darkgoldenrod1", "firebrick3",
                                    "darkred", "darkred",
                                    "darkgoldenrod1", "darkgoldenrod",
                                    "cornflowerblue", "royalblue3",
                                    "green"))
p = p + theme_bw(base_size=20)
p = p + geom_text(data=CC2, aes(Twb,medEvap,label=Plant_ID),size=5)
p = p + geom_hline(aes(yintercept=1))
p

# all points colored by elevation
p2 = ggplot(data=CC)
p2 = p2 + geom_point(aes(Twb,medEvap,color = elev),size=4)
p2 = p2 + scale_color_gradientn(colours = rev(brewer.pal(n=11,name = 'RdGy')))
p2 = p2 + theme_bw(base_size=20)
p2 = p2 + geom_text(data=CC2, aes(Twb,medEvap,label=Plant_ID),size=5)
p2 = p2 + geom_hline(aes(yintercept=1))
p2

# subset of points
p3 = ggplot(data=CC2,aes(Twb,medEvap,color = month))
#p3 = p3 + geom_point(size=5)
p3 = p3 + theme_grey(base_size=20)
p3 = p3 + xlim(25,30)
p3 = p3 + geom_text(aes(label=Plant_ID),size=5)

## Maps ----
library(RColorBrewer)
library(ggmap)

## subset the plants used in the model
emed = data.frame(emed)
colnames(emed) = colnames(DryBulb)
PlantChar$med = emed$design
plants = merge(location,PlantChar, by.x = "Plant_ID", by.y= "Plant.Code")
plants = subset(plants, lon > -130) #excludes AK and HI
plants2 = subset(plants, med>1)

## load the state data
state = map_data('state')

  
m1 = ggplot() + ggtitle("U.S. Thermoelectric Plants")
m1 = m1 + geom_polygon(data=state,aes(long,lat, group=group), color = "white", fill= "black") 
m1 = m1 + coord_fixed(1.3) + theme_bw(base_size = 20)
m1 = m1 + geom_point(data=plants, aes(lon,lat,color=med), size=3)
m1 = m1 + scale_color_gradientn(colours = rev(brewer.pal(n=11,name = 'RdYlBu')))
m1 = m1 + geom_point(data=plants2, aes(lon,lat,color=med), size=3, color="green")
m1

