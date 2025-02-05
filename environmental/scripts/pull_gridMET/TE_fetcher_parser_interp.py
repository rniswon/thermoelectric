#!/usr/bin/env python
# coding: utf-8

# # Pull Gridmet data for Thermoelectric locations using nearest neighbor 
# ## Variables to compare with physics-based model:
# - Air temp [dry bulb]  
# - Relative humidity  
# - Wind speed  
# - Grass reference ET
# 
# ## Variables for Deep learning algorithm
# - Precipitation amount 
# - Specific humidity
# - Surface radition
# - Wind from direction 
# - Alfalfa reference ET
# - Mean vapor pressure deficit
# - Palmer drought severity index
# 
# ## gridMET data
# http://www.climatologylab.org/gridmet.html
# 
# There is a difference between the aggregated and non-aggregated server. For 2015 data, I used the aggregated server, but for multi-year pulls we should use non-aggregated server and choose by date.
# 
# ## Method - use fetcher parser to pull data from thredds.northwestknowledge.net. Use nearest neighbor to pull data by TE location
# 
# ## Some processing
# - Calculate averages (Gridmet has max and min but physical model uses avg)
# - Calculate Wet Bulb (from by dry bulb [air temp] and relative humidity)   
# - Process ET (multiply by 1.05 - ref)
# 
# ### References:
# (0) onhm-fetcher parser https://github.com/nhm-usgs/onhm-fetcher-parser   
# (1) https://climate-cms.org/2018/09/28/line-plots-with-xarray.html  
# (2) https://climate.northwestknowledge.net/MACA/OPENDAP.php  
# (3) Stull, 2011, Wet-Bulb temperature from relative humidity and air temperature  
# (4) Allen, Richard G., et al, 1998, FAO Irrigation and drainage paper No. 56.,  Rome: Food and Agriculture Organization of the United Nations 56.97: e156.
# 
# 
# ### Environment -- use 'ofp_for_te_.yml' 
# Base anaconda installation, conda env update with ofp_env_upd.yml (from onhm-fetcher parser Github, added pyviz)
# 

# ## Setup

# In[1]:


# step 0- Import the needed packages


import geopandas as gpd
import pandas as pd
#import glob
#import zipfile
#import rasterio
import os
#import xarray as xr
#import json
#from rasterstats import zonal_stats
#from rasterio.transform import from_origin
#import geoviews as gv
#import geoviews.feature as gf
#from cartopy import crs

#import datetime as dt
#import numpy as np

## look into these, should be useful for timing how long things take
#from tqdm.notebook import trange, tqdm
#from time import sleep

#gv.extension('bokeh', 'matplotlib')


# In[2]:


# step 1- set up directory, plot the TE locations, set up a geopandas dataframe and add columns for the netcdf data
TEdir = r'C:\WBEEP\Thermoelectric-master\Climate_data_fetcher'
TE_df = gpd.read_file(os.path.join(TEdir, '..\GIS','2015_TE_Model_Estimates_lat.long_COMIDs.shp'))

TE_df.reset_index(drop = True, inplace =True)
TE_df = TE_df[TE_df.STATE !='HI']
TE_df = TE_df[TE_df.STATE !='AK']
TE_df.plot()


# In[19]:


lst_of_plants.iloc[0]


# In[3]:


#get list of plants and change 'NTC/MCRD' name for loop to get data at each point
lst_of_plants = TE_df.loc[:,'PLANT_NAME']
lst_of_plants.update(pd.Series(['NTC_MCRD_Energy_Facility'], index =[737]))


# In[20]:


# for testing purposes since loop of 1122 plants takes a while! Uncomment these if you want to go test something out

lst_of_plants = lst_of_plants[5:100]
lst_of_plants


# In[21]:


# set up lists for loops- 06/16/2020-- add in precip, specific humidity, surface radiation, wind direction, 
# palmer drought index(*only every 10 days, will explore this more), alfalfa ET, vapor pressure deficit


lst_of_vars = ['rmax/rmax', 'rmin/rmin', 'tmmn/tmmn','tmmx/tmmx', 'vs/vs', 'pet/pet']
lst_of_dh = ['relative_humidity', 'relative_humidity', 'air_temperature', 'air_temperature', 
             'wind_speed', 'potential_evapotranspiration']

dirPath='http://thredds.northwestknowledge.net:8080'


# In[22]:


# Step 2- Pull the data from gridMET using two loops-  one for variables and one for plants, this takes some time. 
# Definitely could be faster/better, can work to make this more efficient, but for now here it is.
# On my computer it takes about 1 hour. 
# output csv with data is TE_plants_w_2015_daily_gridMET.csv if you dont want to run this loop

# Pull data from Northwest Knowledge Network. ds is an xarray

for i, plant in enumerate(lst_of_plants):
    for j, val in enumerate(lst_of_vars):
        fileName = '/thredds/dodsC/MET/' + val + '_2015.nc'
        fullfilename= dirPath+fileName
        print(fullfilename)
        ds = xr.open_dataset(fullfilename)
        print(ds, flush =True)
        lathandle=ds['lat']
        lonhandle=ds['lon']
        timehandle=ds['day']
        datahandle = ds[lst_of_dh[j]]
        crshandle=ds['crs']
        ts = datahandle.sizes
        print(type(ts), flush=True)
        print(ts['day'], flush=True)
        dayshape = ts['day']
        Lonshape = ts['lon']
        Latshape = ts['lat']
        var = (ds[lst_of_dh[j]])
        
        var1 = var.interp(lat =TE_df.iloc[i,5] , lon = TE_df.iloc[i,6])
        var_df1 = xr.DataArray.to_dataframe(var1)
        var_df1 = var_df1.drop(['lat', 'lon'], axis = 1)   

        if j == 0:
            var_df = var_df1.copy(deep=True)
        else:
            var_df = var_df.merge(var_df1, left_index = True, right_index = True)
    
    
    var_df['PLANT_NAME'] = plant 
    var_df['EIA_PLANT_'] = TE_df.iloc[i,0]  
    
    if i == 0:
        plants_df = var_df.copy(deep=True)
    else:
        plants_df = plants_df.append(var_df)
        
    print("plant # " + str(i) + " out of 1122 " + "plant name = " + plant)


# In[8]:


plants_df.tail()


# ### Data Processing
# 
# ### Empirical expression for wet-bulb temperature
# ### Eqn (1) from Stull (2011)
# 
# Tw = T * atan[0.151977(RH% + 8.313659)^ (1/2)] + atan(T + RH%) - atan(RH% - 1.676331) + 0.00391838(RH%)^(3/2)* atan(0.023101* RH%) - 4.686035
# 
# 
# Where 
# Tw  = wet bulb temperature (degc) 
# T   = air temperature (degc)
# RH% = relative humidity (%)
# 
# This equation is valid for relative humidities between 5% and 99% and for air temperatures between -20 deg C and
# 50 deg C, except for situations having both low humidity and cold temperature. Over the valid range, errors in
# wet-bulb temperature range from -1 deg C to + 0.65 deg C, with mean absolute error of less than 0.3 deg C.
# 
# ### Are there areas where rel hum is < 5 % ? Are there areas that have low humidity and cold temperatures? Not worried about > 50 deg C

# In[23]:


# verify WB equation from Stull (2011)

T = 20
RH = 50


a = T*np.arctan(0.151977*np.power((RH + 8.313659),(1/2)))
b = np.arctan(T + RH) - np.arctan(RH - 1.676331)
c = 0.00391838*(np.power(RH,(3/2)))*np.arctan(0.023101*RH) - 4.686035

Tw = a + b + c
Tw


# In[24]:


#Step 3 - Manipulate the dataframe
# calculate average rh and air temp from max and min, convert avg temp from Kelvins to C
# calculate wetbulb from air temp (Stull 2011)
# calculate ET from ref_et

#add a column for EIA_PLANT_DATE
plants_df.reset_index(inplace=True)
plants_df['EIA_P_DATE'] = plants_df['EIA_PLANT_'].astype(str) + "_" + plants_df['day'].astype(str)

#rename column headers
cols = ['day', 'rh_max_%', 'rh_min_%', 'air_tmn_K', 'air_tmx_K', 'wnd_spd_m_s','ref_et_gr_mm',
        'PLANT_NAME', 'EIA_PLANT_', 'EIA_P_DATE']
plants_df.columns = cols

#add means and calculated columns
rh = plants_df.loc[:,'rh_max_%':'rh_min_%']
plants_df['rh_avg_%'] = rh.mean(axis = 1)

temp = plants_df.loc[:,'air_tmn_K':'air_tmx_K']
temp = temp.mean(axis = 1)
plants_df['air_tmp_C_avg'] = temp -273.15

#et = plants_df.loc[:,'ref_et_gr_mm']
plants_df['open_wtr_et_mm'] = plants_df['ref_et_gr_mm']*1.05

T = plants_df['air_tmp_C_avg']
RH = plants_df['rh_avg_%']

a = T*np.arctan(0.151977*np.power((RH + 8.313659),(1/2)))
b = np.arctan(T + RH) - np.arctan(RH - 1.676331)
c = 0.00391838*(np.power(RH,(3/2)))*np.arctan(0.023101*RH) - 4.686035

Tw = a + b + c

plants_df['wb_tmp_C'] = Tw


#re_order columns
plants_df = plants_df[['day', 'PLANT_NAME', 'EIA_PLANT_', 'EIA_P_DATE', 'rh_avg_%', 'air_tmp_C_avg', 'open_wtr_et_mm', 
                       'wb_tmp_C', 'rh_max_%', 'rh_min_%','air_tmn_K', 'air_tmx_K','wnd_spd_m_s',
                       'ref_et_gr_mm']]

#create a minimalist df with only variables for comparison with physics-based model
plants_df_minimalist = plants_df[['rh_avg_%','air_tmp_C_avg','wb_tmp_C', 'ref_et_gr_mm']]

#output as a csv 
plants_df_minimalist.to_csv('Interp_from_GM.csv')
plants_df.to_csv('more_vars_interp_from_GM.csv')


# In[25]:


TE_df.to_csv('TE_df.csv')


# In[ ]:




