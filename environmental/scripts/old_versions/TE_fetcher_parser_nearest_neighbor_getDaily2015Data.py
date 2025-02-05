#!/usr/bin/env python
# coding: utf-8

# # Pull Gridmet data for Thermoelectric locations using nearest neighbor. 
# ## Variables:
# - Dry bulb  
# - Relative humidity  
# - Wet Bulb (calculated by dry bulb and relative humidity)  
# - Wind speed  
# - ET
# 
# ## Method - use fetcher parser to pull data from thredds.northwestknowledge.net. Use nearest neighbor to pull data by TE location
# 
# ## Some processing
# - calculate averages (Gridmet shows max and min)
# - calculate wetbulb from Stull (2011)
# - process ET (multiply by 1.05 - ref)
# 
# ## Analysis
# - compare to 2015 estimated values 
# 
# ### Scrapped together by Amy Galanter from these references:
# (0) onhm-fetcher parser https://github.com/nhm-usgs/onhm-fetcher-parser   
# (1) https://climate-cms.org/2018/09/28/line-plots-with-xarray.html  
# (2) https://climate.northwestknowledge.net/MACA/OPENDAP.php  
# (3) Stull, 2011, Wet-Bulb temperature from relative humidity and air temperature
# (4) Allen, Richard G., et al, 1998, FAO Irrigation and drainage paper No. 56.,  Rome: Food and Agriculture Organization of the United Nations 56.97: e156.
# 
# 
# ### Environment -- use ofp_for_te.yml 
# Base anaconda installation, conda env update with ofp_env_upd.yml (from onhm-fetcher parser Github, added pyviz)
# 

# ## Setup

# In[2]:


# step 0- Import the needed packages
get_ipython().run_line_magic('matplotlib', 'inline')
get_ipython().run_line_magic('pylab', 'inline')
pylab.rcParams['figure.figsize'] = (10.0, 8.0)

import geopandas as gpd
import pandas as pd
import glob
import zipfile
import rasterio
import os
import xarray as xr
import json
from rasterstats import zonal_stats
from rasterio.transform import from_origin
import geoviews as gv
import geoviews.feature as gf
from cartopy import crs

import datetime as dt
import numpy as np

gv.extension('bokeh', 'matplotlib')


# In[3]:

# step 0.1 import the df without going through the loop

# step 1- set up directory, plot the TE locations, set up a geopandas dataframe and add columns for the netcdf data
TEdir = r'C:\WBEEP\Thermoelectric-master\Climate_data_fetcher'
TE_df = gpd.read_file(os.path.join(TEdir, '..\GIS','2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
TE_df.plot()
TE_df.reset_index(drop = True, inplace =True)
TE_df.head()


# In[4]:
# step 0.1 import the df without going through the loop


#get list of plants and change 'NTC/MCRD' name for loop to get data at each point
lst_of_plants = TE_df.loc[:,'PLANT_NAME']
lst_of_plants.update(pd.Series(['NTC_MCRD_Energy_Facility'], index =[737]))


# In[12]:


#lst_of_plants = lst_of_plants[0:5]


# In[5]:


# set up lists for loops-
cols = ['rh_max_%', 'rh_min_%', 'air_tmn_K', 'air_tmx_K', 'wnd_spd_m_s','ref_et_gr_mm']
lst_of_vars = ['rmax/rmax', 'rmin/rmin', 'tmmn/tmmn','tmmx/tmmx', 'vs/vs', 'pet/pet']
lst_of_dh = ['relative_humidity', 'relative_humidity', 'air_temperature', 'air_temperature', 
             'wind_speed', 'potential_evapotranspiration']

dirPath='http://thredds.northwestknowledge.net:8080'


# In[14]:


# two loops-  one for variables and one for plants
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
        var1 = var.sel(lat =TE_df.iloc[i,5] , lon = TE_df.iloc[i,6] , method = 'nearest')
        var_df1 = xr.DataArray.to_dataframe(var1)
        var_df1 = var_df1.drop(['lat', 'lon'], axis = 1)
        
        
        
        if j == 0:
            var_df = var_df1.copy(deep=True)
        else:
            var_df = var_df.merge(var_df1, left_index = True, right_index = True)

    var_df['plant_name'] = plant        
    if i == 0:
        plants_df = var_df.copy(deep=True)
    else:
        plants_df = plants_df.append(var_df)
        
    print(i)
 #       f_name    = '2015_'+ str(val) + '.csv'
 #       f_path    = os.path.join(TEdir, 'gridmet_vars_dfs',f_name)
 #       var_df.to_csv(f_path)

#processing
# if you read in from csv
plants_df = pd.read_csv('plants_df.csv')
plants_df = plants_df.set_index('day')


# import TE input data and evap data

#p_mod_evap = pd.read_excel(os.path.join(TEdir, '\Ponds_info','Recirc_Ponds_update_2.4.20.xlsx'), sheet_name = 'Summary')
p_mod_evap = pd.read_csv(os.path.join(TEdir, 'Ponds_info', 'Recirc_Ponds_update_2.4.20.csv'))
TE_input_data_2015 = pd.read_csv(os.path.join(TEdir, '..\TE_Harris_Diehl_2015', '2015_TE_input_data.csv'))

# summarize DB (air temp), WB, WS by month

# convert evap to inches and summarize by April-Oct and annually



