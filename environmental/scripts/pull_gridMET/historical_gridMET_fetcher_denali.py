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

# Modifications- manually modified 2015_TE_Model_Estimates_lat.long_COMIDS.shp 
# by changing NTC/MCRD Energy Facility to NTC_MCRD Energy Facility

# In[1]:


# step 0- Import the needed packages

import geopandas as gpd
import os
import xarray as xr
import geoviews as gv
import numpy as np

## look into these, should be useful for timing how long things take
#from tqdm.notebook import trange, tqdm
#from time import sleep

gv.extension('bokeh', 'matplotlib')


# In[step 0]:
# set up directory, plot the TE locations, set up a geopandas dataframe and add
# columns for the netcdf data

# use this directory if running locally, replace with your file path
TEdir = r'C:\WBEEP\Thermoelectric-master'

# use this directory if running on Denali, replace with your file path
TE_df = gpd.read_file(os.path.join(TEdir, '7_gis',
                                   '2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
# remove AK and HI, gridMET not avaialable outside of CONUS
TE_df_CONUS = TE_df.drop(TE_df.loc[TE_df['STATE'] == 'AK'].index)
TE_df_CONUS = TE_df_CONUS.drop(TE_df.loc[TE_df['STATE'] == 'HI'].index)
lst_of_plants = TE_df_CONUS.loc[:,'PLANT_NAME']
# In[test step]:
# for testing purposes since loop of 1122 plants takes a while! 
# Uncomment these if you want to go test something out
lst_of_plants = lst_of_plants[0:5]

# In[step 1]:
# set up lists for loops- 06/16/2020-- add in precip, specific humidity, surface radiation, wind direction, 
# palmer drought index(*only every 10 days, will explore this more), alfalfa ET, vapor pressure deficit

lst_of_vars = ['rmax/rmax', 'rmin/rmin', 'tmmn/tmmn','tmmx/tmmx', 'vs/vs', 
               'pet/pet', 'pr/pr', 'sph/sph', 'srad/srad','th/th', 'etr/etr', 'vpd/vpd']
lst_of_dh = ['relative_humidity', 'relative_humidity', 'air_temperature', 'air_temperature', 
             'wind_speed', 'potential_evapotranspiration', 'precipitation_amount', 'specific_humidity',
            'surface_downwelling_shortwave_flux_in_air', 'wind_from_direction', 'potential_evapotranspiration', 
            'mean_vapor_pressure_deficit']

dirPath='http://thredds.northwestknowledge.net:8080'


# In[step 2]:
# Pull the data from gridMET using two loops-  one for variables and one for plants, this takes some time. 
# Definitely could be faster/better, can work to make this more efficient, but for now here it is.
# On my computer it takes about 1 hour. 
# output csv with data is TE_plants_w_2015_daily_gridMET.csv if you dont want to run this loop

# Pull data from Northwest Knowledge Network. ds is an xarray


#loop through years, variables, plants, output a csv with variables for every
# within that year.
yr = '_1995'
for i, val in enumerate(lst_of_vars):
    fileName = '/thredds/dodsC/MET/' + val + yr + '.nc'
    fullfilename= dirPath+fileName
    print(fullfilename)
    fullfilename= dirPath+fileName
    ds = xr.open_dataset(fullfilename)
    print(ds, flush =True)
    lathandle=ds['lat']
    lonhandle=ds['lon']
    timehandle=ds['day']
    datahandle = ds[lst_of_dh[i]]
    crshandle=ds['crs']
    ts = datahandle.sizes
    print(type(ts), flush=True)
    print(ts['day'], flush=True)
    dayshape = ts['day']
    Lonshape = ts['lon']
    Latshape = ts['lat']
    var = (ds[lst_of_dh[i]])
    
    for j, plant in enumerate(lst_of_plants):
        var1 = var.sel(lat =TE_df_CONUS.iloc[j,5] , lon = TE_df_CONUS.iloc[j,6] , 
                       method = 'nearest')
        var_df1 = xr.DataArray.to_dataframe(var1)
        var_df1 = var_df1.drop(['lat', 'lon'], axis = 1)
        var_df1['PLANT_NAME'] = plant 
        var_df1['EIA_PLANT_'] = TE_df.iloc[j,0] 
        if j == 0:
            plants_df = var_df1.copy(deep=True)
        else:
            plants_df = plants_df.append(var_df1)

    # create a new column, EIA_P_DATE combining the plant and the date.  
    # come back to this and comment what and why each line
    plants_df.reset_index(inplace=True)
    plants_df['EIA_P_DATE'] = plants_df['EIA_PLANT_'].astype(str) + "_" + plants_df['day'].astype(str)
    plants_df.rename({lst_of_dh[i]: val}, axis =1, inplace = True)

    if i == 0:
        final_df = plants_df.copy(deep=True)
    else:        
        final_df = final_df.merge(plants_df, left_on = 'EIA_P_DATE', right_on = 
                                  'EIA_P_DATE')
    print("var " + str(i) + " out of 11 " + "variables = " + val)


# In[step 3]:
# clean up- lots of repeat columns

#get rid of the extra columns
final_df = final_df[['EIA_P_DATE', 'rmax/rmax', 
        'rmin/rmin', 'tmmn/tmmn','tmmx/tmmx', 'vs/vs', 'pet/pet', 'pr/pr', 
        'sph/sph', 'srad/srad','th/th', 'etr/etr', 'vpd/vpd']]

# In[step 4]:
# verify WB equation from Stull (2011) for relative humidity

T = 20
RH = 50


a = T*np.arctan(0.151977*np.power((RH + 8.313659),(1/2)))
b = np.arctan(T + RH) - np.arctan(RH - 1.676331)
c = 0.00391838*(np.power(RH,(3/2)))*np.arctan(0.023101*RH) - 4.686035

Tw = a + b + c
Tw


# In[Step 5]:
# Manipulate the dataframe and output as csv]

# calculate average rh and air temp from max and min, convert avg temp from Kelvins to C
# calculate wetbulb from air temp (Stull 2011)
# calculate ET from ref_et

#rename column headers
cols = ['EIA_P_DATE', 'rh_max', 'rh_min', 'air_tmn_K', 'air_tmx_K', 'wnd_spd_m_s'
        ,'ref_et_gr_mm', 'precip_mm', 'sp_h_kg_kg', 'sur_rad_Wm2',
        'wnd_dir_dg_clk_frm_N','ref_et_alf_mm','vpdef_kPa']
final_df.columns = cols

#add means and calculated columns
rh = final_df.loc[:,'rh_max':'rh_min']
final_df['rh_avg'] = rh.mean(axis = 1)

temp = final_df.loc[:,'air_tmn_K':'air_tmx_K']
temp = temp.mean(axis = 1)
final_df['air_tmp_C_avg'] = temp - 273.15

#et = plants_df.loc[:,'ref_et_gr_mm']
final_df['open_wtr_et_mm'] = final_df['ref_et_gr_mm']*1.05

T = final_df['air_tmp_C_avg']
RH = final_df['rh_avg']

a = T*np.arctan(0.151977*np.power((RH + 8.313659),(1/2)))
b = np.arctan(T + RH) - np.arctan(RH - 1.676331)
c = 0.00391838*(np.power(RH,(3/2)))*np.arctan(0.023101*RH) - 4.686035

Tw = a + b + c

final_df['wb_tmp_C'] = Tw


#re_order columns
final_df = final_df[['EIA_P_DATE', 'rh_avg', 'air_tmp_C_avg', 'open_wtr_et_mm', 
                       'wb_tmp_C', 'rh_max', 'rh_min', 'sp_h_kg_kg','air_tmn_K',
                       'air_tmx_K', 'precip_mm','wnd_spd_m_s',
                       'wnd_dir_dg_clk_frm_N','ref_et_gr_mm','ref_et_alf_mm',
                       'sur_rad_Wm2','vpdef_kPa']]


final_df_minimalist = final_df[['EIA_P_DATE', 'air_tmp_C_avg', 'wb_tmp_C', 
                                  'open_wtr_et_mm', 'wnd_spd_m_s']]
#output as a csv 
fname = 'gridMET_minimalist_at_TE_locations' + yr + '.csv'
fname2 = 'gridMET_at_TE_locations' + yr + '.csv'
final_df_minimalist.to_csv(os.path.join(TEdir,'5_output', 'historical', fname))
final_df.to_csv(os.path.join(TEdir,'5_output','historical', fname2))

