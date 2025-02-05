#!/usr/bin/env python
# coding: utf-8
"""

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
# ### Environment -- use 'geo.yml' 
# open anaconda prompt
# navigate to location of the geo.yml file (Thermoelectric>environmental)
# type in 'conda env create -f geo.yml'
# conda activate geo 
# 

# ## Setup

# Modifications- manually modified 2015_TE_Model_Estimates_lat.long_COMIDS.shp 
# by changing NTC/MCRD Energy Facility to NTC_MCRD Energy Facility
"""
# In[step 0- import the needed packages]:

import geopandas as gpd
import pandas as pd
import os
#import glob
import xarray as xr
import geoviews as gv
import numpy as np
#import contextily as ctx
#from shapely.geometry import Point, polygon
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
## look into these, should be useful for timing how long things take
#from tqdm.notebook import trange, tqdm
#from time import sleep

gv.extension('bokeh', 'matplotlib')

# In[step 1- set up directory]:
# 

directory = os.path.dirname(__file__) # where this script is located
list_dir = os.chdir(os.path.join(directory, '..','..','data')) # where the all plants list is located

# In[step 2- read in shapefile]:
# 6 plants in AK and 13 plants in HI, remove these because gridMET is 
# not avaialable outside of CONUS, leaves 1479 plants in CONUS
te_df = gpd.read_file('all_plants_all_years_1498.shp')
ak_plants = te_df[te_df.state == 'AK'].shape[0]
hi_plants = te_df[te_df.state == 'HI'].shape[0]
te_df_conus = te_df.drop(te_df.loc[te_df['state'] == 'AK'].index)
te_df_conus = te_df_conus.drop(te_df.loc[te_df['state'] == 'HI'].index)
te_df_conus.reset_index(inplace = True)
#te_df_conus.shape[0]
lst_of_plants = te_df_conus.loc[:,'plant_name']

# In[test step]:
# for testing purposes since loop of 1122 plants takes a while! 
# Uncomment these if you want to go test something out
#lst_of_plants = lst_of_plants[0:5]

# In[step 3- set up loops]:
# set up lists for loops- 06/16/2020-- add in precip, specific humidity, surface radiation, wind direction, 
# palmer drought index(*only every 10 days, will explore this more), alfalfa ET, vapor pressure deficit

lst_of_vars = ['rmax/rmax', 'rmin/rmin', 'tmmn/tmmn','tmmx/tmmx', 'vs/vs', 
               'pet/pet', 'pr/pr', 'sph/sph', 'srad/srad','th/th', 'etr/etr', 'vpd/vpd']
lst_of_dh = ['relative_humidity', 'relative_humidity', 'air_temperature', 'air_temperature', 
             'wind_speed', 'potential_evapotranspiration', 'precipitation_amount', 'specific_humidity',
            'surface_downwelling_shortwave_flux_in_air', 'wind_from_direction', 'potential_evapotranspiration', 
            'mean_vapor_pressure_deficit']

dirPath='http://thredds.northwestknowledge.net:8080'

# In[step 4- verify equation and conversions]:
# verify WB equation from Stull (2011) for relative humidity, answer should be 13.7 dec C

T = 20
RH = 50


a = T*np.arctan(0.151977*np.power((RH + 8.313659),(1/2)))
b = np.arctan(T + RH) - np.arctan(RH - 1.676331)
c = 0.00391838*(np.power(RH,(3/2)))*np.arctan(0.023101*RH) - 4.686035

Tw = a + b + c
Tw

m_s_to_mph = 60*60/1609.344
mm_to_inch = 1/25.4
# In[step 5- pull from gridMET]:
# Pull the data from gridMET using two loops-  one for variables and one for plants, this takes some time. 
# Definitely could be faster/better, can work to make this more efficient, but for now here it is.
# On my computer it takes about 1 hour. 
# output csv with data is TE_plants_w_2015_daily_gridMET.csv if you dont want to run this loop

# Pull data from Northwest Knowledge Network. ds is an xarray


#loop through years, variables, plants, output a csv with variables for every
# within that year.
yrs = ['2021']
# yrs = ['2000','2001','2002','2003','2004','2005','2006','2007','2008','2009',
#         '2010','2011','2012','2013', '2014','2015','2016','2017','2018','2019','2020']
#yrs = ['1995','1996']
for y, yr in enumerate(yrs):
    
    for i, val in enumerate(lst_of_vars):
        fileName = '/thredds/dodsC/MET/' + val + '_' + yr + '.nc'
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
            var1 = var.sel(lat = te_df_conus.loc[j,'latitude'] , lon = te_df_conus.loc[j,'longitude'] , 
                           method = 'nearest')
            var_df1 = xr.DataArray.to_dataframe(var1)
            var_df1 = var_df1.drop(['lat', 'lon'], axis = 1)
            var_df1['plant_name'] = plant 
            var_df1['eia_id'] = te_df_conus.loc[j,'eia_id'] 
            if j == 0:
                plants_df = var_df1.copy(deep=True)
            else:
                plants_df = pd.concat([plants_df, var_df1]) 
    
        # create a new column, EIA_P_DATE combining the plant and the date.  
        # come back to this and comment what and why each line
        plants_df.reset_index(inplace=True)
        plants_df['eia_p_date'] = plants_df['eia_id'].astype(str) + "-" + plants_df['day'].astype(str)
        plants_df.rename({lst_of_dh[i]: val}, axis =1, inplace = True)
    
        if i == 0:
            final_df = plants_df.copy(deep=True)
        else:        
            final_df = final_df.merge(plants_df, left_on = 'eia_p_date', right_on = 
                                      'eia_p_date')
        print("var " + str(i) + " out of 11 " + "variables = " + val)
        
    # clean up- lots of repeat columns
    final_df = final_df[['eia_p_date', 'rmax/rmax', 
    'rmin/rmin', 'tmmn/tmmn','tmmx/tmmx', 'vs/vs', 'pet/pet', 'pr/pr', 
    'sph/sph', 'srad/srad','th/th', 'etr/etr', 'vpd/vpd']]
    
    # Manipulate the dataframe and output as csv]
    # calculate average rh and air temp from max and min, 
    # convert avg temp from Kelvins to C
    # calculate wetbulb from air temp (Stull 2011)
    # calculate ET from ref_et
    #rename column headers
    cols = ['eia_p_date', 'rh_max', 'rh_min', 'air_tmn_K', 'air_tmx_K', 'wnd_spd_m_s'
    ,'ref_et_gr_mm', 'precip_mm', 'sp_h_kg_kg', 'sur_rad_Wm2',
    'wnd_dir_dg_clk_frm_N','ref_et_alf_mm','vpdef_kPa']
    final_df.columns = cols
    #add means and calculated columns
    rh = final_df.loc[:,'rh_max':'rh_min']
    #final_df['rh_avg'] = final_df.loc[:,'rh_max': 'rh_min'].mean(axis = 1)
    final_df['rh_avg'] = rh.mean(axis = 1) # axis = 1 averages between the 2 values in each row
    temp = final_df.loc[:,'air_tmn_K':'air_tmx_K']
    temp = temp.mean(axis = 1)
    final_df['air_tmp_C_avg'] = temp - 273.15 
    final_df['open_wtr_et_mm'] = final_df['ref_et_gr_mm']*1.05
    T = final_df['air_tmp_C_avg']
    RH = final_df['rh_avg']

    a = T*np.arctan(0.151977*np.power((RH + 8.313659),(1/2)))
    b = np.arctan(T + RH) - np.arctan(RH - 1.676331)
    c = 0.00391838*(np.power(RH,(3/2)))*np.arctan(0.023101*RH) - 4.686035
    Tw = a + b + c   
    final_df['wb_tmp_C'] = Tw
    #re_order columns
    final_df = final_df[['eia_p_date', 'rh_avg', 'air_tmp_C_avg', 'open_wtr_et_mm', 
                   'wb_tmp_C', 'rh_max', 'rh_min', 'sp_h_kg_kg','air_tmn_K',
                   'air_tmx_K', 'precip_mm','wnd_spd_m_s',
                   'wnd_dir_dg_clk_frm_N','ref_et_gr_mm','ref_et_alf_mm',
                   'sur_rad_Wm2','vpdef_kPa']]
    final_df['month'] = final_df['eia_p_date'].astype(str).str[-5:-3]
    final_df['eia_plant'] = final_df['eia_p_date'].astype(str).str[0:-11]
    final_df['eia_plant'] = final_df['eia_plant'].astype('int64')
    monthly = pd.pivot_table(final_df, index = ['eia_plant', 'month'], aggfunc=np.mean)
    monthly.reset_index(inplace=True)
    monthly['date'] = monthly['month'].astype(str) + '-15-' + yr
    monthly['date'] = monthly['date'].astype('datetime64[ns]')
    monthly['eia_p_date'] = monthly['eia_plant'].astype(str) + '-' +  yr + '-' + monthly['month']
    
    # create a minimalist dataframe with units converted for physics based model
    df_min = monthly[['eia_p_date', 'eia_plant','date','air_tmp_C_avg', 'wb_tmp_C', 
                              'open_wtr_et_mm', 'wnd_spd_m_s']]
    df_min['wnd_spd_mph'] = df_min.wnd_spd_m_s*m_s_to_mph
    df_min['owtr_et_in'] = df_min.open_wtr_et_mm*mm_to_inch
    df_min.drop(columns = ['wnd_spd_m_s', 'open_wtr_et_mm'], inplace = True)
    # df_min['month'] = df_min['eia_p_date'].astype(str).str[-5:-3]
    # df_min['eia_plant'] = df_min['eia_p_date'].astype(str).str[0:-11]
    # df_min['eia_plant'] = df_min['eia_plant'].astype('int64')
    
    # #average daily values to monthly
    # month_means = pd.pivot_table(df_min, index = ['eia_plant','month'], aggfunc=np.mean)
    # month_means.reset_index(inplace=True)

    # month_means['date'] = month_means['month'].astype(str) + '-15-' + yr
    # month_means['date'] = month_means['date'].astype('datetime64[ns]')
    # month_means['eia_p_date'] = month_means['eia_plant'].astype(str) + '-' +  yr + '-' + month_means['month']
    #re order columns
    df_min.rename(columns ={'air_tmp_C_avg':'db_tmp_c','wb_tmp_C':'wb_tmp_c'}, 
                       inplace = True)
    df_min = df_min[['eia_p_date','eia_plant','date','db_tmp_c','wb_tmp_c','wnd_spd_mph', 'owtr_et_in']]
    
    # Missy wants data from gridMET in native units no min and max averaging
    monthly = monthly[['eia_p_date','eia_plant','date','open_wtr_et_mm','wb_tmp_C', 
                       'rh_max', 'rh_min', 'sp_h_kg_kg','air_tmn_K','air_tmx_K', 
                       'precip_mm','wnd_spd_m_s','wnd_dir_dg_clk_frm_N',
                       'ref_et_gr_mm','ref_et_alf_mm','sur_rad_Wm2','vpdef_kPa']]
    
    # output 2 files for each year- 1 minimalist for physics-based models with 
    # units converted, and another for ML models with no min max averaging or
    # unit conversions.
    fname = yr + '_gridMET_env_input' + '.csv'
    fname2 = yr + '_all_vars_gridMET_env_input' + '.csv'
    print('num rows = ', len(df_min), '(12 months of data for', (len(df_min)/12), ' plants)')
    df_min.to_csv(os.path.join('to_sciencebase','forPB',fname), index = False)
    monthly.to_csv(os.path.join('to_sciencebase','forML',fname2), index = False)
    #output as a csv 
    # fname = 'gridMET_minimalist_at_TE_locations_' + yr + '.csv'
    # fname2 = 'gridMET_at_TE_locations_' + yr + '.csv'
    # final_df_minimalist.to_csv(fname, index = False)
    # final_df.to_csv(fname2, index = False)

# # In[Step 6 - prep for output]:
# # convert meters/sec to mph for wind speed
# m_s_to_mph = 60*60/1609.344
# mm_to_inch = 1/25.4
# df_min['wnd_spd_mph'] = df_min.wnd_spd_m_s*m_s_to_mph
# df_min['owtr_et_in'] = df_min.open_wtr_et_mm*mm_to_inch
# df_min.drop(columns = ['wnd_spd_m_s', 'open_wtr_et_mm'], inplace = True)
# df_min['month'] = df_min['eia_p_date'].astype(str).str[-5:-3]
# df_min['eia_plant'] = df_min['eia_p_date'].astype(str).str[0:-11]
# df_min['eia_plant'] = df_min['eia_plant'].astype('int64')

# # In[Step 7 - ]
# month_means = pd.pivot_table(df_min, index = ['eia_plant','month'], aggfunc=np.mean)
# month_means.reset_index(inplace=True)

# month_means['date'] = month_means['month'].astype(str) + '-15-' + '2015' 
# month_means['date'] = month_means['date'].astype('datetime64[ns]')
# month_means['eia_p_date'] = month_means['eia_plant'].astype(str) + '-2015-' + month_means['month']









