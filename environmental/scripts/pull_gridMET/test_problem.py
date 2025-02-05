#!/usr/bin/env python
# coding: utf-8

# # Pull Gridmet data for Thermoelectric locations using nearest neighbor 
# test problem- 08/18/22
# let's try out the env 'geo.yml'

# In[step 0- import the needed packages]:

import geopandas as gpd
import pandas as pd
import os
import xarray as xr
import numpy as np
import tempfile

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
lst_of_plants = lst_of_plants[0:5]

# In[step 3- set up loops]:
# set up lists for loops- 06/16/2020-- add in precip, specific humidity, surface radiation, wind direction, 
# palmer drought index(*only every 10 days, will explore this more), alfalfa ET, vapor pressure deficit

lst_of_vars = ['rmax/rmax', 'rmin/rmin']
lst_of_dh = ['relative_humidity', 'relative_humidity']

dirPath='http://thredds.northwestknowledge.net:8080'

# In[step 5- pull from gridMET]:
# Pull the data from gridMET using two loops-  one for variables and one for plants, this takes some time. 
# Definitely could be faster/better, can work to make this more efficient, but for now here it is.
# On my computer it takes about 1 hour. 
# output csv with data is TE_plants_w_2015_daily_gridMET.csv if you dont want to run this loop

# Pull data from Northwest Knowledge Network. ds is an xarray


#loop through years, variables, plants, output a csv with variables for every
# within that year.

yr = '2022'

    
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
#%%    
# clean up- lots of repeat columns
final_df = final_df[['eia_p_date', 'rmax/rmax', 
'rmin/rmin']]

# Manipulate the dataframe and output as csv]
# calculate average rh and air temp from max and min, 
# convert avg temp from Kelvins to C
# calculate wetbulb from air temp (Stull 2011)
# calculate ET from ref_et
#rename column headers
cols = ['eia_p_date', 'rh_max', 'rh_min']
final_df.columns = cols
#add means and calculated columns
rh = final_df.loc[:,'rh_max':'rh_min']
#final_df['rh_avg'] = final_df.loc[:,'rh_max': 'rh_min'].mean(axis = 1)
final_df['rh_avg'] = rh.mean(axis = 1)
RH = final_df['rh_avg']

final_df['month'] = final_df['eia_p_date'].astype(str).str[-5:-3]
final_df['eia_plant'] = final_df['eia_p_date'].astype(str).str[0:-11]
final_df['eia_plant'] = final_df['eia_plant'].astype('int64')
monthly = pd.pivot_table(final_df, index = ['eia_plant', 'month'], aggfunc=np.mean)
monthly.reset_index(inplace=True)
monthly['date'] = monthly['month'].astype(str) + '-15-' + yr
monthly['date'] = monthly['date'].astype('datetime64[ns]')
monthly['eia_p_date'] = monthly['eia_plant'].astype(str) + '-' +  yr + '-' + monthly['month']

monthly = monthly[['eia_p_date','eia_plant','date', 'rh_max', 'rh_min']]
fname2 = yr + '_all_vars_gridMET_env_input' + '.csv'

path = os.path.join(tempfile.gettempdir(), fname2)
monthly.to_csv(path, index = False)
print('output in ' + path)
