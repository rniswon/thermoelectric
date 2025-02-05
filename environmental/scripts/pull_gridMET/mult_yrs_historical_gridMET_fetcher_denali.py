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
# ### Environment -- use 'geo.yml' 
# open anaconda prompt
# navigate to location of the geo.yml file (Thermoelectric>environmental)
# type in 'conda env create -f geo.yml'
# conda activate geo 
# 

# ## Setup

# Modifications- manually modified 2015_TE_Model_Estimates_lat.long_COMIDS.shp 
# by changing NTC/MCRD Energy Facility to NTC_MCRD Energy Facility

# In[step 0]:


# step 0- Import the needed packages, 
# pre installed"

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

# In[step 1]:
# set up directory, plot the TE locations, set up a geopandas dataframe and add
# columns for the netcdf data
# points working directory to the location of this script
os.chdir(os.path.dirname(__file__))
print('current working directory:',os.getcwd())

te_dir = os.chdir(os.path.join('..','..','..'))
print('TE working directory:',os.getcwd())
#%%

te_df = gpd.read_file(os.path.join(te_dir, '7_gis',
                                   '2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
# remove AK and HI, gridMET not avaialable outside of CONUS
TE_df_CONUS = TE_df.drop(TE_df.loc[TE_df['STATE'] == 'AK'].index)
TE_df_CONUS = TE_df_CONUS.drop(TE_df.loc[TE_df['STATE'] == 'HI'].index)
lst_of_plants = TE_df_CONUS.loc[:,'PLANT_NAME']
# In[test step]:
# for testing purposes since loop of 1122 plants takes a while! 
# Uncomment these if you want to go test something out
#lst_of_plants = lst_of_plants[0:5]

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
# verify WB equation from Stull (2011) for relative humidity, answer should be 13.7 dec C

T = 20
RH = 50


a = T*np.arctan(0.151977*np.power((RH + 8.313659),(1/2)))
b = np.arctan(T + RH) - np.arctan(RH - 1.676331)
c = 0.00391838*(np.power(RH,(3/2)))*np.arctan(0.023101*RH) - 4.686035

Tw = a + b + c
Tw
# In[step 3]:
# Pull the data from gridMET using two loops-  one for variables and one for plants, this takes some time. 
# Definitely could be faster/better, can work to make this more efficient, but for now here it is.
# On my computer it takes about 1 hour. 
# output csv with data is TE_plants_w_2015_daily_gridMET.csv if you dont want to run this loop

# Pull data from Northwest Knowledge Network. ds is an xarray


#loop through years, variables, plants, output a csv with variables for every
# within that year.
#yrs = ['2010','2015']
yrs = ['1996', '1997', '1998','1999','2000','2001','2002','2003','2004',
       '2005','2006','2007','2008','2009', '2016','2017','2018','2019']
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
            var1 = var.sel(lat =TE_df_CONUS.iloc[j,5] , lon = TE_df_CONUS.iloc[j,6] , 
                           method = 'nearest')
            var_df1 = xr.DataArray.to_dataframe(var1)
            var_df1 = var_df1.drop(['lat', 'lon'], axis = 1)
            var_df1['PLANT_NAME'] = plant 
            var_df1['EIA_PLANT_'] = TE_df_CONUS.iloc[j,0] 
            if j == 0:
                plants_df = var_df1.copy(deep=True)
            else:
                plants_df = plants_df.append(var_df1)
    
        # create a new column, EIA_P_DATE combining the plant and the date.  
        # come back to this and comment what and why each line
        plants_df.reset_index(inplace=True)
        plants_df['EIA_P_DATE'] = plants_df['EIA_PLANT_'].astype(str) + "-" + plants_df['day'].astype(str)
        plants_df.rename({lst_of_dh[i]: val}, axis =1, inplace = True)
    
        if i == 0:
            final_df = plants_df.copy(deep=True)
        else:        
            final_df = final_df.merge(plants_df, left_on = 'EIA_P_DATE', right_on = 
                                      'EIA_P_DATE')
        print("var " + str(i) + " out of 11 " + "variables = " + val)
        
    # clean up- lots of repeat columns
    final_df = final_df[['EIA_P_DATE', 'rmax/rmax', 
    'rmin/rmin', 'tmmn/tmmn','tmmx/tmmx', 'vs/vs', 'pet/pet', 'pr/pr', 
    'sph/sph', 'srad/srad','th/th', 'etr/etr', 'vpd/vpd']]
    
    # Manipulate the dataframe and output as csv]
    # calculate average rh and air temp from max and min, 
    # convert avg temp from Kelvins to C
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
    fname = 'gridMET_minimalist_at_TE_locations_' + yr + '.csv'
    fname2 = 'gridMET_at_TE_locations_' + yr + '.csv'
    final_df_minimalist.to_csv(os.path.join(TEdir,'5_output', 'historical', fname), 
                       index = False)
    final_df.to_csv(os.path.join(TEdir,'5_output','historical', fname2), index = False)

# In[Step 5]:
#Just checking
final_df['YEAR'] = final_df['EIA_P_DATE'].astype(str).str[-10:-6]
final_df['MONTH'] = final_df['EIA_P_DATE'].astype(str).str[-5:-3]
final_df['EIA_PLANT_'] = final_df['EIA_P_DATE'].astype(str).str[0:-11]
final_df['EIA_PLANT_'] = final_df['EIA_PLANT_'].astype('int64')

#%%
month_means = pd.pivot_table(final_df, index = ['EIA_PLANT_','MONTH'], aggfunc=np.mean)
month_medians = pd.pivot_table(final_df, index = ['EIA_PLANT_','MONTH'], aggfunc=np.median)
month_means.reset_index(inplace=True)
month_medians.reset_index(inplace=True)

#%%
mean_plot = pd.merge(month_means, TE_df, on = 'EIA_PLANT_')
mean_plot.drop(columns = ['NAME_OF_WA','COMID', 'COOLING_TY','GENERATION', 'WATER_SOUR', 'WATER_TYPE', 
                         'WITHDRAWAL', 'CONSUMPTIO','MIN_WITHDR', 'MAX_WITHDR', 'MIN_CONSUM', 'MAX_CONSUM', 
                         'NET_GENERA','geometry'], inplace =True)

geometry = [Point(xy) for xy in zip(mean_plot['LONGITUDE'],mean_plot['LATITUDE'])]
geometry[:3]

mean_plot.drop(columns = ['why_no_CID'], inplace = True)
crs = {'init': 'epsg:4326'}
mean_df = gpd.GeoDataFrame(mean_plot, crs = crs, geometry= geometry)

np.linspace(1,12,12)
months = ['01','02','03','04', '05', '06', '07', '08', '09', '10', '11', '12']

var_dict = {'month': months,'DB': months, 'WB': months, 'WS': months, 'EV': months}
var_df = pd.DataFrame(data = var_dict)


#%%
params = ['DB']
units = ['deg_C']

with PdfPages('DB' + '.pdf') as pdf:
    for i, val in enumerate(months):
        monthly_df = mean_df[mean_df['MONTH'] == val]
        fig, (ax1,ax2) = plt.subplots(nrows = 2, figsize = (8,8))
        col_1 = params
        col_2 = params
        ax1.set_title(val + '_' + params)

        monthly_df.plot(ax = ax1, column = col_1, legend = True, 
                        cmap = 'gist_rainbow')
        ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
        
        ax2.set_title(val + params)
        ax2.hist(monthly_df['air_tmp_C_avg'])
        pdf.savefig()
        plt.clf()









