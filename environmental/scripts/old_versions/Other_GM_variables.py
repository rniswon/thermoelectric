#!/usr/bin/env python
# coding: utf-8

# # Pull Gridmet data to look at correlation
# ## Variables to compare with physics-based model:
# - PDSI  
# - Elevation  
# 
# From https://climatedataguide.ucar.edu/climate-data/palmer-drought-severity-index-pdsi#:~:text=The%20Palmer%20Drought%20Severity%20Index,more%20extreme%20values%20are%20possible.
# 
# The Palmer Drought Severity Index (PDSI) uses readily available temperature and precipitation data to estimate relative dryness. It is a standardized index that generally spans -10 (dry) to +10 (wet). Maps of operational agencies like NOAA typically show a range of -4 to +4, but more extreme values are possible. 
# 
# ### Environment -- use 'ofp_for_te.yml' 
# Base anaconda installation, conda env update with ofp_env_upd.yml (from onhm-fetcher parser Github, added pyviz)
# 

# ## Setup

# In[1]:


# step 0- Import the needed packages

import geopandas as gpd
import xarray as xr
import contextily as ctx
import pandas as pd
import os
import numpy as np
from shapely.geometry import Point, polygon
from matplotlib.backends.backend_pdf import PdfPages


# In[2]:


# step 1- set up directory, plot the TE locations, set up a geopandas dataframe and add columns for the netcdf data
TEdir = r'C:\WBEEP\Thermoelectric-master\Climate_data_fetcher'
TE_df = gpd.read_file(os.path.join(TEdir, '..\GIS','2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
TE_df.reset_index(drop = True, inplace =True)
TE_df = TE_df[TE_df.STATE !='HI']
TE_df = TE_df[TE_df.STATE !='AK']
#get list of plants and change 'NTC/MCRD' name for loop to get data at each point
lst_of_plants = TE_df.loc[:,'PLANT_NAME']
lst_of_plants.update(pd.Series(['NTC_MCRD_Energy_Facility'], index =[737]))


# In[ ]:


# for testing purposes since loop of 1122 plants takes a while! Uncomment these if you want to go test something out

#lst_of_plants = lst_of_plants[0:5]
#lst_of_plants


# In[3]:


input_2015 = pd.read_csv(os.path.join(TEdir, '..\TE_Harris_Diehl_2015', '2015_TE_input_data_AEG.csv'))


# In[4]:


input_2015.head()


# In[5]:


conv_ft_mtr = 0.3048
input_2015['ELEVATION_MTR'] = input_2015['ELEVATION_FT']*conv_ft_mtr


# In[7]:

elev_phys_mod = input_2015[['EIA_PLANT_ID','ELEVATION_MTR', 'STATE']]
elev_phys_mod = elev_phys_mod.drop_duplicates(subset = 'EIA_PLANT_ID')
# Exclude AK and HI
elev_phys_mod = elev_phys_mod[elev_phys_mod.STATE !='AK']
elev_phys_mod = elev_phys_mod[elev_phys_mod.STATE !='HI']

# In[ ]:


elev_phys_mod.head()


# In[ ]:


len(elev_phys_mod)

#


# In[ ]:


# step 1- pull elevation from gridMET, units in meters *** or just pull in csv already ran 
#var = 'elev/elev'
dh= 'elevation'
dirPath='http://thredds.northwestknowledge.net:8080'

fileName = '/thredds/dodsC/MET/elev/metdata_elevationdata.nc'
fullfilename= dirPath+fileName
print(fullfilename)
ds = xr.open_dataset(fullfilename)
print(ds, flush =True)

lathandle=ds['lat']
lonhandle=ds['lon']
timehandle=ds['day']
datahandle = ds[dh]
#crshandle=ds['crs']
ts = datahandle.sizes
print(type(ts), flush=True)
print(ts['day'], flush=True)
dayshape = ts['day']
Lonshape = ts['lon']
Latshape = ts['lat']

for i, plant in enumerate(lst_of_plants):
    var = (ds[dh])
    var1 = var.sel(lat =TE_df.iloc[i,5] , lon = TE_df.iloc[i,6] , method = 'nearest')
    var_df = xr.DataArray.to_dataframe(var1)
    var_df = var_df.drop(['lat', 'lon'], axis = 1)   
    
    
    var_df['PLANT_NAME'] = plant 
    var_df['EIA_PLANT_ID'] = TE_df.iloc[i,0]  
    
    if i == 0:
        plants_df = var_df.copy(deep=True)
    else:
        plants_df = plants_df.append(var_df)
        
    print("plant # " + str(i) + " out of 1122 " + "plant name = " + plant)


# In[ ]:


# elev_df = pd.read_csv(os.path.join(TEdir,'elev_comp.csv'))
TE_df.head()


# In[ ]:


#merge to dataframes on EIA_PLANT_ID
# not necessary if pulling in csv
elev_df = pd.merge(plants_df, elev_phys_mod, on = 'EIA_PLANT_ID')
elev_df = pd.merge(elev_df, TE_df, left_on ='EIA_PLANT_ID', right_on = 'EIA_PLANT_')


# In[ ]:


elev_df = elev_df[['EIA_PLANT_ID', 'PLANT_NAME_x','LATITUDE', 'LONGITUDE','ELEVATION_MTR', 'elevation']]


# In[ ]:


#rename columns
cols = ['EIA_PLANT_ID', 'PLANT_NAME','LATITUDE', 'LONGITUDE', 'ELEV_phys_mod', 'ELEV_gm']
elev_df.columns = cols

elev_df['RMSE'] = ''
#%%
elev_df['DIFF'] = ''

# In[ ]:


elev_df.to_csv('elev_df.csv')


# In[ ]:


def rmse(predictions, targets):
    return np.sqrt(((predictions - targets)**2).mean())


# In[ ]:


# calculate the RMSE between gridMET elevation at TE location and phys mod elevation at TE location
for i in range(len(elev_df)):
    elev_df.loc[i, 'RMSE'] = rmse(elev_df.loc[i,'ELEV_gm'], elev_df.loc[i,'ELEV_phys_mod'])

#%%

elev_df['DIFF'] = elev_df['ELEV_gm'] - elev_df['ELEV_phys_mod']


# In[ ]:


#turn it into a pandas geodataframe for plotting
geometry = [Point(xy) for xy in zip(elev_df['LONGITUDE'],elev_df['LATITUDE'])]
geometry[:3]


# In[ ]:


crs = {'init': 'epsg:4326'}
elev_df_gd = gpd.GeoDataFrame(elev_df, crs = crs, geometry= geometry)


# In[ ]:


elev_df_gd.drop(columns = ['Unnamed: 0'], inplace=True) # also drops HI and AK plants, no gridMET data there to compare, so 1106 
#plants instead of 1122


# In[ ]:


RMSE


# In[ ]:


with PdfPages('elev_compare.pdf') as pdf:
    elev_df_gd.plot(column = 'RMSE', cmap = 'gist_rainbow')
    pdf.savefig()
    plt.clf()


# In[ ]:


# step 1- pull pdsi and elevation from gridMET
var = 'pdsi/pdsi'
dh= 'palmer_drought_severity_index'
dirPath='http://thredds.northwestknowledge.net:8080'

fileName = '/thredds/dodsC/MET/' + var + '_2015.nc'
fullfilename= dirPath+fileName
print(fullfilename)
ds = xr.open_dataset(fullfilename)
print(ds, flush =True)

lathandle=ds['lat']
lonhandle=ds['lon']
timehandle=ds['day']
datahandle = ds[dh]
crshandle=ds['crs']
ts = datahandle.sizes
print(type(ts), flush=True)
print(ts['day'], flush=True)
dayshape = ts['day']
Lonshape = ts['lon']
Latshape = ts['lat']
pdsi = (ds[dh])


# In[ ]:


def mean_in_year_month(da):
    month_cnt_2015 = (da.day.dt.year.to_index() - 2015) *12 + da.day.dt.month.to_index()
    
    return da.assign_coords(year_month = month_cnt_2015).groupby('year_month').mean()


# In[ ]:


ds_1 = mean_in_year_month(ds)

#ds_1 = mean_in_year_month(ds)


# In[ ]:


from matplotlib.backends.backend_pdf import PdfPages


# In[ ]:


ds


# In[ ]:


with PdfPages('pdsi2_pdf') as pdf:
    


# In[ ]:


month = ['jan','feb','mar','apr', 'may', 'jun', 'jul', 'aug', 'sept', 'oct', 'nov', 'dec']
#month = 'jan'
for m in range(len(month)):
    if m < 8:
        d_s = '2015-0' + str(m+1) +'-01'
        d_e = '2015-0' + str(m+1) + '-20'
    else: 
        d_s = '2015-' + str(m+1) +'-01'
        d_e = '2015-' + str(m+1) + '-20'        

    pdsi_month = ds.palmer_drought_severity_index.loc[d_s:d_e]
    
    
    with PdfPages('pdsi.pdf') as pdf:
        pdsi_month.mean('day').plot()
        pdf.savefig()
        plt.close()
#jan  = ds.palmer_drought_severity_index.loc['2015-01-01':'2015-01-20']


# In[ ]:


val


# In[ ]:


feb = ds.palmer_drought_severity_index.loc['2015-02-01':'2015-02-20']


# In[ ]:


feb.mean('day').plot()


# In[ ]:





# In[ ]:


#is the elevation of the grid cell much different than the elevation of the TE plant location?


# In[ ]:


#get list of plants and change 'NTC/MCRD' name for loop to get data at each point
lst_of_plants = TE_df.loc[:,'PLANT_NAME']
lst_of_plants.update(pd.Series(['NTC_MCRD_Energy_Facility'], index =[737]))


# In[ ]:


# for testing purposes since loop of 1122 plants takes a while! Uncomment these if you want to go test something out

lst_of_plants = lst_of_plants[0:5]
lst_of_plants


# In[ ]:


# set up lists for loops- 06/16/2020-- add in precip, specific humidity, surface radiation, wind direction, 
# palmer drought index(*only every 10 days, will explore this more), alfalfa ET, vapor pressure deficit

lst_of_vars = ['rmax/rmax', 'rmin/rmin', 'tmmn/tmmn','tmmx/tmmx', 'vs/vs', 'pet/pet', 'pr/pr', 'sph/sph', 'srad/srad',
              'th/th', 'etr/etr', 'vpd/vpd']
lst_of_dh = ['relative_humidity', 'relative_humidity', 'air_temperature', 'air_temperature', 
             'wind_speed', 'potential_evapotranspiration', 'precipitation_amount', 'specific_humidity',
            'surface_downwelling_shortwave_flux_in_air', 'wind_from_direction', 'potential_evapotranspiration', 
            'mean_vapor_pressure_deficit']

dirPath='http://thredds.northwestknowledge.net:8080'


# In[ ]:


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
        var1 = var.sel(lat =TE_df.iloc[i,5] , lon = TE_df.iloc[i,6] , method = 'nearest')
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


# In[ ]:


plants_df.head()


# In[ ]:


# pull the pdsi data as a separate step since data is only avaialble every 10 days
lst_of_vars = ['pdsi/pdsi']
lst_of_dh = ['palmer_drought_severity_index']
dirPath='http://thredds.northwestknowledge.net:8080'

fileName = '/thredds/dodsC/MET/' + lst_of_vars + '_2015.nc'
ds = xr.open_dataset(fullfilename)
ds['']

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
    
    
    var_df['PLANT_NAME'] = plant 
    var_df['EIA_PLANT_'] = TE_df.iloc[i,0]  
    
    if i == 0:
        pdsi_df = var_df.copy(deep=True)
    else:
        pdsi_df = pdsi_df.append(var_df)
        
    print("plant # " + str(i) + " out of 1122 " + "plant name = " + plant)


# In[ ]:


pdsi_df.head()


# In[ ]:


pdsi_df.reset_index(inplace=True)
pdsi_df['EIA_P_DATE'] = pdsi_df['EIA_PLANT_'].astype(str) + "_" + pdsi_df['day'].astype(str)


# In[ ]:


#output pdsi data
pdsi_df.to_csv('pdsi_at_TE.csv')


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

# In[ ]:


# verify WB equation from Stull (2011)

T = 20
RH = 50


a = T*np.arctan(0.151977*np.power((RH + 8.313659),(1/2)))
b = np.arctan(T + RH) - np.arctan(RH - 1.676331)
c = 0.00391838*(np.power(RH,(3/2)))*np.arctan(0.023101*RH) - 4.686035

Tw = a + b + c
Tw


# In[ ]:


#Step 3 - Manipulate the dataframe
# calculate average rh and air temp from max and min, convert avg temp from Kelvins to C
# calculate wetbulb from air temp (Stull 2011)
# calculate ET from ref_et

#add a column for EIA_PLANT_DATE
plants_df.reset_index(inplace=True)
plants_df['EIA_P_DATE'] = plants_df['EIA_PLANT_'].astype(str) + "_" + plants_df['day'].astype(str)

#rename column headers
cols = ['day', 'rh_max_%', 'rh_min_%', 'air_tmn_K', 'air_tmx_K', 'wnd_spd_m_s','ref_et_gr_mm', 'precip_mm', 'sp_h_kg/kg', 
        'sur_rad_Wm2','wnd_dir_dg_clk_frm_N','ref_et_alf_mm','vpdef_kPa','PLANT_NAME', 'EIA_PLANT_', 'EIA_P_DATE']
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
                       'wb_tmp_C', 'rh_max_%', 'rh_min_%', 'sp_h_kg/kg','air_tmn_K', 'air_tmx_K', 'precip_mm','wnd_spd_m_s',
                       'wnd_dir_dg_clk_frm_N','ref_et_gr_mm','ref_et_alf_mm','sur_rad_Wm2','vpdef_kPa']]

#create a minimalist df with only variables for comparison with physics-based model
plants_df_minimalist = plants_df.drop(columns = ['rh_max_%', 'rh_min_%', 'rh_avg_%','air_tmn_K', 'air_tmx_K','sp_h_kg/kg','precip_mm',
                                                 'wnd_dir_dg_clk_frm_N','ref_et_gr_mm','ref_et_alf_mm','sur_rad_Wm2',
                                                 'vpdef_kPa'])

#output as a csv 
plants_df_minimalist.to_csv('TE_plants_w_2015_daily_gridMET.csv')
plants_df.to_csv('TE_plants_w_2015_daily_gridMET_including_processing_variables.csv')


# In[ ]:




