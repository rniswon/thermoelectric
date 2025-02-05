# -*- coding: utf-8 -*-
"""
Created on Wed Feb 10 16:54:22 2021

@author: galanter
"""

import os
import pandas as pd
#import geopandas as gpd
import numpy as np
import matplotlib.pyplot as plt
import contextily as ctx
from matplotlib.backends.backend_pdf import PdfPages
#%%
wd = r'C:\WBEEP\Thermoelectric-master'
os.chdir(wd)
#%%

tower_cons = pd.read_csv(os.path.join(wd, '2_physical_models', 'output', 
                                      'Tower_model_consumption_out.csv'))
tower_clim = pd.read_csv(os.path.join(wd, '2_physical_models', 'input', 
                                      'towers', 'Tower_input.csv'))

tower_clim_gm = tower_clim.copy(deep=True)


gm_clim = pd.read_csv(os.path.join(wd, '5_output', 'historical',
                                   'gridMET_minimalist_at_TE_locations_2015.csv'))

TE_shp = gpd.read_file(os.path.join(wd, '7_gis',
                                   '2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
#%%
#Step 1: get the gridMET data in the physical model format 

# calc the monthly means
gm_clim['EIA_PLANT_'] = gm_clim['EIA_P_DATE'].str.split('-').str[0]
gm_clim['EIA_PLANT_'] = pd.to_numeric(gm_clim['EIA_PLANT_'])
gm_clim['date'] = gm_clim['EIA_P_DATE'].str.split('-').str[1:4]
gm_clim['month'] = gm_clim['date'].str[1]
gm_clim['year'] = gm_clim['date'].str[0]
gm_clim['day'] = gm_clim['date'].str[2]
gm_clim['date'] = gm_clim['year'] + '-' + gm_clim['month']+ '-' + gm_clim['day']
gm_clim['date'] = pd.to_datetime(gm_clim['date'])

gm_month_mean = pd.pivot_table(gm_clim, index = ['EIA_PLANT_','month'], 
                               aggfunc=np.mean)
gm_month_mean.reset_index(inplace=True)

#%%
# convert units for WS
conv_mph = 60*60/1609.34
gm_month_mean['wnd_spd_mph'] = gm_month_mean['wnd_spd_m_s']*conv_mph

#%%
#spread it out to the format for PM
plants = tower_clim['Plant.Code'][0:739]
for i, EIA in enumerate(plants):
    temp = gm_month_mean[gm_month_mean['EIA_PLANT_'] == EIA]
    db = np.array(temp['air_tmp_C_avg'])
    wb = np.array(temp['wb_tmp_C'])
    ws = np.array(temp['wnd_spd_mph'])
    # db = DB.T
    tower_clim_gm.loc[i,'Jan.1':'Dec.1'] = db
    tower_clim_gm.loc[i,'Jan.2':'Dec.2'] = wb
    tower_clim_gm.loc[i,'Jan.4': 'Dec.4'] = ws
#%%   
#Step3: perturb water temps
    
# join df with lat longs for plotting
mean_plot = pd.merge(tower_clim_gm, TE_shp, left_on = 'Plant.Code' , right_on = 'EIA_PLANT_')
wt_mean_plt = mean_plot[['Plant.Code', 'Jan.3', 'Feb.3', 'Mar.3', 'Apr.3', 
                         'May.3', 'Jun.3', 'Jul.3', 'Aug.3', 'Sep.3','Oct.3', 
                         'Nov.3','Dec.3','geometry']]
crs = {'init': 'epsg:4326'}
geometry = wt_mean_plt['geometry']
wt_mean_plt = gpd.GeoDataFrame(wt_mean_plt, crs = crs, geometry= wt_mean_plt['geometry'])
wt_plus_10 = wt_mean_plt.copy(deep = True)
wt_minus_10 = wt_mean_plt.copy(deep = True)
wt_plus_10.loc[:,'Jan.3': 'Dec.3'] = wt_plus_10.loc[:,'Jan.3': 'Dec.3'] + 10
wt_minus_10.loc[:,'Jan.3': 'Dec.3'] = wt_minus_10.loc[:,'Jan.3': 'Dec.3'] - 10
months = wt_mean_plt.columns[1:13]
months = list(months)

#%%
with PdfPages('wt_2015.pdf') as pdf:
    
    for i, val in enumerate(months):
            fig,(ax1,ax2) = plt.subplots(nrows = 2, figsize = (8,8))
            ax1.set_title('Water_temp_degC_'+ val)           
            wt_mean_plt.plot(ax = ax1, column = val, legend = True, cmap = 'coolwarm')
            ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
            
            ax2.set_title('Water_temp_degC_'+ val)
            ax2.hist(wt_mean_plt[val])
            pdf.savefig()
            plt.clf()    


#%%

with PdfPages('wt_plus_10_2015.pdf') as pdf:
    
    for i, val in enumerate(months):
            fig,(ax1,ax2) = plt.subplots(nrows = 2, figsize = (8,8))
            ax1.set_title('Water_temp_degC_'+ val)           
            wt_plus_10.plot(ax = ax1, column = val, legend = True, cmap = 'coolwarm')
            ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
            
            ax2.set_title('Water_temp_degC_'+ val)
            ax2.hist(wt_plus_10[val])
            pdf.savefig()
            plt.clf()    

#%%
with PdfPages('wt_minus_10_2015.pdf') as pdf:
    
    for i, val in enumerate(months):
            fig,(ax1,ax2) = plt.subplots(nrows = 2, figsize = (8,8))
            ax1.set_title('Water_temp_degC_'+ val)           
            wt_minus_10.plot(ax = ax1, column = val, legend = True, cmap = 'coolwarm')
            ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
            
            ax2.set_title('Water_temp_degC_'+ val)
            ax2.hist(wt_minus_10[val])
            pdf.savefig()
            plt.clf() 
















