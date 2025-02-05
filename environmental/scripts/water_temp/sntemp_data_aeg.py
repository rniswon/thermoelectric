# -*- coding: utf-8 -*-
"""
Code adapted from code shared by Steve Markstrom (ask Steve how he wants that
                                                  to be cited)
@author: galanter, agalanter@usgs.gov
==============================================================================

Purpose of script: to link water tempoerature data from various sources
to thermoelectric power plants locations. Water temperature data is a needed 
input to the physics-based linked heat and water budgets 
(galanter et al, 2023, in press). 

==============================================================================

Water temperature sources include:
1. Stream temperature estimates at the segment scale from the Stream Temperature
Network Model [SNTemp] (Markstrom, 2012)

2. Surface temperature from Great Lakes Surface Environmental Analysis 
(ACSPO GLSEA) available at 
https://coastwatch.glerl.noaa.gov/erddap/griddap/GLSEA_ACSPO_GCS.html?sst%

3. Sea Surface Temperatures from the National Oceanic and Atmospheric Administration
available at: 
https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html

===============================================================================

Steps of the script are separated into cells that can be run individually so 
that inputs and outputs can be examined. 

Output of the script: annual comma separated files of water temperatures at 
thermoelectric plant locations using data from the 3 above sources.

===============================================================================

References:
Markstrom, S.L., 2012, P2S—Coupled simulation with the Precipitation-Runoff 
    Modeling System (PRMS) and the Stream Temperature Network (SNTemp) Models: 
    U.S. Geological Survey Open-File Report 2012–1116, 19 p.

Reynolds, Richard W., Thomas M. Smith, Chunying Liu, Dudley B. Chelton, 
    Kenneth S. Casey, Michael G. Schlax, 2007: Daily High-Resolution-Blended 
    Analyses for Sea Surface Temperature. J. Climate, 20, 5473-5496. 

=============================================================================      
"""

# In[step 0- import the needed packages]:
# geo.yml environment has these packages installed
import sys
import numpy as np
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib
import os
import pandas as pd
from mpl_toolkits.axes_grid1 import make_axes_locatable

# In[step 1- set up directories]:
directory = os.path.dirname(__file__) # where this script is located
list_dir = os.chdir(os.path.join(directory, '..','..','data')) # where the all plants list is located
# update this path when we decide how to cite segment_mapping_gf_v11.shp
seg_map_dir = 'C:/Users/galanter/OneDrive - DOI/2-GIS/TE/segment_mapping_gf_v11'
#update this path with your desired output directory 
te_water_temp_dir = 'C:/Users/galanter/DOI/GS-WMA-WBEEP - Thermoelectric/Water_Temps/'

#%% In[step 2- read in files]
plant_list = pd.read_csv('all_plants_all_years_1498.csv')
#update this path when we decide how to cite the SNTemp file
seg_tave_water_fn = "C:/Users/galanter/OneDrive - DOI/1-Projects/TEWU/subset_2000.csv"
seg_map_df = gpd.read_file(os.path.join(seg_map_dir, "segment_mapping_gf_v11.shp"))
seg_map_df = seg_map_df.astype({'nsegment_v': 'string'})
#merge the geodataframe with the plant list
plants_df_geo = seg_map_df.merge(plant_list, how = 'right', left_on = 'nsegment_v',
                                 right_on = 'nsegment_v11_new')


# In[step 3- Read the seg csv file]:
# this takes a few mins, if you want to bybass this step and just use the 
# output file that joins SNTemp data with other env data, jump to step 
fn = seg_tave_water_fn
print("reading", fn)

fp = open(fn) 

ndates = 0
line = fp.readline()
toks = line.strip().split(',')
nfeatures = len(toks) - 1

while line:
    line = fp.readline()
    ndates += 1
fp.close()

print("ndates =", ndates, "nfeatures =", nfeatures)
# print(lncnt)
sim_dates = np.zeros(ndates, dtype='datetime64[D]')
sim_vals = np.zeros((ndates, nfeatures), dtype=float)

with open(fn) as fp:
    # Skip header
#     header = fp.readline()
    
    ii = 0
    Lines = fp.readlines()
    for line in Lines:
        foo = line.strip()
        toks = foo.split(',')
        sim_dates[ii] = toks[0]
        
        for jj in range(len(toks)-1):
            sim_vals[ii,jj] = float(toks[jj+1])
            
        ii += 1
        
print(sim_vals.shape)

# In[step 4- data munging]
# drop NAS and sort so that the list is in numerical order. There are 
# 1498 plants, yet only 1338 have a prms_id
prms_list = plants_df_geo['prms_id'].dropna().astype(int).to_list()
prms_list.sort()
# filter the sntemp data for the segments we chose near a TE plant
# sim_vals is from the above step, we get all the rows (time step) and only
# the columns that we are selecting (the ones re associated with a 
# thermoelectric plant)
sntemp_data = sim_vals[:, prms_list]
# change from a numpy array to a pandas df
sntemp_df = pd.DataFrame(sntemp_data, columns = prms_list)
# add the date column and filter more fore the dates of interest
sntemp_df['date'] = sim_dates.tolist()
sntemp_df['date'] = pd.to_datetime(sntemp_df['date'], format = '%Y-%m-%d')
sntemp_df = sntemp_df.loc[(sntemp_df['date']> '2007-12-31')]
sntemp_df = sntemp_df.loc[(sntemp_df['date']< '2021-01-01')]
sntemp_df.set_index('date', inplace = True)

# In[Step 5- data clean up]
# replace any daily values that are > 35 deg C or < 0 with NA
sntemp_rev = sntemp_df.copy(deep = True)
columns = sntemp_rev.columns.to_list()
for c, col in enumerate(columns):
    sntemp_rev[col] = sntemp_rev[col].mask(sntemp_rev[col]>35, np.nan)
    sntemp_rev[col] = sntemp_rev[col].mask(sntemp_rev[col]<0, np.nan)
#%%    
# output a csv for Steve M. (remove this chunk before data release)
# looks like there are some duplicate columns, when I removed the duplicate 
# columns, there are 1099 columns (reduced from 1338)
highs_lows = sntemp_df.copy(deep = True)
highs_lows = highs_lows.loc[:,~highs_lows.columns.duplicated()].copy()

columns = highs_lows.columns.to_list()
for c, col in enumerate(columns):
    highs_lows[col] = highs_lows[col].mask(highs_lows[col]<35, np.nan)
    
highs_lows = highs_lows.dropna(how = 'all')
# dates for which temp > 35, reduces from 4749 to 3613 (meaning there are 3613
# times steps for which at least one columns value is greater than 35) 
highs_lows = highs_lows.dropna(axis = 1, how = 'all')
#drops to 335 columns, meaning only 335 segments have values > 35
highs_lows.to_csv('highs_lows.csv', na_rep = 'NA')

# In[Step 6- output monthly averages]
sntemp_rev.reset_index(inplace = True)
monthly = sntemp_rev.groupby(pd.PeriodIndex(sntemp_rev['date'], freq = 'M')).mean()
monthly.index = monthly.index.to_series().astype(str)
# monthly.reset_index(inplace = True)
monthly = monthly.T
monthly.reset_index(inplace = True)
monthly.rename(columns = {'index':'prms_id'}, inplace = True)
monthly.drop_duplicates(inplace= True)
#%% now link the plant with the sntemp data
plants = plants_df_geo[['prms_id', 'eia_id']]
plants = plants[plants['prms_id'].notna()]

monthly_df = monthly.merge(plants, how = 'right', left_on = 'prms_id', right_on= 'prms_id')
monthly_df.to_csv(os.path.join(te_water_temp_dir, 'monthly_sntemp_vals.csv'), index = False)

#%% dont need this cell, was for building the loop
# monthly_df.rename(columns = {2008-01:'WT_jan'})

# #%% non looped (for testing)
# # # monthly_df.rename(columns = {2008-01:'WT_jan'})
# # df_08 = monthly_df[['eia_id','2008-01', '2008-02', '2008-03', '2008-04', '2008-05',
# #        '2008-06', '2008-07', '2008-08', '2008-09', '2008-10','2008-11','2008-12']].copy(deep=True)

# kens_dir = 'C:/Users/galanter/DOI/GS-WMA-WBEEP - Thermoelectric/Water_Temps'


# glsea_08 = pd.read_csv(os.path.join(kens_dir,'TEGL_WT_GLSEA_2008.csv'))
# glsea_08['t_source'] = 'glsea'
# glsea_08.drop(columns=['PLANT_NAME','year'], inplace=True)
# old = glsea_08.columns.to_list()
# new = ['eia_id','WT_jan', 'WT_feb', 'WT_mar', 'WT_apr', 'WT_may', 'WT_jun', 
#        'WT_jul', 'WT_aug', 'WT_sep','WT_oct', 'WT_nov', 'WT_dec', 't_source']
# res = {old[i]: new[i] for i in range(len(old))}
# glsea_08.rename(columns = res, inplace = True)

# noaa_08 = pd.read_csv(os.path.join(kens_dir, 'TEsaline_WT_NOAA_2008.csv'))
# noaa_08.drop(columns = ['Unnamed: 0', 'eia_plantnm', 'name_of_wa', 'TE_lat',
#                         'TE_lon', 'cool_type', 'water_src', 'water_type', 
#                         'cool_tye', 'lat','lon', 'dist'], inplace = True)
# noaa_08['t_source'] = 'noaa'
# old = noaa_08.columns.to_list()
# res = {old[i]: new[i] for i in range(len(old))}
# noaa_08.rename(columns = res, inplace = True)

# df_08['t_source'] = 'sntemp'
# old = df_08.columns.to_list()
# res = {old[i]: new[i] for i in range(len(old))}
# df_08.rename(columns = res, inplace = True)

# df_2008 = pd.concat([df_08,glsea_08,noaa_08])

# df = df_2008.merge(plants_df_geo, how ='left', left_on = 'eia_id', right_on = 'eia_id')
# df = df[['eia_id', 'WT_multsrc', 't_source', 'WT_jan', 'WT_feb', 'WT_mar', 'WT_apr',
#        'WT_may', 'WT_jun', 'WT_jul', 'WT_aug', 'WT_sep', 'WT_oct', 'WT_nov',
#        'WT_dec', 'nsegment_v', 'prms_id','water_source_aeg', 'nsegment_v11_new',
#        'cmplxcoo_flg', 'coolty_1', 'coolty_2', 'coolty_3']]
#%%loop through years, join with gm data, and output annual csvs


yrs = ['08','09','10','11','12','13','14','15','16','17','18','19','20']
#yrs = ['08']
for y, yr in enumerate(yrs):
    g_name = 'TEGL_WT_GLSEA_20' + yr + '.csv'
    glsea = pd.read_csv(os.path.join(kens_dir, g_name))
    glsea['t_source'] = 'glsea'
    glsea.drop(columns=['PLANT_NAME','year'], inplace=True)
    old = glsea.columns.to_list()
    new = ['eia_id','WT_jan', 'WT_feb', 'WT_mar', 'WT_apr', 'WT_may', 'WT_jun', 
       'WT_jul', 'WT_aug', 'WT_sep','WT_oct', 'WT_nov', 'WT_dec', 't_source']
    res = {old[i]: new[i] for i in range(len(old))}
    glsea.rename(columns = res, inplace = True)
    
    noaa_name = 'TEsaline_WT_NOAA_20' + yr + '.csv'
    noaa =  pd.read_csv(os.path.join(kens_dir, noaa_name))
    noaa.drop(columns = ['Unnamed: 0', 'eia_plantnm', 'name_of_wa', 'TE_lat',
                            'TE_lon', 'cool_type', 'water_src', 'water_type', 
                            'cool_tye', 'lat','lon', 'dist'], inplace = True)
    noaa['t_source'] = 'noaa'
    old = noaa.columns.to_list()
    res = {old[i]: new[i] for i in range(len(old))}
    noaa.rename(columns = res, inplace = True)
    
    sntemp_list = ['eia_id','2008-01', '2008-02', '2008-03', '2008-04', '2008-05',
            '2008-06', '2008-07', '2008-08', '2008-09', '2008-10','2008-11','2008-12']
    new_yr = '20' + yr
    sntemp_annual = [sub.replace('2008', new_yr) for sub in sntemp_list]
    sntemp= monthly_df[sntemp_annual].copy(deep=True)
    sntemp['t_source'] = 'sntemp'
    old = sntemp.columns.to_list()
    res = {old[i]: new[i] for i in range(len(old))}
    sntemp.rename(columns = res, inplace = True)
    
    df_1 = pd.concat([sntemp,glsea,noaa])
    df_2 = df_1.merge(plants_df_geo, how ='left', left_on = 'eia_id', right_on = 'eia_id')
    df_3 = df_2[['eia_id', 'WT_multsrc', 't_source', 'WT_jan', 'WT_feb', 'WT_mar', 'WT_apr',
            'WT_may', 'WT_jun', 'WT_jul', 'WT_aug', 'WT_sep', 'WT_oct', 'WT_nov',
            'WT_dec', 'nsegment_v', 'prms_id','water_source_aeg', 'nsegment_v11_new',
            'cmplxcoo_flg', 'coolty_1', 'coolty_2', 'coolty_3']]
    wts = df_3[['WT_jan', 'WT_feb', 'WT_mar', 'WT_apr',
            'WT_may', 'WT_jun', 'WT_jul', 'WT_aug', 'WT_sep', 'WT_oct', 'WT_nov',
            'WT_dec']]
    print(yr + 'min temp = ' + str(wts.min().min()))
    print(yr + 'max temp = ' + str(wts.max().max()))
    #andys files
    andys_dir = 'C:/Users/galanter/DOI/GS-WMA-WBEEP - Thermoelectric/GridMet_data/long_to_wide'
    file = '20' + yr + '_wide.csv' 
    gm_data = pd.read_csv(os.path.join(andys_dir,file))

    output = gm_data.merge(df_3, how = 'left', left_on = 'Plant.Code', 
                           right_on = 'eia_id')
    
    # print(yr + '_cd_output_num_rows_' + str(len(gm_data)))
    # print(yr + '_wt_data_num_rows_' + str(len(df_3)))
    # print(yr + '_merged_data_num_rows_' + str(len(output)))
    # print(yr + '_diff = ' + str(len(output)-len(gm_data)))
    fname = '20'+ yr + '_fewsr_tower_input.csv'
    output.to_csv(os.path.join(kens_dir, fname), index = False)


# In[output]: dont run this
# actually just need to merge fewsr_tower_input with cd results 
# merge the wt df and the gridMET df
# flag which ones to use gridMET, which ones to use SST, GLSEA
# output the csv
# do this in a loop
# name the files with the proper naming convention

andys_dir = 'C:/Users/galanter/DOI/GS-WMA-WBEEP - Thermoelectric/GridMet_data/long_to_wide'
for y, yr in enumerate(yrs):
    file = '20' + yr + '_wide.csv' 
    gm_data = pd.read_csv(os.path.join(andys_dir,file))
    print(yr + '_numrows_' + str(len(gm_data)))
    
#gm_data = pd.read_csv(os.path.join(andys_dir,'2020_wide.csv'))

#YYYY_fewsr_tower_input.csv                          
