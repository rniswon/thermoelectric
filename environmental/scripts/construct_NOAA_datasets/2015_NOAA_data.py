#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# injest 2010 NOAA data to create a dataset similar to 2015 NOAA dataset. 
# Use monthly data from QCLCD
# get the data from the WBAN numbers needed, link to the Plant that they are krieged by


# In[1]:


# step 0- Import the needed packages

import pandas as pd
import os
import numpy as np
import matplotlib.pyplot as plt
import glob
import seaborn as sns

## look into these, should be useful for timing how long things take
#from tqdm.notebook import trange, tqdm
#from time import sleep


# In[2]:


# step 1- set up directory
Climate_data_dir = r'C:\WBEEP\Thermoelectric-master\Climate_stations'
year_dir = os.path.join(Climate_data_dir, '2015_data')
sta_DBWB = pd.read_csv(os.path.join(Climate_data_dir, '3sta_DBWB.csv'))
sta_WS = pd.read_csv(os.path.join(Climate_data_dir, '3sta_WS.csv'))
pm_clim_dat = pd.read_csv(os.path.join(Climate_data_dir, '..\TE_Harris_Diehl_2015',
                                      '2015_TE_input_data_AEG_switch.csv'))
os.chdir(year_dir)
filelist = glob.glob('*.txt')

# In[3]:

WBAN = sta_DBWB['WBAN']
WBAN_u = WBAN.unique()
# verify the process with 2015 data
# what to do about missing data? 'M'
for f, file in enumerate(filelist):
    NOAA_data = pd.read_csv(file)
    NOAA_data_vars = NOAA_data[['WBAN','AvgTemp','AvgWetBulb', 'AvgWindSpeed']]
    sel_NOAA_data = NOAA_data_vars[NOAA_data_vars['WBAN'].isin(WBAN_u)]
    sel_NOAA_data = sel_NOAA_data.replace('M', np.nan)
    sel_NOAA_data.reset_index(inplace = True)
    DB = 'DB_' + str(file[4:6])
    WB = 'WB_' + str(file[4:6])
    WS = 'WS_' + str(file[4:6])
    sel_NOAA_data[DB] = (sel_NOAA_data['AvgTemp'].astype(float) - 32)*(5/9)
    sel_NOAA_data[WB] = (sel_NOAA_data['AvgWetBulb'].astype(float) - 32)*(5/9)
    sel_NOAA_data[WS] = sel_NOAA_data['AvgWindSpeed'].astype(float)
    
    if f == 0:
        sel_NOAA_data_all = sel_NOAA_data.copy(deep = True)
    else: 
        sel_NOAA_data_all = sel_NOAA_data_all.merge(sel_NOAA_data, on = 'WBAN')

sel_NOAA_data_all = sel_NOAA_data_all.drop(columns = ['index_x', 'AvgTemp_x', 'AvgWetBulb_x', 
                                  'AvgWindSpeed_x','index_y', 'AvgTemp_y', 
                                  'AvgWetBulb_y','AvgWindSpeed_y'])
    
#%%

# Make a copy of 3sta dataframe in order to populate with the NOAA data. Drop 
# un needed cols, add a col that will be populated with the idw
new_sta_DBWB = sta_DBWB.copy(deep=True)

new_sta_DBWB.drop(columns = ['WMO_num', 'PLANT_ID_1','PLANT_NAME','STATE',
                             'Plnt_LAT','Plnt_LONG','Name', 'State','Location', 
                             'Wsta_Lat', 'Wsta_Long'], inplace =True)
new_sta_DBWB['idw'] = np.nan

new_sta_WS = sta_WS.copy(deep=True)

new_sta_WS.drop(columns = ['PLANT_NAME','STATE',
                             'LAT','LONG','Name', 'State_1','Location', 
                             'Lat_1', 'Long_1'], inplace =True)
new_sta_WS['idw'] = np.nan

# In[25]:
#get variables ready for loop to calculate inverse distance weight and monthly 
# weighted values for DB and WB dimensions. There are 3891 rows in the 3sta data, 
# becaause there are 3 WBAN stations for every TE plant so there are 1297 TE plants,
# but remeber there are only 1122 locations where I pulled gridMET data from, 
# that is because there are 175 plants co-located, so they have the same 
# climate data. I just left them in for now, 
# but that's why the numbers are different. 
plnt_ids = new_sta_DBWB['Plant_ID'] 
plnt_ids_unqe = pd.Series(plnt_ids.unique())

# get the output dataframe ready (1297 X 25)
column_names = ['Plant_ID', 'WB_01', 'WB_02', 'WB_03', 'WB_04', 'WB_05', 'WB_06', 'WB_07', 
                'WB_08', 'WB_09', 'WB_10', 'WB_11', 'WB_12','DB_01', 'DB_02', 'DB_03', 'DB_04',
                'DB_05', 'DB_06', 'DB_07', 'DB_08', 'DB_09', 'DB_10', 'DB_11', 'DB_12',
                'WS_01', 'WS_02','WS_03','WS_04','WS_05','WS_06','WS_07', 'WS_08',
                'WS_09', 'WS_10', 'WS_11', 'WS_12', 'comp_idws']

idw_clim_data = pd.DataFrame(columns = column_names)
idw_clim_data['Plant_ID'] = plnt_ids_unqe

#%%
# populate new_sta with the NOAA data
#merge on WBAN

NOAA_join = pd.merge(new_sta_DBWB, sel_NOAA_data_all, on = 'WBAN')
NOAA_join = NOAA_join.sort_values(by = ['Plant_ID'])
NOAA_join = NOAA_join.drop(columns = ['JAN_WB', 'FEB_WB',
       'MAR_WB', 'APR_WB', 'MAY_WB', 'JUN_WB', 'JUL_WB', 'AUG_WB', 'SEP_WB',
       'OCT_WB', 'NOV_WB', 'DEC_WB', 'JAN_DB', 'FEB_DB', 'MAR_DB', 'APR_DB',
       'MAY_DB', 'JUN_DB', 'JUL_DB', 'AUG_DB', 'SEP_DB', 'OCT_DB', 'NOV_DB',
       'DEC_DB '])
#re order cols
#NOAA_join = NOAA_join[['Plant_ID', 'NEAR_DIST', 'NEAR_RANK', 'WBAN','DB_01', 'WB_01', 'WS_01', 'DB_02', 'WB_02', 'WS_02', 'DB_03',
#       'WB_03', 'WS_03', 'DB_04', 'WB_04', 'WS_04', 'DB_05', 'WB_05', 'WS_05',
#       'DB_06', 'WB_06', 'WS_06', 'DB_07', 'WB_07', 'WS_07', 'DB_08', 'WB_08',
#       'WS_08', 'DB_09', 'WB_09', 'WS_09', 'DB_10', 'WB_10', 'WS_10', 'DB_11',
#       'WB_11', 'WS_11', 'DB_12', 'WB_12', 'WS_12', 'idw']]

#%%
NOAA_join = NOAA_join[['Plant_ID', 'NEAR_DIST', 'NEAR_RANK', 'WBAN', 'WB_01', 'WB_02', 'WB_03', 'WB_04', 'WB_05', 'WB_06', 'WB_07', 
                'WB_08', 'WB_09', 'WB_10', 'WB_11', 'WB_12','DB_01', 'DB_02', 'DB_03', 'DB_04',
                'DB_05', 'DB_06', 'DB_07', 'DB_08', 'DB_09', 'DB_10', 'DB_11', 'DB_12',
                'WS_01', 'WS_02','WS_03','WS_04','WS_05','WS_06','WS_07', 'WS_08',
                'WS_09', 'WS_10', 'WS_11', 'WS_12', 'idw']]


#%%
column_names = ['Plant_ID', 'WB_01', 'WB_02', 'WB_03', 'WB_04', 'WB_05', 'WB_06', 'WB_07', 
                'WB_08', 'WB_09', 'WB_10', 'WB_11', 'WB_12','DB_01', 'DB_02', 'DB_03', 'DB_04',
                'DB_05', 'DB_06', 'DB_07', 'DB_08', 'DB_09', 'DB_10', 'DB_11', 'DB_12',
                'WS_01', 'WS_02','WS_03','WS_04','WS_05','WS_06','WS_07', 'WS_08',
                'WS_09', 'WS_10', 'WS_11', 'WS_12', 'idw']
NOAA_idw = pd.DataFrame(columns = column_names)
NOAA_idw['Plant_ID'] = plnt_ids_unqe
# In[26]:
# loop to calc idw and monthly inverse distance weighted values. 
# i loop goes through new_sta_DBWB dataframe and caculates the inverse distance weight. 
# j loop goes through the columns (WB and DB for each month == 24 cols) and multiplies 
# the idw by the temp
# output is a dataframe called idw_clim_data
for i, plant in enumerate(plnt_ids_unqe):
    df = new_sta_DBWB.loc[new_sta_DBWB['Plant_ID'] == plant]
    dst = np.asarray(df.loc[:, 'NEAR_DIST'])
    ttl_dst = np.sum(dst)
    df.iloc[0,int(df.columns.get_indexer(['idw']))] = (1/(dst[0]/ttl_dst))
    df.iloc[1,int(df.columns.get_indexer(['idw']))] = (1/(dst[1]/ttl_dst))
    df.iloc[2,int(df.columns.get_indexer(['idw']))] = (1/(dst[2]/ttl_dst))
    
    df_w = new_sta_WS.loc[new_sta_WS['Plant_ID'] == plant]
    dst_w = np.asarray(df_w.loc[:, 'NEAR_DIST'])
    ttl_dst_w = np.sum(dst_w)
    df_w.iloc[0,int(df_w.columns.get_indexer(['idw']))] = (1/(dst_w[0]/ttl_dst_w))
    df_w.iloc[1,int(df_w.columns.get_indexer(['idw']))] = (1/(dst_w[1]/ttl_dst_w))
    df_w.iloc[2,int(df_w.columns.get_indexer(['idw']))] = (1/(dst_w[2]/ttl_dst_w))
    
    for j in range(4,28):
        mnth_data = np.asarray(df.iloc[:, j])
        mnth_val = (mnth_data[0]*df.iloc[0,int(df.columns.get_indexer(['idw']))] + 
                    mnth_data[1]*df.iloc[1,int(df.columns.get_indexer(['idw']))] + 
                    mnth_data[2]*df.iloc[2,int(df.columns.get_indexer(['idw']))])/(np.sum(df.loc[:,'idw']))
        idw_clim_data.iloc[i,j-3] = mnth_val
        idw_dbwb = df.loc[:,'idw']

    for j in range(4,16):
        mnth_data = np.asarray(df_w.iloc[:, j])
        mnth_val = (mnth_data[0]*df_w.iloc[0,int(df_w.columns.get_indexer(['idw']))] + 
                    mnth_data[1]*df_w.iloc[1,int(df_w.columns.get_indexer(['idw']))] + 
                    mnth_data[2]*df_w.iloc[2,int(df_w.columns.get_indexer(['idw']))])/(np.sum(df_w.loc[:,'idw']))
        idw_clim_data.iloc[i,j + 21] = mnth_val    
    
    if df.loc[:, 'idw'].all() == df_w.loc[:,'idw'].all():
        idw_clim_data.iloc[i,-1] = 'same'
    else: 
        idw_clim_data.iloc[i,-1] = 'diff'
        
#%%

# Verify that WS has the same Idw, they do, dont have to run this
same  = idw_clim_data[idw_clim_data['comp_idws'] == 'same'] 
diff =  idw_clim_data[idw_clim_data['comp_idws'] == 'diff'] 

# now get rid of that col
idw_clim_data.drop(columns = 'comp_idws')
        
#%% Now do the same for the NOAA data (don't need 2 j loops because DB, WB, and
# WS are all in the same DF). added a loop to drop nas *ask Melissa about this
for i, plant in enumerate(plnt_ids_unqe):
    df = NOAA_join.loc[NOAA_join['Plant_ID'] == plant]
    dst = np.asarray(df.loc[:, 'NEAR_DIST'])
    ttl_dst = np.sum(dst)
    df.iloc[0,int(df.columns.get_indexer(['idw']))] = (1/(dst[0]/ttl_dst))
    df.iloc[1,int(df.columns.get_indexer(['idw']))] = (1/(dst[1]/ttl_dst))
    df.iloc[2,int(df.columns.get_indexer(['idw']))] = (1/(dst[2]/ttl_dst))

    for j in range(4,40):
        mnth_data = df.iloc[:, j]
        mnth_data_df = mnth_data.dropna()
        mnth_data = np.asarray(mnth_data_df)
        if len(mnth_data) == 3:
            mnth_val = (mnth_data[0]*df.iloc[0,int(df.columns.get_indexer(['idw']))] + 
                        mnth_data[1]*df.iloc[1,int(df.columns.get_indexer(['idw']))] + 
                        mnth_data[2]*df.iloc[2,int(df.columns.get_indexer(['idw']))])/(np.sum(df.loc[:,'idw']))
            NOAA_idw.iloc[i,j-3] = mnth_val  
        else: 
            l = len(mnth_data)
            if l == 2:
                idw0 = df.loc[mnth_data_df.index[0],'idw']
                idw1 = df.loc[mnth_data_df.index[1],'idw']                
                a = mnth_data[0]*idw0
                b = mnth_data[1]*idw1
                mnth_val = (a + b)/(idw0 + idw1)
            elif l == 1:
                idw0 = df.loc[mnth_data_df.index[0],'idw']
                mnth_val = (mnth_data[0]*idw0)/idw0
                
            NOAA_idw.iloc[i,j-3] = mnth_val 
            
# issue here, make the mnth_data df with the idw values so that they match up with the mnth_data that are dropped. 
            
#%% testing na problems
a = NOAA_join[['Plant_ID','NEAR_RANK','WBAN', 'DB_12','WS_01','WS_02', 'WS_03',
               'WS_04', 'WS_05', 'WS_06', 'WS_07', 'WS_08', 'WS_09', 'WS_10', 
               'WS_11', 'WS_12']]

b =a[a['WS_01'].isna()]
test = NOAA_join[NOAA_join['Plant_ID'] == 602]
mnth_data = test.loc[:,'WS_01']
check = NOAA_idw[NOAA_idw['Plant_ID'] == 602]

# good enough
 #%% now compare all 3 data types to ensure they are the same
 
pm = pm_clim_dat['EIA_PLANT_ID']  
idw_from3sta = idw_clim_data['Plant_ID']      
        
pm_drop_dups = pm_clim_dat.drop_duplicates(subset = ['EIA_PLANT_ID'])
pm_drop_dups.reset_index(inplace = True)
pm_df = pm_drop_dups.drop(columns = ['WT_Jan', 'WT_Feb', 'WT_Mar','WT_Apr', 'WT_May', 
                  'WT_Jun', 'WT_Jul', 'WT_Aug', 'WT_Sep', 'WT_Oct', 'WT_Nov', 
                  'WT_Dec'])
pm_plants = pm_drop_dups['EIA_PLANT_ID']

#%%

idw_sel_to_comp = idw_clim_data[idw_clim_data['Plant_ID'].isin(pm_plants)]
idw_sel_to_comp.reset_index(inplace = True)
#%%
NOAA_idw_to_comp = NOAA_idw[NOAA_idw['Plant_ID'].isin(pm_plants)]
NOAA_idw_to_comp.reset_index(inplace = True)
#%%
#ok now all three dfs have 1122 plants and either 39 or 73 cols, that's ok, use .loc
diff_matr = idw_sel_to_comp.loc[:,'WB_01':'WS_12'] - NOAA_idw_to_comp.loc[:,'WB_01':'WS_12']
# this is for the NOAA data vs the input for the pm

diff_matr2 = NOAA_idw_to_comp.loc[:,'WB_01':'WS_12'] - pm_df.loc[:,'WB_01':'WS_12']
# this is for the NOAA data vs the processed pm data
#stats
mx = np.max(np.max(diff_matr))
mn = np.min(np.min(diff_matr))
avg = np.mean(np.mean(diff_matr))
med = np.median(np.median(diff_matr))

mx2 = np.max(np.max(diff_matr2))
mn2 = np.min(np.min(diff_matr2))
avg2 = np.mean(np.mean(diff_matr2))
med2 = np.median(np.median(diff_matr2))

stats = {'stat': ['mx','min','avg','med'],'prepped_data - NOAA_data': [mx,mn,avg,med],
         'NOAA_data - phys_md_data': [mx2, mn2, avg2, med2]}
stats = pd.DataFrame.from_dict(stats)
#try a heat map
#%%
      
#%%
diff_matr3 = diff_matr.copy(deep = True)
diff_matr3 = diff_matr3.apply(pd.to_numeric)
diff_matr4 = diff_matr2.copy(deep = True)
diff_matr4 = diff_matr4.apply(pd.to_numeric)
sns.heatmap(diff_matr3)
#%%
sns.heatmap(diff_matr4)


        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
# In[27]:
idw_clim_data.to_csv('2015_idw_check.csv')

