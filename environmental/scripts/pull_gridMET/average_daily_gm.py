# -*- coding: utf-8 -*-
"""
Created on Wed Jun 22 13:26:31 2022

@author: galanter
"""

#%% step 0- Import the needed packages
import os
import pandas as pd
import numpy as np
#%% step 1- set up the directory

wd = r'C:\Users\galanter\OneDrive - DOI\1-Projects\WBEEP\github_backup_2022\Thermoelectric-master'
os.chdir(os.path.join(wd, 'environmental'))

#%% step 2- pull in gridmet and average/median

f = pd.read_csv(os.path.join('data','gridMET_minimalist_at_TE_locations_2015.csv'), 
                             dtype ={'eia_p_date': 'str'})
#%%
# convert meters/sec to mph for wind speed
m_s_to_mph = 60*60/1609.344
f['wnd_spd_mph'] = f.wnd_spd_m_s*m_s_to_mph
f.drop(columns = ['wnd_spd_m_s','open_wtr_et_mm'], inplace = True)
#%%
f['month'] = f['eia_p_date'].astype(str).str[-5:-3]
f['eia_plant'] = f['eia_p_date'].astype(str).str[0:-11]
f['eia_plant'] = f['eia_plant'].astype('int64')
#%%
month_means = pd.pivot_table(f, index = ['eia_plant','month'], aggfunc=np.mean)
#%%                             
month_means.reset_index(inplace=True)

month_means['date'] = month_means['month'].astype(str) + '-15-' + '2015' 
month_means['date'] = month_means['date'].astype('datetime64[ns]')
month_means['eia_p_date'] = month_means['eia_plant'].astype(str) + '-2015-' + month_means['month']
#%%
#re order columns
month_means.rename(columns ={'air_tmp_C_avg':'db_tmp_c','wb_tmp_C':'wb_tmp_c'}, 
                   inplace = True)
month_means = month_means[['eia_p_date','eia_plant','date','db_tmp_c','wb_tmp_c','wnd_spd_mph']]

month_means.to_csv(os.path.join('data','gridMET_env_input_2015.csv'), index = False)
#%% try out medians?
#month_medians = pd.pivot_table(f, index = ['eia_plant','month'], aggfunc=np.median)






# monthly averages


