# -*- coding: utf-8 -*-
"""
Created on Wed Nov 17 15:18:16 2021

@author: galanter
"""
# AEG
# 11/17/21
# compare monthly WD and CU estimates (published to fewsr)


#%%
# import packages
import os
import pandas as pd
import pandas as pd
import os
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt

#%%
# where
wd = r'C:\Users\galanter\OneDrive - DOI\1-Projects\WBEEP\TE\fewsr111721'
os.chdir(wd)

#%%
# import files as df

pub = pd.read_excel('2015_monthly_TE_WD_CU_AEG.xlsx', 
                      sheet_name='2015_Monthly_WD_CU')

fewsr = pd.read_excel('2015_monthly_TE_WD_CU_AEG.xlsx', 
                      sheet_name='fewsr_ran_11_17_21_dr')

#%%
# filter to just look at the simple plants
# simple_fewsr = fewsr.loc[fewsr['flag'] !='COMPLEX'] 
simple_fewsr = fewsr.loc[(fewsr.flag =='ONCE-THROUGH FRESH') | 
                         (fewsr.flag =='RECIRCULATING POND')] 
simple_fewsr.reset_index(inplace=True)


simple_pub = pub.loc[(pub.Cooling_type == 'ONCE-THROUGH FRESH') | 
                     (pub.Cooling_type =='RECIRCULATING POND')]
simple_pub.reset_index(inplace=True)


diff = simple_pub.copy(deep = True)


#%%
# compare 

cols = simple_pub.columns
cols_l = cols[9:34]
for m, val in enumerate(cols_l):
    diff[val] = simple_pub[val]- simple_fewsr[val]
    
# check a few just to be sure
a= simple_pub.loc[simple_pub['Plant_ID']== 7] - simple_fewsr.loc[simple_fewsr[
    'Plant_ID'] == 7]

# max min mean median
r = ['mean', 'median', 'max', 'min']
stats = pd.DataFrame(data=None, index= cols_l, columns = r)
for m, val in enumerate(cols_l):
    stats.loc[val,'mean'] = diff[val].mean()
    stats.loc[val,'median'] = diff[val].median()
    stats.loc[val,'max'] = diff[val].max()   
    stats.loc[val,'min'] = diff[val].min()   
    
#%%
#export for easier viewing
diff.to_csv('diffs.csv')
stats.to_csv('stats.csv')
