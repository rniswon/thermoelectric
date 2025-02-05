# -*- coding: utf-8 -*-
"""
Created on Wed Nov  2 07:59:58 2022

@author: galanter
"""

# In[step 0- import the needed packages]:
import sys
import numpy as np
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib
# from pyPRMS.ParameterFile import ParameterFile
import os
# from PIL import Image
# import glob
import pandas as pd
from mpl_toolkits.axes_grid1 import make_axes_locatable

# In[bring in files]
path = 'C:/Users/galanter/DOI/GS-WMA-WBEEP - Thermoelectric/compiledConsumption_Withdrawal'
cons_ann = pd.read_csv(os.path.join(path, 'compile_Annual_Consump_EST.csv' ))
wd_ann = pd.read_csv(os.path.join(path, 'compile_Annual_WD_EST.csv' ))
shp_file_path = 'C:/WU/Thermoelectric/environmental/data'
plants = gpd.read_file(os.path.join(shp_file_path, 'all_plants_all_years_1498.shp' ))
#huc_path = 'C:/Users/galanter/OneDrive - DOI/2-GIS/shapefiles/'
#hucs = gpd.read_file(os.path.join(huc_path, 'wbdhuc12_1026.shp'))
os.chdir('C:/Users/galanter/OneDrive - DOI/2-GIS/TE')
# In[loop through years, plot the WD and C]
w = wd_ann[(wd_ann['YEAR'] == 2020)]
c = cons_ann[(cons_ann['YEAR'] == 2020)]
#%%
w_c = w.merge(c, how = 'left', on = 'Plant.Code')
w_c.drop(columns = ['YEAR_y','Model_Min_WD', 'Model_Max_WD','Model_Min_Consump',
'Model_Max_Consump'], inplace = True)
w_c.rename(columns ={'Plant.Code':'eia_id','YEAR_x': 'year','Model_Med_WD': 'w_med',
                     'Model_Med_Consump': 'c_med'}, inplace=True)
w_c_geo = plants.merge(w_c, how = 'right', on ='eia_id')

w_c.hist('w_med')
w_c.hist('c_med')



#%%
on_th_r = w_c_geo[(w_c_geo['cmplxcoo_f'] == 0)& 
                (w_c_geo['coo_ty_1'] == 'river_once_through_fresh')] 
                

on_th_l = w_c_geo[(w_c_geo['cmplxcoo_f'] == 0)&
                (w_c_geo['coo_ty_1'] == 'lake_once_through_fresh')]

on_th = 
tower = w_c_geo[(w_c_geo['cmplxcoo_f'] == 0)&
                (w_c_geo['coo_ty_1'] == 'tower_recirculating')]
pond = w_c_geo[(w_c_geo['cmplxcoo_f'] == 0)&
               (w_c_geo['coo_ty_1'] == 'pond_recirculating')]
saline = w_c_geo[(w_c_geo['cmplxcoo_f'] == 0)&
                 (w_c_geo['coo_ty_1'] == 'saline_once_through')]
comp = w_c_geo[(w_c_geo['cmplxcoo_f'] == 1)]

#w_c_geo.to_file('2020.shp')
#%%
on_th_r.to_file('2020_river.shp')
on_th_l.to_file('2020_lake.shp')
tower.to_file('2020_tower.shp')
pond.to_file('2020_pond.shp')
saline.to_file('2020_saline.shp')
comp.to_file('2020_complex.shp')