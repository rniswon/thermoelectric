# -*- coding: utf-8 -*-
"""
Created on Tue Feb 23 17:44:55 2021

@author: galanter
"""

# In[step 0- import the needed packages]:
import os
import geopandas as gpd
from shapely.geometry import Point, polygon
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

# In[step 1- set up directory]:
directory = os.path.dirname(__file__) # where this script is located
list_dir = os.chdir(os.path.join(directory, '..','..','data')) # where the all plants list is located
te_df = gpd.read_file('all_plants_all_years_1498.shp')
TEdir = r'C:\WU\Thermoelectric'
wd = os.path.join(TEdir, 'environmental', 'data','to_sciencebase','forPB')
#TE_shp = gpd.read_file(os.path.join(TEdir, '7_gis','2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
# TE_shp.drop(columns = ['NAME_OF_WA','COMID', 'COOLING_TY','GENERATION', 
#                        'WATER_SOUR', 'WATER_TYPE', 'WITHDRAWAL', 'CONSUMPTIO',
#                        'MIN_WITHDR', 'MAX_WITHDR', 'MIN_CONSUM', 'MAX_CONSUM', 
#                          'NET_GENERA','geometry', 'why_no_CID'], inplace =True)
os.chdir(wd)
#%% set up lists and dicts for loop
#yrs = ['2010','2011','2012','2013','2014','2015']
yrs = ['2000','2001','2002','2003','2004',
       '2005','2006','2007','2008','2009','2016','2017','2018','2019','2020']
params = ['db_tmp_c','wb_tmp_c','wnd_spd_mph','owtr_et_in']

stats_dict = {'param': params, 'mx':np.zeros(len(params)), 
              'mn':np.zeros(len(params)), 'med': np.zeros(len(params)),
              'mean': np.zeros(len(params)),'num_nans': np.zeros(len(params))}
stats_df = pd.DataFrame(stats_dict)
stats_df.set_index('param', inplace =True)
months = ['01','02','03','04', '05', '06', '07', '08', '09', '10', '11', '12']

#os.chdir(os.path.join(TEdir, '5_output','historical'))
#%% loop to print stats df and monthly medians pdf
for y, yr in enumerate(yrs):
    filename =  yr + '_gridMET_env_input' '.csv'
    out_dir = os.path.join(TEdir, 'environmental','analysis', yr)
    df = pd.read_csv(filename)
    # df['eia_plant'] = df['eia_p_date'].str.split('-').str[0]
    # df['eia_plant'] = pd.to_numeric(df['eia_plant'])
    # df['date'] = df['eia_plant'].str.split('-').str[1:4]
    df['month'] = df['date'].astype(str).str[-5:-3]
    
    # month_means
    
    # month_medians
    month_medians = pd.pivot_table(df, index = ['eia_plant', 'month'], 
                                   aggfunc = np.median)
    month_medians.reset_index(inplace = True)
    month_medians = pd.merge(month_medians, te_df, left_on = 'eia_plant', 
                             right_on = 'eia_id')
    geometry = [Point(xy) for xy in zip(month_medians['longitude'],
                month_medians['latitude'])]
    geometry[:3]
    crs = ('epsg:5070')
    month_medians = gpd.GeoDataFrame(month_medians, crs = crs, geometry= geometry)
    print(yr)
    
    for p, param in enumerate(params):
#        print(param)
        stats_df.loc[param,'mx'] = np.max(df[param])
        stats_df.loc[param,'mn'] = np.min(df[param])
        stats_df.loc[param,'med'] = np.median(df[param])
        stats_df.loc[param, 'mean'] = np.mean(df[param])
        a = df[param].isna()
        stats_df.loc[param, 'num_nans'] = a.all()
#        stats_df.reset_index(inplace = True)
        
        #merge df

# make like a million plots
        
        with PdfPages(os.path.join(out_dir, param + '_' + yr + '.pdf')) as pdf:
            for i, val in enumerate(months):
                monthly_df= month_medians[month_medians['month'] == val]
                fig, (ax1,ax2) = plt.subplots(nrows = 2)
                ax1.set_title(val + '_' + param)
                monthly_df.plot(ax = ax1, column = param, legend = True, 
                                   markersize = 2, cmap =  'cividis')
                #ax2.set_title(val + '_' + param)
                ax2.hist(monthly_df[param])
                pdf.savefig()
                plt.close(fig)
#    if y == 0:
#        stats_df_POR = stats_df.copy(deep = True)
##        stats_df_POR.set_index('param', inplace =True)
#        print(stats_df_POR.head())
#    else: 
#        stats_df_POR = stats_df_POR.merge(stats_df, how ='right', left_on = 'param', right_on = 'param')
    stats_df.to_csv(os.path.join(out_dir, 'stats'+ yr + '.csv'))

                
                


#%% monthly medians
        