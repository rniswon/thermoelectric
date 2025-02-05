#!/usr/bin/env python
# coding: utf-8

# # Compare gridMET data and modeled input
# 
# ### What this notebook does: 
# - Compare the 2015 gridMET data pulled at TE locations (using TE_fetcher_parser_nearest_neighbor_Daily_gridMET_data) to 2015 input data from Harris and Diehl (2019)
# 
# ### Why the comparison/ big picture goal: 
# - Develop bias correction for gridMET data. Since 2015 input data from Harris and Diehl (2019) involved a lot of QA/QC and used climate stations, we will capitalize on that knowledge to better understand where the gridMET data might have issues. 
# 
# ### Environment -- use 'ofp_env_upd2.yml' 
# ### will check more into contextily, maybe will update ofp_for_te, but for now kept separate

# In[1]:


# step 0- import the needed packages

import geopandas as gpd
import contextily as ctx 
import pandas as pd
import os
import numpy as np
from shapely.geometry import Point, polygon
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages


# In[2]:


# step 1- bring in df with lat longs of TE plants, gridMET data, 2015 model input data
cdir = r'C:\WBEEP\Thermoelectric-master'
TE_shp = gpd.read_file(os.path.join(cdir, '7_gis','2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
gm_2010 = pd.read_csv(os.path.join(cdir, '5_output','historical', 'gridMET_minimalist_at_TE_locations_2010.csv'))
gm_2015 = pd.read_csv(os.path.join(cdir, '5_output','historical', 'gridMET_minimalist_at_TE_locations_2015.csv'))
pm_2010 = pd.read_csv(os.path.join(cdir, '2_physical_models', 'input', 
                                   'climate_stations','2010_data', '2010_processed_NOAA_data.csv'))
pm_2015 = pd.read_csv(os.path.join(cdir, '2_physical_models', 'input', 'input_data_2015',
                                   'te_Harris_Diehl_2015', '2015_TE_input_data_AEG_switch.csv'))
#elev_df = pd.read_csv(os.path.join(cdir, 'Climate_data_fetcher', 'elev_df.csv'))
# Combined longterm pond net evap2.xlsx with 2015 input data


# In[3]:


# step 2- alter format of input_2015 for easier comparison to gm_2010

# remove duplicate plant EIAs and names, the duplicates are for different system types at the same location
# but not relevant for comparing climate data.Remove columns relating to CD and WT- don't have those variables to compare to
pm_2015.drop_duplicates(subset = 'EIA_PLANT_ID', inplace=True)
pm_2015.drop(columns = ['COUNTY', 'STATE', 'NAME_OF_WATER_SOURCE','COOLING_TYPE', 'MODEL_TYPE', 'PERCENT_CD_ALLOCATION', 
                        'POND_AREA','CD_Jan', 'CD_Feb', 'CD_Mar', 'CD_Apr', 'CD_May', 'CD_Jun', 'CD_Jul', 'CD_Aug',
                       'CD_Sep', 'CD_Oct', 'CD_Nov', 'CD_Dec','WB_01', 'WB_02', 'WB_03',
                       'WB_04', 'WB_05', 'WB_06', 'WB_07', 'WB_08', 'WB_09', 'WB_10', 'WB_11',
                       'WB_12', 'DB_01', 'DB_02', 'DB_03', 'DB_04', 'DB_05', 'DB_06', 'DB_07',
                       'DB_08', 'DB_09', 'DB_10', 'DB_11', 'DB_12', 'WS_01', 'WS_02', 'WS_03',
                       'WS_04', 'WS_05', 'WS_06', 'WS_07', 'WS_08', 'WS_09', 'WS_10', 'WS_11',
                       'WS_12', 'WT_Jan', 'WT_Feb', 'WT_Mar', 'WT_Apr', 'WT_May', 'WT_Jun', 'WT_Jul',
                       'WT_Aug', 'WT_Sep', 'WT_Oct', 'WT_Nov', 'WT_Dec','Map_evap_in_Ap_Oct',
                       'Map_evap_in_2015'], inplace=True)

pm_2010.drop(columns = ['idw'], inplace = True)
pm_2010 = pm_2010.merge(pm_2015,how = 'left', left_on ='Plant_ID', right_on = 'EIA_PLANT_ID')
# 2.1 make rows into columns for input_2015 data
plants = pm_2010['Plant_ID']
cols = ['DB','WB','WS','EV']
#%%
#make the PM data structure similar to GM data structure
for i, EIA in enumerate(plants):
    temp = pm_2010[pm_2010['Plant_ID'] == EIA]
    DB = temp.loc[:, 'DB_01':'DB_12']
    WB = temp.loc[:, 'WB_01':'WB_12']
    WS = temp.loc[:, 'WS_01':'WS_12']
    EV = temp.loc[:, 'EV_01':'EV_12']
    variables = [DB,WB,WS, EV]
    
    for j, var in enumerate(variables):
        df = var.T.reset_index()        
        df['index'] = df['index'].str[-2:]
        df.columns = ['month', cols[j]]
        if j == 0:
            df1 = df.copy(deep=True)
        else: 
            df1 = pd.merge(df1, df, on = 'month')
    
    df1['EIA_PLANT_'] =  EIA
    
    if i == 0:
        df2 = df1.copy(deep=True)
    else:
        df2 = df2.append(df1)


# In[7]:


# step 2.2 a little more data manipulation, ordering of columns, unit conv
conv_mm=25.4 #inches to mm
conv_ms = 1/60/60*1609.34 # mph to meters per second
#reorder columns
pm_2010 =df2.copy(deep=True)
pm_2010 = pm_2010[['EIA_PLANT_','month','DB', 'WB', 'WS', 'EV']]
#conv Evap from inches to mm
pm_2010['EV'] = pm_2010['EV']*conv_mm
#conv WS from mph to m/s
pm_2010['WS'] = pm_2010['WS']*conv_ms

#rename cols
cols = ['EIA_PLANT_','month','DB_C', 'WB_C', 'WS_ms', 'EV_mm']
pm_2010.columns = cols


# In[12]:


# Step 3- calculate monthly means and medians from gridMET data
#gm_2015['year'] = gm_2015['day'].astype(str).str[0:4]
#gm_2015['month'] = gm_2015['day'].astype(str).str[5:7]


gm_2015['EIA_PLANT_'] = gm_2015['EIA_P_DATE'].str.split('-').str[0]
gm_2015['EIA_PLANT_'] = pd.to_numeric(gm_2015['EIA_PLANT_'])
gm_2015['date'] = gm_2015['EIA_P_DATE'].str.split('-').str[1:4]
gm_2015['month'] = gm_2015['date'].str[1]
gm_2015['year'] = gm_2015['date'].str[0]
gm_2015['day'] = gm_2015['date'].str[2]
gm_2015['date'] = gm_2015['year'] + '-' + gm_2015['month']+ '-' + gm_2015['day']
gm_2015['date'] = pd.to_datetime(gm_2015['date'])

gm_2010['EIA_PLANT_'] = gm_2010['EIA_P_DATE'].str.split('-').str[0]
gm_2010['EIA_PLANT_'] = pd.to_numeric(gm_2010['EIA_PLANT_'])
gm_2010['date'] = gm_2010['EIA_P_DATE'].str.split('-').str[1:4]
gm_2010['month'] = gm_2010['date'].str[1]
gm_2010['year'] = gm_2010['date'].str[0]
gm_2010['day'] = gm_2010['date'].str[2]
gm_2010['date'] = gm_2010['year'] + '-' + gm_2010['month']+ '-' + gm_2010['day']
gm_2010['date'] = pd.to_datetime(gm_2010['date'])

#%%
# interp format
#gm_2010['year'] = gm_2010['EIA_P_DATE'].astype(str).str[-10:-6]
#gm_2010['month'] = gm_2010['EIA_P_DATE'].astype(str).str[-5:-3]
#gm_2010['EIA_PLANT_'] = gm_2010['EIA_P_DATE'].astype(str).str[0:-11]
#gm_2010['EIA_PLANT_'] = gm_2010['EIA_PLANT_'].astype('int64')


# In[13]:

#gm_2015.drop(columns = ['Unnamed: 0'], inplace=True)
#gm_2010.drop(columns = ['Unnamed: 0'], inplace=True) # also drops HI and AK plants, no gridMET data there to compare, so 1106 
##plants instead of 1122
#

# In[14]:


# Step 3- calculate monthly means and medians from gridMET data
#gm_2010['YEAR'] = gm_2010['day'].astype(str).str[0:4]
#gm_2010['month'] = gm_2010['day'].astype(str).str[5:7]
month_means_15 = pd.pivot_table(gm_2015, index = ['EIA_PLANT_','month'], aggfunc=np.mean)
month_medians_15 = pd.pivot_table(gm_2015, index = ['EIA_PLANT_','month'], aggfunc=np.median)
month_means_15.reset_index(inplace=True)
month_medians_15.reset_index(inplace=True)

#%%
month_means = pd.pivot_table(gm_2010, index = ['EIA_PLANT_','month'], aggfunc=np.mean)
month_medians = pd.pivot_table(gm_2010, index = ['EIA_PLANT_','month'], aggfunc=np.median)
month_means.reset_index(inplace=True)
month_medians.reset_index(inplace=True)
#gm_2010.drop(columns = ['Unnamed: 0'], inplace=True) ##not sure why I cant do this before the aggfunc, but if I do
# the number of plants reduces to 1106. Maybe there are some plants that are getting aggregated wrong. for now this is the 
# work aroudn to drop that column after




# In[16]:


# Step 4: combine data for comparison
# Adding a column called "EIA_P_DATE" which combines EIA PLANT and DATE 
month_means['EIA_P_DATE'] = month_means.apply(lambda row: str(row.EIA_PLANT_) + "_2015-" + str(row.month), axis = 1)
month_medians['EIA_P_DATE'] = month_medians.apply(lambda row: str(row.EIA_PLANT_) + "_2015-" + str(row.month), axis = 1)
pm_2010['EIA_P_DATE'] = pm_2010.apply(lambda row: str(row.EIA_PLANT_) + "_2015-" + str(row.month), axis = 1)





# In[19]:


#rename cols
cols_mean =['EIA_PLANT_','month','gm_DB_mean', 'gm_EV_mean', 'gm_WB_mean','gm_WS_mean','EIA_P_DATE']
month_means.columns = cols_mean
cols_median =['EIA_PLANT_','month','gm_DB_med', 'gm_EV_med', 'gm_WB_med','gm_WS_med','EIA_P_DATE']
month_medians.columns = cols_median

#reorder cols
month_means = month_means[['EIA_P_DATE','EIA_PLANT_','month','gm_DB_mean', 'gm_WB_mean', 'gm_WS_mean', 'gm_EV_mean']]
month_medians = month_medians[['EIA_P_DATE','EIA_PLANT_','month','gm_DB_med', 'gm_WB_med', 'gm_WS_med', 'gm_EV_med']]
#pm_2010 = pm_2010[['EIA_P_DATE','EIA_PLANT_','month', 'DB_C','WB_C','WS_ms','EV_mm']]


# In[20]:


len(month_means) == 1106*12
len(month_medians) == 1106*12


# In[21]:


#merge the dfs on EIA_P_DATE
mean_stats = pd.merge(month_means, pm_2010, on = 'EIA_P_DATE')


# In[22]:


#compute the obs-sim column
mean_stats['OBS_SIM_DB'] = mean_stats['DB_C'] - mean_stats['gm_DB_mean']
mean_stats['OBS_SIM_WB'] = mean_stats['WB_C'] - mean_stats['gm_WB_mean']
mean_stats['OBS_SIM_WS'] = mean_stats['WS_ms'] - mean_stats['gm_WS_mean']
mean_stats['OBS_SIM_EV'] = mean_stats['EV_mm'] - mean_stats['gm_EV_mean']


# In[23]:


# compute the squared resid column
mean_stats['DB_res_sq'] = (mean_stats['OBS_SIM_DB'])**2
mean_stats['WB_res_sq'] = (mean_stats['OBS_SIM_WB'])**2
mean_stats['WS_res_sq'] = (mean_stats['OBS_SIM_WS'])**2
mean_stats['EV_res_sq'] = (mean_stats['OBS_SIM_EV'])**2



# In[25]:


# fix up the df 
mean_stats.drop(columns= ['EIA_PLANT__y', 'month_y'], inplace = True)





# In[27]:


cols = ['EIA_P_DATE', 'EIA_PLANT_', 'month', 'gm_DB_mean', 'gm_WB_mean',
       'gm_WS_mean', 'gm_EV_mean', 'DB_C', 'WB_C',
       'WS_ms', 'EV_mm', 'OBS_SIM_DB', 'OBS_SIM_WB',
       'OBS_SIM_WS', 'OBS_SIM_EV', 'DB_res_sq',
       'WB_res_sq', 'WS_res_sq', 'EV_res_sq']
mean_stats.columns = cols


# In[28]:


# and add lat long for plotting
mean_plot = pd.merge(mean_stats, TE_shp, on = 'EIA_PLANT_')


# In[30]:


mean_plot.drop(columns = ['NAME_OF_WA','COMID', 'COOLING_TY','GENERATION', 'WATER_SOUR', 'WATER_TYPE', 
                         'WITHDRAWAL', 'CONSUMPTIO','MIN_WITHDR', 'MAX_WITHDR', 'MIN_CONSUM', 'MAX_CONSUM', 
                         'NET_GENERA','geometry'], inplace =True)

# In[34]:


#mean_plot = pd.merge(mean_plot, elev_df, left_on = 'EIA_PLANT_', right_on = 'EIA_PLANT_ID')



# In[36]:


geometry = [Point(xy) for xy in zip(mean_plot['LONGITUDE'],mean_plot['LATITUDE'])]
geometry[:3]

crs = {'init': 'epsg:4326'}
mean_df = gpd.GeoDataFrame(mean_plot, crs = crs, geometry= geometry)

# In[40]:


mean_df.drop(columns = ['why_no_CID'], inplace = True)

# In[41]:


np.linspace(1,12,12)
months = ['01','02','03','04', '05', '06', '07', '08', '09', '10', '11', '12']
rmse_dict = {'month': months,'DB': months, 'WB': months, 'WS': months, 'EV': months}
rmse_df = pd.DataFrame(data = rmse_dict)

# In[48]:

os.chdir(os.path.join(cdir, '8_analysis'))
#%%
## for testing
#months = ['01']
params = ['EV']
units = ['mm']
# not enough memory to get through all of them, need to separate and do EV separate
#params = ['DB','WB','WS','EV']

#units = ['deg_C','deg_C','m_s','mm']

for p, param in enumerate(params):
    with PdfPages(param + units[p] +'.pdf') as pdf:
        for i, val in enumerate(months):
            if param == 'EV':
                monthly_df = mean_df[mean_df['month']==val]
                monthly_df = monthly_df.dropna()
            else:
                monthly_df = mean_df[mean_df['month']==val]
                
            #output shapefile
#            monthly_df.to_file(os.path.join(cdir,'GIS','Residuals', param, param+val+'.shp'))
            
            fig,(ax1,ax2) = plt.subplots(nrows = 2, figsize = (8,8))
            col_1 = 'OBS_SIM_'+ param                             
            col_2 = param + '_res_sq'
            RMSE = np.sqrt(np.mean(monthly_df[col_2]))
            rmse_df.loc[i,param] = RMSE
            ax1.set_title('OBS-SIM_'+ val +'_' + param +'_'+ units[p] )           
            monthly_df.plot(ax = ax1, column = col_1, legend = True, cmap = 'gist_rainbow')
            ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
            
            ax2.set_title('OBS-SIM'+ '_' + val+ '_'+ param + '_'+units[p]+ ', RMSE = ' + str(np.round(RMSE,2)))
            ax2.hist(monthly_df[col_1])
            pdf.savefig()
            plt.clf()
            
#             #x = np.linspace(0,monthly_df[col_1].max())
            plt.figure(figsize = (8,8))
#             ax1 = f.add_subplot(211, aspect = 'equal')
#             ax1 = f.add_subplot(212, aspect = 'equal')
#             fig, (ax1, ax2) = plt.subplots(nrows = 2, figsize = (8,8))
            plt.title('Elev residuals vs ' + col_1 + ' residuals-' + 'month_'+ val)
            plt.scatter(monthly_df['PM-gM'], monthly_df[col_1])
#            ax1.scatter((monthly_df['PM - gM']/np.mean(monthly_df['PM - gM'])), 
                          #(monthly_df[col_1]/np.mean(monthly_df[col_1])))
            plt.xlabel('elev_residuals_PM-gM')
            plt.ylabel(col_1)
            
#             ax2.set_title('zero in on large diff elevs')
#             large_diffs = monthly_df[np.absolute(monthly_df['PM - gM'])>10]
#             ax2.scatter(large_diffs['PM - gM'], large_diffs[col_1])
            
            pdf.savefig()
            plt.clf()
            
        plt.figure(figsize = (8,8))    
        plt.plot(rmse_df['month'],rmse_df[param])
        plt.title('RMSE_' + str(param))
        plt.xlabel('month')
        plt.ylabel('RMSE')
        pdf.savefig()
        plt.clf()


# In[57]:
#
#
### for testing
##months = ['01']
#params = ['WB']
#units = ['deg_C']
## not enough memory to get through all of them, need to separate and do EV separate
##params = ['DB','WB','WS','EV']
#
##units = ['deg_C','deg_C','m_s','mm']
#
#for p, param in enumerate(params):
#    with PdfPages(param + units[p] +'orig_abs'+'.pdf') as pdf:
#        for i, val in enumerate(months):
#                monthly_df = mean_df[mean_df['month']==val]
#                
#            #output shapefile
#            #monthly_df.to_file(os.path.join(TEdir,'..','GIS','Residuals', param, param+val+'.shp'))
#            
#                fig, ax1 = plt.subplots(nrows = 1, figsize = (8,8))
#                col_1 = 'gm_'+ param + '_mean'                             
#                col_2 = param + '_C'
#
#                ax1.set_title('abs_'+ val +'_' + param +'_'+ units[p] )           
#                monthly_df.plot(ax = ax1, column = col_1, legend = True, cmap = 'gist_rainbow')
#                ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
#                pdf.savefig()
#                plt.clf()
#
#
## In[ ]:
#
#
#large_diffs = monthly_df[np.absolute(monthly_df['PM - gM'])>10]
#
#
## In[47]:
#
#
#rmse_df
#
#
## In[ ]:
#
#
#monthly_df.to_file(os.path.join(TEdir,'..','GIS','Residuals', param, param+val+'.shp'))
#
#
## In[ ]:
#
#
#fig, (ax1, ax2) = plt.subplots(nrows = 2, figsize = (8,8), sharex = True, sharey= True)
#ax1.set_title('RMSE_'+ val + '_' + param + '_'+ units[p])
#col_0 = 'RMSE_'+ param
#monthly_df.plot(ax = ax1, column = col_0, legend = True)
#ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
#ax2.set_title('OBS-SIM_'+ val +'_' + param +'_'+ units[p])
#col_1 = 'OBS_SIM_'+ param
#monthly_df.plot(ax = ax2, column = col_1, legend = True)
#ctx.add_basemap(ax2, crs = crs, source = ctx.providers.OpenTopoMap)
#
#
## In[ ]:
#
#
#ctx.providers.keys()
#
#
## In[ ]:
#
#
#monthly_df = monthly_df.dropna(inplace =True)
#
#
## In[ ]:
#
#
#with PdfPages('January.pdf') as pdf:
#    fig, (ax1, ax2) = plt.subplots(nrows = 2, sharex = True, sharey= True)
#    ax1.set_title('RMSE')
#    monthly_df.plot(ax = ax1, column = 'RMSE_DB', legend = True)
#    ax2.set_title('OBS-SIM')
#    monthly_df.plot(ax = ax2, column = 'OBS_SIM_DB', legend = True)
#
#    pdf.savefig()
#
#
## In[ ]:
#
#
#fig, (ax1, ax2) = plt.subplots(nrows = 2)
#ax1.set_title('RMSE')
#ax1.hist(jan_df['RMSE_DB'])
#ax2.set_title('OBS-SIM')
#ax2.hist(jan_df['OBS_SIM'])
#
#
## In[ ]:
#
#
#for_plot_jan.to_csv('RMSE_Jan.csv')
#
#
## In[ ]:
#
#
#for_plot_feb = for_plot[for_plot['month']=='02']
#
#
## In[ ]:
#
#
#for_plot_jan.to_csv('RMSE_Jan.csv')
#
#
## In[ ]:
#
#
#range_pm = np.max(for_plot_jan['DB_C'])-np.min(for_plot_jan['DB_C'])
#range_gm = np.max(for_plot_jan['gm_DB_mean'])-np.min(for_plot_jan['gm_DB_mean'])
#
#
## In[ ]:
#
#
#range_gm
#
#
## In[ ]:
#
#
## Identify and analyze these fits. Are the trends regional? correlations with elevation? 
#
#
## In[ ]:
#
#
## Calculate a bias correction for the gridMET data
## underlying assumption is that the 2015 input data is "true". Any caveats here? Although 2015 input data was rigorously
## examined, there are more temporal data with gridMET, for which there might be some value. 
#
#
## In[ ]:




