#!/usr/bin/env python
# coding: utf-8

# # Compare gridMET data and modeled input
# 
# ### What this notebook does: 
# - Compare the 2015 gridMET data pulled at TE locations (using TE_fetcher_parser_nearest_neighbor_Daily_gridMET_data) to 2015 input data from Harris and Diehl (2019)
# - Integrate the interp data set and the NN dataset
# 
# ### Why the comparison/ big picture goal: 
# - Develop bias correction for gridMET data. Since 2015 input data from Harris and Diehl (2019) involved a lot of QA/QC and used climate stations, we will capitalize on that knowledge to better understand where the gridMET data might have issues. 
# 
# ### Environment -- use 'ofp_env_upd2.yml' -- aka ofp_for_te2
# ### will check more into contextily, maybe will update ofp_for_te, but for now kept separate

# In[1]:


# step 0- import the needed packages
get_ipython().run_line_magic('matplotlib', 'inline')
get_ipython().run_line_magic('pylab', 'inline')
pylab.rcParams['figure.figsize'] = (10.0, 8.0)

import geopandas as gpd
import contextily as ctx
import pandas as pd
import os
import numpy as np
from shapely.geometry import Point, polygon
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt


# In[2]:


# step 1- bring in df with lat longs of TE plants, gridMET data, 2015 model input data
TEdir = r'C:\WBEEP\Thermoelectric-master\Climate_data_fetcher'
TE_shp = gpd.read_file(os.path.join(TEdir, '..\GIS','2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
GM_data = pd.read_csv(os.path.join(TEdir, 'interpTE_plants_w_2015_daily_gridMET.csv'))
#interp_data =pd.read_csv(os.path.join(TEdir, 'interp_TE_plants_w_2010_daily_gridMET.csv'))
input_2015 = pd.read_csv(os.path.join(TEdir, '..\TE_Harris_Diehl_2015', '2015_TE_input_data_AEG_switch.csv'))
# Combined longterm pond net evap2.xlsx with 2015 input data


# In[ ]:


interp_data


# In[ ]:


GM_data


# In[ ]:


# Compare the interp data and the NN data to the input data


# In[3]:


# step 2- alter format of input_2015 for easier comparison to GM_data

# remove duplicate plant EIAs and names, the duplicates are for different system types at the same location
# but not relevant for comparing climate data.Remove columns relating to CD and WT- don't have those variables to compare to
input_2015.drop_duplicates(subset = 'EIA_PLANT_ID', inplace=True)
input_2015.drop(columns = ['COUNTY', 'STATE', 'NAME_OF_WATER_SOURCE','COOLING_TYPE', 'MODEL_TYPE', 'PERCENT_CD_ALLOCATION', 
                        'POND_AREA','CD_Jan', 'CD_Feb', 'CD_Mar', 'CD_Apr', 'CD_May', 'CD_Jun', 'CD_Jul', 'CD_Aug',
                       'CD_Sep', 'CD_Oct', 'CD_Nov', 'CD_Dec','WT_Jan', 'WT_Feb', 'WT_Mar', 'WT_Apr', 'WT_May', 'WT_Jun', 'WT_Jul',
                       'WT_Aug', 'WT_Sep', 'WT_Oct', 'WT_Nov', 'WT_Dec'], inplace=True)

    


# In[4]:


len(input_2015)


# In[5]:


# step 2.1 make rows into columns for input_2015 data
plants = input_2015['EIA_PLANT_ID']
cols = ['DB','WB','WS','EV']

for i, EIA in enumerate(plants):
    temp = input_2015[input_2015['EIA_PLANT_ID'] == EIA]
    DB = temp.loc[:, 'DB_01':'DB_12']
    WB = temp.loc[:, 'WB_01':'WB_12']
    WS = temp.loc[:, 'WS_01':'WS_12']
    EV = temp.loc[:, 'EV_01':'EV_12']
    variables = [DB,WB,WS,EV]
    
    for j, var in enumerate(variables):
        df = var.T.reset_index()        
        df['index'] = df['index'].str[-2:]
        df.columns = ['MONTH', cols[j]]
        if j == 0:
            df1 = df.copy(deep=True)
        else: 
            df1 = pd.merge(df1, df, on = 'MONTH')
    
    df1['EIA_PLANT_'] =  EIA
    
    if i == 0:
        df2 = df1.copy(deep=True)
    else:
        df2 = df2.append(df1)


# In[ ]:


df2


# In[6]:


# step 2.2 a little more data manipulation, ordering of columns, unit conv
conv_mm=25.4 #inches to mm
conv_ms = 1/60/60*1609.34
#reorder columns
p_mod_input =df2.copy(deep=True)
p_mod_input = p_mod_input[['EIA_PLANT_','MONTH','DB', 'WB', 'WS', 'EV']]
#conv Evap from inches to mm
p_mod_input['EV'] = p_mod_input['EV']*conv_mm
#conv WS from mph to m/s
p_mod_input['WS'] = p_mod_input['WS']*conv_ms

#rename cols
cols = ['EIA_PLANT_','MONTH','DB_C', 'WB_C', 'WS_ms','EV_mm']
p_mod_input.columns = cols


# In[11]:


a = GM_data.iloc[0,1]


# In[18]:


# with the new interp data structure, use this line
#GM_data.columns
GM_data['YEAR'] = GM_data['EIA_P_DATE'].astype(str).str[-10:-6]
GM_data['MONTH'] = GM_data['EIA_P_DATE'].astype(str).str[-5:-3]
GM_data['EIA_PLANT_'] = GM_data['EIA_P_DATE'].astype('int64').str[0:-11]


# In[7]:


# Step 3- calculate monthly means and medians from gridMET data, with the previous structure
#GM_data['YEAR'] = GM_data['day'].astype(str).str[0:4]
#GM_data['MONTH'] = GM_data['day'].astype(str).str[5:7]


# In[19]:


GM_data.drop(columns = ['Unnamed: 0'], inplace=True) # also drops HI and AK plants, no gridMET data there to compare, so 1106 
#plants instead of 1122



# In[26]:


GM_data.columns


# In[27]:


# Step 3- calculate monthly means and medians from gridMET data
#GM_data['YEAR'] = GM_data['day'].astype(str).str[0:4]
#GM_data['MONTH'] = GM_data['day'].astype(str).str[5:7]

month_means = pd.pivot_table(GM_data, index = ['EIA_PLANT_','MONTH'], aggfunc=np.mean)
month_medians = pd.pivot_table(GM_data, index = ['EIA_PLANT_','MONTH'], aggfunc=np.median)
month_means.reset_index(inplace=True)
month_medians.reset_index(inplace=True)
#GM_data.drop(columns = ['Unnamed: 0'], inplace=True) ##not sure why I cant do this before the aggfunc, but if I do
# the number of plants reduces to 1106. Maybe there are some plants that are getting aggregated wrong. for now this is the 
# work aroudn to drop that column after


# In[28]:


month_means.head()


# In[29]:


# Step 4: combine data for comparison
# Adding a column called "EIA_P_DATE" which combines EIA PLANT and DATE 
month_means['EIA_P_DATE'] = month_means.apply(lambda row: str(row.EIA_PLANT_) + "_2015-" + str(row.MONTH), axis = 1)
month_medians['EIA_P_DATE'] = month_medians.apply(lambda row: str(row.EIA_PLANT_) + "_2015-" + str(row.MONTH), axis = 1)
p_mod_input['EIA_P_DATE'] = p_mod_input.apply(lambda row: str(row.EIA_PLANT_) + "_2015-" + str(row.MONTH), axis = 1)


# In[30]:


month_means.shape


# In[31]:


13272/1106


# In[32]:


#rename cols
cols_mean =['EIA_PLANT_','MONTH','gm_DB_mean', 'gm_EV_mean', 'gm_WB_mean','gm_WS_mean','EIA_P_DATE']
month_means.columns = cols_mean
cols_median =['EIA_PLANT_','MONTH','gm_DB_med', 'gm_EV_med', 'gm_WB_med','gm_WS_med','EIA_P_DATE']
month_medians.columns = cols_median

#reorder cols
month_means = month_means[['EIA_P_DATE','EIA_PLANT_','MONTH','gm_DB_mean', 'gm_WB_mean', 'gm_WS_mean', 'gm_EV_mean']]
month_medians = month_medians[['EIA_P_DATE','EIA_PLANT_','MONTH','gm_DB_med', 'gm_WB_med', 'gm_WS_med', 'gm_EV_med']]
p_mod_input = p_mod_input[['EIA_P_DATE','EIA_PLANT_','MONTH', 'DB_C','WB_C','WS_ms','EV_mm']]


# In[33]:


len(month_means) == 1106*12
len(month_medians) == 1106*12


# In[34]:


#merge the dfs on EIA_P_DATE
mean_stats = pd.merge(month_means, p_mod_input, on = 'EIA_P_DATE')


# In[55]:


p_mod_input


# In[35]:


# Calculate the RMSE between monthly values
# future- use sklearn mean_squared_error, for now not messing with the env, just using numpy
def rmse(predictions, targets):
    return np.sqrt(((predictions - targets)**2).mean())


# In[36]:


mean_stats['RMSE_DB'] = ''
mean_stats['RMSE_WB'] = ''
mean_stats['RMSE_WS'] = ''
mean_stats['RMSE_EV'] = ''


# In[37]:


# calculate the rmse between predictions (gridMET data) and targets (climate station corrected data).
for i in range(len(mean_stats)):
    mean_stats.loc[i, 'RMSE_DB'] = rmse(mean_stats.loc[i,'gm_DB_mean'], mean_stats.loc[i,'DB_C'])
    mean_stats.loc[i, 'RMSE_WB'] = rmse(mean_stats.loc[i,'gm_WB_mean'], mean_stats.loc[i,'WB_C'])
    mean_stats.loc[i, 'RMSE_WS'] = rmse(mean_stats.loc[i,'gm_WS_mean'], mean_stats.loc[i,'WS_ms'])
    mean_stats.loc[i, 'RMSE_EV'] = rmse(mean_stats.loc[i,'gm_EV_mean'], mean_stats.loc[i,'EV_mm'])


# In[38]:


#compute the obs-sim column
mean_stats['OBS_SIM_DB'] = mean_stats['DB_C'] - mean_stats['gm_DB_mean']
mean_stats['OBS_SIM_WB'] = mean_stats['WB_C'] - mean_stats['gm_WB_mean']
mean_stats['OBS_SIM_WS'] = mean_stats['WS_ms'] - mean_stats['gm_WS_mean']
mean_stats['OBS_SIM_EV'] = mean_stats['EV_mm'] - mean_stats['gm_EV_mean']


# In[39]:


# fix up the df 
mean_stats.drop(columns= ['EIA_PLANT__y', 'MONTH_y'], inplace = True)


# In[40]:


mean_stats.columns


# In[41]:


cols = ['EIA_P_DATE', 'EIA_PLANT_', 'MONTH', 'gm_DB_mean', 'gm_WB_mean',
       'gm_WS_mean', 'gm_EV_mean', 'DB_C', 'WB_C',
       'WS_ms', 'EV_mm', 'RMSE_DB', 'RMSE_WB', 'RMSE_WS', 'RMSE_EV', 'OBS_SIM_DB', 'OBS_SIM_WB',
       'OBS_SIM_WS', 'OBS_SIM_EV']
mean_stats.columns = cols


# In[42]:


# and add lat long for plotting
mean_plot = pd.merge(mean_stats, TE_shp, on = 'EIA_PLANT_')


# In[43]:


mean_plot.head()
#mean_plot.fillna(-999)
#maybe dont need this


# In[44]:


mean_plot.drop(columns = ['NAME_OF_WA','COMID', 'COOLING_TY','GENERATION', 'WATER_SOUR', 'WATER_TYPE', 
                         'WITHDRAWAL', 'CONSUMPTIO','MIN_WITHDR', 'MAX_WITHDR', 'MIN_CONSUM', 'MAX_CONSUM', 
                         'NET_GENERA','geometry'], inplace =True)


# In[45]:


mean_plot.columns


# In[46]:


geometry = [Point(xy) for xy in zip(mean_plot['LONGITUDE'],mean_plot['LATITUDE'])]
geometry[:3]


# In[47]:


crs = {'init': 'epsg:4326'}


# In[48]:


#turn it into a pandas geodataframe
mean_df = gpd.GeoDataFrame(mean_plot, crs = crs, geometry= geometry)


# In[49]:


mean_df.columns


# In[50]:


mean_df


# In[51]:


## for testing
#months = ['01']
params = ['DB']
units = ['deg_C']
# not enough memory to get through all of them, need to separate and do EV separate

months = ['01','02','03','04', '05', '06', '07', '08', '09', '10', '11', '12']
#params = ['DB','WB','WS','EV']

#units = ['deg_C','deg_C','m_s','mm']

for p, param in enumerate(params):
    with PdfPages(param + units[p] +'.pdf') as pdf:
        for i, val in enumerate(months):
            if param == 'EV':
                monthly_df = mean_df[mean_df['MONTH']==val]
                monthly_df = monthly_df.dropna()
            else:
                monthly_df = mean_df[mean_df['MONTH']==val]

            fig, (ax1, ax2) = plt.subplots(nrows = 2, figsize = (8,8), sharex = True, sharey= True)
            ax1.set_title('RMSE_'+ val + '_' + param + '_'+ units[p])
            col_0 = 'RMSE_'+ param
#            mn = np.min(col_0)
#            mx = np.max(col_0)
            monthly_df.plot(ax = ax1, column = col_0, cmap = 'gist_rainbow')
            ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
            
            ax2.set_title('OBS-SIM_'+ val +'_' + param +'_'+ units[p])
            col_1 = 'OBS_SIM_'+ param
            monthly_df.plot(ax = ax2, column = col_1, cmap = 'gist_rainbow')
            ctx.add_basemap(ax2, crs = crs, source = ctx.providers.OpenTopoMap)
            pdf.savefig()
            plt.clf()
            
            fig1, (ax1, ax2) = plt.subplots(nrows = 2, figsize = (8,8), sharey=True)
            ax1.set_title('RMSE_' + val + '_' + param +'_'+ units[p])
            ax1.hist(monthly_df[col_0])
            ax2.set_title('OBS-SIM'+ '_' + val+ '_'+ param + '_'+units[p])
            ax2.hist(monthly_df[col_1])

            pdf.savefig()
            plt.clf()


# In[ ]:


fig, (ax1, ax2) = plt.subplots(nrows = 2, figsize = (8,8), sharex = True, sharey= True)
ax1.set_title('RMSE_'+ val + '_' + param + '_'+ units[p])
col_0 = 'RMSE_'+ param
monthly_df.plot(ax = ax1, column = col_0, legend = True)
ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
ax2.set_title('OBS-SIM_'+ val +'_' + param +'_'+ units[p])
col_1 = 'OBS_SIM_'+ param
monthly_df.plot(ax = ax2, column = col_1, legend = True)
ctx.add_basemap(ax2, crs = crs, source = ctx.providers.OpenTopoMap)


# In[ ]:


ctx.providers.keys()


# In[ ]:


monthly_df = monthly_df.dropna(inplace =True)


# In[ ]:


with PdfPages('January.pdf') as pdf:
    fig, (ax1, ax2) = plt.subplots(nrows = 2, sharex = True, sharey= True)
    ax1.set_title('RMSE')
    monthly_df.plot(ax = ax1, column = 'RMSE_DB', legend = True)
    ax2.set_title('OBS-SIM')
    monthly_df.plot(ax = ax2, column = 'OBS_SIM_DB', legend = True)

    pdf.savefig()


# In[ ]:


fig, (ax1, ax2) = plt.subplots(nrows = 2)
ax1.set_title('RMSE')
ax1.hist(jan_df['RMSE_DB'])
ax2.set_title('OBS-SIM')
ax2.hist(jan_df['OBS_SIM'])


# In[ ]:


for_plot_jan.to_csv('RMSE_Jan.csv')


# In[ ]:


for_plot_feb = for_plot[for_plot['MONTH']=='02']


# In[ ]:


for_plot_jan.to_csv('RMSE_Jan.csv')


# In[ ]:


range_pm = np.max(for_plot_jan['DB_C'])-np.min(for_plot_jan['DB_C'])
range_gm = np.max(for_plot_jan['gm_DB_mean'])-np.min(for_plot_jan['gm_DB_mean'])


# In[ ]:


range_gm


# In[ ]:


# Identify and analyze these fits. Are the trends regional? correlations with elevation? 


# In[ ]:


# Calculate a bias correction for the gridMET data
# underlying assumption is that the 2015 input data is "true". Any caveats here? Although 2015 input data was rigorously
# examined, there are more temporal data with gridMET, for which there might be some value. 


# In[ ]:




