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
# ### Environment -- use 'ofp_for_te_.yml' 

# In[1]:


# step 0- import the needed packages
import geopandas as gpd
import pandas as pd
import os
import numpy as np


# In[2]:


# step 1- bring df with lat longs of TE plants, gridMET data, 2015 model input data
TEdir = r'C:\WBEEP\Thermoelectric-master\Climate_data_fetcher'
TE_df = gpd.read_file(os.path.join(TEdir, '..\GIS','2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
GM_data = pd.read_csv(os.path.join(TEdir, 'TE_plants_w_2015_daily_gridMET.csv'))



# In[ ]:
input_2015 = pd.read_csv(os.path.join(TEdir, '..\TE_Harris_Diehl_2015', '2015_TE_input_data_AEG.csv'))
#pond_evap = pd.read_csv(os.path.join(TEdir, 'Climate_data_fetcher', 'Ponds_info', 'longterm pond net evap2.xlsx'))
#AEG combined longterm pond net evap2 with 2015 input data
input_2015.drop_duplicates(subset = 'EIA_PLANT_ID', inplace=True)
input_2015.drop(columns = ['COUNTY', 'STATE', 'NAME_OF_WATER_SOURCE','COOLING_TYPE', 'MODEL_TYPE', 'PERCENT_CD_ALLOCATION', 
                        'ELEVATION','POND_AREA','CD_Jan', 'CD_Feb', 'CD_Mar', 'CD_Apr', 'CD_May', 'CD_Jun', 'CD_Jul', 'CD_Aug',
                       'CD_Sep', 'CD_Oct', 'CD_Nov', 'CD_Dec','WT_Jan', 'WT_Feb', 'WT_Mar', 'WT_Apr', 'WT_May', 'WT_Jun', 'WT_Jul',
                       'WT_Aug', 'WT_Sep', 'WT_Oct', 'WT_Nov', 'WT_Dec'], inplace=True)

#inpt_phys_mod = pd.DataFrame(columns = ['EIA_PLANT_ID','MONTH','DRY_BULB_C','WET_BULB_C','WIND_SPEED_MPH','EVAP'])
plants = input_2015['EIA_PLANT_ID']


    


# In[3]:

cols = ['DB','WB','WS','EV']
#df1 = pd.DataFrame['Month','DB','WB','WS','EV']
for i, EIA in enumerate(plants):
    temp = input_2015[input_2015['EIA_PLANT_ID'] == EIA]
    DB = temp.loc[:, 'DB_Jan':'DB_Dec']
    WB = temp.loc[:, 'WB_Jan':'WB_Dec']
    WS = temp.loc[:, 'WS_Jan':'WS_Dec']
    EV = temp.loc[:, 'EV_Jan':'EV_Dec']
    variables = [DB,WB,WS,EV]
    
    for j, var in enumerate(variables):
        df = var.T.reset_index()        
        df['index'] = df['index'].str[-3:]
        df.columns = ['Month', cols[j]]
        if j == 0:
            df1 = df.copy(deep=True)
        else: 
            df1 = pd.merge(df1, df, on = 'Month')
    
    df1['EIA_PLANT_ID'] =  EIA
    
    if i == 0:
        df2 = df1.copy(deep=True)
    else:
        df2 = df2.append(df1)
#        df['EIA_PLANT_ID'] = EIA
        
#    temp_row_T = temp_row.T.reset_index()
#    temp_row_T['index'] = temp_row_T['index'].str[-3:]
#    temp_row_T['EIA_PLANT_ID'] = EIA
    
#%%
conv=25.4 #inches to mm
#reorder columns
p_mod_input15 = df2[['EIA_PLANT_ID','DB', 'WB', 'EV']]


# In[4]:


# calculate monthly averages from gridMET data
GM_data['YEAR'] = GM_data['day'].astype(str).str[0:4]
GM_data['MONTH'] = GM_data['day'].astype(str).str[5:7]


# In[5]:


len(GM_data)


# In[ ]:


month_means = pd.pivot_table(GM_data, index = ['EIA_PLANT_','MONTH'], aggfunc=np.mean)
month_means.reset_index(inplace=True)

# In[ ]:


month_medians = pd.pivot_table(GM_data, index = ['EIA_PLANT_','MONTH'], aggfunc=np.median)

month_medians.reset_index(inplace=True)
# In[ ]:


month_means.drop(columns = ['Unnamed: 0'], inplace=True)
month_medians.drop(columns = ['Unnamed: 0'], inplace=True)
#GM_data.drop(columns = ['Unnamed: 0'], inplace=True) ##not sure why I cant do this before the aggfunc, but if I do
# the number of plants reduces to 1106. Maybe there are some plants that are getting aggregated wrong. for now this is the 
# work aroudn to drop that column after


# In[ ]:

df_1 = input_2015[input_2015['EIA_PLANT_ID'] == 3]
db_df = df_1.iloc[:, 4:16]
df_T = db_df.T.reset_index()
df_T['index'] = df_T['index'].str[-3:]


#%%
for i, EIA in enumerate(plants[:1]):
    temp = input_2015[input_2015['EIA_PLANT_ID'] == EIA]
    temp_row = df_1.iloc[:, 4:16]
    temp_row_T = db_df.T.reset_index()
    temp_row_T['index'] = temp_row_T['index'].str[-3:]
    #temp_rowT = temp_row.T.reset_index()
    #temp_rowT['index'] = temp_row['index'].str[-3:]
    
#%%
inpt_phys_mod = pd.DataFrame(columns = ['EIA_PLANT_ID','PLANT_NAME','MONTH','DRY_BULB_C','WET_BULB_C','WIND_SPEED_MPH','EVAP'])
#months = ['01','02','03','04','05','06','07','08','09','10','11','12']
for i, EIA in enumerate(plants[:1]):    
        inpt_phys_mod.iloc[j,0] = EIA
        inpt_phys_mod.iloc[j,1] = input_2015.iloc[i,2]
        inpt_pys_mod.iloc[j,2] = month
        inpt_pys_mod.iloc[j,3] = input_2015.iloc[i,j+4]
        
        
# In[8]:

input_2015.drop_duplicates(subset = 'EIA_PLANT_ID', inplace=True)
plants = input_2015['EIA_PLANT_ID']

# In[14]:





# In[32]:


conv=25.4 #inches to mm
input_2015['Map_evap_ap-oct-mm'] = input_2015['Map_evap_in_Ap_Oct']*conv
input_2015['Map_evap_2015-mm'] = input_2015['Map_evap_in_2015']*conv

#%%
input_test = input_2015.copy(deep = True) 
#%%
input_test.drop(columns = ['Map_evap_in_Ap_Oct', 'Map_evap_in_2015','WB_Jan', 'WB_Feb','WB_Mar', 'WB_Apr',
                           'WB_May', 'WB_Jun', 'WB_Jul', 'WB_Aug', 'WB_Sep',
       'WB_Oct', 'WB_Nov', 'WB_Dec', 'WS_Jan', 'WS_Feb', 'WS_Mar', 'WS_Apr',
       'WS_May', 'WS_Jun', 'WS_Jul', 'WS_Aug', 'WS_Sep', 'WS_Oct', 'WS_Nov',
       'WS_Dec', 'Map_evap_ap-oct-mm', 'Map_evap_2015-mm','WB_Jan', 'WB_Feb'], inplace= True)

#%%
input_test.drop(columns = ['WB_Jan', 'WB_Feb'], inplace=True)
# In[30]:


input_test.pivot(index = ['EIA_PLANT_ID','PLANT_NAME'], columns = 'DB)


# In[ ]:


# Spatially compare the RMSE between monthly values, use a color scale and statistics for define 'poor' 'moderate' 'good' fits


# In[ ]:


# Identify and analyze these fits. Are the trends regional? correlations with elevation? 


# In[ ]:


# Calculate a bias correction for the gridMET data
# underlying assumption is that the 2015 input data is "true". Any caveats here? Although 2015 input data was rigorously
# examined, there are more temporal data with gridMET, for which there might be some value. 


# In[ ]:


input_2015 = input_2015.drop(columns = ['COUNTY', 'STATE', 'NAME_OF_WATER_SOURCE','COOLING_TYPE', 'MODEL_TYPE', 'PERCENT_CD_ALLOCATION', 
                        'ELEVATION','POND_AREA','CD_Jan', 'CD_Feb', 'CD_Mar', 'CD_Apr', 'CD_May', 'CD_Jun', 'CD_Jul', 'CD_Aug',
                       'CD_Sep', 'CD_Oct', 'CD_Nov', 'CD_Dec','WT_Jan', 'WT_Feb', 'WT_Mar', 'WT_Apr', 'WT_May', 'WT_Jun', 'WT_Jul',
                       'WT_Aug', 'WT_Sep', 'WT_Oct', 'WT_Nov', 'WT_Dec'])


# In[ ]:


monthly_GM = input_2015.copy(deep = True) 
monthly_gm_nd = monthly_GM.drop_duplicates(subset = 'EIA_PLANT_ID')


# In[ ]:


monthly_gm_nd.head()


# In[ ]:


monthly_GM.shape


# In[ ]:




