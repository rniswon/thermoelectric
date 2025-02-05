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


# step 1- bring in df with lat longs of TE plants, gridMET data, 2015 model input data
TEdir = r'C:\WBEEP\Thermoelectric-master\Climate_data_fetcher'
TE_shp = gpd.read_file(os.path.join(TEdir, '..\GIS','2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
GM_data = pd.read_csv(os.path.join(TEdir, 'TE_plants_w_2015_daily_gridMET.csv'))
input_2015 = pd.read_csv(os.path.join(TEdir, '..\TE_Harris_Diehl_2015', '2015_TE_input_data_AEG.csv'))
# Combined longterm pond net evap2.xlsx with 2015 input data


# In[3]:


# step 2- alter format of input_2015 for easier comparison to GM_data

# remove duplicate plant EIAs and names, the duplicates are for different system types at the same location
# but not relevant for comparing climate data.Remove columns relating to CD and WT- don't have those variables to compare to
input_2015.drop_duplicates(subset = 'EIA_PLANT_ID', inplace=True)
input_2015.drop(columns = ['COUNTY', 'STATE', 'NAME_OF_WATER_SOURCE','COOLING_TYPE', 'MODEL_TYPE', 'PERCENT_CD_ALLOCATION', 
                        'ELEVATION','POND_AREA','CD_Jan', 'CD_Feb', 'CD_Mar', 'CD_Apr', 'CD_May', 'CD_Jun', 'CD_Jul', 'CD_Aug',
                       'CD_Sep', 'CD_Oct', 'CD_Nov', 'CD_Dec','WT_Jan', 'WT_Feb', 'WT_Mar', 'WT_Apr', 'WT_May', 'WT_Jun', 'WT_Jul',
                       'WT_Aug', 'WT_Sep', 'WT_Oct', 'WT_Nov', 'WT_Dec'], inplace=True)

    


# In[ ]:


input_2015.head()


# In[4]:


# step 2.1 a little more data manipulation, make rows into columns basically.
plants = input_2015['EIA_PLANT_ID']
cols = ['DB','WB','WS','EV']

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
        df.columns = ['MONTH', cols[j]]
        if j == 0:
            df1 = df.copy(deep=True)
        else: 
            df1 = pd.merge(df1, df, on = 'MONTH')
    
    df1['EIA_PLANT_ID'] =  EIA
    
    if i == 0:
        df2 = df1.copy(deep=True)
    else:
        df2 = df2.append(df1)


# In[13]:


# step 2.2 a little more data manipulation, ordering of columns, unit conv
conv=25.4 #inches to mm
#reorder columns
p_mod_input =df2.copy(deep=True)
p_mod_input = p_mod_input[['EIA_PLANT_ID','MONTH','DB', 'WB', 'EV']]
#conv Evap from inches to mm
p_mod_input['EV'] = p_mod_input['EV']*conv 

cols = ['EIA_PLANT_','MONTH','DB_C', 'WB_C', 'EV_mm']
p_mod_input.columns = cols


# In[14]:


p_mod_input.head()


# In[10]:


# Step 3- calculate monthly means and medians from gridMET data
GM_data['YEAR'] = GM_data['day'].astype(str).str[0:4]
GM_data['MONTH'] = GM_data['day'].astype(str).str[5:7]

month_means = pd.pivot_table(GM_data, index = ['EIA_PLANT_','MONTH'], aggfunc=np.mean)
month_medians = pd.pivot_table(GM_data, index = ['EIA_PLANT_','MONTH'], aggfunc=np.median)
month_means.drop(columns = ['Unnamed: 0'], inplace=True)
month_medians.drop(columns = ['Unnamed: 0'], inplace=True)
month_means.reset_index(inplace=True)
month_medians.reset_index(inplace=True)
#GM_data.drop(columns = ['Unnamed: 0'], inplace=True) ##not sure why I cant do this before the aggfunc, but if I do
# the number of plants reduces to 1106. Maybe there are some plants that are getting aggregated wrong. for now this is the 
# work aroudn to drop that column after


# In[11]:


month_medians.head()


# In[16]:


month_means['EIA_P_DATE'] = month_means.apply(lambda row: str(row.EIA_PLANT_) + "_2015-" + str(row.MONTH), axis = 1)
month_medians['EIA_P_DATE'] = month_medians.apply(lambda row: str(row.EIA_PLANT_) + "_2015-" + str(row.MONTH), axis = 1)
p_mod_input['EIA_P_DATE'] = p_mod_input.apply(lambda row: str(row.EIA_PLANT_) + "_2015-" + str(row.MONTH), axis = 1)


# In[ ]:


month_means.drop(columns = ['Unnamed: 0'], inplace=True)
month_medians.drop(columns = ['Unnamed: 0'], inplace=True)
#GM_data.drop(columns = ['Unnamed: 0'], inplace=True) ##not sure why I cant do this before the aggfunc, but if I do
# the number of plants reduces to 1106. Maybe there are some plants that are getting aggregated wrong. for now this is the 
# work aroudn to drop that column after


# In[ ]:


#month_means.head()
# check- 409,530 columns of data for 1122 plants for 365 days
# len(GM_data) == 1122*365
# monthly means = 1122 plants for 12 months
len(month_means) == 1122*12
len(month_medians) == 1122*12

# In[ ]:
for i in range(len(month_means)):
    
def rmse(predictions, targets):
    return np.sqrt(((predictions - targets)**2).mean())


month_means['RMSE_DB_C'] = 