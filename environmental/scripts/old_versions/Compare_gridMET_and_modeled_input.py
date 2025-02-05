#!/usr/bin/env python
# coding: utf-8

# In[4]:


import geopandas as gpd
import pandas as pd
import os
from datetime import datetime

# In[27]:


#bring in the df with gridMET data, bring in df with input data
TEdir = r'C:\WBEEP\Thermoelectric-master\Climate_data_fetcher'
TE_df = gpd.read_file(os.path.join(TEdir, '..\GIS','2015_TE_Model_Estimates_lat.long_COMIDs.shp'))
GM_data = pd.read_csv(os.path.join(TEdir, 'TE_plants_w_2015_daily_gridMET.csv'))
input_2015 = pd.read_csv(os.path.join(TEdir, '..\TE_Harris_Diehl_2015', '2015_TE_input_data_AEG.csv'))

# In[36]:
TE_input = input_2015.drop(columns = ['COUNTY', 'STATE', 'NAME_OF_WATER_SOURCE','COOLING_TYPE', 'MODEL_TYPE', 'PERCENT_CD_ALLOCATION', 
                        'ELEVATION','POND_AREA','CD_Jan', 'CD_Feb', 'CD_Mar', 'CD_Apr', 'CD_May', 'CD_Jun', 'CD_Jul', 'CD_Aug',
                       'CD_Sep', 'CD_Oct', 'CD_Nov', 'CD_Dec','WT_Jan', 'WT_Feb', 'WT_Mar', 'WT_Apr', 'WT_May', 'WT_Jun', 'WT_Jul',
                       'WT_Aug', 'WT_Sep', 'WT_Oct', 'WT_Nov', 'WT_Dec'])




# In[41]:
GM_data['YEAR'] = GM_data['day'].astype(str).str[0:4]
GM_data['MONTH'] = GM_data['day'].astype(str).str[5:7]
#%%


monthly_GM = input_2015.copy(deep = True) 


# In[45]:


monthly_gm_nd = monthly_GM.drop_duplicates(subset = 'EIA_PLANT_ID')


# In[46]:

days = gridMETdata['day']
gridMETdata['date'] = ""
#gridMETdata['date'] = 
for d, val in enumerate(days):
    gridMETdata.iloc[d,11] = datetime.strptime(val,'%Y-%m-%d').date()
    

gridMETdata['date'] =gridMETdata['day']

# In[ ]:




