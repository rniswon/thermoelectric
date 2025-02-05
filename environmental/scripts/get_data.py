# -*- coding: utf-8 -*-
"""
Created on Thu Apr 22 12:44:51 2021

@author: galanter
"""

#digest NOAA buoy data

#%% import necessary packages
import os
import pandas as pd
import requests
import urllib.request
import time
from bs4 import BeautifulSoup

#%%
os.chdir(r'C:\WBEEP\Thermoelectric-master\4_scripts\water_temp_saline')

#%%
url = 'https://www.ndbc.noaa.gov/view_text_file.php?filename=46224h2015.txt.gz&dir=data/historical/stdmet/'
response = requests.get(url)

#%%
soup = BeautifulSoup(response.text,"html.parser")
soup.findAll('a')

#%%
download_url = 'https://www.ndbc.noaa.gov/view_text_file.php?filename=46224h2015.txt.gz&dir=data/historical/stdmet/'

#%%
urllib.request.urlretrieve(download_url, 'test.txt')

#%%
a = pd.read_fwf('test.txt')

a['date_time'] = a['MM'] + a['DD'] + a['#YY']
a['date_time'] =  pd.to_datetime(a['date_time'], format = '%m%d%Y', errors = 'coerce')

#%%
a.set_index('date_time', inplace = True)
b = a['WTMP'].astype(float)
daily = b.resample('D').mean()
monthly = daily.resample('M').mean()