# -*- coding: utf-8 -*-
"""
Created on Wed Mar 10 09:31:53 2021

@author: galanter
"""

# add dates from the plant_date column
import os
import pandas as pd
#%%
te_dir = r'C:\WBEEP\Thermoelectric-master'
wd = os.path.join(te_dir, '5_output', 'historical')

#%%


#%%
#yrs = ['1995','1996','1997','1998','1999','2000','2001','2002','2003','2004',
#       '2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015']
yrs = ['2016','2017','2018','2019']
#yrs = ['2020']
for y, yr in enumerate(yrs):
    fname = 'gridMET_at_TE_locations_' + yr + '.csv'
    df = pd.read_csv(os.path.join(wd,fname))
           
    df['EIA_PLANT_'] = df['EIA_P_DATE'].str.split('-').str[0]
    df['EIA_PLANT_'] = pd.to_numeric(df['EIA_PLANT_'])
    df['date'] = df['EIA_P_DATE'].str.split('-').str[1:4]
#    df['date-as-dt'] = df['date'].str[0] + '-' + df.loc[0,'date'][1] + '-'+ df.loc[0,'date'][2]
    #df['date-as-dt'] = pd.to_datetime(df['date-as-dt'])
    
    #reorder cols
    df = df[['EIA_P_DATE','EIA_PLANT_','date','rh_avg', 'air_tmp_C_avg', 
             'open_wtr_et_mm', 'wb_tmp_C','rh_max', 'rh_min', 'sp_h_kg_kg', 
             'air_tmn_K', 'air_tmx_K', 'precip_mm', 'wnd_spd_m_s', 
             'wnd_dir_dg_clk_frm_N', 'ref_et_gr_mm', 'ref_et_alf_mm','sur_rad_Wm2', 
             'vpdef_kPa',]]
    
    df.to_csv(os.path.join(wd,fname), index = False)