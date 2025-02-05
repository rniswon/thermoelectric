#! /usr/bin/python
#
#  File - env_long_to_wide.py
#
#  Purpose - Merge condenser duty output table and gridMET
#            environmental table, and produce result in
#            rawInput_FEWSR_TOWER.csv format.
#
#  Author - Andrew Halper <ashalper@usgs.gov>
# Amy's adaptation 

#%%
import csv
import os.path
import sys
import pandas as pd
#%%
# # check cmd. line arguments
# if len(sys.argv) != 4:
#     print(
#     'usage: %s <condenser-duty-path> <gridMET-env-path> <output-path>' %
#         os.path.basename(sys.argv[0])
#     )
#     exit(1)

# quotes = '\"\''
teams = 'C:/Users/galanter/DOI/GS-WMA-WBEEP - Thermoelectric/'
condenser_duty_path = 'Final_CD_results_2008-2020/updated_12.27.22/'
gridmet_env_path = 'C:/WU/Thermoelectric/environmental/data/to_sciencebase/forPB/'
output_path = 'GridMet_data/long_to_wide/'
#%%
# for condenser_duty in ../../CD_results/CD_results????.csv; do
# 	# parse year from file name
#     year=$(basename "$condenser_duty" .csv | sed 's/CD_results//')
# 	# invoke env_long_to_wide.py to merge and transform
#     python env_long_to_wide.py \
# 	   "$condenser_duty" \
# 	   for_PB/"$year"_gridMET_env_input.csv \
# 	   "$year"_wide.csv
# done
# read CSV input files
yrs = ['08']
# yrs = ['08','09','10','11','12','13','14','15','16','17','18','19','20']
for y, yr in enumerate(yrs):
    cd = 'CD_results20' + yr + '.csv'
    gm = '20' + yr + '_gridMET_env_input.csv'
    condenser_duty = pd.read_csv(os.path.join(teams, condenser_duty_path, cd),
                                          index_col='Plant.Code')
    gridmet_env = pd.read_csv(os.path.join(teams, gridmet_env_path, gm),
                              index_col='eia_plant',
    # I don't understand why this part re orders the columns, why does owtr_et_in
    # read in before wb_tmp_c, seems to be alphabetical order?
    usecols=['eia_plant', 'date', 'db_tmp_c',
             'wb_tmp_c', 'wnd_spd_mph', 'owtr_et_in']
    ).pivot_table(index='eia_plant', columns='date')

    gridmet_env.index.rename('Plant.Code', inplace=True)

# Re-name columns.
#
# TODO: this still needs work; need to collapse these two index
# levels into one.
    gridmet_env.columns.set_levels(
        [['DB', 'EV', 'WB', 'WS'],
          ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']],
          inplace=True
    )
    
    # join tables
    join = pd.merge(condenser_duty, gridmet_env, on='Plant.Code')
    
    # A hack to serve as a short-term solution for the TODO above.
    columns = {}
    for c in join.columns:
        if isinstance(c, tuple):
            columns[c] = c[0] + '_' + c[1]
        else:
            columns[c] = c
    join.rename(columns = columns, inplace = True)
    
    # write to new CSV file
    out = '20' + yr + '_wide.csv'
    join.to_csv(os.path.join(teams, output_path,out))
