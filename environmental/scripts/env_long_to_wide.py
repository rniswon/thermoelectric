#! /usr/bin/python
#
#  File - env_long_to_wide.py
#
#  Purpose - Merge condenser duty output table and gridMET
#            environmental table, and produce result in
#            rawInput_FEWSR_TOWER.csv format.
#
#  Author - Andrew Halper <ashalper@usgs.gov>
#

import csv
import os.path
import sys

import pandas as pd

# check cmd. line arguments
if len(sys.argv) != 4:
    print(
    'usage: %s <condenser-duty-path> <gridMET-env-path> <output-path>' %
        os.path.basename(sys.argv[0])
    )
    exit(1)

quotes = '\"\''
condenser_duty_path = sys.argv[1].strip(quotes)
gridmet_env_path = sys.argv[2].strip(quotes)

# read CSV input files
condenser_duty = pd.read_csv(condenser_duty_path, index_col='Plant.Code')
gridmet_env = pd.read_csv(
    gridmet_env_path, index_col='eia_plant',
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
join.to_csv(sys.argv[3].strip(quotes), quoting=csv.QUOTE_NONNUMERIC)
