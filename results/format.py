# File - format.py
#
# Purpose - Format TEWU model results for consumption by visualization
#           team.
#
# Author - Andrew Halper
#

# This script was developed in the "geo" Conda environment. See
# ../environmental/envs/geo.yml to create it.

import os.path
import sys

import pandas as pd


# check cmd. line arguments
if len(sys.argv) != 3:
    print('usage: %s <input-path> <output-path>' %
          os.path.basename(sys.argv[0]))
    exit(1)

quotes = '\"\''
dir_in = sys.argv[1].strip(quotes)
dir_out = sys.argv[2].strip(quotes)

# for each temporal point type
for point_tp in ['Annual', 'Monthly']:
    # for each parameter
    for parameter_nm in ['Consump', 'WD']:
        csv_in = pd.read_csv(
            os.path.join(dir_in, 'compile_' + point_tp +
                         '_' + parameter_nm + '_EST.csv'),
            header=0
        )
        # rename "YEAR" column to "Year"
        csv_in.rename(columns={'YEAR': 'Year'}, inplace=True)
        # for each statistic
        for stat_id in ['Med', 'Min', 'Max']:
            # subdivide DataFrame by dependent statistic (column)
            dependent = 'Model_' + stat_id + '_' + parameter_nm
            if point_tp == 'Annual':
                out = csv_in[['Plant.Code', 'Year', dependent]].pivot_table(
                          index='Year', columns='Plant.Code'
                      )
            elif point_tp == 'Monthly':
                out = csv_in[['Plant.Code', 'Year', 'Month', dependent]].pivot_table(
                          index=['Year', 'Month'], columns='Plant.Code'
                      )
            # first row containing repeating variable names is not
            # needed in output format, so remove it
            out.columns = out.columns.droplevel()
            # write output CSV file
            out.to_csv(
                os.path.join(dir_out, parameter_nm + '_' + point_tp.lower() + '_' +
                             stat_id.lower() + '_eiaid_2008_2020.csv')
            )
