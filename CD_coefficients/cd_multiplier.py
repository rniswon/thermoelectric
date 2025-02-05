import os.path

import pandas as pd
import numpy as np
import datetime
import flopy
from flopy.plot import styles
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

#data_dir = r"C:\Users\rniswon\Documents\Data\Water_Use_Project\Thermoelectric\strategy 2000-2007\updated_Intermediate_with_bogen_dom_fuel_05.18.23"
dirname = os.path.dirname(__file__)
data_dir = os.path.join(dirname, './')
new_headers = pd.read_csv(os.path.join(data_dir, "new_headers.csv"))
new_headers = new_headers['new_head'].values.tolist()
months = ['January', 'February', 'March', 'April', 'May', 'June', 'July',
          'August', 'September', 'October', 'November', 'December']
gen_types = ['biomass^ST', 'coal^ST', 'gas^NGCC', 'gas^ST', 'NA^ST', 'oil^NGCC', 'oil^ST', 'other^NGCC', 'other^ST']
#exported_headers
exported_header1 = r"Exported_Heat_January Exported_Heat_February Exported_Heat_March Exported_Heat_April Exported_Heat_May Exported_Heat_June Exported_Heat_July Exported_Heat_August Exported_Heat_September Exported_Heat_October Exported_Heat_November Exported_Heat_December"
exported_header1 = exported_header1.split()

exported_header2 = r"good_CD_january good_CD_february good_CD_march good_CD_april good_CD_may good_CD_june good_CD_july good_CD_august good_CD_september good_CD_october good_CD_november good_CD_december"
exported_header2 = exported_header2.split()

exported_header3 = r"good_MWh_january good_MWh_february good_MWh_march good_MWh_april good_MWh_may good_MWh_june good_MWh_july good_MWh_august good_MWh_september good_MWh_october good_MWh_november good_MWh_december"
exported_header3 = exported_header3.split()

exported_header4 = r"good_Fuel_Heat_january good_Fuel_Heat_february good_Fuel_Heat_march good_Fuel_Heat_april good_Fuel_Heat_may good_Fuel_Heat_june good_Fuel_Heat_july good_Fuel_Heat_august good_Fuel_Heat_september good_Fuel_Heat_october good_Fuel_Heat_november good_Fuel_Heat_december"
exported_header4 = exported_header4.split()

na_values = ["",
             "#N/A",
             "#N/A N/A",
#             "#NA",
             "-1.#IND",
             "-1.#QNAN",
             "-NaN",
             "-nan",
             "1.#IND",
             "1.#QNAN",
#             "<NA>",
             "N/A",
#              "NA",
             "NULL",
             "NaN",
             "n/a",
             "nan",
             "null"]
# read in intermediate results file to include water use estimates
exported_header8 = r"Consumption_MGD Withdrawal_MGD CD_MMBtu_permonth FUELHEAT NETGEN"
exported_header8 = exported_header8.split()
exported_header9 = r"Plant.Code Plant.Name County State cooling dom_fuel Plant.level_dom_fuel general_mover percentAllocation Year"
exported_header9 = exported_header9.split()
#index_2 = ['BE', 'Month']
#df_be.index =  index_2
#plant, month, year, cooling type, percent allocation, and also dom_fuel
file = 'result_intermediates_monthly.csv'
# Create your original DataFrame with multiple rows
df = pd.read_csv(os.path.join(data_dir, file))
num_rows = len(df)
index = "Plant.Name	County	State	cooling	dom_fuel	Plant.level_dom_fuel	general_mover Year percentAllocation".split()
df4 = df.pivot_table(index = index, columns = 'Month', values = ['Consumption_MGD', 'Withdrawal_MGD', 'CD_MMBtu_permonth',
                     'FUELHEAT', 'NETGEN'])
# loop over get type and then years in separate files
files = os.listdir(data_dir)  #this is for looking over files in the folder
pd.set_option('mode.use_inf_as_na', True)  # all inf values are set to NAN
df_be = pd.DataFrame()
df_be_ave = pd.DataFrame()
df_annual_list = []
year_df = pd.DataFrame()
year_df['year'] = np.arange(2008,2021)
# Outer loop on gen type
for gen in gen_types:
    all_dfs = []
    for file in files:  #this is loop for each year's file
        #file = r"compileCDintermediate2020.csv"  # hardcoded to testing to this 2020 file
        if not("compile" in file):   #this is for looping over files
            continue
        if not (file.split(".")[-1] == "csv"):
            continue
        year = int(file.split(".")[0][-4:])
        cols_with_NA = ['bogen_dom_fuel']
        curr_df_Na = pd.read_csv(os.path.join(data_dir, file), usecols = cols_with_NA, na_values= na_values, keep_default_na= False )
        curr_df = pd.read_csv(os.path.join(data_dir, file) ) #na_filter=False
        for c in cols_with_NA:   #this deals with the use of "NA" for nuclear plants
            curr_df[c] = curr_df_Na[c]
        col_list = []
    # these headers are for summing across columns
        exported_header5 = r"good_" + gen + "_CD_january good_" + gen + "_CD_february good_" + gen + "_CD_march good_" + gen + "_CD_april good_" + gen + "_CD_may good_" + gen + "_CD_june good_" + gen + "_CD_july good_" + gen + "_CD_august good_" + gen + "_CD_september good_" + gen + "_CD_october good_" + gen + "_CD_november good_" + gen + "_CD_december"
        exported_header5 = exported_header5.split()

        exported_header6 = r"good_" + gen + "_MWh_january good_" + gen + "_MWh_february good_" + gen + "_MWh_march good_" + gen + "_MWh_april good_" + gen + "_MWh_may good_" + gen + "_MWh_june good_" + gen + "_MWh_july good_" + gen + "_MWh_august good_" + gen + "_MWh_september good_" + gen + "_MWh_october good_" + gen + "_MWh_november good_" + gen + "_MWh_december"
        exported_header6 = exported_header6.split()

        exported_header7 = r"good_" + gen + "_Fuel_Heat_january good_" + gen + "_Fuel_Heat_february good_" + gen + "_Fuel_Heat_march good_" + gen + "_Fuel_Heat_april good_" + gen + "_Fuel_Heat_may good_" + gen + "_Fuel_Heat_june good_" + gen + "_Fuel_Heat_july good_" + gen + "_Fuel_Heat_august good_" + gen + "_Fuel_Heat_september good_" + gen + "_Fuel_Heat_october good_" + gen + "_Fuel_Heat_november good_" + gen + "_Fuel_Heat_december"
        exported_header7 = exported_header7.split()

        exported_header10 = r"good_" + gen + "steam_heat_january" "good_" + gen + "steam_heat_february" "good_" + gen + "steam_heat_march" "good_" + gen + "steam_heat_april" "good_" + gen + "steam_heat_may" "good_" + gen + "steam_heat_june" "good_" + gen + "steam_heat_july" "good_" + gen + "steam_heat_august" "good_" + gen + "steam_heat_september" "good_" + gen + "steam_heat_october" "good_" + gen + "steam_heat_november" "good_" + gen + "steam_heat_december"
        exported_header10 = exported_header10.split()

        # first (EH)
        curr_df["Gen_Cat"] = curr_df["bogen_dom_fuel"].astype(str) + "^" + curr_df["general_mover"].astype(str)
        col_list.append("Gen_Cat")

        # plant exported heat  (EI)
        curr_df['plant exported heat'] = curr_df[exported_header1].sum(axis=1)
        col_list.append("plant exported heat")
        curr_df.loc[curr_df['plant exported heat'] > 0, 'plant exported heat'] = \
            curr_df.loc[curr_df['plant exported heat'] > 0, "Plant.Code"].values

        mask = curr_df['Plant.Code'].isin(curr_df['plant exported heat'].values)
        curr_df['Plant with exported heat'] = 0
        curr_df.loc[mask, 'Plant with exported heat'] = 1
        col_list.append('Plant with exported heat')

        #ST calc (EK-EV)
        mask = curr_df['general_mover'].isin(['ST'])
        for mon in months:
            nm = "ST_CD_" + mon.lower()
            curr_df[nm] = curr_df["steamHeat_"+mon.lower()] - 3.412142 * curr_df["NetGen_"+mon.lower()]-curr_df["nomLoss_"+mon.lower()]
            curr_df.loc[~mask, nm] = 0
            col_list.append(nm)

       # NGCC_CD calc  (EW-FH) =IF($EH2="NGCC",IFERROR(FuelHeat_january-NetGen_january*3.412142-nomLoss_january,0),0)
        mask = curr_df['general_mover'].isin(['NGCC'])
        for mon in months:
            nm = "NGCC_CD_" + mon.lower()
            curr_df[nm] = curr_df["FuelHeat_" + mon.lower()] - 3.412142 * curr_df["NetGen_" + mon.lower()] - curr_df[
                "nomLoss_" + mon.lower()]
            curr_df.loc[~mask, nm] = 0
            col_list.append(nm)

        # Nuke calc (FI-FT)  =IF($EH2="NA^ST",IFERROR(Q2-AC2*3.414124-Q2/50,0),0)
        mask = curr_df['Gen_Cat']=='NA^ST'
        for mon in months:
            nm = "Nuke_CD_" + mon.lower()
            curr_df[nm] = curr_df["FuelHeat_" + mon.lower()] - 3.414124 * curr_df["NetGen_" + mon.lower()] - (curr_df[
                "FuelHeat_" + mon.lower()])/50.0
            curr_df.loc[~mask, nm] = 0
            col_list.append(nm)

        # Bad_CD calc (FU-GF)  =IF(ABS(1-(MAX(Nuke_CD_january,NGCC_CD_january,ST_CD_january))/CD_january))>0.002,1,0)
        for mon in months:
            nm = "Bad_CD_" + mon.lower()
            s = curr_df[["Nuke_CD_" + mon.lower(), "NGCC_CD_" + mon.lower(), "ST_CD_" + mon.lower()]]
            #s.fillna(0, inplace=True)
            curr_df[nm] = s.max(axis=1)
            curr_df[nm] = curr_df[nm]/curr_df["CD_" + mon.lower()]
            curr_df[nm] = 1.0-curr_df[nm]
            curr_df[nm] = curr_df[nm].abs()
            mask = curr_df[nm].gt(0.002)
            curr_df.loc[mask, nm] = 1
            curr_df.loc[~mask, nm] = np.nan
            col_list.append(nm)
            # Bad_month calc (GG-GR)  =MAX(CK2,CW2,DI2,DU2,$EJ2,FU2)
        for mon in months:
            nm = "Bad_month_" + mon.lower()
            s = curr_df[["flag_HighTE_" + mon.lower(), "flag_ZeroFH_ZeroNetGen_" + mon.lower(), "Flag_gen_no_fuel_" + mon.lower(),
                         "Flag_fuel_no_gen_" + mon.lower(), "Plant with exported heat", "Bad_CD_" + mon.lower()]]
            #s.fillna(0, inplace=True)
            curr_df[nm] = s.max(axis=1)
            col_list.append(nm)
            # good_CD calc (GS-HD)  =IF(AND(Bad_month=0,ISNUMBER(E2)),E2,0)
        for mon in months:
            nm = "good_CD_" + mon.lower()
            curr_df[nm] = 0.0
            mask = (curr_df["Bad_month_" + mon.lower()] == 0) & (curr_df["CD_" + mon.lower()] > 0)
            curr_df.loc[mask, nm] = curr_df["CD_" + mon.lower()]
            col_list.append(nm)
           # good_MWh calc (HE-HP)  =IF(AND(Bad_month_january=0,ISNUMBER(NetGen_january)),NetGen_january,0)
        for mon in months:
            nm = "good_MWh_" + mon.lower()
            curr_df[nm] = 0.0
            mask = (curr_df["Bad_month_" + mon.lower()] == 0) & (curr_df["NetGen_" + mon.lower()] > 0)
            curr_df.loc[mask, nm] = curr_df["NetGen_" + mon.lower()]
            col_list.append(nm)
           # CD/MWh calc (HQ-IB)  =IFERROR(good_CD_/good_MWh,0)
        for mon in months:
            nm = "CD/MWh_" + mon.lower()
            #mask = (curr_df["good_CD_" + mon.lower()] > 0) | (curr_df["good_MWh_" + mon.lower()] > 0)
            curr_df[nm] = curr_df["good_CD_" + mon.lower()]/curr_df["good_MWh_" + mon.lower()]
            mask = curr_df[nm].isna()
            curr_df.loc[mask, nm] = 0
            col_list.append(nm)

        #good annual CD/MWh (column IC)  =IFERROR(SUM(good_CD_Jan:HD2)/SUM(good_CD_Jan:HP2),0)
        nmM = "good annual CD/MWh"
        s = curr_df[exported_header2].sum(axis=1)
        t = curr_df[exported_header3].sum(axis=1)
        curr_df[nmM] = s / t
        curr_df.loc[curr_df[nmM].isna(), nmM]= 0.0
        for mon in months:
            nm = "good_CD_" + mon.lower()
            mask = curr_df[nm].isna()
            curr_df.loc[mask, nmM] = 0
            nm = "good_MWh_" + mon.lower()
            mask = curr_df[nm].isna()
            curr_df.loc[mask, nmM] = 0
        col_list.append(nmM)
          # good_Fuel_Heat calc  (ID-IO) =IF(AND(Bad_month=0,ISNUMBER(FuelHeat)),FuelHeat,0)
        for mon in months:
            nm = "good_Fuel_Heat_" + mon.lower()
            curr_df[nm] = 0
            mask = (curr_df["Bad_month_" + mon.lower()] == 0) & (curr_df["FuelHeat_" + mon.lower()] > 0)
            curr_df.loc[mask, nm] = curr_df["FuelHeat_" + mon.lower()]
            col_list.append(nm)
        # Good annual TE calc (IP)  =IFERROR(SUM(HE2:HP2)*3.412142/SUM(ID2:IO2),0)
        nmM = "Good annual TE"
        s = curr_df[exported_header3].sum(axis=1)
        t = curr_df[exported_header4].sum(axis=1)
        curr_df[nmM] = s*3.412142/t
        curr_df.loc[curr_df[nmM].isna(), nmM] = 0.0
        for mon in months:
            nm = "good_MWh_" + mon.lower()
            mask = curr_df[nm].isna()
            curr_df.loc[mask, nmM] = 0
            nm = "good_Fuel_Heat_" + mon.lower()
            mask = curr_df[nm].isna()
            curr_df.loc[mask, nmM] = 0
        col_list.append(nmM)
          # good_gas^NGCC_CD calc (IQ-JB)  =IF($Gen_Cat="gas^NGCC",good_CD_,0)
        mask = curr_df['Gen_Cat'] == gen
        for mon in months:
            nm = "good_" + gen + '_CD_' + mon.lower()
            #curr_df[nm] = 0
            curr_df[nm] = curr_df["good_CD_" + mon.lower()]
            curr_df.loc[~mask, nm] = 0
            col_list.append(nm)
         # good_gas^NGCC_MWh_ calc (JC)  =IF($Gen_Cat="gas^NGCC",good_MWh,0)
        mask = curr_df['Gen_Cat'] == gen
        for mon in months:
            nm = "good_" + gen + "_MWh_" + mon.lower()
            #curr_df[nm] = 0
            curr_df[nm] = curr_df["good_MWh_" + mon.lower()]
            curr_df.loc[~mask, nm] = 0
            col_list.append(nm)
         # gas^NGCC_CD/MWh  (JO) calc   =IFERROR(IQ2/JC2,0)
        for mon in months:
            nm = gen + "_CD/MWh_" + mon.lower()
            mask = curr_df["good_" + gen + "_MWh_" + mon.lower()].gt(0.0)
            curr_df.loc[mask, nm] = curr_df["good_" + gen + '_CD_' + mon.lower()]/\
                                    curr_df["good_" + gen + "_MWh_" + mon.lower()]
            col_list.append(nm)
        # Good gas^NGCC  annual CD/MWh (KA) calc  =IFERROR(SUM(IQ2:JB2)/SUM(JC2:JN2),0)
        nmM = "Good " + gen + "annual CD/MWh"
        s = curr_df[exported_header5].sum(axis=1)
        t = curr_df[exported_header6].sum(axis=1)
        curr_df[nmM] = s/t
        curr_df.loc[curr_df[nmM].isna(), nmM] = 0.0
        mask = curr_df['Gen_Cat'] == gen
        curr_df.loc[~mask, nmM] = 0
        for mon in months:
            nm = "good_" + gen + "_CD_" + mon.lower()
            mask = curr_df[nm].isna()
            curr_df.loc[mask, nmM] = 0
            nm = "good_" + gen + "_MWh_" + mon.lower()
            mask = curr_df[nm].isna()
            curr_df.loc[mask, nmM] = 0
        col_list.append(nmM)
         # good_gas^NGCC_Fuel_Heat_Jan (starts at KB) calc   =IF($$Gen_Cat="gas^NGCC",good_Fuel_Heat,0)
        mask = curr_df['Gen_Cat'] == gen
        for mon in months:
            nm = "good_" + gen + "_Fuel_Heat_" + mon.lower()
            curr_df[nm] = 0
            curr_df.loc[mask, nm] = curr_df["good_Fuel_Heat_" + mon.lower()]
            curr_df.loc[~mask, nm] = 0
            col_list.append(nm)
        # Good gas^NGCC annual TE (KN) calc =IFERROR(SUM(good_gas^NGCC_MWh:JM2)*3.412142/SUM(good_gas^NGCC_Fuel_Heat:KM2),0)
        nmM = "Good " + gen + " annual TE"
        curr_df.loc[nmM] = 0.0
        s = 3.412142*curr_df[exported_header6].sum(axis=1, skipna = True)
        t = curr_df[exported_header7].sum(axis=1, skipna = True)
        curr_df[nmM] = s/t
        curr_df.loc[curr_df[nmM].isna(), nmM] = 0.0
        for mon in months:
            nm = "good_" + gen + "_MWh_" + mon.lower()
            mask = curr_df[nm].isna()
            curr_df.loc[mask, nmM] = 0
            nm = "good_" + gen + "_Fuel_Heat_" + mon.lower()
            mask = curr_df[nm].isna()
            curr_df.loc[mask, nmM] = 0
        col_list.append(nmM)
        # good Fuel_Heat_Jan =IF($EH2="other^ST",ID2,0)
        mask = curr_df['Gen_Cat'] == gen
        for mon in months:
            nm = "good_" + gen + "_Fuel_Heat_" + mon.lower()
            #mask = curr_df['Gen_Cat'].isin([gen])
            curr_df[nm] = curr_df['good_Fuel_Heat_' + mon.lower()]
            curr_df.loc[~mask, nm] = np.nan
        col_list.append(nm)
        # good  steam heat Jan (LQ) calc =IF(good_other^ST_Fuel_Heat_Jan>0,steamHeat_january,0)
        for mon in months:
            nm = "good_" + gen + "_steam_heat_" + mon.lower()
            mask = curr_df['good_' + gen + '_Fuel_Heat_' + mon.lower()] > 0.0
            curr_df[nm] = curr_df['steamHeat_' + mon.lower()]
            curr_df.loc[~mask, nm] = np.nan
            mask = curr_df['Gen_Cat'] == gen
            curr_df.loc[~mask, nm] = 0
        col_list.append(nm)
        # other ^ ST weighted average BE =SUM(LR2:MC1664)/SUM(KB2:KM1664)
        df_temp = pd.DataFrame()
        #if gen == "other^ST":
        for mon in months:
            nm = "weighted_average_" + gen + "_BE_" + mon.lower()
            #index_1 = ['Row_1','Row_2']
            #df_temp.index = index_1
            #df_temp.loc['Row_1',nm] = curr_df["good_" + gen + "_steam_heat_" + mon.lower()].sum(axis=0)
            #df_temp.loc['Row_2',nm] = curr_df["good_" + gen + "_Fuel_Heat_" + mon.lower()].sum(axis=0)
            s = curr_df["good_" + gen + "_steam_heat_" + mon.lower()].sum(axis=0)
            t = curr_df["good_" + gen + "_Fuel_Heat_" + mon.lower()].sum(axis=0)
            df_be.loc['BE', gen + "_weighted_average_BE_" + mon.lower() + '_' + str(year)] = s/t #df_temp.loc['Row_1',nm]/df_temp.loc['Row_2',nm]
            df_be.loc['Month', gen + "_weighted_average_BE_" + mon.lower() + '_' + str(year)] = mon.lower()
            df_be.loc['Gen_type', gen + "_weighted_average_BE_" + mon.lower() + '_' + str(year)] = gen
            # generate summary results for each gen type and year
        #=SUM(JC2: JC1664)*3.412142 / SUM(KB2: KB1664) thermal_efficiency-Weighted_average_across_all_bogens
        #=SUM(IQ2:IQ1664)/SUM(JC2:JC1664) CD/MWh-Weighted_average_across_all_bogensbe
        sum_df = pd.DataFrame()
        sum_df['month'] = months
        sum_df.set_index('month', inplace = True)
        sum_df["gen_type"] = gen
        sum_df["year"] = year
        i=0
        mask = curr_df['Gen_Cat'] == gen
        for mon in months:
            i+=1
            nm = "thermal_efficiency-Weighted_average_across_all_bogens"
            s = 3.412142 * curr_df["good_" + gen + "_MWh_" + mon.lower()].sum(axis=0)
            t = curr_df["good_" + gen + "_Fuel_Heat_" + mon.lower()].sum(axis=0)
            sum_df.at[mon,nm] = s/t

            # for now use =IFERROR(BA2/ID2,""),"")  steamHeat_january/good_Fuel_Heat_Jan
            nm = "boiler_efficiency"
            #sum_df.at[mon, nm] = float('nan')
            #index_1 = ['Row_1']
            #df_temp.index = index_1
            #df_temp["steamHeat"] = curr_df["steamHeat_" + mon.lower()]
            df_temp['steamHeat'] = curr_df["steamHeat_" + mon.lower()]
            df_temp.loc[~mask,'steamHeat'] = 0
# next mask is because fuel heat is check for being good but stream heat is not.
            mask2 = curr_df["good_Fuel_Heat_" + mon.lower()] > 0
            df_temp.loc[~mask2, 'steamHeat'] = 0
            s = df_temp['steamHeat'].sum()
            df_temp["good_Fuel_Heat"] = curr_df["good_Fuel_Heat_" + mon.lower()]
            df_temp.loc[~mask,'good_Fuel_Heat'] = 0
            t = df_temp['good_Fuel_Heat'].sum()
            sum_df.at[mon, nm] = s / t

            nm = " Weighted Annual_CD/MWh"
            nm1 = "Sum-CD"
            nm2 = "Sum-MWh"
            s = curr_df["good_" + gen + "_CD_" + mon.lower()].sum(axis=0)
            t = curr_df["good_" + gen + "_MWh_" + mon.lower()].sum(axis=0)
            sum_df.at[mon,nm1] = s
            sum_df.at[mon,nm2] = t
            d = datetime.datetime(int(year), i, 1)
            sum_df.at[mon, 'date'] = d
        all_dfs.append(sum_df.copy())
        M = pd.concat(all_dfs)# Sum across years
        year_mask = year_df['year'] == year
        year_df.loc[year_mask, nm1] = sum_df[nm1].sum(axis=0)
        year_df.loc[year_mask,nm2] = sum_df[nm2].sum(axis=0)
        year_df.loc[year_mask,nm] = year_df[nm1]/year_df[nm2]

# create annual averages for CD/MWh and TE

        #curr_df.to_csv(os.path.join(data_dir, 'large_file_' + gen + str(year) + '.csv'))
        #M.to_csv(os.path.join(data_dir, 'Summary_' + gen + '.csv'))
        year_df.to_csv(os.path.join(data_dir, 'Annual_Summary_' + gen + '.csv'))
# create dataframe with year and boiler efficiency
'''
    with styles.USGSMap():
        fig, axis = plt.subplots(1, 1, figsize=(10, 10))
        plt.rcParams.update({'font.size': 100})
        axis.set_xlabel("Month")
        axis.set_ylabel("Boiler Efficiency")
        axis.set_title(gen)
        x = 'Month'
        y = 'BE'
        axis.plot(x, y, data=df_be, color='r', linewidth=1.5)
        #axis.xaxis.set_major_locator(mdates.MonthLocator(bymonth=(1)))
        handles, labels = axis.get_legend_handles_labels()
        axis.legend(handles, labels)
        plt.tight_layout()
        plt.savefig('boiler_efficiency_'+ gen, bbox_inches='tight', facecolor='w')
'''
df_be = df_be.transpose()
df_be_ave = df_be.groupby(['Gen_type','Month']).mean()
df_be_ave = df_be_ave.reset_index()
# df_be['year'] = df_be.index.str[-4:].astype(int)
# df_be.rename(columns = {0:'BE'}, inplace = True)
'''
fig, ax = plt.subplots(3,3)
i = 0
j = 0
for type in df_be_ave['Gen_type'].unique():
    print(i)
    if j > 2:
        i = i + 1
        j = 0
    vv_ = df_be_ave[df_be_ave['Gen_type'].isin([type])]
    ax[i][j].plot(vv_['Month'], vv_['BE'])
    ax[i][j].set_title(type)
    ax[i][j].set_xticklabels(ax[i][j].get_xticks(), rotation = 45)
    j = j + 1
'''
###########################################
# Code below here is for plotting and was used for debugging
# create time series
    #with styles.USGSMap():
        #fig, axis = plt.subplots(4, 1, figsize=(10, 20))
        #plt.rcParams.update({'font.size': 100})
        #axis[0].set_xlabel("Date")
        #axis[0].set_ylabel("CD/MWh")
        #axis[0].set_title('CD/MWh_' + gen)
        #x = pd.DataFrame({'date'})
        #x = 'date'
        #y1 = 'CD/MWh-Weighted_average_across_all_bogens'
        #axis[0].plot(x, y1, data = M, color='r', linewidth=1.5, label='Weighted average CD/MWh')
        #axis[0].xaxis.set_major_locator(mdates.MonthLocator(bymonth=(1)))
        #handles, labels = axis[0].get_legend_handles_labels()
        #axis[0].legend(handles, labels)
# second plot
        #axis[1].set_xlabel("Date")
        #axis[1].set_ylabel("thermal_efficiency")
        #axis[1].set_title('thermal_efficiency_' + gen)
        #y2 = 'thermal_efficiency-Weighted_average_across_all_bogens'
        #axis[1].plot(x, y2, data=M, color='r', linewidth=1.5, label='Weighted average thermal efficiency')
        #axis[1].xaxis.set_major_locator(mdates.MonthLocator(bymonth=(1)))
        #handles, labels = axis[1].get_legend_handles_labels()
        #axis[1].legend(handles, labels)
# third plot
        #axis[2].set_xlabel("thermal_efficiency")
        #axis[2].set_ylabel("CD/MWh")
        #axis[2].set_title('TE/(CD/MWh)_' + gen)
        #y3 = 'CD/MWh-Weighted_average_across_all_bogens'
        #axis[2].scatter(y2, y3, data=M, color='r', label='TE vs. CD/MWh')
        #handles, labels = axis[2].get_legend_handles_labels()
        #axis[2].legend(handles, labels)
        # forth plot
        #axis[3].set_xlabel("date")
        #axis[3].set_ylabel("boiler efficiency")
        #axis[3].set_title('boiler_efficiency' + gen)
        #y4 = 'boiler_efficiency'
        #axis[3].plot(x, y4, data=M, color='r', linewidth=1.5, label='boiler_efficiency')
        #axis[3].xaxis.set_major_locator(mdates.MonthLocator(bymonth=(1)))
        #handles, labels = axis[3].get_legend_handles_labels()
        #axis[3].legend(handles, labels)

        #plt.tight_layout()
        #plt.savefig('boiler_efficiency' + gen, bbox_inches='tight', facecolor='w')
    #if 0:   #supress debug files
    #if gen == 'other^ST':