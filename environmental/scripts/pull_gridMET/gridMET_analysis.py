# -*- coding: utf-8 -*-
"""
Created on Fri Aug  5 08:44:14 2022

@author: galanter
"""

#gridMET analysis
#%%
mean_plot = pd.merge(month_means, TE_df, on = 'EIA_PLANT_')
mean_plot.drop(columns = ['NAME_OF_WA','COMID', 'COOLING_TY','GENERATION', 'WATER_SOUR', 'WATER_TYPE', 
                         'WITHDRAWAL', 'CONSUMPTIO','MIN_WITHDR', 'MAX_WITHDR', 'MIN_CONSUM', 'MAX_CONSUM', 
                         'NET_GENERA','geometry'], inplace =True)

geometry = [Point(xy) for xy in zip(mean_plot['LONGITUDE'],mean_plot['LATITUDE'])]
geometry[:3]

mean_plot.drop(columns = ['why_no_CID'], inplace = True)
crs = {'init': 'epsg:4326'}
mean_df = gpd.GeoDataFrame(mean_plot, crs = crs, geometry= geometry)

np.linspace(1,12,12)
months = ['01','02','03','04', '05', '06', '07', '08', '09', '10', '11', '12']

var_dict = {'month': months,'DB': months, 'WB': months, 'WS': months, 'EV': months}
var_df = pd.DataFrame(data = var_dict)


#%%
params = ['DB']
units = ['deg_C']

with PdfPages('DB' + '.pdf') as pdf:
    for i, val in enumerate(months):
        monthly_df = mean_df[mean_df['MONTH'] == val]
        fig, (ax1,ax2) = plt.subplots(nrows = 2, figsize = (8,8))
        col_1 = params
        col_2 = params
        ax1.set_title(val + '_' + params)

        monthly_df.plot(ax = ax1, column = col_1, legend = True, 
                        cmap = 'gist_rainbow')
        ctx.add_basemap(ax1, crs = crs, source = ctx.providers.OpenTopoMap)
        
        ax2.set_title(val + params)
        ax2.hist(monthly_df['air_tmp_C_avg'])
        pdf.savefig()
        plt.clf()
