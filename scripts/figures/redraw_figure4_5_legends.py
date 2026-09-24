"""Rebuild the coffee manuscript's figures from archived project CSV outputs."""
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

ROOT=Path(__file__).resolve().parents[2]
SRC=ROOT/'data/figure_inputs'; OUT=ROOT/'figures'
A='#315a70'; C='#69784a'; BLUE='#497f9c'; RED='#ba654e'; DARK='#26323a'; LIGHT='#b9c5c8'
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':16,'axes.labelsize':16,
 'axes.labelweight':'medium','axes.titlesize':19,'xtick.labelsize':14,'ytick.labelsize':14,
 'legend.fontsize':15,'figure.constrained_layout.w_pad':.12,'figure.constrained_layout.h_pad':.14,
 'axes.spines.top':False,'axes.spines.right':False,'savefig.facecolor':'white'})
def read(stem): return pd.read_csv(SRC/(stem+'.csv'))
def panel(ax,label,title):
 ax.set_title(f'{label}   {title}',loc='left',fontweight='bold',pad=16)
def axis(ax,grid=True):
 ax.spines['left'].set_visible(True);ax.spines['bottom'].set_visible(True)
 ax.spines['left'].set_linewidth(1.05);ax.spines['bottom'].set_linewidth(1.05)
 ax.tick_params(direction='out',length=4,width=.85)
 if grid:ax.grid(axis='y',alpha=.16,lw=.65)
def save(fig,name):
 fig.savefig(OUT/name,dpi=360,bbox_inches='tight',pad_inches=.22)
 fig.savefig(OUT/name.replace('.png','.pdf'),bbox_inches='tight',pad_inches=.22)
 plt.close(fig)
legend=[Line2D([0],[0],color=A,lw=2,marker='o',label='Arabica'),Line2D([0],[0],color=C,lw=2,marker='s',label='Canephora')]

bands=read('coffee_elevation_band_distribution');shifts=read('coffee_elevation_shift_summary');redis=read('coffee_redistribution_climatology_summary')
fig,axs=plt.subplots(2,2,figsize=(12.5,9.2),layout='constrained')
for ax,sp,lab in [(axs[0,0],'Arabica','a'),(axs[0,1],'Canephora','b')]:
 d=bands[(bands.species==sp)&(bands.weighting=='Harvested_area')&bands.elevation_band.notna()]
 cats=d.elevation_band.drop_duplicates().tolist();x=np.arange(len(cats))
 for per,col,mark in [('2012 to 2015',LIGHT,'o'),('2021 to 2024',A if sp=='Arabica' else C,'s')]:
  vals=d[d.period==per].set_index('elevation_band').reindex(cats).share_of_weight.values*100
  ax.plot(x,vals,color=col,lw=1.9,marker=mark,ms=5,label=per.replace(' to ','–'))
 panel(ax,lab,f'{sp} · elevation bands');ax.set(xticks=x,xticklabels=cats,ylabel='Share of harvested area (%)',xlabel='Municipal reference elevation band');ax.tick_params(axis='x',rotation=38);axis(ax);ax.legend(frameon=False,fontsize=13)
ax=axs[1,0];panel(ax,'c','Area-weighted municipal elevation');d=shifts[(shifts.weighting=='Harvested_area')&shifts.species.isin(['Arabica','Canephora'])]
for i,sp in enumerate(['Arabica','Canephora']):
 r=d[d.species==sp].iloc[0];v0=r['weighted_mean_elevation_m__2012 to 2015'];v1=r['weighted_mean_elevation_m__2021 to 2024'];col=A if i==0 else C
 dv=v1-v0
 ax.plot([0,dv],[i,i],color=col,lw=2.2);ax.scatter(dv,i,color=col,s=90,zorder=3)
 ax.text(dv+1.0,i,f'+{dv:.1f} m',va='center',color=col,fontweight='bold',fontsize=16)
ax.axvline(0,color=DARK,lw=.8);ax.set(yticks=[0,1],yticklabels=['Arabica','Canephora'],xlabel='Change in weighted reference elevation (m)',xlim=(-2,36));ax.invert_yaxis();axis(ax)
ax=axs[1,1];panel(ax,'d','Shift in fixed historical climate');d=redis[(redis.weighting=='Harvested_area')&redis.species.isin(['Arabica','Canephora'])]
for i,sp in enumerate(['Arabica','Canephora']):
 for j,(metric,fac,marker) in enumerate([('tmean_c',1,'o'),('hot_days_ge32',.05,'s')]):
  v=d[(d.species==sp)&(d.metric==metric)].redistribution_only_change.iloc[0]
  ax.scatter(v*fac,i+(j-.5)*.3,color=A if sp=='Arabica' else C,marker=marker,s=75)
  ax.text(v*fac+.022,i+(j-.5)*.3,f'{v:+.2f} °C' if j==0 else f'{v:+.1f} days',va='center',fontsize=14,fontweight='bold')
ax.axvline(0,color=DARK,lw=.7);ax.set(yticks=[0,1],yticklabels=['Arabica','Canephora'],xlabel='Shift (hot days divided by 20 for display)',xlim=(-.5,.25));ax.invert_yaxis();axis(ax)
axs[1,1].legend(handles=[Line2D([0],[0],marker='o',color='none',markerfacecolor=DARK,label='Temperature'),Line2D([0],[0],marker='s',color='none',markerfacecolor=DARK,label='Hot days')],loc='upper center',bbox_to_anchor=(.5,-.30),ncol=2,frameon=False,fontsize=13)
save(fig,'Figure4_environmental_redistribution_coauthor_final.png')

water=read('coffee_water_stress_scenario_sensitivity')
assoc=read('coffee_seasonal_water_plot_data');val=read('coffee_water_temporal_model_comparison');mc=read('coffee_seasonal_water_model_comparison')
fig,axs=plt.subplots(2,2,figsize=(13,9.5),layout='constrained')
ax=axs[0,0];panel(ax,'a','Change in rainfed water satisfaction')
for sp,col,off,mark in [('Arabica',A,-.12,'o'),('Canephora',C,.12,'s')]:
 d=water[water.species==sp].set_index('window_id').loc[['W1','W2','W3','W4']];v=d.central_change_pp.values
 ax.errorbar(np.arange(4)+off,v,yerr=[v-d.scenario_minimum_change_pp.values,d.scenario_maximum_change_pp.values-v],fmt=mark,capsize=3,color=col,ms=5)
ax.axhline(0,color=DARK,lw=.7);ax.set(xticks=np.arange(4),xticklabels=['W1','W2','W3','W4'],ylabel='Change in ETr/ETc (percentage points)',xlabel='Calendar window');axis(ax)
ax=axs[0,1];panel(ax,'b','Conditional productivity associations')
d=assoc[(assoc.control=='Trend_parity_year')&(((assoc.family=='P')&(assoc.predictor=='rain'))|((assoc.family=='W')&(assoc.scenario_id=='CAD100_Kc095')&(assoc.predictor=='water')))]
for i,sp in enumerate(['Arabica','Canephora']):
 for j,pred in enumerate(['rain','water']):
  rows=d[(d.species==sp)&(d.predictor==pred)].set_index('window_id').reindex(['W1','W2','W3','W4'])
  y=np.arange(4)+(i-.5)*.16+(j-.5)*.08;v=rows.association_pct.values
  ax.errorbar(v,y,xerr=[v-rows.lower95_pct.values,rows.upper95_pct.values-v],fmt='o' if pred=='rain' else '^',color=A if i==0 else C,ms=5.5,capsize=3,lw=1.1)
ax.axvline(0,color=DARK,lw=.7);ax.set(yticks=range(4),yticklabels=['W1','W2','W3','W4'],xlabel='Conditional association with productivity (%)');ax.invert_yaxis();axis(ax,False)
ax=axs[1,0];panel(ax,'c','Chronological validation, 2020–2024')
d=val[(val.test_period=='POOLED_2020_2024')&(val.control=='Local_trend')]
for i,sp in enumerate(['Arabica','Canephora']):
 rows=d[d.species==sp].set_index('family').reindex(['H','P','W']);v=rows.mse_skill_vs_N.values*100
 ax.scatter(v,np.arange(3)+(i-.5)*.18,color=A if i==0 else C,marker='o' if i==0 else 's',s=72)
ax.axvline(0,color=DARK,lw=.7);ax.set(yticks=range(3),yticklabels=['Heat','Heat + rainfall','Heat + water balance'],xlabel='Squared-error improvement versus local history (%)');ax.invert_yaxis();axis(ax,False)
ax=axs[1,1];panel(ax,'d','Within-sample explanatory value')
d=mc[(mc.control=='Trend_parity_year')&(mc.family.isin(['H','P','W']))&((mc.family!='W')|(mc.scenario_id=='CAD100_Kc095'))]
for i,sp in enumerate(['Arabica','Canephora']):
 rows=d[d.species==sp].set_index('family').reindex(['H','P','W']);v=rows.partial_R2_beyond_nuisance.values*100
 ax.scatter(v,np.arange(3)+(i-.5)*.18,color=A if i==0 else C,marker='o' if i==0 else 's',s=72)
ax.set(yticks=range(3),yticklabels=['Heat','Heat + rainfall','Heat + water balance'],xlabel='Partial variance explained (%)');ax.invert_yaxis();axis(ax,False)
fig.legend(handles=[Line2D([0],[0],color=A,lw=2,label='Arabica'),Line2D([0],[0],color=C,lw=2,label='Canephora')],loc='outside upper center',ncol=2,frameon=False,fontsize=14)
axs[0,1].legend(handles=[Line2D([0],[0],marker='o',lw=0,color=DARK,label='Rain +100 mm'),Line2D([0],[0],marker='^',lw=0,color=DARK,label='ETr/ETc −0.10')],loc='upper center',bbox_to_anchor=(.5,-.24),ncol=2,frameon=False,fontsize=12.5)
save(fig,'Figure5_water_productivity_coauthor_final.png')
