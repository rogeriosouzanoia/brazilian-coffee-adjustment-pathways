"""Rebuild the coffee manuscript's figures from archived project CSV outputs."""
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

SRC=Path(__file__).resolve().parents[2]/'data'/'figure_inputs'; OUT=Path(__file__).resolve().parents[2]/'figures'; OUT.mkdir(exist_ok=True)
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

series=read('national_series_2012_2024')
fig,axs=plt.subplots(2,2,figsize=(13,9.2),layout='constrained')
for ax,(col,title,ylab,scale),lab in zip(axs.flat,[('production_t','National production','Million tonnes',1e6),('area_harvested_ha','Harvested area','Million hectares',1e6),('productivity_kg_ha','Aggregate productivity','kg ha⁻¹',1)],'abc'):
 for sp,color,mark in [('Arabica',A,'o'),('Canephora',C,'s')]:
  d=series[series.species==sp];ax.plot(d.year,d[col]/scale,color=color,marker=mark,ms=5.2,lw=2)
 panel(ax,lab,title);ax.set(xlabel='Year',ylabel=ylab,xticks=[2012,2015,2018,2021,2024]);axis(ax)
ax=axs[1,1];panel(ax,'d','Exact production-change decomposition')
names=['Total area','Redistribution','Within-municipality productivity','Entry','Exit']; ys=np.arange(5)
shapley_table=read('shapley_decomposition_from_supplement').set_index('species')
shapley={sp:shapley_table.loc[sp,['area','redistribution','within_municipality_productivity','entry','exit']].tolist() for sp in ['Arabica','Canephora']}
for sp,col,off in [('Arabica',A,-.105),('Canephora',C,.105)]:
 vals=shapley[sp]
 ax.barh(ys+off,vals,height=.17,color=col,alpha=.94)
ax.axvline(0,color=DARK,lw=.7);ax.set(yticks=ys,yticklabels=names,xlabel='Percentage points of baseline production',xlim=(-26,69));ax.invert_yaxis();axis(ax)
fig.legend(handles=legend,loc='outside upper center',ncol=2,frameon=False)
save(fig,'Figure1_adjustment_pathways.png')

period=read('municipal_area_share_periods')
cross=period[['municipality_code','lon','lat']].drop_duplicates('municipality_code')
def geo(ax):
 ax.set(xlim=(-75,-33),ylim=(-35,7),xticks=[-70,-60,-50,-40],yticks=[-30,-20,-10,0]);ax.set_aspect('equal',adjustable='box')
 ax.grid(color='#e2e7e9',lw=.6);axis(ax,False)
 ax.set_xlabel('Longitude (°)');ax.set_ylabel('Latitude (°)')
# Two columns give each map twice the print width of the earlier six-across grid.
fig,axs=plt.subplots(3,2,figsize=(12.8,16.5),layout='constrained')
for row,(sp,col) in enumerate([('Arabica',A),('Canephora',C)]):
 subset=period[period.species==sp]
 for j,per in enumerate(['2012–2015','2021–2024']):
  ax=axs[row,j];d=subset[subset.period==per].dropna(subset=['lon','lat']);
  ax.scatter(d.lon,d.lat,s=np.clip(5+6500*d.share,4,105),color=col,alpha=.62,linewidths=0,rasterized=True)
  panel(ax,'abcd'[row*2+j],f'{sp} · {per}');geo(ax)
 piv=subset.pivot(index='municipality_code',columns='period',values='share').fillna(0)
 delta=(piv.get('2021–2024',0)-piv.get('2012–2015',0))*100
 d=delta.rename('change_pp').reset_index().merge(cross,on='municipality_code').dropna(subset=['lon','lat'])
 ax=axs[2,row];s=np.clip(6+180*np.abs(d.change_pp),5,115)
 ax.scatter(d.loc[d.change_pp<0,'lon'],d.loc[d.change_pp<0,'lat'],s=s[d.change_pp<0],color=RED,alpha=.65,linewidths=0,rasterized=True)
 ax.scatter(d.loc[d.change_pp>0,'lon'],d.loc[d.change_pp>0,'lat'],s=s[d.change_pp>0],color=BLUE,alpha=.65,linewidths=0,rasterized=True)
 panel(ax,'ef'[row],f'{sp} · area-share change');geo(ax)
fig.legend(handles=[Line2D([0],[0],marker='o',color='none',markerfacecolor=RED,label='Share loss'),Line2D([0],[0],marker='o',color='none',markerfacecolor=BLUE,label='Share gain')],loc='outside lower center',ncol=2,frameon=False)
save(fig,'Figure2_geographic_reorganization.png')

climate=read('fixed_geography_climate_summary_from_manuscript').set_index('metric')
fig,axs=plt.subplots(2,2,figsize=(13,10),layout='constrained')
for ax,vals,title,ylab,lab in zip([axs[0,0],axs[0,1],axs[1,0]],[climate.loc[m,['Arabica','Canephora']].astype(float).tolist() for m in ['recent_temperature_change_c','recent_hot_days_change','long_term_temperature_c_per_decade']],['Recent warming','Recent hot-day increase','Long-term warming · 1981–2024'],['Change in °C','Change in days yr⁻¹','°C per decade'],'abc'):
 ax.bar([0,1],vals,color=[A,C],width=.28);panel(ax,lab,title)
 for xx,v in enumerate(vals):ax.text(xx,v+max(vals)*.035,f'{v:.2f}' if max(vals)<1 else f'{v:.1f}',ha='center',fontweight='bold',fontsize=16,color=DARK)
 ax.set(xticks=[0,1],xticklabels=['Arabica','Canephora'],ylabel=ylab,ylim=(0,max(vals)*1.28));axis(ax)
water=read('coffee_water_stress_scenario_sensitivity');ax=axs[1,1];panel(ax,'d','Rainfed water satisfaction');x=np.arange(4)
for sp,col,off,mark in [('Arabica',A,-.12,'o'),('Canephora',C,.12,'s')]:
 d=water[water.species==sp].set_index('window_id').loc[['W1','W2','W3','W4']]
 val=d.central_change_pp.values;lo=d.scenario_minimum_change_pp.values;hi=d.scenario_maximum_change_pp.values
 ax.errorbar(x+off,val,yerr=[val-lo,hi-val],fmt=mark,color=col,capsize=3,lw=1.3,ms=5)
ax.axhline(0,color=DARK,lw=.8);ax.set(xticks=x,xticklabels=['W1','W2','W3','W4'],ylabel='Change in ETr/ETc (percentage points)',xlabel='Calendar window');axis(ax)
fig.legend(handles=legend,loc='outside upper center',ncol=2,frameon=False)
save(fig,'Figure3_climate_pressure.png')

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
fig.legend(handles=[Line2D([0],[0],marker='o',color='none',markerfacecolor=DARK,label='Mean temperature'),Line2D([0],[0],marker='s',color='none',markerfacecolor=DARK,label='Hot days')],loc='outside lower center',ncol=2,frameon=False)
save(fig,'Figure4_environmental_redistribution.png')

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
fig.legend(handles=legend+[Line2D([0],[0],marker='o',lw=0,color=DARK,label='Rainfall +100 mm'),Line2D([0],[0],marker='^',lw=0,color=DARK,label='Water satisfaction −0.10')],loc='outside upper center',ncol=2,frameon=False,fontsize=14)
save(fig,'Figure5_water_productivity.png')

# A linear methodology diagram with independent data streams and labelled estimands.
fig,ax=plt.subplots(figsize=(12,6.4),layout='constrained');ax.set(xlim=(0,12),ylim=(0,6.4));ax.axis('off')
def box(x,y,w,h,title,detail,fc='#f2f5f6'):
 p=FancyBboxPatch((x,y),w,h,boxstyle='round,pad=.06,rounding_size=.12',facecolor=fc,edgecolor='#4b626e',linewidth=.9)
 ax.add_patch(p);ax.text(x+.18,y+h-.18,title,fontweight='bold',ha='left',va='top',fontsize=16)
 ax.text(x+.18,y+h-.54,detail,ha='left',va='top',fontsize=13,color='#41505a')
def arrow(x1,y1,x2,y2):ax.add_patch(FancyArrowPatch((x1,y1),(x2,y2),arrowstyle='-|>',mutation_scale=12,lw=1.1,color='#667a82'))
box(.4,4.95,3.35,1.08,'Coffee production','IBGE PAM · species · 2012–2024')
box(4.32,4.95,3.35,1.08,'Climate histories','NASA POWER · 1981–2024')
box(8.24,4.95,3.35,1.08,'Spatial sensitivity','Municipal elevation · coffee pixels')
box(3.95,3.38,4.1,1.02,'Linked municipal panel','IBGE code · yield · reference points','#e8eff2')
ax.plot([2.1,9.9],[4.73,4.73],color='#667a82',lw=1)
for x in [2.1,6,9.9]:arrow(x,4.95,x,4.73)
arrow(6,4.73,6,4.43)
box(.3,1.58,3.5,1.04,'Production pathway','Exact Shapley decomposition')
box(4.25,1.58,3.5,1.04,'Exposure and location','Fixed weights · water balance')
box(8.2,1.58,3.5,1.04,'Productivity tests','Associations · temporal holdout')
ax.plot([2.05,9.95],[2.95,2.95],color='#667a82',lw=1)
arrow(6,3.38,6,2.95)
for x in [2.05,6,9.95]:arrow(x,2.95,x,2.64)
box(2.45,.12,7.1,.82,'Interpretation','Account for output · characterize exposure · test prediction','#e8eff2')
ax.plot([2.05,9.95],[1.14,1.14],color='#667a82',lw=1)
for x in [2.05,6,9.95]:arrow(x,1.58,x,1.14)
arrow(6,1.14,6,.99)
save(fig,'FigureS1_analytical_workflow.png')

# The verified annual history supersedes the older summary-only Figure 3
# constructed above. Keep that older code for provenance while making this
# single command reproduce exactly the figure in the current manuscript.
import runpy
runpy.run_path(str(Path(__file__).with_name('figure03_verified_history.py')))
print('Five current main figures and Supplementary Figure S1 generated.')
