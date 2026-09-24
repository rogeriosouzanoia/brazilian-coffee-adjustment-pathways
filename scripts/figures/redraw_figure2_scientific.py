"""Figure 2 for coauthor review: panel-specific area and change keys, no zero-share background points."""
from pathlib import Path
import json
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

ROOT=Path(__file__).resolve().parents[2]
BASE=ROOT/'figures'
SRC=ROOT/'data/figure_inputs'
D=pd.read_csv(SRC/'municipal_area_share_periods.csv')
geo=json.loads((SRC/'brazil_boundary_naturalearth_110m.geojson').read_text())['features'][0]['geometry']
outline=np.array(geo['coordinates'][0]); bx,by=outline.T
A='#315a70'; C='#627447'; GAIN='#72539a'; LOSS='#b65d37'; DARK='#334147'
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':15,'axes.labelsize':15,'axes.titlesize':17,
 'xtick.labelsize':13,'ytick.labelsize':13,'axes.spines.top':False,'axes.spines.right':False,
 'savefig.facecolor':'white','figure.constrained_layout.w_pad':.15,'figure.constrained_layout.h_pad':.17})

# Matplotlib interprets scatter s as marker area in points squared; no size clipping.
def s_share(fraction): return 6+3800*np.asarray(fraction)
def s_change(percentage_points): return 6+120*np.asarray(percentage_points)
def key(color,value,size,label,filled=True):
 return Line2D([0],[0],ls='none',marker='o',ms=np.sqrt(size(value)),
               mfc=color if filled else 'white',mec=color,mew=1.0,label=label)
def bg(ax):
 ax.fill(bx,by,color='#f6f7f6',zorder=0)
 ax.plot(bx,by,color='#536267',lw=1.05,zorder=4)
 ax.set(xlim=(-75,-33),ylim=(-35,7),xticks=[-70,-60,-50,-40],yticks=[-30,-20,-10,0])
 ax.set_aspect('equal',adjustable='box');ax.set_axisbelow(True)
 ax.grid(color='#e4e9e9',lw=.6);ax.set_xlabel('Longitude (°)');ax.set_ylabel('Latitude (°)')
 ax.spines['left'].set_linewidth(.9);ax.spines['bottom'].set_linewidth(.9)
 ax.tick_params(direction='out',length=4,width=.8)
fig=plt.figure(figsize=(13.2,19.5),layout='constrained')
outer=fig.add_gridspec(3,2,wspace=.12,hspace=.14)
axs=np.empty((3,2),dtype=object)
keys=np.empty((3,2),dtype=object)
for row in range(3):
 for col in range(2):
  cell=outer[row,col].subgridspec(2,1,height_ratios=[7,1.0],hspace=.02)
  axs[row,col]=fig.add_subplot(cell[0])
  keys[row,col]=fig.add_subplot(cell[1]);keys[row,col].axis('off')
for row,(species,color) in enumerate([('Arabica',A),('Canephora',C)]):
 subset=D.loc[D.species==species]
 for j,period in enumerate(['2012–2015','2021–2024']):
  ax=axs[row,j]; bg(ax)
  d=subset.loc[(subset.period==period)&(subset.share>0)].dropna(subset=['lon','lat','share'])
  ax.scatter(d.lon,d.lat,s=s_share(d.share),color=color,alpha=.84,linewidths=0,rasterized=True,zorder=3)
  ax.set_title(f"{'abcd'[row*2+j]}  {species} · {period}",loc='left',fontweight='bold',pad=14)
  values=[.001,.005,.02] if species=='Arabica' else [.001,.01,.04]
  labels=['0.1%','0.5%','2%'] if species=='Arabica' else ['0.1%','1%','4%']
  handles=[key(color,v,s_share,lab) for v,lab in zip(values,labels)]
  keys[row,j].legend(handles=handles,title='National area share',ncol=3,
            loc='center',fontsize=12,title_fontsize=12,frameon=False,
            handletextpad=.5,columnspacing=1.4)
 # Missing period shares are not recoded to zero; only measured pairs enter change maps.
 wide=subset.pivot(index='municipality_code',columns='period',values='share').dropna()
 coords=subset[['municipality_code','lon','lat']].drop_duplicates('municipality_code')
 d=((wide['2021–2024']-wide['2012–2015'])*100).rename('change_pp').reset_index().merge(coords,on='municipality_code').dropna(subset=['lon','lat'])
 ax=axs[2,row]; bg(ax)
 for mask,col in [(d.change_pp<0,LOSS),(d.change_pp>0,GAIN)]:
  v=d.loc[mask]; ax.scatter(v.lon,v.lat,s=s_change(np.abs(v.change_pp)),
                             color=col,alpha=.85,linewidths=0,rasterized=True,zorder=3)
 ax.set_title(f"{'ef'[row]}  {species} · area-share change",loc='left',fontweight='bold',pad=14)
 color_handles=[Line2D([0],[0],ls='none',marker='o',ms=6,mfc=LOSS,mec=LOSS,label='Loss'),
                Line2D([0],[0],ls='none',marker='o',ms=6,mfc=GAIN,mec=GAIN,label='Gain')]
 lax=keys[2,row]
 leg=lax.legend(handles=color_handles,title='Direction',ncol=2,
               loc='center left',bbox_to_anchor=(0,.50),fontsize=12,
               title_fontsize=12,frameon=False,columnspacing=.65,handletextpad=.35)
 lax.add_artist(leg)
 values=[.05,.2,.5] if species=='Arabica' else [.05,.5,1.0]
 size_handles=[key(DARK,v,s_change,f'{v:g} pp',filled=False) for v in values]
 lax.legend(handles=size_handles,title='Absolute change (pp)',ncol=3,
            loc='center right',bbox_to_anchor=(1,.50),fontsize=12,
            title_fontsize=12,frameon=False,columnspacing=.55,handletextpad=.3)
for ext in ('png','pdf'):
 fig.savefig(BASE/f'Figure2_geographic_reorganization_coauthor_final.{ext}',
             dpi=360 if ext=='png' else None,bbox_inches='tight',pad_inches=.18)
plt.close(fig)
print('Figure 2 revised; municipality counts by species and period:')
for (sp,pr),g in D.groupby(['species','period']):print(sp,pr,(g.share>0).sum())
