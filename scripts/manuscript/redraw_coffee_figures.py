from pathlib import Path
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle, Patch
from matplotlib.lines import Line2D
from matplotlib.colors import LinearSegmentedColormap

BASE=Path('coffee_bundle/Figures')
OUT=Path('coffee_revision/figures')
OUT.mkdir(parents=True,exist_ok=True)
BLUE='#31566c'; GREEN='#64734b'; SKY='#588197'; GRAY='#9da4a9'; RED='#bc6047'; ENTRY='#9aaa7d'
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':9.5,'axes.spines.top':False,'axes.spines.right':False,'axes.labelsize':9.5,'xtick.labelsize':8.5,'ytick.labelsize':8.5})

def canvas(name):
    arr=plt.imread(BASE/name)
    h,w=arr.shape[:2]
    fig=plt.figure(figsize=(w/320,h/320),dpi=320,facecolor='white')
    bg=fig.add_axes([0,0,1,1]);bg.imshow(arr);bg.set_axis_off()
    return fig

def wipe(fig,x,y,w,h):fig.add_artist(Rectangle((x,y),w,h,transform=fig.transFigure,facecolor='white',edgecolor='none',zorder=2))
def heading(fig,x,y,title):fig.text(x,y,title,weight='bold',fontsize=11,va='top',zorder=5)
def style(ax,grid='x'):
    ax.grid(axis=grid,color='#e8ebed',linewidth=.6,zorder=0)
    ax.spines['left'].set_color('#333');ax.spines['bottom'].set_color('#333')
    ax.tick_params(length=3,color='#333')

# Figure 1. Exact signed Shapley contributions, with the legend clear of bars.
fig=canvas('Figure1_adjustment_pathways.png')
wipe(fig,.445,0,.555,.52)
heading(fig,.515,.475,'d   Exact decomposition of production change')
ax=fig.add_axes([.54,.105,.425,.272],zorder=4)
rows=[('Arabica',[-5.12,4.17,3.67,.09,-.47],2.35),('Canephora',[-21.57,2.39,62.66,.38,-.29],43.56)]
colors=[GRAY,SKY,BLUE,ENTRY,RED]
for j,(_,v,net) in enumerate(rows):
    neg=0;pos=0
    for value,c in zip(v,colors):
        if value<0:
            ax.barh(j,value,left=neg,height=.36,color=c,edgecolor='white',linewidth=.3,zorder=3);neg+=value
        else:
            ax.barh(j,value,left=pos,height=.36,color=c,edgecolor='white',linewidth=.3,zorder=3);pos+=value
    ax.text(67,j,f'Observed +{net:.2f}%',ha='right',va='center',fontsize=8.3,weight='bold',bbox=dict(facecolor='white',edgecolor='none',pad=1),zorder=6)
ax.set(yticks=[0,1],yticklabels=['Arabica','Canephora'],xlim=(-25,69),xlabel='Contribution to baseline production (percentage points)')
ax.invert_yaxis();ax.axvline(0,color='#555',lw=.8);style(ax)
handles=[Patch(color=c,label=l) for c,l in zip(colors,['Area','Redistribution','Productivity','Entry','Exit'])]
fig.legend(handles=handles,loc='center',bbox_to_anchor=(.74,.405),ncol=3,frameon=False,fontsize=7.9,columnspacing=.9,handlelength=1.2)
fig.savefig(OUT/'Figure1_adjustment_pathways.png',dpi=320,facecolor='white');plt.close(fig)

# Figure 3. Redraw central scenario water satisfaction without claiming unrecorded range endpoints.
fig=canvas('Figure3_climate_pressure.png')
wipe(fig,.42,0,.58,.577)
grad=LinearSegmentedColormap.from_list('heat_difference',['#b97661','#f7f7f6','#81aabb'])
cbar=fig.add_axes([.603,.563,.195,.012],zorder=6)
cbar.imshow(np.linspace(0,1,256)[None,:],aspect='auto',cmap=grad,extent=(-75,75,0,1))
cbar.set_yticks([]);cbar.set_xticks([-75,-50,-25,0,25,50,75]);cbar.tick_params(labelsize=7,length=2,pad=2)
for spine in cbar.spines.values():spine.set_visible(True);spine.set_linewidth(.5)
fig.text(.70,.515,'Days yr⁻¹ with Tmax ≥ 32 °C',ha='center',va='center',fontsize=8.5,weight='bold',zorder=6)
heading(fig,.48,.475,'d   Seasonal rainfed water satisfaction')
ax=fig.add_axes([.525,.12,.44,.29],zorder=4)
x=np.arange(4);ab=[3.6,-11.0,2.7,9.4];ca=[-.4,-8.4,-6.8,9.4]
ax.plot(x-.11,ab,'o-',color=BLUE,lw=1.5,ms=5,label='Arabica',zorder=3)
ax.plot(x+.11,ca,'s--',color=GREEN,lw=1.5,ms=5,label='Canephora',zorder=3)
ax.set(xticks=x,xticklabels=['W1','W2','W3','W4'],ylim=(-13,12),ylabel='Change in ETr/ETc (percentage points)',xlabel='Calendar window')
ax.axhline(0,color='#777',lw=.8);ax.legend(loc='upper center',ncol=2,frameon=False,fontsize=8);style(ax,'y')
fig.text(.54,.035,'Central CAD = 100 mm, Kc = 0.95; scenario sensitivity in Supplementary Fig. S6.',fontsize=7.7,color='#444')
fig.savefig(OUT/'Figure3_climate_pressure.png',dpi=320,facecolor='white');plt.close(fig)

# Figure 4. Municipal compositional shifts versus pixels whose species assignment is uncertain.
fig=canvas('Figure4_environmental_redistribution.png')
wipe(fig,0,0,1,.51)
heading(fig,.055,.49,'c   Municipal environmental redistribution')
heading(fig,.54,.49,'d   Mapped coffee-pixel elevation sensitivity')
ax=fig.add_axes([.09,.115,.36,.29],zorder=4)
for y,v,c in [(0,26,BLUE),(1,20,GREEN)]:
    ax.plot([0,v],[y,y],color=c,lw=3,zorder=3);ax.plot(v,y,'o',color=c,ms=6,zorder=4)
    ax.text(v+.6,y,f'+{v} m',va='center',color=c,fontsize=8.5)
ax.set(yticks=[0,1],yticklabels=['Arabica','Canephora'],xlim=(-1,34),ylim=(-.55,1.55),xlabel='Change in weighted municipal elevation (m)')
ax.invert_yaxis();ax.axvline(0,lw=.7,color='#777');style(ax)
fig.text(.07,.045,'Fixed climate shifts: Arabica −0.10 °C, −3.8 hot days; Canephora −0.28 °C, −5.1 hot days.',fontsize=7.7,color='#444')
ax=fig.add_axes([.57,.115,.39,.29],zorder=4)
for y,c,v1,v2 in [(0,BLUE,-1.6,-1.6),(1,GREEN,25.0,-6.3)]:
    ax.plot(v1,y-.11,'o',color=c,ms=6,zorder=4)
    ax.plot(v2,y+.11,'s',mfc='white',mec=c,ms=6,zorder=4)
    ax.text(v1+.7,y-.11,f'{v1:+.1f}',fontsize=7.9,va='center',color=c)
    ax.text(v2+.7,y+.11,f'{v2:+.1f}',fontsize=7.9,va='center',color=c)
ax.set(yticks=[0,1],yticklabels=['Arabica','Canephora'],xlim=(-11,32),ylim=(-.55,1.55),xlabel='Change in mapped pixel elevation (m)')
ax.invert_yaxis();ax.axvline(0,lw=.7,color='#777');style(ax)
fig.legend(handles=[Line2D([0],[0],marker='o',linestyle='',color='black',label='Dominant-species municipalities'),Line2D([0],[0],marker='s',mfc='white',mec='black',linestyle='',label='Proportional species allocation')],loc='center',bbox_to_anchor=(.76,.435),frameon=False,ncol=1,fontsize=7.5)
fig.savefig(OUT/'Figure4_environmental_redistribution.png',dpi=320,facecolor='white');plt.close(fig)

# Figure 5. Preserve the source association plot; reconstruct other quantitative panels.
fig=canvas('Figure5_water_productivity.png')
wipe(fig,0,.45,.54,.55)
wipe(fig,0,0,1,.45)
heading(fig,.045,.977,'a   Seasonal rainfed water satisfaction')
ax=fig.add_axes([.10,.59,.38,.285],zorder=4)
x=np.arange(4)
ax.plot(x-.10,[3.6,-11,2.7,9.4],'o-',lw=1.5,ms=4.5,color=BLUE,label='Arabica')
ax.plot(x+.10,[-.4,-8.4,-6.8,9.4],'s--',lw=1.5,ms=4.5,color=GREEN,label='Canephora')
ax.axhline(0,color='#777',lw=.8);ax.set(xticks=x,xticklabels=['W1','W2','W3','W4'],ylim=(-13,12),ylabel='Change in ETr/ETc (percentage points)')
ax.legend(loc='upper center',ncol=2,frameon=False,fontsize=8);style(ax,'y')
fig.text(.07,.472,'Central rainfed scenario; 12-scenario range in Supplementary Fig. S6.',fontsize=7.5,color='#444')
heading(fig,.045,.435,'c   Chronological skill, 2020–2024')
heading(fig,.55,.435,'d   Within-sample explanatory value')
models=['Heat','Heat + precipitation','Heat + water balance']
ax=fig.add_axes([.13,.09,.35,.245],zorder=4)
y=np.arange(3)
ax.plot([1.02,.18,1.48],y-.12,'o',ms=5,color=BLUE,label='Arabica',zorder=3)
ax.plot([-9.2,-9.2,-2.94],y+.12,'s',ms=5,color=GREEN,label='Canephora',zorder=3)
ax.axvline(0,color='#777',lw=.8);ax.set(yticks=y,yticklabels=models,xlim=(-11,3),xlabel='Squared-error improvement (%)')
style(ax)
ax=fig.add_axes([.67,.09,.28,.245],zorder=4)
ax.plot([1.09,1.42,1.39],y-.12,'o',ms=5,color=BLUE,label='Arabica',zorder=3)
ax.plot([1.19,2.15,5.31],y+.12,'s',ms=5,color=GREEN,label='Canephora',zorder=3)
ax.set(yticks=y,yticklabels=[],xlim=(0,6),xlabel='Partial variance explained (%)')
style(ax)
fig.legend(handles=[Line2D([0],[0],marker='o',linestyle='',color=BLUE,label='Arabica'),Line2D([0],[0],marker='s',linestyle='',color=GREEN,label='Canephora')],loc='center',bbox_to_anchor=(.48,.37),ncol=2,frameon=False,fontsize=8)
fig.savefig(OUT/'Figure5_water_productivity.png',dpi=320,facecolor='white');plt.close(fig)

print('Created four revised precise raster charts')
