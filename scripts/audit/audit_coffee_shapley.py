"""Independent Python replication of the five-term exact production accounting."""
from pathlib import Path
from itertools import permutations
import json
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "audits"
OUT.mkdir(exist_ok=True)
source = pd.read_csv(ROOT / "data/verification_sources/coffee_brazil_municipality_audited_1974_2024.csv.gz",
                     dtype={"municipality_code":"string"}, low_memory=False)
published = pd.read_csv(ROOT / "data/figure_inputs/shapley_decomposition_from_supplement.csv")
print('PUBLISHED_COLUMNS',published.columns.tolist())
rows=[]
for species in ('Arabica','Canephora'):
    d=source.loc[(source.species==species) & (source.year.isin([2012,2013,2014,2015,2021,2022,2023,2024])),
                 ['municipality_code','year','area_harvested_ha','production_t']].copy()
    d['period']=np.where(d.year<=2015,'old','new')
    period=d.groupby(['municipality_code','period'])[['area_harvested_ha','production_t']].mean().unstack('period')
    a0=period['area_harvested_ha']['old'];a1=period['area_harvested_ha']['new']
    p0=period['production_t']['old'];p1=period['production_t']['new']
    cont=(a0>0)&(a1>0)&p0.notna()&p1.notna()
    entrant=(a0.isna()|(a0<=0))&(a1>0)&p1.notna()
    exiting=(a0>0)&p0.notna()&(a1.isna()|(a1<=0))
    A=[a0[cont].sum(),a1[cont].sum()]
    S=[(a0[cont]/A[0]).to_numpy(),(a1[cont]/A[1]).to_numpy()]
    Y=[(1000*p0[cont]/a0[cont]).to_numpy(),(1000*p1[cont]/a1[cont]).to_numpy()]
    def fn(state):return float(A[state['A']]*np.sum(S[state['S']]*Y[state['Y']])/1000)
    contributions={c:[] for c in 'ASY'}
    for perm in permutations('ASY'):
        state={c:0 for c in 'ASY'};old=fn(state)
        for c in perm:
            state[c]=1;new=fn(state)
            contributions[c].append(new-old);old=new
    shap={c:np.mean(contributions[c]) for c in 'ASY'}
    b0=p0.sum();b1=p1.sum()
    parts={'harvested_area_level':shap['A'], 'geographic_redistribution':shap['S'],
           'within_municipality_productivity':shap['Y'],
           'entry':p1[entrant].sum(),'exit':-p0[exiting].sum()}
    residual=sum(parts.values())-(b1-b0)
    assert abs(residual)<1e-6,(species,residual)
    row={'species':species,'baseline_t':b0,'recent_t':b1,'observed_change_pct':100*(b1-b0)/b0,
         'residual_t':residual, **{k+'_pct':100*v/b0 for k,v in parts.items()},
         'continuing':int(cont.sum()),'entrants':int(entrant.sum()),'exits':int(exiting.sum())}
    rows.append(row)
result=pd.DataFrame(rows)
published_cols={
    'area':'harvested_area_level_pct',
    'redistribution':'geographic_redistribution_pct',
    'within_municipality_productivity':'within_municipality_productivity_pct',
    'entry':'entry_pct','exit':'exit_pct',
    'observed_change':'observed_change_pct',
}
joined=result.merge(published,on='species',validate='one_to_one')
for rounded,exact in published_cols.items():
    assert np.allclose(joined[rounded],joined[exact],atol=0.0051,rtol=0), (rounded,exact)
result.to_csv(OUT/'shapley_independent_comparison.csv',index=False)
print(result.to_string(index=False))
print(published.to_string(index=False))
