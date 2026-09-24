"""Independent check of published fixed-area climate exposure from two source CSVs.

This deliberately reconstructs only the four meteorological indicators plotted in
the historical Figure 3. It does not validate NASA POWER's daily calculations.
"""

from pathlib import Path
import hashlib
import json
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
CLIMATE = ROOT / "data/verification_sources/coffee_power_brazil_municipality_annual_indices.csv.gz"
COFFEE = ROOT / "data/verification_sources/coffee_brazil_municipality_audited_1974_2024.csv.gz"
REFERENCE = ROOT / "data/figure_inputs/figure03_annual_anomaly_plot_data.csv"
PERIOD = ROOT / "data/figure_inputs/figure03_selected_period_changes.csv"
OUT = ROOT / "audits"
OUT.mkdir(exist_ok=True)
METRICS = ["tmean_c", "hot_days_ge32", "precipitation_total_mm", "longest_dry_spell_days"]
SPECIES = ["Arabica", "Canephora"]


def sha256(path):
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


clim = pd.read_csv(CLIMATE, dtype={"municipality_code": "string"}, low_memory=False)
coffee = pd.read_csv(COFFEE, dtype={"municipality_code": "string"}, low_memory=False)
old = pd.read_csv(REFERENCE)
period = pd.read_csv(PERIOD)
assert clim.shape[0] == 1876 * 44 and clim.year.min() == 1981 and clim.year.max() == 2024
assert not clim.duplicated(["municipality_code", "year"]).any()

base = coffee.loc[coffee.year.between(2012, 2024)].copy()
# Original fixed panel: a record for every year and all three species, each with
# observed area and output, followed by any active municipality in the panel.
observed = base.production_t.notna() & base.area_harvested_ha.notna()
counts = base.loc[observed].groupby("municipality_code").size()
panel_codes = set(counts[counts == 13 * 3].index)
active = base.loc[(base.production_t > 0) | (base.area_harvested_ha > 0), "municipality_code"]
target_codes = panel_codes.intersection(set(active))
assert len(target_codes) == 1876, f"Unexpected panel target count: {len(target_codes)}"
assert target_codes == set(clim.municipality_code), "Climate/production target mismatch"

rows = []
for species in SPECIES:
    panel = base.loc[base.municipality_code.isin(panel_codes) & (base.species == species)]
    b = panel.loc[panel.year.between(2012, 2015)].groupby("municipality_code").area_harvested_ha.mean()
    weights = b / b.sum()
    weights = weights.loc[weights.index.isin(target_codes)]
    assert abs(weights.sum() - 1) < 1e-12
    expected_n = int((weights > 0).sum())
    for metric in METRICS:
        wide = clim.pivot(index="municipality_code", columns="year", values=metric)
        x = wide.loc[weights.index]
        assert x.loc[weights > 0].notna().all().all(), f"Missing positive-weight {species} {metric}"
        annual = x.mul(weights, axis=0).sum(axis=0)
        reference_mean = annual.loc[1981:2010].mean()
        for year, estimate in annual.items():
            rows.append({"species": species, "year": int(year), "metric": metric,
                         "estimate_independent": estimate, "baseline_1981_2010_independent": reference_mean,
                         "anomaly_independent": estimate - reference_mean,
                         "positive_weight_municipalities": expected_n})

check = pd.DataFrame(rows).merge(old, on=["species", "year", "metric"], validate="one_to_one")
assert len(check) == 2 * 44 * 4
for a,b in [("estimate_independent","estimate_strict"),
            ("baseline_1981_2010_independent","reference_mean_1981_2010"),
            ("anomaly_independent","anomaly_1981_2010")]:
    check[f"abs_diff_{a}"] = (check[a] - check[b]).abs()
assert (check.positive_weight_municipalities_x == check.positive_weight_municipalities_y).all()
assert check.filter(like="abs_diff_").max().max() < 1e-8, check.filter(like="abs_diff_").max()
check.to_csv(OUT / "figure03_annual_independent_comparison.csv", index=False)

recent = []
for (species, metric), z in check.groupby(["species", "metric"]):
    a = z.loc[z.year.between(2012, 2015), "estimate_independent"].mean()
    b = z.loc[z.year.between(2021, 2024), "estimate_independent"].mean()
    src = period.loc[(period.species == species) & (period.metric == metric) &
                     (period.weighting == "Fixed_2012_2015_area")]
    assert len(src) == 1
    recent.append({"species": species, "metric": metric,
                   "initial_mean": a, "recent_mean": b, "independent_change": b-a,
                   "published_change": src.absolute_change.iloc[0],
                   "absolute_difference": abs((b-a)-src.absolute_change.iloc[0])})
recent = pd.DataFrame(recent)
assert recent.absolute_difference.max() < 1e-8
recent.to_csv(OUT / "figure03_period_independent_comparison.csv", index=False)

report = {
    "status": "PASS: independent fixed-area weighting for four meteorological indicators",
    "annual_rows": len(clim), "municipalities": len(target_codes),
    "species_metric_year_comparisons": len(check),
    "maximum_annual_absolute_difference": float(check.filter(like="abs_diff_").max().max()),
    "maximum_period_absolute_difference": float(recent.absolute_difference.max()),
    "limits": "Daily climate-source calculations, water balance and full upstream R pipeline not tested",
    "input_sha256": {p.name: sha256(p) for p in [CLIMATE, COFFEE, REFERENCE, PERIOD]},
}
(OUT / "AUDIT.json").write_text(json.dumps(report, indent=2) + "\n")
print(json.dumps(report, indent=2))
