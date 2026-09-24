"""Figure 3 from the original, independently checked fixed-area annual series.

Run from the repository root after installing requirements-figures.txt:
  python scripts/figures/figure03_verified_history.py
"""
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "data/figure_inputs/figure03_annual_anomaly_plot_data.csv"
OUT = ROOT / "figures"
OUT.mkdir(exist_ok=True)
data = pd.read_csv(SOURCE)
assert len(data) == 352
assert set(data.species) == {"Arabica", "Canephora"}
assert data.missing_positive_weight_municipalities.eq(0).all()
assert data.observed_weight_fraction.eq(1).all()
assert data.weighting.eq("Fixed_2012_2015_area").all()

plt.rcParams.update({
    "font.family": "DejaVu Sans", "font.size": 12.5,
    "axes.titlesize": 17, "axes.titleweight": "bold",
    "axes.labelsize": 13.5, "axes.linewidth": 1.0,
    "xtick.labelsize": 11.6, "ytick.labelsize": 11.6,
    "pdf.fonttype": 42,
})
colors = {"Arabica": "#31586d", "Canephora": "#697c4c"}
panels = [
    ("a", "Mean temperature", "tmean_c", "Anomaly (°C)"),
    ("b", "Hot days ≥32 °C", "hot_days_ge32", "Anomaly (days yr⁻¹)"),
    ("c", "Annual precipitation", "precipitation_total_mm", "Anomaly (mm yr⁻¹)"),
    ("d", "Longest dry sequence", "longest_dry_spell_days", "Anomaly (days yr⁻¹)"),
]

fig, axes = plt.subplots(2, 2, figsize=(12.0, 9.3), constrained_layout=False)
for ax, (letter, label, metric, ylabel) in zip(axes.flat, panels):
    sub = data[data.metric.eq(metric)]
    for species in ("Arabica", "Canephora"):
        d = sub[sub.species.eq(species)].sort_values("year")
        assert d.year.tolist() == list(range(1981, 2025))
        ax.plot(d.year, d.anomaly_1981_2010, label=species,
                color=colors[species], linewidth=2.15, alpha=0.96,
                marker="o", markersize=2.8, markeredgewidth=0)
    ax.axhline(0, color="#555b5d", linewidth=0.85, zorder=0)
    ax.set(xlim=(1980.4, 2024.6), ylabel=ylabel, xlabel="Year")
    ax.set_xticks([1981, 1990, 2000, 2010, 2020])
    ax.set_title(label, loc="left", pad=12)
    ax.text(-0.12, 1.08, letter, transform=ax.transAxes,
            fontsize=19, fontweight="bold", va="top", ha="left")
    ax.spines[["top", "right"]].set_visible(False)
    ax.tick_params(axis="both", length=4, width=1, pad=5)
    ax.grid(axis="y", alpha=0.16, linewidth=0.7)
handles, labels = axes.flat[0].get_legend_handles_labels()
fig.legend(handles, labels, loc="upper center", frameon=False,
           bbox_to_anchor=(0.5, 0.99), ncol=2, fontsize=15,
           handlelength=2.3, columnspacing=2.8)
fig.subplots_adjust(left=0.115, right=0.98, top=0.90, bottom=0.075,
                    hspace=0.35, wspace=0.22)
for ext, kwargs in [("png", {"dpi": 360}), ("pdf", {})]:
    fig.savefig(OUT / f"Figure3_climate_pressure.{ext}",
                facecolor="white", bbox_inches=None, **kwargs)
plt.close(fig)
