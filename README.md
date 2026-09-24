# Brazilian coffee adjustment pathways — research code

Code and verified figure inputs for the manuscript **“Contrasting adjustments sustain Brazilian coffee production amid rising climatic exposure”**. This is a research archive for review and coauthor use. The analysis distinguishes *Coffea arabica* and *Coffea canephora*, national production accounting, municipal redistribution, fixed-geography climate exposure, simulated water satisfaction, and temporal validation.

## Contents

| Path | Purpose |
| --- | --- |
| `scripts/pipeline/` | 36 selected R scripts covering IBGE production, NASA POWER, water balance, models, validation, spatial sensitivity, and the earlier R figures. |
| `scripts/legacy/` | 13 superseded variants retained to document analytical changes. Do not mix their outputs with the selected scripts. |
| `scripts/figures/redraw_exact.py` | Original figure builder for the five main figures and Supplementary Fig. S1; follow it with the reviewed figure scripts below. |
| `scripts/figures/redraw_figure2_scientific.py` and `redraw_figure4_5_legends.py` | Final coauthor revisions of Figures 2, 4 and 5, with panel-specific keys. |
| `scripts/audit/` and `audits/` | Independent Python checks of fixed-area climate histories and exact production accounting, with comparison CSVs and hashes. |
| `data/verification_sources/` | Complete compressed audited municipal coffee series and municipal annual climate indices for these two checks. |
| `scripts/manuscript/` | Earlier Word assembly and figure editing scripts, retained for provenance; the Word source documents are separate artifacts. |
| `data/figure_inputs/` | Small processed tables sufficient to rebuild the **current figures**, including area shares by municipality and the exact Shapley table. |
| `figures/` | Submission figures in PNG (360 dpi) and PDF. |
| `SOURCE_MANIFEST.csv` | Source file identity and hashes for the selected and superseded scripts. |

## Rebuild the figures

From the repository root, with Python 3.12, `numpy`, `pandas`, `matplotlib` and `Pillow` installed:

```bash
python scripts/figures/redraw_exact.py
```

Then run the two reviewed revisions:

```bash
python scripts/figures/redraw_figure2_scientific.py
python scripts/figures/redraw_figure4_5_legends.py
```

For coauthor review, use the files ending in `_coauthor_final` for Figures 2, 4 and 5. The accompanying Word manuscript and supplement embed those versions. Earlier files without that suffix are retained as prior iterations. The water scenario whiskers are **parameter sensitivity ranges**, not confidence intervals. Figure 3 is built from the original 1981–2024 annual series, independently checked against the complete annual municipal indices.

Figure 2 uses the national outline from Natural Earth 1:110m as geographic context. Its maps a–d show only municipalities with positive measured species area shares; maps e–f show only nonzero changes with both period shares observed. Each panel gives its own size key; blue and green encode species in a–d, whereas orange and purple encode loss and gain in e–f. Marker area in points² is `6 + 3800 × fractional national area share` in a–d and `6 + 120 × absolute percentage-point change` in e–f. There is no point-size cap. Figure 4's shape key belongs to panel d, and Figure 5's rain/water shape key belongs to panel b.

## Independently audited results

Run from the repository root:

```bash
python scripts/audit/audit_coffee_climate.py
python scripts/audit/audit_coffee_shapley.py
```

The first test reproduces **352** species–metric–year estimates for four meteorological indices from **82,544** municipal annual records and the complete audited coffee panel. The largest absolute difference from the archived estimates is **6.6 × 10⁻¹²** in the corresponding indicator's units. The second test reconstructs all five exact Shapley components for each coffee species and compares them with the rounded Supplementary Table S3. These checks do not independently recalculate NASA POWER daily weather or the water-balance model. See `REPRODUCIBILITY_AUDIT.md`.

## Re-run the full analysis

The selected historical R workflow is in `scripts/pipeline/`. Scripts retain their original numerical logic and output filenames. Their original personal Windows project path has been replaced with `.` so the working directory is the project root; where a script already uses `COFFEE_STUDY_DIR`, set that environment variable to the project root. Read each header before running it. The workflow requires R and the packages declared by each script, as well as public IBGE PAM, NASA POWER, MapBiomas and elevation source downloads. The publication figures supplied here can be rebuilt **without** rerunning those downloads.

The complete audited municipal CSV and the derived annual climate indices are included in compressed form for the independent checks. The original daily source-cell climate downloads, raster tiles, serialized model objects and other intermediate datasets are **not** included. Consequently, this archive supports an independent rerun of the checks above and of all figures, but **not an end-to-end rerun of the entire R analysis without additional source downloads and intermediate generation**. The R workflow has not been executed end to end in the current preparation environment because R is unavailable here. Some acquisition steps are long running and require external service access. The optional ERA5-Land sensitivity uses a deterministic Open-Meteo sample. No API keys are stored; if an authorized commercial endpoint is used, the relevant script reads `OPEN_METEO_API_KEY` from the environment.

The historical R figure builder (`31_...R`) and the current Python builder reflect different manuscript iterations; use the three Python commands above for the coauthor visual design. The source provenance of the figure inputs is documented in `data/figure_inputs/README.md`.

## Interpretation and availability

Municipal reference points are proxies for production geography, not field coordinates. MapBiomas coffee class does not resolve species. The municipal productivity models estimate conditional associations and chronological generalization, not causal climate damage or an operational forecast. Version the source datasets and record the download date before any independent full rerun.

This working repository is intended to remain private while authors review the code and manuscript. No reuse license has been chosen. A final public archive and DOI should be coordinated with the article's data availability statement.
