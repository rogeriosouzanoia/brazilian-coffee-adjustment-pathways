# Figure input provenance

- `national_series_2012_2024.csv`: species-year sums of `production_t` and `area_harvested_ha`, with aggregate productivity recomputed as tonnes × 1,000 / hectares, from the archived audited municipal CSV.
- `municipal_area_share_periods.csv`: species-municipality means in 2012–2015 and 2021–2024, divided by the respective species-period national harvested area and joined to the NASA POWER municipal reference-coordinate crosswalk. These are municipality reference locations, not farm locations.
- `shapley_decomposition_from_supplement.csv`: exact accounting contributions transcribed from Supplementary Table S3. These differ from the older non-Shapley decomposition summary CSV and must not be interchanged.
- `figure03_annual_anomaly_plot_data.csv` and `figure03_selected_period_changes.csv`: original selected Figure 3 output tables from the national climate workflow. The complete municipal annual climate CSV was recovered later and used for independent reconstruction of all 352 annual values; see `../../audits/AUDIT.json`.
- `fixed_geography_climate_summary_from_manuscript.csv`: historical summary-only Figure 3 input, retained for provenance. It is superseded by the verified annual series in the current Figure 3.
- The seven `coffee_*.csv` files are archived project analysis outputs for the water balance, elevation redistribution, model associations and chronological validation.

The figure tables reproduce the plotted quantities. Two additional compressed source tables in `../verification_sources/` permit independent reconstruction of Figure 3 and the Shapley accounting. They do not contain the original daily NASA POWER source data. See the selected R scripts and the manuscript's Methods/Supplementary Methods for the upstream definitions.
