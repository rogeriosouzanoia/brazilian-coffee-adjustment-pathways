# Reproducibility audit — 24 September 2026

## Verified in this preparation environment

- The complete annual climate table contains 1,876 municipalities × 44 years (1981–2024), with unique municipality–year keys. The previously circulated 20 MB copy ended inside a CSV field and was discarded; the complete 37 MB copy was obtained from the project's source archive.
- Independent Python code rederived the 2012–2015 harvested-area weights from the audited municipal coffee series and rebuilt all 352 annual fixed-geography estimates for Arabica and Canephora for mean temperature, days with Tmax ≥32 °C, precipitation and the longest annual dry sequence. The maximum absolute discrepancy against the original Step 18 output is 6.6 × 10⁻¹² in indicator units. The eight species–metric period changes agree within 1.2 × 10⁻¹².
- The Theil–Sen slopes, slope intervals and Kendall statistics in Supplementary Table S4 agree with the four annual histories to the reported precision.
- Independent Python reconstruction of the 2012–2015 versus 2021–2024 five-term Shapley decomposition closes to less than 10⁻⁶ t for both species and reproduces the rounded percentages in Supplementary Table S3.
- `scripts/figures/redraw_exact.py` regenerates the five embedded main-figure PNGs and Supplementary Figure S1. The five main PNG byte hashes match the Word manuscript embedded images. Figure 3 draws from the original annual table rather than the provisional summary-only graphic.
- Both Word files were rendered and inspected after the Figure 3 and text changes.

## Scope and open checks

These are **partial, identified validation layers**, not a full independent replay of every step. The original NASA POWER daily meteorological calculations, FAO-56 ET0, Thornthwaite–Mather daily water balance, MapBiomas raster sensitivity, regressions, and chronological validation have not been independently recalculated in this round. Their published values remain the archived project outputs. The full R pipeline was not executed here because R is unavailable in the current environment, and the daily/raster/model intermediate files are absent from this archive.

Before submission, run the selected R scripts with frozen package versions and dated source extracts in an R environment; record the command sequence, session information and expected output hashes. Check the unverified water, pixel, model and event-episode tables against the final manuscript. Publish an identified code/data release and replace the manuscript's provisional availability statements. Confirm authorship, CRediT roles, affiliations and competing-interest declarations with all authors.

The archived code contains earlier variants under `scripts/legacy/`. The selected R pipeline and current Python figure builder correspond to different stages of manuscript development; use the Figure 3 override in `redraw_exact.py` for the current article.
