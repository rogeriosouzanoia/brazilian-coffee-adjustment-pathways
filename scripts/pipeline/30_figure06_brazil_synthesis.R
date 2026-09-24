# ============================================================
# COFFE STUDY
# STEP 30
# FIGURE 6: BRAZILIAN COFFEE CLIMATE RISK AND ADAPTATION SYNTHESIS
# ============================================================

rm(list = ls())

packages <- c(
  "dplyr", "tidyr", "readr", "stringr", "ggplot2", "patchwork", "tibble", "grid"
)

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

# ------------------------------------------------------------
# Directories
# ------------------------------------------------------------

base_dir <- "."
out_dir <- file.path(base_dir, "04_outputs", "figure06_brazil_synthesis")
fig_dir <- file.path(base_dir, "05_figures", "Figure06_brazil_synthesis")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Helper to resolve project files robustly
# ------------------------------------------------------------

resolve_file <- function(preferred, basename_target) {
  if (file.exists(preferred)) return(preferred)

  hits <- list.files(
    base_dir,
    pattern = paste0("^", gsub("([.])", "\\\\1", basename_target), "$"),
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(hits) == 0) {
    stop("Required file not found: ", basename_target, call. = FALSE)
  }

  # Prefer files below 04_outputs when duplicates exist.
  ord <- order(!grepl("04_outputs", hits, fixed = TRUE), file.mtime(hits), decreasing = FALSE)
  hits[ord][1]
}

# ------------------------------------------------------------
# Inputs from previous completed analyses
# ------------------------------------------------------------

f_climate <- resolve_file(
  file.path(base_dir, "04_outputs", "figure03_climate_exposure", "figure03_selected_period_changes.csv"),
  "figure03_selected_period_changes.csv"
)

f_water <- resolve_file(
  file.path(base_dir, "04_outputs", "water_stress_synthesis", "coffee_water_stress_scenario_sensitivity.csv"),
  "coffee_water_stress_scenario_sensitivity.csv"
)

f_elevation <- resolve_file(
  file.path(base_dir, "04_outputs", "figure05_altitude_shift", "coffee_elevation_shift_summary.csv"),
  "coffee_elevation_shift_summary.csv"
)

f_upslope <- resolve_file(
  file.path(base_dir, "04_outputs", "figure05_altitude_shift", "coffee_upslope_indicators.csv"),
  "coffee_upslope_indicators.csv"
)

f_climredis <- resolve_file(
  file.path(base_dir, "04_outputs", "figure05_altitude_shift", "coffee_redistribution_climatology_summary.csv"),
  "coffee_redistribution_climatology_summary.csv"
)

f_assoc <- resolve_file(
  file.path(base_dir, "04_outputs", "seasonal_water_associations", "coffee_seasonal_water_plot_data.csv"),
  "coffee_seasonal_water_plot_data.csv"
)

f_validation <- resolve_file(
  file.path(base_dir, "04_outputs", "seasonal_water_temporal_validation", "coffee_water_temporal_model_comparison.csv"),
  "coffee_water_temporal_model_comparison.csv"
)

# ------------------------------------------------------------
# Read data
# ------------------------------------------------------------

climate <- readr::read_csv(f_climate, show_col_types = FALSE)
water <- readr::read_csv(f_water, show_col_types = FALSE)
elevation <- readr::read_csv(f_elevation, show_col_types = FALSE)
upslope <- readr::read_csv(f_upslope, show_col_types = FALSE)
climredis <- readr::read_csv(f_climredis, show_col_types = FALSE)
assoc <- readr::read_csv(f_assoc, show_col_types = FALSE)
validation <- readr::read_csv(f_validation, show_col_types = FALSE)

# ------------------------------------------------------------
# Formatting helpers
# ------------------------------------------------------------

fmt_signed <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "NA",
    paste0(ifelse(x > 0, "+", ""), format(round(x, digits), nsmall = digits, trim = TRUE))
  )
}

fmt_pp <- function(x, digits = 1) paste0(fmt_signed(x, digits), " pp")
fmt_deg <- function(x, digits = 2) paste0(fmt_signed(x, digits), " °C")
fmt_days <- function(x, digits = 1) paste0(fmt_signed(x, digits), " days yr⁻¹")
fmt_m <- function(x, digits = 0) paste0(fmt_signed(x, digits), " m")
fmt_pct <- function(x, digits = 1) paste0(fmt_signed(x, digits), "%")

get_change <- function(sp, metric, weighting = "Fixed_2012_2015_area") {
  z <- climate |>
    filter(species == sp, metric == !!metric, weighting == !!weighting)
  if (nrow(z) != 1) stop("Climate lookup failed: ", sp, " / ", metric)
  z$absolute_change[[1]]
}

get_water_change <- function(sp, w) {
  z <- water |> filter(species == sp, window_id == w)
  if (nrow(z) != 1) stop("Water lookup failed: ", sp, " / ", w)
  z$central_change_pp[[1]]
}

get_elev_change <- function(sp, weighting = "Harvested_area") {
  z <- elevation |> filter(species == sp, weighting == !!weighting)
  if (nrow(z) != 1) stop("Elevation lookup failed: ", sp, " / ", weighting)
  z$elevation_change_m[[1]]
}

get_threshold_change <- function(sp, threshold = 800, weighting = "Harvested_area") {
  z <- upslope |>
    filter(species == sp, threshold_m == threshold, weighting == !!weighting)
  if (nrow(z) != 1) stop("Threshold lookup failed: ", sp, " / ", threshold)
  100 * z$change_share_above_threshold[[1]]
}

get_redistribution_change <- function(sp, metric, weighting = "Harvested_area") {
  z <- climredis |>
    filter(species == sp, metric == !!metric, weighting == !!weighting)
  if (nrow(z) != 1) stop("Redistribution climate lookup failed: ", sp, " / ", metric)
  z$redistribution_only_change[[1]]
}

get_water_assoc <- function(sp, w) {
  z <- assoc |>
    filter(
      species == sp,
      control == "Trend_parity_year",
      family == "W",
      scenario_id == "CAD100_Kc095",
      window_id == w,
      predictor == "water"
    )
  if (nrow(z) != 1) stop("Association lookup failed: ", sp, " / ", w)
  z$association_pct[[1]]
}

get_holdout_skill <- function(sp, control = "Local_trend", family = "W") {
  z <- validation |>
    filter(
      species == sp,
      control == !!control,
      test_period == "POOLED_2020_2024",
      family == !!family
    )
  if (nrow(z) != 1) stop("Holdout lookup failed: ", sp, " / ", control, " / ", family)
  100 * z$mse_skill_vs_N[[1]]
}

# ------------------------------------------------------------
# Quantitative synthesis values
# ------------------------------------------------------------

arabica <- list(
  temp = get_change("Arabica", "tmean_c"),
  hot = get_change("Arabica", "hot_days_ge32"),
  dry = get_change("Arabica", "longest_dry_spell_days"),
  w2 = get_water_change("Arabica", "W2"),
  elev = get_elev_change("Arabica"),
  above800 = get_threshold_change("Arabica", 800),
  redistribution_temp = get_redistribution_change("Arabica", "tmean_c"),
  redistribution_hot = get_redistribution_change("Arabica", "hot_days_ge32"),
  water_assoc_w2 = get_water_assoc("Arabica", "W2"),
  holdout = get_holdout_skill("Arabica", "Local_trend", "W")
)

canephora <- list(
  temp = get_change("Canephora", "tmean_c"),
  hot = get_change("Canephora", "hot_days_ge32"),
  dry = get_change("Canephora", "longest_dry_spell_days"),
  w2 = get_water_change("Canephora", "W2"),
  w3 = get_water_change("Canephora", "W3"),
  elev = get_elev_change("Canephora"),
  above800 = get_threshold_change("Canephora", 800),
  redistribution_temp = get_redistribution_change("Canephora", "tmean_c"),
  redistribution_hot = get_redistribution_change("Canephora", "hot_days_ge32"),
  water_assoc_w2 = get_water_assoc("Canephora", "W2"),
  water_assoc_w3 = get_water_assoc("Canephora", "W3"),
  holdout = get_holdout_skill("Canephora", "Local_trend", "W")
)

# ------------------------------------------------------------
# Card plotting helpers
# ------------------------------------------------------------

palette <- list(
  climate = "#F5E8E5",
  water = "#E7EFF5",
  spatial = "#EEE9F4",
  response = "#F4EFE3",
  adaptation = "#E7F0E5",
  gaps = "#EEEEEE",
  border = "#3A3A3A"
)

base_card_theme <- theme_void(base_family = "Times New Roman") +
  theme(
    plot.margin = margin(4, 5, 4, 5),
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0)
  )

species_card <- function(title, subtitle, arabica_text, canephora_text, fill) {
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", colour = NA) +
    annotate("rect", xmin = 0.04, xmax = 0.48, ymin = 0.08, ymax = 0.82,
             fill = fill, colour = palette$border, linewidth = 0.45) +
    annotate("rect", xmin = 0.52, xmax = 0.96, ymin = 0.08, ymax = 0.82,
             fill = fill, colour = palette$border, linewidth = 0.45) +
    annotate("text", x = 0.08, y = 0.75, label = "Arabica", hjust = 0,
             family = "Times New Roman", fontface = "bold", size = 4.0) +
    annotate("text", x = 0.56, y = 0.75, label = "Canephora", hjust = 0,
             family = "Times New Roman", fontface = "bold", size = 4.0) +
    annotate("text", x = 0.08, y = 0.68,
             label = stringr::str_wrap(arabica_text, 31), hjust = 0, vjust = 1,
             family = "Times New Roman", size = 3.35, lineheight = 1.15) +
    annotate("text", x = 0.56, y = 0.68,
             label = stringr::str_wrap(canephora_text, 31), hjust = 0, vjust = 1,
             family = "Times New Roman", size = 3.35, lineheight = 1.15) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    labs(title = title, subtitle = subtitle) +
    base_card_theme
}

box_panel <- function(title, subtitle, boxes, fill) {
  n <- length(boxes)
  heights <- 0.70 / n
  g <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", colour = NA)

  for (i in seq_along(boxes)) {
    ytop <- 0.84 - (i - 1) * heights
    ybot <- ytop - heights + 0.025
    g <- g +
      annotate("rect", xmin = 0.05, xmax = 0.95, ymin = ybot, ymax = ytop,
               fill = fill, colour = palette$border, linewidth = 0.4) +
      annotate("text", x = 0.08, y = (ytop + ybot) / 2,
               label = stringr::str_wrap(boxes[[i]], 62),
               hjust = 0, vjust = 0.5,
               family = "Times New Roman", size = 3.25, lineheight = 1.08)
  }

  g +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    labs(title = title, subtitle = subtitle) +
    base_card_theme
}

# ------------------------------------------------------------
# Panel a: changing exposure
# ------------------------------------------------------------

p_a <- species_card(
  "a  Climate exposure is intensifying",
  "2021 to 2024 minus 2012 to 2015, fixed harvested area geography",
  paste0(
    "Mean temperature: ", fmt_deg(arabica$temp), "\n",
    "Tmax ≥32 °C: ", fmt_days(arabica$hot), "\n",
    "Longest annual dry sequence: ", fmt_days(arabica$dry)
  ),
  paste0(
    "Mean temperature: ", fmt_deg(canephora$temp), "\n",
    "Tmax ≥32 °C: ", fmt_days(canephora$hot), "\n",
    "Longest annual dry sequence: ", fmt_days(canephora$dry)
  ),
  palette$climate
)

# ------------------------------------------------------------
# Panel b: seasonal water satisfaction
# ------------------------------------------------------------

p_b <- species_card(
  "b  Seasonal water limitation is changing",
  "Central rainfed scenario: CAD 100 mm, Kc 0.95",
  paste0(
    "W2 water satisfaction: ", fmt_pp(arabica$w2), "\n",
    "Largest deterioration occurs from April to August of t−1."
  ),
  paste0(
    "W2 water satisfaction: ", fmt_pp(canephora$w2), "\n",
    "W3 water satisfaction: ", fmt_pp(canephora$w3), "\n",
    "Both deteriorate across all tested CAD and Kc scenarios."
  ),
  palette$water
)

# ------------------------------------------------------------
# Panel c: redistribution
# ------------------------------------------------------------

p_c <- species_card(
  "c  Coffee is redistributing across environmental gradients",
  "Municipal reference elevation and 1981 to 2010 climatology",
  paste0(
    "Area weighted elevation: ", fmt_m(arabica$elev), "\n",
    "Area ≥800 m: ", fmt_pp(arabica$above800), "\n",
    "Redistribution only temperature: ", fmt_deg(arabica$redistribution_temp), "\n",
    "Redistribution only hot days: ", fmt_days(arabica$redistribution_hot)
  ),
  paste0(
    "Area weighted elevation: ", fmt_m(canephora$elev), "\n",
    "Area ≥800 m: ", fmt_pp(canephora$above800), "\n",
    "Redistribution only temperature: ", fmt_deg(canephora$redistribution_temp), "\n",
    "Redistribution only hot days: ", fmt_days(canephora$redistribution_hot)
  ),
  palette$spatial
)

# ------------------------------------------------------------
# Panel d: yield association and temporal validation
# ------------------------------------------------------------

p_d <- species_card(
  "d  Climate signals explain only part of productivity variation",
  "Conditional associations and chronological outcome holdout",
  paste0(
    "W2: 0.10 lower ETr/ETc → ", fmt_pct(arabica$water_assoc_w2), " productivity association.\n",
    "Signs differ among seasonal windows.\n",
    "2020 to 2024 holdout skill of water model vs no climate: ", fmt_pct(arabica$holdout), "."
  ),
  paste0(
    "W2: 0.10 lower ETr/ETc → ", fmt_pct(canephora$water_assoc_w2), "\n",
    "W3: 0.10 lower ETr/ETc → ", fmt_pct(canephora$water_assoc_w3), "\n",
    "2020 to 2024 holdout skill of water model vs no climate: ", fmt_pct(canephora$holdout), "."
  ),
  palette$response
)

# ------------------------------------------------------------
# Panel e: adaptation portfolio
# ------------------------------------------------------------

adaptation_boxes <- c(
  "Microclimate management: shade and agroforestry can reduce daytime thermal exposure and preserve suitability in some Brazilian regions.",
  "Water management: supplemental irrigation and improved soil water conservation become increasingly relevant where seasonal water limitation intensifies.",
  "Genetic adaptation: drought and heat tolerant genotypes and cultivars expand the range of options within current production regions.",
  "Spatial planning: climatic zoning, relocation within production landscapes and expansion into newly suitable areas can complement farm level adaptation."
)

p_e <- box_panel(
  "e  Adaptation requires a portfolio rather than a single response",
  "Brazilian literature supports complementary management, genetic and spatial options",
  adaptation_boxes,
  palette$adaptation
)

# ------------------------------------------------------------
# Panel f: evidence gaps
# ------------------------------------------------------------

gap_boxes <- c(
  "Map coffee fields directly: municipal reference points cannot establish within municipality uphill movement.",
  "Observe management: irrigation, shade, cultivar and soil practices are required to separate exposure from adaptation.",
  "Align climate with phenology: flowering, fruit growth and harvest dates should replace fixed national calendar windows where possible.",
  "Validate microclimate and water balance: station and field observations are needed for local exposure and root zone water dynamics.",
  "Separate attribution from association: future work should quantify how much observed change is climate driven versus technological, genetic and market change."
)

p_f <- box_panel(
  "f  The next evidence frontier is farm level attribution",
  "Current municipal analyses identify signals, not complete causal mechanisms",
  gap_boxes,
  palette$gaps
)

# ------------------------------------------------------------
# Integrated figure
# ------------------------------------------------------------

fig <- (
  p_a + p_b
) / (
  p_c + p_d
) / (
  p_e + p_f
) +
  plot_annotation(
    title = "Climate change is reshaping Brazilian coffee production",
    subtitle = "Observed exposure, spatial redistribution, productivity responses and adaptation pathways",
    caption = paste(
      "Quantitative panels synthesize analyses developed in Figures 1 to 5.",
      "Climate changes compare 2021 to 2024 with 2012 to 2015 unless stated otherwise.",
      "Redistribution only climate metrics hold 1981 to 2010 municipal climatology fixed and change only coffee weights.",
      "Municipal reference elevation is not observed coffee field altitude.",
      "Water balance scenarios are standardized rainfed sensitivity analyses and do not represent measured municipal soils or irrigation.",
      "Productivity coefficients are conditional associations and chronological validation is retrospective, not causal attribution or operational forecasting."
    ),
    theme = theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(face = "bold", size = 17),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 8.5, hjust = 0)
    )
  )

png_file <- file.path(fig_dir, "Figure06_brazil_climate_adaptation_synthesis.png")
pdf_file <- file.path(fig_dir, "Figure06_brazil_climate_adaptation_synthesis.pdf")

ggsave(
  filename = png_file,
  plot = fig,
  width = 13.5,
  height = 14.2,
  dpi = 400,
  bg = "white"
)

pdf_status <- "NOT_SAVED"
try({
  grDevices::cairo_pdf(
    filename = pdf_file,
    width = 13.5,
    height = 14.2,
    family = "Times New Roman"
  )
  print(fig)
  grDevices::dev.off()
  pdf_status <- "SAVED"
}, silent = TRUE)

# ------------------------------------------------------------
# Export quantitative synthesis used in the figure
# ------------------------------------------------------------

quantitative_summary <- tibble::tribble(
  ~species, ~domain, ~metric, ~value, ~unit,
  "Arabica", "Climate exposure", "Mean temperature change", arabica$temp, "degC",
  "Arabica", "Climate exposure", "Hot days >=32C change", arabica$hot, "days per year",
  "Arabica", "Climate exposure", "Longest dry sequence change", arabica$dry, "days",
  "Arabica", "Water", "W2 ETr/ETc change", arabica$w2, "percentage points",
  "Arabica", "Redistribution", "Area weighted reference elevation change", arabica$elev, "m",
  "Arabica", "Redistribution", "Area share >=800m change", arabica$above800, "percentage points",
  "Arabica", "Redistribution", "Climatological temperature change from redistribution", arabica$redistribution_temp, "degC",
  "Arabica", "Redistribution", "Climatological hot day change from redistribution", arabica$redistribution_hot, "days per year",
  "Arabica", "Yield association", "W2 association for -0.10 ETr/ETc", arabica$water_assoc_w2, "%",
  "Arabica", "Temporal validation", "Water model MSE skill vs no climate", arabica$holdout, "%",
  "Canephora", "Climate exposure", "Mean temperature change", canephora$temp, "degC",
  "Canephora", "Climate exposure", "Hot days >=32C change", canephora$hot, "days per year",
  "Canephora", "Climate exposure", "Longest dry sequence change", canephora$dry, "days",
  "Canephora", "Water", "W2 ETr/ETc change", canephora$w2, "percentage points",
  "Canephora", "Water", "W3 ETr/ETc change", canephora$w3, "percentage points",
  "Canephora", "Redistribution", "Area weighted reference elevation change", canephora$elev, "m",
  "Canephora", "Redistribution", "Area share >=800m change", canephora$above800, "percentage points",
  "Canephora", "Redistribution", "Climatological temperature change from redistribution", canephora$redistribution_temp, "degC",
  "Canephora", "Redistribution", "Climatological hot day change from redistribution", canephora$redistribution_hot, "days per year",
  "Canephora", "Yield association", "W2 association for -0.10 ETr/ETc", canephora$water_assoc_w2, "%",
  "Canephora", "Yield association", "W3 association for -0.10 ETr/ETc", canephora$water_assoc_w3, "%",
  "Canephora", "Temporal validation", "Water model MSE skill vs no climate", canephora$holdout, "%"
)

readr::write_csv(
  quantitative_summary,
  file.path(out_dir, "figure06_quantitative_summary.csv")
)

# ------------------------------------------------------------
# Adaptation evidence table for manuscript references
# ------------------------------------------------------------

adaptation_sources <- tibble::tribble(
  ~adaptation_pathway, ~evidence_statement, ~reference_short, ~doi_or_url,
  "Shade and agroforestry",
  "Brazilian studies report lower daytime temperatures under shaded coffee and spatial modelling indicates that agroforestry can preserve suitable coffee area under warming.",
  "Zaro et al. 2022; Agriculture Ecosystems and Environment 2020",
  "https://www.embrapa.br/en/busca-de-publicacoes/-/publicacao/1144186/coffee-crops-adaptation-to-climate-change-in-agroforestry-systems-with-rubber-trees-in-southern-brazil | https://doi.org/10.1016/j.agee.2020.106858",
  "Irrigation and water management",
  "Brazilian modelling and economic studies identify irrigation as an important option where warming and water deficits intensify, while its feasibility remains context dependent.",
  "Science of the Total Environment 2024; Embrapa Cerrados 2024",
  "https://doi.org/10.1016/j.scitotenv.2024.177546 | https://www.embrapa.br/en/busca-de-publicacoes/-/publicacao/1170170/economic-viability-of-irrigated-coffee-with-different-water-regimes-in-the-cerrado",
  "Genetic adaptation",
  "Brazilian breeding studies identify Arabica genotypes and cultivars with traits associated with improved drought tolerance and productivity under water deficit.",
  "Silva et al. 2022; Embrapa Coffee 2025",
  "https://www.embrapa.br/en/busca-de-publicacoes/-/publicacao/1150626/strategy-for-selection-of-drought-tolerant-arabica-coffee-genotypes-in-brazil | https://www.embrapa.br/en/busca-de-publicacoes/-/publicacao/1176218/selection-of-genotypes-of-coffea-arabica-for-drought-tolerance-based-on-anatomical-and-physiological-characteristics",
  "Spatial planning",
  "Projected climate impacts vary strongly among Brazilian regions, supporting locally differentiated adaptation and land use planning rather than one national prescription.",
  "Xiao et al. 2025",
  "https://doi.org/10.1007/s10584-025-04037-9"
)

readr::write_csv(
  adaptation_sources,
  file.path(out_dir, "figure06_adaptation_sources.csv")
)

# ------------------------------------------------------------
# Provenance
# ------------------------------------------------------------

provenance <- tibble::tibble(
  role = c(
    "climate exposure",
    "water stress",
    "elevation redistribution",
    "upslope indicators",
    "redistribution climatology",
    "yield associations",
    "temporal validation"
  ),
  file = c(
    f_climate,
    f_water,
    f_elevation,
    f_upslope,
    f_climredis,
    f_assoc,
    f_validation
  ),
  md5 = unname(tools::md5sum(c(
    f_climate,
    f_water,
    f_elevation,
    f_upslope,
    f_climredis,
    f_assoc,
    f_validation
  )))
)

readr::write_csv(
  provenance,
  file.path(out_dir, "figure06_input_provenance.csv")
)

# ------------------------------------------------------------
# Report
# ------------------------------------------------------------

report <- tibble::tibble(
  check = c(
    "execution_status",
    "figure_version",
    "png_status",
    "pdf_status",
    "new_weather_downloaded",
    "new_models_fitted",
    "causal_attribution",
    "adaptation_evidence_table_created"
  ),
  result = c(
    "COMPLETED_FIGURE06_BRAZIL_SYNTHESIS",
    "30_brazil_climate_adaptation_synthesis_v1",
    "SAVED",
    pdf_status,
    "FALSE",
    "FALSE",
    "FALSE",
    "TRUE"
  )
)

readr::write_csv(
  report,
  file.path(out_dir, "figure06_report.csv")
)

message("Figure 6 Brazil synthesis completed.")
message("PNG: ", png_file)
message("PDF: ", pdf_file)
