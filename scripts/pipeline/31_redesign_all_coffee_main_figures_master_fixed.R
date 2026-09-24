# ============================================================
# BRAZILIAN COFFEE PAPER
# MASTER SCRIPT TO REBUILD MAIN FIGURES 1-5
# FIXED VERSION USING THE ACTUAL PROJECT COLUMN NAMES
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------
req <- c(
  "dplyr", "tidyr", "readr", "stringr", "ggplot2", "patchwork",
  "scales", "sf", "rnaturalearth", "rnaturalearthdata", "forcats"
)
new <- req[!req %in% rownames(installed.packages())]
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

# ------------------------------------------------------------
# 1. Directories
# ------------------------------------------------------------
project_dir <- "."
if (!dir.exists(project_dir)) stop("Project folder not found: ", project_dir)

out_dir <- file.path(project_dir, "99_manuscript_redesign")
fig_dir <- file.path(out_dir, "figures_main")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

find_project_file <- function(filename) {
  x <- list.files(project_dir, pattern = paste0("^", filename, "$"), recursive = TRUE, full.names = TRUE)
  if (!length(x)) stop("Required file not found: ", filename)
  x[which.max(file.info(x)$mtime)]
}

# ------------------------------------------------------------
# 2. Input files: exact filenames from the validated workflow
# ------------------------------------------------------------
f_coffee <- find_project_file("coffee_brazil_municipality_audited_1974_2024.csv")
f_decomp <- find_project_file("coffee_productivity_decomposition_summary.csv")
f_cross <- find_project_file("coffee_municipality_power_grid_crosswalk.csv")
f_annual <- find_project_file("coffee_power_brazil_municipality_annual_indices.csv")
f_water_sens <- find_project_file("coffee_water_stress_scenario_sensitivity.csv")
f_water_plot <- find_project_file("coffee_seasonal_water_plot_data.csv")
f_validation <- find_project_file("coffee_water_temporal_model_comparison.csv")
f_modelcomp <- find_project_file("coffee_seasonal_water_model_comparison.csv")
f_elev_ref <- find_project_file("coffee_municipality_elevation_reference.csv")
f_elev_band <- find_project_file("coffee_elevation_band_distribution.csv")
f_elev_shift <- find_project_file("coffee_elevation_shift_summary.csv")
f_redclim <- find_project_file("coffee_redistribution_climatology_summary.csv")

coffee <- read_csv(f_coffee, show_col_types = FALSE)
decomp <- read_csv(f_decomp, show_col_types = FALSE)
cross <- read_csv(f_cross, show_col_types = FALSE)
annual <- read_csv(f_annual, show_col_types = FALSE)
water_sens <- read_csv(f_water_sens, show_col_types = FALSE)
water_plot <- read_csv(f_water_plot, show_col_types = FALSE)
validation <- read_csv(f_validation, show_col_types = FALSE)
modelcomp <- read_csv(f_modelcomp, show_col_types = FALSE)
elev_ref <- read_csv(f_elev_ref, show_col_types = FALSE)
elev_band <- read_csv(f_elev_band, show_col_types = FALSE)
elev_shift <- read_csv(f_elev_shift, show_col_types = FALSE)
redclim <- read_csv(f_redclim, show_col_types = FALSE)

# Exact source contract
stopifnot(all(c("municipality_code", "year", "species", "production_t", "area_harvested_ha") %in% names(coffee)))
stopifnot(all(c("municipality_code", "family", "requested_municipal_longitude", "requested_municipal_latitude") %in% names(cross)))
stopifnot(all(c("municipality_code", "year", "period_type", "tmean_c", "hot_days_ge32", "longest_dry_spell_days") %in% names(annual)))
stopifnot(all(c("species", "window_id", "central_change_pp", "scenario_minimum_change_pp", "scenario_maximum_change_pp") %in% names(water_sens)))
stopifnot(all(c("species", "control", "family", "scenario_id", "window_id", "predictor", "association_pct", "lower95_pct", "upper95_pct") %in% names(water_plot)))
stopifnot(all(c("species", "control", "test_period", "family", "mse_skill_vs_N") %in% names(validation)))
stopifnot(all(c("species", "control", "family", "scenario_id", "partial_R2_beyond_nuisance") %in% names(modelcomp)))

# ------------------------------------------------------------
# 3. Visual identity
# ------------------------------------------------------------
COL_ARABICA <- "#315A70"
COL_CANEPHORA <- "#69784A"
COL_ARABICA_LIGHT <- "#8DA9B7"
COL_CANEPHORA_LIGHT <- "#A4AF88"
COL_WARM <- "#C56F53"
COL_WARM_DARK <- "#9A4935"
COL_COOL <- "#6695A8"
COL_COOL_DARK <- "#416F82"
COL_NEUTRAL <- "#A6A6A6"
COL_DARK <- "#222222"

species_cols <- c(Arabica = COL_ARABICA, Canephora = COL_CANEPHORA)

theme_rogerio <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "Times New Roman") +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black", linewidth = 0.45),
      axis.ticks = element_line(colour = "black", linewidth = 0.45),
      axis.text = element_text(colour = "black", size = base_size - 1),
      axis.title = element_text(colour = "black", face = "bold", size = base_size),
      plot.title = element_text(face = "bold", size = base_size + 4, colour = "black"),
      plot.subtitle = element_text(size = base_size, colour = "#444444"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      strip.text = element_text(face = "bold", size = base_size + 1),
      panel.spacing = unit(0.9, "lines"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

save_figure <- function(p, stem, width, height) {
  ggsave(file.path(fig_dir, paste0(stem, ".png")), p, width = width, height = height, dpi = 400, bg = "white")
  ggsave(file.path(fig_dir, paste0(stem, ".pdf")), p, width = width, height = height, device = cairo_pdf, bg = "white")
}

# map base
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
sa_names <- c("Brazil", "Argentina", "Uruguay", "Paraguay", "Bolivia", "Peru", "Colombia", "Venezuela", "Guyana", "Suriname")
sa <- world |> filter(admin %in% sa_names)
br <- world |> filter(admin == "Brazil")

map_theme <- theme_rogerio(12) +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 15, face = "bold")
  )

base_map_layers <- list(
  geom_sf(data = sa, fill = "#F7F7F5", colour = "#A5A5A5", linewidth = 0.3),
  geom_sf(data = br, fill = NA, colour = "#5B5B5B", linewidth = 0.55),
  coord_sf(xlim = c(-74, -34), ylim = c(-34, 6), expand = FALSE)
)

# ------------------------------------------------------------
# 4. Core derived datasets
# ------------------------------------------------------------
coffee_use <- coffee |>
  filter(species %in% c("Arabica", "Canephora"), year >= 2012, year <= 2024)

series <- coffee_use |>
  group_by(species, year) |>
  summarise(
    production_t = sum(production_t, na.rm = TRUE),
    area_ha = sum(area_harvested_ha, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(productivity_kg_ha = production_t * 1000 / area_ha)

coords <- cross |>
  filter(family == "MET") |>
  distinct(municipality_code, .keep_all = TRUE) |>
  transmute(
    municipality_code,
    lon = requested_municipal_longitude,
    lat = requested_municipal_latitude
  )

period_area <- coffee_use |>
  filter(year %in% c(2012:2015, 2021:2024)) |>
  mutate(period = if_else(year <= 2015, "2012 to 2015", "2021 to 2024")) |>
  group_by(species, municipality_code, period) |>
  summarise(area_ha = mean(area_harvested_ha, na.rm = TRUE), .groups = "drop") |>
  group_by(species, period) |>
  mutate(area_share = area_ha / sum(area_ha, na.rm = TRUE)) |>
  ungroup() |>
  left_join(coords, by = "municipality_code")

area_change <- period_area |>
  select(species, municipality_code, period, area_share, lon, lat) |>
  pivot_wider(names_from = period, values_from = area_share, values_fill = 0) |>
  mutate(change_pp = 100 * (`2021 to 2024` - `2012 to 2015`))

annual_period <- annual |>
  filter(period_type == "year", year %in% c(2012:2015, 2021:2024)) |>
  mutate(period = if_else(year <= 2015, "2012 to 2015", "2021 to 2024")) |>
  group_by(municipality_code, period) |>
  summarise(
    tmean_c = mean(tmean_c, na.rm = TRUE),
    hot_days_ge32 = mean(hot_days_ge32, na.rm = TRUE),
    longest_dry_spell_days = mean(longest_dry_spell_days, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = period, values_from = c(tmean_c, hot_days_ge32, longest_dry_spell_days)) |>
  mutate(
    delta_tmean = tmean_c_`2021 to 2024` - tmean_c_`2012 to 2015`,
    delta_hot32 = hot_days_ge32_`2021 to 2024` - hot_days_ge32_`2012 to 2015`,
    delta_dry = longest_dry_spell_days_`2021 to 2024` - longest_dry_spell_days_`2012 to 2015`
  ) |>
  left_join(coords, by = "municipality_code")

# ------------------------------------------------------------
# FIGURE 1
# ------------------------------------------------------------
p1a <- ggplot(series, aes(year, production_t / 1e6, colour = species)) +
  geom_line(linewidth = 1.25) + geom_point(size = 2.6) +
  scale_colour_manual(values = species_cols) +
  scale_x_continuous(breaks = c(2012, 2015, 2018, 2021, 2024)) +
  labs(title = "a  National production", x = "Year", y = "Production (million t)") + theme_rogerio()

p1b <- ggplot(series, aes(year, area_ha / 1e6, colour = species)) +
  geom_line(linewidth = 1.25) + geom_point(size = 2.6) +
  scale_colour_manual(values = species_cols) +
  scale_x_continuous(breaks = c(2012, 2015, 2018, 2021, 2024)) +
  labs(title = "b  Harvested area", x = "Year", y = "Harvested area (million ha)") + theme_rogerio()

p1c <- ggplot(series, aes(year, productivity_kg_ha, colour = species)) +
  geom_line(linewidth = 1.25) + geom_point(size = 2.6) +
  scale_colour_manual(values = species_cols) +
  scale_x_continuous(breaks = c(2012, 2015, 2018, 2021, 2024)) +
  labs(title = "c  Aggregate productivity", x = "Year", y = expression(Productivity~(kg~ha^{-1}))) + theme_rogerio()

bar1 <- decomp |>
  filter(species %in% c("Arabica", "Canephora")) |>
  select(species, production_change_pct, area_change_pct, productivity_change_pct) |>
  pivot_longer(-species, names_to = "metric", values_to = "value") |>
  mutate(metric = recode(metric,
                         production_change_pct = "Production",
                         area_change_pct = "Harvested area",
                         productivity_change_pct = "Productivity"))

p1d <- ggplot(bar1, aes(species, value, fill = metric)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%+.1f%%", value)), position = position_dodge(width = 0.8),
            vjust = ifelse(bar1$value >= 0, -0.4, 1.3), fontface = "bold", size = 4.2) +
  scale_fill_manual(values = c("Production" = "#3F3F3F", "Harvested area" = COL_NEUTRAL, "Productivity" = "#5C8494")) +
  labs(title = "d  Contrasting adjustment pathways", x = NULL,
       y = "Change from 2012–2015 to 2021–2024 (%)") + theme_rogerio()

fig1 <- (p1a | p1b) / (p1c | p1d) +
  plot_annotation(title = "Two contrasting adjustment pathways sustain Brazilian coffee production",
                  subtitle = "Production remained stable or expanded while harvested area contracted, especially through Canephora intensification")
save_figure(fig1, "Figure01_two_contrasting_pathways", 14, 10.5)

# ------------------------------------------------------------
# FIGURE 2
# ------------------------------------------------------------
share_map <- function(sp, per, title, colour) {
  d <- period_area |> filter(species == sp, period == per)
  ggplot() + base_map_layers +
    geom_point(data = d, aes(lon, lat, size = area_share), colour = scales::alpha(colour, 0.68)) +
    scale_size_continuous(range = c(0.4, 5.5), guide = "none") +
    labs(title = title, subtitle = per) + map_theme
}
change_map <- function(sp, title) {
  d <- area_change |> filter(species == sp)
  lim <- quantile(abs(d$change_pp), 0.98, na.rm = TRUE)
  ggplot() + base_map_layers +
    geom_point(data = d, aes(lon, lat, size = abs(change_pp), fill = change_pp),
               shape = 21, colour = NA, alpha = 0.86) +
    scale_size_continuous(range = c(0.4, 6), guide = "none") +
    scale_fill_gradient2(low = COL_WARM_DARK, mid = "#EFEDE8", high = COL_COOL_DARK,
                         midpoint = 0, limits = c(-lim, lim), oob = scales::squish) +
    labs(title = title, subtitle = "Change in harvested-area share", fill = "Percentage points") + map_theme
}

fig2 <- (
  share_map("Arabica", "2012 to 2015", "a  Arabica harvested area", COL_ARABICA) |
  share_map("Arabica", "2021 to 2024", "b  Arabica harvested area", COL_ARABICA) |
  change_map("Arabica", "c  Arabica redistribution")
) / (
  share_map("Canephora", "2012 to 2015", "d  Canephora harvested area", COL_CANEPHORA) |
  share_map("Canephora", "2021 to 2024", "e  Canephora harvested area", COL_CANEPHORA) |
  change_map("Canephora", "f  Canephora redistribution")
) + plot_annotation(title = "Brazilian coffee production is being geographically reorganized",
                    subtitle = "Municipal harvested-area shares reveal distinct spatial trajectories for Arabica and Canephora")
save_figure(fig2, "Figure02_geographic_reorganization", 18, 11.5)

# ------------------------------------------------------------
# FIGURE 3
# ------------------------------------------------------------
climate_map <- function(var, title, low = COL_COOL, high = COL_WARM) {
  d <- annual_period
  lim <- quantile(abs(d[[var]]), 0.98, na.rm = TRUE)
  ggplot() + base_map_layers +
    geom_point(data = d, aes(lon, lat, fill = .data[[var]]), shape = 21, size = 2.0, colour = NA, alpha = 0.82) +
    scale_fill_gradient2(low = low, mid = "#F5F5F3", high = high, midpoint = 0,
                         limits = c(-lim, lim), oob = scales::squish) +
    labs(title = title) + map_theme
}

p3d <- ggplot(water_sens, aes(window_id, central_change_pp, colour = species, group = species)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.45) +
  geom_linerange(aes(ymin = scenario_minimum_change_pp, ymax = scenario_maximum_change_pp),
                 position = position_dodge(width = 0.22), linewidth = 1.0) +
  geom_point(position = position_dodge(width = 0.22), size = 3.3) +
  scale_colour_manual(values = species_cols) +
  labs(title = "d  Seasonal water limitation", x = "Calendar window",
       y = "Change in ETr/ETc (percentage points)") + theme_rogerio()

fig3 <- (climate_map("delta_tmean", "a  Mean temperature change") |
         climate_map("delta_hot32", "b  Hot-day exposure change") |
         climate_map("delta_dry", "c  Longest dry-sequence change")) / p3d +
  plot_annotation(title = "Climate pressure is intensifying across Brazilian coffee regions",
                  subtitle = "Recent production resilience has occurred alongside more heat and stronger seasonal water limitation")
save_figure(fig3, "Figure03_climate_pressure", 18, 12.5)

# ------------------------------------------------------------
# FIGURE 4
# ------------------------------------------------------------
elev_change <- area_change |>
  left_join(elev_ref |> select(municipality_code, ref_longitude, ref_latitude, elevation_m), by = "municipality_code")

alt_map <- function(sp, title) {
  d <- elev_change |> filter(species == sp)
  ggplot() + base_map_layers +
    geom_point(data = d, aes(ref_longitude, ref_latitude, fill = elevation_m), shape = 21,
               colour = NA, size = 1.8, alpha = 0.38) +
    geom_point(data = d |> filter(change_pp > 0),
               aes(ref_longitude, ref_latitude, size = change_pp), shape = 21,
               fill = NA, colour = COL_COOL_DARK, linewidth = 0.8) +
    geom_point(data = d |> filter(change_pp < 0),
               aes(ref_longitude, ref_latitude, size = -change_pp), shape = 21,
               fill = NA, colour = COL_WARM_DARK, linewidth = 0.8) +
    scale_fill_gradient(low = "#E8E3D8", high = "#54412F") +
    scale_size_continuous(range = c(0.4, 6), guide = "none") +
    labs(title = title, fill = "Reference elevation (m)") + map_theme
}

band_use <- elev_band |> filter(weighting == "Harvested_area", !is.na(elevation_band))

p4c <- band_use |> filter(species == "Arabica") |>
  ggplot(aes(elevation_band, 100 * share_of_weight, colour = period, group = period)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.7) +
  scale_colour_manual(values = c("2012 to 2015" = COL_ARABICA, "2021 to 2024" = COL_COOL)) +
  labs(title = "c  Arabica area across elevation bands", x = "Elevation band (m)", y = "Share of harvested area (%)") +
  theme_rogerio(12) + theme(axis.text.x = element_text(angle = 40, hjust = 1))

p4d <- band_use |> filter(species == "Canephora") |>
  ggplot(aes(elevation_band, 100 * share_of_weight, colour = period, group = period)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.7) +
  scale_colour_manual(values = c("2012 to 2015" = COL_CANEPHORA, "2021 to 2024" = COL_WARM)) +
  labs(title = "d  Canephora area across elevation bands", x = "Elevation band (m)", y = "Share of harvested area (%)") +
  theme_rogerio(12) + theme(axis.text.x = element_text(angle = 40, hjust = 1))

p4e <- elev_shift |> filter(weighting == "Harvested_area", species %in% c("Arabica", "Canephora")) |>
  ggplot(aes(y = forcats::fct_rev(species))) +
  geom_segment(aes(x = `weighted_mean_elevation_m__2012 to 2015`,
                   xend = `weighted_mean_elevation_m__2021 to 2024`, yend = forcats::fct_rev(species)),
               colour = "#777777", linewidth = 1.1) +
  geom_point(aes(x = `weighted_mean_elevation_m__2012 to 2015`, colour = species), size = 3.2) +
  geom_point(aes(x = `weighted_mean_elevation_m__2021 to 2024`, colour = species), size = 3.5, shape = 17) +
  geom_text(aes(x = `weighted_mean_elevation_m__2021 to 2024`, label = paste0("+", round(elevation_change_m), " m"), colour = species),
            hjust = -0.2, fontface = "bold", size = 4.0) +
  scale_colour_manual(values = species_cols) +
  labs(title = "e  Weighted elevation shift", x = "Area-weighted municipal reference elevation (m)", y = NULL) +
  theme_rogerio()

red <- redclim |> filter(weighting == "Harvested_area", species %in% c("Arabica", "Canephora"), metric %in% c("tmean_c", "hot_days_ge32"))
red <- red |>
  mutate(display = if_else(metric == "tmean_c", redistribution_only_change, redistribution_only_change / 20),
         label = if_else(metric == "tmean_c", sprintf("%+.2f °C", redistribution_only_change), sprintf("%+.1f days", redistribution_only_change)),
         metric_label = recode(metric, tmean_c = "Mean temperature", hot_days_ge32 = "Hot days ≥32 °C"))

p4f <- ggplot(red, aes(display, forcats::fct_rev(species), shape = metric_label, colour = species)) +
  geom_vline(xintercept = 0, linetype = 2) + geom_point(size = 3.4, position = position_dodge(width = 0.45)) +
  geom_text(aes(label = label), position = position_dodge(width = 0.45), hjust = -0.15, fontface = "bold", size = 3.8) +
  scale_colour_manual(values = species_cols) +
  labs(title = "f  Redistribution toward different climates", x = "Redistribution-only climatic shift\n(hot-day values scaled for display)", y = NULL) + theme_rogerio()

fig4 <- (alt_map("Arabica", "a  Arabica redistribution across elevation") |
         alt_map("Canephora", "b  Canephora redistribution across elevation")) /
        (p4c | p4d) /
        (p4e | p4f) +
  plot_annotation(title = "Redistribution is shifting coffee production across elevation and climatic gradients",
                  subtitle = "The altitude analysis is retained and integrated with spatial redistribution")
save_figure(fig4, "Figure04_altitude_environmental_redistribution", 16.5, 14.5)

# ------------------------------------------------------------
# FIGURE 5
# ------------------------------------------------------------
p5a <- ggplot(water_sens, aes(window_id, central_change_pp, colour = species, group = species)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_linerange(aes(ymin = scenario_minimum_change_pp, ymax = scenario_maximum_change_pp),
                 position = position_dodge(width = 0.22), linewidth = 1.0) +
  geom_point(position = position_dodge(width = 0.22), size = 3.4) +
  scale_colour_manual(values = species_cols) +
  labs(title = "a  Seasonal water limitation", x = "Calendar window", y = "Change in ETr/ETc (percentage points)") + theme_rogerio()

assoc <- water_plot |>
  filter(control == "Trend_parity_year",
         (family == "P" & predictor == "rain") |
         (family == "W" & scenario_id == "CAD100_Kc095" & predictor == "water")) |>
  mutate(indicator = recode(predictor, rain = "Precipitation +100 mm", water = "Water limitation −0.10 ETr/ETc"))

p5b <- ggplot(assoc, aes(association_pct, forcats::fct_rev(window_id), shape = indicator, colour = species)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(aes(xmin = lower95_pct, xmax = upper95_pct), height = 0.15, position = position_dodge(width = 0.45), na.rm = TRUE) +
  geom_point(size = 3.0, position = position_dodge(width = 0.45)) +
  scale_colour_manual(values = species_cols) +
  labs(title = "b  Climate–productivity associations", x = "Conditional association with productivity (%)", y = NULL) + theme_rogerio()

val <- validation |>
  filter(test_period == "POOLED_2020_2024", control == "Local_trend") |>
  mutate(skill_pct = 100 * mse_skill_vs_N,
         model = recode(family, N = "No climate", H = "Heat", P = "Heat + precipitation", W = "Heat + water balance"),
         model = factor(model, levels = c("No climate", "Heat", "Heat + precipitation", "Heat + water balance")))

p5c <- ggplot(val, aes(skill_pct, forcats::fct_rev(model), colour = species, shape = species)) +
  geom_vline(xintercept = 0, linetype = 2) + geom_point(size = 3.5) +
  scale_colour_manual(values = species_cols) +
  labs(title = "c  Chronological validation, 2020–2024",
       x = "Reduction in squared prediction error relative to no-climate model (%)", y = NULL) + theme_rogerio()

mc <- modelcomp |>
  filter(control == "Trend_parity_year", family %in% c("H", "P", "W"), family != "W" | scenario_id == "CAD100_Kc095") |>
  mutate(model = recode(family, H = "Heat", P = "Heat + precipitation", W = "Heat + water balance"),
         model = factor(model, levels = c("Heat", "Heat + precipitation", "Heat + water balance")))

p5d <- ggplot(mc, aes(100 * partial_R2_beyond_nuisance, forcats::fct_rev(model), colour = species, shape = species)) +
  geom_point(size = 3.5) + scale_colour_manual(values = species_cols) +
  labs(title = "d  Within-sample explanatory value", x = "Partial variance explained beyond local controls (%)", y = NULL) + theme_rogerio()

fig5 <- (p5a | p5b) / (p5c | p5d) +
  plot_annotation(title = "Seasonal water limitation explains part, but not all, of productivity dynamics",
                  subtitle = "Hydroclimate is informative, especially for Canephora, but climate alone does not reliably predict productivity")
save_figure(fig5, "Figure05_water_productivity_predictability", 15.5, 11.5)

# ------------------------------------------------------------
# captions
# ------------------------------------------------------------
writeLines(c(
  "Figure 1. Two contrasting adjustment pathways sustain Brazilian coffee production.",
  "Figure 2. Brazilian coffee production is being geographically reorganized.",
  "Figure 3. Climate pressure is intensifying across Brazilian coffee regions.",
  "Figure 4. Redistribution is shifting coffee production across elevation and climatic gradients.",
  "Figure 5. Seasonal water limitation explains part, but not all, of productivity dynamics."
), file.path(out_dir, "captions_main_figures_short.txt"))

message("All five figures completed in: ", fig_dir)
