
# ============================================================
# COFFEE STUDY
# STEP 36
# RECENT EXTREME CLIMATE SHOCKS AND MULTI-YEAR PRODUCTION LEGACIES
# ============================================================

rm(list = ls())

packages <- c("dplyr","tidyr","readr","stringr","purrr","ggplot2","patchwork","ragg","scales","tibble")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

base_dir <- "."

find_one <- function(name){
  x <- list.files(base_dir, pattern = paste0("^", gsub("\\.","\\\\.", name), "$"),
                  recursive = TRUE, full.names = TRUE, ignore.case = FALSE)
  if(length(x) == 0) stop("Required file not found: ", name)
  x[which.max(file.info(x)$mtime)]
}

f_coffee <- find_one("coffee_brazil_municipality_audited_1974_2024.csv")
f_annual <- find_one("coffee_power_brazil_municipality_annual_indices.csv")
f_monthly <- find_one("coffee_power_brazil_municipality_monthly_indices.csv")

out_dir <- file.path(base_dir, "04_outputs", "extreme_event_case_studies")
fig_dir <- file.path(base_dir, "05_figures", "Figure06_extreme_events")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

coffee <- read_csv(f_coffee, show_col_types = FALSE)
annual <- read_csv(f_annual, show_col_types = FALSE)
monthly <- read_csv(f_monthly, show_col_types = FALSE)

col_arabica <- "#315A70"
col_canephora <- "#69784A"
col_warm <- "#C56F53"
col_cool <- "#6695A8"
col_grey <- "#777777"

theme_event <- function(base_size = 12){
  theme_minimal(base_size = base_size, base_family = "serif") +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black", linewidth = 0.4),
      axis.ticks = element_line(colour = "black", linewidth = 0.4),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(face = "bold", colour = "black"),
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(size = base_size),
      legend.position = "top",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

weighted_mean_safe <- function(x,w){
  ok <- is.finite(x) & is.finite(w) & w > 0
  if(!any(ok)) return(NA_real_)
  sum(x[ok]*w[ok]) / sum(w[ok])
}

make_weights <- function(species, states, years){
  x <- coffee |>
    filter(species == !!species, state %in% states, year %in% years) |>
    group_by(municipality_code) |>
    summarise(weight = mean(area_harvested_ha, na.rm = TRUE), .groups = "drop") |>
    filter(is.finite(weight), weight > 0) |>
    mutate(weight = weight / sum(weight))
  x
}

# ------------------------------------------------------------
# Arabica 2020-2022 shock episode
# ------------------------------------------------------------

arabica_states <- c("MG","SP","PR")
w_arb <- make_weights("Arabica", arabica_states, 2017:2019)

arabica_prod <- coffee |>
  filter(species == "Arabica", state %in% arabica_states, year >= 2012, year <= 2024) |>
  group_by(year) |>
  summarise(
    production_t = sum(production_t, na.rm = TRUE),
    harvested_area_ha = sum(area_harvested_ha, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    productivity_kg_ha = 1000 * production_t / harvested_area_ha,
    parity = if_else(year %% 2 == 0, "even", "odd")
  )

parity_base <- arabica_prod |>
  filter(year <= 2019) |>
  group_by(parity) |>
  summarise(
    production_base = mean(production_t),
    productivity_base = mean(productivity_kg_ha),
    .groups = "drop"
  )

arabica_prod <- arabica_prod |>
  left_join(parity_base, by = "parity") |>
  mutate(
    production_index_parity = 100 * production_t / production_base,
    productivity_index_parity = 100 * productivity_kg_ha / productivity_base
  )

arabica_prec <- monthly |>
  filter(
    municipality_code %in% w_arb$municipality_code,
    month %in% 4:9,
    year >= 1981, year <= 2024
  ) |>
  select(municipality_code, year, month, precipitation_total_mm) |>
  left_join(w_arb, by = "municipality_code") |>
  group_by(year, municipality_code) |>
  summarise(
    precip = sum(precipitation_total_mm, na.rm = FALSE),
    weight = first(weight),
    .groups = "drop"
  ) |>
  group_by(year) |>
  summarise(
    apr_sep_precipitation_mm = weighted_mean_safe(precip, weight),
    .groups = "drop"
  )

arabica_prec_base <- arabica_prec |>
  filter(year >= 1981, year <= 2010) |>
  summarise(x = mean(apr_sep_precipitation_mm, na.rm = TRUE)) |>
  pull(x)

arabica_prec <- arabica_prec |>
  mutate(apr_sep_precip_anomaly_mm = apr_sep_precipitation_mm - arabica_prec_base)

arabica_cold <- monthly |>
  filter(
    municipality_code %in% w_arb$municipality_code,
    month %in% c(6,7),
    year >= 2012, year <= 2024
  ) |>
  select(municipality_code, year, month, cold_days_lt2, cold_days_lt0, tmin_lowest_c) |>
  left_join(w_arb, by = "municipality_code") |>
  group_by(year, municipality_code) |>
  summarise(
    cold_days_lt2 = sum(cold_days_lt2, na.rm = TRUE),
    cold_days_lt0 = sum(cold_days_lt0, na.rm = TRUE),
    tmin_lowest_c = min(tmin_lowest_c, na.rm = TRUE),
    weight = first(weight),
    .groups = "drop"
  ) |>
  group_by(year) |>
  summarise(
    weighted_cold_days_lt2 = weighted_mean_safe(cold_days_lt2, weight),
    weighted_cold_days_lt0 = weighted_mean_safe(cold_days_lt0, weight),
    share_area_tmin_lt2_pct = 100 * sum(weight[tmin_lowest_c < 2], na.rm = TRUE) / sum(weight),
    share_area_tmin_lt0_pct = 100 * sum(weight[tmin_lowest_c < 0], na.rm = TRUE) / sum(weight),
    minimum_tmin_c = min(tmin_lowest_c, na.rm = TRUE),
    .groups = "drop"
  )

arabica_event <- arabica_prod |>
  left_join(arabica_prec, by = "year") |>
  left_join(arabica_cold, by = "year")

# ------------------------------------------------------------
# Canephora 2014-2017 drought episode in ES
# ------------------------------------------------------------

w_can <- make_weights("Canephora", "ES", 2012:2013)

canephora_prod <- coffee |>
  filter(species == "Canephora", state == "ES", year >= 2012, year <= 2019) |>
  group_by(year) |>
  summarise(
    production_t = sum(production_t, na.rm = TRUE),
    harvested_area_ha = sum(area_harvested_ha, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(productivity_kg_ha = 1000 * production_t / harvested_area_ha)

can_base <- canephora_prod |>
  filter(year %in% 2012:2013) |>
  summarise(
    production_base = mean(production_t),
    productivity_base = mean(productivity_kg_ha)
  )

canephora_prod <- canephora_prod |>
  mutate(
    production_index = 100 * production_t / can_base$production_base,
    productivity_index = 100 * productivity_kg_ha / can_base$productivity_base
  )

can_clim <- annual |>
  filter(
    municipality_code %in% w_can$municipality_code,
    year >= 1981, year <= 2019
  ) |>
  select(
    municipality_code, year,
    precipitation_total_mm, hot_days_ge32, tmean_c,
    longest_dry_spell_days, et0_total_mm, p_minus_et0_mm
  ) |>
  left_join(w_can, by = "municipality_code") |>
  group_by(year) |>
  summarise(
    across(
      c(precipitation_total_mm, hot_days_ge32, tmean_c,
        longest_dry_spell_days, et0_total_mm, p_minus_et0_mm),
      ~ weighted_mean_safe(.x, weight)
    ),
    .groups = "drop"
  )

baseline_1981_2010 <- can_clim |>
  filter(year >= 1981, year <= 2010) |>
  summarise(
    across(
      c(precipitation_total_mm, hot_days_ge32, tmean_c, longest_dry_spell_days),
      ~ mean(.x, na.rm = TRUE)
    )
  )

baseline_2001_2010 <- can_clim |>
  filter(year >= 2001, year <= 2010) |>
  summarise(
    across(c(et0_total_mm, p_minus_et0_mm), ~ mean(.x, na.rm = TRUE))
  )

can_clim <- can_clim |>
  mutate(
    precipitation_total_mm_anomaly =
      precipitation_total_mm - baseline_1981_2010$precipitation_total_mm,
    hot_days_ge32_anomaly =
      hot_days_ge32 - baseline_1981_2010$hot_days_ge32,
    tmean_c_anomaly =
      tmean_c - baseline_1981_2010$tmean_c,
    longest_dry_spell_days_anomaly =
      longest_dry_spell_days - baseline_1981_2010$longest_dry_spell_days,
    et0_total_mm_anomaly =
      et0_total_mm - baseline_2001_2010$et0_total_mm,
    p_minus_et0_mm_anomaly =
      p_minus_et0_mm - baseline_2001_2010$p_minus_et0_mm
  )

canephora_event <- canephora_prod |>
  left_join(can_clim, by = "year")

# ------------------------------------------------------------
# Outputs
# ------------------------------------------------------------

write_csv(arabica_event, file.path(out_dir, "Arabica_2020_2022_shock_episode.csv"))
write_csv(canephora_event, file.path(out_dir, "Canephora_ES_2014_2017_shock_episode.csv"))

# Main four-panel figure
a1 <- arabica_event |>
  filter(year >= 2017) |>
  ggplot(aes(year)) +
  geom_rect(aes(xmin = 2019.55, xmax = 2022.45, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = col_warm, alpha = 0.07) +
  geom_col(aes(y = apr_sep_precip_anomaly_mm), fill = col_cool, width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.35) +
  labs(
    title = "a  Arabica drought sequence",
    y = "Apr-Sep precipitation anomaly (mm)",
    x = NULL
  ) +
  theme_event()

a2 <- arabica_event |>
  filter(year >= 2017) |>
  pivot_longer(
    c(production_index_parity, productivity_index_parity),
    names_to = "metric", values_to = "index"
  ) |>
  mutate(metric = recode(metric,
    production_index_parity = "Production",
    productivity_index_parity = "Productivity"
  )) |>
  ggplot(aes(year, index, linetype = metric, shape = metric)) +
  geom_rect(aes(xmin = 2019.55, xmax = 2022.45, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = col_warm, alpha = 0.07) +
  geom_hline(yintercept = 100, linetype = 2, linewidth = 0.35) +
  geom_line(colour = col_arabica, linewidth = 1.0) +
  geom_point(colour = col_arabica, size = 2.2) +
  scale_linetype_manual(values = c(Production = 1, Productivity = 2)) +
  labs(
    title = "b  Arabica response and carryover",
    y = "Same-parity index (=100)",
    x = NULL
  ) +
  theme_event()

c1 <- canephora_event |>
  ggplot(aes(year)) +
  geom_rect(aes(xmin = 2013.55, xmax = 2016.45, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = col_warm, alpha = 0.07) +
  geom_col(aes(y = precipitation_total_mm_anomaly), fill = col_cool, width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.35) +
  labs(
    title = "c  Canephora drought sequence in Espirito Santo",
    y = "Annual precipitation anomaly (mm)",
    x = "Year"
  ) +
  theme_event()

c2 <- canephora_event |>
  pivot_longer(
    c(production_index, productivity_index),
    names_to = "metric", values_to = "index"
  ) |>
  mutate(metric = recode(metric,
    production_index = "Production",
    productivity_index = "Productivity"
  )) |>
  ggplot(aes(year, index, linetype = metric, shape = metric)) +
  geom_rect(aes(xmin = 2013.55, xmax = 2016.45, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = col_warm, alpha = 0.07) +
  geom_hline(yintercept = 100, linetype = 2, linewidth = 0.35) +
  geom_line(colour = col_canephora, linewidth = 1.0) +
  geom_point(colour = col_canephora, size = 2.2) +
  scale_linetype_manual(values = c(Production = 1, Productivity = 2)) +
  labs(
    title = "d  Canephora decline and recovery",
    y = "2012-2013 index (=100)",
    x = "Year"
  ) +
  theme_event()

main_fig <- (a1 | a2) / (c1 | c2) +
  plot_annotation(
    title = "Climate shocks can leave multi-year production legacies in perennial coffee systems"
  )

ragg::agg_png(
  file.path(fig_dir, "Figure06_multi_year_climate_shocks.png"),
  width = 4200, height = 2700, res = 300
)
print(main_fig)
dev.off()

message("STEP 36 COMPLETED")
message("Outputs: ", out_dir)
message("Figure: ", fig_dir)
