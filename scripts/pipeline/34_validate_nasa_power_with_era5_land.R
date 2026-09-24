# ============================================================
# COFFE-STUDY | STEP 34
# Independent climate sensitivity check using ERA5-Land via Open-Meteo
# Stratified geographic sample of coffee municipalities
# ============================================================

rm(list = ls())

packages <- c("dplyr", "tidyr", "readr", "purrr", "httr", "jsonlite", "lubridate", "ggplot2", "tibble")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

base_dir <- "."
raw_dir <- file.path(base_dir, "01_data_raw", "climate", "ERA5_Land_validation")
out_dir <- file.path(base_dir, "04_outputs", "NASA_ERA5Land_validation")
fig_dir <- file.path(base_dir, "05_figures", "NASA_ERA5Land_validation")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

find_project_file <- function(name) {
  x <- list.files(base_dir, pattern = paste0("^", name, "$"), recursive = TRUE, full.names = TRUE)
  if (!length(x)) stop("File not found in project: ", name)
  x[1]
}

coffee <- read_csv(find_project_file("coffee_brazil_municipality_audited_1974_2024.csv"), show_col_types = FALSE)
cross <- read_csv(find_project_file("coffee_municipality_power_grid_crosswalk.csv"), show_col_types = FALSE) |>
  filter(family == "MET") |>
  distinct(municipality_code, .keep_all = TRUE) |>
  transmute(
    municipality_code,
    lon = requested_municipal_longitude,
    lat = requested_municipal_latitude
  )

nasa <- read_csv(find_project_file("coffee_power_brazil_municipality_annual_indices.csv"), show_col_types = FALSE) |>
  filter(period_type == "year")

# 2012-2015 reference harvested area
ref <- coffee |>
  filter(species %in% c("Arabica", "Canephora"), year %in% 2012:2015) |>
  group_by(species, municipality_code) |>
  summarise(area = mean(area_harvested_ha, na.rm = TRUE), .groups = "drop") |>
  filter(is.finite(area), area > 0) |>
  left_join(cross, by = "municipality_code") |>
  filter(is.finite(lon), is.finite(lat))

# Deterministic spatial representation: 25 geographic clusters per species,
# then choose the largest-area municipality in each cluster.
select_sites <- function(d, k = 25) {
  k <- min(k, nrow(d))
  set.seed(42)
  km <- kmeans(scale(d[, c("lon", "lat")]), centers = k, nstart = 50)
  d$cluster <- km$cluster
  d |>
    group_by(cluster) |>
    slice_max(area, n = 1, with_ties = FALSE) |>
    ungroup()
}

sites_species <- bind_rows(
  select_sites(ref |> filter(species == "Arabica"), 25),
  select_sites(ref |> filter(species == "Canephora"), 25)
)

sites <- sites_species |>
  distinct(municipality_code, lon, lat)

write_csv(sites_species, file.path(out_dir, "era5land_validation_selected_sites_by_species.csv"))

longest_run <- function(x) {
  if (!length(x)) return(NA_integer_)
  r <- rle(x)
  if (!any(r$values)) return(0L)
  max(r$lengths[r$values])
}

get_site <- function(code, lat, lon) {
  out_csv <- file.path(raw_dir, paste0("ERA5Land_", code, ".csv"))
  if (file.exists(out_csv)) return(read_csv(out_csv, show_col_types = FALSE))

  query <- list(
    latitude = lat,
    longitude = lon,
    start_date = "1981-01-01",
    end_date = "2024-12-31",
    daily = "temperature_2m_mean,temperature_2m_max,precipitation_sum",
    models = "era5_land",
    elevation = "nan",
    timezone = "GMT"
  )

  req <- GET("https://archive-api.open-meteo.com/v1/archive", query = query, user_agent("coffee-study-era5land-validation/1.0"))
  stop_for_status(req)
  js <- content(req, as = "text", encoding = "UTF-8")
  x <- fromJSON(js)

  ans <- tibble(
    date = as.Date(x$daily$time),
    tmean_c = x$daily$temperature_2m_mean,
    tmax_c = x$daily$temperature_2m_max,
    precipitation_mm = x$daily$precipitation_sum,
    municipality_code = code
  )
  write_csv(ans, out_csv)
  Sys.sleep(0.25)
  ans
}

message("Downloading/reusing ERA5-Land for ", nrow(sites), " unique municipalities...")
era_daily <- pmap_dfr(sites, ~ get_site(..1, ..3, ..2))

era_annual <- era_daily |>
  mutate(year = year(date)) |>
  group_by(municipality_code, year) |>
  summarise(
    tmean_c = mean(tmean_c, na.rm = TRUE),
    hot_days_ge32 = sum(tmax_c >= 32, na.rm = TRUE),
    precipitation_total_mm = sum(precipitation_mm, na.rm = TRUE),
    longest_dry_spell_days = longest_run(precipitation_mm < 1),
    .groups = "drop"
  )

write_csv(era_annual, file.path(out_dir, "era5land_validation_annual_indices.csv"))

nasa2 <- nasa |>
  filter(municipality_code %in% sites$municipality_code, year >= 1981, year <= 2024) |>
  select(municipality_code, year, tmean_c, hot_days_ge32, precipitation_total_mm, longest_dry_spell_days) |>
  pivot_longer(-c(municipality_code, year), names_to = "metric", values_to = "NASA_POWER")

era2 <- era_annual |>
  pivot_longer(-c(municipality_code, year), names_to = "metric", values_to = "ERA5_Land")

paired <- inner_join(nasa2, era2, by = c("municipality_code", "year", "metric")) |>
  filter(is.finite(NASA_POWER), is.finite(ERA5_Land))

metrics <- paired |>
  group_by(metric) |>
  summarise(
    n = n(),
    pearson_r = cor(NASA_POWER, ERA5_Land, method = "pearson"),
    spearman_rho = cor(NASA_POWER, ERA5_Land, method = "spearman"),
    bias_NASA_minus_ERA5 = mean(NASA_POWER - ERA5_Land),
    rmse = sqrt(mean((NASA_POWER - ERA5_Land)^2)),
    .groups = "drop"
  )

# Compare recent-period changes at municipality level
period_change <- paired |>
  filter(year %in% c(2012:2015, 2021:2024)) |>
  mutate(period = if_else(year <= 2015, "baseline", "recent")) |>
  group_by(municipality_code, metric, period) |>
  summarise(
    NASA_POWER = mean(NASA_POWER),
    ERA5_Land = mean(ERA5_Land),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = period, values_from = c(NASA_POWER, ERA5_Land)) |>
  mutate(
    NASA_change = NASA_POWER_recent - NASA_POWER_baseline,
    ERA5_change = ERA5_Land_recent - ERA5_Land_baseline,
    sign_agreement = sign(NASA_change) == sign(ERA5_change)
  )

change_summary <- period_change |>
  group_by(metric) |>
  summarise(
    n = n(),
    change_pearson_r = cor(NASA_change, ERA5_change, use = "complete.obs"),
    change_spearman_rho = cor(NASA_change, ERA5_change, use = "complete.obs", method = "spearman"),
    sign_agreement_fraction = mean(sign_agreement, na.rm = TRUE),
    median_NASA_change = median(NASA_change, na.rm = TRUE),
    median_ERA5_change = median(ERA5_change, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(metrics, file.path(out_dir, "NASA_ERA5Land_annual_agreement_summary.csv"))
write_csv(period_change, file.path(out_dir, "NASA_ERA5Land_period_change_by_site.csv"))
write_csv(change_summary, file.path(out_dir, "NASA_ERA5Land_period_change_summary.csv"))

# Plot recent changes
p <- ggplot(period_change, aes(x = ERA5_change, y = NASA_change)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(alpha = 0.75, colour = "#315A70") +
  facet_wrap(~ metric, scales = "free") +
  labs(
    title = "Independent sensitivity of recent climate changes to ERA5-Land",
    subtitle = "Municipal changes are 2021 to 2024 minus 2012 to 2015; ERA5-Land is an independent reanalysis sensitivity",
    x = "ERA5-Land change", y = "NASA POWER change"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(face = "bold", size = 18),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(fig_dir, "NASA_ERA5Land_period_change_validation.png"), p, width = 11, height = 8, dpi = 400)
ggsave(file.path(fig_dir, "NASA_ERA5Land_period_change_validation.pdf"), p, width = 11, height = 8)

message("STEP 34 COMPLETED")
print(metrics)
print(change_summary)
