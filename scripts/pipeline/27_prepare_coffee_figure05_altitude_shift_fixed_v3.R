# ============================================================
# COFFE STUDY
# STEP 27
# PREPARE ALTITUDE AND CLIMATIC REDISTRIBUTION ANALYSIS
# FOR FIGURE 5
# ============================================================

rm(list = ls())

packages <- c(
  "dplyr", "tidyr", "readr", "stringr", "purrr",
  "httr", "jsonlite", "tibble"
)

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

# ------------------------------------------------------------
# Directories
# ------------------------------------------------------------

base_dir <- "."

raw_dir <- file.path(base_dir, "01_data_raw")
processed_dir <- file.path(base_dir, "02_data_processed")
out_dir <- file.path(base_dir, "04_outputs", "figure05_altitude_shift")
cache_dir <- file.path(raw_dir, "elevation", "opentopodata_aster30m")

for (x in c(raw_dir, processed_dir, out_dir, cache_dir)) {
  dir.create(x, recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------------------------------------
# Input files
# ------------------------------------------------------------

# Exact project locations created in earlier steps
f_prod_rds <- file.path(
  base_dir,
  "02_data_processed",
  "coffee_brazil_municipality_audited_1974_2024.rds"
)

f_prod_csv <- file.path(
  base_dir,
  "02_data_processed",
  "coffee_brazil_municipality_audited_1974_2024.csv"
)

f_coords <- file.path(
  base_dir,
  "02_data_processed",
  "climate",
  "NASA_POWER",
  "spatial_plan",
  "coffee_municipality_power_grid_crosswalk.csv"
)

f_clim_rds <- file.path(
  base_dir,
  "02_data_processed",
  "climate",
  "NASA_POWER",
  "brazil_indices",
  "coffee_power_brazil_municipality_annual_indices.rds"
)

f_clim_csv <- file.path(
  base_dir,
  "02_data_processed",
  "climate",
  "NASA_POWER",
  "brazil_indices",
  "coffee_power_brazil_municipality_annual_indices.csv"
)

if (file.exists(f_prod_rds)) {
  f_prod <- f_prod_rds
  prod_format <- "rds"
} else if (file.exists(f_prod_csv)) {
  f_prod <- f_prod_csv
  prod_format <- "csv"
} else {
  stop(
    paste0(
      "Audited coffee dataset not found. Checked:\n",
      f_prod_rds, "\n",
      f_prod_csv
    )
  )
}

if (file.exists(f_clim_rds)) {
  f_clim <- f_clim_rds
  clim_format <- "rds"
} else if (file.exists(f_clim_csv)) {
  f_clim <- f_clim_csv
  clim_format <- "csv"
} else {
  stop(
    paste0(
      "Municipal climate indices not found. Checked:\n",
      f_clim_rds, "\n",
      f_clim_csv
    )
  )
}

if (!file.exists(f_coords)) {
  stop(
    paste0(
      "Municipality POWER grid crosswalk not found:\n",
      f_coords
    )
  )
}

# ------------------------------------------------------------
# Parameters
# ------------------------------------------------------------

baseline_years <- 2012:2015
recent_years <- 2021:2024
climate_reference_years <- 1981:2010
species_keep <- c("Arabica", "Canephora", "Total")
weight_map <- c(
  production_t = "Production",
  area_harvested_ha = "Harvested_area"
)
selected_climate_metrics <- c(
  "tmean_c",
  "hot_days_ge32",
  "precipitation_total_mm",
  "longest_dry_spell_days"
)
api_base <- "https://api.opentopodata.org/v1/aster30m"
batch_size <- 80
pause_seconds <- 0.35

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

safe_num <- function(x) suppressWarnings(as.numeric(x))

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

weighted_share_safe <- function(x) {
  s <- sum(x, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) return(rep(NA_real_, length(x)))
  x / s
}

period_label <- function(year) {
  dplyr::case_when(
    year %in% baseline_years ~ "2012 to 2015",
    year %in% recent_years ~ "2021 to 2024",
    TRUE ~ NA_character_
  )
}

format_band <- function(lower, upper) {
  if (is.infinite(upper)) {
    paste0(lower, "+")
  } else {
    paste0(lower, " to ", upper - 1)
  }
}

# ------------------------------------------------------------
# Coordinates for municipality reference points
# ------------------------------------------------------------

coords_raw <- readr::read_csv(
  f_coords,
  show_col_types = FALSE,
  col_types = cols(
    municipality_code = col_character()
  )
)

coords_met <- coords_raw |>
  mutate(
    municipality_code = as.character(municipality_code)
  ) |>
  filter(family == "MET") |>
  transmute(
    municipality_code,
    municipality = municipality,
    state = stringr::str_extract(municipality, "(?<=\\()[A-Z]{2}(?=\\))"),
    ref_longitude = requested_municipal_longitude,
    ref_latitude = requested_municipal_latitude,
    sampling_longitude = sampling_longitude,
    sampling_latitude = sampling_latitude,
    point_to_sampling_center_km = point_to_sampling_center_km
  )

if (n_distinct(coords_met$municipality_code) == 0) {
  stop("No MET municipality coordinates found.")
}

# ------------------------------------------------------------
# Elevation download with cache
# ------------------------------------------------------------

coord_table <- coords_met |>
  distinct(municipality_code, .keep_all = TRUE) |>
  arrange(municipality_code) |>
  mutate(batch_id = ceiling(row_number() / batch_size))

get_batch_elevation <- function(df_batch) {
  loc_string <- paste0(df_batch$ref_latitude, ",", df_batch$ref_longitude, collapse = "|")
  req <- httr::GET(
    url = api_base,
    query = list(locations = loc_string),
    httr::user_agent("coffee-study-altitude-shift/1.0")
  )
  httr::stop_for_status(req)
  txt <- httr::content(req, as = "text", encoding = "UTF-8")
  raw <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)

  if (!"results" %in% names(raw)) {
    stop("Elevation API response did not contain results.")
  }

  results <- raw$results
  if (nrow(results) != nrow(df_batch)) {
    stop("Elevation result count did not match request count.")
  }

  tibble::tibble(
    municipality_code = df_batch$municipality_code,
    ref_latitude = df_batch$ref_latitude,
    ref_longitude = df_batch$ref_longitude,
    elevation_m = safe_num(results$elevation),
    elevation_source = "OpenTopoData ASTER30m",
    batch_id = unique(df_batch$batch_id),
    api_status = if ("status" %in% names(raw)) raw$status else "OK"
  )
}

batch_ids <- sort(unique(coord_table$batch_id))
batch_records <- vector("list", length(batch_ids))
n_failed_batches <- 0L

for (i in seq_along(batch_ids)) {
  bid <- batch_ids[i]
  df_batch <- coord_table |> filter(batch_id == bid)
  cache_csv <- file.path(cache_dir, sprintf("batch_%03d.csv", bid))
  cache_json <- file.path(cache_dir, sprintf("batch_%03d_raw.json", bid))

  if (file.exists(cache_csv)) {
    batch_records[[i]] <- readr::read_csv(
      cache_csv,
      show_col_types = FALSE,
      col_types = cols(municipality_code = col_character())
    )
    next
  }

  message(sprintf("Elevation batch %03d of %03d", bid, max(batch_ids)))

  out <- tryCatch({
    req <- httr::GET(
      url = api_base,
      query = list(
        locations = paste0(df_batch$ref_latitude, ",", df_batch$ref_longitude, collapse = "|")
      ),
      httr::user_agent("coffee-study-altitude-shift/1.0")
    )
    httr::stop_for_status(req)
    txt <- httr::content(req, as = "text", encoding = "UTF-8")
    writeLines(txt, cache_json, useBytes = TRUE)
    raw <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)
    if (!"results" %in% names(raw)) stop("Elevation API response did not contain results.")
    results <- raw$results
    if (nrow(results) != nrow(df_batch)) stop("Elevation result count did not match request count.")
    tibble::tibble(
      municipality_code = df_batch$municipality_code,
      ref_latitude = df_batch$ref_latitude,
      ref_longitude = df_batch$ref_longitude,
      elevation_m = safe_num(results$elevation),
      elevation_source = "OpenTopoData ASTER30m",
      batch_id = bid,
      api_status = if ("status" %in% names(raw)) raw$status else "OK"
    )
  }, error = function(e) {
    n_failed_batches <<- n_failed_batches + 1L
    warning(sprintf("Elevation batch %03d failed: %s", bid, e$message))
    tibble::tibble(
      municipality_code = df_batch$municipality_code,
      ref_latitude = df_batch$ref_latitude,
      ref_longitude = df_batch$ref_longitude,
      elevation_m = NA_real_,
      elevation_source = "OpenTopoData ASTER30m",
      batch_id = bid,
      api_status = paste0("FAILED: ", e$message)
    )
  })

  readr::write_csv(out, cache_csv)
  batch_records[[i]] <- out
  Sys.sleep(pause_seconds)
}

elevation_raw <- dplyr::bind_rows(batch_records)

municipality_elevation <- coords_met |>
  left_join(
    elevation_raw |>
      select(municipality_code, elevation_m, elevation_source, batch_id, api_status),
    by = "municipality_code"
  ) |>
  mutate(
    elevation_quality = case_when(
      is.na(elevation_m) ~ "Missing",
      elevation_m < -50 ~ "Check",
      TRUE ~ "OK"
    ),
    municipal_reference_point = TRUE,
    actual_farm_altitude_measured = FALSE
  )

readr::write_csv(
  municipality_elevation,
  file.path(out_dir, "coffee_municipality_elevation_reference.csv")
)

# ------------------------------------------------------------
# Production and harvested area by municipality and period
# ------------------------------------------------------------

prod <- if (prod_format == "rds") {
  readRDS(f_prod)
} else {
  readr::read_csv(
    f_prod,
    show_col_types = FALSE,
    col_types = cols(municipality_code = col_character())
  )
}

prod <- prod |>
  mutate(
    municipality_code = as.character(municipality_code),
    species = as.character(species),
    production_t = safe_num(production_t),
    area_harvested_ha = safe_num(area_harvested_ha),
    period = period_label(year)
  ) |>
  filter(
    species %in% species_keep,
    !is.na(period)
  )

period_weights <- prod |>
  group_by(municipality_code, municipality, state, species, period) |>
  summarise(
    production_t = mean(production_t, na.rm = TRUE),
    area_harvested_ha = mean(area_harvested_ha, na.rm = TRUE),
    n_years = dplyr::n(),
    .groups = "drop"
  ) |>
  mutate(
    production_t = ifelse(is.nan(production_t), NA_real_, production_t),
    area_harvested_ha = ifelse(is.nan(area_harvested_ha), NA_real_, area_harvested_ha)
  )

# ------------------------------------------------------------
# Municipal climatology for redistribution analysis
# ------------------------------------------------------------

clim <- if (clim_format == "rds") {
  readRDS(f_clim)
} else {
  readr::read_csv(
    f_clim,
    show_col_types = FALSE,
    col_types = cols(municipality_code = col_character())
  )
}

clim <- clim |>
  mutate(municipality_code = as.character(municipality_code)) |>
  filter(
    period_type == "year",
    year %in% climate_reference_years
  ) |>
  select(
    municipality_code,
    municipality,
    state,
    year,
    all_of(selected_climate_metrics)
  )

climatology <- clim |>
  group_by(municipality_code, municipality, state) |>
  summarise(
    across(
      all_of(selected_climate_metrics),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    across(
      all_of(selected_climate_metrics),
      ~ sum(is.finite(.x)),
      .names = "valid_years_{.col}"
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Municipality level figure-5 panel
# ------------------------------------------------------------

municipal_panel <- period_weights |>
  left_join(
    municipality_elevation |>
      select(municipality_code, ref_longitude, ref_latitude, elevation_m),
    by = "municipality_code"
  ) |>
  left_join(
    climatology |>
      select(municipality_code, all_of(selected_climate_metrics), starts_with("valid_years_")),
    by = "municipality_code"
  )

readr::write_csv(
  municipal_panel,
  file.path(out_dir, "coffee_figure05_municipal_panel.csv")
)

# ------------------------------------------------------------
# Weighted mean elevation and weighted mean municipal climatology
# ------------------------------------------------------------

long_weights <- municipal_panel |>
  pivot_longer(
    cols = all_of(names(weight_map)),
    names_to = "weight_variable",
    values_to = "weight_value"
  ) |>
  mutate(weighting = recode(weight_variable, !!!weight_map))

elevation_shift_summary <- long_weights |>
  group_by(species, weighting, period) |>
  summarise(
    municipalities_with_positive_weight = sum(is.finite(weight_value) & weight_value > 0, na.rm = TRUE),
    total_weight = sum(weight_value, na.rm = TRUE),
    weighted_mean_elevation_m = weighted_mean_safe(elevation_m, weight_value),
    missing_elevation_weight = sum(weight_value[is.na(elevation_m)], na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = period,
    values_from = c(municipalities_with_positive_weight, total_weight, weighted_mean_elevation_m, missing_elevation_weight),
    names_sep = "__"
  ) |>
  mutate(
    elevation_change_m = .data[["weighted_mean_elevation_m__2021 to 2024"]] -
      .data[["weighted_mean_elevation_m__2012 to 2015"]]
  )

readr::write_csv(
  elevation_shift_summary,
  file.path(out_dir, "coffee_elevation_shift_summary.csv")
)

climatology_long <- municipal_panel |>
  pivot_longer(
    cols = all_of(selected_climate_metrics),
    names_to = "metric",
    values_to = "climatology_value"
  ) |>
  pivot_longer(
    cols = all_of(names(weight_map)),
    names_to = "weight_variable",
    values_to = "weight_value"
  ) |>
  mutate(weighting = recode(weight_variable, !!!weight_map))

redistribution_climatology_summary <- climatology_long |>
  group_by(species, weighting, metric, period) |>
  summarise(
    total_weight = sum(weight_value, na.rm = TRUE),
    weighted_mean_climatology = weighted_mean_safe(climatology_value, weight_value),
    missing_climatology_weight = sum(weight_value[is.na(climatology_value)], na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = period,
    values_from = c(total_weight, weighted_mean_climatology, missing_climatology_weight),
    names_sep = "__"
  ) |>
  mutate(
    redistribution_only_change = .data[["weighted_mean_climatology__2021 to 2024"]] -
      .data[["weighted_mean_climatology__2012 to 2015"]]
  )

readr::write_csv(
  redistribution_climatology_summary,
  file.path(out_dir, "coffee_redistribution_climatology_summary.csv")
)

# ------------------------------------------------------------
# Elevation band distributions
# ------------------------------------------------------------

finite_elev <- municipality_elevation$elevation_m[is.finite(municipality_elevation$elevation_m)]

if (length(finite_elev) == 0) {
  stop("No finite municipality elevations were available for elevation-band analysis.")
}

max_upper <- max(200, ceiling(max(finite_elev, na.rm = TRUE) / 200) * 200)

# Build the full set of bands first and pass the labels directly to cut().
# This preserves unused bands and avoids factor() dropping empty levels.
band_breaks <- c(-Inf, seq(0, max_upper, by = 200), Inf)

band_labels <- c(
  "<0",
  purrr::map_chr(seq(0, max_upper - 200, by = 200), function(lower) {
    paste0(lower, " to ", lower + 199)
  }),
  paste0(max_upper, "+")
)

if (length(band_labels) != length(band_breaks) - 1L) {
  stop("Internal elevation-band definition mismatch.")
}

municipal_panel_band <- municipal_panel |>
  mutate(
    elevation_band = cut(
      elevation_m,
      breaks = band_breaks,
      labels = band_labels,
      right = FALSE,
      include.lowest = TRUE,
      ordered_result = TRUE
    )
  )

elevation_band_distribution <- municipal_panel_band |>
  pivot_longer(
    cols = all_of(names(weight_map)),
    names_to = "weight_variable",
    values_to = "weight_value"
  ) |>
  mutate(weighting = recode(weight_variable, !!!weight_map)) |>
  group_by(species, weighting, period, elevation_band) |>
  summarise(
    total_weight = sum(weight_value, na.rm = TRUE),
    municipalities_in_band = sum(is.finite(weight_value) & weight_value > 0, na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  mutate(
    share_of_weight = weighted_share_safe(total_weight)
  ) |>
  ungroup()

readr::write_csv(
  elevation_band_distribution,
  file.path(out_dir, "coffee_elevation_band_distribution.csv")
)

# ------------------------------------------------------------
# Simple upslope indicators
# ------------------------------------------------------------

thresholds <- c(600, 800, 1000)
upslope_indicators <- purrr::map_dfr(thresholds, function(thr) {
  long_weights |>
    mutate(above_threshold = is.finite(elevation_m) & elevation_m >= thr) |>
    group_by(species, weighting, period) |>
    summarise(
      threshold_m = thr,
      total_weight = sum(weight_value, na.rm = TRUE),
      weight_above_threshold = sum(weight_value[above_threshold], na.rm = TRUE),
      share_above_threshold = ifelse(total_weight > 0, weight_above_threshold / total_weight, NA_real_),
      .groups = "drop"
    )
}) |>
  pivot_wider(
    names_from = period,
    values_from = c(total_weight, weight_above_threshold, share_above_threshold),
    names_sep = "__"
  ) |>
  mutate(
    change_share_above_threshold = .data[["share_above_threshold__2021 to 2024"]] -
      .data[["share_above_threshold__2012 to 2015"]]
  )

readr::write_csv(
  upslope_indicators,
  file.path(out_dir, "coffee_upslope_indicators.csv")
)

# ------------------------------------------------------------
# Diagnostics and notes
# ------------------------------------------------------------

elevation_download_summary <- tibble::tibble(
  check = c(
    "execution_status",
    "municipalities_requested",
    "municipalities_with_coordinates",
    "municipalities_with_elevation",
    "failed_batches",
    "baseline_period",
    "recent_period",
    "climate_reference_period",
    "elevation_source"
  ),
  result = c(
    "COMPLETED",
    n_distinct(coord_table$municipality_code),
    n_distinct(coords_met$municipality_code),
    sum(is.finite(municipality_elevation$elevation_m)),
    n_failed_batches,
    "2012 to 2015",
    "2021 to 2024",
    "1981 to 2010",
    "OpenTopoData ASTER30m"
  )
)

readr::write_csv(
  elevation_download_summary,
  file.path(out_dir, "coffee_elevation_download_summary.csv")
)

figure05_summary <- tibble::tibble(
  check = c(
    "execution_status",
    "municipality_panel_rows",
    "species_in_panel",
    "elevation_bins",
    "selected_climate_metrics",
    "actual_farm_altitude_measured",
    "climate_attribution_performed"
  ),
  result = c(
    "COMPLETED",
    nrow(municipal_panel),
    paste(sort(unique(municipal_panel$species)), collapse = ", "),
    length(unique(elevation_band_distribution$elevation_band)),
    paste(selected_climate_metrics, collapse = ", "),
    FALSE,
    FALSE
  )
)

readr::write_csv(
  figure05_summary,
  file.path(out_dir, "coffee_figure05_preparation_summary.csv")
)

notes <- c(
  "COFFEE FIGURE 5 PREPARATION",
  "",
  "Municipal elevation is extracted at the municipal reference point used in the climate workflow.",
  "It is not a farm altitude measurement and not a municipality-wide mean elevation.",
  "",
  "Redistribution climatology is based on 1981 to 2010 municipal annual means.",
  "This isolates the climatic character of the municipalities that gained or lost weight,",
  "rather than the realized year-to-year warming already shown in Figure 3.",
  "",
  "Weighted summaries use mean production or mean harvested area within 2012 to 2015",
  "and within 2021 to 2024.",
  "",
  "Figure 5 can now be built around four complementary ideas:",
  "1. Weighted mean elevation shift.",
  "2. Redistribution across elevation bands.",
  "3. Share of coffee above key altitude thresholds.",
  "4. Change in weighted municipal climatology associated with redistribution.",
  "",
  "No claim of actual farm relocation or causal climate attribution is made at this stage."
)

writeLines(notes, file.path(out_dir, "coffee_figure05_method_notes.txt"))

message("Figure 5 preparation completed.")
message("Main output: ", file.path(out_dir, "coffee_figure05_preparation_summary.csv"))
