# ======================================================================
# COFFE-STUDY | STEP 11 | CLIMATE TARGETS AND A SMALL DOWNLOAD PILOT
# Run the complete file with Source in RStudio.
#
# Uses the existing audited IBGE data, fixed code panel and spatial registry.
# Does NOT download production or boundaries again, or change earlier files.
# Downloads ONLY 2024 weather for six selected municipal reference points.
# A national, multi-decade climate download is NOT launched by this script.
#
# Primary study target: 1981-2024 weather, future baseline 1981-2010.
# This is a proposed next-stage period, not data already acquired.
# Explicit weather sources (no automatic Best Match or model substitution):
#   ERA5-Land, 0.1 degree: temperature, relative humidity, maximum daily VPD.
#   ERA5, 0.25 degree: precipitation, radiation, wind and provider-derived ET0.
# ET0 is retained as the provider's ERA5-based FAO-56 reference ET0.
# It is NOT coffee evapotranspiration or a soil-water deficit.
#
# All daily values refer to 00:00-24:00 GMT/UTC, NOT local calendar days.
# Downscaling is disabled (elevation=nan). Returned elevations describe the
# selected weather grid, NOT coffee-field altitude or municipal mean terrain.
# Municipal interior points are sampling proxies, NOT coffee-field locations
# and NOT averages of weather across a municipal polygon or coffee mask.
#
# Documentation consulted for this implementation:
# https://open-meteo.com/en/docs/historical-weather-api
# https://open-meteo.com/en/terms
# https://open-meteo.com/en/pricing
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels
#
# Free endpoint: non-commercial use only. For licensed commercial access,
# set COFFEE_WEATHER_USE=commercial and OPEN_METEO_API_KEY in .Renviron.
# Keys are never written into request manifests or raw-cache filenames.
# To prepare targets without making requests, set COFFEE_SKIP_PILOT=TRUE.
# ======================================================================

coffee_climate_step11 <- function() {
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  years <- 2012:2024
  baseline_years <- 2012:2015
  categories <- c("Total", "Arabica", "Canephora")
  pilot_start <- "2024-01-01"
  pilot_end <- "2024-12-31"
  planned_start <- "1981-01-01"
  planned_end <- "2024-12-31"
  timezone <- "GMT"
  skip_pilot <- toupper(Sys.getenv("COFFEE_SKIP_PILOT", "FALSE")) == "TRUE"

  processed_dir <- file.path(base_dir, "02_data_processed")
  target_dir <- file.path(processed_dir, "climate")
  raw_dir <- file.path(base_dir, "01_data_raw", "climate", "openmeteo_pilot")
  out_dir <- file.path(base_dir, "04_outputs", "climate_pilot")
  for (p in c(target_dir, raw_dir, out_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create directory: ", p, call. = FALSE)
  }
  old_options <- options(scipen = 999, timeout = max(180, getOption("timeout", 60)))
  on.exit(options(old_options), add = TRUE)

  write_csv <- function(x, path) {
    tmp <- tempfile("coffee11_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("Cannot save: ", path, call. = FALSE)
  }
  write_rds <- function(x, path) {
    tmp <- tempfile("coffee11_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("Cannot save: ", path, call. = FALSE)
  }
  read_csv <- function(path) {
    if (!file.exists(path)) stop("Required input not found:\n", path, call. = FALSE)
    utils::read.csv(path, colClasses = "character", check.names = FALSE,
                   stringsAsFactors = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  need <- function(x, cols, label) {
    missing <- setdiff(cols, names(x))
    if (length(missing)) stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  number <- function(x, label, allow_na = TRUE) {
    original <- as.character(x)
    ans <- suppressWarnings(as.numeric(original))
    if (any(!is.na(original) & is.na(ans)) || any(!is.na(ans) & !is.finite(ans)) ||
        (!allow_na && anyNA(ans))) stop("Invalid numeric data: ", label, call. = FALSE)
    ans
  }
  check <- function(x, key) {
    need(x, c("check", "result"), "Summary")
    z <- x$result[x$check == key]
    if (length(z) != 1L || is.na(z)) stop("Missing or ambiguous check: ", key, call. = FALSE)
    z
  }
  hash <- function(path) unname(tools::md5sum(path))
  assert <- function(condition, message) {
    if (!isTRUE(condition)) stop(message, call. = FALSE)
  }
  bind <- function(parts) {
    if (!length(parts)) return(data.frame())
    z <- do.call(rbind, parts)
    rownames(z) <- NULL
    z
  }
  safe_sum <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

  report <- list(
    execution_status = "INCOMPLETE", last_stage = "Loading inputs",
    fixed_panel_codes = NA, climate_target_municipal_codes = NA,
    active_codes_outside_fixed_panel = NA,
    input_md5 = NA, spatial_registry_md5 = NA,
    baseline_area_weights_verified = FALSE, dynamic_area_weights_verified = FALSE,
    planned_climate_period = paste(planned_start, "to", planned_end),
    pilot_period = paste(pilot_start, "to", pilot_end), pilot_municipal_points = NA,
    requests_validated = 0L, requests_from_cache = 0L, pilot_site_days = 0L,
    weather_quality_passed = FALSE, nationwide_weather_downloaded = FALSE,
    regional_climate_means_calculated = FALSE, climate_trends_calculated = FALSE,
    historical_boundaries_harmonized = FALSE, farm_locations_measured = FALSE,
    coffee_field_elevation_estimated = FALSE, climate_attribution_performed = FALSE,
    original_inputs_modified = FALSE, error_if_incomplete = "None"
  )
  save_report <- function() {
    write_csv(data.frame(check = names(report), result = vapply(report, as.character, character(1)),
                         stringsAsFactors = FALSE), file.path(out_dir, "coffee_climate_pilot_summary.csv"))
  }
  on.exit({
    try(save_report(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step11.txt")), silent = TRUE)
  }, add = TRUE)

  worker <- function() {
    # ------------------------------------------------------------------
    # 1. Reuse the unchanged input and the exact step 08 fixed code panel.
    # ------------------------------------------------------------------
    source_paths <- c(
      audited = file.path(processed_dir, "coffee_brazil_municipality_audited_1974_2024.rds"),
      membership = file.path(base_dir, "04_outputs", "geographic_redistribution", "coffee_redistribution_panel_membership.csv"),
      registry = file.path(processed_dir, "spatial", "coffee_municipality_spatial_registry.csv"),
      step08 = file.path(base_dir, "04_outputs", "geographic_redistribution", "coffee_redistribution_summary.csv"),
      step10 = file.path(base_dir, "04_outputs", "figure02_integrated", "figure02_integrated_summary.csv")
    )
    assert(all(file.exists(source_paths)), paste("Missing input(s):", paste(source_paths[!file.exists(source_paths)], collapse = "\n")))
    provenance <- data.frame(role = names(source_paths), file = unname(source_paths),
                             md5 = vapply(source_paths, hash, character(1)), stringsAsFactors = FALSE)
    write_csv(provenance, file.path(out_dir, "coffee_step11_input_provenance.csv"))
    source_md5 <- hash(source_paths[["audited"]])
    registry_md5 <- hash(source_paths[["registry"]])
    s08 <- read_csv(source_paths[["step08"]])
    s10 <- read_csv(source_paths[["step10"]])
    assert(check(s10, "execution_status") == "COMPLETED", "Step 10 is incomplete.")
    assert(check(s10, "source_national_state_totals_verified") == "TRUE", "Official validation has not passed.")
    assert(check(s10, "component_identity_verified") == "TRUE", "Step 10 component check has not passed.")
    assert(source_md5 == check(s10, "source_input_md5") && source_md5 == check(s08, "input_md5"),
           "The audited input has changed since steps 08/10. Revalidate the affected steps first.")
    assert(registry_md5 == check(s08, "spatial_registry_md5"), "The spatial registry has changed since step 08.")
    report$input_md5 <<- source_md5
    report$spatial_registry_md5 <<- registry_md5

    dat <- as.data.frame(readRDS(source_paths[["audited"]]), stringsAsFactors = FALSE)
    need(dat, c("municipality_code", "municipality", "state", "year", "species", "production_t", "area_harvested_ha"), "Audited master")
    dat$municipality_code <- as.character(dat$municipality_code)
    dat$year <- number(dat$year, "year", FALSE)
    d <- dat[dat$year %in% years & dat$species %in% categories,
             c("municipality_code", "municipality", "state", "year", "species", "production_t", "area_harvested_ha")]
    rm(dat)
    assert(nrow(d) > 0L && !anyNA(d$municipality_code) && all(grepl("^[0-9]{7}$", d$municipality_code)), "Invalid municipal codes.")
    assert(!anyDuplicated(paste(d$municipality_code, d$year, d$species)), "Duplicate municipal/year/species keys.")
    for (v in c("production_t", "area_harvested_ha")) {
      d[[v]] <- number(d[[v]], v)
      assert(all(d[[v]] >= 0, na.rm = TRUE), paste("Negative values in", v))
    }
    membership <- read_csv(source_paths[["membership"]])
    need(membership, c("municipality_code", "included"), "Panel membership")
    assert(!anyDuplicated(membership$municipality_code) && !anyNA(membership$included) &&
             all(membership$included %in% c("TRUE", "FALSE")), "Invalid panel membership.")
    panel_codes <- sort(membership$municipality_code[membership$included == "TRUE"])
    observed <- !is.na(d$production_t) & !is.na(d$area_harvested_ha)
    counts <- table(d$municipality_code[observed])
    independently_complete <- sort(names(counts)[counts == length(years) * length(categories)])
    assert(identical(panel_codes, independently_complete), "Fixed panel differs from complete input records.")
    assert(length(panel_codes) == as.integer(check(s10, "fixed_panel_codes")), "Panel size differs from Figure 2.")
    panel <- d[d$municipality_code %in% panel_codes, , drop = FALSE]
    assert(!anyNA(panel$production_t) && !anyNA(panel$area_harvested_ha), "Missing data inside fixed panel.")
    report$fixed_panel_codes <<- length(panel_codes)

    # Include historical producers and entrants. Do not select only 2024 producers.
    is_active <- (!is.na(d$production_t) & d$production_t > 0) |
                 (!is.na(d$area_harvested_ha) & d$area_harvested_ha > 0)
    all_active_codes <- sort(unique(d$municipality_code[is_active]))
    target_codes <- intersect(panel_codes, all_active_codes)
    excluded_codes <- setdiff(all_active_codes, panel_codes)
    report$climate_target_municipal_codes <<- length(target_codes)
    report$active_codes_outside_fixed_panel <<- length(excluded_codes)
    assert(length(target_codes) > 0L, "There are no climate target municipalities.")
    exclusions <- d[match(excluded_codes, d$municipality_code), c("municipality_code", "municipality", "state"), drop = FALSE]
    exclusions$reason <- rep("Active in 2012-2024 but outside the complete step08 code panel", nrow(exclusions))
    write_csv(exclusions, file.path(out_dir, "coffee_climate_target_exclusions.csv"))

    registry <- read_csv(source_paths[["registry"]])
    need(registry, c("municipality_code", "spatial_match", "reference_boundary_year",
                    "municipality_reference_longitude", "municipality_reference_latitude",
                    "municipality_centroid_longitude", "municipality_centroid_latitude"), "Spatial registry")
    assert(!anyNA(registry$municipality_code) && !anyDuplicated(registry$municipality_code), "Invalid registry key.")
    j <- match(target_codes, registry$municipality_code)
    assert(!anyNA(j), "Missing climate-target coordinates in the existing spatial registry.")
    ref <- registry[j, , drop = FALSE]
    assert(all(ref$spatial_match == "TRUE") && all(ref$reference_boundary_year == "2024"), "Spatial reference is not verified for 2024.")
    targets <- panel[match(target_codes, panel$municipality_code), c("municipality_code", "municipality", "state"), drop = FALSE]
    targets$climate_sampling_id <- paste0("M_", targets$municipality_code)
    targets$longitude <- number(ref$municipality_reference_longitude, "interior longitude", FALSE)
    targets$latitude <- number(ref$municipality_reference_latitude, "interior latitude", FALSE)
    targets$centroid_longitude <- number(ref$municipality_centroid_longitude, "centroid longitude", FALSE)
    targets$centroid_latitude <- number(ref$municipality_centroid_latitude, "centroid latitude", FALSE)
    assert(all(targets$longitude >= -75 & targets$longitude <= -30 & targets$latitude >= -35 & targets$latitude <= 6),
           "Coordinates fall outside the broad Brazil envelope. No guessing or name-based replacement is performed.")
    targets$coordinate_definition <- "Municipal_interior_point_from_step05"
    targets$reference_boundary_year <- 2024L
    targets$historical_boundaries_harmonized <- FALSE
    targets$coffee_field_location <- FALSE
    targets$polygon_average_climate <- FALSE
    write_csv(targets, file.path(target_dir, "coffee_climate_target_registry.csv"))

    # ------------------------------------------------------------------
    # 2. Fixed baseline weights and changing annual weights are separate.
    # A static area weight will permit climate changes to be described at
    # a fixed production geography. Dynamic weights add compositional change.
    # No weather aggregation is calculated at this pilot stage.
    # ------------------------------------------------------------------
    report$last_stage <<- "Preparing climate weights and pilot selection"
    weights_parts <- species_parts <- coverage_parts <- list()
    for (sp in categories) {
      q <- panel[panel$species == sp, , drop = FALSE]
      b <- q[q$year %in% baseline_years, , drop = FALSE]
      b <- stats::aggregate(b[c("production_t", "area_harvested_ha")], list(municipality_code = b$municipality_code), mean)
      denom_b_area <- sum(b$area_harvested_ha)
      denom_b_prod <- sum(b$production_t)
      assert(denom_b_area > 0 && denom_b_prod > 0, "Empty baseline weights.")
      q$baseline_area_weight <- b$area_harvested_ha[match(q$municipality_code, b$municipality_code)] / denom_b_area
      q$baseline_production_weight <- b$production_t[match(q$municipality_code, b$municipality_code)] / denom_b_prod
      q$dynamic_area_weight <- q$area_harvested_ha / ave(q$area_harvested_ha, q$year, FUN = sum)
      q$dynamic_production_weight <- q$production_t / ave(q$production_t, q$year, FUN = sum)
      q <- q[q$municipality_code %in% target_codes, , drop = FALSE]
      for (v in c("baseline_area_weight", "baseline_production_weight", "dynamic_area_weight", "dynamic_production_weight")) {
        assert(all(is.finite(q[[v]])) && all(q[[v]] >= 0), paste("Invalid", v))
        assert(max(abs(tapply(q[[v]], q$year, sum) - 1)) < 1e-9, paste("Weights do not sum to one:", v))
      }
      weights_parts[[sp]] <- q[c("municipality_code", "year", "species", "production_t", "area_harvested_ha",
                                "baseline_area_weight", "baseline_production_weight", "dynamic_area_weight", "dynamic_production_weight")]
      species_parts[[sp]] <- bind(lapply(split(q, q$municipality_code), function(z) {
        positive <- z$production_t > 0 | z$area_harvested_ha > 0
        data.frame(municipality_code = z$municipality_code[1L], municipality = z$municipality[1L],
                   state = z$state[1L], species = sp, mean_production_t_2012_2024 = mean(z$production_t),
                   mean_harvested_area_ha_2012_2024 = mean(z$area_harvested_ha),
                   production_t_2024 = z$production_t[z$year == 2024],
                   positive_in_any_analysis_year = any(positive),
                   first_positive_year = if (any(positive)) min(z$year[positive]) else NA_integer_,
                   last_positive_year = if (any(positive)) max(z$year[positive]) else NA_integer_, stringsAsFactors = FALSE)
      }))
      coverage_parts[[sp]] <- bind(lapply(years, function(yr) {
        all <- d[d$species == sp & d$year == yr, ]
        p <- q[q$year == yr, ]
        data.frame(year = yr, species = sp, fixed_panel_codes = length(panel_codes),
                   target_codes = length(target_codes),
                   production_coverage_pct = 100 * sum(p$production_t) / safe_sum(all$production_t),
                   area_coverage_pct = 100 * sum(p$area_harvested_ha) / safe_sum(all$area_harvested_ha),
                   baseline_area_weight_sum = sum(p$baseline_area_weight),
                   dynamic_area_weight_sum = sum(p$dynamic_area_weight))
      }))
    }
    weights <- bind(weights_parts)
    weights <- weights[order(weights$species, weights$year, weights$municipality_code), ]
    weights$baseline_period <- "2012 to 2015"
    weights$historical_boundaries_harmonized <- FALSE
    species_targets <- bind(species_parts)
    write_csv(weights, file.path(target_dir, "coffee_climate_municipal_weights.csv"))
    write_rds(weights, file.path(target_dir, "coffee_climate_municipal_weights.rds"))
    write_csv(species_targets, file.path(target_dir, "coffee_climate_target_species.csv"))
    write_csv(bind(coverage_parts), file.path(out_dir, "coffee_climate_target_coverage.csv"))
    report$baseline_area_weights_verified <<- TRUE
    report$dynamic_area_weights_verified <<- TRUE

    # Demonstration points, not a statistical sample of Brazil.
    pairs <- data.frame(state = c("MG", "BA", "ES", "ES", "BA", "RO"),
                        species = c("Arabica", "Arabica", "Arabica", "Canephora", "Canephora", "Canephora"))
    pilot <- bind(lapply(seq_len(nrow(pairs)), function(i) {
      z <- species_targets[species_targets$state == pairs$state[i] &
                           species_targets$species == pairs$species[i] &
                           species_targets$production_t_2024 > 0, , drop = FALSE]
      assert(nrow(z) > 0, paste("No eligible pilot municipality for", pairs$state[i], pairs$species[i]))
      z <- z[order(-z$mean_production_t_2012_2024, z$municipality_code), , drop = FALSE]
      z <- z[1L, , drop = FALSE]
      k <- match(z$municipality_code, targets$municipality_code)
      z$climate_sampling_id <- targets$climate_sampling_id[k]
      z$longitude <- targets$longitude[k]
      z$latitude <- targets$latitude[k]
      z$selection_rule <- "Largest mean 2012-2024 production in selected state/species, positive production in 2024"
      z
    }))
    write_csv(pilot, file.path(out_dir, "coffee_climate_pilot_sites.csv"))
    download_sites <- pilot[!duplicated(pilot$municipality_code), c("municipality_code", "municipality", "climate_sampling_id", "longitude", "latitude")]
    report$pilot_municipal_points <<- nrow(download_sites)

    # Sources refer to each API model request, not an assumed unified grid.
    variables <- data.frame(
      api_model = c(rep("era5_land", 5), rep("era5", 4)),
      api_variable = c("temperature_2m_mean", "temperature_2m_min", "temperature_2m_max",
                       "relative_humidity_2m_mean", "vapour_pressure_deficit_max", "precipitation_sum",
                       "shortwave_radiation_sum", "wind_speed_10m_mean", "et0_fao_evapotranspiration"),
      output_column = c("tmean_c", "tmin_c", "tmax_c", "rhmean_pct", "vpdmax_kpa", "precipitation_mm",
                        "shortwave_mj_m2", "wind10m_mean_m_s", "et0_reference_mm"),
      expected_unit = c(rep("\u00b0C", 3), "%", "kPa", "mm", "MJ/m\u00b2", "m/s", "mm"),
      native_grid_degrees = c(rep(0.1, 5), rep(0.25, 4)),
      daily_statistic = c("mean", "minimum", "maximum", "mean", "maximum", "sum", "sum", "mean", "sum"),
      lower_check = c(-60, -60, -60, 0, 0, 0, 0, 0, 0),
      upper_check = c(60, 60, 60, 100, 20, 1500, 60, 100, 35),
      stringsAsFactors = FALSE)
    variables$source_note <- ifelse(variables$api_model == "era5_land", "ERA5-Land via Open-Meteo", "ERA5 via Open-Meteo")
    variables$source_note[variables$output_column == "et0_reference_mm"] <- "Open-Meteo FAO-56 reference ET0 from ERA5 meteorology; not coffee ET"
    variables$quality_bounds_note <- "Broad screening limits, not coffee stress thresholds"
    variables$documentation <- "https://open-meteo.com/en/docs/historical-weather-api"
    write_csv(variables, file.path(out_dir, "coffee_climate_variable_dictionary.csv"))

    notes <- c(
      "COFFE-STUDY | STEP 11 | CLIMATE PILOT",
      paste("Executed:", format(Sys.time(), usetz = TRUE)),
      paste("Input MD5:", source_md5), paste("Spatial registry MD5:", registry_md5),
      "The 2012-2024 fixed municipal-code panel is inherited from step 08.",
      "Targets have positive production or harvested area in at least one 2012-2024 record.",
      "Former producers and entrants are retained; selection is not restricted to recent producers.",
      "Active municipalities outside the complete code panel are separately listed, not set to zero.",
      "Baseline area weights are normalized 2012-2015 mean harvested areas within each category.",
      "Dynamic area weights use each year's harvested areas. Production weights are sensitivity alternatives.",
      "Weights always remain separate for Total, Arabica and Canephora. Never add Total to its species.",
      "No regional climate average or trend is computed in this step.",
      paste("Multi-decade period proposed for a later acquisition stage:", planned_start, "to", planned_end),
      "Proposed anomaly reference: 1981-2010. The present pilot contains only 2024.",
      "The six state/species demonstration selections are not a representative national climate sample.",
      "ERA5-Land is requested explicitly for temperature, RH and daily maximum VPD (0.1 degree grid).",
      "ERA5 is requested explicitly for precipitation, radiation, wind and reference ET0 (0.25 degree grid).",
      "No Best Match, IFS, ERA5-Seamless, silent variable removal or automatic source substitution is used.",
      "API-derived ET0 is retained as delivered and is not recalculated with ERA5-Land temperatures.",
      "Daily values refer to GMT/UTC days. These are not local agronomic-day summaries.",
      "Interior municipal reference points are used. They are not coffee farms or polygon means.",
      "Nearest cell selection and elevation=nan disable coordinate-elevation downscaling.",
      "Returned weather-cell latitude, longitude and elevation are retained separately for each source.",
      "Weather-cell elevations are not evidence for coffee-field altitude or uphill movement.",
      "Crop-location masks and spatial representativeness must be addressed before final exposure inference.",
      "A gridded daily minimum is not a verified frost observation at a coffee field.",
      "P minus reference ET0 is not a root-zone water balance and is not calculated here.",
      "Dates, units, array lengths, missing values and broad physical ranges are checked before merging.",
      "Missing values are preserved. No undefined or missing climate value is set to zero.",
      "A valid raw response is reused only for the same exact public query, without a source substitution.",
      "Rate-limit/block responses stop the run. Rerunning preserves previously successful requests.",
      "The pilot uses the documented free endpoint only for non-commercial use, or an authorized customer key.",
      "Historical boundaries remain unharmonized. No climatic attribution or adaptation effect is estimated.",
      "All previously prepared data and figures remain unchanged. No new main figure is produced.",
      "https://open-meteo.com/en/docs/historical-weather-api",
      "https://open-meteo.com/en/terms", "https://open-meteo.com/en/pricing")
    writeLines(notes, file.path(out_dir, "coffee_climate_pilot_method_notes.txt"), useBytes = TRUE)
    message("Climate target municipalities: ", nrow(targets), "; planned pilot points: ", nrow(download_sites))
    message("Only the 2024 pilot will be downloaded. Existing production and spatial files are reused.")
    if (skip_pilot) {
      report$execution_status <<- "TARGETS_READY_PILOT_NOT_RUN"
      report$last_stage <<- "Targets prepared; download skipped by COFFEE_SKIP_PILOT"
      save_report()
      return(invisible(NULL))
    }

    # ------------------------------------------------------------------
    # 3. Small weather download; exact-query cache, explicit HTTP failures.
    # ------------------------------------------------------------------
    report$last_stage <<- "Preparing pilot download"
    packages <- c("curl", "jsonlite")
    missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_packages)) utils::install.packages(missing_packages, repos = "https://cloud.r-project.org")
    assert(all(vapply(packages, requireNamespace, logical(1), quietly = TRUE)), "Packages curl and jsonlite are required.")
    api_use <- tolower(Sys.getenv("COFFEE_WEATHER_USE", "noncommercial"))
    assert(api_use %in% c("noncommercial", "commercial"), "COFFEE_WEATHER_USE must be noncommercial or commercial.")
    api_key <- if (api_use == "commercial") Sys.getenv("OPEN_METEO_API_KEY", "") else ""
    if (api_use == "commercial" && !nzchar(api_key)) stop("Commercial API use requires your authorized OPEN_METEO_API_KEY.", call. = FALSE)
    endpoint <- if (api_use == "commercial") "https://customer-archive-api.open-meteo.com/v1/archive" else "https://archive-api.open-meteo.com/v1/archive"
    message(if (api_use == "commercial") "Using the authorized customer endpoint." else "Using the non-commercial pilot endpoint. Provider limits and licence apply.")
    expected_dates <- seq(as.Date(pilot_start), as.Date(pilot_end), by = "day")
    make_url <- function(site, model) {
      v <- variables$api_variable[variables$api_model == model]
      args <- list(latitude = sprintf("%.7f", site$latitude), longitude = sprintf("%.7f", site$longitude),
                   start_date = pilot_start, end_date = pilot_end, daily = paste(v, collapse = ","), models = model,
                   timezone = timezone, elevation = "nan", cell_selection = "nearest",
                   temperature_unit = "celsius", precipitation_unit = "mm", wind_speed_unit = "ms", timeformat = "iso8601")
      vals <- vapply(args, function(z) utils::URLencode(as.character(z), reserved = TRUE), character(1))
      paste0(endpoint, "?", paste(paste(names(args), vals, sep = "="), collapse = "&"))
    }
    query_hash <- function(url) {
      f <- tempfile("coffee11_query_", tmpdir = raw_dir)
      on.exit(unlink(f), add = TRUE)
      writeLines(url, f, useBytes = TRUE)
      hash(f)
    }
    redact <- function(text) {
      if (nzchar(api_key)) text <- gsub(api_key, "[REDACTED]", text, fixed = TRUE)
      text <- gsub("[\r\n]+", " ", text)
      substr(text, 1L, 350L)
    }
    retrieve <- function(public_url, raw_path) {
      if (file.exists(raw_path)) return(list(path = raw_path, cached = TRUE))
      request_url <- if (nzchar(api_key)) paste0(public_url, "&apikey=", utils::URLencode(api_key, reserved = TRUE)) else public_url
      last <- "Unknown download failure"
      for (attempt in 1:3) {
        h <- curl::new_handle()
        curl::handle_setopt(h, connecttimeout = 25, timeout = 120, followlocation = TRUE,
                            useragent = "CoffeeClimateReview-Step11/1.0")
        curl::handle_setheaders(h, Accept = "application/json")
        response <- tryCatch(curl::curl_fetch_memory(request_url, handle = h), error = function(e) e)
        if (inherits(response, "error")) {
          last <- redact(conditionMessage(response))
        } else {
          status <- response$status_code
          text <- rawToChar(response$content)
          if (status == 429L) stop("Open-Meteo rate limit (HTTP 429). Stop here and rerun later; successful cache files are retained.", call. = FALSE)
          if (status %in% c(401L, 403L)) stop("Open-Meteo denied access (HTTP ", status, "). No bypass or repeated blocked requests are attempted.", call. = FALSE)
          if (status == 200L) {
            assert(!grepl("^\\s*<", text), "The endpoint returned HTML rather than weather JSON.")
            assert(jsonlite::validate(text), "The endpoint returned invalid JSON.")
            obj <- jsonlite::fromJSON(text, simplifyVector = FALSE)
            if (isTRUE(obj$error)) stop("Weather API error: ", redact(as.character(obj$reason)), call. = FALSE)
            assert(is.list(obj) && !is.null(obj$daily), "API JSON has no daily weather object.")
            tmp <- tempfile("coffee11_raw_", tmpdir = raw_dir, fileext = ".json")
            con <- file(tmp, "wb")
            writeBin(response$content, con)
            close(con)
            ok <- file.copy(tmp, raw_path, overwrite = FALSE)
            unlink(tmp)
            assert(ok, paste("Cannot save raw response:", raw_path))
            # At most 12 one-year requests. No national loop is hidden here.
            Sys.sleep(2)
            return(list(path = raw_path, cached = FALSE))
          }
          last <- paste("HTTP", status)
          if (jsonlite::validate(text)) {
            reason <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE)$reason, error = function(e) NULL)
            if (!is.null(reason)) last <- paste(last, redact(as.character(reason)))
          }
          if (!status %in% c(408L, 500L, 502L, 503L, 504L)) stop(last, call. = FALSE)
        }
        if (attempt < 3L) Sys.sleep(c(4, 10)[attempt])
      }
      stop("Weather download failed after 3 attempts: ", last, call. = FALSE)
    }
    scalar <- function(x, label) {
      assert(is.numeric(x) && length(x) == 1L && is.finite(x), paste("Invalid response", label))
      as.numeric(x)
    }
    normalize_unit <- function(x) {
      x <- as.character(x)
      x <- gsub("\u00b2", "2", x, fixed = TRUE)
      x <- gsub("\u00b0", "", x, fixed = TRUE)
      tolower(gsub("[[:space:]]+", "", x))
    }
    great_circle_km <- function(lon1, lat1, lon2, lat2) {
      rad <- pi / 180
      a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
      6371.0088 * 2 * asin(sqrt(min(1, max(0, a))))
    }
    parse_weather <- function(raw_path, model, site) {
      obj <- jsonlite::fromJSON(raw_path, simplifyVector = FALSE)
      assert(!isTRUE(obj$error) && !is.null(obj$daily), "Cached response is not a successful weather object.")
      tt <- unlist(obj$daily$time, use.names = FALSE)
      assert(is.character(tt) && identical(tt, format(expected_dates, "%Y-%m-%d")),
             "Weather response dates are incomplete, duplicated, reordered, or outside the pilot period.")
      assert(scalar(obj$utc_offset_seconds, "UTC offset") == 0, "Weather response does not use the requested GMT days.")
      lon <- scalar(obj$longitude, "longitude")
      lat <- scalar(obj$latitude, "latitude")
      elev <- if (is.null(obj$elevation)) NA_real_ else scalar(obj$elevation, "elevation")
      assert(abs(lon) <= 180 && abs(lat) <= 90, "Invalid response coordinates.")
      distance <- great_circle_km(site$longitude, site$latitude, lon, lat)
      assert(distance < 60, "Weather response coordinates are unexpectedly far from the requested municipal point.")
      z <- variables[variables$api_model == model, , drop = FALSE]
      daily <- data.frame(date = expected_dates)
      qa <- list()
      for (i in seq_len(nrow(z))) {
        variable <- z$api_variable[i]
        key <- variable
        if (!key %in% names(obj$daily)) key <- paste0(variable, "_", model)
        assert(key %in% names(obj$daily), paste("Missing API variable:", variable, "for", model))
        values <- obj$daily[[key]]
        assert(length(values) == length(expected_dates), paste("Wrong array length:", key))
        values <- vapply(values, function(v) {
          if (is.null(v)) return(NA_real_)
          assert(is.numeric(v) && length(v) == 1L && is.finite(v), paste("Invalid numeric value in", key))
          as.numeric(v)
        }, numeric(1))
        unit <- obj$daily_units[[key]]
        if (is.null(unit)) unit <- obj$daily_units[[variable]]
        assert(length(unit) == 1L && normalize_unit(unit) == normalize_unit(z$expected_unit[i]),
               paste("Unexpected or missing unit for", key, ":", paste(unit, collapse = " ")))
        daily[[z$output_column[i]]] <- values
        outside <- !is.na(values) & (values < z$lower_check[i] - 1e-6 | values > z$upper_check[i] + 1e-6)
        qa[[i]] <- data.frame(municipality_code = site$municipality_code, api_model = model,
                              variable = variable, received_unit = as.character(unit), expected_days = length(expected_dates),
                              received_days = length(values), missing_days = sum(is.na(values)),
                              outside_broad_bounds_days = sum(outside),
                              minimum = if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE),
                              maximum = if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE),
                              status = if (anyNA(values) || any(outside)) "REVIEW_REQUIRED" else "PASS", stringsAsFactors = FALSE)
      }
      if (model == "era5_land") {
        bad <- with(daily, tmin_c > tmean_c + 0.11 | tmean_c > tmax_c + 0.11)
        bad[is.na(bad)] <- FALSE
        qa[[length(qa) + 1L]] <- data.frame(municipality_code = site$municipality_code, api_model = model,
          variable = "tmin_le_tmean_le_tmax", received_unit = "ordering", expected_days = length(expected_dates),
          received_days = nrow(daily), missing_days = 0L, outside_broad_bounds_days = sum(bad),
          minimum = NA_real_, maximum = NA_real_, status = if (any(bad)) "REVIEW_REQUIRED" else "PASS", stringsAsFactors = FALSE)
      }
      list(daily = daily, qa = bind(qa), longitude = lon, latitude = lat,
           grid_elevation_m = elev, coordinate_offset_km = distance)
    }

    manifest <- data.frame(municipality_code = character(), municipality = character(), api_model = character(),
                           public_url = character(), raw_file = character(), raw_md5 = character(),
                           cached = logical(), received_days = integer(), quality_status = character(),
                           requested_longitude = numeric(), requested_latitude = numeric(),
                           returned_longitude = numeric(), returned_latitude = numeric(),
                           returned_grid_elevation_m = numeric(), coordinate_offset_km = numeric(), stringsAsFactors = FALSE)
    quality <- list()
    site_daily <- list()
    write_csv(manifest, file.path(out_dir, "coffee_climate_pilot_requests.csv"))
    for (site_index in seq_len(nrow(download_sites))) {
      site <- download_sites[site_index, , drop = FALSE]
      daily <- data.frame(municipality_code = site$municipality_code, climate_sampling_id = site$climate_sampling_id,
                          date = expected_dates, requested_longitude = site$longitude, requested_latitude = site$latitude,
                          day_definition = "GMT_UTC", stringsAsFactors = FALSE)
      for (model in c("era5_land", "era5")) {
        report$last_stage <<- paste("Pilot", site$municipality_code, model)
        public_url <- make_url(site, model)
        raw_path <- file.path(raw_dir, paste0(site$climate_sampling_id, "_", model, "_2024_", query_hash(public_url), ".json"))
        message("Pilot ", site_index, "/", nrow(download_sites), ": ", site$municipality, " | ", model)
        cached <- retrieve(public_url, raw_path)
        z <- parse_weather(raw_path, model, site)
        daily <- cbind(daily, z$daily[setdiff(names(z$daily), "date")])
        daily[[paste0(model, "_returned_longitude")]] <- z$longitude
        daily[[paste0(model, "_returned_latitude")]] <- z$latitude
        daily[[paste0(model, "_grid_elevation_m")]] <- z$grid_elevation_m
        quality[[length(quality) + 1L]] <- z$qa
        m <- data.frame(municipality_code = site$municipality_code, municipality = site$municipality, api_model = model,
                        public_url = public_url, raw_file = raw_path, raw_md5 = hash(raw_path), cached = cached$cached,
                        received_days = nrow(z$daily), quality_status = if (all(z$qa$status == "PASS")) "PASS" else "REVIEW_REQUIRED",
                        requested_longitude = site$longitude, requested_latitude = site$latitude,
                        returned_longitude = z$longitude, returned_latitude = z$latitude,
                        returned_grid_elevation_m = z$grid_elevation_m, coordinate_offset_km = z$coordinate_offset_km,
                        stringsAsFactors = FALSE)
        manifest <- rbind(manifest, m)
        write_csv(manifest, file.path(out_dir, "coffee_climate_pilot_requests.csv"))
        write_csv(bind(quality), file.path(out_dir, "coffee_climate_pilot_quality.csv"))
        report$requests_validated <<- nrow(manifest)
        report$requests_from_cache <<- sum(manifest$cached)
        save_report()
      }
      site_daily[[site$municipality_code]] <- daily
      report$pilot_site_days <<- sum(vapply(site_daily, nrow, integer(1)))
      write_rds(daily, file.path(target_dir, paste0("coffee_weather_pilot_", site$municipality_code, "_2024.rds")))
    }
    combined <- bind(site_daily)
    combined <- combined[order(combined$municipality_code, combined$date), ]
    write_csv(combined, file.path(target_dir, "coffee_weather_pilot_daily_2024.csv"))
    write_rds(combined, file.path(target_dir, "coffee_weather_pilot_daily_2024.rds"))
    all_quality <- bind(quality)
    report$weather_quality_passed <<- all(all_quality$status == "PASS")
    assert(nrow(combined) == nrow(download_sites) * length(expected_dates), "Pilot output has an unexpected row count.")
    assert(identical(source_md5, hash(source_paths[["audited"]])) && identical(registry_md5, hash(source_paths[["registry"]])),
           "A source input changed during this execution.")
    report$last_stage <<- "Pilot finished; full climate acquisition has not started"
    report$execution_status <<- if (all(all_quality$status == "PASS")) "COMPLETED_PILOT_ONLY" else "PILOT_COMPLETE_REVIEW_REQUIRED"
    save_report()
    message("\nPilot finished: ", nrow(download_sites), " municipal points; ", nrow(combined), " site-days.")
    message("Quality status: ", report$execution_status)
    message("No climate trend, climate attribution, or farm-altitude result has been calculated.")
    message("Send coffee_climate_pilot_summary.csv and coffee_climate_pilot_quality.csv from:\n", out_dir)
    invisible(NULL)
  }

  tryCatch(worker(), error = function(e) {
    report$execution_status <<- "INCOMPLETE"
    err <- conditionMessage(e)
    key <- Sys.getenv("OPEN_METEO_API_KEY", "")
    if (nzchar(key)) err <- gsub(key, "[REDACTED]", err, fixed = TRUE)
    report$error_if_incomplete <<- substr(gsub("[\r\n]+", " ", err), 1L, 500L)
    try(save_report(), silent = TRUE)
    stop("Step 11 stopped. Completed raw requests are retained.\n", report$error_if_incomplete,
         "\nDiagnostic file: ", file.path(out_dir, "coffee_climate_pilot_summary.csv"), call. = FALSE)
  })
}

coffee_climate_step11()
