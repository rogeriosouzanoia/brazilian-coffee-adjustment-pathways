# ======================================================================
# COFFE-STUDY | STEP 12 | HISTORICAL WEATHER AT THE SIX PILOT POINTS
# Run this COMPLETE file with Source in RStudio.
#
# DEFAULT SCOPE: the SAME SIX POINTS used in step 11, 1981-2024.
# This does NOT launch the 1,876-municipality national download.
# The national-scale request burden is exported separately as an estimate.
#
# Explicit sources, daily variables, UTC days and municipal points are
# inherited from step 11. Existing 2024 raw responses are reused unchanged.
# Data are downloaded in calendar blocks, then saved by municipality.
# No municipal production, geometry, weights or previous figures are changed.
#
# Free API: non-commercial use only. Existing environment settings:
#   COFFEE_STUDY_DIR : optional project directory override
#   COFFEE_WEATHER_USE : noncommercial (default) or commercial
#   OPEN_METEO_API_KEY : authorized customer key, when commercial
# Paid access requires a plan that includes the Historical Weather API.
# This script does NOT purchase a plan or initiate any payment.
#
# Rate accounting: long requests can count as MANY provider API calls.
# Conservative LOCAL estimates and rolling budgets are used, with a ledger
# retained between runs. They cannot measure usage by other applications.
# HTTP 429 / 401 / 403 stop the run. No bypass or automatic host switching.
# On a local daily/monthly budget pause, run again after the printed time.
# Do not delete the ledger to reset it. Complete responses are retained.
#
# Documentation checked 2026-09-20:
# https://open-meteo.com/en/docs/historical-weather-api
# https://open-meteo.com/en/pricing
# https://open-meteo.com/en/terms
# ======================================================================

coffee_climate_step12 <- function() {
  # ---- Configuration --------------------------------------------------
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  first_year <- 1981L
  last_year <- 2024L
  block_years <- 5L
  # Deliberately six points. Full national acquisition needs separate sizing.
  local_minute_budget <- 450
  local_hour_budget <- 4000
  local_day_budget <- 8500
  local_31day_budget <- 250000
  cost_safety_factor <- 1.10

  pilot_dir <- file.path(base_dir, "04_outputs", "climate_pilot")
  out_dir <- file.path(base_dir, "04_outputs", "climate_history6")
  raw_dir <- file.path(base_dir, "01_data_raw", "climate", "openmeteo_history6")
  daily_dir <- file.path(base_dir, "02_data_processed", "climate", "history6")
  for (p in c(out_dir, raw_dir, daily_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create directory: ", p, call. = FALSE)
  }
  old_options <- options(timeout = max(240, getOption("timeout", 60)), scipen = 999)
  on.exit(options(old_options), add = TRUE)

  # Avoid two simultaneous runs writing the same quota ledger or cache.
  lock_dir <- file.path(out_dir, ".step12_running")
  if (!dir.create(lock_dir, showWarnings = FALSE)) {
    stop("Step 12 is already running, or an interrupted run left its lock.\n",
         "Check that no other R session is running this script. Only then remove:\n",
         lock_dir, call. = FALSE)
  }
  writeLines(paste("PID", Sys.getpid(), format(Sys.time(), usetz = TRUE)),
             file.path(lock_dir, "owner.txt"))
  on.exit(unlink(lock_dir, recursive = TRUE), add = TRUE)

  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(x, cols, label) {
    absent <- setdiff(cols, names(x))
    assert(!length(absent), paste(label, "is missing:", paste(absent, collapse = ", ")))
  }
  hash <- function(path) unname(tools::md5sum(path))
  read_csv <- function(path) {
    assert(file.exists(path), paste("Required file not found:", path))
    utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE,
                   check.names = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  number <- function(x, label, allow_na = FALSE) {
    a <- as.character(x)
    z <- suppressWarnings(as.numeric(a))
    assert(!any(!is.na(a) & is.na(z)) && all(is.finite(z) | is.na(z)) &&
             (allow_na || !anyNA(z)), paste("Invalid numeric values:", label))
    z
  }
  checked <- function(x, key) {
    need(x, c("check", "result"), "Summary")
    z <- x$result[x$check == key]
    assert(length(z) == 1L && !is.na(z), paste("Missing or repeated check:", key))
    z
  }
  write_csv <- function(x, path) {
    f <- tempfile("coffee12_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(f), add = TRUE)
    utils::write.csv(x, f, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(f, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  write_rds <- function(x, path) {
    f <- tempfile("coffee12_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(f), add = TRUE)
    saveRDS(x, f, compress = "gzip")
    assert(file.copy(f, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  bind <- function(parts) {
    if (!length(parts)) return(data.frame())
    z <- do.call(rbind, parts)
    rownames(z) <- NULL
    z
  }
  api_key <- ""
  redact <- function(x) {
    if (nzchar(api_key)) x <- gsub(api_key, "[REDACTED]", x, fixed = TRUE)
    substr(gsub("[\r\n]+", " ", x), 1, 550)
  }
  pause <- function(reason, seconds = NA_real_) {
    after <- if (is.finite(seconds)) format(Sys.time() + seconds + 3,
                                          "%Y-%m-%d %H:%M:%S %Z", tz = "UTC") else "See provider response"
    stop(structure(list(message = reason, call = NULL, after = after),
                   class = c("coffee_budget_pause", "error", "condition")))
  }

  report <- list(execution_status = "INCOMPLETE", download_scope = "SIX_PILOT_POINTS_ONLY",
    historical_period = paste(first_year, "to", last_year), selected_points = 0,
    planned_source_blocks = 0, validated_source_blocks = 0, reused_pilot_responses = 0,
    reused_history_responses = 0, new_http_attempts_this_run = 0,
    complete_municipal_histories = 0, expected_site_days = 0, complete_site_days = 0,
    local_budget_accounting = "Conservative estimates, not provider billing measurements",
    nationwide_download_launched = FALSE, national_climate_mean_calculated = FALSE,
    climate_trends_calculated = FALSE, climate_attribution_performed = FALSE,
    coffee_field_locations_measured = FALSE, original_inputs_modified = FALSE,
    last_stage = "Loading pilot inputs", resume_after_utc = "Not paused", error_if_incomplete = "None")
  requests <- quality_parts <- annual_parts <- list()
  plan <- coverage <- data.frame()
  save_progress <- function() {
    write_csv(data.frame(check = names(report),
                        result = vapply(report, as.character, character(1)),
                        stringsAsFactors = FALSE),
              file.path(out_dir, "coffee_climate_history_summary.csv"))
    if (length(requests)) write_csv(bind(requests), file.path(out_dir, "coffee_climate_history_requests.csv"))
    if (length(quality_parts)) write_csv(bind(quality_parts), file.path(out_dir, "coffee_climate_history_quality.csv"))
    if (nrow(plan)) write_csv(plan, file.path(out_dir, "coffee_climate_history_download_plan.csv"))
    if (nrow(coverage)) write_csv(coverage, file.path(out_dir, "coffee_climate_history_coverage.csv"))
    if (length(annual_parts)) write_csv(bind(annual_parts), file.path(out_dir, "coffee_climate_history_annual.csv"))
  }
  on.exit({
    try(save_progress(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step12.txt")), silent = TRUE)
  }, add = TRUE)

  worker <- function() {
    inputs <- c(
      pilot_summary = file.path(pilot_dir, "coffee_climate_pilot_summary.csv"),
      pilot_sites = file.path(pilot_dir, "coffee_climate_pilot_sites.csv"),
      pilot_requests = file.path(pilot_dir, "coffee_climate_pilot_requests.csv"),
      pilot_quality = file.path(pilot_dir, "coffee_climate_pilot_quality.csv"),
      dictionary = file.path(pilot_dir, "coffee_climate_variable_dictionary.csv"),
      audited = file.path(base_dir, "02_data_processed", "coffee_brazil_municipality_audited_1974_2024.rds"),
      registry = file.path(base_dir, "02_data_processed", "spatial", "coffee_municipality_spatial_registry.csv"))
    assert(all(file.exists(inputs)), paste("Missing input files:", paste(inputs[!file.exists(inputs)], collapse = "\n")))
    input_hashes <- vapply(inputs, hash, character(1))
    write_csv(data.frame(role = names(inputs), file = unname(inputs), md5 = unname(input_hashes)),
              file.path(out_dir, "coffee_step12_input_provenance.csv"))
    s <- read_csv(inputs[["pilot_summary"]])
    assert(checked(s, "execution_status") == "COMPLETED_PILOT_ONLY" &&
             checked(s, "weather_quality_passed") == "TRUE", "The step 11 pilot has not passed its checks.")
    assert(input_hashes[["audited"]] == checked(s, "input_md5"), "Audited production input changed since step 11.")
    assert(input_hashes[["registry"]] == checked(s, "spatial_registry_md5"), "Spatial registry changed since step 11.")
    sites <- read_csv(inputs[["pilot_sites"]])
    need(sites, c("municipality_code", "municipality", "state", "species", "climate_sampling_id", "longitude", "latitude"), "Pilot sites")
    assert(nrow(sites) == 6L && !anyNA(sites$municipality_code) && !anyDuplicated(sites$municipality_code),
           "Expected the same six distinct step 11 pilot municipal points.")
    for (v in c("longitude", "latitude")) sites[[v]] <- number(sites[[v]], v)
    reg <- read_csv(inputs[["registry"]])
    need(reg, c("municipality_code", "municipality_reference_longitude", "municipality_reference_latitude"), "Spatial registry")
    assert(!anyDuplicated(reg$municipality_code), "Duplicate registry municipal codes.")
    j <- match(sites$municipality_code, reg$municipality_code)
    assert(!anyNA(j), "Pilot point missing from the registry.")
    assert(all(abs(sites$longitude - number(reg$municipality_reference_longitude[j], "registry longitude")) < 1e-7) &&
             all(abs(sites$latitude - number(reg$municipality_reference_latitude[j], "registry latitude")) < 1e-7),
           "Pilot coordinates differ from the unchanged municipal reference points.")

    pm <- read_csv(inputs[["pilot_requests"]])
    need(pm, c("municipality_code", "api_model", "public_url", "raw_file", "raw_md5", "quality_status",
               "returned_longitude", "returned_latitude", "returned_grid_elevation_m"), "Pilot requests")
    assert(nrow(pm) == 12L && !anyDuplicated(paste(pm$municipality_code, pm$api_model)) && all(pm$quality_status == "PASS"),
           "The pilot request manifest must have twelve unique passed requests.")
    pq <- read_csv(inputs[["pilot_quality"]])
    need(pq, c("status", "missing_days", "outside_broad_bounds_days"), "Pilot quality")
    assert(all(pq$status == "PASS") && all(number(pq$missing_days, "missing days") == 0) &&
             all(number(pq$outside_broad_bounds_days, "outside bounds") == 0), "Pilot quality requires review.")
    dictionary <- read_csv(inputs[["dictionary"]])
    need(dictionary, c("api_model", "api_variable", "output_column", "expected_unit", "lower_check", "upper_check"), "Variable dictionary")
    expected_variables <- c("temperature_2m_mean", "temperature_2m_min", "temperature_2m_max", "relative_humidity_2m_mean",
                            "vapour_pressure_deficit_max", "precipitation_sum", "shortwave_radiation_sum",
                            "wind_speed_10m_mean", "et0_fao_evapotranspiration")
    assert(identical(dictionary$api_variable, expected_variables) &&
             identical(dictionary$api_model, c(rep("era5_land", 5), rep("era5", 4))) &&
             !anyDuplicated(dictionary$output_column), "The step 11 variable/source definition has changed.")
    for (v in c("lower_check", "upper_check")) dictionary[[v]] <- number(dictionary[[v]], v)
    for (v in c("returned_longitude", "returned_latitude")) pm[[v]] <- number(pm[[v]], v)
    pm$returned_grid_elevation_m <- number(pm$returned_grid_elevation_m, "grid elevation", TRUE)
    expected_dates <- seq(as.Date(sprintf("%d-01-01", first_year)), as.Date(sprintf("%d-12-31", last_year)), by = "day")
    report$selected_points <<- nrow(sites)
    report$expected_site_days <<- nrow(sites) * length(expected_dates)

    for (package in c("curl", "jsonlite")) {
      if (!requireNamespace(package, quietly = TRUE)) utils::install.packages(package, repos = "https://cloud.r-project.org")
      assert(requireNamespace(package, quietly = TRUE), paste("Required R package is unavailable:", package))
    }
    use <- tolower(Sys.getenv("COFFEE_WEATHER_USE", "noncommercial"))
    assert(use %in% c("noncommercial", "commercial"), "Invalid COFFEE_WEATHER_USE setting.")
    api_key <<- if (use == "commercial") Sys.getenv("OPEN_METEO_API_KEY", "") else ""
    assert(use != "commercial" || nzchar(api_key), "Authorized historical customer access requires OPEN_METEO_API_KEY.")
    endpoint <- if (use == "commercial") "https://customer-archive-api.open-meteo.com/v1/archive" else "https://archive-api.open-meteo.com/v1/archive"
    report$api_access <<- use
    # Keep exactly the step 11 parameter order and seven-decimal coordinates.
    make_url <- function(site, model, start, end) {
      v <- dictionary$api_variable[dictionary$api_model == model]
      args <- list(latitude = sprintf("%.7f", site$latitude), longitude = sprintf("%.7f", site$longitude),
        start_date = start, end_date = end, daily = paste(v, collapse = ","), models = model,
        timezone = "GMT", elevation = "nan", cell_selection = "nearest", temperature_unit = "celsius",
        precipitation_unit = "mm", wind_speed_unit = "ms", timeformat = "iso8601")
      vals <- vapply(args, function(a) utils::URLencode(as.character(a), reserved = TRUE), character(1))
      paste0(endpoint, "?", paste(paste(names(args), vals, sep = "="), collapse = "&"))
    }
    query_hash <- function(url) {
      f <- tempfile("coffee12_query_")
      on.exit(unlink(f), add = TRUE)
      writeBin(charToRaw(enc2utf8(url)), f)
      hash(f)
    }
    cost <- function(n_days, n_variables) ceiling(cost_safety_factor * max(1, n_days / 14) * max(1, n_variables / 10))
    block_start_year <- seq.int(first_year, last_year - 1L, by = block_years)
    periods <- data.frame(start = sprintf("%d-01-01", block_start_year),
      end = sprintf("%d-12-31", pmin(block_start_year + block_years - 1L, last_year - 1L)), stringsAsFactors = FALSE)
    # Standalone 2024 block permits EXACT reuse of the successful pilot queries.
    periods <- rbind(data.frame(start = "2024-01-01", end = "2024-12-31"), periods)
    parts <- list()
    for (ii in seq_len(nrow(sites))) for (kk in seq_len(nrow(periods))) for (model in c("era5_land", "era5")) {
      site <- sites[ii, , drop = FALSE]
      url <- make_url(site, model, periods$start[kk], periods$end[kk])
      nd <- as.integer(as.Date(periods$end[kk]) - as.Date(periods$start[kk])) + 1L
      nv <- sum(dictionary$api_model == model)
      parts[[length(parts) + 1L]] <- data.frame(municipality_code = site$municipality_code, municipality = site$municipality,
        api_model = model, start_date = periods$start[kk], end_date = periods$end[kk], expected_days = nd,
        public_url = url, raw_file = file.path(raw_dir, paste0(site$climate_sampling_id, "_", model, "_",
          periods$start[kk], "_", periods$end[kk], "_", query_hash(url), ".json")),
        estimated_call_units = cost(nd, nv), status = "PENDING", stringsAsFactors = FALSE)
    }
    plan <<- bind(parts)
    report$planned_source_blocks <<- nrow(plan)
    coverage <<- data.frame(municipality_code = sites$municipality_code, municipality = sites$municipality,
      expected_years = last_year - first_year + 1L, expected_days = length(expected_dates),
      validated_era5_land_days = 0L, validated_era5_days = 0L, complete_daily_file = FALSE,
      daily_file = NA_character_, status = "PENDING", stringsAsFactors = FALSE)
    n_targets <- number(checked(s, "climate_target_municipal_codes"), "target count")
    # These are estimates, not provider invoices or promised completion times.
    sizing <- data.frame(scope = c("Six historical pilot points", "All step11 target points, no grid deduplication"),
      points = c(6L, n_targets), years = last_year - first_year + 1L,
      site_days = c(6L, n_targets) * length(expected_dates), explicit_api_models = 2L,
      approximate_provider_units_before_margin = c(6L, n_targets) * length(expected_dates) / 14 * 2,
      national_download_authorized_by_this_script = FALSE,
      note = "Linear planning estimate from published long-request counting; actual provider accounting may differ")
    write_csv(sizing, file.path(out_dir, "coffee_climate_acquisition_size_estimate.csv"))
    write_csv(sites, file.path(out_dir, "coffee_climate_history_sites.csv"))

    notes <- c("COFFE-STUDY | STEP 12 | SIX-POINT HISTORICAL SERIES",
      paste("Executed:", format(Sys.time(), usetz = TRUE)),
      paste("Period:", first_year, "to", last_year),
      "Scope: the same six demonstration points from step 11, not a national sample.",
      "The full 1,876-target acquisition is NOT launched. An indicative sizing table is exported.",
      "A municipal reference point is neither a coffee-field coordinate nor a polygon average.",
      "Source definitions and all nine variables are inherited from the step 11 dictionary.",
      "ERA5-Land: daily mean/min/max temperature, mean RH, daily maximum VPD.",
      "ERA5: precipitation, radiation, wind, provider-computed reference ET0.",
      "Sources remain distinct. No Best Match, source substitution, elevation correction or interpolation.",
      "Dates refer to GMT/UTC calendar days. Returned grid coordinates are checked against the pilot.",
      "Grid elevation is NOT coffee-field elevation. This step does not test uphill movement.",
      "Successful pilot JSON is reused by exact public URL and checked by MD5.",
      "History raw cache uses exact query identity and response hashes. Missing values stay missing.",
      "Broad bounds and temperature ordering are screening checks, not validation against weather stations.",
      "Histories requiring review are preserved but are not passed to complete-series outputs.",
      "Full daily files are written only after all requested dates/variables pass for one site.",
      "Annual summaries use complete UTC years; VPD is the mean of DAILY MAXIMUM VPD.",
      "Precipitation, radiation and ET0 are sums of daily values; wind/RH/temperature are daily-value means.",
      "No soil water balance, crop evapotranspiration, climate trends, attribution or regional means are calculated.",
      "No use of six-site averages as national estimates is permitted by this workflow.",
      "Cost estimates use 1.10 * max(1, days/14) * max(1, variables/10), rounded up per request.",
      "These estimates are conservative planning counters, not a verified account usage meter.",
      "Rolling local budgets count this script and identifiable pilot calls, not other account/IP activity.",
      "Minute/hour limits cause waits while the local R process is running; daily/monthly budgets cause a resumable pause.",
      "Provider 429 or access blocks stop processing. No host or key rotation is performed.",
      "Free service is for non-commercial use; customer access requires an authorized historical-data plan.",
      "All earlier production, spatial, pilot and figure files remain unchanged.",
      "https://open-meteo.com/en/docs/historical-weather-api",
      "https://open-meteo.com/en/pricing", "https://open-meteo.com/en/terms")
    writeLines(notes, file.path(out_dir, "coffee_climate_history_method_notes.txt"), useBytes = TRUE)
    save_progress()
    message("\nSTEP 12: six municipal points, 1981-2024. National download is NOT launched.")
    message("2024 cache is reused. New requests cover 1981-2023 in calendar blocks.")
    message("Free-access long requests count as multiple API calls. Local quota pauses are expected.")

    # ---- Persistent conservative rate accounting ----------------------
    ledger_file <- file.path(out_dir, "coffee_openmeteo_usage_ledger.csv")
    ledger <- if (file.exists(ledger_file)) read_csv(ledger_file) else
      data.frame(time_unix = numeric(), estimated_call_units = numeric(), api_access = character(),
                 event = character(), public_url = character(), stringsAsFactors = FALSE)
    need(ledger, c("time_unix", "estimated_call_units", "api_access", "event", "public_url"), "Usage ledger")
    ledger$time_unix <- number(ledger$time_unix, "ledger time")
    ledger$estimated_call_units <- number(ledger$estimated_call_units, "ledger cost")
    assert(all(ledger$estimated_call_units >= 0), "Negative ledger usage.")
    # Include recognizable pilot calls in local limits; mtime is only a conservative local proxy.
    for (ii in seq_len(nrow(pm))) {
      id <- pm$public_url[ii]
      if (!id %in% ledger$public_url && file.exists(pm$raw_file[ii])) {
        pilot_mode <- if (grepl("^https://customer-", id)) "commercial" else "noncommercial"
        ledger <- rbind(ledger, data.frame(time_unix = as.numeric(file.info(pm$raw_file[ii])$mtime),
          estimated_call_units = cost(366L, if (pm$api_model[ii] == "era5_land") 5L else 4L),
          api_access = pilot_mode, event = "PILOT_MTIME_ESTIMATE", public_url = id))
      }
    }
    write_csv(ledger, ledger_file)
    wait_for_window <- function(units, span, cap) {
      now <- as.numeric(Sys.time())
      active <- ledger[ledger$api_access == use & ledger$time_unix > now - span, , drop = FALSE]
      if (!nrow(active) || sum(active$estimated_call_units) + units <= cap) return(0)
      active <- active[order(active$time_unix), , drop = FALSE]
      remain <- sum(active$estimated_call_units)
      for (ii in seq_len(nrow(active))) {
        remain <- remain - active$estimated_call_units[ii]
        if (remain + units <= cap) return(max(0, active$time_unix[ii] + span - now) + 3)
      }
      span + 3
    }
    reserve_call <- function(units, url) {
      if (use == "noncommercial") {
        assert(units <= local_minute_budget, "A single block exceeds the local per-minute budget. Reduce block_years.")
        repeat {
          day_wait <- wait_for_window(units, 86400, local_day_budget)
          month_wait <- wait_for_window(units, 31 * 86400, local_31day_budget)
          if (day_wait > 0 || month_wait > 0) pause("Local rolling daily/monthly usage budget reached. Cache is retained.", max(day_wait, month_wait))
          seconds <- max(wait_for_window(units, 60, local_minute_budget),
                         wait_for_window(units, 3600, local_hour_budget))
          if (seconds <= 0) break
          message("Rate-control wait: ", ceiling(seconds), " seconds. You may stop R and resume later.")
          Sys.sleep(ceiling(seconds))
        }
      } else {
        # Serial customer requests only; subscription quota remains the account holder's responsibility.
        Sys.sleep(1)
      }
      ledger <<- rbind(ledger, data.frame(time_unix = as.numeric(Sys.time()), estimated_call_units = units,
                       api_access = use, event = "HTTP_ATTEMPT_RESERVED", public_url = url))
      write_csv(ledger, ledger_file)
      report$new_http_attempts_this_run <<- report$new_http_attempts_this_run + 1L
    }

    normalize_unit <- function(x) {
      x <- gsub("\u00b2", "2", as.character(x), fixed = TRUE)
      x <- gsub("\u00b0", "", x, fixed = TRUE)
      tolower(gsub("[[:space:]]+", "", x))
    }
    scalar <- function(x, label) {
      assert(is.numeric(x) && length(x) == 1L && is.finite(x), paste("Invalid response", label))
      as.numeric(x)
    }
    parse_weather <- function(path, job, site, anchor) {
      obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
      assert(!isTRUE(obj$error) && is.list(obj$daily), "Raw JSON is not a successful daily weather response.")
      dates <- seq(as.Date(job$start_date), as.Date(job$end_date), by = "day")
      tt <- unlist(obj$daily$time, use.names = FALSE)
      assert(is.character(tt) && identical(tt, format(dates, "%Y-%m-%d")), "Incomplete, duplicate, reordered or unexpected dates.")
      assert(scalar(obj$utc_offset_seconds, "UTC offset") == 0, "Response uses non-UTC days.")
      lon <- scalar(obj$longitude, "longitude")
      lat <- scalar(obj$latitude, "latitude")
      elev <- if (is.null(obj$elevation)) NA_real_ else scalar(obj$elevation, "elevation")
      assert(abs(lon - anchor$returned_longitude) < 1e-4 && abs(lat - anchor$returned_latitude) < 1e-4,
             paste("Returned grid cell changed relative to the 2024 pilot:", site$municipality, job$api_model))
      assert(is.na(elev) == is.na(anchor$returned_grid_elevation_m), "Grid elevation availability changed relative to pilot.")
      if (!is.na(elev)) assert(abs(elev - anchor$returned_grid_elevation_m) <= 0.51, "Returned grid elevation changed relative to pilot.")
      vd <- dictionary[dictionary$api_model == job$api_model, , drop = FALSE]
      daily <- data.frame(date = dates)
      qa <- list()
      for (ii in seq_len(nrow(vd))) {
        variable <- vd$api_variable[ii]
        key <- if (variable %in% names(obj$daily)) variable else paste0(variable, "_", job$api_model)
        assert(key %in% names(obj$daily), paste("Missing variable:", variable))
        vals <- obj$daily[[key]]
        assert(length(vals) == length(dates), paste("Incorrect array length:", variable))
        values <- vapply(vals, function(v) {
          if (is.null(v)) return(NA_real_)
          scalar(v, variable)
        }, numeric(1))
        unit <- obj$daily_units[[key]]
        if (is.null(unit)) unit <- obj$daily_units[[variable]]
        assert(length(unit) == 1L && normalize_unit(unit) == normalize_unit(vd$expected_unit[ii]), paste("Unexpected unit:", variable))
        outside <- !is.na(values) & (values < vd$lower_check[ii] - 1e-6 | values > vd$upper_check[ii] + 1e-6)
        qa[[ii]] <- data.frame(municipality_code = site$municipality_code, api_model = job$api_model,
          start_date = job$start_date, end_date = job$end_date, variable = variable, received_unit = as.character(unit),
          expected_days = length(dates), received_days = length(values), missing_days = sum(is.na(values)),
          outside_broad_bounds_days = sum(outside),
          minimum = if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE),
          maximum = if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE),
          status = if (anyNA(values) || any(outside)) "REVIEW_REQUIRED" else "PASS", stringsAsFactors = FALSE)
        daily[[vd$output_column[ii]]] <- values
      }
      if (job$api_model == "era5_land") {
        missing <- !stats::complete.cases(daily[c("tmin_c", "tmean_c", "tmax_c")])
        bad <- with(daily, tmin_c > tmean_c + 0.11 | tmean_c > tmax_c + 0.11)
        bad[is.na(bad)] <- FALSE
        qa[[length(qa) + 1L]] <- data.frame(municipality_code = site$municipality_code, api_model = job$api_model,
          start_date = job$start_date, end_date = job$end_date, variable = "tmin_le_tmean_le_tmax", received_unit = "ordering",
          expected_days = length(dates), received_days = nrow(daily), missing_days = sum(missing),
          outside_broad_bounds_days = sum(bad), minimum = NA_real_, maximum = NA_real_,
          status = if (any(missing) || any(bad)) "REVIEW_REQUIRED" else "PASS", stringsAsFactors = FALSE)
      }
      list(daily = daily, qa = bind(qa), longitude = lon, latitude = lat, elevation = elev)
    }

    retrieve <- function(job) {
      # Exact same public query only. Never overwrite a successful pilot file.
      pidx <- match(job$public_url, pm$public_url)
      if (!is.na(pidx)) {
        assert(file.exists(pm$raw_file[pidx]), paste("Pilot raw file is missing; restore it before proceeding:", pm$raw_file[pidx]))
        assert(hash(pm$raw_file[pidx]) == pm$raw_md5[pidx], "Pilot raw response checksum differs from its manifest.")
        return(list(path = pm$raw_file[pidx], reuse = "PILOT_CACHE"))
      }
      path <- job$raw_file
      sidecar <- paste0(path, ".rds")
      if (file.exists(path)) {
        assert(file.exists(sidecar), paste("Unverified cache file has no query sidecar. Preserve and review:", path))
        m <- readRDS(sidecar)
        assert(identical(m$public_url, job$public_url) && identical(m$raw_md5, hash(path)), "Historical raw cache identity/hash mismatch.")
        return(list(path = path, reuse = "HISTORY_CACHE"))
      }
      actual_url <- if (nzchar(api_key)) paste0(job$public_url, "&apikey=", utils::URLencode(api_key, reserved = TRUE)) else job$public_url
      last_error <- "No response"
      for (attempt in 1:3) {
        reserve_call(job$estimated_call_units, job$public_url)
        h <- curl::new_handle()
        curl::handle_setopt(h, connecttimeout = 25, timeout = 240, followlocation = FALSE,
                            useragent = "CoffeeClimateReview-Step12/1.0")
        curl::handle_setheaders(h, Accept = "application/json")
        r <- tryCatch(curl::curl_fetch_memory(actual_url, handle = h), error = function(e) e)
        if (inherits(r, "error")) {
          last_error <- redact(conditionMessage(r))
        } else {
          status <- as.integer(r$status_code)
          text <- rawToChar(r$content)
          if (status == 429L) pause("Provider rate limit: HTTP 429. Stop and respect its reset period. No automatic retry or endpoint change.")
          if (status %in% c(401L, 403L)) stop("Provider denied access: HTTP ", status, ". No bypass attempted.", call. = FALSE)
          if (status == 200L) {
            assert(!grepl("^\\s*<", text) && jsonlite::validate(text), "API returned HTML or invalid JSON.")
            obj <- jsonlite::fromJSON(text, simplifyVector = FALSE)
            assert(!isTRUE(obj$error) && !is.null(obj$daily), paste("API error:", redact(paste(obj$reason, collapse = " "))))
            f <- tempfile("coffee12_raw_", tmpdir = raw_dir, fileext = ".json")
            writeBin(r$content, f)
            assert(file.copy(f, path, overwrite = FALSE), paste("Cannot save raw JSON:", path))
            unlink(f)
            write_rds(list(public_url = job$public_url, raw_md5 = hash(path),
                           downloaded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)), sidecar)
            return(list(path = path, reuse = "NEW_DOWNLOAD"))
          }
          last_error <- paste("HTTP", status)
          if (jsonlite::validate(text)) {
            reason <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE)$reason, error = function(e) NULL)
            if (!is.null(reason)) last_error <- paste(last_error, redact(as.character(reason)))
          }
          if (!status %in% c(408L, 500L, 502L, 503L, 504L)) stop(last_error, call. = FALSE)
        }
        if (attempt < 3L) Sys.sleep(c(5, 15)[attempt])
      }
      stop("Download failed after three attempts: ", last_error, call. = FALSE)
    }

    # ---- Acquisition and complete-site assembly -----------------------
    for (site_index in seq_len(nrow(sites))) {
      site <- sites[site_index, , drop = FALSE]
      site_rows <- which(plan$municipality_code == site$municipality_code)
      source_parts <- list(era5_land = list(), era5 = list())
      for (jj in site_rows) {
        job <- plan[jj, , drop = FALSE]
        anchor <- pm[pm$municipality_code == site$municipality_code & pm$api_model == job$api_model, , drop = FALSE]
        assert(nrow(anchor) == 1L, "Missing or ambiguous source/grid reference in pilot manifest.")
        report$last_stage <<- paste(site$municipality, job$api_model, job$start_date, "to", job$end_date)
        message("[", jj, "/", nrow(plan), "] ", report$last_stage)
        cached <- retrieve(job)
        z <- parse_weather(cached$path, job, site, anchor)
        quality_parts[[length(quality_parts) + 1L]] <<- z$qa
        valid <- all(z$qa$status == "PASS")
        requests[[length(requests) + 1L]] <<- data.frame(municipality_code = site$municipality_code, municipality = site$municipality,
          api_model = job$api_model, start_date = job$start_date, end_date = job$end_date, public_url = job$public_url,
          raw_file = cached$path, raw_md5 = hash(cached$path), cache_status = cached$reuse,
          received_days = nrow(z$daily), quality_status = if (valid) "PASS" else "REVIEW_REQUIRED",
          returned_longitude = z$longitude, returned_latitude = z$latitude, returned_grid_elevation_m = z$elevation,
          estimated_call_units_if_downloaded = job$estimated_call_units, stringsAsFactors = FALSE)
        plan$status[jj] <<- if (valid) "VALIDATED" else "REVIEW_REQUIRED"
        plan$raw_file[jj] <<- cached$path
        report$validated_source_blocks <<- sum(plan$status == "VALIDATED")
        report$reused_pilot_responses <<- report$reused_pilot_responses + as.integer(cached$reuse == "PILOT_CACHE")
        report$reused_history_responses <<- report$reused_history_responses + as.integer(cached$reuse == "HISTORY_CACHE")
        if (valid) {
          dest <- paste0("validated_", job$api_model, "_days")
          coverage[[dest]][site_index] <<- coverage[[dest]][site_index] + nrow(z$daily)
        }
        save_progress()
        assert(valid, paste("History quality needs review. Raw data preserved:", cached$path))
        model <- job$api_model
        source_parts[[model]][[length(source_parts[[model]]) + 1L]] <- z$daily
      }
      daily <- data.frame(municipality_code = site$municipality_code, municipality = site$municipality,
        state = site$state, pilot_selection_species = site$species, climate_sampling_id = site$climate_sampling_id,
        date = expected_dates, requested_longitude = site$longitude, requested_latitude = site$latitude,
        day_definition = "GMT_UTC", coordinate_definition = "Municipal_interior_point_from_step05", stringsAsFactors = FALSE)
      for (model in c("era5_land", "era5")) {
        d <- bind(source_parts[[model]])
        d <- d[order(d$date), , drop = FALSE]
        assert(!anyDuplicated(d$date) && identical(as.character(d$date), as.character(expected_dates)), "Full-site dates are incomplete or overlapping.")
        cols <- dictionary$output_column[dictionary$api_model == model]
        assert(!anyNA(d[cols]), "Missing data in an apparently complete site.")
        daily <- cbind(daily, d[cols])
        anchor <- pm[pm$municipality_code == site$municipality_code & pm$api_model == model, , drop = FALSE]
        daily[[paste0(model, "_returned_longitude")]] <- anchor$returned_longitude
        daily[[paste0(model, "_returned_latitude")]] <- anchor$returned_latitude
        daily[[paste0(model, "_grid_elevation_m")]] <- anchor$returned_grid_elevation_m
      }
      rownames(daily) <- NULL
      output_file <- file.path(daily_dir, paste0("coffee_weather_history_", site$municipality_code, "_", first_year, "_", last_year, ".rds"))
      write_rds(daily, output_file)
      annual <- lapply(seq.int(first_year, last_year), function(year) {
        d <- daily[format(daily$date, "%Y") == as.character(year), , drop = FALSE]
        data.frame(municipality_code = site$municipality_code, municipality = site$municipality, state = site$state,
          pilot_selection_species = site$species, year = year, observed_days = nrow(d),
          tmean_c = mean(d$tmean_c), tmin_mean_c = mean(d$tmin_c), tmax_mean_c = mean(d$tmax_c),
          annual_minimum_tmin_c = min(d$tmin_c), annual_maximum_tmax_c = max(d$tmax_c),
          rhmean_pct = mean(d$rhmean_pct), mean_of_daily_max_vpd_kpa = mean(d$vpdmax_kpa),
          precipitation_mm = sum(d$precipitation_mm), shortwave_mj_m2 = sum(d$shortwave_mj_m2),
          wind10m_mean_m_s = mean(d$wind10m_mean_m_s), et0_reference_mm = sum(d$et0_reference_mm),
          national_estimate = FALSE, climate_attribution_performed = FALSE, stringsAsFactors = FALSE)
      })
      annual_parts[[site$municipality_code]] <<- bind(annual)
      coverage$complete_daily_file[site_index] <<- TRUE
      coverage$daily_file[site_index] <<- output_file
      coverage$status[site_index] <<- "COMPLETE_HISTORY"
      report$complete_municipal_histories <<- sum(coverage$complete_daily_file)
      report$complete_site_days <<- sum(coverage$complete_daily_file) * length(expected_dates)
      message("Complete daily history saved: ", site$municipality)
      save_progress()
    }
    assert(identical(vapply(inputs, hash, character(1)), input_hashes), "A prior-stage input changed during execution.")
    assert(all(plan$status == "VALIDATED") && all(coverage$complete_daily_file), "Some historical downloads are incomplete.")
    report$execution_status <<- "COMPLETED_SIX_POINT_HISTORY"
    report$last_stage <<- "Six complete historical point series saved. Nationwide acquisition not launched."
    save_progress()
    message("\nCOMPLETE: six points, ", length(expected_dates), " days per point; ", report$complete_site_days, " site-days.")
    message("Send coffee_climate_history_summary.csv, coffee_climate_history_coverage.csv and coffee_climate_history_annual.csv from:\n", out_dir)
    invisible(report)
  }

  tryCatch(worker(),
    coffee_budget_pause = function(e) {
      report$execution_status <<- "PAUSED_USAGE_LIMIT"
      report$error_if_incomplete <<- redact(conditionMessage(e))
      report$resume_after_utc <<- e$after
      save_progress()
      message("\nPAUSED, NOT COMPLETE: ", report$error_if_incomplete)
      message("Resume after (UTC): ", e$after, "\nRun this same file again. Verified caches will be reused.")
      message("Progress report: ", file.path(out_dir, "coffee_climate_history_summary.csv"))
      invisible(report)
    },
    error = function(e) {
      report$execution_status <<- "INCOMPLETE_REVIEW_REQUIRED"
      report$error_if_incomplete <<- redact(conditionMessage(e))
      try(save_progress(), silent = TRUE)
      stop("Step 12 stopped. Completed raw responses are retained.\n", report$error_if_incomplete,
           "\nReport: ", file.path(out_dir, "coffee_climate_history_summary.csv"), call. = FALSE)
    })
}

coffee_climate_step12()
