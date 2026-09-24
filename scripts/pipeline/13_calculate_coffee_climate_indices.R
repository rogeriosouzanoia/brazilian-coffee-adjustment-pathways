# ======================================================================
# COFFE-STUDY | STEP 13 | DAILY CLIMATE INDICATORS, SIX NASA POWER POINTS
# Run this complete file with Source in RStudio. Requires only base R.
# No downloads, package installation, climate-source mixing or new figures.
# Source observations and outputs from steps 01-12 remain unchanged.
#
# Meteorology: 1981-2024. Solar-dependent calculations: 1984-2024.
# Daily UTC data are retained as UTC, not shifted to local civil dates.
# NASA mean temperature is retained for climate summaries. FAO-56 ET0 uses
# (Tmax+Tmin)/2 as its standardized mean temperature, saved separately.
#
# IMPORTANT INTERPRETATION
# * The six municipal reference points are not a national sample or farms.
# * Heat/cold/rain thresholds describe weather, not validated coffee damage.
# * VPD is a daily-input approximation, NOT observed daily maximum VPD or
#   the exact average of hourly VPD. ea is estimated from mean dewpoint.
# * ET0 is reference-grass atmospheric demand, NOT actual coffee water use.
# * P-ET0 is climatic water balance, NOT soil storage or crop water stress.
# * No soil-water balance, irrigation requirement or causal effect is fit.
# * Solar, ET0 and P-ET0 preserve SRB/CERES epochs. No trend is fit across
#   the 2001 radiation-source change. No ET0 is fabricated before 1984.
#
# METHODS
# Allen et al. (1998), FAO Irrigation and Drainage Paper 56, chapters 2-4:
# https://www.fao.org/4/x0490e/x0490e06.htm
# https://www.fao.org/4/x0490e/x0490e07.htm
# https://www.fao.org/4/x0490e/x0490e08.htm
# https://www.climdex.org/learn/indices/
# https://power.larc.nasa.gov/docs/methodology/data/sources/
#
# Explicit numerical policies (not adjustments to downloaded observations):
# 1. Rs/Rso for net-longwave estimation is bounded to [0.30,1.00]. This is
#    a declared implementation choice. Original ratio, adjustment flags and
#    a sensitivity ET0 with only the FAO upper bound are all retained.
# 2. Negative estimated VPD is flagged and set to NA in usable VPD/ET0.
#    No replacement from RH or temperature is made. Raw values are saved.
# 3. Negative calculated ET0 is retained in *_raw_mm and set to zero in
#    the demand column, with a flag. This is not an estimate of dewfall.
# 4. Missing/physically invalid inputs propagate to dependent indicators.
# 5. Period totals, counts and maxima require every expected day usable.
#    Dry/hot spells and 5-day rain windows are bounded by each month/year.
#    These period-bounded variants must not be called a complete ETCCDI
#    implementation. No cross-year or cross-month spell is stitched here.
# 6. Broad screening bounds are inherited from step12, not crop thresholds.
# ======================================================================

coffee_climate_step13 <- function() {
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  step12_dir <- file.path(base_dir, "04_outputs", "climate_nasa_power")
  daily_dir <- file.path(base_dir, "02_data_processed", "climate", "NASA_POWER", "history6")
  out_dir <- file.path(base_dir, "04_outputs", "climate_nasa_power_indices")
  derived_dir <- file.path(base_dir, "02_data_processed", "climate", "NASA_POWER", "indices6")
  first_year <- 1981L
  last_year <- 2024L
  baseline_years <- 1981:2010
  comparison_initial <- 2012:2015
  comparison_recent <- 2021:2024
  cloud_ratio_lower <- 0.30
  cloud_ratio_upper <- 1.00
  script_version <- "13_NASA_POWER_daily_indices_v1"
  # Test-only switch skips all project reads/writes and checks the functions.
  self_test_only <- identical(toupper(Sys.getenv("COFFEE_INDICES_SELF_TEST_ONLY", "FALSE")), "TRUE")

  assert <- function(x, text) if (!isTRUE(x)) stop(text, call. = FALSE)
  need <- function(x, cols, label) {
    missing <- setdiff(cols, names(x))
    assert(!length(missing), paste(label, "is missing:", paste(missing, collapse = ", ")))
  }
  hash <- function(path) unname(tools::md5sum(path))
  read_csv <- function(path) {
    assert(file.exists(path), paste("Required input not found:", path))
    utils::read.csv(path, colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  number <- function(x, label, allow_na = FALSE) {
    old <- as.character(x)
    val <- suppressWarnings(as.numeric(old))
    assert(!any(!is.na(old) & is.na(val)) && all(is.na(val) | is.finite(val)) &&
      (allow_na || !anyNA(val)), paste("Invalid numeric input:", label))
    val
  }
  check_value <- function(x, key) {
    need(x, c("check", "result"), "Summary")
    val <- x$result[x$check == key]
    assert(length(val) == 1L && !is.na(val), paste("Missing or ambiguous check:", key))
    val
  }
  bind <- function(x) {
    if (!length(x)) return(data.frame())
    ans <- do.call(rbind, x)
    rownames(ans) <- NULL
    ans
  }
  write_csv <- function(x, path) {
    tmp <- tempfile("indices13_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save", path))
  }
  write_rds <- function(x, path) {
    tmp <- tempfile("indices13_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save", path))
  }
  complete <- function(x) length(x) > 0L && all(is.finite(x))
  strict <- function(x, fun) if (complete(x)) as.numeric(fun(x)) else NA_real_
  longest_run <- function(x) {
    if (!length(x) || anyNA(x)) return(NA_real_)
    r <- rle(as.logical(x))
    if (!any(r$values)) return(0)
    as.numeric(max(r$lengths[r$values]))
  }
  rain5 <- function(x) {
    if (length(x) < 5L || !complete(x)) return(NA_real_)
    cs <- c(0, cumsum(x))
    max(cs[6:length(cs)] - cs[1:(length(cs) - 5L)])
  }
  es <- function(t) 0.6108 * exp(17.27 * t / (t + 237.3))
  solar_geometry <- function(date, latitude, z) {
    J <- as.integer(format(date, "%j"))
    phi <- latitude * pi / 180
    dr <- 1 + 0.033 * cos(2 * pi * J / 365)
    delta <- 0.409 * sin(2 * pi * J / 365 - 1.39)
    ws <- acos(pmin(1, pmax(-1, -tan(phi) * tan(delta))))
    ra <- (24 * 60 / pi) * 0.0820 * dr *
      (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))
    data.frame(ra_mj_m2 = pmax(ra, 0), rso_mj_m2 = (0.75 + 2e-5 * z) * pmax(ra, 0))
  }
  pm <- function(tmin, tmax, dewpoint, wind2m, pressure, rs, ra, rso) {
    t <- (tmin + tmax) / 2
    ea <- es(dewpoint)
    sat <- (es(tmin) + es(tmax)) / 2
    vpd_raw <- sat - ea
    vpd <- vpd_raw
    vpd[!is.na(vpd_raw) & vpd_raw < 0] <- NA_real_
    slope <- 4098 * es(t) / (t + 237.3)^2
    gamma <- 0.000665 * pressure
    ratio_raw <- rs / rso
    ratio_used <- pmin(cloud_ratio_upper, pmax(cloud_ratio_lower, ratio_raw))
    emissivity <- 0.34 - 0.14 * sqrt(ea)
    thermal <- 4.903e-9 * ((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2
    longwave <- thermal * emissivity * (1.35 * ratio_used - 0.35)
    longwave_unfloored <- thermal * emissivity * (1.35 * pmin(1, ratio_raw) - 0.35)
    net_sw <- 0.77 * rs
    net <- net_sw - longwave
    net_unfloored <- net_sw - longwave_unfloored
    denom <- slope + gamma * (1 + 0.34 * wind2m)
    calc_et <- function(rn) (0.408 * slope * rn + gamma * 900 / (t + 273) * wind2m * vpd) / denom
    raw <- calc_et(net)
    raw_unfloored <- calc_et(net_unfloored)
    bad_domain <- (!is.na(tmin) & !is.na(tmax) & tmin > tmax) |
      (!is.na(rso) & rso <= 0) | (!is.na(emissivity) & emissivity < 0) |
      (!is.na(wind2m) & wind2m < 0) | (!is.na(pressure) & pressure <= 0) |
      (!is.na(rs) & rs < 0)
    raw[bad_domain] <- NA_real_
    raw_unfloored[bad_domain] <- NA_real_
    data.frame(tmean_fao56_c = t, actual_vapour_pressure_est_kpa = ea,
      saturation_vapour_pressure_fao56_kpa = sat, vpd_fao56_raw_kpa = vpd_raw,
      vpd_fao56_estimated_kpa = vpd, ra_mj_m2 = ra, rso_mj_m2 = rso,
      rs_rso_raw = ratio_raw, rs_rso_used = ratio_used,
      net_longwave_estimated_mj_m2 = longwave, net_radiation_estimated_mj_m2 = net,
      et0_fao56_raw_mm = raw, et0_reference_mm = pmax(raw, 0),
      et0_no_lower_cloud_bound_mm = pmax(raw_unfloored, 0),
      flag_negative_vpd = !is.na(vpd_raw) & vpd_raw < 0,
      flag_cloud_ratio_lower_bound = !is.na(ratio_raw) & ratio_raw < cloud_ratio_lower,
      flag_cloud_ratio_upper_bound = !is.na(ratio_raw) & ratio_raw > cloud_ratio_upper,
      flag_negative_et0_raw = !is.na(raw) & raw < 0,
      flag_et0_formula_domain = bad_domain)
  }

  self_tests <- function() {
    ea <- (es(12.3) * 0.84 + es(21.5) * 0.63) / 2
    lg <- log(ea / 0.6108)
    td <- 237.3 * lg / (17.27 - lg)
    geo <- solar_geometry(as.Date("2001-07-06"), 50.8, 100)
    f <- pm(12.3, 21.5, td, 2.078, 100.1, 22.07, geo$ra_mj_m2, geo$rso_mj_m2)
    missing <- pm(12.3, 21.5, td, 2.078, 100.1, NA_real_, geo$ra_mj_m2, geo$rso_mj_m2)
    negative <- pm(10, 20, 25, 2, 100, 20, 35, 26)
    bad_t <- pm(25, 20, 10, 2, 100, 20, 35, 26)
    values <- c(es(24.5), (es(24.5) + es(15)) / 2, geo$ra_mj_m2,
      f$et0_reference_mm, as.numeric(is.na(missing$et0_reference_mm)),
      as.numeric(is.na(negative$vpd_fao56_estimated_kpa)),
      as.numeric(is.na(bad_t$et0_reference_mm)),
      longest_run(c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)),
      longest_run(rep(FALSE, 5)), rain5(1:6),
      as.numeric(is.na(rain5(c(1, 2, NA, 4, 5)))), sum(c(29.9, 30, 31) >= 30))
    expected <- c(3.075, 2.39, 41.09, 3.88, 1, 1, 1, 3, 0, 20, 1, 2)
    tol <- c(0.005, 0.005, 0.08, 0.04, rep(0, 8))
    names <- c("FAO_example3_es_Tmax", "FAO_example3_mean_es", "FAO_example18_Ra",
      "FAO_example18_ET0", "Missing_radiation_propagates", "Negative_VPD_not_imputed",
      "Reversed_temperature_excluded", "Longest_spell", "No_spell_is_zero",
      "Five_day_rainfall", "Missing_rainfall_propagates", "Inclusive_heat_threshold")
    tab <- data.frame(test = names, calculated = values, expected = expected,
      tolerance = tol, pass = abs(values - expected) <= tol)
    assert(all(tab$pass), paste("Internal numerical test failed:", paste(tab$test[!tab$pass], collapse = ", ")))
    tab
  }
  tests <- self_tests()
  if (self_test_only) {
    print(tests, row.names = FALSE)
    message("All numerical self-tests passed. No project files accessed or changed.")
    return(invisible(tests))
  }

  for (p in c(out_dir, derived_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    assert(dir.exists(p), paste("Cannot create", p))
  }
  lock <- file.path(out_dir, ".step13_running")
  assert(dir.create(lock, showWarnings = FALSE), paste("A step13 run may be active. Check R sessions before removing:", lock))
  writeLines(paste(Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  report <- list(execution_status = "INCOMPLETE", script_version = script_version,
    scope = "SIX_EXISTING_PILOT_POINTS", source = "NASA POWER", time_standard = "UTC",
    numerical_tests_passed = TRUE, input_daily_files_checked = 0L, input_md5_verified = FALSE,
    processed_points = 0L, daily_rows = 0L, annual_rows = 0L, monthly_rows = 0L,
    actual_coffee_et_calculated = FALSE, vpd_maximum_calculated = FALSE,
    soil_water_balance_calculated = FALSE, new_downloads = 0L,
    nationwide_climate_calculated = FALSE, climate_attribution_performed = FALSE,
    source_observations_modified = FALSE, error_if_incomplete = "None")
  annual_parts <- monthly_parts <- qa_parts <- flags_parts <- provenance_parts <- list()
  save_summary <- function() write_csv(data.frame(check = names(report),
    result = vapply(report, as.character, character(1))), file.path(out_dir, "coffee_power_indices_summary.csv"))
  on.exit({
    try(save_summary(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step13.txt")), silent = TRUE)
  }, add = TRUE)

  # Definitions form the analysis contract; these are not species-response curves.
  definitions <- data.frame(
    metric = c("tmean_c", "tmin_mean_c", "tmax_mean_c", "tmin_lowest_c", "tmax_highest_c",
      "hot_days_ge30", "hot_days_ge32", "hot_days_ge35", "heat_excess35_c_days",
      "longest_hot_spell_ge35_days", "cold_days_lt2", "cold_days_lt0", "precipitation_total_mm",
      "dry_days_lt1", "wet_days_ge1", "longest_dry_spell_days", "rain_ge20_days", "rain_ge50_days",
      "rain_max1day_mm", "rain_max5day_within_period_mm", "vpd_fao56_mean_kpa", "et0_total_mm",
      "et0_no_lower_cloud_bound_total_mm", "p_minus_et0_mm", "solar_mean_mj_m2_day", "rhmean_pct"),
    unit = c(rep("degC", 5), rep("days", 3), "degC day", rep("days", 3), "mm",
      rep("days", 5), "mm", "mm", "kPa", "mm", "mm", "mm", "MJ m-2 day-1", "%"),
    definition = c("Arithmetic mean of NASA daily T2M", "Mean of daily Tmin", "Mean of daily Tmax",
      "Lowest daily Tmin", "Highest daily Tmax", "Count Tmax >=30 C", "Count Tmax >=32 C",
      "Count Tmax >=35 C", "Sum max(Tmax-35,0); not hourly exposure or injury",
      "Longest consecutive Tmax>=35 run contained within period", "Count Tmin<2 C, not verified frost",
      "Count Tmin<0 C, gridded threshold not field frost observation", "Sum of all daily precipitation depths",
      "Count precipitation<1 mm", "Count precipitation>=1 mm",
      "Longest precipitation<1 mm run contained within period", "Count precipitation>=20 mm",
      "Count precipitation>=50 mm", "Largest daily precipitation",
      "Largest 5-day sum with all five dates inside the period",
      "Mean daily-input VPD estimate: (es(Tmin)+es(Tmax))/2-es(mean dewpoint); not daily maximum",
      "Sum FAO56 reference demand with explicitly bounded cloud ratio; not actual coffee ET",
      "Sensitivity sum: FAO56 ET0 without the lower cloud-ratio bound, still capped above at 1",
      "Precipitation total minus reference ET0; no soil, irrigation, runoff, rooting or drainage",
      "Mean daily downward shortwave radiation; preserve source epochs", "Mean NASA daily relative humidity"),
    stringsAsFactors = FALSE)
  definitions$completeness <- "Requires all expected dates and valid dependent variables within period; no rescaling of partial totals"
  definitions$reference <- c(rep("Study-defined descriptive summary / https://www.climdex.org/learn/indices/", 20),
    rep("FAO56 chapters 2-4, https://www.fao.org/4/x0490e/x0490e07.htm", 4),
    "https://power.larc.nasa.gov/docs/methodology/data/sources/", "NASA POWER daily RH2M")

  aggregate_period <- function(z, site, period_type) {
    # z is already sorted and spans a complete calendar period.
    Tn <- z$tmin_usable_c
    Tx <- z$tmax_usable_c
    pr <- z$precipitation_usable_mm
    out <- data.frame(municipality_code = site$municipality_code, municipality = site$municipality,
      state = site$state, pilot_species_selection = site$species,
      period_type = period_type, period_start = as.character(min(z$date)), period_end = as.character(max(z$date)),
      year = as.integer(format(z$date[1L], "%Y")),
      month = if (period_type == "month") as.integer(format(z$date[1L], "%m")) else NA_integer_,
      expected_days = nrow(z), time_standard = "UTC", source = "NASA POWER",
      solar_source_epoch = paste(unique(z$solar_source_epoch), collapse = " | "), stringsAsFactors = FALSE)
    out$tmean_c <- strict(z$tmean_usable_c, mean)
    out$tmin_mean_c <- strict(Tn, mean)
    out$tmax_mean_c <- strict(Tx, mean)
    out$tmin_lowest_c <- strict(Tn, min)
    out$tmax_highest_c <- strict(Tx, max)
    out$hot_days_ge30 <- strict(Tx, function(x) sum(x >= 30))
    out$hot_days_ge32 <- strict(Tx, function(x) sum(x >= 32))
    out$hot_days_ge35 <- strict(Tx, function(x) sum(x >= 35))
    out$heat_excess35_c_days <- strict(Tx, function(x) sum(pmax(x - 35, 0)))
    out$longest_hot_spell_ge35_days <- longest_run(Tx >= 35)
    out$cold_days_lt2 <- strict(Tn, function(x) sum(x < 2))
    out$cold_days_lt0 <- strict(Tn, function(x) sum(x < 0))
    out$precipitation_total_mm <- strict(pr, sum)
    out$dry_days_lt1 <- strict(pr, function(x) sum(x < 1))
    out$wet_days_ge1 <- strict(pr, function(x) sum(x >= 1))
    out$longest_dry_spell_days <- longest_run(pr < 1)
    out$rain_ge20_days <- strict(pr, function(x) sum(x >= 20))
    out$rain_ge50_days <- strict(pr, function(x) sum(x >= 50))
    out$rain_max1day_mm <- strict(pr, max)
    out$rain_max5day_within_period_mm <- rain5(pr)
    out$vpd_fao56_mean_kpa <- strict(z$vpd_fao56_estimated_kpa, mean)
    out$et0_total_mm <- strict(z$et0_reference_mm, sum)
    out$et0_no_lower_cloud_bound_total_mm <- strict(z$et0_no_lower_cloud_bound_mm, sum)
    out$p_minus_et0_mm <- out$precipitation_total_mm - out$et0_total_mm
    out$solar_mean_mj_m2_day <- strict(z$shortwave_usable_mj_m2, mean)
    out$rhmean_pct <- strict(z$rhmean_usable_pct, mean)
    out$valid_temperature_days <- sum(is.finite(z$tmean_usable_c) & is.finite(Tn) & is.finite(Tx))
    out$valid_precipitation_days <- sum(is.finite(pr))
    out$valid_vpd_days <- sum(is.finite(z$vpd_fao56_estimated_kpa))
    out$valid_et0_days <- sum(is.finite(z$et0_reference_mm))
    out$solar_structurally_unavailable_days <- sum(z$year < 1984)
    out$cloud_ratio_lower_bound_days <- sum(z$flag_cloud_ratio_lower_bound)
    out$cloud_ratio_upper_bound_days <- sum(z$flag_cloud_ratio_upper_bound)
    out$negative_vpd_days <- sum(z$flag_negative_vpd)
    out$negative_et0_raw_days <- sum(z$flag_negative_et0_raw)
    out
  }

  worker <- function() {
    paths <- c(summary = file.path(step12_dir, "coffee_power_history_summary.csv"),
      coverage = file.path(step12_dir, "coffee_power_history_coverage.csv"),
      sites = file.path(step12_dir, "coffee_power_sampling_points.csv"),
      requests = file.path(step12_dir, "coffee_power_requests.csv"),
      dictionary = file.path(step12_dir, "coffee_power_variable_dictionary.csv"),
      source_metadata = file.path(step12_dir, "coffee_power_source_metadata.csv"))
    assert(all(file.exists(paths)), paste("Step12 inputs missing:", paste(paths[!file.exists(paths)], collapse = "\n")))
    if (file.exists(file.path(out_dir, "coffee_power_indices_summary.csv"))) {
      old <- file.path(out_dir, "previous_runs", format(Sys.time(), "%Y%m%dT%H%M%S"))
      dir.create(old, recursive = TRUE, showWarnings = FALSE)
      old_files <- list.files(out_dir, pattern = "\\.(csv|txt)$", full.names = TRUE)
      if (length(old_files)) assert(all(file.copy(old_files, old, overwrite = FALSE)), "Cannot archive previous step13 reports.")
    }
    provenance_parts[[1L]] <<- data.frame(file = unname(paths), md5 = vapply(paths, hash, character(1)), role = names(paths))
    s <- read_csv(paths[["summary"]])
    assert(check_value(s, "execution_status") %in% c("COMPLETED_SIX_POINT_HISTORY", "COMPLETED_SIX_POINT_HISTORY_WITH_QA_FLAGS"), "The six-point NASA history is incomplete.")
    assert(check_value(s, "source") == "NASA POWER" && check_value(s, "combined_with_openmeteo") == "FALSE", "Unexpected climate source.")
    cov <- read_csv(paths[["coverage"]])
    sites <- read_csv(paths[["sites"]])
    req <- read_csv(paths[["requests"]])
    dic <- read_csv(paths[["dictionary"]])
    src <- read_csv(paths[["source_metadata"]])
    need(sites, c("municipality_code", "municipality", "state", "species", "latitude", "longitude"), "Sampling points")
    need(cov, c("municipality_code", "output_rds_md5"), "Coverage")
    need(req, c("municipality_code", "group", "response_elevation_m"), "Request metadata")
    need(dic, c("parameter", "output_column", "output_unit", "lower_screen", "upper_screen"), "Variable dictionary")
    need(src, c("municipality_code", "group", "source_labels_verified"), "Source metadata")
    assert(nrow(sites) == 6L && !anyNA(sites$municipality_code) && !anyDuplicated(sites$municipality_code), "Expected six unique pilot codes.")
    assert(!anyDuplicated(cov$municipality_code) && setequal(sites$municipality_code, cov$municipality_code), "Coverage and sampling codes disagree.")
    assert(nrow(src) == 18L && all(src$source_labels_verified == "TRUE"), "NASA source identification has not passed.")
    raw_vars <- c("tmean_c", "tmin_c", "tmax_c", "dewpoint_mean_c", "rhmean_pct", "precipitation_mm", "wind2m_mean_m_s", "surface_pressure_kpa", "shortwave_mj_m2")
    expected_units <- c("degC", "degC", "degC", "degC", "%", "mm/day", "m/s", "kPa", "MJ/m2/day")
    k <- match(raw_vars, dic$output_column)
    assert(!anyNA(k) && !anyDuplicated(dic$output_column), "Variable dictionary is incomplete or ambiguous.")
    assert(identical(dic$output_unit[k], expected_units), "Unexpected step12 output units. No conversion guessed.")
    dic <- dic[k, , drop = FALSE]
    low <- number(dic$lower_screen, "lower bounds")
    high <- number(dic$upper_screen, "upper bounds")
    expected_dates <- seq(as.Date("1981-01-01"), as.Date("2024-12-31"), by = "day")
    write_csv(definitions, file.path(out_dir, "coffee_power_indices_dictionary.csv"))
    write_csv(tests, file.path(out_dir, "coffee_power_indices_numerical_tests.csv"))

    for (i in seq_len(nrow(sites))) {
      site <- sites[i, , drop = FALSE]
      code <- site$municipality_code
      message(sprintf("[%d/%d] %s | daily indicators, no download", i, nrow(sites), site$municipality))
      input <- file.path(daily_dir, paste0("NASA_POWER_M_", code, "_1981_2024.rds"))
      assert(file.exists(input), paste("Daily NASA file missing:", input))
      expected_md5 <- cov$output_rds_md5[match(code, cov$municipality_code)]
      assert(identical(hash(input), expected_md5), paste("Daily NASA file changed since step12:", input))
      provenance_parts[[length(provenance_parts) + 1L]] <<- data.frame(file = input, md5 = hash(input), role = paste0("daily_", code))
      report$input_daily_files_checked <<- report$input_daily_files_checked + 1L
      d <- as.data.frame(readRDS(input), stringsAsFactors = FALSE)
      need(d, c("municipality_code", "date", "source", "time_standard", "requested_longitude", "requested_latitude", "solar_source_epoch", raw_vars), "Daily NASA data")
      if (inherits(d$date, "Date")) {
        dates <- d$date
      } else if (is.character(d$date)) {
        dates <- as.Date(d$date)
      } else stop("Unexpected daily date class. No date origin guessed.", call. = FALSE)
      assert(!anyNA(dates) && !anyDuplicated(dates), "Daily dates missing or duplicated.")
      d$date <- dates
      d <- d[order(d$date), , drop = FALSE]
      assert(identical(as.character(d$date), as.character(expected_dates)), "Daily series is not a complete 1981-2024 calendar.")
      assert(all(d$source == "NASA POWER") && all(d$time_standard == "UTC") && all(as.character(d$municipality_code) == code), "Source, time standard or municipal code mismatch.")
      lat <- number(site$latitude, "site latitude")
      lon <- number(site$longitude, "site longitude")
      assert(max(abs(number(d$requested_latitude, "daily latitude") - lat)) < 1e-7 &&
        max(abs(number(d$requested_longitude, "daily longitude") - lon)) < 1e-7, "Daily coordinates do not match sampling metadata.")
      met <- req[req$municipality_code == code & req$group == "meteorology", , drop = FALSE]
      assert(nrow(met) == 1L, "Missing or ambiguous meteorological request metadata.")
      z <- number(met$response_elevation_m, "NASA response elevation")
      assert(z >= -500 && z <= 6000, "NASA response elevation outside broad bounds.")
      d$year <- as.integer(format(d$date, "%Y"))
      d$month <- as.integer(format(d$date, "%m"))
      expected_epoch <- ifelse(d$year < 1984, "NOT_AVAILABLE_BEFORE_1984", ifelse(d$year <= 2000, "SRB_1984_2000", "CERES_2001_2024"))
      assert(identical(as.character(d$solar_source_epoch), expected_epoch), "Unexpected solar source epoch.")
      clean <- list()
      d$flag_any_input_outside_bounds <- FALSE
      for (v in seq_along(raw_vars)) {
        x <- number(d[[raw_vars[v]]], raw_vars[v], allow_na = TRUE)
        bad <- !is.na(x) & (x < low[v] | x > high[v])
        d$flag_any_input_outside_bounds <- d$flag_any_input_outside_bounds | bad
        x[bad] <- NA_real_
        clean[[raw_vars[v]]] <- x
      }
      assert(all(is.na(d$shortwave_mj_m2[d$year < 1984])), "Unexpected radiation values before 1984.")
      order_bad <- (!is.na(clean$tmin_c) & !is.na(clean$tmean_c) & clean$tmin_c > clean$tmean_c + 0.05) |
        (!is.na(clean$tmean_c) & !is.na(clean$tmax_c) & clean$tmean_c > clean$tmax_c + 0.05) |
        (!is.na(clean$tmin_c) & !is.na(clean$tmax_c) & clean$tmin_c > clean$tmax_c)
      d$flag_temperature_order <- order_bad
      for (v in c("tmean_c", "tmin_c", "tmax_c")) clean[[v]][order_bad] <- NA_real_
      d$tmean_usable_c <- clean$tmean_c
      d$tmin_usable_c <- clean$tmin_c
      d$tmax_usable_c <- clean$tmax_c
      d$precipitation_usable_mm <- clean$precipitation_mm
      d$shortwave_usable_mj_m2 <- clean$shortwave_mj_m2
      d$rhmean_usable_pct <- clean$rhmean_pct
      geo <- solar_geometry(d$date, lat, z)
      calculations <- pm(clean$tmin_c, clean$tmax_c, clean$dewpoint_mean_c,
        clean$wind2m_mean_m_s, clean$surface_pressure_kpa, clean$shortwave_mj_m2,
        geo$ra_mj_m2, geo$rso_mj_m2)
      assert(!length(intersect(names(d), names(calculations))), "Input already has step13 calculation columns.")
      d <- cbind(d, calculations)
      d$p_minus_et0_mm <- d$precipitation_usable_mm - d$et0_reference_mm
      d$response_elevation_used_for_rso_m <- z
      d$et0_method <- "FAO56_PM_daily_TminTmax_Tdew_PS_WS2M_Rs_cloud_ratio_0.30_to_1.00"
      d$vpd_method <- "Daily_input_approximation_not_maximum_or_hourly_mean"
      d$historical_boundaries_harmonized <- FALSE
      d$coffee_field_location <- FALSE
      annual <- bind(lapply(split(d, d$year), aggregate_period, site = site, period_type = "year"))
      monthly <- bind(lapply(split(d, format(d$date, "%Y-%m")), aggregate_period, site = site, period_type = "month"))
      annual_parts[[i]] <<- annual
      monthly_parts[[i]] <<- monthly
      flag_cols <- grep("^flag_", names(d), value = TRUE)
      any_flag <- Reduce(`|`, d[flag_cols])
      flags_parts[[i]] <<- d[any_flag, c("municipality_code", "date", "solar_source_epoch", flag_cols,
        "rs_rso_raw", "rs_rso_used", "vpd_fao56_raw_kpa", "et0_fao56_raw_mm", "et0_no_lower_cloud_bound_mm"), drop = FALSE]
      solar_days <- sum(d$year >= 1984)
      qa_parts[[i]] <<- data.frame(municipality_code = code, municipality = site$municipality,
        daily_rows = nrow(d), meteorological_expected_days = nrow(d),
        vpd_available_days = sum(is.finite(d$vpd_fao56_estimated_kpa)), et0_expected_days = solar_days,
        et0_available_days = sum(is.finite(d$et0_reference_mm)),
        et0_structurally_unavailable_days = sum(d$year < 1984),
        input_outside_bounds_days = sum(d$flag_any_input_outside_bounds),
        temperature_order_failures = sum(d$flag_temperature_order),
        negative_vpd_days = sum(d$flag_negative_vpd),
        cloud_ratio_lower_bound_days = sum(d$flag_cloud_ratio_lower_bound),
        cloud_ratio_upper_bound_days = sum(d$flag_cloud_ratio_upper_bound),
        negative_et0_raw_days = sum(d$flag_negative_et0_raw),
        et0_domain_failures = sum(d$flag_et0_formula_domain),
        annual_et0_complete_years = sum(!is.na(annual$et0_total_mm)),
        response_elevation_for_rso_m = z, station_validation_performed = FALSE,
        raw_observations_changed = FALSE)
      out <- file.path(derived_dir, paste0("NASA_POWER_indices_M_", code, "_1981_2024"))
      write_rds(d, paste0(out, ".rds"))
      write_csv(d, paste0(out, ".csv"))
      report$processed_points <<- i
      report$daily_rows <<- report$daily_rows + nrow(d)
      report$annual_rows <<- report$annual_rows + nrow(annual)
      report$monthly_rows <<- report$monthly_rows + nrow(monthly)
      save_summary()
    }
    annual <- bind(annual_parts)
    monthly <- bind(monthly_parts)
    qa <- bind(qa_parts)
    # Baseline anomalies only for meteorological indicators independent of Rs.
    # No full-epoch radiation, ET0 or P-ET0 anomaly/trend is generated here.
    met_metrics <- setdiff(definitions$metric, c("et0_total_mm", "et0_no_lower_cloud_bound_total_mm", "p_minus_et0_mm", "solar_mean_mj_m2_day"))
    for (v in met_metrics) annual[[paste0(v, "_anomaly_1981_2010")]] <- NA_real_
    baselines <- contrasts <- list()
    for (code in sites$municipality_code) {
      a <- annual[annual$municipality_code == code, , drop = FALSE]
      b <- a[a$year %in% baseline_years, , drop = FALSE]
      old <- a[a$year %in% comparison_initial, , drop = FALSE]
      recent <- a[a$year %in% comparison_recent, , drop = FALSE]
      for (v in definitions$metric) {
        def <- definitions[definitions$metric == v, , drop = FALSE]
        old_mean <- if (nrow(old) == length(comparison_initial)) strict(old[[v]], mean) else NA_real_
        new_mean <- if (nrow(recent) == length(comparison_recent)) strict(recent[[v]], mean) else NA_real_
        contrasts[[length(contrasts) + 1L]] <- data.frame(municipality_code = code,
          municipality = a$municipality[1L], metric = v, unit = def$unit,
          baseline_period = "2012 to 2015", recent_period = "2021 to 2024",
          baseline_period_mean = old_mean, recent_period_mean = new_mean,
          absolute_change = new_mean - old_mean, all_required_years_valid = is.finite(old_mean) & is.finite(new_mean),
          interpretation = "Difference of pilot-point period means, not a trend or climate attribution")
        if (v %in% met_metrics) {
          ref <- if (nrow(b) == length(baseline_years)) strict(b[[v]], mean) else NA_real_
          rows <- annual$municipality_code == code
          annual[[paste0(v, "_anomaly_1981_2010")]][rows] <- a[[v]] - ref
          baselines[[length(baselines) + 1L]] <- data.frame(municipality_code = code,
            municipality = a$municipality[1L], metric = v, unit = def$unit,
            reference_period = "1981 to 2010", reference_mean = ref,
            reference_valid_years = sum(is.finite(b[[v]])))
        }
      }
    }
    prov <- bind(provenance_parts)
    assert(all(vapply(prov$file, hash, character(1)) == prov$md5), "An input changed during step13.")
    report$input_md5_verified <<- TRUE
    critical <- sum(qa$input_outside_bounds_days + qa$temperature_order_failures + qa$negative_vpd_days + qa$et0_domain_failures)
    report$critical_flag_day_sum <<- critical
    report$cloud_ratio_lower_bound_day_count <<- sum(qa$cloud_ratio_lower_bound_days)
    report$cloud_ratio_upper_bound_day_count <<- sum(qa$cloud_ratio_upper_bound_days)
    report$et0_available_site_days <<- sum(qa$et0_available_days)
    report$execution_status <<- if (critical > 0L) "COMPLETED_SIX_POINT_INDICES_WITH_QA_FLAGS" else "COMPLETED_SIX_POINT_INDICES"
    write_csv(annual, file.path(out_dir, "coffee_power_indices_annual.csv"))
    write_csv(monthly, file.path(out_dir, "coffee_power_indices_monthly.csv"))
    write_csv(bind(baselines), file.path(out_dir, "coffee_power_indices_baseline.csv"))
    write_csv(bind(contrasts), file.path(out_dir, "coffee_power_indices_period_comparison.csv"))
    write_csv(qa, file.path(out_dir, "coffee_power_indices_quality.csv"))
    write_csv(bind(flags_parts), file.path(out_dir, "coffee_power_indices_flagged_days.csv"))
    write_csv(prov, file.path(out_dir, "coffee_power_indices_input_provenance.csv"))
    notes <- c("COFFE-STUDY | STEP13 | SIX NASA POWER DAILY CLIMATE SERIES",
      paste("Executed:", format(Sys.time(), usetz = TRUE)), paste("Script:", script_version),
      "All original daily columns are retained in new derivative files. No previous-step input is modified.",
      "Original daily RDS hashes are compared with the step12 coverage manifest.",
      "Sampling points are municipal reference proxies, not mapped coffee fields or area averages.",
      "Pilot species selection labels describe site selection, not species-specific weather or response functions.",
      "Each day is UTC. No conversion to local agronomic days or phenological windows is attempted.",
      "Monthly/yearly statistics require every expected day usable for the metric's inputs.",
      "Temperature summaries use NASA T2M. ET0 uses (Tmin+Tmax)/2 per FAO56 daily convention.",
      "FAO56 e_s=(e(Tmin)+e(Tmax))/2; e_a=e(mean dewpoint). This is not exact hourly-mean or maximum VPD.",
      "NASA daily pressure gives gamma=0.000665*PS; NASA WS2M is already at 2 m.",
      "NASA response elevation is used only in Rso=(0.75+0.00002*z)*Ra. It is not coffee altitude.",
      "Net radiation uses albedo=0.23, Stefan-Boltzmann=4.903e-9, daily soil heat flux G=0.",
      "Explicit implementation policy: Rs/Rso used in the cloud term is bounded to [0.30,1.00].",
      "Unmodified ratio and every adjustment are retained. ET0 with only the upper bound is a sensitivity output.",
      "Lower cloud bound is an implementation choice here, not claimed to be a calibration or a data correction.",
      "Negative VPD is retained raw but excluded from usable VPD/ET0. No fallback data source is used.",
      "Negative formula ET0 is retained raw; reference-demand ET0 uses max(raw,0) and flags the adjustment.",
      "Negative ET0 is not interpreted as measured condensation. ET0 is not actual coffee evapotranspiration.",
      "Radiation before 1984 remains NA. All solar-dependent results preserve the SRB/CERES source epochs.",
      "No radiation/ET0/P-ET0 trend or baseline anomaly crossing the 2001 source change is estimated.",
      "2012-2015 and 2021-2024 period comparisons share the CERES solar epoch.",
      "Meteorological annual anomalies subtract each point's complete 1981-2010 mean of annual indices.",
      "A positive P-ET0 balance does not rule out seasonal water stress. There is no soil reservoir in this calculation.",
      "Annual and monthly longest spells and 5-day windows are contained within their respective calendar periods.",
      "Tmax thresholds >=30,>=32,>=35 and Tmin<2,<0 are descriptive. They are not species injury thresholds.",
      "No missing value is turned into zero or scaled to a full-period total. Structural radiation gaps are explicit.",
      "No national mean, crop yield model, formal climate-change attribution or station validation is produced.",
      "No additional main figure is produced. These tables develop the climate component of the planned figures.",
      "https://www.fao.org/4/x0490e/x0490e06.htm",
      "https://www.fao.org/4/x0490e/x0490e07.htm",
      "https://www.fao.org/4/x0490e/x0490e08.htm",
      "https://www.climdex.org/learn/indices/",
      "https://power.larc.nasa.gov/docs/methodology/data/sources/")
    writeLines(notes, file.path(out_dir, "coffee_power_indices_method_notes.txt"))
    save_summary()
    message("\n", report$execution_status, "\nReports: ", out_dir,
      "\nNext files to share: coffee_power_indices_summary.csv and coffee_power_indices_quality.csv",
      "\nOptional for interpretation: coffee_power_indices_annual.csv")
    invisible(list(summary = report, annual = annual, monthly = monthly, quality = qa))
  }
  tryCatch(worker(), error = function(e) {
    report$error_if_incomplete <<- conditionMessage(e)
    report$execution_status <<- "INCOMPLETE"
    try(save_summary(), silent = TRUE)
    stop(conditionMessage(e), "\nOriginal input files were not changed.\nSummary: ",
      file.path(out_dir, "coffee_power_indices_summary.csv"), call. = FALSE)
  })
}

coffee_climate_step13()
