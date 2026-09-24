# ======================================================================
# COFFE-STUDY | STEP 17 | MUNICIPAL CLIMATE INDICES AND WEIGHTED EXPOSURE
# Run this COMPLETE file with Source in RStudio. Base R only.
# No API calls, downloads or installation. Steps 01-16 are left unchanged.
#
# Input: all source-cell daily RDS files acquired by step16, its manifest,
# the step15 crosswalk and the ORIGINAL step11 municipal weights.
# Output: annual/monthly municipal indices and area/production-weighted
# summaries. These are coffee-weighted regional climate proxies, NOT a
# national territorial climate average, farm observations or causal effects.
#
# METHOD CONTRACT (inherited from step13)
# * Meteorology 1981-2024; radiation/ET0 1984-2024, separate SRB/CERES epochs.
# * Daily UTC dates; no local-calendar or crop-phenology realignment.
# * FAO56 PM, wind at 2 m, daily pressure, Tmin/Tmax and mean dewpoint.
# * Solar geometry uses each municipality's ORIGINAL reference latitude.
#   Rso elevation comes from its MET source-cell response in step16. It is
#   not coffee-field elevation. This metadata may differ from the pilot's
#   response at its interior point: both values are explicitly retained.
# * Calculate nonlinear indices/ET0 BEFORE spatial weighting. Source cells
#   may be shared, but municipal latitudes/weights are not collapsed.
# * Same explicit Rs/Rso [0.30,1] policy and upper-bound-only sensitivity.
# * Negative estimated VPD remains raw in critical-day diagnostics; usable
#   VPD/ET0 stay NA. No filling, no weather-source substitution.
# * Strict daily completeness within each calendar month/year per metric.
# * Strict spatial estimates require every POSITIVE weight to have a valid
#   local index. Coverage and a separately named available-weight diagnostic
#   are exported. The latter is NOT silently used as the full-coverage mean.
# * Fixed weights: 2012-2015 mean municipal area/production, all 1981-2024.
#   They describe historical climate at that FIXED reference geography.
#   Dynamic weights: observed annual weights ONLY for 2012-2024.
# * Means of local longest spells are not a countrywide contiguous drought.
# * No trend, regression, soil-water balance or extra main figure is fitted.
#
# Resume: rerun this file. Valid municipal derivative caches are reused.
# Optional environment settings (normal run needs none):
# COFFEE_STUDY_DIR                    alternative root of the same project
# COFFEE_INDICES17_TEST_ONLY=TRUE      mathematical tests only
# COFFEE_INDICES17_MAX_MUNICIPALITIES  cap on NEW municipal calculations
#                                    (default: unlimited; rerun to resume)
#
# References inherited from step13:
# https://www.fao.org/4/x0490e/x0490e07.htm
# https://www.fao.org/4/x0490e/x0490e08.htm
# https://www.climdex.org/learn/indices/
# https://power.larc.nasa.gov/docs/methodology/data/sources/
# ======================================================================

coffee_power_step17 <- function() {
  version <- "17_municipal_indices_and_weighted_exposure_v1"
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  first_year <- 1981L
  last_year <- 2024L
  years <- first_year:last_year
  weight_years <- 2012:2024
  baseline_years <- 1981:2010
  initial_years <- 2012:2015
  recent_years <- 2021:2024
  cloud_ratio_lower <- 0.30
  cloud_ratio_upper <- 1.00
  test_only <- toupper(Sys.getenv("COFFEE_INDICES17_TEST_ONLY", "FALSE")) == "TRUE"
  limit_new <- suppressWarnings(as.numeric(Sys.getenv("COFFEE_INDICES17_MAX_MUNICIPALITIES", "Inf")))
  if (is.na(limit_new) || limit_new < 0) stop("Invalid new-municipality limit.", call. = FALSE)
  step16 <- file.path(base_dir, "04_outputs", "climate_power_brazil_download")
  climate_dir <- file.path(base_dir, "02_data_processed", "climate")
  out_dir <- file.path(base_dir, "04_outputs", "climate_power_brazil_indices")
  derived_dir <- file.path(climate_dir, "NASA_POWER", "brazil_indices")
  cache_dir <- file.path(derived_dir, "municipality_cache")

  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(d, cols, label) {
    absent <- setdiff(cols, names(d))
    assert(!length(absent), paste(label, "missing:", paste(absent, collapse = ", ")))
  }
  hash <- function(p) unname(tools::md5sum(p))
  number <- function(x, label, allow_na = FALSE) {
    old <- as.character(x)
    v <- suppressWarnings(as.numeric(old))
    assert(!any(!is.na(old) & is.na(v)) && all(is.na(v) | is.finite(v)) &&
      (allow_na || !anyNA(v)), paste("Invalid numeric data:", label))
    v
  }
  read_csv <- function(p) {
    assert(file.exists(p), paste("Missing required input:", p))
    utils::read.csv(p, colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  write_csv <- function(x, p) {
    tmp <- tempfile("coffee17_", tmpdir = dirname(p), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, p, overwrite = TRUE), paste("Cannot write:", p))
  }
  write_rds <- function(x, p) {
    tmp <- tempfile("coffee17_", tmpdir = dirname(p), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    assert(file.copy(tmp, p, overwrite = TRUE), paste("Cannot write:", p))
  }
  bind <- function(x) {
    x <- Filter(function(z) is.data.frame(z) && nrow(z) > 0L, x)
    if (!length(x)) return(data.frame())
    z <- do.call(rbind, x); rownames(z) <- NULL; z
  }
  checked <- function(d, key) {
    need(d, c("check", "result"), "Summary")
    z <- d$result[d$check == key]
    assert(length(z) == 1L && !is.na(z), paste("Missing/ambiguous summary key:", key))
    z
  }
  local_path <- function(p) {
    assert(length(p) == 1L && !is.na(p), "Missing input path.")
    if (file.exists(p)) return(p)
    s <- gsub("\\", "/", p, fixed = TRUE)
    key <- "/coffe-study/"
    i <- regexpr(key, s, fixed = TRUE)[1L]
    if (i > 0L) {
      alt <- file.path(base_dir, substring(s, i + nchar(key)))
      if (file.exists(alt)) return(alt)
    }
    stop("Required file not found at its recorded project path:\n", p,
      "\nNo filename or nearest-location replacement was attempted.", call. = FALSE)
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

  # Calendar indices are built once and reused for every municipality.
  dates_all <- seq(as.Date("1981-01-01"), as.Date("2024-12-31"), by = "day")
  year_all <- as.integer(format(dates_all, "%Y"))
  month_all <- as.integer(format(dates_all, "%m"))
  calendar_groups <- function(type) {
    key <- if (type == "year") year_all else year_all * 100L + month_all
    lev <- unique(key)
    g <- match(key, lev)
    slices <- split(seq_along(g), factor(g, levels = seq_along(lev)))
    begin <- vapply(slices, `[`, integer(1), 1L)
    end <- vapply(slices, function(z) z[length(z)], integer(1))
    list(g = g, slices = slices, n = lengths(slices),
      period_type = type, period_start = as.character(dates_all[begin]),
      period_end = as.character(dates_all[end]), year = year_all[begin],
      month = if (type == "month") month_all[begin] else rep(NA_integer_, length(begin)))
  }
  cg_year <- calendar_groups("year")
  cg_month <- calendar_groups("month")

  summarize_periods <- function(d, cg) {
    # Vectorized sums/counts; spell/max calculations remain local to each
    # calendar period. National series never get spell calculations applied
    # after spatial averaging.
    x <- cbind(tmean_c = d$tmean_usable_c, tmin_mean_c = d$tmin_usable_c,
      tmax_mean_c = d$tmax_usable_c, hot_days_ge30 = d$tmax_usable_c >= 30,
      hot_days_ge32 = d$tmax_usable_c >= 32, hot_days_ge35 = d$tmax_usable_c >= 35,
      heat_excess35_c_days = pmax(d$tmax_usable_c - 35, 0),
      cold_days_lt2 = d$tmin_usable_c < 2, cold_days_lt0 = d$tmin_usable_c < 0,
      precipitation_total_mm = d$precipitation_usable_mm,
      dry_days_lt1 = d$precipitation_usable_mm < 1,
      wet_days_ge1 = d$precipitation_usable_mm >= 1,
      rain_ge20_days = d$precipitation_usable_mm >= 20,
      rain_ge50_days = d$precipitation_usable_mm >= 50,
      vpd_fao56_mean_kpa = d$vpd_fao56_estimated_kpa,
      et0_total_mm = d$et0_reference_mm,
      et0_no_lower_cloud_bound_total_mm = d$et0_no_lower_cloud_bound_mm,
      solar_mean_mj_m2_day = d$shortwave_usable_mj_m2, rhmean_pct = d$rhmean_usable_pct)
    valid <- is.finite(x)
    observed <- rowsum(1L * valid, cg$g, reorder = FALSE)
    x[!valid] <- 0
    totals <- rowsum(x, cg$g, reorder = FALSE)
    totals[observed != cg$n] <- NA_real_
    means <- c("tmean_c", "tmin_mean_c", "tmax_mean_c", "vpd_fao56_mean_kpa",
      "solar_mean_mj_m2_day", "rhmean_pct")
    totals[, means] <- totals[, means, drop = FALSE] / cg$n
    tab <- as.data.frame(totals)
    per_group <- function(x, fun) vapply(cg$slices, function(j) fun(x[j]), numeric(1))
    tab$tmin_lowest_c <- per_group(d$tmin_usable_c, function(z) strict(z, min))
    tab$tmax_highest_c <- per_group(d$tmax_usable_c, function(z) strict(z, max))
    tab$longest_hot_spell_ge35_days <- per_group(d$tmax_usable_c >= 35, longest_run)
    tab$longest_dry_spell_days <- per_group(d$precipitation_usable_mm < 1, longest_run)
    tab$rain_max1day_mm <- per_group(d$precipitation_usable_mm, function(z) strict(z, max))
    tab$rain_max5day_within_period_mm <- per_group(d$precipitation_usable_mm, rain5)
    tab$p_minus_et0_mm <- tab$precipitation_total_mm - tab$et0_total_mm
    tab <- tab[definitions$metric]
    counts <- cbind(valid_temperature_days = is.finite(d$tmean_usable_c) &
        is.finite(d$tmin_usable_c) & is.finite(d$tmax_usable_c),
      valid_precipitation_days = is.finite(d$precipitation_usable_mm),
      valid_vpd_days = is.finite(d$vpd_fao56_estimated_kpa),
      valid_et0_days = is.finite(d$et0_reference_mm),
      solar_structurally_unavailable_days = year_all < 1984,
      missing_solar_available_period_days = year_all >= 1984 & is.na(d$shortwave_usable_mj_m2),
      negative_vpd_days = d$flag_negative_vpd,
      cloud_ratio_lower_bound_days = d$flag_cloud_ratio_lower_bound,
      cloud_ratio_upper_bound_days = d$flag_cloud_ratio_upper_bound,
      negative_et0_raw_days = d$flag_negative_et0_raw)
    ctab <- as.data.frame(rowsum(1L * counts, cg$g, reorder = FALSE))
    metadata <- data.frame(period_type = cg$period_type, period_start = cg$period_start,
      period_end = cg$period_end, year = cg$year, month = cg$month,
      expected_days = cg$n, solar_source_epoch = ifelse(cg$year < 1984,
        "NOT_AVAILABLE_BEFORE_1984", ifelse(cg$year <= 2000, "SRB_1984_2000", "CERES_2001_2024")),
      stringsAsFactors = FALSE)
    rownames(tab) <- rownames(ctab) <- NULL
    cbind(metadata, tab, ctab)
  }
  spatial_stats <- function(values, w) {
    assert(is.matrix(values) && nrow(values) == length(w), "Weight/index shape mismatch.")
    assert(all(is.finite(w)) && all(w >= 0) && abs(sum(w) - 1) < 1e-8,
      "Spatial weights do not sum to one.")
    ok <- is.finite(values)
    observed_w <- colSums(ok * w)
    missing_positive <- colSums(!ok & (w > 0))
    values[!ok] <- 0
    numerator <- colSums(values * w)
    strict_est <- numerator / sum(w)
    strict_est[missing_positive > 0L] <- NA_real_
    diagnostic <- ifelse(observed_w > 0, numerator / observed_w, NA_real_)
    data.frame(metric = colnames(values), estimate_strict = strict_est,
      available_weight_normalized_diagnostic = diagnostic,
      observed_weight_fraction = observed_w / sum(w),
      missing_positive_weight_municipalities = missing_positive,
      contributing_municipalities = colSums(ok & (w > 0)), stringsAsFactors = FALSE)
  }

  tests <- self_tests()
  st <- spatial_stats(matrix(c(10, NA, 5, 15), nrow = 2,
    dimnames = list(NULL, c("has_gap", "complete"))), c(0.25, 0.75))
  more <- data.frame(test = c("Strict_spatial_gap_is_not_renormalized", "Available_weight_is_explicit",
    "Weighted_value", "Calendar_years", "Calendar_months"),
    calculated = c(as.numeric(is.na(st$estimate_strict[1])), st$observed_weight_fraction[1],
      st$estimate_strict[2], length(cg_year$n), length(cg_month$n)),
    expected = c(1, 0.25, 12.5, 44, 528), tolerance = c(0, 1e-12, 1e-12, 0, 0))
  more$pass <- abs(more$calculated - more$expected) <= more$tolerance
  tests <- rbind(tests, more)
  # Constructed numerical fixture, never written into observation datasets.
  nn <- length(dates_all)
  fixture <- data.frame(tmean_usable_c = rep(20, nn), tmin_usable_c = rep(15, nn),
    tmax_usable_c = rep(32, nn), precipitation_usable_mm = rep(1, nn),
    vpd_fao56_estimated_kpa = rep(1, nn), et0_reference_mm = rep(2, nn),
    et0_no_lower_cloud_bound_mm = rep(2, nn), shortwave_usable_mj_m2 = rep(20, nn),
    rhmean_usable_pct = rep(70, nn), flag_negative_vpd = rep(FALSE, nn),
    flag_cloud_ratio_lower_bound = rep(FALSE, nn), flag_cloud_ratio_upper_bound = rep(FALSE, nn),
    flag_negative_et0_raw = rep(FALSE, nn))
  a_test <- summarize_periods(fixture, cg_year)
  m_test <- summarize_periods(fixture, cg_month)
  fixture$precipitation_usable_mm[1L] <- NA_real_
  missing_test <- summarize_periods(fixture, cg_year)
  added <- data.frame(test = c("Fast_annual_sum_leap_year", "Fast_inclusive_heat_count",
    "Fast_monthly_count", "Fast_no_dry_spell_at_one_mm", "Fast_missing_period_stays_NA"),
    calculated = c(a_test$precipitation_total_mm[a_test$year == 1984],
      a_test$hot_days_ge32[a_test$year == 1981],
      m_test$hot_days_ge32[m_test$year == 1984 & m_test$month == 2],
      a_test$longest_dry_spell_days[1L], as.numeric(is.na(missing_test$precipitation_total_mm[1L]))),
    expected = c(366, 365, 29, 0, 1), tolerance = 0)
  added$pass <- abs(added$calculated - added$expected) <= added$tolerance
  tests <- rbind(tests, added)
  rm(fixture, a_test, m_test, missing_test)
  assert(all(tests$pass), "A local numerical test failed.")
  if (test_only) {
    print(tests, row.names = FALSE)
    message("Step17 mathematical tests passed. No project files accessed.")
    return(invisible(tests))
  }
  for (p in c(out_dir, derived_dir, cache_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    assert(dir.exists(p), paste("Cannot create:", p))
  }
  lock <- file.path(out_dir, ".step17_running")
  assert(dir.create(lock, showWarnings = FALSE), paste(
    "Another step17 run may be active. Confirm it is stopped before removing:", lock))
  writeLines(paste(Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  prior <- list.files(out_dir, pattern = "\\.(csv|txt)$", full.names = TRUE)
  if (length(prior)) {
    archive_root <- file.path(out_dir, "previous_runs")
    dir.create(archive_root, showWarnings = FALSE)
    archive <- tempfile(paste0("before_", format(Sys.time(), "%Y%m%dT%H%M%S"), "_"), tmpdir = archive_root)
    assert(dir.create(archive) && all(file.copy(prior, archive)), "Cannot archive previous step17 reports.")
  }
  report <- list(execution_status = "INCOMPLETE", script_version = version,
    source = "NASA POWER", municipal_targets = 0L, source_histories_checked = 0L,
    unique_source_missing_values = 0L, source_out_of_bounds_values = 0L,
    municipalities_processed = 0L, municipalities_reused = 0L, new_municipalities_calculated = 0L,
    municipal_annual_rows = 0L, municipal_monthly_rows = 0L,
    municipal_negative_vpd_days = 0L, municipal_et0_missing_available_period_days = 0L,
    weighted_exposure_rows = 0L, incomplete_weighted_exposure_rows = 0L,
    period_comparison_rows = 0L, numerical_tests_passed = TRUE,
    no_missing_value_imputed = TRUE, time_standard = "UTC",
    daily_downloads = 0L, municipal_daily_files_replicated = FALSE,
    climate_weighted_by_coffee_distribution = FALSE, national_territorial_mean = FALSE,
    climate_trends_estimated = FALSE, climate_attribution_performed = FALSE,
    station_validation_performed = FALSE, historical_boundaries_harmonized = FALSE,
    source_data_modified = FALSE, last_stage = "Reading step16 inputs", error_if_incomplete = "None")
  progress <- list()
  save_progress <- function() {
    write_csv(data.frame(check = names(report), result = vapply(report, as.character, character(1))),
      file.path(out_dir, "coffee_power_brazil_indices_summary.csv"))
    if (length(progress)) write_csv(bind(progress), file.path(out_dir, "coffee_power_brazil_indices_cache_index.csv"))
  }
  on.exit({
    try(save_progress(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step17.txt")), silent = TRUE)
  }, add = TRUE)

  worker <- function() {
    paths <- c(summary16 = file.path(step16, "coffee_power_brazil_download_summary.csv"),
      manifest = file.path(step16, "coffee_power_brazil_download_manifest.csv"),
      index = file.path(step16, "coffee_power_brazil_municipality_series_index.csv"),
      provenance16 = file.path(step16, "coffee_power_brazil_input_provenance.csv"),
      targets = file.path(climate_dir, "coffee_climate_target_registry.csv"),
      weights = file.path(climate_dir, "coffee_climate_municipal_weights.csv"))
    assert(all(file.exists(paths)), paste("Missing inputs:", paste(paths[!file.exists(paths)], collapse = "\n")))
    inputs_md5 <- vapply(paths, hash, character(1))
    write_csv(data.frame(role = names(paths), file = unname(paths), md5 = inputs_md5),
      file.path(out_dir, "coffee_power_brazil_indices_input_provenance.csv"))
    s <- read_csv(paths[["summary16"]])
    assert(checked(s, "all_source_histories_acquired") == "TRUE" &&
      checked(s, "failed_cells") == "0" && checked(s, "pending_cells") == "0",
      "Step16 is not complete. Resume the step16 download first.")
    assert(checked(s, "source") == "NASA POWER", "Unexpected source.")
    origin <- read_csv(paths[["provenance16"]])
    need(origin, c("file", "md5"), "Step16 input provenance")
    for (role in c("targets", "weights")) {
      # Find the exact recorded path suffix, not a name-based file search.
      suffix <- paste0("/02_data_processed/climate/", basename(paths[[role]]))
      candidates <- endsWith(gsub("\\", "/", origin$file, fixed = TRUE), suffix)
      assert(sum(candidates) == 1L && hash(paths[[role]]) == origin$md5[candidates],
        paste("Step11", role, "has changed since step16. Revalidate affected steps."))
    }
    man <- read_csv(paths[["manifest"]])
    need(man, c("cell_id", "family", "start", "end", "daily_file", "daily_md5",
      "status", "response_elevation_m", "source_product", "time_standard"), "Manifest")
    assert(!anyDuplicated(man$cell_id) && nrow(man) == as.integer(checked(s, "planned_source_cells")),
      "Manifest keys or cell counts disagree.")
    assert(all(man$status %in% c("VALID_COMPLETE", "VALID_WITH_QA_FLAGS")) &&
      all(man$time_standard == "UTC"), "Manifest contains incomplete or non-UTC cells.")
    man$local_daily <- vapply(man$daily_file, local_path, character(1))
    man$response_elevation_m <- number(man$response_elevation_m, "source elevation", TRUE)
    meta_rows <- man$family == "MET"
    assert(all(is.finite(man$response_elevation_m[meta_rows])) &&
      all(man$response_elevation_m[meta_rows] >= -500 & man$response_elevation_m[meta_rows] <= 6000),
      "MET response elevation is missing or outside the calculation domain.")
    idx <- read_csv(paths[["index"]]); targets <- read_csv(paths[["targets"]])
    need(idx, c("municipality_code", "family", "cell_id", "history_status",
      "requested_municipal_latitude", "requested_municipal_longitude"), "Municipal series index")
    need(targets, c("municipality_code", "municipality", "state", "latitude", "longitude"), "Target registry")
    assert(!anyDuplicated(targets$municipality_code) &&
      !anyDuplicated(paste(idx$municipality_code, idx$family)) &&
      setequal(unique(idx$municipality_code), targets$municipality_code), "Municipal target keys disagree.")
    codes <- sort(targets$municipality_code)
    n_sites <- length(codes)
    assert(nrow(idx) == n_sites * 3L && all(idx$family %in% c("MET", "SRB", "CERES")) &&
      all(idx$history_status %in% c("VALID_COMPLETE", "VALID_WITH_QA_FLAGS")), "Incomplete source crosswalk.")
    sites <- targets[match(codes, targets$municipality_code), c("municipality_code", "municipality", "state", "longitude", "latitude")]
    sites$latitude <- number(sites$latitude, "municipal reference latitude")
    sites$longitude <- number(sites$longitude, "municipal reference longitude")
    for (fam in c("MET", "SRB", "CERES")) {
      z <- idx[idx$family == fam, , drop = FALSE]
      k <- match(codes, z$municipality_code)
      assert(!anyNA(k), paste("Missing source family", fam))
      sites[[paste0(fam, "_cell_id")]] <- z$cell_id[k]
      assert(max(abs(number(z$requested_municipal_latitude[k], "index latitude") - sites$latitude)) < 1e-7 &&
        max(abs(number(z$requested_municipal_longitude[k], "index longitude") - sites$longitude)) < 1e-7,
        "Crosswalk coordinates differ from original municipal reference points.")
      j <- match(z$cell_id, man$cell_id)
      assert(!anyNA(j) && all(man$family[j] == fam), "Source identifier/family mismatch.")
    }
    sites$response_elevation_for_rso_m <- man$response_elevation_m[match(sites$MET_cell_id, man$cell_id)]
    report$municipal_targets <<- n_sites
    write_csv(sites, file.path(out_dir, "coffee_power_brazil_indices_municipality_index.csv"))

    weights <- read_csv(paths[["weights"]])
    weight_names <- c("baseline_area_weight", "dynamic_area_weight", "baseline_production_weight", "dynamic_production_weight")
    need(weights, c("municipality_code", "species", "year", weight_names), "Original municipal weights")
    weights$year <- number(weights$year, "weight year")
    for (v in weight_names) weights[[v]] <- number(weights[[v]], v)
    assert(!anyDuplicated(paste(weights$municipality_code, weights$species, weights$year)), "Duplicate municipal weights.")
    assert(setequal(weights$municipality_code, codes) && setequal(weights$year, weight_years) &&
      setequal(weights$species, c("Total", "Arabica", "Canephora")) &&
      nrow(weights) == n_sites * 3L * length(weight_years), "Weights do not cover the fixed analysis panel.")
    wb <- list(); checks <- list()
    for (sp in c("Total", "Arabica", "Canephora")) {
      for (yr in weight_years) {
        z <- weights[weights$species == sp & weights$year == yr, , drop = FALSE]
        z <- z[match(codes, z$municipality_code), , drop = FALSE]
        assert(!anyNA(z$municipality_code), "Missing municipal weight.")
        for (v in weight_names) {
          assert(all(z[[v]] >= 0) && abs(sum(z[[v]]) - 1) < 1e-8, "Weights do not sum to one.")
          checks[[length(checks) + 1L]] <- data.frame(species = sp, year = yr, weighting = v, sum = sum(z[[v]]))
        }
        wb[[paste(sp, yr, sep = "_")]] <- z
        if (yr != min(weight_years)) {
          ref <- wb[[paste(sp, min(weight_years), sep = "_")]]
          assert(max(abs(z$baseline_area_weight - ref$baseline_area_weight)) < 1e-12 &&
            max(abs(z$baseline_production_weight - ref$baseline_production_weight)) < 1e-12,
            "Fixed baseline weights change over time.")
        }
      }
    }
    write_csv(bind(checks), file.path(out_dir, "coffee_power_brazil_indices_weight_checks.csv"))
    write_csv(definitions, file.path(out_dir, "coffee_power_brazil_indices_dictionary.csv"))
    write_csv(tests, file.path(out_dir, "coffee_power_brazil_indices_numerical_tests.csv"))

    raw_vars <- c("tmean_c", "tmin_c", "tmax_c", "dewpoint_mean_c", "rhmean_pct",
      "precipitation_mm", "wind2m_mean_m_s", "surface_pressure_kpa", "shortwave_mj_m2")
    low <- c(-60, -60, -60, -90, 0, 0, 0, 40, 0)
    high <- c(60, 60, 60, 60, 100, 1500, 100, 110, 60)
    fam_dates <- list(MET = dates_all,
      SRB = dates_all[year_all >= 1984 & year_all <= 2000],
      CERES = dates_all[year_all >= 2001])
    load_cell <- function(k) {
      row <- man[k, , drop = FALSE]
      d <- as.data.frame(readRDS(row$local_daily), stringsAsFactors = FALSE)
      vars <- if (row$family == "MET") raw_vars[1:8] else raw_vars[9]
      need(d, c("date", "cell_id", "family", "source", "source_product", "time_standard", vars), row$cell_id)
      assert(inherits(d$date, "Date") || is.character(d$date), "Unrecognized date encoding.")
      d$date <- as.Date(d$date)
      assert(!anyNA(d$date) && !anyDuplicated(d$date), "Missing/duplicate source dates.")
      d <- d[order(d$date), , drop = FALSE]
      assert(identical(as.character(d$date), as.character(fam_dates[[row$family]])), "Unexpected source calendar.")
      assert(all(d$cell_id == row$cell_id) && all(d$family == row$family) &&
        all(d$source == "NASA POWER") && all(d$time_standard == "UTC") &&
        all(d$source_product == row$source_product), "Source metadata do not match the manifest.")
      for (v in vars) d[[v]] <- number(d[[v]], paste(row$cell_id, v), TRUE)
      d
    }
    # This source check records the EXACT missing dates (not just their count).
    source_issues <- list()
    report$last_stage <<- "Checking local daily source files and hashes"
    for (k in seq_len(nrow(man))) {
      if (k == 1L || k %% 100L == 0L || k == nrow(man)) message("Local source check ", k, "/", nrow(man))
      assert(hash(man$local_daily[k]) == man$daily_md5[k], paste("Source file changed since step16:", man$cell_id[k]))
      d <- load_cell(k)
      vars <- if (man$family[k] == "MET") raw_vars[1:8] else raw_vars[9]
      for (v in vars) {
        j <- match(v, raw_vars); x <- d[[v]]
        bad <- !is.na(x) & (x < low[j] | x > high[j])
        problem <- is.na(x) | bad
        if (any(problem)) source_issues[[length(source_issues) + 1L]] <- data.frame(
          cell_id = man$cell_id[k], family = man$family[k], date = as.character(d$date[problem]),
          variable = v, original_value = x[problem], issue = ifelse(is.na(x[problem]), "MISSING_SOURCE_VALUE", "OUTSIDE_BROAD_BOUNDS"),
          stringsAsFactors = FALSE)
      }
      report$source_histories_checked <<- k
    }
    issue_tab <- bind(source_issues)
    if (!nrow(issue_tab)) issue_tab <- data.frame(cell_id = character(), family = character(), date = character(),
      variable = character(), original_value = numeric(), issue = character())
    write_csv(issue_tab, file.path(out_dir, "coffee_power_brazil_source_value_issues.csv"))
    affected <- if (nrow(issue_tab)) merge(issue_tab, idx[c("cell_id", "family", "municipality_code", "municipality")],
      by = c("cell_id", "family"), all.x = TRUE, sort = FALSE) else data.frame(
        cell_id = character(), family = character(), date = character(), variable = character(),
        original_value = numeric(), issue = character(), municipality_code = character(), municipality = character())
    write_csv(affected, file.path(out_dir, "coffee_power_brazil_source_issues_by_municipality.csv"))
    report$unique_source_missing_values <<- sum(issue_tab$issue == "MISSING_SOURCE_VALUE")
    report$source_out_of_bounds_values <<- sum(issue_tab$issue == "OUTSIDE_BROAD_BOUNDS")
    rm(d)
    # A small source-file cache prevents repeated reads while keeping RAM bounded.
    source_cache <- new.env(parent = emptyenv()); fifo <- character()
    get_cell <- function(id) {
      if (exists(id, envir = source_cache, inherits = FALSE)) return(get(id, envir = source_cache))
      if (length(fifo) >= 12L) { rm(list = fifo[1L], envir = source_cache); fifo <<- fifo[-1L] }
      k <- match(id, man$cell_id)
      assert(!is.na(k), "Unknown source cell.")
      d <- load_cell(k)
      assign(id, d, envir = source_cache); fifo <<- c(fifo, id)
      d
    }
    order_sites <- order(sites$MET_cell_id, sites$SRB_cell_id, sites$municipality_code)
    cache_files <- setNames(file.path(cache_dir, paste0("M_", codes, "_indices.rds")), codes)
    for (ii in seq_along(order_sites)) {
      site <- sites[order_sites[ii], , drop = FALSE]
      code <- site$municipality_code
      ids <- c(site$MET_cell_id, site$SRB_cell_id, site$CERES_cell_id)
      signature <- list(version = version, municipality_code = code, latitude = site$latitude,
        response_elevation = site$response_elevation_for_rso_m, cell_ids = ids,
        daily_md5 = man$daily_md5[match(ids, man$cell_id)])
      cache_file <- cache_files[[code]]
      sidecar <- paste0(cache_file, ".md5.txt")
      valid_cache_bytes <- file.exists(cache_file) && file.exists(sidecar) &&
        identical(paste(readLines(sidecar, warn = FALSE), collapse = ""), hash(cache_file))
      cached <- if (valid_cache_bytes) tryCatch(readRDS(cache_file), error = function(e) NULL) else NULL
      reuse <- is.list(cached) && identical(cached$signature, signature) &&
        is.data.frame(cached$annual) && nrow(cached$annual) == length(years) &&
        is.data.frame(cached$monthly) && nrow(cached$monthly) == length(years) * 12L &&
        is.data.frame(cached$qa) && nrow(cached$qa) == 1L
      if (!reuse) {
        if (report$new_municipalities_calculated >= limit_new) {
          report$execution_status <<- "PAUSED_LOCAL_MUNICIPALITY_LIMIT"
          report$last_stage <<- "Derivative caches retained. Rerun to continue. No partial national mean is published."
          save_progress(); message(report$last_stage); return(invisible(NULL))
        }
        message(sprintf("[%d/%d] %s | local indices, no download", ii, n_sites, site$municipality))
        met <- get_cell(site$MET_cell_id)
        srb <- get_cell(site$SRB_cell_id)
        ceres <- get_cell(site$CERES_cell_id)
        d <- met[c("date", raw_vars[1:8])]
        d$shortwave_mj_m2 <- NA_real_
        d$shortwave_mj_m2[match(srb$date, d$date)] <- srb$shortwave_mj_m2
        d$shortwave_mj_m2[match(ceres$date, d$date)] <- ceres$shortwave_mj_m2
        clean <- list(); outside <- rep(FALSE, nrow(d))
        for (j in seq_along(raw_vars)) {
          v <- raw_vars[j]; x <- d[[v]]
          bad <- !is.na(x) & (x < low[j] | x > high[j])
          outside <- outside | bad; x[bad] <- NA_real_; clean[[v]] <- x
        }
        bad_order <- (!is.na(clean$tmin_c) & !is.na(clean$tmean_c) & clean$tmin_c > clean$tmean_c + 0.05) |
          (!is.na(clean$tmean_c) & !is.na(clean$tmax_c) & clean$tmean_c > clean$tmax_c + 0.05) |
          (!is.na(clean$tmin_c) & !is.na(clean$tmax_c) & clean$tmin_c > clean$tmax_c)
        for (v in c("tmean_c", "tmin_c", "tmax_c")) clean[[v]][bad_order] <- NA_real_
        geo <- solar_geometry(d$date, site$latitude, site$response_elevation_for_rso_m)
        calc <- pm(clean$tmin_c, clean$tmax_c, clean$dewpoint_mean_c, clean$wind2m_mean_m_s,
          clean$surface_pressure_kpa, clean$shortwave_mj_m2, geo$ra_mj_m2, geo$rso_mj_m2)
        derived <- cbind(data.frame(date = d$date, tmean_usable_c = clean$tmean_c,
          tmin_usable_c = clean$tmin_c, tmax_usable_c = clean$tmax_c,
          precipitation_usable_mm = clean$precipitation_mm, shortwave_usable_mj_m2 = clean$shortwave_mj_m2,
          rhmean_usable_pct = clean$rhmean_pct), calc)
        annual <- summarize_periods(derived, cg_year)
        monthly <- summarize_periods(derived, cg_month)
        for (nm in c("municipality_code", "municipality", "state", "MET_cell_id", "SRB_cell_id", "CERES_cell_id")) {
          annual[[nm]] <- site[[nm]]; monthly[[nm]] <- site[[nm]]
        }
        annual$source <- monthly$source <- "NASA POWER"
        annual$time_standard <- monthly$time_standard <- "UTC"
        source_gap <- Reduce(`|`, lapply(clean[raw_vars[1:8]], is.na)) |
          (year_all >= 1984 & is.na(clean$shortwave_mj_m2))
        critical <- source_gap | outside | bad_order | calc$flag_negative_vpd |
          calc$flag_negative_et0_raw | calc$flag_et0_formula_domain
        flags <- data.frame(municipality_code = rep(code, sum(critical)), date = as.character(d$date[critical]),
          missing_dependent_source = source_gap[critical], outside_bounds = outside[critical],
          temperature_order = bad_order[critical], negative_vpd = calc$flag_negative_vpd[critical],
          et0_domain = calc$flag_et0_formula_domain[critical], negative_et0_raw = calc$flag_negative_et0_raw[critical],
          vpd_raw_kpa = calc$vpd_fao56_raw_kpa[critical], et0_raw_mm = calc$et0_fao56_raw_mm[critical],
          stringsAsFactors = FALSE)
        qa <- data.frame(municipality_code = code, municipality = site$municipality,
          MET_cell_id = site$MET_cell_id, SRB_cell_id = site$SRB_cell_id, CERES_cell_id = site$CERES_cell_id,
          daily_rows = nrow(d), latitude_for_solar_geometry = site$latitude,
          response_elevation_for_rso_m = site$response_elevation_for_rso_m,
          vpd_available_days = sum(is.finite(calc$vpd_fao56_estimated_kpa)),
          et0_expected_days = sum(year_all >= 1984), et0_available_days = sum(is.finite(calc$et0_reference_mm)),
          missing_solar_available_period_days = sum(year_all >= 1984 & is.na(clean$shortwave_mj_m2)),
          negative_vpd_days = sum(calc$flag_negative_vpd), temperature_order_failures = sum(bad_order),
          input_outside_bounds_days = sum(outside), cloud_ratio_lower_bound_days = sum(calc$flag_cloud_ratio_lower_bound),
          cloud_ratio_upper_bound_days = sum(calc$flag_cloud_ratio_upper_bound),
          negative_et0_raw_days = sum(calc$flag_negative_et0_raw), et0_domain_failures = sum(calc$flag_et0_formula_domain),
          annual_et0_complete_years = sum(is.finite(annual$et0_total_mm)),
          maximum_annual_et0_sensitivity_mm = if (any(is.finite(annual$et0_total_mm)))
            max(abs(annual$et0_no_lower_cloud_bound_total_mm - annual$et0_total_mm), na.rm = TRUE) else NA_real_,
          station_validation_performed = FALSE, raw_observations_changed = FALSE)
        cached <- list(signature = signature, annual = annual, monthly = monthly, qa = qa, critical_days = flags)
        write_rds(cached, cache_file)
        writeLines(hash(cache_file), paste0(cache_file, ".md5.txt"))
        report$new_municipalities_calculated <<- report$new_municipalities_calculated + 1L
      } else {
        report$municipalities_reused <<- report$municipalities_reused + 1L
        if (ii %% 50L == 0L) message("Reused municipality cache ", ii, "/", n_sites)
      }
      report$municipalities_processed <<- ii
      report$last_stage <<- paste("Municipality", code, ii, "of", n_sites)
      progress[[code]] <<- data.frame(municipality_code = code, cache_file = cache_file,
        cache_md5 = hash(cache_file), reused = reuse, annual_rows = nrow(cached$annual),
        monthly_rows = nrow(cached$monthly), negative_vpd_days = cached$qa$negative_vpd_days)
      if (ii %% 20L == 0L || ii == n_sites) save_progress()
    }
    rm(list = ls(source_cache), envir = source_cache)
    # Stream monthly CSV into a temporary file, avoiding a million-row
    # in-memory data frame. Annual data remain small enough for an RDS/CSV.
    report$last_stage <<- "Writing municipal annual and monthly datasets"
    monthly_final <- file.path(derived_dir, "coffee_power_brazil_municipality_monthly_indices.csv")
    monthly_tmp <- tempfile("monthly_", tmpdir = derived_dir, fileext = ".csv")
    on.exit(unlink(monthly_tmp), add = TRUE)
    con <- file(monthly_tmp, open = "wt", encoding = "UTF-8")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    aa <- qq <- ff <- vector("list", n_sites)
    for (i in seq_along(codes)) {
      z <- readRDS(cache_files[[codes[i]]])
      aa[[i]] <- z$annual; qq[[i]] <- z$qa; ff[[i]] <- z$critical_days
      utils::write.table(z$monthly, con, sep = ",", row.names = FALSE,
        col.names = i == 1L, quote = TRUE, na = "NA", qmethod = "double")
    }
    close(con)
    assert(file.copy(monthly_tmp, monthly_final, overwrite = TRUE), "Cannot write monthly dataset.")
    annual <- bind(aa); qa <- bind(qq); flagged <- bind(ff)
    annual <- annual[order(annual$municipality_code, annual$year), , drop = FALSE]
    assert(nrow(annual) == n_sites * length(years) &&
      !anyDuplicated(paste(annual$municipality_code, annual$year)), "Annual municipal keys disagree.")
    write_rds(annual, file.path(derived_dir, "coffee_power_brazil_municipality_annual_indices.rds"))
    write_csv(annual, file.path(derived_dir, "coffee_power_brazil_municipality_annual_indices.csv"))
    write_csv(qa, file.path(out_dir, "coffee_power_brazil_indices_quality.csv"))
    if (!nrow(flagged)) flagged <- data.frame(municipality_code = character(), date = character(),
      missing_dependent_source = logical(), outside_bounds = logical(), temperature_order = logical(),
      negative_vpd = logical(), et0_domain = logical(), negative_et0_raw = logical(),
      vpd_raw_kpa = numeric(), et0_raw_mm = numeric())
    write_csv(flagged, file.path(out_dir, "coffee_power_brazil_indices_critical_days.csv"))
    report$municipal_annual_rows <<- nrow(annual)
    report$municipal_monthly_rows <<- n_sites * length(years) * 12L
    report$municipal_negative_vpd_days <<- sum(qa$negative_vpd_days)
    report$municipal_et0_missing_available_period_days <<- sum(qa$et0_expected_days - qa$et0_available_days)
    rm(aa, qq, ff, z)

    # Area and production weights are separate analyses. Total is never
    # added to Arabica+Canephora. A missing local estimate propagates only
    # to categories/years with a positive weight on that local estimate.
    report$last_stage <<- "Calculating fixed and dynamic weighted exposure"
    exposure_parts <- list()
    for (yr in years) {
      ay <- annual[annual$year == yr, , drop = FALSE]
      ay <- ay[match(codes, ay$municipality_code), , drop = FALSE]
      values <- as.matrix(ay[definitions$metric])
      for (sp in c("Total", "Arabica", "Canephora")) {
        ref <- wb[[paste(sp, min(weight_years), sep = "_")]]
        specs <- list(Fixed_2012_2015_area = ref$baseline_area_weight,
                      Fixed_2012_2015_production = ref$baseline_production_weight)
        if (yr %in% weight_years) {
          dy <- wb[[paste(sp, yr, sep = "_")]]
          specs$Annual_area <- dy$dynamic_area_weight
          specs$Annual_production <- dy$dynamic_production_weight
        }
        for (name in names(specs)) {
          tab <- spatial_stats(values, specs[[name]])
          tab$species <- sp; tab$year <- yr; tab$weighting <- name
          tab$unit <- definitions$unit[match(tab$metric, definitions$metric)]
          tab$positive_weight_municipalities <- sum(specs[[name]] > 0)
          tab$positive_weight_MET_cells <- length(unique(sites$MET_cell_id[specs[[name]] > 0]))
          tab$solar_source_epoch <- if (yr < 1984) "NOT_AVAILABLE_BEFORE_1984" else
            if (yr <= 2000) "SRB_1984_2000" else "CERES_2001_2024"
          tab$source <- "NASA POWER"
          tab$aggregation <- "Weighted mean of local annual indices; not a territorial national mean"
          exposure_parts[[length(exposure_parts) + 1L]] <- tab
        }
      }
    }
    exposure <- bind(exposure_parts)
    # Fixed-geography anomalies only for meteorological indices, never for
    # SRB/CERES-crossing solar, ET0 or P-ET0 series.
    met_metrics <- setdiff(definitions$metric, c("solar_mean_mj_m2_day", "et0_total_mm",
      "et0_no_lower_cloud_bound_total_mm", "p_minus_et0_mm"))
    exposure$reference_mean_1981_2010 <- NA_real_
    exposure$reference_valid_years <- NA_integer_
    exposure$anomaly_1981_2010 <- NA_real_
    base_parts <- list()
    for (sp in c("Total", "Arabica", "Canephora")) {
      for (name in c("Fixed_2012_2015_area", "Fixed_2012_2015_production")) {
        for (metric in met_metrics) {
          k <- which(exposure$species == sp & exposure$weighting == name & exposure$metric == metric)
          b <- k[exposure$year[k] %in% baseline_years]
          n <- sum(is.finite(exposure$estimate_strict[b]))
          mean_b <- if (length(b) == length(baseline_years) && n == length(baseline_years))
            mean(exposure$estimate_strict[b]) else NA_real_
          exposure$reference_mean_1981_2010[k] <- mean_b
          exposure$reference_valid_years[k] <- n
          exposure$anomaly_1981_2010[k] <- exposure$estimate_strict[k] - mean_b
          base_parts[[length(base_parts) + 1L]] <- data.frame(species = sp, weighting = name,
            metric = metric, reference_period = "1981 to 2010", valid_years = n, reference_mean = mean_b)
        }
      }
    }
    write_csv(exposure, file.path(out_dir, "coffee_power_brazil_weighted_exposure_annual.csv"))
    write_csv(bind(base_parts), file.path(out_dir, "coffee_power_brazil_fixed_exposure_baselines.csv"))
    write_csv(exposure[exposure$observed_weight_fraction < 1 - 1e-12 |
      !is.finite(exposure$estimate_strict), , drop = FALSE],
      file.path(out_dir, "coffee_power_brazil_weighted_exposure_gaps.csv"))
    comparisons <- list()
    for (sp in c("Total", "Arabica", "Canephora")) {
      for (name in unique(exposure$weighting)) {
        for (metric in definitions$metric) {
          z <- exposure[exposure$species == sp & exposure$weighting == name & exposure$metric == metric, ]
          b <- z$estimate_strict[z$year %in% initial_years]
          r <- z$estimate_strict[z$year %in% recent_years]
          bm <- if (length(b) == 4L) strict(b, mean) else NA_real_
          recent_mean <- if (length(r) == 4L) strict(r, mean) else NA_real_
          comparisons[[length(comparisons) + 1L]] <- data.frame(species = sp, weighting = name,
            metric = metric, unit = definitions$unit[match(metric, definitions$metric)],
            baseline_period = "2012 to 2015", recent_period = "2021 to 2024",
            baseline_mean = bm, recent_mean = recent_mean, absolute_change = recent_mean - bm,
            baseline_valid_years = sum(is.finite(b)), recent_valid_years = sum(is.finite(r)),
            interpretation = "Difference of weighted annual-index means; not a trend or causal climate effect")
        }
      }
    }
    comparisons <- bind(comparisons)
    write_csv(comparisons, file.path(out_dir, "coffee_power_brazil_exposure_period_comparison.csv"))
    report$weighted_exposure_rows <<- nrow(exposure)
    report$incomplete_weighted_exposure_rows <<- sum(!is.finite(exposure$estimate_strict))
    report$period_comparison_rows <<- nrow(comparisons)
    report$climate_weighted_by_coffee_distribution <<- TRUE
    notes <- c("COFFE-STUDY | STEP17 | MUNICIPAL INDICES AND WEIGHTED CLIMATE EXPOSURE",
      paste("Script:", version), paste("Executed:", format(Sys.time(), usetz = TRUE)),
      "No downloads; all input data are the unchanged step16 NASA source-cell outputs.",
      "The original municipal reference latitude is used for solar geometry, as in step13.",
      "Rso elevation is the MET source-cell response elevation, not the pilot point elevation or coffee-field altitude.",
      "Solar uses the municipality's separate SRB and CERES IDs. It is joined to MET by date BEFORE ET0.",
      "NASA mean T2M is used for climate means. ET0 uses mean(Tmin,Tmax) per FAO56 convention.",
      "FAO56 es = mean(es(Tmin),es(Tmax)); ea = es(mean dewpoint). Estimated VPD is not daily maximum.",
      "The same step13 Rs/Rso [0.30,1] policy and upper-only sensitivity are retained.",
      "Negative VPD is retained in critical-day diagnostics and excluded from usable VPD/ET0; never replaced.",
      "No source missing value is imputed. A missing day invalidates only dependent local period indices.",
      "The exact source gap dates and affected municipalities are exported separately.",
      "Monthly/annual dry and hot sequences and rain windows are bounded by their calendar periods.",
      "One municipal derivative cache stores annual/monthly indices; no 1876-fold daily replication is written.",
      "Meteorological samples shared by municipalities are not independent climate observations.",
      "Counts of flagged municipal-days can repeat the same underlying source-cell event across municipalities.",
      "Fixed weights are 2012-2015 municipal area or production; their 1981-2024 series uses a fixed reference geography.",
      "Dynamic weights use each observed year, only 2012-2024; no historical weights are invented.",
      "Total, Arabica and Canephora remain separate. Production weights are a sensitivity, not the primary climate weighting.",
      "The primary spatial estimate requires every positive municipal weight to have a valid local index.",
      "A separately labelled available-weight-normalized diagnostic and observed weight fraction are exported.",
      "The available-weight diagnostic is not substituted for the strict series or its baseline.",
      "A weighted local longest dry spell is not a synchronized national contiguous drought event.",
      "A temperature threshold count is not an observed coffee injury count; ET0 is not actual coffee water use.",
      "P minus ET0 is a climatic balance, without roots, soil storage, irrigation, runoff or drainage.",
      "Only fixed-geography meteorological anomalies are computed against complete 1981-2010 baselines.",
      "Solar, ET0 and P-ET0 are not given trends or baseline anomalies across the 2001 source transition.",
      "Four-year comparisons use CERES at both endpoints and are not trend estimates or attribution.",
      "Historical municipal boundaries and field locations remain unharmonized/unmeasured.",
      "No station validation, yield regression, altitude effect, trend test or main figure is produced.")
    writeLines(notes, file.path(out_dir, "coffee_power_brazil_indices_method_notes.txt"))
    assert(all(vapply(paths, hash, character(1)) == inputs_md5), "A controlling input changed during the run.")
    report$execution_status <<- if (report$municipal_et0_missing_available_period_days > 0L ||
      report$source_out_of_bounds_values > 0L) "COMPLETED_NATIONAL_INDICES_WITH_GAPS" else "COMPLETED_NATIONAL_INDICES"
    report$last_stage <<- "Municipal annual/monthly indices and fixed/dynamic weighted exposure saved. No trend or attribution estimated."
    save_progress()
    message("\n", report$execution_status, "\nReports: ", out_dir, "\nMunicipal data: ", derived_dir)
    invisible(report)
  }
  tryCatch(worker(), error = function(e) {
    report$execution_status <<- "INCOMPLETE"
    report$error_if_incomplete <<- substr(conditionMessage(e), 1, 1600)
    try(save_progress(), silent = TRUE)
    stop(conditionMessage(e), "\nNo original observations changed. Valid derivative caches retained.\nSummary: ",
      file.path(out_dir, "coffee_power_brazil_indices_summary.csv"), call. = FALSE)
  }, interrupt = function(e) {
    report$execution_status <<- "INTERRUPTED_RESUMABLE"
    report$error_if_incomplete <<- "Interrupted; valid municipal derivative caches retained."
    try(save_progress(), silent = TRUE)
    message("\nInterrupted. Rerun the same script to resume from validated derivative caches.")
    invisible(NULL)
  })
}

coffee_power_step17()
