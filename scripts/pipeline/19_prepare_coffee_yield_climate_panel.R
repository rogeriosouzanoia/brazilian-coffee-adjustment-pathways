# ======================================================================
# COFFE-STUDY | STEP 19 | MUNICIPAL PRODUCTIVITY AND CLIMATE PANEL
# Run the COMPLETE file with Source in RStudio. Uses base R only.
#
# Inputs: audited PAM, NASA POWER annual indices, fixed-code membership,
#         and the completed Figure 3 / step 08 provenance.
# Scope: Arabica and Canephora, 2012-2024. Total is NOT added to species.
# No downloads, daily climate recalculation, new main figure, or climate
# response regression. All earlier inputs/outputs remain unchanged.
#
# This prepares data for the production-shock analysis behind Figure 4:
#   - exact municipality/species/year joins;
#   - climate in calendar year t, t-1 and t-2, kept separately;
#   - local complete 1981-2010 meteorological references;
#   - retrospective productivity departures, with two reference choices;
#   - observed 1- and 2-year productivity changes and explicit eligibility.
#
# IMPORTANT: t is the PAM reporting year, not an observed harvest date.
# Same-year annual climate can include weather AFTER harvest. These columns
# must NOT automatically be used as preharvest predictors. Region/species
# harvest calendars and phenological windows remain a later modelling step.
#
# The reference fits are descriptive, NOT climate-counterfactual yields:
# log(productivity) ~ linear year; sensitivity adds an odd/even-year term.
# Parity allows a fitted local alternating contrast, but is NOT a validated
# model of biennial bearing. Both specifications are retained, not selected
# by whichever one produces a stronger climate association.
# A reference for each observed year excludes that year's observation.
# Other training years can be later: this is NOT real-time forecasting.
# No climate effect, causal loss, adaptation efficacy or p-value is computed.
#
# R implementation references:
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.influence.html
# ======================================================================

coffee_step19 <- function() {
  version <- "19_yield_climate_panel_v1"
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  study_years <- 2012:2024
  reference_years <- 1981:2010
  species_order <- c("Arabica", "Canephora")
  minimum_positive_yield_years <- 10L
  minimum_years_per_parity <- 4L
  climate_lags <- 0:2
  # Explicit study choices, not universal coffee injury thresholds.
  primary_metrics <- c("tmean_c", "hot_days_ge32", "precipitation_total_mm", "longest_dry_spell_days")
  met_metrics <- c("tmean_c", "tmin_mean_c", "tmax_mean_c", "hot_days_ge32",
                   "hot_days_ge35", "precipitation_total_mm", "longest_dry_spell_days",
                   "vpd_fao56_mean_kpa", "rhmean_pct")
  solar_metrics <- c("et0_total_mm", "p_minus_et0_mm", "solar_mean_mj_m2_day")
  metrics <- c(met_metrics, solar_metrics)
  metric_units <- c("degC", "degC", "degC", "days", "days", "mm", "days", "kPa", "%",
                    "mm", "mm", "MJ m-2 day-1")
  test_only <- identical(toupper(Sys.getenv("COFFEE_STEP19_TEST_ONLY", "FALSE")), "TRUE")
  out_dir <- file.path(base_dir, "04_outputs", "climate_yield_panel")
  panel_dir <- file.path(base_dir, "02_data_processed", "climate_yield_panel")
  for (p in c(out_dir, panel_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create: ", p, call. = FALSE)
  }
  old_options <- options(scipen = 999)
  on.exit(options(old_options), add = TRUE)
  lock <- file.path(out_dir, ".step19_running")
  if (!dir.create(lock, showWarnings = FALSE)) stop(
    "Another step19 session may be running. Confirm that it has stopped before removing:\n", lock, call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  writeLines(paste("PID", Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))

  assert <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
  need <- function(x, columns, label) {
    absent <- setdiff(columns, names(x))
    assert(!length(absent), paste(label, "missing columns:", paste(absent, collapse = ", ")))
  }
  key <- function(x, columns) do.call(paste, c(x[columns], sep = "|"))
  hash <- function(path) unname(tools::md5sum(path))
  number <- function(x, label, allow_na = TRUE) {
    original <- as.character(x)
    z <- suppressWarnings(as.numeric(original))
    assert(!any(!is.na(original) & is.na(z)) && all(is.finite(z) | is.na(z)) &&
             (allow_na || !anyNA(z)), paste("Invalid numeric column:", label))
    z
  }
  read_csv <- function(path) {
    assert(file.exists(path), paste("Required input not found:", path))
    utils::read.csv(path, colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  checked <- function(x, name) {
    need(x, c("check", "result"), "Summary")
    y <- x$result[x$check == name]
    assert(length(y) == 1L && !is.na(y), paste("Missing or repeated check:", name))
    y
  }
  write_csv <- function(x, path) {
    tmp <- tempfile("coffee19_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  write_rds <- function(x, path) {
    tmp <- tempfile("coffee19_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  bind <- function(x) {
    if (!length(x)) return(data.frame())
    ans <- do.call(rbind, x)
    rownames(ans) <- NULL
    ans
  }
  safe_ratio_pct <- function(n, d) if (is.finite(d) && d > 0) 100 * n / d else NA_real_
  same_na <- function(x, y, tol = 1e-8) {
    ok <- is.na(x) & is.na(y)
    ii <- is.finite(x) & is.finite(y)
    ok[ii] <- abs(x[ii] - y[ii]) <= tol * pmax(1, abs(x[ii]), abs(y[ii]))
    ok
  }
  productivity <- function(p, a) {
    ans <- rep(NA_real_, length(p))
    ok <- is.finite(p) & is.finite(a) & a > 0 & p >= 0
    ans[ok] <- 1000 * p[ok] / a[ok]
    ans
  }

  # Small fixed-design OLS fits. LOO is computed by the exact leverage
  # identity and checked against explicit refits in the numerical tests.
  reference_fit <- function(year, log_y, with_parity = FALSE) {
    n <- length(year)
    assert(n == length(log_y) && all(is.finite(year)) && all(is.finite(log_y)),
      "Invalid reference-fit input.")
    x <- cbind(intercept = 1, centered_year = year - 2018)
    if (with_parity) x <- cbind(x, odd_year = year %% 2L)
    fit <- stats::lm.fit(x, log_y)
    if (fit$rank != ncol(x) || n <= ncol(x) + 1L) return(NULL)
    q <- qr.Q(fit$qr)[, seq_len(fit$rank), drop = FALSE]
    h <- rowSums(q^2)
    if (any(!is.finite(h) | h >= 1 - 1e-10)) return(NULL)
    residual_loo <- as.numeric(fit$residuals) / (1 - h)
    fitted_loo <- log_y - residual_loo
    list(fitted_loo = fitted_loo, residual_loo = residual_loo,
         departure_pct = 100 * expm1(residual_loo), leverage = h,
         coefficients = as.numeric(fit$coefficients),
         rmse_log_loo = sqrt(mean(residual_loo^2)), rank = fit$rank)
  }
  tests <- list()
  add_test <- function(name, pass) {
    tests[[length(tests) + 1L]] <<- data.frame(test = name, pass = isTRUE(pass))
  }
  yy <- 2012:2024
  ly <- log(1400) + 0.02 * (yy - 2018) + 0.13 * (yy %% 2L) + 0.03 * sin(yy)
  for (par in c(FALSE, TRUE)) {
    z <- reference_fit(yy, ly, par)
    xx <- cbind(1, yy - 2018)
    if (par) xx <- cbind(xx, yy %% 2L)
    brute <- vapply(seq_along(yy), function(i) {
      ff <- stats::lm.fit(xx[-i, , drop = FALSE], ly[-i])
      as.numeric(xx[i, , drop = FALSE] %*% ff$coefficients)
    }, numeric(1))
    add_test(paste0("LOO identity vs explicit refit; parity=", par),
             max(abs(z$fitted_loo - brute)) < 1e-9)
  }
  add_test("Undefined productivity stays missing", is.na(productivity(0, 0)))
  add_test("Zero productivity with positive area stays zero", productivity(0, 1) == 0)
  add_test("Units tonnes to kg per hectare", productivity(20, 10) == 2000)
  add_test("Ten percent negative departure", abs(100 * expm1(log(0.9)) + 10) < 1e-10)
  add_test("Missing calendar year is not the previous row",
           is.na(match(2014 - 1L, c(2012L, 2014L))))
  add_test("Solar dependent variables excluded from 1981-2010 reference",
           !any(solar_metrics %in% met_metrics))
  write_csv(bind(tests), file.path(out_dir, "coffee_yield_climate_numerical_tests.csv"))
  assert(all(bind(tests)$pass), "Step19 numerical test failed.")
  if (test_only) {
    message("Step19 numerical tests passed. Test-only mode: no study data read.")
    return(invisible(bind(tests)))
  }

  report <- list(execution_status = "INCOMPLETE", script_version = version,
    species = paste(species_order, collapse = " | "), reporting_period = "2012 to 2024",
    municipal_targets = 0L, panel_rows = 0L, positive_productivity_rows = 0L,
    positive_area_zero_production_rows = 0L, no_harvested_area_rows = 0L,
    series_with_trend_reference = 0L, series_with_parity_reference = 0L,
    climate_lags = "0 | 1 | 2 calendar years", minimum_positive_yield_years = minimum_positive_yield_years,
    minimum_years_per_parity = minimum_years_per_parity,
    missing_primary_climate_join_rows = 0L, numerical_tests_passed = TRUE,
    source_data_downloaded = FALSE, missing_data_imputed = FALSE,
    farm_harvest_dates_available = FALSE, phenological_alignment_verified = FALSE,
    real_time_prediction = FALSE, climate_response_regression = FALSE,
    causal_attribution_performed = FALSE, historical_boundaries_harmonized = FALSE,
    previous_inputs_modified = FALSE, error_if_incomplete = "None")
  save_summary <- function() write_csv(data.frame(check = names(report),
    result = vapply(report, as.character, character(1))), file.path(out_dir, "coffee_yield_climate_panel_summary.csv"))
  on.exit({
    try(save_summary(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step19.txt")), silent = TRUE)
  }, add = TRUE)

  worker <- function() {
    paths <- c(
      production = file.path(base_dir, "02_data_processed", "coffee_brazil_municipality_audited_1974_2024.rds"),
      climate = file.path(base_dir, "02_data_processed", "climate", "NASA_POWER", "brazil_indices", "coffee_power_brazil_municipality_annual_indices.rds"),
      membership = file.path(base_dir, "04_outputs", "geographic_redistribution", "coffee_redistribution_panel_membership.csv"),
      step08 = file.path(base_dir, "04_outputs", "geographic_redistribution", "coffee_redistribution_summary.csv"),
      step18 = file.path(base_dir, "04_outputs", "figure03_climate_exposure", "figure03_climate_summary.csv"),
      step18_provenance = file.path(base_dir, "04_outputs", "figure03_climate_exposure", "figure03_input_provenance.csv"))
    assert(all(file.exists(paths)), paste("Missing input(s):\n", paste(paths[!file.exists(paths)], collapse = "\n")))
    prov <- data.frame(role = names(paths), file = unname(paths),
                       md5 = vapply(paths, hash, character(1)), stringsAsFactors = FALSE)
    write_csv(prov, file.path(out_dir, "coffee_yield_climate_input_provenance.csv"))
    s08 <- read_csv(paths[["step08"]])
    s18 <- read_csv(paths[["step18"]])
    p18 <- read_csv(paths[["step18_provenance"]])
    assert(grepl("^COMPLETED", checked(s18, "execution_status")) &&
             checked(s18, "weighted_checks_passed") == "TRUE", "Figure 3 verification is not complete.")
    assert(checked(s08, "official_validation_passed_for_input") == "TRUE" &&
             hash(paths[["production"]]) == checked(s08, "input_md5"),
             "PAM input has changed or its official validation did not pass.")
    need(p18, c("role", "md5"), "Figure 3 provenance")
    h <- p18$md5[p18$role == "annual"]
    assert(length(h) == 1L && identical(hash(paths[["climate"]]), h), "Annual climate file differs from Figure 3 input.")
    prod <- as.data.frame(readRDS(paths[["production"]]), stringsAsFactors = FALSE)
    clim <- as.data.frame(readRDS(paths[["climate"]]), stringsAsFactors = FALSE)
    need(prod, c("municipality_code", "municipality", "state", "year", "species", "production_t", "area_harvested_ha"), "PAM")
    need(clim, c("municipality_code", "municipality", "state", "year", "MET_cell_id", "SRB_cell_id", "CERES_cell_id", "source", "time_standard", "solar_source_epoch", metrics), "Climate")
    prod$municipality_code <- as.character(prod$municipality_code)
    prod$species <- as.character(prod$species)
    prod$year <- number(prod$year, "PAM year", FALSE)
    clim$municipality_code <- as.character(clim$municipality_code)
    clim$year <- number(clim$year, "climate year", FALSE)
    assert(!anyDuplicated(key(prod, c("municipality_code", "species", "year"))), "Duplicate PAM key.")
    assert(!anyDuplicated(key(clim, c("municipality_code", "year"))), "Duplicate climate key.")
    assert(all(clim$source == "NASA POWER") && all(clim$time_standard == "UTC"), "Unexpected climate source/time standard.")
    for (m in metrics) clim[[m]] <- number(clim[[m]], m)
    for (m in c("production_t", "area_harvested_ha")) {
      prod[[m]] <- number(prod[[m]], m)
      assert(all(prod[[m]] >= 0, na.rm = TRUE), paste("Negative", m))
    }
    membership <- read_csv(paths[["membership"]])
    need(membership, c("municipality_code", "included"), "Fixed-code membership")
    assert(!anyDuplicated(membership$municipality_code) && all(membership$included %in% c("TRUE", "FALSE")), "Invalid membership.")
    panel_codes <- membership$municipality_code[membership$included == "TRUE"]
    target_codes <- sort(unique(clim$municipality_code))
    assert(length(target_codes) == as.integer(checked(s18, "municipal_targets")) &&
             all(target_codes %in% panel_codes), "Climate targets are not the expected fixed-code subset.")
    assert(all(grepl("^[0-9]{7}$", target_codes)), "Invalid municipality codes.")
    report$municipal_targets <<- length(target_codes)
    d <- prod[prod$municipality_code %in% target_codes & prod$species %in% species_order & prod$year %in% study_years, , drop = FALSE]
    d <- d[order(d$municipality_code, match(d$species, species_order), d$year), , drop = FALSE]
    rownames(d) <- NULL
    assert(nrow(d) == length(target_codes) * length(species_order) * length(study_years), "Unexpected municipality/species/year coverage.")
    assert(!anyNA(d$production_t) && !anyNA(d$area_harvested_ha), "Missing PAM values inside the inherited complete panel.")
    assert(!any(d$production_t > 0 & d$area_harvested_ha == 0), "Positive production with zero harvested area needs review.")
    d$reporting_year <- as.integer(d$year)
    d$calendar_parity_odd <- as.integer(d$year %% 2L)
    d$analysis_yield_kg_ha <- productivity(d$production_t, d$area_harvested_ha)
    if ("yield_calculated_kg_ha" %in% names(d)) assert(all(same_na(d$analysis_yield_kg_ha,
      number(d$yield_calculated_kg_ha, "audited calculated yield"))), "Calculated productivity differs from the audited ratio.")
    d$has_harvested_area <- d$area_harvested_ha > 0
    d$positive_yield_for_log <- is.finite(d$analysis_yield_kg_ha) & d$analysis_yield_kg_ha > 0
    d$zero_production_positive_area <- d$production_t == 0 & d$area_harvested_ha > 0
    d$log_yield <- NA_real_
    d$log_yield[d$positive_yield_for_log] <- log(d$analysis_yield_kg_ha[d$positive_yield_for_log])
    d$no_harvested_area <- d$area_harvested_ha == 0
    d$year_is_observed_harvest_date <- FALSE
    d$source_grid_climate_is_farm_observation <- FALSE

    message("Preparing local climate references and exact calendar joins...")
    groups <- split(seq_len(nrow(clim)), clim$municipality_code)
    refs <- list()
    for (code in target_codes) {
      z <- clim[groups[[code]], , drop = FALSE]
      assert(identical(sort(as.integer(z$year)), 1981:2024), paste("Incomplete climate-year keys:", code))
      for (column in c("MET_cell_id", "SRB_cell_id", "CERES_cell_id"))
        assert(length(unique(z[[column]])) == 1L && !anyNA(z[[column]]), paste("Changing or missing cell ID:", code, column))
      b <- z[z$year %in% reference_years, , drop = FALSE]
      for (m in met_metrics) {
        ok <- nrow(b) == length(reference_years) && all(is.finite(b[[m]]))
        refs[[length(refs) + 1L]] <- data.frame(municipality_code = code, metric = m,
          reference_period = "1981 to 2010", valid_years = sum(is.finite(b[[m]])),
          reference_mean = if (ok) mean(b[[m]]) else NA_real_,
          reference_complete = ok, stringsAsFactors = FALSE)
      }
    }
    refs <- bind(refs)
    write_csv(refs, file.path(out_dir, "coffee_yield_climate_local_references.csv"))
    rkey <- key(refs, c("municipality_code", "metric"))
    ckey <- key(clim, c("municipality_code", "year"))
    first <- match(d$municipality_code, clim$municipality_code)
    assert(!anyNA(first), "Missing climate target in join.")
    for (col in c("MET_cell_id", "SRB_cell_id", "CERES_cell_id")) d[[col]] <- as.character(clim[[col]][first])
    d$climate_source <- "NASA POWER"
    d$climate_time_standard <- "UTC"
    join_audit <- list()
    for (lag in climate_lags) {
      ii <- match(paste(d$municipality_code, d$year - lag, sep = "|"), ckey)
      assert(!anyNA(ii), paste("Missing exact calendar-year climate join, lag", lag))
      assert(all(clim$year[ii] == d$year - lag), "Off-by-one climate-year error.")
      assert(all(as.character(clim$state[ii]) == as.character(d$state)), "State-code inconsistency between source tables.")
      d[[paste0("climate_year_lag", lag)]] <- as.integer(clim$year[ii])
      d[[paste0("solar_source_epoch_lag", lag)]] <- as.character(clim$solar_source_epoch[ii])
      for (m in metrics) {
        col <- paste0("clim_", m, "_lag", lag)
        d[[col]] <- clim[[m]][ii]
        if (m %in% met_metrics) {
          r <- match(paste(d$municipality_code, m, sep = "|"), rkey)
          assert(!anyNA(r), "Missing local-reference key.")
          d[[paste0("clim_", m, "_anom1981_2010_lag", lag)]] <- d[[col]] - refs$reference_mean[r]
        }
        join_audit[[length(join_audit) + 1L]] <- data.frame(lag_years = lag, metric = m,
          joined_rows = nrow(d), nonmissing_values = sum(is.finite(d[[col]])),
          missing_values = sum(is.na(d[[col]])), exact_calendar_keys = TRUE)
      }
      pcols <- paste0("clim_", primary_metrics, "_lag", lag)
      d[[paste0("primary_climate_complete_lag", lag)]] <- stats::complete.cases(d[pcols])
      report$missing_primary_climate_join_rows <<- report$missing_primary_climate_join_rows +
        sum(!d[[paste0("primary_climate_complete_lag", lag)]])
    }
    write_csv(bind(join_audit), file.path(out_dir, "coffee_yield_climate_join_audit.csv"))

    # Observed annual and two-year differences: exact years, never row shifts.
    pkey <- key(d, c("municipality_code", "species", "year"))
    for (lag in 1:2) {
      j <- match(paste(d$municipality_code, d$species, d$year - lag, sep = "|"), pkey)
      d[[paste0("yield_kg_ha_lag", lag)]] <- d$analysis_yield_kg_ha[j]
      d[[paste0("log_yield_change_lag", lag)]] <- d$log_yield - d$log_yield[j]
      d[[paste0("yield_change_pct_lag", lag)]] <- 100 * expm1(d[[paste0("log_yield_change_lag", lag)]])
      # Absent 2010/2011 species records stay absent, not backfilled from Total.
    }

    for (model in c("trend", "trend_parity")) {
      for (suffix in c("reference_log_loo", "reference_kg_ha_loo", "departure_log_loo", "departure_pct_loo"))
        d[[paste0("yield_", model, "_", suffix)]] <- NA_real_
      d[[paste0("yield_", model, "_eligible")]] <- FALSE
    }
    d$positive_yield_years_in_series <- 0L
    d$active_any_study_year <- FALSE
    series_parts <- list()
    splits <- split(seq_len(nrow(d)), paste(d$municipality_code, d$species, sep = "|"))
    nseries <- length(splits)
    message("Calculating retrospective productivity references for ", nseries, " municipality/species series...")
    counter <- 0L
    for (ix in splits) {
      counter <- counter + 1L
      rows <- ix[d$positive_yield_for_log[ix]]
      n <- length(rows)
      nodd <- sum(d$calendar_parity_odd[rows] == 1L)
      neven <- n - nodd
      d$positive_yield_years_in_series[ix] <- n
      d$active_any_study_year[ix] <- any(d$has_harvested_area[ix] | d$production_t[ix] > 0)
      for (par in c(FALSE, TRUE)) {
        model <- if (par) "trend_parity" else "trend"
        status <- if (n < minimum_positive_yield_years) "INSUFFICIENT_POSITIVE_YEARS" else if
          (par && min(nodd, neven) < minimum_years_per_parity) "INSUFFICIENT_PARITY_SUPPORT" else "READY"
        fit <- NULL
        if (status == "READY") {
          fit <- reference_fit(d$year[rows], d$log_yield[rows], par)
          if (is.null(fit)) status <- "SINGULAR_OR_HIGH_LEVERAGE" else {
            status <- "REFERENCE_CALCULATED"
            d[[paste0("yield_", model, "_reference_log_loo")]][rows] <- fit$fitted_loo
            d[[paste0("yield_", model, "_reference_kg_ha_loo")]][rows] <- exp(fit$fitted_loo)
            d[[paste0("yield_", model, "_departure_log_loo")]][rows] <- fit$residual_loo
            d[[paste0("yield_", model, "_departure_pct_loo")]][rows] <- fit$departure_pct
            d[[paste0("yield_", model, "_eligible")]][rows] <- TRUE
          }
        }
        series_parts[[length(series_parts) + 1L]] <- data.frame(
          municipality_code = d$municipality_code[ix[1L]], municipality = as.character(d$municipality[ix[1L]]),
          state = as.character(d$state[ix[1L]]), species = d$species[ix[1L]], reference_model = model,
          observed_reporting_years = length(ix), positive_productivity_years = n,
          odd_positive_years = nodd, even_positive_years = neven,
          first_positive_year = if (n) min(d$year[rows]) else NA_integer_,
          last_positive_year = if (n) max(d$year[rows]) else NA_integer_, status = status,
          slope_log_per_year_descriptive = if (!is.null(fit)) fit$coefficients[2L] else NA_real_,
          odd_year_log_contrast_descriptive = if (!is.null(fit) && par) fit$coefficients[3L] else NA_real_,
          rmse_log_loo_retrospective = if (!is.null(fit)) fit$rmse_log_loo else NA_real_,
          maximum_leverage = if (!is.null(fit)) max(fit$leverage) else NA_real_,
          actual_biennial_phase_verified = FALSE, climate_effect_estimated = FALSE, stringsAsFactors = FALSE)
      }
      if (counter %% 400L == 0L) message("  Series ", counter, " / ", nseries)
    }
    series <- bind(series_parts)
    d$ready_for_exploratory_annual_panel <- d$yield_trend_eligible &
      d$primary_climate_complete_lag0 & d$primary_climate_complete_lag1 & d$primary_climate_complete_lag2
    d$harvest_calendar_alignment_required <- TRUE
    d$climate_attribution_performed <- FALSE
    d$historical_boundaries_harmonized <- FALSE
    # Store values only. No municipality/series is removed from this output.
    write_csv(series, file.path(out_dir, "coffee_yield_reference_diagnostics.csv"))

    coverage <- list()
    for (sp in species_order) for (yr in study_years) {
      q <- d[d$species == sp & d$year == yr, , drop = FALSE]
      a <- q$yield_trend_eligible
      b <- q$yield_trend_parity_eligible
      c <- q$ready_for_exploratory_annual_panel
      coverage[[length(coverage) + 1L]] <- data.frame(species = sp, year = yr,
        retained_municipal_rows = nrow(q), positive_area_rows = sum(q$has_harvested_area),
        positive_yield_rows = sum(q$positive_yield_for_log),
        zero_production_positive_area_rows = sum(q$zero_production_positive_area),
        undefined_yield_rows = sum(!is.finite(q$analysis_yield_kg_ha)),
        trend_reference_rows = sum(a), parity_reference_rows = sum(b), exploratory_ready_rows = sum(c),
        total_target_production_t = sum(q$production_t), total_target_area_ha = sum(q$area_harvested_ha),
        trend_production_coverage_pct = safe_ratio_pct(sum(q$production_t[a]), sum(q$production_t)),
        trend_area_coverage_pct = safe_ratio_pct(sum(q$area_harvested_ha[a]), sum(q$area_harvested_ha)),
        parity_production_coverage_pct = safe_ratio_pct(sum(q$production_t[b]), sum(q$production_t)),
        parity_area_coverage_pct = safe_ratio_pct(sum(q$area_harvested_ha[b]), sum(q$area_harvested_ha)),
        primary_climate_lag0_missing_rows = sum(!q$primary_climate_complete_lag0),
        primary_climate_lag1_missing_rows = sum(!q$primary_climate_complete_lag1),
        primary_climate_lag2_missing_rows = sum(!q$primary_climate_complete_lag2),
        positive_yield_MET_cells = length(unique(q$MET_cell_id[q$positive_yield_for_log])),
        retained_MET_cells = length(unique(q$MET_cell_id)), stringsAsFactors = FALSE)
    }
    write_csv(bind(coverage), file.path(out_dir, "coffee_yield_climate_panel_coverage.csv"))
    # Explicit timing dictionary rather than assuming every municipality has
    # the same flowering, filling and harvest dates.
    timing <- data.frame(lag_years = climate_lags,
      window = c("January to December of PAM year t", "January to December of t-1", "January to December of t-2"),
      includes_possible_postharvest_weather = c(TRUE, FALSE, FALSE),
      phenological_stage_assigned = FALSE,
      intended_use = c("Retrospective diagnostic; NOT automatically a preharvest predictor",
                       "Candidate antecedent exposure; regional calendar still required",
                       "Candidate earlier exposure; regional calendar still required"))
    write_csv(timing, file.path(out_dir, "coffee_yield_climate_timing_dictionary.csv"))
    dict <- data.frame(metric = metrics, unit = metric_units,
      complete_1981_2010_anomaly_exported = metrics %in% met_metrics,
      primary_complete_meteorological_indicator = metrics %in% primary_metrics,
      interpretation = c(rep("Local annual index; lag is a calendar-year offset", length(met_metrics)),
                         "FAO56 reference demand, not actual coffee ET", "Climatic P minus reference ET0, not root-zone storage",
                         "Retain SRB/CERES source epochs"))
    write_csv(dict, file.path(out_dir, "coffee_yield_climate_metric_dictionary.csv"))
    notes <- c(
      "COFFE-STUDY | STEP19 | MUNICIPAL PRODUCTIVITY AND CLIMATE PANEL",
      paste("Version:", version), paste("Executed:", format(Sys.time(), usetz = TRUE)),
      "Original audited measurement columns are preserved. analysis_yield_kg_ha is a separate P/A ratio.",
      "Study years: 2012-2024. Separate Arabica/Canephora records; Total is never added or used to backfill species.",
      "All climate targets and zero-area records are retained. Positive log-response eligibility is explicit.",
      "The inherited complete municipal-code panel does not ensure constant historical boundaries.",
      "Climate joins use exact code/year keys at t,t-1,t-2. A missing year is not replaced with an adjacent row.",
      "Calendar-year t may include postharvest weather. No observed harvest/flowering dates are available in these inputs.",
      "Antecedent annual windows are candidate exposures, not verified phenological phases or a fitted lag response.",
      "All climate observations and existing VPD/ET0 missing-value rules remain unchanged.",
      "Local meteorological references require 30 complete annual indices from 1981-2010. No partial reference substitutes them.",
      "Solar/ET0/P-ET0 anomalies across SRB/CERES are not calculated. Both analysis and lag years here are within 2001-2024.",
      "MET/SRB/CERES IDs are retained. Shared weather cells are not independent meteorological observations.",
      "Reference A: log(productivity) ~ intercept + year-2018. Reference B adds odd/even calendar year as a sensitivity.",
      "Threshold: at least 10 positive-yield years. Parity sensitivity additionally requires at least four positive years in each parity.",
      "These fixed study choices control reference support, not source-data validity. Short series remain in the panel with missing reference outputs.",
      "For each year, its own response is omitted by the exact OLS leave-one-out leverage identity, tested against explicit refits.",
      "Remaining training years can be later than the omitted year: retrospective diagnostic, not real-time forecasting.",
      "Percent departure = 100*expm1(log(observed productivity)-LOO fitted log productivity).",
      "Exponentiated fitted log productivity is a geometric-scale reference, not an unbiased arithmetic expectation or potential yield.",
      "Odd/even contrast is fitted separately for every municipality/species; actual bearing phase is not observed or validated.",
      "Neither trend nor parity reference is chosen using climate significance. Both are retained without p-values.",
      "Departures can contain management, genotypes, diseases, weather, reporting error, area composition and boundary changes.",
      "No negative departure is labelled a climate-attributable loss. No percent threshold defines confirmed crop damage.",
      "Short/zero-yield series excluded from log references may differ systematically. Coverage reports quantify this selection.",
      "No yield-climate regression, causality, adaptation efficacy, future projection or new main figure is estimated.",
      "Earlier datasets and figures are unchanged. No downloads or raw-daily recalculation occur.",
      "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html",
      "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.influence.html")
    writeLines(notes, file.path(out_dir, "coffee_yield_climate_panel_method_notes.txt"))
    panel_rds <- file.path(panel_dir, "coffee_yield_climate_panel_2012_2024.rds")
    panel_csv <- file.path(panel_dir, "coffee_yield_climate_panel_2012_2024.csv")
    write_rds(d, panel_rds)
    write_csv(d, panel_csv)
    assert(all(vapply(paths, hash, character(1)) == prov$md5), "An input changed during processing.")
    report$panel_rows <<- nrow(d)
    report$positive_productivity_rows <<- sum(d$positive_yield_for_log)
    report$positive_area_zero_production_rows <<- sum(d$zero_production_positive_area)
    report$no_harvested_area_rows <<- sum(d$no_harvested_area)
    report$series_with_trend_reference <<- sum(series$reference_model == "trend" & series$status == "REFERENCE_CALCULATED")
    report$series_with_parity_reference <<- sum(series$reference_model == "trend_parity" & series$status == "REFERENCE_CALCULATED")
    report$execution_status <<- "COMPLETED_YIELD_CLIMATE_PANEL"
    save_summary()
    message("\n", report$execution_status, "\nPanel: ", panel_rds,
      "\nRows retained: ", nrow(d), "\nReports: ", out_dir,
      "\nNo climate response or causal loss has been estimated.")
    invisible(NULL)
  }
  tryCatch(worker(), error = function(e) {
    report$error_if_incomplete <<- conditionMessage(e)
    try(save_summary(), silent = TRUE)
    stop(conditionMessage(e), "\nStep19 summary: ", file.path(out_dir, "coffee_yield_climate_panel_summary.csv"), call. = FALSE)
  })
}

coffee_step19()
