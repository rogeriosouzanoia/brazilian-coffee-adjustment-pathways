# ======================================================================
# COFFE-STUDY | STEP 20 | EXPLORATORY CLIMATE / PRODUCTIVITY ASSOCIATIONS
# Run the COMPLETE file with Source in RStudio.
# Reuses the completed step19 panel. No climate or PAM downloads.
# Prior files are never overwritten. This is NOT climate attribution.
#
# Response: observed log(productivity), not the step19 LOO departures.
# Municipality intercepts, municipality-specific linear time trends and
# year effects are fitted JOINTLY with the climate regressors. The same
# nuisance adjustment therefore applies to the response and predictors.
#
# Four fixed specifications per species, with the SAME eligible records:
# A: heat, rain, dry sequence in t-1 and t-2; unit trends + year effects.
# B: A + municipality-specific odd/even calendar-year contrast.
# C: B with mean temperature replacing hot-day count (not added to it).
# D: B + calendar-year t; retrospective/postharvest-contaminated diagnostic.
# t is the PAM reporting year. None of these windows is a verified crop phase.
#
# Equal weight per included municipality/year record; no production weights.
# Intervals: exploratory CR1 two-way cluster covariance (MET cell and year),
# with t degrees of freedom = min(number of cells, number of years)-1.
# Cell-only intervals are exported separately, never substituted silently.
# Non-PSD two-way covariance is flagged and its intervals are withheld.
# No stars, significance-based model/lag selection or missing-value imputation.
#
# Requires fixest >= 0.13.0 (installed from CRAN only when needed).
# Optional plots use ggplot2, with the user's theme_rogerio style.
# COFFEE_STEP20_TEST_ONLY=TRUE runs the numerical tests and stops.
# COFFEE_STEP20_SKIP_PLOT=TRUE avoids plotting without altering estimation.
# COFFEE_STEP20_SKIP_YEAR_SENSITIVITY=TRUE skips the year-omission checks.
#
# Implementation documentation checked 2026-09-20:
# https://lrberge.github.io/fixest/reference/feols.html
# https://lrberge.github.io/fixest/reference/vcov_cluster.html
# https://lrberge.github.io/fixest/reference/ssc.html
# https://lrberge.github.io/fixest/articles/standard_errors.html
# https://sandwich.r-forge.r-project.org/reference/vcovCL.html
# ======================================================================

coffee_step20 <- function() {
  version <- "20_exploratory_annual_associations_v1"
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  out_dir <- file.path(base_dir, "04_outputs", "climate_yield_associations")
  fig_dir <- file.path(base_dir, "05_figures", "Figure04_association_diagnostic")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out_dir) || !dir.exists(fig_dir)) stop("Cannot create output directories.", call. = FALSE)
  old <- options(scipen = 999)
  on.exit(options(old), add = TRUE)
  lock <- file.path(out_dir, ".step20_running")
  if (!dir.create(lock, showWarnings = FALSE)) stop(
    "A step20 run may be active. Confirm it has stopped before removing:\n", lock, call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  writeLines(paste("PID", Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))

  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(x, cols, label) {
    absent <- setdiff(cols, names(x))
    assert(!length(absent), paste(label, "is missing:", paste(absent, collapse = ", ")))
  }
  key <- function(x) paste(x$municipality_code, x$species, x$year, sep = "|")
  hash <- function(path) unname(tools::md5sum(path))
  bind <- function(x) {
    if (!length(x)) return(data.frame())
    ans <- do.call(rbind, x); rownames(ans) <- NULL; ans
  }
  read_csv <- function(path) {
    assert(file.exists(path), paste("Required input not found:", path))
    utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE,
      check.names = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  number <- function(x, label, allow_na = TRUE) {
    txt <- as.character(x); ans <- suppressWarnings(as.numeric(txt))
    assert(!any(!is.na(txt) & is.na(ans)) && all(is.finite(ans) | is.na(ans)) &&
      (allow_na || !anyNA(ans)), paste("Invalid numeric field:", label))
    ans
  }
  logical_col <- function(x, label) {
    assert(!anyNA(x) && all(as.character(x) %in% c("TRUE", "FALSE")), paste("Invalid logical field:", label))
    as.character(x) == "TRUE"
  }
  checked <- function(x, nm) {
    need(x, c("check", "result"), "Summary")
    ans <- x$result[x$check == nm]
    assert(length(ans) == 1L && !is.na(ans), paste("Missing or ambiguous check:", nm))
    ans
  }
  write_csv <- function(x, path) {
    tmp <- tempfile("coffee20_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  write_rds <- function(x, path) {
    tmp <- tempfile("coffee20_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  report <- list(execution_status = "INCOMPLETE", script_version = version,
    input_rows = 0L, eligible_rows = 0L, eligible_series = 0L,
    main_fits_expected = 8L, main_fits_completed = 0L,
    non_psd_two_way_covariances = 0L, numerical_tests_passed = FALSE,
    year_omission_refits_completed = 0L, year_omission_refits_failed = 0L,
    estimate_type = "Exploratory conditional annual association",
    model_selection_by_significance = FALSE, weights = "Equal included municipality-year records",
    response_is_LOO_departure = FALSE, controls_jointly_fitted = TRUE,
    current_year_primary_predictor = FALSE, phenological_alignment_verified = FALSE,
    real_time_prediction = FALSE, causal_attribution_performed = FALSE,
    climate_data_downloaded = FALSE, missing_data_imputed = FALSE,
    previous_inputs_modified = FALSE, png_status = "NOT_ATTEMPTED", svg_status = "NOT_ATTEMPTED",
    fixest_version = NA_character_, error_if_incomplete = "None")
  warnings_log <- coefficients <- diagnostics <- coverage_parts <- omitted <- compact_fits <- list()
  save_progress <- function() {
    write_csv(data.frame(check = names(report), result = vapply(report, as.character, character(1))),
      file.path(out_dir, "coffee_climate_association_summary.csv"))
    for (entry in list(c("coefficients", "coffee_climate_association_coefficients.csv"),
                       c("diagnostics", "coffee_climate_association_model_diagnostics.csv"),
                       c("coverage_parts", "coffee_climate_association_sample_coverage.csv"),
                       c("warnings_log", "coffee_climate_association_warnings.csv"),
                       c("omitted", "coffee_climate_association_year_omission.csv"))) {
      obj <- get(entry[1], inherits = TRUE)
      if (length(obj)) write_csv(bind(obj), file.path(out_dir, entry[2]))
    }
  }
  on.exit({
    try(save_progress(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step20.txt")), silent = TRUE)
  }, add = TRUE)
  record_warning <- function(label, msg) {
    warnings_log[[length(warnings_log) + 1L]] <<- data.frame(context = label, message = msg)
  }
  capture_warnings <- function(expr, label) withCallingHandlers(expr, warning = function(w) {
    record_warning(label, conditionMessage(w)); invokeRestart("muffleWarning")
  })

  worker <- function() {
    pkg_ok <- requireNamespace("fixest", quietly = TRUE)
    if (!pkg_ok || utils::packageVersion("fixest") < "0.13.0") {
      message("Installing the required fixest package from CRAN...")
      utils::install.packages("fixest", repos = "https://cloud.r-project.org")
    }
    assert(requireNamespace("fixest", quietly = TRUE) && utils::packageVersion("fixest") >= "0.13.0",
      "fixest >= 0.13.0 is required. Update it, restart R, and run this file again.")
    assert("vcov_fix" %in% names(formals(fixest::vcov_cluster)),
      "The installed fixest::vcov_cluster does not expose vcov_fix. Update fixest and restart R.")
    report$fixest_version <<- as.character(utils::packageVersion("fixest"))

    # ------------------------------------------------------------
    # Numerical tests: joint adjustment, scale and cluster covariance.
    # They run before the study data are read.
    # ------------------------------------------------------------
    tests <- list()
    add_test <- function(label, value, tolerance) {
      tests[[length(tests) + 1L]] <<- data.frame(test = label, absolute_error = value,
        tolerance = tolerance, pass = is.finite(value) && value <= tolerance)
    }
    td <- expand.grid(id = seq_len(12), yr = 2012:2024)
    td$tc <- td$yr - 2018; td$odd <- td$yr %% 2L
    td$x1 <- sin(td$id * 1.3 + td$yr * 0.71)
    td$x2 <- cos(td$id * 0.8 - td$yr * 0.43)
    td$y <- 2 + td$id * 0.12 + td$id * 0.004 * td$tc +
      td$id * 0.012 * td$odd + sin(td$yr) + 0.3 * td$x1 - 0.2 * td$x2 +
      0.13 * cos(td$id * td$yr)
    for (par in c(FALSE, TRUE)) {
      f <- stats::as.formula(paste0("y ~ x1 + x2 | id[tc", if (par) ", odd" else "", "] + yr"))
      fast <- fixest::feols(f, td, nthreads = 1, fixef.tol = 1e-9,
        fixef.iter = 20000, notes = FALSE, demeaned = TRUE)
      plain_f <- stats::as.formula(paste0("y ~ x1 + x2 + factor(id) + factor(yr) + factor(id):tc",
        if (par) " + factor(id):odd" else ""))
      plain <- stats::lm(plain_f, td)
      add_test(paste("Joint FE coefficients vs explicit dummy regression; parity", par),
        max(abs(stats::coef(fast)[c("x1", "x2")] - stats::coef(plain)[c("x1", "x2")])), 1e-6)
      add_test(paste("Joint FE residuals vs explicit dummy regression; parity", par),
        max(abs(stats::residuals(fast) - stats::residuals(plain))), 1e-6)
      manual_fwl <- stats::lm.fit(fast$X_demeaned, fast$y_demeaned)$coefficients
      add_test(paste("Both response and predictors residualized; parity", par),
        max(abs(as.numeric(stats::coef(fast)) - as.numeric(manual_fwl))), 1e-7)
    }
    simple <- fixest::feols(y ~ x1 + x2, td, notes = FALSE)
    X <- stats::model.matrix(~x1 + x2, td)
    e <- stats::residuals(simple); bread <- solve(crossprod(X)); scores <- X * as.numeric(e)
    cluster_v <- function(g) bread %*% crossprod(rowsum(scores, as.character(g))) %*% bread
    direct_v <- cluster_v(td$id) + cluster_v(td$yr) - cluster_v(paste(td$id, td$yr, sep = "|"))
    raw_v <- capture_warnings(fixest::vcov_cluster(simple, cluster = list(td$id, td$yr),
      ssc = fixest::ssc(K.adj = FALSE, G.adj = FALSE), vcov_fix = FALSE), "Numerical cluster test")
    add_test("Two-way cluster covariance vs direct sandwich without corrections", max(abs(raw_v - direct_v)), 1e-8)
    add_test("Ten percent multiplicative association", abs(100 * expm1(log(1.1)) - 10), 1e-10)
    write_csv(bind(tests), file.path(out_dir, "coffee_climate_association_numerical_tests.csv"))
    assert(all(bind(tests)$pass), "A numerical test failed. No study model has been fitted.")
    report$numerical_tests_passed <<- TRUE
    if (toupper(Sys.getenv("COFFEE_STEP20_TEST_ONLY", "FALSE")) == "TRUE") {
      report$execution_status <<- "NUMERICAL_TESTS_ONLY"; save_progress()
      message("Step20 numerical tests passed. No study data read."); return(invisible(NULL))
    }

    paths <- c(panel = file.path(base_dir, "02_data_processed", "climate_yield_panel", "coffee_yield_climate_panel_2012_2024.rds"),
      summary = file.path(base_dir, "04_outputs", "climate_yield_panel", "coffee_yield_climate_panel_summary.csv"),
      coverage = file.path(base_dir, "04_outputs", "climate_yield_panel", "coffee_yield_climate_panel_coverage.csv"),
      references = file.path(base_dir, "04_outputs", "climate_yield_panel", "coffee_yield_reference_diagnostics.csv"),
      provenance = file.path(base_dir, "04_outputs", "climate_yield_panel", "coffee_yield_climate_input_provenance.csv"))
    assert(all(file.exists(paths)), paste("Missing step19 input(s):", paste(paths[!file.exists(paths)], collapse = "\n")))
    prov <- data.frame(role = names(paths), file = unname(paths), md5 = vapply(paths, hash, character(1)))
    write_csv(prov, file.path(out_dir, "coffee_climate_association_input_provenance.csv"))
    s <- read_csv(paths[["summary"]]); prior_cov <- read_csv(paths[["coverage"]])
    assert(checked(s, "execution_status") == "COMPLETED_YIELD_CLIMATE_PANEL", "Step19 is not complete.")
    assert(checked(s, "numerical_tests_passed") == "TRUE", "Step19 numerical tests did not pass.")
    d <- as.data.frame(readRDS(paths[["panel"]]), stringsAsFactors = FALSE)
    required <- c("municipality_code", "municipality", "species", "year", "state", "MET_cell_id",
      "production_t", "area_harvested_ha", "analysis_yield_kg_ha", "log_yield", "climate_source",
      "yield_trend_eligible", "yield_trend_parity_eligible", "positive_yield_for_log")
    metrics <- c("hot_days_ge32", "tmean_c", "precipitation_total_mm", "longest_dry_spell_days")
    climate_cols <- unlist(lapply(0:2, function(lag) paste0("clim_", metrics, "_lag", lag)), use.names = FALSE)
    need(d, c(required, climate_cols, paste0("climate_year_lag", 0:2)), "Step19 panel")
    d$municipality_code <- as.character(d$municipality_code); d$MET_cell_id <- as.character(d$MET_cell_id)
    d$species <- as.character(d$species); d$year <- number(d$year, "year", FALSE)
    assert(!anyDuplicated(key(d)) && all(d$species %in% c("Arabica", "Canephora")), "Duplicate keys or unexpected species.")
    assert(identical(sort(unique(d$year)), as.numeric(2012:2024)), "Unexpected reporting years.")
    assert(all(d$climate_source == "NASA POWER"), "Unexpected climate source.")
    assert(nrow(d) == as.integer(checked(s, "panel_rows")), "Panel count differs from the step19 report.")
    for (col in c("production_t", "area_harvested_ha", "analysis_yield_kg_ha", "log_yield", climate_cols))
      d[[col]] <- number(d[[col]], col)
    for (lag in 0:2) assert(all(number(d[[paste0("climate_year_lag", lag)]], "climate year", FALSE) == d$year - lag),
      paste("Incorrect exact calendar join for lag", lag))
    eligible <- logical_col(d$yield_trend_eligible, "trend eligibility") &
      logical_col(d$yield_trend_parity_eligible, "parity eligibility") &
      logical_col(d$positive_yield_for_log, "positive productivity") &
      is.finite(d$log_yield) & stats::complete.cases(d[climate_cols])
    assert(all(is.finite(d$analysis_yield_kg_ha[eligible]) & d$analysis_yield_kg_ha[eligible] > 0), "Nonpositive eligible productivity.")
    assert(max(abs(d$log_yield[eligible] - log(1000 * d$production_t[eligible] / d$area_harvested_ha[eligible]))) < 1e-9,
      "Log response differs from the production/area ratio.")
    report$input_rows <<- nrow(d); report$eligible_rows <<- sum(eligible)
    report$eligible_series <<- length(unique(paste(d$municipality_code[eligible], d$species[eligible])))
    sample_index <- d[c("municipality_code", "species", "year", "MET_cell_id")]
    sample_index$included <- eligible
    sample_index$reason <- ifelse(eligible, "Common sample for all four specifications", "Does not meet the inherited positive-yield/reference/climate criteria")
    write_csv(sample_index, file.path(out_dir, "coffee_climate_association_sample_membership.csv"))
    for (sp in c("Arabica", "Canephora")) for (yr in 2012:2024) {
      ix <- d$species == sp & d$year == yr; use <- ix & eligible
      oldrow <- prior_cov[prior_cov$species == sp & as.integer(prior_cov$year) == yr, ]
      assert(nrow(oldrow) == 1L && sum(use) == as.integer(oldrow$exploratory_ready_rows),
        paste("Model sample differs from the step19 eligible count:", sp, yr))
      coverage_parts[[length(coverage_parts) + 1L]] <<- data.frame(species = sp, year = yr,
        eligible_records = sum(use), municipalities = length(unique(d$municipality_code[use])),
        MET_cells = length(unique(d$MET_cell_id[use])),
        production_coverage_pct = 100 * sum(d$production_t[use]) / sum(d$production_t[ix]),
        area_coverage_pct = 100 * sum(d$area_harvested_ha[use]) / sum(d$area_harvested_ha[ix]))
    }
    # A single eligibility set is used for ALL models, not outcome-selected per lag.
    zall <- d[eligible, , drop = FALSE]
    zall$year_centered <- zall$year - 2018; zall$odd_year <- zall$year %% 2L
    for (lag in 0:2) {
      zall[[paste0("heat_l", lag)]] <- zall[[paste0("clim_hot_days_ge32_lag", lag)]] / 10
      zall[[paste0("temperature_l", lag)]] <- zall[[paste0("clim_tmean_c_lag", lag)]]
      zall[[paste0("rain_l", lag)]] <- zall[[paste0("clim_precipitation_total_mm_lag", lag)]] / 100
      zall[[paste0("dry_l", lag)]] <- zall[[paste0("clim_longest_dry_spell_days_lag", lag)]] / 10
    }
    # Predefined models. No data-driven variable selection is performed.
    specs <- data.frame(model_id = c("A_antecedent_trend", "B_antecedent_parity", "C_temperature_parity", "D_current_year_diagnostic"),
      thermal = c("heat", "heat", "temperature", "heat"),
      parity = c(FALSE, TRUE, TRUE, TRUE), include_current_year = c(FALSE, FALSE, FALSE, TRUE),
      role = c("Primary exploratory association", "Parity sensitivity", "Alternative thermal indicator sensitivity", "Retrospective diagnostic with possible postharvest weather"))
    get_terms <- function(sp) {
      lags <- if (sp$include_current_year) 0:2 else 1:2
      unlist(lapply(lags, function(lag) paste0(c(sp$thermal, "rain", "dry"), "_l", lag)), use.names = FALSE)
    }
    get_formula <- function(sp) stats::as.formula(paste0("log_yield ~ ", paste(get_terms(sp), collapse = " + "),
      " | municipality_code[year_centered", if (sp$parity) ", odd_year" else "", "] + year"))
    specs$formula <- vapply(seq_len(nrow(specs)), function(i) paste(deparse(get_formula(specs[i, ])), collapse = " "), character(1))
    write_csv(specs, file.path(out_dir, "coffee_climate_association_specifications.csv"))
    ssc <- fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, G.df = "min", t.df = "min")
    fit_one <- function(z, sp, label) {
      fit <- capture_warnings(fixest::feols(get_formula(sp), data = z, nthreads = 1,
        fixef.tol = 1e-8, fixef.iter = 20000, collin.tol = 1e-10,
        fixef.rm = "none", notes = FALSE, demeaned = TRUE, data.save = TRUE, vcov = "iid"), label)
      assert(stats::nobs(fit) == nrow(z), paste(label, "unexpected automatic row removal."))
      assert(setequal(names(stats::coef(fit)), get_terms(sp)), paste(label, "climate term dropped by collinearity; review required."))
      assert(all(is.finite(stats::coef(fit))), paste(label, "nonfinite coefficients."))
      fwl <- stats::lm.fit(fit$X_demeaned, fit$y_demeaned)$coefficients
      assert(max(abs(as.numeric(fwl) - as.numeric(stats::coef(fit)))) < 1e-6, paste(label, "FWL coefficient check failed."))
      fit
    }
    make_rows <- function(fit, V, covariance, z, sp) {
      b <- stats::coef(fit); V <- V[names(b), names(b), drop = FALSE]
      ev <- eigen(V, symmetric = TRUE, only.values = TRUE)$values
      psd <- min(ev) >= -1e-10 * max(1, max(abs(ev)))
      df_t <- if (covariance == "MET_cell_and_year") min(length(unique(z$MET_cell_id)), length(unique(z$year))) - 1L else length(unique(z$MET_cell_id)) - 1L
      quant <- stats::qt(0.975, df = df_t)
      lags <- if (sp$include_current_year) 0:2 else 1:2
      contrasts <- lapply(names(b), function(term) {a <- rep(0, length(b)); names(a) <- names(b); a[term] <- 1; a})
      names(contrasts) <- names(b)
      for (v in c(sp$thermal, "rain", "dry")) {
        a <- rep(0, length(b)); names(a) <- names(b); a[paste0(v, "_l", 1:2)] <- 1
        contrasts[[paste0(v, "_joint_l1_l2")]] <- a
      }
      bind(lapply(names(contrasts), function(term) {
        a <- contrasts[[term]]; est <- sum(a * b); variance <- as.numeric(t(a) %*% V %*% a)
        valid_ci <- psd && is.finite(variance) && variance > 0 && df_t > 1
        se <- if (valid_ci) sqrt(variance) else NA_real_
        variable <- sub("_.*$", "", term)
        increment <- switch(variable, heat = "10 additional Tmax>=32 C days", temperature = "1 C higher mean temperature",
          rain = "100 mm additional annual precipitation", dry = "10 additional days in local longest dry sequence")
        data.frame(species = z$species[1], model_id = sp$model_id, role = sp$role, term = term,
          predictor = variable, increment = increment,
          timing = if (grepl("joint", term)) "Same increment in BOTH t-1 and t-2" else paste0("Calendar year t-", sub(".*_l", "", term)),
          beta_log = est, se_log = se, association_pct = 100 * expm1(est),
          lower95_pct = if (valid_ci) 100 * expm1(est - quant * se) else NA_real_,
          upper95_pct = if (valid_ci) 100 * expm1(est + quant * se) else NA_real_,
          covariance = covariance, covariance_psd = psd, ci_status = if (valid_ci) "EXPLORATORY_T_INTERVAL" else "WITHHELD_INVALID_COVARIANCE_OR_VARIANCE",
          df_t = df_t, n_obs = nrow(z), municipalities = length(unique(z$municipality_code)), MET_cells = length(unique(z$MET_cell_id)),
          year_clusters = length(unique(z$year)), parity_control = sp$parity,
          includes_possible_postharvest_weather = sp$include_current_year, causal_effect = FALSE)
      }))
    }

    for (species in c("Arabica", "Canephora")) {
      z <- zall[zall$species == species, , drop = FALSE]
      assert(length(unique(z$year)) == 13L, "Unexpected year support.")
      assert(all(vapply(split(z$MET_cell_id, z$municipality_code), function(x) length(unique(x)) == 1L, logical(1))),
        "A municipality has more than one MET cell.")
      for (i in seq_len(nrow(specs))) {
        sp <- specs[i, ]; label <- paste(species, sp$model_id, sep = " | ")
        message("Fitting: ", label, " | records: ", nrow(z))
        fit <- fit_one(z, sp, label)
        V2 <- capture_warnings(fixest::vcov_cluster(fit, cluster = list(MET_cell = z$MET_cell_id, year = z$year),
          ssc = ssc, vcov_fix = FALSE), paste(label, "two-way covariance"))
        V1 <- capture_warnings(fixest::vcov_cluster(fit, cluster = z$MET_cell_id,
          ssc = ssc, vcov_fix = FALSE), paste(label, "cell-only covariance"))
        rows2 <- make_rows(fit, V2, "MET_cell_and_year", z, sp)
        rows1 <- make_rows(fit, V1, "MET_cell_only_sensitivity", z, sp)
        coefficients[[length(coefficients) + 1L]] <<- rbind(rows2, rows1)
        if (!all(rows2$covariance_psd)) report$non_psd_two_way_covariances <<- report$non_psd_two_way_covariances + 1L
        xd <- fit$X_demeaned
        corx <- stats::cor(xd); eig <- eigen(corx, symmetric = TRUE, only.values = TRUE)$values
        conditioning <- if (min(eig) > 0) sqrt(max(eig) / min(eig)) else Inf
        partial_r2 <- 1 - sum(stats::residuals(fit)^2) / sum(fit$y_demeaned^2)
        diagnostics[[length(diagnostics) + 1L]] <<- data.frame(species = species, model_id = sp$model_id,
          n_obs = nrow(z), municipalities = length(unique(z$municipality_code)), MET_cells = length(unique(z$MET_cell_id)), year_clusters = 13,
          incremental_in_sample_R2 = partial_r2, residual_RMSE_log = sqrt(mean(stats::residuals(fit)^2)),
          residualized_predictor_condition_index = conditioning,
          maximum_absolute_predictor_correlation = max(abs(corx[upper.tri(corx)])),
          two_way_covariance_psd = all(rows2$covariance_psd),
          full_coefficient_rank = length(stats::coef(fit)), causal_interpretation = FALSE,
          prediction_validation_performed = FALSE)
        cp <- as.data.frame(as.table(corx), stringsAsFactors = FALSE)
        names(cp) <- c("term1", "term2", "correlation_after_nuisance_adjustment")
        write_csv(cp, file.path(out_dir, paste0("correlations_", species, "_", sp$model_id, ".csv")))
        compact_fits[[paste(species, sp$model_id, sep = "_")]] <- list(formula = sp$formula, coefficient = stats::coef(fit),
          covariance_cell_year = V2, covariance_cell_only = V1, input_panel_md5 = prov$md5[prov$role == "panel"],
          observation_keys = key(z), fitted_log = as.numeric(stats::fitted(fit)), residual_log = as.numeric(stats::residuals(fit)),
          inference = "Exploratory; 13 year clusters; no covariance repair; no causal claim")
        report$main_fits_completed <<- report$main_fits_completed + 1L
        save_progress()
        if (i <= 2L && toupper(Sys.getenv("COFFEE_STEP20_SKIP_YEAR_SENSITIVITY", "FALSE")) != "TRUE") {
          for (yr in sort(unique(z$year))) {
            message("  Coefficient stability: omit reporting year ", yr)
            zz <- z[z$year != yr, , drop = FALSE]
            small <- tryCatch(fit_one(zz, sp, paste(label, "omit", yr)), error = function(e) e)
            if (inherits(small, "error")) {
              record_warning(paste(label, "omit", yr), conditionMessage(small))
              report$year_omission_refits_failed <<- report$year_omission_refits_failed + 1L
              omitted[[length(omitted) + 1L]] <<- data.frame(species = species, model_id = sp$model_id,
                omitted_year = yr, term = get_terms(sp), beta_log = NA_real_, association_pct = NA_real_,
                full_beta_log = as.numeric(stats::coef(fit)[get_terms(sp)]), status = "FAILED_REFIT")
            } else {
              bb <- stats::coef(small)
              omitted[[length(omitted) + 1L]] <<- data.frame(species = species, model_id = sp$model_id,
                omitted_year = yr, term = names(bb), beta_log = as.numeric(bb), association_pct = 100 * expm1(as.numeric(bb)),
                full_beta_log = as.numeric(stats::coef(fit)[names(bb)]), status = "REFITTED_NOT_FORECAST_VALIDATION")
              report$year_omission_refits_completed <<- report$year_omission_refits_completed + 1L
            }
          }
          save_progress()
        }
      }
    }
    write_rds(compact_fits, file.path(out_dir, "coffee_climate_association_compact_fits.rds"))
    if (length(omitted)) {
      oo <- bind(omitted)
      stability <- bind(lapply(split(oo, paste(oo$species, oo$model_id, oo$term, sep = "|")), function(q) {
        valid <- is.finite(q$beta_log); complete <- all(valid) && nrow(q) == 13L
        data.frame(species = q$species[1], model_id = q$model_id[1], term = q$term[1], successful_refits = sum(valid),
          full_beta_log = q$full_beta_log[1],
          minimum_omitted_year_beta_log = if (any(valid)) min(q$beta_log[valid]) else NA_real_,
          maximum_omitted_year_beta_log = if (any(valid)) max(q$beta_log[valid]) else NA_real_,
          sign_preserved_in_all_13_refits = if (complete) all(sign(q$beta_log) == sign(q$full_beta_log)) else NA,
          interpretation = "Sensitivity to omission of one reporting year; NOT predictive cross-validation")
      }))
      write_csv(stability, file.path(out_dir, "coffee_climate_association_stability_summary.csv"))
    }

    # Tables are the primary output. A plotting failure never discards them.
    if (toupper(Sys.getenv("COFFEE_STEP20_SKIP_PLOT", "FALSE")) != "TRUE") {
      tryCatch({
        if (!requireNamespace("ggplot2", quietly = TRUE)) utils::install.packages("ggplot2", repos = "https://cloud.r-project.org")
        assert(requireNamespace("ggplot2", quietly = TRUE) && utils::packageVersion("ggplot2") >= "3.5.0", "ggplot2 >= 3.5.0 is required for every-facet axes.")
        font <- "serif"
        if (requireNamespace("systemfonts", quietly = TRUE) && "Times New Roman" %in% systemfonts::system_fonts()$family) font <- "Times New Roman"
        report$font_used <<- font
        theme_rogerio <- function() ggplot2::theme_minimal(base_size = 14, base_family = font) +
          ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black", linewidth = 0.45),
            axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.4), axis.text = ggplot2::element_text(colour = "black"),
            axis.ticks.length = grid::unit(0.15, "cm"), legend.position = "top", legend.title = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(face = "bold"), panel.spacing = grid::unit(0.8, "lines"),
            plot.title = ggplot2::element_text(face = "bold"), plot.subtitle = ggplot2::element_text(size = 11))
        pp <- bind(coefficients)
        pp <- pp[pp$covariance == "MET_cell_and_year" & pp$model_id %in% specs$model_id[1:2] & !grepl("joint", pp$term), , drop = FALSE]
        terms <- c("heat_l1", "heat_l2", "rain_l1", "rain_l2", "dry_l1", "dry_l2")
        labels <- c("10 additional hot days | t-1", "10 additional hot days | t-2", "100 mm additional rain | t-1", "100 mm additional rain | t-2", "10 days longer dry sequence | t-1", "10 days longer dry sequence | t-2")
        pp$control <- factor(ifelse(pp$parity_control, "Trend plus parity", "Trend"), levels = c("Trend", "Trend plus parity"))
        pp$ypos <- 7 - match(pp$term, terms) + ifelse(pp$parity_control, -0.12, 0.12)
        pp$species <- factor(pp$species, levels = c("Arabica", "Canephora"))
        p <- ggplot2::ggplot(pp, ggplot2::aes(x = association_pct, y = ypos, shape = control)) +
          ggplot2::geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.4) +
          ggplot2::geom_segment(ggplot2::aes(x = lower95_pct, xend = upper95_pct, yend = ypos), linewidth = 0.6, na.rm = TRUE) +
          ggplot2::geom_point(size = 2.6) + ggplot2::scale_shape_manual(values = c(16, 15)) +
          ggplot2::scale_y_continuous(breaks = 6:1, labels = labels, limits = c(0.5, 6.5)) +
          ggplot2::facet_wrap(~species, nrow = 1, scales = "fixed", axes = "all", axis.labels = "all") +
          ggplot2::labs(title = "Antecedent climate and coffee productivity",
            subtitle = "Exploratory annual associations | 2012 to 2024 | Separate species estimates",
            x = "Conditional multiplicative association with productivity (%)", y = NULL,
            caption = paste("Hot days: Tmax >= 32 degrees C. Dry sequence: precipitation < 1 mm/day. Both antecedent years fitted jointly.",
              "Intervals: exploratory 95% t intervals clustered by MET cell and reporting year (13 years). Missing intervals are flagged in the tables.",
              "Municipality effects, local trends and year effects fitted jointly. Not climate-attributable losses or validated crop-stage responses.", sep = "\n")) + theme_rogerio()
        png_file <- file.path(fig_dir, "Figure04_antecedent_climate_associations_draft.png")
        if (requireNamespace("ragg", quietly = TRUE)) {
          ggplot2::ggsave(png_file, p, width = 14, height = 7.5, units = "in", dpi = 300, device = ragg::agg_png)
        } else ggplot2::ggsave(png_file, p, width = 14, height = 7.5, units = "in", dpi = 300)
        report$png_status <<- "Saved"
        svg_file <- file.path(fig_dir, "Figure04_antecedent_climate_associations_draft.svg")
        ggplot2::ggsave(svg_file, p, width = 14, height = 7.5, units = "in", device = grDevices::svg)
        report$svg_status <<- "Saved"
        write_csv(pp, file.path(out_dir, "coffee_climate_association_plot_data.csv"))
      }, error = function(e) {
        record_warning("Plot", conditionMessage(e)); report$plot_error <<- conditionMessage(e)
        message("Tables are preserved. Plot needs review: ", conditionMessage(e))
      })
    }
    notes <- c("COFFE-STUDY | STEP20 | EXPLORATORY ANNUAL ASSOCIATIONS", paste("Version:", version),
      paste("Executed:", format(Sys.time(), usetz = TRUE)), paste("fixest:", report$fixest_version),
      "Observed log productivity is fitted directly. Step19 LOO departures are retained upstream, not used as this regression response.",
      "The response AND the climate regressors are adjusted jointly for unit intercepts, unit linear trends and reporting-year effects.",
      "Both trend-only and added unit-specific odd/even sensitivity are kept. Calendar parity is not verified biennial bearing phase.",
      "The common sample inherits step19 requirements: >=10 positive-yield years and >=4 years in each parity. No new response-based trimming.",
      "Equal weight per included municipal-year record. Coefficients are not production-weighted national effects.",
      "A/B use heat, rainfall and longest dry sequence in t-1 and t-2 simultaneously. C replaces heat count by mean temperature.",
      "D also includes t; this is a postharvest-contaminated retrospective diagnostic, not a primary preharvest equation.",
      "All climate inputs keep the step19 exact calendar-year keys and original values. No phenological dates are inferred.",
      "Solar, VPD and ET0 are not used in this first fit, and their missing-value rules remain unchanged.",
      "Area-zero records remain in step19 but have no defined positive productivity and cannot enter this log response.",
      "Current-year area and production are not extra regression controls; they define the observed response and sample-coverage diagnostics.",
      "MET-cell clustering accommodates shared weather and serial dependence within that cell. Year clustering adds contemporaneous cross-cell dependence.",
      "This is not a complete model of correlation across nearby cells and different years.",
      "Only 13 reporting-year clusters: intervals are exploratory and require further small-cluster/spatial robustness before publication.",
      "SSC: K.adj=TRUE, K.fixef=nonnested, G.adj=TRUE, G.df=min, t.df=min. Cell-only covariance is a labelled sensitivity.",
      "vcov_fix=FALSE. Non-PSD two-way covariance is recorded and its intervals withheld rather than repaired or replaced by cell-only intervals.",
      "Reported percent association is 100*expm1(beta) for the stated predictor increment, conditional on the fitted terms.",
      "It is a multiplicative log-scale contrast, not an unbiased arithmetic mean yield change or a counterfactual loss.",
      "Joint t-1/t-2 contrasts shift each of BOTH antecedent years by the stated increment, holding the other regressors fixed.",
      "Year omission refits all nuisance controls and coefficients; it is coefficient sensitivity, not forecasting or out-of-sample validation.",
      "No p-value stars, significance-driven variable/lag selection, causal loss aggregation, future prediction or adaptation efficacy.",
      "Condition indices and correlations refer to the predictors AFTER the joint nuisance adjustment.",
      "Shared source cells are not independent municipality-level weather observations. Unit boundaries and field locations remain unharmonized.",
      "No previous source, figure, panel or diagnostic is overwritten. Outputs are under climate_yield_associations.",
      "https://lrberge.github.io/fixest/reference/feols.html", "https://lrberge.github.io/fixest/reference/vcov_cluster.html",
      "https://lrberge.github.io/fixest/reference/ssc.html", "https://lrberge.github.io/fixest/articles/standard_errors.html")
    writeLines(notes, file.path(out_dir, "coffee_climate_association_method_notes.txt"))
    assert(all(vapply(paths, hash, character(1)) == prov$md5), "An input file changed during this run.")
    report$execution_status <<- if (report$non_psd_two_way_covariances > 0L || report$year_omission_refits_failed > 0L)
      "COMPLETED_EXPLORATORY_ASSOCIATIONS_WITH_INFERENCE_FLAGS" else "COMPLETED_EXPLORATORY_ASSOCIATIONS"
    save_progress()
    message("\n", report$execution_status, "\nTables: ", out_dir, "\nPlot: ", fig_dir,
      "\nDo not interpret these annual coefficients as climate-attributable crop losses.")
    invisible(NULL)
  }
  tryCatch(worker(), error = function(e) {
    report$error_if_incomplete <<- conditionMessage(e)
    try(save_progress(), silent = TRUE)
    stop(conditionMessage(e), "\nCompleted outputs retained. Summary: ", file.path(out_dir, "coffee_climate_association_summary.csv"), call. = FALSE)
  }, interrupt = function(e) {
    report$execution_status <<- "INTERRUPTED"; report$error_if_incomplete <<- "User interruption; completed tables retained"
    try(save_progress(), silent = TRUE)
    message("Interrupted. Previous source data unchanged. Rerun this script to refit the analyses.")
  })
}

coffee_step20()
