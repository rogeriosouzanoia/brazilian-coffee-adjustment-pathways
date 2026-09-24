# ======================================================================
# COFFE-STUDY | STEP 25 | CHRONOLOGICAL OUTCOME HOLDOUT
# Run this COMPLETE file with Source in RStudio. Base R only.
#
# Existing step24 sample, central water scenario only; no scenario tuning.
# Tests: 2020, 2021, 2022, 2023, 2024. Training uses reporting years < test.
# N: local intercept/trend; H: N + seasonal heat; P: H + precipitation;
# W: H + (1-ETr/ETc)/0.10. Each is fitted with/without municipal parity.
#
# IMPORTANT DIFFERENCE FROM STEP24:
# Reporting-year fixed effects are NOT fitted: the held-out year's effect
# cannot be estimated from its unavailable outcomes. These are separate
# prediction equations, not a re-estimation of step24's inferential claims.
#
# This is retrospective outcome holdout on the already selected step24 pool.
# The pool was selected with 2012-2024 completeness. Weather and production
# are current historical vintages, not vintages available in each test year.
# W4 contains Jan-Mar of the test year, possibly overlapping harvest. Thus
# this is NOT a verified operational forecast or a full real-time backtest.
#
# At each origin: >=8 prior observations, >=3 odd and >=3 even prior years.
# These prior-only rules are shared by all four models and both controls.
# Fit all local trends/parity terms and global climate slopes on training
# data only. Held-out outcomes are used ONLY for scoring, never centering.
# No daily recalculation, downloads, imputation, new soil parameters, p-value
# selection, confidence intervals, or attribution of production losses.
#
# Documentation consulted 2026-09-21:
# https://otexts.com/fpp3/tscv.html
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/qr.html
# ======================================================================

coffee_step25 <- function() {
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  version <- "25_forward_holdout_central_v1"
  source_dir <- file.path(base_dir, "04_outputs", "seasonal_water_associations")
  out_dir <- file.path(base_dir, "04_outputs", "seasonal_water_temporal_validation")
  cache_dir <- file.path(out_dir, "fold_cache_v1")
  test_years <- 2020:2024
  min_past <- 8L
  min_parity <- 3L
  central <- "CAD100_Kc095"
  species_set <- c("Arabica", "Canephora")
  controls <- c("Local_trend", "Local_trend_parity")
  families <- c("N", "H", "P", "W")
  windows <- paste0("W", 1:4)
  test_only <- toupper(Sys.getenv("COFFEE_FORWARD_TEST_ONLY", "FALSE")) == "TRUE"
  for (p in c(out_dir, cache_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create: ", p, call. = FALSE)
  }
  old <- options(scipen = 999)
  on.exit(options(old), add = TRUE)
  lock <- file.path(out_dir, ".step25_running")
  if (!dir.create(lock, showWarnings = FALSE)) stop(
    "A step25 run may be active. Confirm it has stopped before removing:\n", lock, call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  writeLines(paste("PID", Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))

  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(d, cols, label) assert(all(cols %in% names(d)),
    paste(label, "missing:", paste(setdiff(cols, names(d)), collapse = ", ")))
  hash <- function(p) unname(tools::md5sum(p))
  number <- function(x, label) {
    y <- suppressWarnings(as.numeric(as.character(x)))
    assert(length(y) == length(x) && all(is.finite(y)), paste("Invalid numeric:", label))
    y
  }
  flag <- function(x, label) {
    y <- toupper(as.character(x))
    assert(!anyNA(y) && all(y %in% c("TRUE", "FALSE")), paste("Invalid logical:", label))
    y == "TRUE"
  }
  key <- function(x) paste(x$municipality_code, x$species, x$year, sep = "|")
  read_csv <- function(p) {
    assert(file.exists(p), paste("Required file not found:", p))
    utils::read.csv(p, colClasses = "character", stringsAsFactors = FALSE,
      check.names = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  write_csv <- function(d, p) {
    tmp <- tempfile("cv25_", tmpdir = dirname(p), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(d, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, p, overwrite = TRUE), paste("Cannot save:", p))
  }
  write_rds <- function(d, p) {
    tmp <- tempfile("cv25_", tmpdir = dirname(p), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(d, tmp, compress = "gzip", version = 3)
    assert(file.copy(tmp, p, overwrite = TRUE), paste("Cannot save:", p))
  }
  object_hash <- function(x) {
    p <- tempfile(fileext = ".rds"); on.exit(unlink(p), add = TRUE)
    saveRDS(x, p, compress = FALSE, version = 3)
    hash(p)
  }
  bind <- function(x) {
    if (!length(x)) return(data.frame())
    z <- do.call(rbind, x); rownames(z) <- NULL; z
  }
  checked <- function(x, k) {
    need(x, c("check", "result"), "Source summary")
    a <- x$result[x$check == k]
    assert(length(a) == 1L && !is.na(a), paste("Missing/ambiguous source check:", k))
    a
  }
  maxabs <- function(x) if (length(x)) max(abs(x)) else 0

  report <- list(execution_status = "INCOMPLETE", script_version = version,
    upstream_step24_rows = NA, test_years = paste(test_years, collapse = " | "),
    minimum_prior_observations = min_past, minimum_prior_years_each_parity = min_parity,
    central_scenario = central, completed_fold_control_groups = 0L,
    models_fitted_this_run = 0L, cached_groups_reused = 0L,
    unique_test_records = 0L, numerical_tests_passed = FALSE,
    chronology_verified = FALSE, same_holdout_records_within_origin = FALSE,
    year_fixed_effects_fitted = FALSE, heldout_outcomes_used_in_fit = FALSE,
    source_pool_selected_retrospectively = TRUE, historical_data_vintages_available = FALSE,
    operational_forecast_validated = FALSE, observed_harvest_alignment = FALSE,
    parameter_tuning_on_test_years = FALSE, original_inputs_modified = FALSE,
    weather_downloads = 0L, daily_water_balance_recomputed = FALSE,
    confidence_intervals_calculated = FALSE, causal_attribution = FALSE,
    last_stage = "Numerical self-tests", error_if_incomplete = "None")
  pred_parts <- diagnostic_parts <- profile_parts <- unit_parts <- coefficient_parts <- list()
  provenance <- data.frame()
  progress <- function() {
    write_csv(data.frame(check = names(report), result = vapply(report, as.character, character(1))),
      file.path(out_dir, "coffee_water_temporal_validation_summary.csv"))
    if (length(pred_parts)) write_csv(bind(pred_parts), file.path(out_dir, "coffee_water_temporal_predictions.csv"))
    if (length(diagnostic_parts)) write_csv(bind(diagnostic_parts), file.path(out_dir, "coffee_water_temporal_fold_diagnostics.csv"))
    if (length(profile_parts)) write_csv(bind(profile_parts), file.path(out_dir, "coffee_water_temporal_membership.csv"))
  }
  on.exit({
    try(progress(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step25.txt")), silent = TRUE)
  }, add = TRUE)

  # Local regressors are deterministic and never estimated from test outcomes.
  local_design <- function(year, parity) {
    z <- cbind(intercept = 1, time = as.numeric(year) - 2018)
    if (parity) z <- cbind(z, odd = as.numeric(year) %% 2L)
    z
  }
  # Exact joint OLS via within-unit projections. No reporting-year effects.
  fit_projection <- function(train, terms, parity) {
    need(train, c("municipality_code", "year", "log_yield", terms), "Training data")
    n <- nrow(train); p <- length(terms)
    y <- number(train$log_yield, "training log yield")
    X <- if (p) as.matrix(train[terms]) else matrix(numeric(0), nrow = n, ncol = 0)
    storage.mode(X) <- "double"
    assert(all(is.finite(X)), "Missing training covariates; no model-specific row removal.")
    groups <- split(seq_len(n), as.character(train$municipality_code))
    Z <- local_design(train$year, parity)
    q <- ncol(Z)
    assert(n > q * length(groups) + p, "Not enough training residual degrees of freedom.")
    residualized <- matrix(NA_real_, nrow = n, ncol = 1L + p)
    local_qr <- vector("list", length(groups)); names(local_qr) <- names(groups)
    for (id in names(groups)) {
      j <- groups[[id]]
      zqr <- qr(Z[j, , drop = FALSE], tol = 1e-10, LAPACK = FALSE)
      assert(zqr$rank == q, paste("Rank-deficient local design:", id))
      local_qr[[id]] <- zqr
      residualized[j, ] <- qr.resid(zqr, cbind(y = y[j], X[j, , drop = FALSE]))
    }
    beta <- setNames(numeric(p), terms)
    condition_index <- NA_real_
    if (p) {
      XR <- residualized[, -1L, drop = FALSE]
      Xqr <- qr(XR, tol = 1e-10, LAPACK = FALSE)
      assert(Xqr$rank == p, "Climate design is rank deficient. No predictors automatically dropped.")
      beta[] <- as.numeric(qr.coef(Xqr, residualized[, 1L]))
      assert(all(is.finite(beta)), "Nonfinite climate coefficients.")
      scales <- sqrt(colSums(XR^2))
      assert(all(scales > 0), "A climate predictor has no training variation after controls.")
      sing <- svd(sweep(XR, 2L, scales, "/"), nu = 0L, nv = 0L)$d
      condition_index <- max(sing) / min(sing)
    }
    nuisance <- matrix(NA_real_, nrow = length(groups), ncol = q,
      dimnames = list(names(groups), colnames(Z)))
    fitted <- numeric(n)
    xb <- if (p) as.numeric(X %*% beta) else rep(0, n)
    for (id in names(groups)) {
      j <- groups[[id]]
      a <- as.numeric(qr.coef(local_qr[[id]], y[j] - xb[j]))
      assert(all(is.finite(a)), paste("Nonfinite local coefficients:", id))
      nuisance[id, ] <- a
      fitted[j] <- as.numeric(Z[j, , drop = FALSE] %*% a) + xb[j]
    }
    list(beta = beta, nuisance = nuisance, terms = terms, parity = parity,
      train_fitted = fitted, train_rmse_log = sqrt(mean((y - fitted)^2)),
      n_training = n, n_training_units = length(groups),
      climate_condition_index = condition_index,
      total_parameter_count = q * length(groups) + p)
  }
  # Deliberately never accesses newdata$log_yield or observed productivity.
  predict_projection <- function(fit, newdata) {
    need(newdata, c("municipality_code", "year", fit$terms), "Prediction inputs")
    ids <- as.character(newdata$municipality_code)
    j <- match(ids, rownames(fit$nuisance))
    assert(!anyNA(j), "Held-out municipality has no estimated training intercept/trend.")
    z <- local_design(newdata$year, fit$parity)
    ans <- rowSums(z * fit$nuisance[j, , drop = FALSE])
    if (length(fit$terms)) {
      x <- as.matrix(newdata[fit$terms]); storage.mode(x) <- "double"
      assert(all(is.finite(x)), "Nonfinite prediction inputs.")
      ans <- ans + as.numeric(x %*% fit$beta)
    }
    assert(all(is.finite(ans)), "Nonfinite predictions.")
    ans
  }
  score <- function(actual, predicted) {
    assert(length(actual) == length(predicted) && length(actual) > 0L &&
      all(is.finite(actual)) && all(is.finite(predicted)), "Invalid scoring pair.")
    err <- predicted - actual
    c(n = length(err), sse_log = sum(err^2), rmse_log = sqrt(mean(err^2)),
      mae_log = mean(abs(err)), bias_log = mean(err))
  }
  # Test projection calculations against an independent, explicit dummy lm.
  self_tests <- function() {
    tests <- list()
    add <- function(name, ok, error = NA_real_) {
      tests[[length(tests) + 1L]] <<- data.frame(test = name, absolute_error = error, pass = isTRUE(ok))
    }
    g <- expand.grid(municipality_code = c("A", "B", "C", "D"), year = 2012:2020,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    i <- seq_len(nrow(g)); unit <- match(g$municipality_code, c("A", "B", "C", "D"))
    g$x1 <- sin(i * 1.71); g$x2 <- cos(i * 0.93) + unit / 11
    g$time <- g$year - 2018; g$odd <- g$year %% 2L
    g$log_yield <- 6 + unit / 9 + (unit / 100) * g$time + unit / 17 * g$odd +
      0.12 * g$x1 - 0.09 * g$x2 + sin(i * 2.6) / 25
    tr <- g[g$year < 2020, ]; te <- g[g$year == 2020, ]
    for (parity in c(FALSE, TRUE)) {
      for (terms in list(character(0), c("x1", "x2"))) {
        f <- fit_projection(tr, terms, parity)
        rhs <- c("0 + factor(municipality_code)", "factor(municipality_code):time",
          if (parity) "factor(municipality_code):odd", terms)
        direct <- stats::lm(stats::as.formula(paste("log_yield ~", paste(rhs, collapse = " + "))), data = tr)
        e1 <- maxabs(f$train_fitted - stats::fitted(direct))
        e2 <- maxabs(predict_projection(f, te) - stats::predict(direct, newdata = te))
        label <- paste("parity", parity, "climate", length(terms))
        add(paste("Training fit matches dummy lm:", label), e1 < 1e-8, e1)
        add(paste("Holdout prediction matches dummy lm:", label), e2 < 1e-8, e2)
      }
    }
    f <- fit_projection(tr, c("x1", "x2"), TRUE)
    changed <- te; changed$log_yield <- te$log_yield + 100
    e <- maxabs(predict_projection(f, changed) - predict_projection(f, te))
    add("Changing held-out outcomes cannot change predictions", e == 0, e)
    feature_only <- te[setdiff(names(te), "log_yield")]
    add("Prediction works without any held-out outcome column",
      maxabs(predict_projection(f, feature_only) - predict_projection(f, te)) < 1e-12)
    shuffled <- te[nrow(te):1L, ]
    add("Unit coefficients matched by exact ID, not row order",
      maxabs(predict_projection(f, shuffled) - rev(predict_projection(f, te))) < 1e-12)
    bad <- te; bad$municipality_code[1L] <- "UNSEEN"
    add("Unseen municipality is rejected", inherits(tryCatch(predict_projection(f, bad), error = identity), "error"))
    add("Training chronology excludes the held-out year", max(tr$year) < min(te$year))
    add("Central limitation increment remains 0.10 lower satisfaction", abs(((1-0.7)/0.1)-((1-0.8)/0.1)-1) < 1e-12)
    sc <- score(c(1, 2), c(2, 2))
    add("Paired scoring formula", abs(sc[["rmse_log"]] - sqrt(0.5)) < 1e-12 && sc[["bias_log"]] == 0.5)
    write_csv(bind(tests), file.path(out_dir, "coffee_water_temporal_numerical_tests.csv"))
    assert(all(bind(tests)$pass), "A numerical self-test failed; source data have not been processed.")
    invisible(TRUE)
  }

  worker <- function() {
    self_tests(); report$numerical_tests_passed <<- TRUE
    if (test_only) {
      report$execution_status <<- "NUMERICAL_TESTS_ONLY"
      report$last_stage <<- "Local tests passed; no source inputs read."
      progress(); return(invisible(NULL))
    }
    paths <- c(input = file.path(source_dir, "coffee_seasonal_water_analysis_input.rds"),
      summary24 = file.path(source_dir, "coffee_seasonal_water_association_summary.csv"),
      membership24 = file.path(source_dir, "coffee_seasonal_water_sample_membership.csv"),
      methods24 = file.path(source_dir, "coffee_seasonal_water_method_notes.txt"))
    assert(all(file.exists(paths)), paste("Missing step24 input(s):", paste(paths[!file.exists(paths)], collapse = "\n")))
    provenance <<- data.frame(role = names(paths), file = unname(paths),
      md5 = vapply(paths, hash, character(1)), stringsAsFactors = FALSE)
    write_csv(provenance, file.path(out_dir, "coffee_water_temporal_input_provenance.csv"))
    s24 <- read_csv(paths[["summary24"]])
    assert(grepl("^COMPLETED_SEASONAL_ASSOCIATIONS", checked(s24, "execution_status")), "Step24 has not completed.")
    assert(checked(s24, "same_sample_across_model_families") == "TRUE", "Step24 shared sample was not verified.")
    d <- readRDS(paths[["input"]])
    assert(is.data.frame(d), "Step24 analysis input is not a data.frame. No nested object is guessed.")
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    heat <- paste0("heat_", windows); rain <- paste0("rain_", windows)
    ratios <- paste(central, windows, sep = "__")
    must <- c("municipality_code", "municipality", "state", "species", "year", "MET_cell_id", "CERES_cell_id",
      "log_yield", "analysis_yield_kg_ha", heat, rain, ratios)
    need(d, must, "Analysis input")
    d <- d[must]
    for (v in c("municipality_code", "municipality", "state", "species", "MET_cell_id", "CERES_cell_id"))
      d[[v]] <- as.character(d[[v]])
    for (v in c("year", "log_yield", "analysis_yield_kg_ha", heat, rain, ratios)) d[[v]] <- number(d[[v]], v)
    assert(all(d$year == as.integer(d$year)) && all(d$year %in% 2012:2024), "Unexpected study years.")
    d$year <- as.integer(d$year)
    assert(!anyNA(d[c("municipality_code", "species", "MET_cell_id", "CERES_cell_id")]) &&
      all(grepl("^[0-9]{7}$", d$municipality_code)) && setequal(d$species, species_set), "Invalid identifiers/species.")
    assert(!anyDuplicated(key(d)), "Duplicate municipality/species/year records.")
    assert(all(d$analysis_yield_kg_ha > 0) &&
      maxabs(d$log_yield - log(d$analysis_yield_kg_ha)) < 1e-9, "Response identity failed.")
    assert(all(as.matrix(d[c(heat, rain)]) >= 0), "Negative precipitation or heat counts.")
    assert(all(as.matrix(d[ratios]) >= -1e-7) && all(as.matrix(d[ratios]) <= 1 + 1e-7), "Invalid central ISNA.")
    for (w in windows) d[[paste0("water_", w)]] <- (1 - d[[paste(central, w, sep = "__")]]) / 0.10
    m <- read_csv(paths[["membership24"]]); need(m, c("municipality_code", "species", "year", "included_step24"), "Step24 membership")
    m$year <- number(m$year, "membership year")
    included <- flag(m$included_step24, "membership included_step24")
    assert(!anyDuplicated(key(m)) && setequal(key(d), key(m)[included]), "Input does not match the exact step24 sample.")
    assert(nrow(d) == as.integer(checked(s24, "retained_common_rows")), "Step24 row count mismatch.")
    d <- d[order(d$species, d$municipality_code, d$year), ]; rownames(d) <- NULL
    report$upstream_step24_rows <<- nrow(d)
    # Values heat_W* and rain_W* were ALREADY divided by 10 and 100 in step24.
    # They are reused as-is, not scaled a second time.
    specs <- data.frame(family = families,
      climate_predictors = c("None", paste(heat, collapse = " | "), paste(c(heat, rain), collapse = " | "),
        paste(c(heat, paste0("water_", windows)), collapse = " | ")),
      source_water_scenario = c("NOT_APPLICABLE", "NOT_APPLICABLE", "NOT_APPLICABLE", central),
      reporting_year_effect = FALSE, scenario_tuning = FALSE)
    write_csv(specs, file.path(out_dir, "coffee_water_temporal_specifications.csv"))
    notes <- c("COFFE-STUDY | STEP25 | CHRONOLOGICAL OUTCOME HOLDOUT", paste("Version:", version),
      "Training reporting years are strictly earlier than each test reporting year (2020-2024).",
      "At least 8 prior observations and at least 3 per parity; exact calendar years, no adjacent-row lags.",
      "All models use identical training and test records within species/origin/control; controls share them too.",
      "The original step24 pool was selected using 2012-2024 data. This selection is retrospective, not real-time.",
      "Training/test data use the available historical production and reanalysis vintages, not archived release vintages.",
      "Forecast equations exclude reporting-year fixed effects. Test-year outcomes never estimate a common year shock.",
      "Thus this is a new forecast-oriented family comparison, not validation of the exact step24 explanation equation.",
      "Local intercepts/trends/parity AND global climate slopes are jointly fitted using training only.",
      "The central scenario is prespecified. Twelve-scenario tuning and coefficient-sign selection are not performed.",
      "H heat; P heat plus rain; W heat plus (1-ISNA)/0.10; N no climate, with the same local controls.",
      "heat_W* and rain_W* already have units of 10 days and 100 mm in step24 and are not rescaled.",
      "W4 is Jan-Mar of the test PAM year. Actual field harvest dates are unknown and may overlap W4.",
      "Prior-year PAM availability at any proposed forecast date is not verified. No operational lead time is claimed.",
      "Scores are equal-record log-productivity RMSE/MAE/bias; bias is prediction minus observation.",
      "Pooled scores weight all held-out records equally; per-year scores also remain available.",
      "MSE skill compares each model to N; RMSE changes vs P are forecast-error changes, not yield changes.",
      "Five held-out years and shared climate cells do not justify iid record-level uncertainty intervals.",
      "No confidence intervals or significance ranking, automatic winner, or causal-loss calculations are produced.",
      "Records missing from the original step24 pool cannot be restored from these inputs; coverage is relative to that pool.",
      "No weather download, water-balance rerun, missing-value imputation, or prior-file modification.",
      "https://otexts.com/fpp3/tscv.html", "https://stat.ethz.ch/R-manual/R-devel/library/base/html/qr.html")
    writeLines(notes, file.path(out_dir, "coffee_water_temporal_method_notes.txt"), useBytes = TRUE)
    methods_hash <- object_hash(list(version = version, input_md5 = provenance$md5,
      years = test_years, min_past = min_past, min_parity = min_parity, scenario = central,
      no_year_FE = TRUE, controls = controls, source_pool = "retrospective_step24"))
    profile_total <- 0L
    for (species in species_set) for (yr in test_years) {
      a <- d[d$species == species, , drop = FALSE]
      groups <- split(seq_len(nrow(a)), a$municipality_code)
      profiles <- bind(lapply(names(groups), function(id) {
        z <- a[groups[[id]], , drop = FALSE]; past <- z$year[z$year < yr]
        n <- length(past); odd <- sum(past %% 2L == 1L); even <- sum(past %% 2L == 0L)
        ready <- n >= min_past && odd >= min_parity && even >= min_parity
        has_test <- any(z$year == yr)
        data.frame(species = species, test_year = yr, municipality_code = id,
          municipality = z$municipality[1L], state = z$state[1L],
          training_observations = n, prior_odd_years = odd, prior_even_years = even,
          first_training_year = if (n) min(past) else NA_integer_,
          last_training_year = if (n) max(past) else NA_integer_,
          training_eligible = ready, test_record_in_step24 = has_test, test_included = ready && has_test,
          reason = if (!ready) "INSUFFICIENT_PRIOR_TIME_SUPPORT" else if (!has_test) "NO_TEST_RECORD_IN_STEP24_POOL" else "INCLUDED",
          source_pool_selected_retrospectively = TRUE, stringsAsFactors = FALSE)
      }))
      profile_parts[[length(profile_parts) + 1L]] <<- profiles
      profile_total <- profile_total + sum(profiles$test_included)
      ids <- profiles$municipality_code[profiles$training_eligible]
      tr <- a[a$year < yr & a$municipality_code %in% ids, , drop = FALSE]
      te <- a[a$year == yr & a$municipality_code %in% ids, , drop = FALSE]
      assert(nrow(te) > 0L && length(ids) > 1L, paste("No adequate holdout:", species, yr))
      assert(max(tr$year) < min(te$year), "Training chronology violated.")
      assert(!length(intersect(key(tr), key(te))), "Training/test outcome overlap.")
      for (control in controls) {
        parity <- identical(control, "Local_trend_parity")
        signature <- object_hash(list(methods = methods_hash, species = species, test_year = yr, control = control,
          train_keys = key(tr), test_keys = key(te)))
        cache_file <- file.path(cache_dir, paste0(species, "_", yr, "_", control, "_", substr(signature, 1L, 16L), ".rds"))
        side <- paste0(cache_file, ".md5.txt")
        if (file.exists(cache_file)) {
          assert(file.exists(side) && identical(trimws(readLines(side, warn = FALSE)[1L]), hash(cache_file)), "Fold cache hash mismatch.")
          bundle <- readRDS(cache_file)
          assert(identical(bundle$signature, signature) && identical(bundle$train_keys, key(tr)) &&
            identical(bundle$test_keys, key(te)), "Fold cache contract mismatch.")
          assert(nrow(bundle$predictions) == nrow(te) * length(families) &&
            setequal(bundle$predictions$family, families) && all(is.finite(bundle$predictions$predicted_log_yield)), "Invalid fold cache.")
          report$cached_groups_reused <<- report$cached_groups_reused + 1L
          message("Reusing ", species, " | ", yr, " | ", control)
        } else {
          message("Testing ", species, " | ", yr, " | ", control,
            " | train=", nrow(tr), " | test=", nrow(te))
          ps <- dg <- coeflist <- local_list <- list()
          for (fam in families) {
            terms <- switch(fam, N = character(0), H = heat, P = c(heat, rain), W = c(heat, paste0("water_", windows)))
            fit <- fit_projection(tr, terms, parity)
            feature_only <- te[unique(c("municipality_code", "year", terms))]
            predicted <- predict_projection(fit, feature_only)
            ps[[fam]] <- data.frame(municipality_code = te$municipality_code, municipality = te$municipality,
              state = te$state, species = species, year = yr, MET_cell_id = te$MET_cell_id,
              CERES_cell_id = te$CERES_cell_id, control = control, family = fam,
              scenario_id = if (fam == "W") central else "NOT_APPLICABLE",
              observed_log_yield = te$log_yield, predicted_log_yield = predicted,
              error_log = predicted - te$log_yield, training_last_year = max(tr$year),
              heldout_outcome_used_in_fit = FALSE, stringsAsFactors = FALSE)
            dg[[fam]] <- data.frame(species = species, test_year = yr, control = control, family = fam,
              training_rows = nrow(tr), training_municipalities = length(unique(tr$municipality_code)),
              test_rows = nrow(te), test_municipalities = length(unique(te$municipality_code)),
              test_MET_cells = length(unique(te$MET_cell_id)), test_CERES_cells = length(unique(te$CERES_cell_id)),
              step24_test_records = sum(a$year == yr), test_record_coverage_pct = 100 * nrow(te) / sum(a$year == yr),
              first_training_year = min(tr$year), last_training_year = max(tr$year),
              training_rmse_log = fit$train_rmse_log, climate_parameters = length(terms),
              total_parameter_count = fit$total_parameter_count,
              climate_condition_index = fit$climate_condition_index, year_fixed_effect = FALSE)
            if (length(terms)) coeflist[[fam]] <- data.frame(species = species, test_year = yr,
              control = control, family = fam, term = names(fit$beta), beta_log = unname(fit$beta),
              coefficient_role = "TRAINING_ONLY_PREDICTION_COEFFICIENT_NOT_STEP24_INFERENCE")
            local_list[[fam]] <- data.frame(municipality_code = rownames(fit$nuisance),
              species = species, test_year = yr, control = control, family = fam,
              intercept = fit$nuisance[, "intercept"], time = fit$nuisance[, "time"],
              odd = if (parity) fit$nuisance[, "odd"] else NA_real_, row.names = NULL)
            report$models_fitted_this_run <<- report$models_fitted_this_run + 1L
          }
          bundle <- list(signature = signature, train_keys = key(tr), test_keys = key(te),
            predictions = bind(ps), diagnostics = bind(dg), climate_coefficients = bind(coeflist),
            local_coefficients = bind(local_list))
          write_rds(bundle, cache_file); writeLines(hash(cache_file), side, useBytes = TRUE)
        }
        pred_parts[[length(pred_parts) + 1L]] <<- bundle$predictions
        diagnostic_parts[[length(diagnostic_parts) + 1L]] <<- bundle$diagnostics
        coefficient_parts[[length(coefficient_parts) + 1L]] <<- bundle$climate_coefficients
        unit_parts[[length(unit_parts) + 1L]] <<- bundle$local_coefficients
        report$completed_fold_control_groups <<- report$completed_fold_control_groups + 1L
        report$last_stage <<- paste(species, yr, control)
        progress()
      }
    }
    # Equal-record test metrics; reference comparisons require identical keys.
    preds <- bind(pred_parts)
    assert(!anyDuplicated(paste(key(preds), preds$control, preds$family, sep = "|")), "Duplicate predictions.")
    metrics <- list()
    for (sp in species_set) for (ctrl in controls) for (period in c(as.character(test_years), "POOLED_2020_2024")) {
      q <- preds[preds$species == sp & preds$control == ctrl, , drop = FALSE]
      if (period != "POOLED_2020_2024") q <- q[q$year == as.integer(period), , drop = FALSE]
      ref <- q[q$family == "N", , drop = FALSE]
      rainpred <- q[q$family == "P", , drop = FALSE]
      rainpred <- rainpred[match(key(ref), key(rainpred)), ]
      assert(!anyNA(rainpred$municipality_code), "Missing paired precipitation benchmark.")
      sn <- score(ref$observed_log_yield, ref$predicted_log_yield)
      sr <- score(rainpred$observed_log_yield, rainpred$predicted_log_yield)
      for (fam in families) {
        z <- q[q$family == fam, , drop = FALSE]
        assert(setequal(key(z), key(ref)), "Models differ in held-out records.")
        z <- z[match(key(ref), key(z)), ]
        assert(maxabs(z$observed_log_yield - ref$observed_log_yield) == 0, "Held-out response differs across models.")
        ss <- score(z$observed_log_yield, z$predicted_log_yield)
        metrics[[length(metrics) + 1L]] <- data.frame(species = sp, control = ctrl, test_period = period,
          family = fam, n_obs = as.integer(ss[["n"]]), test_years = length(unique(z$year)),
          municipalities = length(unique(z$municipality_code)), rmse_log = ss[["rmse_log"]],
          mae_log = ss[["mae_log"]], bias_log = ss[["bias_log"]], sum_squared_error_log = ss[["sse_log"]],
          mse_skill_vs_N = if (sn[["sse_log"]] > 0) 1 - ss[["sse_log"]] / sn[["sse_log"]] else NA_real_,
          difference_rmse_log_vs_P = ss[["rmse_log"]] - sr[["rmse_log"]],
          relative_rmse_reduction_vs_P_pct = if (sr[["rmse_log"]] > 0) 100 * (1 - ss[["rmse_log"]] / sr[["rmse_log"]]) else NA_real_,
          interpretation = "Chronological outcome holdout on retrospective step24 pool; error change is not yield loss",
          row.names = NULL)
      }
    }
    scores <- bind(metrics)
    write_csv(scores, file.path(out_dir, "coffee_water_temporal_model_comparison.csv"))
    write_csv(bind(coefficient_parts), file.path(out_dir, "coffee_water_temporal_training_climate_coefficients.csv"))
    write_rds(bind(unit_parts), file.path(out_dir, "coffee_water_temporal_training_local_coefficients.rds"))
    peryear <- scores[scores$family == "W" & scores$test_period != "POOLED_2020_2024", ]
    consistency <- bind(lapply(split(peryear, paste(peryear$species, peryear$control)), function(z) {
      data.frame(species = z$species[1L], control = z$control[1L], test_years = nrow(z),
        years_lower_water_RMSE_than_rain = sum(z$difference_rmse_log_vs_P < 0),
        years_higher_water_RMSE_than_rain = sum(z$difference_rmse_log_vs_P > 0),
        minimum_delta_rmse = min(z$difference_rmse_log_vs_P), maximum_delta_rmse = max(z$difference_rmse_log_vs_P),
        confidence_interval = "NOT_ESTIMATED", automatic_model_selection = FALSE)
    }))
    write_csv(consistency, file.path(out_dir, "coffee_water_temporal_year_consistency.csv"))
    assert(report$completed_fold_control_groups == length(species_set) * length(test_years) * length(controls), "Incomplete fold plan.")
    assert(all(preds$training_last_year < preds$year), "Prediction uses a non-prior training year.")
    for (sp in species_set) for (yr in test_years) {
      q <- preds[preds$species == sp & preds$year == yr, ]
      kk <- lapply(split(q, paste(q$control, q$family)), key)
      assert(all(vapply(kk, function(k) setequal(k, kk[[1L]]), logical(1))), "Control-dependent holdout sample.")
    }
    assert(all(vapply(provenance$file, hash, character(1)) == provenance$md5), "Source input changed during execution.")
    report$unique_test_records <<- length(unique(key(preds)))
    assert(report$unique_test_records == profile_total, "Test membership total mismatch.")
    report$chronology_verified <<- TRUE
    report$same_holdout_records_within_origin <<- TRUE
    report$execution_status <<- "COMPLETED_CHRONOLOGICAL_OUTCOME_HOLDOUT"
    report$last_stage <<- "Test predictions and paired error metrics saved. No scenario selected and no causal claim."
    progress()
    message("\n", report$execution_status, "\nOutputs: ", out_dir,
      "\nSend coffee_water_temporal_validation_summary.csv and coffee_water_temporal_model_comparison.csv")
  }
  tryCatch(worker(), error = function(e) {
    report$error_if_incomplete <<- substr(conditionMessage(e), 1L, 1500L)
    report$execution_status <<- "INCOMPLETE"
    try(progress(), silent = TRUE)
    stop(conditionMessage(e), "\nPrior inputs were not modified. Completed fold caches were retained.\n", out_dir, call. = FALSE)
  }, interrupt = function(e) {
    report$execution_status <<- "INTERRUPTED_RESUMABLE"
    report$error_if_incomplete <<- "Interrupted; rerun this file to reuse verified fold caches."
    try(progress(), silent = TRUE)
    message("\nInterrupted. Completed fold caches retained; no source data changed.")
    invisible(NULL)
  })
}

coffee_step25()
