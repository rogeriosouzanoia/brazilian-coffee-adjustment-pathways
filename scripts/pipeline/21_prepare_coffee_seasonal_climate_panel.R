# ======================================================================
# COFFE-STUDY | STEP 21 | CANDIDATE SEASONAL EXPOSURE WINDOWS
# Run the COMPLETE file with Source in RStudio.
#
# Uses unchanged step19/20 panel/sample and step16 NASA POWER daily files.
# NO downloads, regressions, imputation, p-value selection or new main figure.
# Every step20 record remains present; eligibility is carried forward.
#
# Four explicit, non-overlapping candidate windows relative to PAM year t:
# W1: Sep(t-2) through Mar(t-1)
# W2: Apr(t-1) through Aug(t-1)
# W3: Sep(t-1) through Dec(t-1)
# W4: Jan(t)   through Mar(t)
# The first four phases in Camargo & Camargo (2001) motivate the partition
# for Arabica. Dates are CANDIDATES, not observations of local crop phases.
# For Canephora these are CALENDAR WINDOWS ONLY: the Arabica phase labels
# are NOT transferred. Regional/genotypic flowering and harvest timing
# remain to be established. W4 can contain postharvest days at some sites.
# No claim that these four windows cover every yield-relevant period.
#
# Sources consulted for the candidate design (not downloaded by script):
# Camargo AP & Camargo MBP (2001), Bragantia 60:65-68.
# https://doi.org/10.1590/S0006-87052001000100008
# https://www.ciiagro.sp.gov.br/monitoramentocafe/Fenologia.htm
# Crasque J et al. (2025), Agronomy Journal 117:e70103.
# https://doi.org/10.1002/agj2.70103
# The latter documents contrasting Canephora genotypes at ONE ES site,
# not a universal calendar for Canephora in Brazil.
#
# Longest sequences are recalculated on CONTIGUOUS DAILY data across month
# and year boundaries. Monthly maxima must not be added/maximized to get
# an entire-window longest dry sequence. No daily files are replicated.
# Source-cell sharing, UTC days and the original missing-data rules remain.
#
# Base R only. Optional environment variables:
#   COFFEE_STUDY_DIR           project root override
#   COFFEE_STEP21_TEST_ONLY    TRUE runs internal tests without project data
#   COFFEE_STEP21_NO_AUTORUN   TRUE loads the function for explicit testing
# ======================================================================

coffee_step21 <- function() {
  version <- "21_candidate_seasonal_windows_v1"
  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(x, cols, label) {
    absent <- setdiff(cols, names(x))
    assert(!length(absent), paste(label, "missing:", paste(absent, collapse = ", ")))
  }
  bind <- function(xs) {
    if (!length(xs)) return(data.frame())
    z <- do.call(rbind, xs); rownames(z) <- NULL; z
  }
  key <- function(d) paste(d$municipality_code, d$species, d$year, sep = "|")
  hash <- function(f) unname(tools::md5sum(f))
  checked <- function(d, name) {
    need(d, c("check", "result"), "Summary")
    v <- d$result[d$check == name]
    assert(length(v) == 1L && !is.na(v), paste("Missing summary key:", name)); v
  }
  number <- function(x, label, allow_na = TRUE) {
    txt <- as.character(x); z <- suppressWarnings(as.numeric(txt))
    assert(!any(!is.na(txt) & is.na(z)) && all(is.na(z) | is.finite(z)) &&
      (allow_na || !anyNA(z)), paste("Invalid numeric field:", label)); z
  }
  logical_col <- function(x, label) {
    assert(!anyNA(x) && all(as.character(x) %in% c("TRUE", "FALSE")),
      paste("Invalid logical field:", label)); as.character(x) == "TRUE"
  }
  read_csv <- function(f) {
    assert(file.exists(f), paste("Required input not found:", f))
    utils::read.csv(f, colClasses = "character", stringsAsFactors = FALSE,
      check.names = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  write_csv <- function(d, f) {
    tmp <- tempfile("coffee21_", tmpdir = dirname(f), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(d, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, f, overwrite = TRUE), paste("Cannot write:", f))
  }
  write_rds <- function(d, f) {
    tmp <- tempfile("coffee21_", tmpdir = dirname(f), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(d, tmp, compress = "gzip")
    assert(file.copy(tmp, f, overwrite = TRUE), paste("Cannot write:", f))
  }
  longest <- function(x) {
    if (anyNA(x)) return(NA_real_)
    if (!length(x) || !any(x)) return(0)
    r <- rle(x); as.numeric(max(r$lengths[r$values]))
  }
  make_windows <- function(years) {
    bind(lapply(years, function(yr) {
      data.frame(year = as.integer(yr), window_id = paste0("W", 1:4),
        start = as.Date(c(paste0(yr - 2, "-09-01"), paste0(yr - 1, "-04-01"),
                          paste0(yr - 1, "-09-01"), paste0(yr, "-01-01"))),
        end = as.Date(c(paste0(yr - 1, "-03-31"), paste0(yr - 1, "-08-31"),
                        paste0(yr - 1, "-12-31"), paste0(yr, "-03-31"))),
        calendar_label = c("Sep(t-2) to Mar(t-1)", "Apr(t-1) to Aug(t-1)",
                           "Sep(t-1) to Dec(t-1)", "Jan(t) to Mar(t)"),
        includes_PAM_year_weather = c(FALSE, FALSE, FALSE, TRUE),
        farm_harvest_alignment_verified = FALSE, stringsAsFactors = FALSE)
    }))
  }
  summarize_window <- function(d, start, end) {
    expected <- seq(as.Date(start), as.Date(end), by = "day")
    ix <- match(expected, d$date)
    z <- d[ix, , drop = FALSE]  # Missing calendar days remain NA, never zero.
    tt <- cbind(z$tmin_c, z$tmean_c, z$tmax_c)
    order_bad <- stats::complete.cases(tt) &
      ((z$tmin_c > z$tmean_c + 0.05) | (z$tmean_c > z$tmax_c + 0.05) |
       (z$tmin_c > z$tmax_c + 0.05))
    order_bad[is.na(order_bad)] <- FALSE
    tok <- stats::complete.cases(tt) & rowSums(!is.finite(tt) | tt < -60 | tt > 60) == 0L & !order_bad
    pok <- is.finite(z$precipitation_mm) & z$precipitation_mm >= 0 & z$precipitation_mm <= 1500
    tcomplete <- all(tok); pcomplete <- all(pok)
    tx <- z$tmax_c; rain <- z$precipitation_mm
    data.frame(expected_days = length(expected), source_date_gaps = sum(is.na(ix)),
      valid_temperature_days = sum(tok), valid_precipitation_days = sum(pok),
      temperature_order_failures = sum(order_bad),
      temperature_complete = tcomplete, precipitation_complete = pcomplete,
      tmean_c = if (tcomplete) mean(z$tmean_c) else NA_real_,
      tmin_mean_c = if (tcomplete) mean(z$tmin_c) else NA_real_,
      tmax_mean_c = if (tcomplete) mean(tx) else NA_real_,
      hot_days_ge30 = if (tcomplete) sum(tx >= 30) else NA_real_,
      hot_days_ge32 = if (tcomplete) sum(tx >= 32) else NA_real_,
      hot_days_ge35 = if (tcomplete) sum(tx >= 35) else NA_real_,
      heat_excess35_c_days = if (tcomplete) sum(pmax(tx - 35, 0)) else NA_real_,
      longest_hot_spell_ge35_days = if (tcomplete) longest(tx >= 35) else NA_real_,
      precipitation_total_mm = if (pcomplete) sum(rain) else NA_real_,
      dry_days_lt1 = if (pcomplete) sum(rain < 1) else NA_real_,
      longest_dry_spell_days = if (pcomplete) longest(rain < 1) else NA_real_,
      rain_ge20_days = if (pcomplete) sum(rain >= 20) else NA_real_)
  }
  metrics <- c("tmean_c", "tmin_mean_c", "tmax_mean_c", "hot_days_ge30", "hot_days_ge32",
    "hot_days_ge35", "heat_excess35_c_days", "longest_hot_spell_ge35_days",
    "precipitation_total_mm", "dry_days_lt1", "longest_dry_spell_days", "rain_ge20_days")
  primary <- c("tmean_c", "hot_days_ge32", "precipitation_total_mm", "longest_dry_spell_days")

  # Internal tests use artificial data and do not assert observed phenology.
  tests <- list()
  add_test <- function(label, ok) tests[[length(tests) + 1L]] <<- data.frame(test = label, pass = isTRUE(ok))
  wtest <- make_windows(2012:2013)
  add_test("Four windows per reporting year", nrow(wtest) == 8L)
  w1 <- wtest[wtest$year == 2012 & wtest$window_id == "W1", ]
  add_test("Lag dates use reporting years, not adjacent rows",
    as.character(w1$start) == "2010-09-01" && as.character(w1$end) == "2011-03-31")
  n4 <- as.integer(wtest$end[wtest$window_id == "W4"] - wtest$start[wtest$window_id == "W4"]) + 1L
  add_test("Leap-year January-March length", identical(n4, c(91L, 90L)))
  dtest <- data.frame(date = seq(as.Date("2019-12-29"), as.Date("2020-01-03"), by = "day"),
    tmean_c = 25, tmin_c = 20, tmax_c = c(31, 32, 33, 34, 35, 36),
    precipitation_mm = c(4, 0, 0, 0, 0, 4))
  s <- summarize_window(dtest, min(dtest$date), max(dtest$date))
  add_test("Dry sequence crosses December-January", s$longest_dry_spell_days == 4)
  add_test("Inclusive heat threshold", s$hot_days_ge32 == 5 && s$hot_days_ge35 == 2)
  add_test("Heat excess is daily Tmax excess, not hourly exposure", s$heat_excess35_c_days == 1)
  dtest$precipitation_mm[3] <- NA_real_
  s <- summarize_window(dtest, min(dtest$date), max(dtest$date))
  add_test("Missing precipitation excludes only dependent metrics",
    is.na(s$precipitation_total_mm) && is.na(s$longest_dry_spell_days) && is.finite(s$tmean_c))
  s <- summarize_window(dtest[-2, ], min(dtest$date), max(dtest$date))
  add_test("Missing date is retained as a gap", s$source_date_gaps == 1 && is.na(s$tmean_c))
  add_test("All wet days imply zero dry sequence", longest(rep(FALSE, 5)) == 0)
  w <- make_windows(2012)
  all_dates <- unlist(lapply(seq_len(nrow(w)), function(i) as.character(seq(w$start[i], w$end[i], by = "day"))))
  add_test("Four candidate windows do not overlap", !anyDuplicated(all_dates))
  test_table <- bind(tests)
  assert(all(test_table$pass), "A step21 numerical test failed. No project files were read.")
  if (toupper(Sys.getenv("COFFEE_STEP21_TEST_ONLY", "FALSE")) == "TRUE") {
    print(test_table); return(invisible(test_table))
  }

  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  out_dir <- file.path(base_dir, "04_outputs", "seasonal_climate_yield_panel")
  data_dir <- file.path(base_dir, "02_data_processed", "seasonal_climate_yield_panel")
  cache_dir <- file.path(data_dir, "cell_cache")
  for (p in c(out_dir, data_dir, cache_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    assert(dir.exists(p), paste("Cannot create:", p))
  }
  lock <- file.path(out_dir, ".step21_running")
  assert(dir.create(lock, showWarnings = FALSE), paste(
    "Another step21 session may be active. Confirm it has stopped before removing:", lock))
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  writeLines(paste(Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))
  old <- options(scipen = 999); on.exit(options(old), add = TRUE)
  report <- list(execution_status = "INCOMPLETE", script_version = version,
    source = "NASA POWER", reporting_period = "2012 to 2024", candidate_windows = 4L,
    panel_rows = 0L, inherited_eligible_rows = 0L, required_MET_cells = 0L,
    processed_MET_cells = 0L, cached_MET_cells = 0L, source_cell_window_rows = 0L,
    incomplete_source_cell_windows = 0L, seasonal_join_missing_rows = 0L,
    inherited_eligible_rows_with_complete_windows = 0L, annual_benchmarks_passed = FALSE,
    numerical_tests_passed = TRUE, source_downloads = 0L, fitted_regressions = 0L,
    selection_by_association_significance = FALSE, windows_observed_at_farms = FALSE,
    arabica_phases_transferred_to_canephora = FALSE, preharvest_alignment_verified = FALSE,
    daily_data_replicated_by_municipality = FALSE, missing_values_imputed = FALSE,
    previous_inputs_modified = FALSE, error_if_incomplete = "None")
  save_report <- function() write_csv(data.frame(check = names(report),
    result = vapply(report, as.character, character(1))), file.path(out_dir, "coffee_seasonal_climate_summary.csv"))
  on.exit({
    try(save_report(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step21.txt")), silent = TRUE)
  }, add = TRUE)
  write_csv(test_table, file.path(out_dir, "coffee_seasonal_numerical_tests.csv"))

  worker <- function() {
    paths <- c(
      panel = file.path(base_dir, "02_data_processed", "climate_yield_panel", "coffee_yield_climate_panel_2012_2024.rds"),
      summary20 = file.path(base_dir, "04_outputs", "climate_yield_associations", "coffee_climate_association_summary.csv"),
      membership20 = file.path(base_dir, "04_outputs", "climate_yield_associations", "coffee_climate_association_sample_membership.csv"),
      provenance20 = file.path(base_dir, "04_outputs", "climate_yield_associations", "coffee_climate_association_input_provenance.csv"),
      manifest16 = file.path(base_dir, "04_outputs", "climate_power_brazil_download", "coffee_power_brazil_download_manifest.csv"))
    assert(all(file.exists(paths)), paste("Missing input(s):", paste(paths[!file.exists(paths)], collapse = "\n")))
    provenance <- data.frame(role = names(paths), file = unname(paths),
      md5 = vapply(paths, hash, character(1)), stringsAsFactors = FALSE)
    write_csv(provenance, file.path(out_dir, "coffee_seasonal_input_provenance.csv"))
    s20 <- read_csv(paths[["summary20"]]); p20 <- read_csv(paths[["provenance20"]])
    assert(checked(s20, "execution_status") == "COMPLETED_EXPLORATORY_ASSOCIATIONS", "Step20 is incomplete.")
    h20 <- p20$md5[p20$role == "panel"]
    assert(length(h20) == 1L && hash(paths[["panel"]]) == h20, "Step19 panel has changed since step20.")
    d <- as.data.frame(readRDS(paths[["panel"]]), stringsAsFactors = FALSE)
    need(d, c("municipality_code", "municipality", "species", "year", "MET_cell_id", "production_t",
      "area_harvested_ha", "log_yield", "yield_trend_eligible", "yield_trend_parity_eligible", "positive_yield_for_log"), "Step19 panel")
    for (v in c("municipality_code", "species", "MET_cell_id")) d[[v]] <- as.character(d[[v]])
    d$year <- as.integer(number(d$year, "year", FALSE))
    assert(!anyDuplicated(key(d)) && identical(sort(unique(d$year)), 2012:2024), "Unexpected panel keys/years.")
    assert(all(d$species %in% c("Arabica", "Canephora")), "Unexpected species.")
    ix20 <- read_csv(paths[["membership20"]]); need(ix20, c("municipality_code", "species", "year", "MET_cell_id", "included"), "Step20 membership")
    assert(!anyDuplicated(key(ix20)) && setequal(key(ix20), key(d)), "Step20 and step19 row keys differ.")
    j <- match(key(d), key(ix20))
    assert(all(d$MET_cell_id == ix20$MET_cell_id[j]), "Cell identities differ from step20.")
    d$included_step20 <- logical_col(ix20$included[j], "step20 membership")
    clim20 <- unlist(lapply(0:2, function(k) paste0("clim_", primary, "_lag", k)), use.names = FALSE)
    need(d, clim20, "Annual benchmark columns")
    same_sample <- logical_col(d$yield_trend_eligible, "trend") &
      logical_col(d$yield_trend_parity_eligible, "parity") & logical_col(d$positive_yield_for_log, "positive yield") &
      is.finite(d$log_yield) & stats::complete.cases(d[clim20])
    assert(identical(same_sample, d$included_step20), "Inherited eligibility differs from step20 criteria.")
    assert(sum(d$included_step20) == as.integer(checked(s20, "eligible_rows")), "Eligible row count differs.")
    report$panel_rows <<- nrow(d); report$inherited_eligible_rows <<- sum(d$included_step20)

    win <- make_windows(2012:2024); win$expected_days <- as.integer(win$end - win$start) + 1L
    write_csv(win, file.path(out_dir, "coffee_seasonal_window_dates.csv"))
    phases <- c("Vegetative growth and leaf-bud formation", "Floral induction and bud development",
                "Flowering and early fruit expansion", "Grain filling")
    design <- bind(lapply(c("Arabica", "Canephora"), function(sp) data.frame(
      species = sp, window_id = paste0("W", 1:4), calendar_label = win$calendar_label[1:4],
      interpretation = if (sp == "Arabica") paste("Candidate only:", phases) else rep("Calendar exposure only; crop phase unassigned", 4),
      source_basis = if (sp == "Arabica") "Camargo & Camargo 2001; DOI 10.1590/S0006-87052001000100008" else
        "Calendar partition only; Crasque et al. 2025 (10.1002/agj2.70103) cautions against a universal phase calendar",
      observed_phase_dates = FALSE, local_calendar_verified = FALSE,
      includes_PAM_year_weather = c(FALSE, FALSE, FALSE, TRUE),
      possible_postharvest_weather = c(FALSE, FALSE, FALSE, TRUE), stringsAsFactors = FALSE)))
    write_csv(design, file.path(out_dir, "coffee_seasonal_window_dictionary.csv"))
    md <- data.frame(metric = metrics, unit = c(rep("degC",3), rep("days",3), "degC day", "days", "mm", "days", "days", "days"),
      definition = c("Mean NASA T2M over all window days", "Mean daily Tmin", "Mean daily Tmax", "Count Tmax >= 30 C",
        "Count Tmax >= 32 C", "Count Tmax >= 35 C", "Sum max(Tmax-35,0)", "Longest contiguous Tmax>=35 sequence inside window",
        "Sum daily precipitation", "Count precipitation<1 mm", "Longest contiguous precipitation<1 mm sequence inside window",
        "Count precipitation>=20 mm"),
      completeness = "All dependent daily values required; no imputation or rescaling", stringsAsFactors = FALSE)
    write_csv(md, file.path(out_dir, "coffee_seasonal_metric_dictionary.csv"))
    design_token_file <- tempfile(fileext = ".rds")
    saveRDS(list(version, win, md), design_token_file); design_hash <- hash(design_token_file); unlink(design_token_file)

    mf <- read_csv(paths[["manifest16"]]); need(mf, c("cell_id", "family", "status", "daily_file", "daily_md5"), "Source manifest")
    cells <- sort(unique(d$MET_cell_id)); report$required_MET_cells <<- length(cells)
    jobs <- mf[mf$family == "MET" & mf$cell_id %in% cells, , drop = FALSE]
    assert(!anyDuplicated(jobs$cell_id) && setequal(jobs$cell_id, cells), "MET manifest does not cover panel cells exactly.")
    jobs <- jobs[match(cells, jobs$cell_id), , drop = FALSE]
    assert(all(jobs$status %in% c("VALID_COMPLETE", "VALID_WITH_QA_FLAGS")), "A source history was not validated.")
    # Only remap the exact project-relative file path when the root is changed.
    resolve_daily <- function(p) {
      if (file.exists(p)) return(p)
      p <- gsub("\\", "/", p, fixed = TRUE)
      marker <- "02_data_processed/climate/NASA_POWER/brazil_grid/"
      at <- regexpr(marker, p, fixed = TRUE)[1L]
      if (at > 0L) {
        candidate <- file.path(base_dir, substr(p, at, nchar(p)))
        if (file.exists(candidate)) return(candidate)
      }
      stop("Required downloaded history not found: ", p, call. = FALSE)
    }
    source_rows <- cache_rows <- benchmarks <- cell_parts <- list()
    for (i in seq_along(cells)) {
      job <- jobs[i, ]; id <- cells[i]
      daily_file <- resolve_daily(job$daily_file)
      source_hash <- hash(daily_file)
      assert(identical(source_hash, job$daily_md5), paste("Downloaded MET history changed:", id))
      cache <- file.path(cache_dir, paste0(id, "_", substr(source_hash,1,12), "_", substr(design_hash,1,12), ".rds"))
      message(sprintf("[%d/%d] %s", i, length(cells), id))
      reused <- FALSE
      if (file.exists(cache)) {
        result <- readRDS(cache)
        assert(identical(result$version, version) && identical(result$source_md5, source_hash) &&
          identical(result$design_md5, design_hash), paste("Cache provenance mismatch:", cache))
        reused <- TRUE
      } else {
        raw <- as.data.frame(readRDS(daily_file), stringsAsFactors = FALSE)
        need(raw, c("date", "tmean_c", "tmin_c", "tmax_c", "precipitation_mm", "cell_id", "family", "source", "time_standard"), "MET history")
        assert(all(raw$cell_id == id & raw$family == "MET" & raw$source == "NASA POWER" & raw$time_standard == "UTC"), "MET identity/time standard mismatch.")
        assert(inherits(raw$date, "Date") || is.character(raw$date), "Unexpected daily date type.")
        raw$date <- as.Date(raw$date)
        assert(!anyNA(raw$date) && !anyDuplicated(raw$date), "Invalid/duplicate daily dates.")
        raw <- raw[order(raw$date), , drop = FALSE]
        assert(identical(as.character(raw$date), as.character(seq(as.Date("1981-01-01"), as.Date("2024-12-31"), by="day"))),
          paste("Unexpected daily calendar in", id))
        for (v in c("tmean_c","tmin_c","tmax_c","precipitation_mm")) raw[[v]] <- number(raw[[v]], v)
        windows <- bind(lapply(seq_len(nrow(win)), function(k) cbind(
          data.frame(cell_id = id, year = win$year[k], window_id = win$window_id[k],
            start = win$start[k], end = win$end[k]), summarize_window(raw, win$start[k], win$end[k]))))
        # Exact annual benchmarks verify that the new daily engine agrees
        # with already-fitted annual exposures, without changing those inputs.
        annual <- bind(lapply(2010:2023, function(yr) cbind(data.frame(cell_id = id, climate_year = yr),
          summarize_window(raw, paste0(yr,"-01-01"), paste0(yr,"-12-31")))))
        result <- list(version = version, source_md5 = source_hash, design_md5 = design_hash,
          windows = windows, annual = annual)
        write_rds(result, cache)
        rm(raw)
      }
      assert(nrow(result$windows) == nrow(win) && all(result$windows$cell_id == id), "Incomplete cell-window cache.")
      cell_parts[[i]] <- result$windows; benchmarks[[i]] <- result$annual
      source_rows[[i]] <- data.frame(cell_id=id, daily_file=daily_file, daily_md5=source_hash)
      cache_rows[[i]] <- data.frame(cell_id=id, cache_file=cache, cache_md5=hash(cache), reused=reused)
      report$processed_MET_cells <<- i
      report$cached_MET_cells <<- report$cached_MET_cells + as.integer(reused)
      if (i %% 20L == 0L || i == length(cells)) save_report()
    }
    cell_windows <- bind(cell_parts); benchmark <- bind(benchmarks)
    assert(!anyDuplicated(paste(cell_windows$cell_id, cell_windows$year, cell_windows$window_id)), "Duplicate source-window keys.")
    write_csv(bind(source_rows), file.path(out_dir,"coffee_seasonal_source_provenance.csv"))
    write_csv(bind(cache_rows), file.path(out_dir,"coffee_seasonal_cache_index.csv"))
    write_rds(cell_windows, file.path(data_dir,"coffee_seasonal_source_cell_indices.rds"))
    write_csv(cell_windows, file.path(data_dir,"coffee_seasonal_source_cell_indices.csv"))
    report$source_cell_window_rows <<- nrow(cell_windows)
    report$incomplete_source_cell_windows <<- sum(!cell_windows$temperature_complete | !cell_windows$precipitation_complete)

    validation <- list()
    bkey <- paste(benchmark$cell_id, benchmark$climate_year, sep="|")
    for (lag in 1:2) for (m in primary) {
      j <- match(paste(d$MET_cell_id, d$year-lag, sep="|"), bkey)
      assert(!anyNA(j), "Missing annual benchmark keys.")
      old <- number(d[[paste0("clim_",m,"_lag",lag)]], "Annual panel value")
      new <- benchmark[[m]][j]
      same <- (is.na(old) & is.na(new)) | (!is.na(old) & !is.na(new) & abs(old-new)<=1e-8)
      same[is.na(same)] <- FALSE
      err <- if (any(is.finite(old) & is.finite(new))) max(abs(old-new),na.rm=TRUE) else NA_real_
      validation[[length(validation)+1L]] <- data.frame(metric=m,lag_years=lag,
        compared_rows=nrow(d),all_values_agree=all(same),maximum_absolute_difference=err)
    }
    validation <- bind(validation)
    write_csv(validation, file.path(out_dir,"coffee_seasonal_annual_benchmark_check.csv"))
    assert(all(validation$all_values_agree), "New daily engine differs from the step19 annual inputs; inspect benchmark check.")
    report$annual_benchmarks_passed <<- TRUE

    core <- c("municipality_code","municipality","state","species","year","MET_cell_id", "included_step20",
      "production_t","area_harvested_ha","analysis_yield_kg_ha","log_yield")
    need(d, core, "Output panel")
    panel <- d[core]  # Earlier annual/LOO columns stay in the untouched step19 file.
    coverage_parts <- list(); ready_all <- rep(TRUE,nrow(d))
    for (wid in paste0("W",1:4)) {
      cw <- cell_windows[cell_windows$window_id == wid, , drop=FALSE]
      j <- match(paste(d$MET_cell_id,d$year,sep="|"), paste(cw$cell_id,cw$year,sep="|"))
      assert(!anyNA(j), paste("Missing seasonal keys:",wid))
      for (m in metrics) panel[[paste0("season_",wid,"_",m)]] <- cw[[m]][j]
      panel[[paste0("season_",wid,"_start")]] <- cw$start[j]
      panel[[paste0("season_",wid,"_end")]] <- cw$end[j]
      panel[[paste0("season_",wid,"_expected_days")]] <- cw$expected_days[j]
      ok <- stats::complete.cases(cw[j,primary,drop=FALSE])
      panel[[paste0("season_",wid,"_complete_primary")]] <- ok
      ready_all <- ready_all & ok
      for (sp in c("Arabica","Canephora")) for (yr in 2012:2024) {
        sel <- d$species == sp & d$year == yr
        eligible <- sel & d$included_step20
        complete <- eligible & ok
        denom <- sum(d$production_t[eligible]); denomA <- sum(d$area_harvested_ha[eligible])
        coverage_parts[[length(coverage_parts)+1L]] <- data.frame(species=sp,year=yr,window_id=wid,
          panel_rows=sum(sel),step20_eligible_rows=sum(eligible),complete_seasonal_rows=sum(complete),
          missing_among_eligible=sum(eligible & !ok),eligible_MET_cells=length(unique(d$MET_cell_id[eligible])),
          production_coverage_pct_within_step20=if(denom>0) 100*sum(d$production_t[complete])/denom else NA_real_,
          area_coverage_pct_within_step20=if(denomA>0) 100*sum(d$area_harvested_ha[complete])/denomA else NA_real_,
          observed_phase_alignment=FALSE)
      }
    }
    panel$all_four_windows_complete <- ready_all
    panel$step20_eligible_with_complete_windows <- d$included_step20 & ready_all
    panel$climate_source <- "NASA POWER"
    panel$time_standard <- "UTC"
    panel$seasonal_windows_are_observed_phenology <- FALSE
    panel$local_harvest_alignment_verified <- FALSE
    panel$phase_interpretation <- ifelse(panel$species=="Arabica",
      "Literature-informed candidate dates; regional timing not verified", "Calendar dates only; no Arabica phenological stages assigned")
    assert(identical(key(panel),key(d)), "Row identity/order changed in output panel.")
    report$inherited_eligible_rows_with_complete_windows <<- sum(panel$step20_eligible_with_complete_windows)
    write_csv(bind(coverage_parts),file.path(out_dir,"coffee_seasonal_climate_coverage.csv"))
    write_csv(panel,file.path(data_dir,"coffee_yield_seasonal_climate_panel_2012_2024.csv"))
    write_rds(panel,file.path(data_dir,"coffee_yield_seasonal_climate_panel_2012_2024.rds"))
    notes <- c("COFFE-STUDY | STEP21 | CANDIDATE SEASONAL PANEL",paste("Version:",version),
      paste("Executed:",format(Sys.time(),usetz=TRUE)),
      "Source: unchanged step16 normalized NASA MET cell files. UTC days. No radiation/ET0/VPD requested or recalculated.",
      "Four fixed, non-overlapping calendar windows. Design is motivated by Arabica literature, not selected by association significance.",
      "W1 Sep(t-2)-Mar(t-1); W2 Apr-Aug(t-1); W3 Sep-Dec(t-1); W4 Jan-Mar(t). t is PAM reporting year.",
      "The design omits other potentially relevant months and does not define an exhaustive biological cycle.",
      "Arabica phenological names denote literature-informed hypotheses, not observations or locally verified dates.",
      "For Canephora, only calendar labels apply. Do not present a shared universal phase calendar across species/regions/genotypes.",
      "Crasque et al. (2025) is evidence of variable Canephora timing at a single ES location, not a national phenology map.",
      "W4 uses current-year weather; possible postharvest overlap remains flagged. No model is declared operational or causal.",
      "All step20 records and eligibility flags are preserved. Additional completeness is exported without silently changing the earlier sample.",
      "Means use all days, with actual leap-year lengths. Totals are not rescaled or annualized.",
      "Longest sequences can span internal month/year boundaries, but are truncated at the explicitly defined outer window boundaries.",
      "Temperature thresholds are the step13/17 descriptive thresholds, not species-specific injury thresholds.",
      "A dry day has precipitation<1 mm; this is not measured root-zone drought or plant stress.",
      "All dependent daily values must pass source bounds and temperature ordering. Missing values remain NA.",
      "Annual benchmarks independently reproduce four existing indicators for exact t-1/t-2 keys before panel assembly.",
      "Many municipalities share a MET cell; expansion does not create independent weather observations.",
      "Earlier annual coefficients remain valid outputs of their stated design and are not superseded by a more favorable result.",
      "This step fits no regressions and makes no claim that shorter windows improve explanatory/predictive performance.",
      "Next estimation needs an explicit species/region timing protocol, sensitivity specifications and limited-cluster inference.",
      "Camargo AP, Camargo MBP (2001). Bragantia 60:65-68. DOI 10.1590/S0006-87052001000100008.",
      "https://www.ciiagro.sp.gov.br/monitoramentocafe/Fenologia.htm",
      "Crasque J et al. (2025). Agronomy Journal 117:e70103. DOI 10.1002/agj2.70103.")
    writeLines(notes,file.path(out_dir,"coffee_seasonal_climate_method_notes.txt"),useBytes=TRUE)
    assert(all(vapply(paths,hash,character(1)) == provenance$md5), "An upstream input changed during this run.")
    report$execution_status <<- if (all(ready_all)) "COMPLETED_CANDIDATE_SEASONAL_PANEL" else "COMPLETED_CANDIDATE_SEASONAL_PANEL_WITH_GAPS"
    save_report()
    message("\n",report$execution_status,"\nReports: ",out_dir,"\nSeasonal panel: ",data_dir)
    invisible(panel)
  }
  tryCatch(worker(),error=function(e) {
    report$error_if_incomplete <<- substr(conditionMessage(e),1,1500)
    try(save_report(),silent=TRUE)
    stop(conditionMessage(e),"\nPrevious inputs retained. Completed source-cell caches can be reused.",call.=FALSE)
  },interrupt=function(e) {
    report$execution_status <<- "INTERRUPTED_RESUMABLE"
    report$error_if_incomplete <<- "Interrupted by user; completed cell caches retained."
    try(save_report(),silent=TRUE)
    message("Interrupted. Run the same file to resume without new downloads.")
    invisible(NULL)
  })
}

if (toupper(Sys.getenv("COFFEE_STEP21_NO_AUTORUN", "FALSE")) != "TRUE") coffee_step21()
