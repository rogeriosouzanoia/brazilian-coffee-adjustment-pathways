# ======================================================================
# COFFE-STUDY | STEP 18 | FIGURE 3: CLIMATE EXPOSURE AND AREA WEIGHTING
# Run the COMPLETE file with Source in RStudio.
#
# Uses completed steps 11/17. NO climate or production data are downloaded.
# Does not rerun daily calculations, change missing-value rules or alter
# earlier files. Rechecks all spatial annual estimates from municipal annual
# indices and the ORIGINAL municipal weights, including explicit NA support.
#
# Figure 3: four annual anomaly panels using FIXED 2012-2015 area weights,
# and two period-contrast panels comparing fixed with annual area weights.
# VPD/ET0 are NOT silently replaced by available-weight diagnostics: their
# coverage is audited and their strict estimates remain as in step17.
# No climate attribution, crop-response regression or trend test is fitted.
#
# Optional settings (not needed for the normal project):
# COFFEE_STUDY_DIR               override the existing project root
# COFFEE_STEP18_VALIDATE_ONLY    TRUE skips figure rendering only
#
# Plotting documentation checked 2026-09-20:
# https://ggplot2.tidyverse.org/reference/ggplot.html
# https://patchwork.data-imaginist.com/reference/plot_layout.html
# https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/cairo.html
# ======================================================================

coffee_step18 <- function() {
  version <- "18_climate_exposure_figure_v1"
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  input_dir <- file.path(base_dir, "04_outputs", "climate_power_brazil_indices")
  out_dir <- file.path(base_dir, "04_outputs", "figure03_climate_exposure")
  fig_dir <- file.path(base_dir, "05_figures", "Figure03_climate_exposure")
  validate_only <- toupper(Sys.getenv("COFFEE_STEP18_VALIDATE_ONLY", "FALSE")) == "TRUE"
  years <- 1981:2024
  reference_years <- 1981:2010
  initial_years <- 2012:2015
  recent_years <- 2021:2024
  categories <- c("Total", "Arabica", "Canephora")
  shown_species <- c("Arabica", "Canephora")
  met_figure <- c("tmean_c", "hot_days_ge32", "precipitation_total_mm", "longest_dry_spell_days")
  solar_metrics <- c("solar_mean_mj_m2_day", "et0_total_mm",
                     "et0_no_lower_cloud_bound_total_mm", "p_minus_et0_mm")
  for (p in c(out_dir, fig_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create: ", p, call. = FALSE)
  }
  lock <- file.path(out_dir, ".step18_running")
  if (!dir.create(lock, showWarnings = FALSE)) stop(
    "Another step18 session may be running. Confirm it has stopped before removing:\n", lock, call. = FALSE)
  writeLines(paste(Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  old_options <- options(scipen = 999)
  on.exit(options(old_options), add = TRUE)
  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(d, cols, label) {
    absent <- setdiff(cols, names(d))
    assert(!length(absent), paste(label, "missing:", paste(absent, collapse = ", ")))
  }
  read_csv <- function(p) {
    assert(file.exists(p), paste("Missing required input:", p))
    utils::read.csv(p, colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  num <- function(x, label, allow_na = TRUE) {
    original <- as.character(x)
    y <- suppressWarnings(as.numeric(original))
    assert(!any(!is.na(original) & is.na(y)) && all(is.na(y) | is.finite(y)) &&
      (allow_na || !anyNA(y)), paste("Invalid numeric input:", label))
    y
  }
  numeric_cols <- function(d, columns) {
    for (v in columns) d[[v]] <- num(d[[v]], v)
    d
  }
  bind <- function(parts) {
    d <- do.call(rbind, parts)
    rownames(d) <- NULL
    d
  }
  hash <- function(p) unname(tools::md5sum(p))
  write_csv <- function(d, p) {
    tmp <- tempfile("coffee18_", tmpdir = dirname(p), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(d, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, p, overwrite = TRUE), paste("Cannot write:", p))
  }
  get_check <- function(d, key) {
    need(d, c("check", "result"), "Summary")
    x <- d$result[d$check == key]
    assert(length(x) == 1L && !is.na(x), paste("Missing/ambiguous summary key:", key))
    x
  }
  key <- function(d, cols) do.call(paste, c(d[cols], sep = "|"))
  equal_na <- function(x, y, atol = 1e-8, rtol = 1e-12) {
    out <- is.na(x) & is.na(y)
    ok <- is.finite(x) & is.finite(y)
    out[ok] <- abs(x[ok] - y[ok]) <= atol + rtol * pmax(abs(x[ok]), abs(y[ok]))
    out
  }
  strict_mean <- function(x, n) if (length(x) == n && all(is.finite(x))) mean(x) else NA_real_
  max_finite <- function(x) if (any(is.finite(x))) max(x[is.finite(x)]) else NA_real_
  report <- list(execution_status = "INCOMPLETE", script_version = version,
    municipal_targets = NA_integer_, municipal_annual_rows = NA_integer_,
    weighted_rows_recomputed = 0L, weighted_checks_passed = FALSE,
    period_comparisons_verified = 0L, baselines_verified = 0L,
    maximum_weighted_value_difference = NA_real_,
    negative_vpd_municipal_days = NA_integer_, negative_vpd_unique_MET_cell_days = NA_integer_,
    strict_missing_exposure_rows = NA_integer_, figure_metrics = paste(met_figure, collapse = " | "),
    missing_estimates_imputed = FALSE, available_weight_diagnostic_used_as_primary = FALSE,
    daily_equations_recomputed = FALSE, source_data_downloaded = FALSE,
    prior_step_inputs_modified = FALSE, climate_trends_estimated = FALSE,
    climate_attribution_performed = FALSE, historical_boundaries_harmonized = FALSE,
    font_requested = "Times New Roman", font_used = NA_character_,
    png_status = "NOT_RENDERED", pdf_status = "NOT_RENDERED", error_if_incomplete = "None")
  save_summary <- function() write_csv(data.frame(check = names(report),
    result = vapply(report, as.character, character(1))), file.path(out_dir, "figure03_climate_summary.csv"))
  on.exit({
    try(save_summary(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step18.txt")), silent = TRUE)
  }, add = TRUE)

  worker <- function() {
    paths <- c(summary = file.path(input_dir, "coffee_power_brazil_indices_summary.csv"),
      exposure = file.path(input_dir, "coffee_power_brazil_weighted_exposure_annual.csv"),
      comparisons = file.path(input_dir, "coffee_power_brazil_exposure_period_comparison.csv"),
      baselines = file.path(input_dir, "coffee_power_brazil_fixed_exposure_baselines.csv"),
      annual = file.path(base_dir, "02_data_processed", "climate", "NASA_POWER", "brazil_indices",
                         "coffee_power_brazil_municipality_annual_indices.rds"),
      weights = file.path(base_dir, "02_data_processed", "climate", "coffee_climate_municipal_weights.csv"),
      quality = file.path(input_dir, "coffee_power_brazil_indices_quality.csv"),
      critical = file.path(input_dir, "coffee_power_brazil_indices_critical_days.csv"))
    assert(all(file.exists(paths)), paste("Missing input(s):", paste(paths[!file.exists(paths)], collapse = "\n")))
    original_hash <- vapply(paths, hash, character(1))
    write_csv(data.frame(role = names(paths), file = unname(paths), md5 = original_hash),
              file.path(out_dir, "figure03_input_provenance.csv"))
    s <- read_csv(paths[["summary"]])
    assert(get_check(s, "execution_status") %in% c("COMPLETED_NATIONAL_INDICES", "COMPLETED_NATIONAL_INDICES_WITH_GAPS"),
           "Step17 is incomplete; do not construct Figure 3 from partial municipal outputs.")
    assert(get_check(s, "numerical_tests_passed") == "TRUE", "Step17 numerical tests did not pass.")
    exp <- read_csv(paths[["exposure"]])
    need(exp, c("species", "weighting", "metric", "year", "unit", "estimate_strict",
      "available_weight_normalized_diagnostic", "observed_weight_fraction", "missing_positive_weight_municipalities",
      "reference_mean_1981_2010", "reference_valid_years", "anomaly_1981_2010", "source"), "Weighted exposure")
    exp <- numeric_cols(exp, c("year", "estimate_strict", "available_weight_normalized_diagnostic",
      "observed_weight_fraction", "missing_positive_weight_municipalities", "reference_mean_1981_2010",
      "reference_valid_years", "anomaly_1981_2010"))
    assert(all(exp$source == "NASA POWER"), "Unexpected climate source.")
    ekey_cols <- c("species", "weighting", "year", "metric")
    assert(!anyDuplicated(key(exp, ekey_cols)), "Duplicate exposure keys.")
    metrics <- unique(exp$metric)
    assert(all(met_figure %in% metrics), "Required Figure 3 indicators are missing.")
    annual <- as.data.frame(readRDS(paths[["annual"]]), stringsAsFactors = FALSE)
    need(annual, c("municipality_code", "year", "MET_cell_id", metrics), "Municipal annual indices")
    annual$municipality_code <- as.character(annual$municipality_code)
    annual <- numeric_cols(annual, c("year", metrics))
    codes <- sort(unique(annual$municipality_code))
    assert(all(grepl("^[0-9]{7}$", codes)) && !anyNA(codes), "Invalid municipal codes.")
    assert(!anyDuplicated(key(annual, c("municipality_code", "year"))), "Duplicate local annual keys.")
    assert(setequal(annual$year, years) && nrow(annual) == length(codes) * length(years), "Incomplete annual index grid.")
    assert(length(codes) == as.integer(get_check(s, "municipalities_processed")), "Municipal count differs from step17.")
    weights <- read_csv(paths[["weights"]])
    wc <- c("baseline_area_weight", "dynamic_area_weight", "baseline_production_weight", "dynamic_production_weight")
    need(weights, c("municipality_code", "year", "species", wc), "Original municipal weights")
    weights <- numeric_cols(weights, c("year", wc))
    assert(!anyDuplicated(key(weights, c("municipality_code", "species", "year"))), "Duplicate municipal weights.")
    assert(setequal(weights$municipality_code, codes) && setequal(weights$year, 2012:2024) &&
      setequal(weights$species, categories) && nrow(weights) == length(codes) * 13L * 3L, "Incomplete weight grid.")
    assert(all(is.finite(as.matrix(weights[wc]))) && all(as.matrix(weights[wc]) >= 0), "Invalid weights.")
    wb <- split(weights, paste(weights$species, weights$year, sep = "_"))
    wb <- lapply(wb, function(z) {
      k <- match(codes, z$municipality_code)
      assert(!anyNA(k), "Missing weights for a target municipality.")
      z[k, , drop = FALSE]
    })
    # Fixed weight columns must really be fixed, not redefined each year.
    for (sp in categories) for (yr in 2012:2024) {
      a <- wb[[paste(sp, 2012, sep = "_")]]
      b <- wb[[paste(sp, yr, sep = "_")]]
      for (v in c("baseline_area_weight", "baseline_production_weight"))
        assert(all(equal_na(a[[v]], b[[v]], atol = 1e-12)), "Fixed weights change across years.")
    }
    report$municipal_targets <<- length(codes)
    report$municipal_annual_rows <<- nrow(annual)
    message("Rechecking all weighted estimates from local annual indices and original municipal weights...")
    recomputed <- list()
    for (yr in years) {
      ay <- annual[annual$year == yr, , drop = FALSE]
      at <- match(codes, ay$municipality_code)
      assert(!anyNA(at), paste("Missing annual municipal index in", yr))
      x <- as.matrix(ay[at, metrics, drop = FALSE])
      ok <- is.finite(x)
      x0 <- x
      x0[!ok] <- 0 # algebraic numerator only; invalid estimates remain NA
      for (sp in categories) {
        ref <- wb[[paste(sp, 2012, sep = "_")]]
        specs <- list(Fixed_2012_2015_area = ref$baseline_area_weight,
                      Fixed_2012_2015_production = ref$baseline_production_weight)
        if (yr >= 2012) {
          dy <- wb[[paste(sp, yr, sep = "_")]]
          specs$Annual_area <- dy$dynamic_area_weight
          specs$Annual_production <- dy$dynamic_production_weight
        }
        for (wt in names(specs)) {
          w <- specs[[wt]]
          assert(abs(sum(w) - 1) < 1e-8, "Weights do not sum to one.")
          numerator <- colSums(sweep(x0, 1L, w, "*"))
          observed <- colSums(sweep(ok, 1L, w, "*"))
          missing <- colSums(!ok & (w > 0))
          est <- numerator / sum(w)
          est[missing > 0L] <- NA_real_
          diag <- ifelse(observed > 0, numerator / observed, NA_real_)
          recomputed[[length(recomputed) + 1L]] <- data.frame(species = sp, weighting = wt, year = yr,
            metric = metrics, estimate_strict = est, available_weight_normalized_diagnostic = diag,
            observed_weight_fraction = observed / sum(w), missing_positive_weight_municipalities = missing)
        }
      }
    }
    recalculated <- bind(recomputed)
    k <- match(key(exp, ekey_cols), key(recalculated, ekey_cols))
    assert(nrow(exp) == nrow(recalculated) && !anyNA(k), "Exposure combinations differ from local reconstruction.")
    recalculated <- recalculated[k, , drop = FALSE]
    audit <- exp[ekey_cols]
    for (v in c("estimate_strict", "available_weight_normalized_diagnostic", "observed_weight_fraction",
                "missing_positive_weight_municipalities")) {
      audit[[paste0(v, "_difference")]] <- recalculated[[v]] - exp[[v]]
      audit[[paste0(v, "_verified")]] <- equal_na(recalculated[[v]], exp[[v]])
    }
    write_csv(audit, file.path(out_dir, "figure03_weighted_exposure_verification.csv"))
    assert(all(as.matrix(audit[grepl("_verified$", names(audit))])), "Reweighted estimates disagree. Audit saved; plotting stopped.")
    report$weighted_rows_recomputed <<- nrow(audit)
    report$weighted_checks_passed <<- TRUE
    report$maximum_weighted_value_difference <<- max_finite(abs(audit$estimate_strict_difference))
    report$strict_missing_exposure_rows <<- sum(!is.finite(exp$estimate_strict))

    # Check the published four-year comparisons without substituting diagnostic values.
    cp <- read_csv(paths[["comparisons"]])
    cp <- numeric_cols(cp, c("baseline_mean", "recent_mean", "absolute_change", "baseline_valid_years", "recent_valid_years"))
    ck <- c("species", "weighting", "metric")
    assert(!anyDuplicated(key(cp, ck)), "Duplicate period comparison keys.")
    eg <- split(exp, key(exp, ck))
    pa <- vector("list", nrow(cp))
    for (i in seq_len(nrow(cp))) {
      z <- eg[[key(cp[i, , drop = FALSE], ck)]]
      b <- z$estimate_strict[z$year %in% initial_years]
      r <- z$estimate_strict[z$year %in% recent_years]
      bm <- strict_mean(b, 4L); rm <- strict_mean(r, 4L)
      pa[[i]] <- data.frame(cp[i, ck, drop = FALSE], baseline_mean_recomputed = bm,
        recent_mean_recomputed = rm, change_recomputed = rm - bm,
        verified = all(equal_na(c(bm, rm, rm-bm, sum(is.finite(b)), sum(is.finite(r))),
          unlist(cp[i, c("baseline_mean", "recent_mean", "absolute_change", "baseline_valid_years", "recent_valid_years")], use.names = FALSE))))
    }
    pa <- bind(pa)
    write_csv(pa, file.path(out_dir, "figure03_period_verification.csv"))
    assert(all(pa$verified), "Period comparisons disagree with strict annual estimates.")
    report$period_comparisons_verified <<- nrow(pa)
    ba <- read_csv(paths[["baselines"]])
    ba <- numeric_cols(ba, c("valid_years", "reference_mean"))
    assert(!anyDuplicated(key(ba, ck)), "Duplicate baseline keys.")
    bv <- vector("list", nrow(ba))
    for (i in seq_len(nrow(ba))) {
      z <- eg[[key(ba[i, , drop = FALSE], ck)]]
      v <- z$estimate_strict[z$year %in% reference_years]
      bm <- strict_mean(v, 30L)
      bv[[i]] <- data.frame(ba[i, ck, drop = FALSE], reference_recomputed = bm,
        valid_years_recomputed = sum(is.finite(v)), verified =
          all(equal_na(c(bm, sum(is.finite(v))), c(ba$reference_mean[i], ba$valid_years[i]))) &&
          all(equal_na(z$reference_mean_1981_2010, rep(bm, nrow(z)))) &&
          all(equal_na(z$anomaly_1981_2010, z$estimate_strict - bm)))
    }
    bv <- bind(bv)
    write_csv(bv, file.path(out_dir, "figure03_baseline_verification.csv"))
    assert(all(bv$verified), "Baseline means/anomalies disagree.")
    report$baselines_verified <<- nrow(bv)

    # Expose missing spatial support, separating structural radiation years.
    coverage <- bind(lapply(eg, function(z) {
      start <- if (z$metric[1L] %in% solar_metrics) 1984L else 1981L
      a <- z[z$year >= start, , drop = FALSE]
      p <- a[a$year %in% c(initial_years, recent_years), , drop = FALSE]
      data.frame(z[1L, ck, drop = FALSE], evaluated_first_year = min(a$year),
        evaluated_last_year = max(a$year), years_evaluated = nrow(a),
        strict_valid_years = sum(is.finite(a$estimate_strict)),
        minimum_observed_weight_pct = 100 * min(a$observed_weight_fraction),
        maximum_observed_weight_pct = 100 * max(a$observed_weight_fraction),
        comparison_years = nrow(p), strict_valid_comparison_years = sum(is.finite(p$estimate_strict)),
        minimum_comparison_weight_pct = 100 * min(p$observed_weight_fraction),
        structural_pre1984_rows = sum(z$year < start),
        diagnostic_substituted = FALSE)
    }))
    write_csv(coverage, file.path(out_dir, "figure03_coverage_audit.csv"))
    q <- read_csv(paths[["quality"]]); cr <- read_csv(paths[["critical"]])
    need(q, c("municipality_code", "MET_cell_id", "negative_vpd_days"), "Municipal quality")
    need(cr, c("municipality_code", "date", "negative_vpd"), "Critical day table")
    assert(!anyDuplicated(q$municipality_code), "Duplicate municipal quality codes.")
    negatives <- cr[cr$negative_vpd == "TRUE", , drop = FALSE]
    negatives$MET_cell_id <- q$MET_cell_id[match(negatives$municipality_code, q$municipality_code)]
    assert(!anyNA(negatives$MET_cell_id), "A critical day cannot be linked to its MET cell.")
    report$negative_vpd_municipal_days <<- nrow(negatives)
    report$negative_vpd_unique_MET_cell_days <<- nrow(unique(negatives[c("MET_cell_id", "date")]))
    write_csv(unique(negatives[c("MET_cell_id", "date")]), file.path(out_dir, "figure03_negative_vpd_unique_cell_days.csv"))

    # Difference of the two exposure contrasts. This is descriptive composition,
    # NOT a controlled counterfactual, proof of adaptation or a warming avoidance estimate.
    effect <- list()
    for (sp in categories) for (m in metrics) {
      fixed <- cp[cp$species == sp & cp$metric == m & cp$weighting == "Fixed_2012_2015_area", , drop = FALSE]
      dyn <- cp[cp$species == sp & cp$metric == m & cp$weighting == "Annual_area", , drop = FALSE]
      assert(nrow(fixed) == 1L && nrow(dyn) == 1L, "Missing area-weight period comparison.")
      effect[[length(effect)+1L]] <- data.frame(species = sp, metric = m, unit = fixed$unit,
        fixed_area_change = fixed$absolute_change, annual_area_change = dyn$absolute_change,
        difference_between_changes = dyn$absolute_change - fixed$absolute_change,
        baseline_period = "2012 to 2015", recent_period = "2021 to 2024",
        causal_adaptation_effect = FALSE)
    }
    effect <- bind(effect)
    write_csv(effect, file.path(out_dir, "figure03_area_weight_contrasts.csv"))
    selected <- cp[cp$species %in% categories & cp$weighting %in% c("Fixed_2012_2015_area", "Annual_area") &
      cp$metric %in% met_figure, , drop = FALSE]
    write_csv(selected, file.path(out_dir, "figure03_selected_period_changes.csv"))
    ts <- exp[exp$species %in% shown_species & exp$weighting == "Fixed_2012_2015_area" &
      exp$metric %in% met_figure, , drop = FALSE]
    assert(nrow(ts) == 44L * 2L * length(met_figure) && all(is.finite(ts$anomaly_1981_2010)) &&
      all(ts$missing_positive_weight_municipalities == 0), "A chosen primary figure series is incomplete. No diagnostic replacement allowed.")
    write_csv(ts, file.path(out_dir, "figure03_annual_anomaly_plot_data.csv"))
    caption <- paste(
      "Figure 3. Climate exposure of Brazilian coffee using municipal reference samples and harvested-area weights.",
      "(a-d) Annual anomalies of mean temperature, days with Tmax >= 32 degrees C, annual precipitation, and each municipality's longest annual dry sequence, relative to 1981 to 2010.",
      "Fixed weights are normalized mean municipal harvested areas from 2012 to 2015, separately for Arabica and Canephora. Thus the historical series describe climate at a fixed reference distribution, not the annual historical geography of coffee.",
      "Climate inputs were sampled from NASA POWER source cells for 1876 municipal reference points; multiple municipalities share meteorological cells. Daily and annual indices precede spatial weighting.",
      "A dry day has precipitation <1 mm. The weighted mean of local longest sequences is not a single spatially synchronized national drought.",
      "(e-f) Differences between the means of annual exposures in 2021 to 2024 and 2012 to 2015 for temperature and days with Tmax >=32 degrees C, under fixed area weights (circles) or annual area weights (triangles).",
      "The connectors join two weighting estimates and are not confidence intervals. Differences between weighting schemes are compositional associations, not estimates of adaptation effectiveness or anthropogenic climate attribution.",
      "The four primary meteorological indicators have complete positive-weight support for these panels. Strict VPD and ET0 series retain gaps and are not substituted by available-weight-normalized diagnostics.",
      "Historical municipal boundaries and actual field locations are not harmonized/measured. Annual differences and threshold counts are not crop injury estimates.")
    writeLines(caption, file.path(out_dir, "figure03_caption_draft.txt"))
    notes <- c("COFFE-STUDY | STEP18 | CLIMATE EXPOSURE FIGURE",
      paste("Executed:", format(Sys.time(), usetz = TRUE)),
      "All 8892 exposure rows are reweighted from municipal annual indices and original step11 weights when the unaltered study plan is used.",
      "All strict values, available-weight diagnostics, support fractions and missing positive weights are compared separately.",
      "Period averages and 1981-2010 fixed-weight baselines are recomputed from strict exposure only.",
      "The 404 MB monthly table and daily weather are not reread; daily equations are not independently recomputed here.",
      "Panels a-d use separate species baselines and the same horizontal time axis. Each indicator has its own physical units; species share its scale.",
      "Panels e-f compare means of annual exposure, not a pooled multiyear area-weight mean.",
      "Annual area-weighted baseline means need not equal fixed-weight baseline means: annual weights and annual climate can covary.",
      "Annual minus fixed contrasts therefore include the change of the weighting contrast between both windows, not only a recent-period difference.",
      "No source observation, missing-value rule or earlier output is replaced. VPD/ET0 coverage is a separate result, not silently fixed.",
      "No regression, p-value, trend estimate, field-altitude effect or climate attribution is produced.",
      "https://ggplot2.tidyverse.org/reference/ggplot.html",
      "https://patchwork.data-imaginist.com/reference/plot_layout.html",
      "https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/cairo.html")
    writeLines(notes, file.path(out_dir, "figure03_method_notes.txt"))

    if (!validate_only) {
      for (pkg in c("ggplot2", "patchwork", "systemfonts")) {
        if (!requireNamespace(pkg, quietly = TRUE)) utils::install.packages(pkg, repos = "https://cloud.r-project.org")
        assert(requireNamespace(pkg, quietly = TRUE), paste("Required plotting package is unavailable:", pkg))
      }
      fonts <- unique(systemfonts::system_fonts()$family)
      font <- if ("Times New Roman" %in% fonts) "Times New Roman" else "serif"
      if (font != "Times New Roman") warning("Times New Roman unavailable. Using serif and recording the fallback.", call. = FALSE)
      report$font_used <<- font
      theme_rogerio <- function(base_size = 14) {
        ggplot2::theme_minimal(base_size = base_size, base_family = font) +
          ggplot2::theme(panel.grid = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black", linewidth = 0.45),
            axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.4),
            axis.ticks.length = grid::unit(0.14, "cm"),
            axis.text = ggplot2::element_text(colour = "black"),
            axis.title = ggplot2::element_text(colour = "black"),
            legend.position = "top", legend.title = ggplot2::element_blank(),
            legend.key.width = grid::unit(1.1, "cm"),
            strip.text = ggplot2::element_text(face = "bold"),
            panel.spacing = grid::unit(0.8, "lines"),
            plot.title = ggplot2::element_text(face = "bold", size = 14),
            plot.subtitle = ggplot2::element_text(size = 11),
            plot.margin = ggplot2::margin(8, 12, 8, 8))
      }
      ts$species <- factor(ts$species, levels = shown_species)
      series_panel <- function(m, title, ylab, show_legend = FALSE) {
        d <- ts[ts$metric == m, , drop = FALSE]
        yl <- range(c(0, d$anomaly_1981_2010))
        pad <- max(diff(yl) * 0.08, 0.01)
        ggplot2::ggplot(d, ggplot2::aes(x = year, y = anomaly_1981_2010, linetype = species, group = species)) +
          ggplot2::geom_hline(yintercept = 0, colour = "grey65", linetype = "dotted", linewidth = 0.4) +
          ggplot2::geom_line(linewidth = 0.65, show.legend = show_legend) +
          ggplot2::scale_linetype_manual(values = c(Arabica = "solid", Canephora = "longdash"), drop = FALSE) +
          ggplot2::scale_x_continuous(limits = c(1981, 2024), breaks = c(1981, 1990, 2000, 2010, 2020, 2024),
            expand = ggplot2::expansion(mult = c(0.015, 0.02))) +
          ggplot2::scale_y_continuous(limits = yl + c(-pad, pad), breaks = scales::breaks_pretty(n = 5)) +
          ggplot2::labs(title = title, x = "Year", y = ylab, linetype = NULL) + theme_rogerio()
      }
      p1 <- series_panel("tmean_c", "a  Mean temperature", expression("Anomaly (" * degree * "C)"), TRUE)
      p2 <- series_panel("hot_days_ge32", "b  Days with Tmax \u2265 32 \u00b0C", "Anomaly (days per year)")
      p3 <- series_panel("precipitation_total_mm", "c  Annual precipitation", "Anomaly (mm per year)")
      p4 <- series_panel("longest_dry_spell_days", "d  Longest annual dry sequence", "Anomaly in weighted local duration (days)")
      contrast_panel <- function(m, title, xlab, decimals, show_legend = FALSE) {
        d <- selected[selected$species %in% shown_species & selected$metric == m, , drop = FALSE]
        d$y <- ifelse(d$species == "Arabica", 2, 1)
        d$scheme <- factor(ifelse(d$weighting == "Fixed_2012_2015_area", "Fixed area", "Annual area"),
                           levels = c("Fixed area", "Annual area"))
        d$label <- sprintf(paste0("%+.", decimals, "f"), d$absolute_change)
        d$label_y <- d$y + ifelse(d$scheme == "Fixed area", 0.17, -0.17)
        e <- effect[effect$species %in% shown_species & effect$metric == m, , drop = FALSE]
        e$y <- ifelse(e$species == "Arabica", 2, 1)
        xr <- range(c(0, d$absolute_change))
        pad <- max(diff(xr) * 0.12, 0.01)
        ggplot2::ggplot(d, ggplot2::aes(x = absolute_change, y = y)) +
          ggplot2::geom_vline(xintercept = 0, colour = "grey65", linetype = "dotted", linewidth = 0.4) +
          ggplot2::geom_segment(data = e, ggplot2::aes(x = fixed_area_change, xend = annual_area_change, y = y, yend = y),
            inherit.aes = FALSE, colour = "grey60", linewidth = 0.6) +
          ggplot2::geom_point(ggplot2::aes(shape = scheme), size = 2.8, show.legend = show_legend) +
          ggplot2::geom_text(ggplot2::aes(y = label_y, label = label), family = font, size = 3.7, show.legend = FALSE) +
          ggplot2::scale_shape_manual(values = c("Fixed area" = 16, "Annual area" = 17), drop = FALSE) +
          ggplot2::scale_y_continuous(limits = c(0.6, 2.4), breaks = c(1,2), labels = c("Canephora", "Arabica"), expand = c(0,0)) +
          ggplot2::scale_x_continuous(limits = xr + c(-pad * 0.3, pad), breaks = scales::breaks_pretty(n = 5)) +
          ggplot2::labs(title = title, subtitle = "2021 to 2024 minus 2012 to 2015", x = xlab, y = NULL, shape = NULL) +
          theme_rogerio()
      }
      p5 <- contrast_panel("tmean_c", "e  Temperature change by area weighting", expression("Change (" * degree * "C)"), 2L, TRUE)
      p6 <- contrast_panel("hot_days_ge32", "f  Hot-day change by area weighting", "Change (days per year)", 1L)
      figure <- patchwork::wrap_plots(list(p1, p2, p3, p4, p5, p6), ncol = 2,
        heights = c(1, 1, 0.82), guides = "collect") +
        patchwork::plot_annotation(title = "Climate exposure of Brazilian coffee",
          subtitle = "NASA POWER | Municipal reference samples weighted by harvested area",
          caption = paste("a-d: fixed 2012 to 2015 area weights; anomalies relative to 1981 to 2010.",
            "e-f: alternative area-weight contrasts; connectors are not confidence intervals.",
            "Regional climate proxies, not farm observations or climate-attributable crop impacts.", sep = "\n"),
          theme = ggplot2::theme(plot.title = ggplot2::element_text(family = font, face = "bold", size = 19),
            plot.subtitle = ggplot2::element_text(family = font, size = 12),
            plot.caption = ggplot2::element_text(family = font, size = 10, hjust = 0),
            plot.margin = ggplot2::margin(12, 12, 12, 12)))
      figure <- figure & ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank())
      png_path <- file.path(fig_dir, "Figure03_coffee_climate_exposure.png")
      if (requireNamespace("ragg", quietly = TRUE)) {
        ggplot2::ggsave(png_path, figure, device = ragg::agg_png, width = 14, height = 12.5, units = "in", dpi = 300, bg = "white")
      } else {
        ggplot2::ggsave(png_path, figure, device = "png", width = 14, height = 12.5, units = "in", dpi = 300, bg = "white")
      }
      report$png_status <<- "Saved"
      if (isTRUE(capabilities("cairo"))) {
        report$pdf_status <<- tryCatch({
          ggplot2::ggsave(file.path(fig_dir, "Figure03_coffee_climate_exposure.pdf"), figure,
            device = grDevices::cairo_pdf, width = 14, height = 12.5, units = "in", family = font, bg = "white")
          "Saved"
        }, error = function(e) paste("NOT_SAVED:", conditionMessage(e)))
      } else report$pdf_status <<- "NOT_SAVED: Cairo unavailable; PNG retained"
      if (interactive()) print(figure)
    }
    assert(all(vapply(paths, hash, character(1)) == original_hash), "A source input changed during this run.")
    report$execution_status <<- if (validate_only) "COMPLETED_CHECKS_ONLY" else "COMPLETED_FIGURE03_WITH_COVERAGE_CAVEATS"
    save_summary()
    message("\n", report$execution_status, "\nTables: ", out_dir, "\nFigure: ", fig_dir,
      "\nVPD/ET0 gaps were preserved. No download or climate-attribution analysis was performed.")
    invisible(report)
  }
  tryCatch(worker(), error = function(e) {
    report$execution_status <<- "INCOMPLETE"
    report$error_if_incomplete <<- substr(conditionMessage(e), 1, 1600)
    try(save_summary(), silent = TRUE)
    stop(conditionMessage(e), "\nPrevious-step inputs are unchanged. See: ",
      file.path(out_dir, "figure03_climate_summary.csv"), call. = FALSE)
  })
}

coffee_step18()
