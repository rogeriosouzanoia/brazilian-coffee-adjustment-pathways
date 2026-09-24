# ======================================================================
# COFFE-STUDY | STEP 14 | SYNTHESIS OF THE SIX-POINT CLIMATE PILOT
# Run the COMPLETE file with Source in RStudio.
#
# Reads the step13 tables, independently checks their aggregation and
# displays differences between 2012-2015 and 2021-2024. No climate download.
# This is a PILOT diagnostic, not an additional main figure of the paper.
# Original data, earlier figures and the step13 missing-data rules are kept.
#
# No trends, significance tests, yield effects, field frost, soil-water stress,
# altitude migration or national climate means are inferred by this script.
# Point labels are municipalities, not farms or area-averaged coffee weather.
# The two periods are inside the same documented CERES radiation epoch.
#
# Optional settings, normally leave unchanged:
#   COFFEE_STUDY_DIR: project directory override.
#   COFFEE_PILOT14_TABLES_ONLY=TRUE: run checks/tables without graphic packages.
# ======================================================================

coffee_pilot_step14 <- function() {
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  input_dir <- file.path(base_dir, "04_outputs", "climate_nasa_power_indices")
  out_dir <- file.path(base_dir, "04_outputs", "climate_pilot_synthesis")
  figure_dir <- file.path(base_dir, "05_figures", "Climate_pilot_diagnostics")
  tables_only <- identical(toupper(Sys.getenv("COFFEE_PILOT14_TABLES_ONLY", "FALSE")), "TRUE")
  years <- 1981:2024
  initial <- 2012:2015
  recent <- 2021:2024
  preferred_codes <- c("3148103", "2902906", "3201159", "3204351", "2915601", "1100320")
  tolerance <- 1e-6
  for (p in c(out_dir, figure_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create directory: ", p, call. = FALSE)
  }
  old_options <- options(scipen = 999)
  on.exit(options(old_options), add = TRUE)
  assert <- function(ok, text) if (!isTRUE(ok)) stop(text, call. = FALSE)
  need <- function(x, cols, label) {
    absent <- setdiff(cols, names(x))
    assert(!length(absent), paste(label, "is missing:", paste(absent, collapse = ", ")))
  }
  read_csv <- function(path) {
    assert(file.exists(path), paste("Required file not found:", path))
    utils::read.csv(path, colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  number <- function(x, label, allow_na = TRUE) {
    v <- suppressWarnings(as.numeric(x))
    assert(!any(!is.na(x) & is.na(v)) && all(is.na(v) | is.finite(v)) &&
      (allow_na || !anyNA(v)), paste("Invalid numeric column:", label))
    v
  }
  logical_column <- function(x, label) {
    assert(!anyNA(x) && all(x %in% c("TRUE", "FALSE")), paste("Invalid logical column:", label))
    x == "TRUE"
  }
  bind <- function(parts) {
    if (!length(parts)) return(data.frame())
    ans <- do.call(rbind, parts)
    rownames(ans) <- NULL
    ans
  }
  strict_mean <- function(x) if (length(x) && all(is.finite(x))) mean(x) else NA_real_
  same <- function(x, y) {
    (is.na(x) & is.na(y)) |
      (!is.na(x) & !is.na(y) & abs(x - y) <= tolerance)
  }
  write_csv <- function(x, path) {
    temp <- tempfile("coffee14_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(temp), add = TRUE)
    utils::write.csv(x, temp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(temp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  hash <- function(path) unname(tools::md5sum(path))
  report <- list(execution_status = "INCOMPLETE", script_version = "14_pilot_synthesis_v1",
    points = 0L, annual_records = 0L, monthly_records = 0L,
    period_comparisons_verified = 0L, monthly_annual_checks_verified = 0L,
    baseline_checks_verified = 0L, numerical_tests_passed = FALSE,
    negative_vpd_days = NA, cloud_ratio_lower_bound_days = NA,
    cloud_ratio_upper_bound_days = NA, missing_baseline_rows = NA,
    maximum_annual_et0_sensitivity_mm = NA, maximum_annual_et0_sensitivity_pct = NA,
    source = "NASA POWER", scope = "SIX_MUNICIPAL_REFERENCE_POINTS_ONLY",
    climate_data_downloaded = FALSE, original_inputs_modified = FALSE,
    daily_equations_recomputed = FALSE, national_mean_calculated = FALSE,
    climate_trends_estimated = FALSE, causal_attribution_performed = FALSE,
    font_used = "Not rendered", png_status = "Not rendered", pdf_status = "Not rendered",
    error_if_incomplete = "None")
  save_report <- function() write_csv(data.frame(check = names(report),
    result = vapply(report, as.character, character(1))),
    file.path(out_dir, "coffee_climate_pilot_synthesis_summary.csv"))
  on.exit({
    try(save_report(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()),
      file.path(out_dir, "sessionInfo_step14.txt")), silent = TRUE)
  }, add = TRUE)

  worker <- function() {
    filenames <- c(annual = "coffee_power_indices_annual.csv",
      monthly = "coffee_power_indices_monthly.csv",
      comparison = "coffee_power_indices_period_comparison.csv",
      baseline = "coffee_power_indices_baseline.csv",
      quality = "coffee_power_indices_quality.csv",
      flags = "coffee_power_indices_flagged_days.csv",
      tests = "coffee_power_indices_numerical_tests.csv",
      dictionary = "coffee_power_indices_dictionary.csv")
    paths <- setNames(file.path(input_dir, filenames), names(filenames))
    assert(all(file.exists(paths)), paste("Missing step13 output(s):",
      paste(paths[!file.exists(paths)], collapse = "\n")))
    provenance <- data.frame(role = names(paths), file = unname(paths),
      md5 = vapply(paths, hash, character(1)), stringsAsFactors = FALSE)
    write_csv(provenance, file.path(out_dir, "coffee_climate_pilot_synthesis_provenance.csv"))
    a <- read_csv(paths[["annual"]]); m <- read_csv(paths[["monthly"]])
    p <- read_csv(paths[["comparison"]]); b <- read_csv(paths[["baseline"]])
    q <- read_csv(paths[["quality"]]); f <- read_csv(paths[["flags"]])
    tests <- read_csv(paths[["tests"]]); dict <- read_csv(paths[["dictionary"]])
    need(dict, c("metric", "unit", "definition"), "Metric dictionary")
    metrics <- dict$metric
    assert(!anyNA(metrics) && !anyDuplicated(metrics), "Invalid metric dictionary.")
    need(a, c("municipality_code", "municipality", "year", "expected_days", "source",
      "time_standard", metrics), "Annual data")
    need(m, c("municipality_code", "year", "month", "expected_days", metrics), "Monthly data")
    need(p, c("municipality_code", "metric", "baseline_period", "recent_period",
      "baseline_period_mean", "recent_period_mean", "absolute_change", "all_required_years_valid"), "Period comparison")
    need(b, c("municipality_code", "metric", "reference_mean", "reference_valid_years"), "Baseline")
    need(q, c("municipality_code", "daily_rows", "negative_vpd_days",
      "cloud_ratio_lower_bound_days", "cloud_ratio_upper_bound_days"), "Quality report")
    need(f, c("municipality_code", "date", "flag_negative_vpd",
      "flag_cloud_ratio_lower_bound", "flag_cloud_ratio_upper_bound"), "Flagged days")
    need(tests, c("test", "calculated", "expected", "tolerance", "pass"), "Numerical tests")

    for (v in c("year", "expected_days", metrics)) a[[v]] <- number(a[[v]], paste("annual", v))
    for (v in c("year", "month", "expected_days", metrics)) m[[v]] <- number(m[[v]], paste("monthly", v))
    for (v in c("baseline_period_mean", "recent_period_mean", "absolute_change")) p[[v]] <- number(p[[v]], v)
    b$reference_mean <- number(b$reference_mean, "baseline mean")
    b$reference_valid_years <- number(b$reference_valid_years, "baseline valid years", FALSE)
    for (v in c("daily_rows", "negative_vpd_days", "cloud_ratio_lower_bound_days", "cloud_ratio_upper_bound_days"))
      q[[v]] <- number(q[[v]], paste("quality", v), FALSE)
    for (v in c("calculated", "expected", "tolerance")) tests[[v]] <- number(tests[[v]], v, FALSE)
    test_ok <- logical_column(tests$pass, "tests$pass") &
      abs(tests$calculated - tests$expected) <= tests$tolerance + 1e-12
    assert(all(test_ok), "At least one step13 numerical test failed.")
    report$numerical_tests_passed <<- TRUE
    assert(all(a$source == "NASA POWER") && all(a$time_standard == "UTC"), "Unexpected climate source or time standard.")
    codes <- unique(a$municipality_code)
    assert(setequal(codes, preferred_codes), "This diagnostic expects the original six pilot municipalities.")
    codes <- preferred_codes
    assert(!anyDuplicated(paste(a$municipality_code, a$year)), "Duplicate annual keys.")
    assert(!anyDuplicated(paste(m$municipality_code, m$year, m$month)), "Duplicate monthly keys.")
    assert(!anyDuplicated(paste(p$municipality_code, p$metric)), "Duplicate comparison keys.")
    assert(!anyDuplicated(paste(b$municipality_code, b$metric)), "Duplicate baseline keys.")
    assert(!anyDuplicated(paste(f$municipality_code, f$date)), "Duplicate flagged-day keys.")
    assert(!anyDuplicated(q$municipality_code) && setequal(q$municipality_code, codes), "Invalid quality-report membership.")
    for (code in codes) {
      assert(setequal(a$year[a$municipality_code == code], years), paste("Incomplete annual calendar:", code))
      for (yr in years) {
        z <- m[m$municipality_code == code & m$year == yr, , drop = FALSE]
        assert(setequal(z$month, 1:12) && nrow(z) == 12L, paste("Incomplete monthly calendar:", code, yr))
        expected <- as.integer(as.Date(paste0(yr + 1L, "-01-01")) - as.Date(paste0(yr, "-01-01")))
        assert(sum(z$expected_days) == expected, paste("Calendar length mismatch:", code, yr))
      }
    }
    report$points <<- length(codes); report$annual_records <<- nrow(a); report$monthly_records <<- nrow(m)

    # Recompute all reported four-year means. Do not infer a trend from them.
    checked_periods <- list()
    for (i in seq_len(nrow(p))) {
      z <- p[i, , drop = FALSE]
      assert(z$metric %in% metrics && z$baseline_period == "2012 to 2015" &&
        z$recent_period == "2021 to 2024", "Unexpected metric or comparison window.")
      local <- a[a$municipality_code == z$municipality_code, , drop = FALSE]
      v0 <- local[[z$metric]][match(initial, local$year)]
      v1 <- local[[z$metric]][match(recent, local$year)]
      b0 <- strict_mean(v0); b1 <- strict_mean(v1)
      ok <- same(b0, z$baseline_period_mean) && same(b1, z$recent_period_mean) && same(b1 - b0, z$absolute_change)
      valid <- all(is.finite(c(v0, v1)))
      ok <- ok && identical(valid, logical_column(z$all_required_years_valid, "comparison validity"))
      checked_periods[[i]] <- data.frame(municipality_code = z$municipality_code, metric = z$metric,
        initial_recomputed = b0, recent_recomputed = b1, difference_recomputed = b1 - b0,
        reported_difference = z$absolute_change, all_required_years_valid = valid,
        verified = ok, stringsAsFactors = FALSE)
    }
    cp <- bind(checked_periods)
    write_csv(cp, file.path(out_dir, "coffee_climate_period_verification.csv"))
    assert(nrow(cp) == length(codes) * length(metrics) && all(cp$verified), "Period means do not reproduce the annual data.")
    report$period_comparisons_verified <<- nrow(cp)

    # Only metrics with valid monthly-to-annual aggregation are checked here.
    # Longest spells and rolling five-day maxima must NOT be added across months.
    additive <- c("hot_days_ge30", "hot_days_ge32", "hot_days_ge35", "heat_excess35_c_days",
      "cold_days_lt2", "cold_days_lt0", "precipitation_total_mm", "dry_days_lt1", "wet_days_ge1",
      "rain_ge20_days", "rain_ge50_days", "et0_total_mm", "et0_no_lower_cloud_bound_total_mm", "p_minus_et0_mm")
    averaged <- c("tmean_c", "tmin_mean_c", "tmax_mean_c", "vpd_fao56_mean_kpa", "solar_mean_mj_m2_day", "rhmean_pct")
    parts <- list(); n <- 0L
    for (i in seq_len(nrow(a))) {
      ar <- a[i, , drop = FALSE]
      z <- m[m$municipality_code == ar$municipality_code & m$year == ar$year, , drop = FALSE]
      for (v in c(additive, averaged)) {
        x <- z[[v]]
        calc <- if (all(is.finite(x))) {
          if (v %in% additive) sum(x) else sum(x * z$expected_days) / sum(z$expected_days)
        } else NA_real_
        n <- n + 1L
        parts[[n]] <- data.frame(municipality_code = ar$municipality_code, year = ar$year,
          metric = v, annual_reported = ar[[v]], from_monthly = calc,
          verified = same(ar[[v]], calc), stringsAsFactors = FALSE)
      }
    }
    ma <- bind(parts)
    write_csv(ma, file.path(out_dir, "coffee_climate_monthly_annual_verification.csv"))
    assert(all(ma$verified), "Monthly and annual aggregations do not agree.")
    report$monthly_annual_checks_verified <<- nrow(ma)

    base_checks <- list()
    for (i in seq_len(nrow(b))) {
      z <- b[i, , drop = FALSE]
      assert(z$metric %in% metrics, "Unexpected baseline metric.")
      local <- a[a$municipality_code == z$municipality_code, , drop = FALSE]
      x <- local[[z$metric]][match(1981:2010, local$year)]
      calc <- strict_mean(x)
      anomaly_column <- paste0(z$metric, "_anomaly_1981_2010")
      need(local, anomaly_column, "Annual anomalies")
      anomaly <- number(local[[anomaly_column]], anomaly_column)
      ok <- same(calc, z$reference_mean) && sum(is.finite(x)) == z$reference_valid_years &&
        all(same(anomaly, local[[z$metric]] - calc))
      base_checks[[i]] <- data.frame(municipality_code = z$municipality_code, metric = z$metric,
        reference_valid_years = sum(is.finite(x)), reference_mean = calc,
        baseline_available = is.finite(calc), anomalies_verified = ok, stringsAsFactors = FALSE)
    }
    bc <- bind(base_checks)
    write_csv(bc, file.path(out_dir, "coffee_climate_baseline_verification.csv"))
    assert(all(bc$anomalies_verified), "Baseline/anomaly definitions do not match step13.")
    report$baseline_checks_verified <<- nrow(bc)
    report$missing_baseline_rows <<- sum(!bc$baseline_available)

    # Flag counts are checked; calculations with incomplete inputs stay missing.
    flag_map <- c(negative_vpd_days = "flag_negative_vpd", cloud_ratio_lower_bound_days = "flag_cloud_ratio_lower_bound",
      cloud_ratio_upper_bound_days = "flag_cloud_ratio_upper_bound")
    for (key in names(flag_map)) {
      values <- logical_column(f[[flag_map[[key]]]], flag_map[[key]])
      for (code in codes) {
        assert(sum(values[f$municipality_code == code]) == q[[key]][match(code, q$municipality_code)],
          paste("Flag count does not match quality report:", code, key))
      }
      report[[key]] <<- sum(values)
    }
    critical <- f[logical_column(f$flag_negative_vpd, "negative VPD"), , drop = FALSE]
    write_csv(critical, file.path(out_dir, "coffee_climate_pilot_critical_days.csv"))
    write_csv(bc[!bc$baseline_available, , drop = FALSE], file.path(out_dir, "coffee_climate_missing_baselines.csv"))
    missing_parts <- list(); n <- 0L
    for (v in c("vpd_fao56_mean_kpa", "et0_total_mm", "p_minus_et0_mm")) {
      z <- a[!is.finite(a[[v]]), c("municipality_code", "municipality", "year"), drop = FALSE]
      z$metric <- rep(v, nrow(z))
      z$reason <- ifelse(z$year < 1984 & v != "vpd_fao56_mean_kpa",
        "Radiation structurally unavailable before 1984", "At least one daily dependent input unavailable or excluded")
      n <- n + 1L; missing_parts[[n]] <- z
    }
    write_csv(bind(missing_parts), file.path(out_dir, "coffee_climate_pilot_missing_annual_indices.csv"))

    # Quantify the effect of the declared cloud-ratio implementation choice.
    ss <- a[, c("municipality_code", "municipality", "year", "solar_source_epoch",
      "et0_total_mm", "et0_no_lower_cloud_bound_total_mm")]
    ss$difference_mm <- ss$et0_no_lower_cloud_bound_total_mm - ss$et0_total_mm
    ss$difference_pct <- ifelse(is.finite(ss$et0_total_mm) & ss$et0_total_mm > 0,
      100 * ss$difference_mm / ss$et0_total_mm, NA_real_)
    write_csv(ss, file.path(out_dir, "coffee_climate_et0_cloud_sensitivity.csv"))
    if (any(is.finite(ss$difference_mm))) report$maximum_annual_et0_sensitivity_mm <<- max(abs(ss$difference_mm), na.rm = TRUE)
    if (any(is.finite(ss$difference_pct))) report$maximum_annual_et0_sensitivity_pct <<- max(abs(ss$difference_pct), na.rm = TRUE)

    selected <- c("tmean_c", "hot_days_ge32", "precipitation_total_mm", "longest_dry_spell_days", "vpd_fao56_mean_kpa", "et0_total_mm")
    plot_data <- p[p$metric %in% selected, , drop = FALSE]
    plot_data$municipality <- a$municipality[match(plot_data$municipality_code, a$municipality_code)]
    plot_data$unit <- dict$unit[match(plot_data$metric, dict$metric)]
    plot_data <- plot_data[order(match(plot_data$metric, selected), match(plot_data$municipality_code, codes)), ]
    write_csv(plot_data, file.path(out_dir, "coffee_climate_pilot_selected_changes.csv"))
    notes <- c("COFFE-STUDY | STEP14 | PILOT CLIMATE SYNTHESIS",
      paste("Executed:", format(Sys.time(), usetz = TRUE)),
      "Only step13 tables are read. Daily equations are not independently recomputed in this step.",
      "All period means are independently recomputed from annual indices; monthly aggregations and baseline anomalies are cross-checked.",
      "The figure displays 2021-2024 minus 2012-2015, with one value per site per indicator.",
      "These are differences between four-year windows, not trend slopes, significance tests or attribution estimates.",
      "The six municipal reference points are selected demonstrations, not a representative national sample or actual coffee-field measurements.",
      "The same municipality order is used in all panels. Horizontal scales differ because variables have different units.",
      "Negative-VPD days stay excluded in the step13 usable estimates; no imputation or replacement is made.",
      "The strict completeness rule can make an annual estimate or its 30-year reference unavailable because of an isolated excluded day.",
      "All missing values remain missing; a missing reference is not replaced by a mean of fewer years.",
      "Cloud-bound flags are implementation adjustments, not automatically invalid climate observations.",
      "Their annual ET0 impact is quantified against the already-computed sensitivity alternative; this is not station validation.",
      "ET0 is reference demand; P-ET0 does not include soil storage, roots, irrigation, runoff or drainage.",
      "Sequences are bounded by their calendar periods. They are not observed crop injury events.",
      "Both comparison windows use CERES radiation. No trend across SRB/CERES is estimated.",
      "No additional main figure is numbered. The output belongs to the climate pilot diagnostic folder.",
      "No source observations, step13 outputs, municipal weights, production data or prior figures are changed.")
    writeLines(notes, file.path(out_dir, "coffee_climate_pilot_synthesis_notes.txt"), useBytes = TRUE)

    # Render an optional diagnostic after all tables/checks have succeeded.
    if (!tables_only) {
      plotting_result <- tryCatch({
        for (pkg in c("ggplot2", "patchwork", "systemfonts")) {
          if (!requireNamespace(pkg, quietly = TRUE)) utils::install.packages(pkg, repos = "https://cloud.r-project.org")
          assert(requireNamespace(pkg, quietly = TRUE), paste("Graphics package unavailable:", pkg))
        }
        fonts <- unique(systemfonts::system_fonts()$family)
        family <- if ("Times New Roman" %in% fonts) "Times New Roman" else if ("Liberation Serif" %in% fonts) "Liberation Serif" else "serif"
        report$font_used <<- family
        theme_rogerio <- function(base_size = 14) {
          ggplot2::theme_minimal(base_size = base_size, base_family = family) +
            ggplot2::theme(panel.grid = ggplot2::element_blank(),
              axis.line = ggplot2::element_line(colour = "black", linewidth = 0.45),
              axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.35),
              axis.ticks.length = grid::unit(0.13, "cm"),
              axis.text = ggplot2::element_text(colour = "black"),
              axis.text.y = ggplot2::element_text(size = 10.5),
              axis.title = ggplot2::element_text(size = 12),
              legend.position = "top", legend.title = ggplot2::element_blank(),
              strip.text = ggplot2::element_text(face = "bold"),
              panel.spacing = grid::unit(0.8, "lines"),
              plot.title = ggplot2::element_text(face = "bold", size = 13),
              plot.subtitle = ggplot2::element_text(size = 11),
              plot.margin = ggplot2::margin(8, 12, 8, 8))
        }
        panel_titles <- c("a  Mean temperature", "b  Days with Tmax >= 32 C", "c  Annual precipitation",
          "d  Longest annual dry sequence", "e  Estimated mean VPD", "f  Reference evapotranspiration")
        xlabels <- c("Change (degrees C)", "Change (days per year)", "Change (mm per year)",
          "Change (days)", "Change (kPa)", "Change (mm per year)")
        precision <- c(2L, 1L, 1L, 1L, 2L, 1L)
        name_labels <- a$municipality[match(codes, a$municipality_code)]
        name_labels <- gsub("São Miguel do Guaporé", "São Miguel do\nGuaporé", name_labels, fixed = TRUE)
        panels <- lapply(seq_along(selected), function(k) {
          z <- plot_data[plot_data$metric == selected[k], , drop = FALSE]
          z$position <- length(codes) + 1L - match(z$municipality_code, codes)
          z$value <- z$absolute_change
          ok <- is.finite(z$value)
          extent <- max(abs(z$value[ok]), 1e-9)
          z$label <- ifelse(ok, sprintf(paste0("%+.", precision[k], "f"), z$value), "NA")
          z$label_x <- ifelse(ok, z$value + ifelse(z$value < 0, -1, 1) * 0.045 * extent, 0)
          z$label_hjust <- ifelse(ok & z$value < 0, 1, ifelse(ok, 0, 0.5))
          # Symmetric scale around zero WITHIN each metric, not across unlike units.
          ggplot2::ggplot(z, ggplot2::aes(x = value, y = position)) +
            ggplot2::geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.4, colour = "grey45") +
            ggplot2::geom_segment(data = z[ok, ], ggplot2::aes(x = 0, xend = value, yend = position),
              linewidth = 0.6, colour = "grey55") +
            ggplot2::geom_point(data = z[ok, ], size = 2.7, colour = "black") +
            ggplot2::geom_text(ggplot2::aes(x = label_x, label = label, hjust = label_hjust),
              family = family, size = 3.2, colour = "black") +
            ggplot2::scale_y_continuous(breaks = 1:length(codes), labels = rev(name_labels),
              limits = c(0.5, length(codes) + 0.5), expand = ggplot2::expansion(mult = 0)) +
            ggplot2::scale_x_continuous(limits = c(-1.32, 1.32) * extent,
              breaks = function(lim) pretty(lim, n = 5), expand = ggplot2::expansion(mult = 0)) +
            ggplot2::labs(title = panel_titles[k], x = xlabels[k], y = NULL) + theme_rogerio()
        })
        fig <- patchwork::wrap_plots(panels, ncol = 2) + patchwork::plot_annotation(
          title = "Climate contrasts at six municipal reference points",
          subtitle = "2021 to 2024 minus 2012 to 2015 | NASA POWER | Pilot, not national averages",
          caption = "Different horizontal units and scales. Period differences are not trend estimates or crop impacts.\nDry sequence: precipitation < 1 mm/day. ET0 is reference demand; VPD is estimated from daily inputs.",
          theme = ggplot2::theme(text = ggplot2::element_text(family = family),
            plot.title = ggplot2::element_text(face = "bold", size = 18),
            plot.subtitle = ggplot2::element_text(size = 12),
            plot.caption = ggplot2::element_text(size = 10, hjust = 0),
            plot.margin = ggplot2::margin(12, 14, 12, 14)))
        png <- file.path(figure_dir, "coffee_climate_pilot_period_changes.png")
        if (requireNamespace("ragg", quietly = TRUE)) {
          ggplot2::ggsave(png, fig, device = ragg::agg_png, width = 14, height = 11, units = "in", dpi = 300, bg = "white")
        } else if (capabilities("cairo")) {
          png_device <- function(...) grDevices::png(..., type = "cairo")
          ggplot2::ggsave(png, fig, device = png_device, width = 14, height = 11, units = "in", dpi = 300, bg = "white")
        } else {
          ggplot2::ggsave(png, fig, device = "png", width = 14, height = 11, units = "in", dpi = 300, bg = "white")
        }
        report$png_status <<- "Saved"
        if (capabilities("cairo")) {
          pdf_error <- tryCatch({
            ggplot2::ggsave(file.path(figure_dir, "coffee_climate_pilot_period_changes.pdf"), fig,
              device = grDevices::cairo_pdf, width = 14, height = 11, units = "in", bg = "white")
            NULL
          }, error = function(e) conditionMessage(e))
          report$pdf_status <<- if (is.null(pdf_error)) "Saved" else paste("Not saved:", pdf_error)
        } else report$pdf_status <<- "Skipped: Cairo not available"
        TRUE
      }, error = function(e) {
        report$error_if_incomplete <<- paste("Tables checked; graphics issue:", conditionMessage(e))
        message("Graphics issue; checked tables were preserved: ", conditionMessage(e))
        FALSE
      })
      report$execution_status <<- if (plotting_result) "COMPLETED_PILOT_SYNTHESIS" else "COMPLETED_TABLES_GRAPHICS_NEEDS_REVIEW"
    } else report$execution_status <<- "COMPLETED_TABLES_ONLY"
    assert(all(vapply(paths, hash, character(1)) == provenance$md5), "An input file changed during this run.")
    save_report()
    message("\n", report$execution_status,
      "\nPeriod comparisons verified: ", report$period_comparisons_verified,
      "\nMonthly/annual checks: ", report$monthly_annual_checks_verified,
      "\nNegative VPD days retained as exclusions: ", report$negative_vpd_days,
      "\nReports: ", out_dir, "\nDiagnostic figure: ", figure_dir,
      "\nNo climate downloads or changes to earlier files.")
    invisible(report)
  }
  tryCatch(worker(), error = function(e) {
    report$execution_status <<- "INCOMPLETE"
    report$error_if_incomplete <<- conditionMessage(e)
    try(save_report(), silent = TRUE)
    stop(conditionMessage(e), "\nEarlier files were not changed. Report: ", out_dir, call. = FALSE)
  })
}

coffee_pilot_step14()
