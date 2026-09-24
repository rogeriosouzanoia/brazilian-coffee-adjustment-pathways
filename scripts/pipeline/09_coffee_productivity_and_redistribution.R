# ======================================================================
# COFFE-STUDY | STEP 09
# Municipal productivity changes and redistribution of harvested area
#
# Run the ENTIRE script with Source in RStudio.
# Uses the outputs of step 08. No downloads and no additional R packages.
# Keeps all previous inputs, tables and figures unchanged.
#
# Windows: 2012 to 2015 versus 2021 to 2024.
# Same fixed municipal-code panel and categories as step 08.
# These are exact accounting decompositions, NOT causal effects.
# ======================================================================

main <- function() {
  base_dir <- Sys.getenv(
    "COFFEE_STUDY_DIR",
    unset = "."
  )
  in_dir <- file.path(base_dir, "04_outputs", "geographic_redistribution")
  out_dir <- file.path(base_dir, "04_outputs", "productivity_redistribution")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out_dir)) stop("Cannot create: ", out_dir, call. = FALSE)
  old_options <- options(scipen = 999, digits = 15)
  on.exit(options(old_options), add = TRUE)
  species_levels <- c("Total", "Arabica", "Canephora")
  baseline_period <- "2012 to 2015"
  recent_period <- "2021 to 2024"

  read_csv <- function(path) {
    if (!file.exists(path)) stop("Required input not found:\n", path, call. = FALSE)
    utils::read.csv(path, colClasses = "character", check.names = FALSE,
                   stringsAsFactors = FALSE, na.strings = c("NA", ""),
                   fileEncoding = "UTF-8")
  }
  write_csv <- function(x, name) {
    target <- file.path(out_dir, name)
    temp <- tempfile("coffee09_", tmpdir = out_dir, fileext = ".csv")
    on.exit(unlink(temp), add = TRUE)
    utils::write.csv(x, temp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    if (!file.copy(temp, target, overwrite = TRUE)) {
      stop("Cannot save: ", target, call. = FALSE)
    }
  }
  need_cols <- function(d, required, label) {
    missing <- setdiff(required, names(d))
    if (length(missing)) stop(label, ": missing columns: ",
                             paste(missing, collapse = ", "), call. = FALSE)
  }
  numeric_checked <- function(x, name, missing_ok = FALSE) {
    out <- suppressWarnings(as.numeric(x))
    if (any(!is.na(x) & is.na(out)) || any(!is.na(out) & !is.finite(out)) ||
        (!missing_ok && anyNA(out))) stop("Invalid numeric input: ", name, call. = FALSE)
    out
  }
  get_check <- function(d, key) {
    value <- d$result[d$check == key]
    if (length(value) != 1L || is.na(value)) stop("Missing/ambiguous check: ", key, call. = FALSE)
    value
  }
  # Ratios are undefined for zero denominators. No yield is imputed.
  ratio <- function(numerator, denominator, multiplier = 1) {
    answer <- rep(NA_real_, length(numerator))
    good <- is.finite(numerator) & is.finite(denominator) & denominator > 0
    answer[good] <- multiplier * numerator[good] / denominator[good]
    answer
  }
  relative_change <- function(before, after) ratio(after - before, before, 100)
  close_enough <- function(a, b, label, tolerance = 1e-5) {
    if (length(a) != length(b) || any(!is.finite(a)) || any(!is.finite(b)) ||
        max(abs(a - b)) > tolerance) stop("Consistency check failed: ", label, call. = FALSE)
  }

  # --------------------------------------------------------------------
  # 1. Input provenance and consistency with the previous completed step.
  # --------------------------------------------------------------------
  municipal_file <- file.path(in_dir, "coffee_municipal_redistribution.csv")
  state_file <- file.path(in_dir, "coffee_state_redistribution.csv")
  shift_file <- file.path(in_dir, "coffee_geographic_shift_by_species.csv")
  summary_file <- file.path(in_dir, "coffee_redistribution_summary.csv")
  input_rds <- file.path(base_dir, "02_data_processed",
                        "coffee_brazil_municipality_audited_1974_2024.rds")
  summary08 <- read_csv(summary_file)
  need_cols(summary08, c("check", "result"), "Step 08 summary")
  if (!get_check(summary08, "execution_status") %in%
      c("COMPLETED", "TABLES_COMPLETE_FIGURE_ERROR")) stop("Step 08 is incomplete.", call. = FALSE)
  if (get_check(summary08, "analysis_period") != "2012 to 2024" ||
      get_check(summary08, "official_validation_passed_for_input") != "TRUE") {
    stop("Unexpected analysis period or missing official-total validation.", call. = FALSE)
  }
  if (!file.exists(input_rds)) stop("Audited source RDS is missing:\n", input_rds, call. = FALSE)
  input_md5 <- unname(tools::md5sum(input_rds))
  if (!identical(input_md5, get_check(summary08, "input_md5"))) {
    stop("The source RDS has changed since step 08. Rerun steps 07 and 08.", call. = FALSE)
  }
  expected_codes <- as.integer(get_check(summary08, "fixed_panel_municipal_codes"))
  if (is.na(expected_codes) || expected_codes < 1) stop("Invalid panel size.", call. = FALSE)

  municipal <- read_csv(municipal_file)
  states08 <- read_csv(state_file)
  shifts08 <- read_csv(shift_file)
  value_cols <- c("baseline_production_t", "recent_production_t",
                  "baseline_area_ha", "recent_area_ha")
  need_cols(municipal, c("municipality_code", "municipality", "state", "species", value_cols),
            "Municipal period means")
  need_cols(states08, c("state", "species", value_cols), "State period means")
  need_cols(shifts08, c("species", "weighting", "coordinate_definition", "baseline_period",
    "recent_period", "baseline_total_weight", "recent_total_weight"), "Period centers")
  for (v in value_cols) {
    municipal[[v]] <- numeric_checked(municipal[[v]], v)
    states08[[v]] <- numeric_checked(states08[[v]], v)
    if (any(municipal[[v]] < 0)) stop("Negative values: ", v, call. = FALSE)
  }
  for (v in c("baseline_total_weight", "recent_total_weight")) {
    shifts08[[v]] <- numeric_checked(shifts08[[v]], v)
  }
  if (anyNA(municipal$municipality_code) || anyNA(municipal$state) ||
      anyNA(municipal$species) ||
      any(!grepl("^[0-9]{7}$", municipal$municipality_code)) ||
      !setequal(unique(municipal$species), species_levels) ||
      anyDuplicated(paste(municipal$municipality_code, municipal$species))) {
    stop("Invalid municipal identifiers or duplicate municipality/species keys.", call. = FALSE)
  }
  if (anyDuplicated(paste(states08$state, states08$species))) stop("Duplicate state keys.", call. = FALSE)
  code_lists <- lapply(species_levels, function(sp) sort(municipal$municipality_code[municipal$species == sp]))
  if (any(vapply(code_lists, length, integer(1)) != expected_codes) ||
      !all(vapply(code_lists, identical, logical(1), code_lists[[1L]]))) {
    stop("Species do not use the same complete municipal-code panel.", call. = FALSE)
  }
  for (period in c("baseline", "recent")) {
    a <- municipal[[paste0(period, "_area_ha")]]
    p <- municipal[[paste0(period, "_production_t")]]
    if (any(a == 0 & p > 0)) stop("Positive production with zero area: ", period, call. = FALSE)
  }
  # Verify total = Arabica + Canephora without adding Total to either species.
  aligned <- lapply(species_levels, function(sp) {
    z <- municipal[municipal$species == sp, , drop = FALSE]
    z[order(z$municipality_code), , drop = FALSE]
  })
  for (v in value_cols) close_enough(aligned[[1L]][[v]], aligned[[2L]][[v]] + aligned[[3L]][[v]],
                                     paste("Species additivity:", v))

  # --------------------------------------------------------------------
  # 2. Exact decomposition of aggregate productivity.
  #
  # Y_t = sum_i(a_it * y_it), a_it = A_it / sum_i(A_it).
  # y_it = 1000 * P_it/A_it, only where A_it > 0.
  # For municipalities with area in BOTH periods:
  #   within_i = 0.5*(a_i0+a_i1)*(y_i1-y_i0)
  #   weight_i = 0.5*(y_i0+y_i1)*(a_i1-a_i0)
  # For zero-to-positive area: +1000*P_i1/sum(A_i1).
  # For positive-to-zero area: -1000*P_i0/sum(A_i0).
  # Each total uses ALL municipalities in its period, including transitions.
  # No undefined counterfactual yield is assigned to entering/exiting units.
  # --------------------------------------------------------------------
  component_cols <- c("within_municipality_component_kg_ha", "area_weight_component_kg_ha",
                     "zero_to_positive_area_component_kg_ha", "positive_to_zero_area_component_kg_ha")
  municipal_results <- summary_results <- state_results <- redistribution_results <- list()

  for (sp in species_levels) {
    message("Analysing: ", sp)
    z <- municipal[municipal$species == sp,
                   c("municipality_code", "municipality", "state", "species", value_cols), drop = FALSE]
    z <- z[order(z$municipality_code), , drop = FALSE]
    P0 <- sum(z$baseline_production_t)
    P1 <- sum(z$recent_production_t)
    A0 <- sum(z$baseline_area_ha)
    A1 <- sum(z$recent_area_ha)
    if (any(c(P0, P1, A0, A1) <= 0)) stop("Non-positive category totals: ", sp, call. = FALSE)
    for (weighting in c("Production", "Harvested_area")) {
      reference <- shifts08[shifts08$species == sp & shifts08$weighting == weighting &
        shifts08$coordinate_definition == "Municipal_polygon_centroid", , drop = FALSE]
      if (nrow(reference) != 1L || reference$baseline_period != baseline_period ||
          reference$recent_period != recent_period) stop("Unexpected period-center metadata.", call. = FALSE)
      totals <- if (weighting == "Production") c(P0, P1) else c(A0, A1)
      close_enough(totals, c(reference$baseline_total_weight, reference$recent_total_weight),
                   paste("Period-center totals:", sp, weighting))
    }
    z$baseline_area_share <- z$baseline_area_ha / A0
    z$recent_area_share <- z$recent_area_ha / A1
    z$baseline_productivity_kg_ha <- ratio(z$baseline_production_t, z$baseline_area_ha, 1000)
    z$recent_productivity_kg_ha <- ratio(z$recent_production_t, z$recent_area_ha, 1000)
    both <- z$baseline_area_ha > 0 & z$recent_area_ha > 0
    entry <- z$baseline_area_ha == 0 & z$recent_area_ha > 0
    exit <- z$baseline_area_ha > 0 & z$recent_area_ha == 0
    z$area_transition <- "Zero in both periods"
    z$area_transition[both] <- "Positive in both periods"
    z$area_transition[entry] <- "Zero baseline; positive recent"
    z$area_transition[exit] <- "Positive baseline; zero recent"
    for (v in component_cols) z[[v]] <- 0
    z$within_municipality_component_kg_ha[both] <-
      (z$baseline_area_share[both] + z$recent_area_share[both]) / 2 *
      (z$recent_productivity_kg_ha[both] - z$baseline_productivity_kg_ha[both])
    z$area_weight_component_kg_ha[both] <-
      (z$baseline_productivity_kg_ha[both] + z$recent_productivity_kg_ha[both]) / 2 *
      (z$recent_area_share[both] - z$baseline_area_share[both])
    z$zero_to_positive_area_component_kg_ha[entry] <- 1000 * z$recent_production_t[entry] / A1
    z$positive_to_zero_area_component_kg_ha[exit] <- -1000 * z$baseline_production_t[exit] / A0
    z$observed_contribution_change_kg_ha <-
      1000 * z$recent_production_t / A1 - 1000 * z$baseline_production_t / A0
    z$reconstructed_contribution_change_kg_ha <- rowSums(z[component_cols])
    close_enough(z$observed_contribution_change_kg_ha, z$reconstructed_contribution_change_kg_ha,
                 paste("Municipal productivity identity:", sp), tolerance = 1e-8)
    Y0 <- 1000 * P0 / A0
    Y1 <- 1000 * P1 / A1
    components <- vapply(z[component_cols], sum, numeric(1))
    residual <- (Y1 - Y0) - sum(components)
    close_enough(Y1 - Y0, sum(components), paste("Aggregate productivity identity:", sp), 1e-7)
    s <- data.frame(species = sp, baseline_period = baseline_period, recent_period = recent_period,
      fixed_panel_codes = nrow(z), baseline_production_t = P0, recent_production_t = P1,
      baseline_area_ha = A0, recent_area_ha = A1,
      baseline_aggregate_productivity_kg_ha = Y0, recent_aggregate_productivity_kg_ha = Y1,
      production_change_pct = relative_change(P0, P1), area_change_pct = relative_change(A0, A1),
      productivity_change_kg_ha = Y1 - Y0, productivity_change_pct = relative_change(Y0, Y1),
      area_positive_both_periods = sum(both), zero_to_positive_area_codes = sum(entry),
      positive_to_zero_area_codes = sum(exit), zero_area_both_periods = sum(!both & !entry & !exit),
      identity_residual_kg_ha = residual, stringsAsFactors = FALSE)
    for (v in component_cols) {
      s[[v]] <- components[[v]]
      # Percentage points relative to baseline productivity, NOT % of net gain.
      s[[sub("_kg_ha$", "_pp_of_baseline_productivity", v)]] <- 100 * components[[v]] / Y0
    }
    s$historical_boundaries_harmonized <- FALSE
    s$climate_attribution_performed <- FALSE
    summary_results[[sp]] <- s
    z$baseline_period <- baseline_period
    z$recent_period <- recent_period
    municipal_results[[sp]] <- z

    # State-level sums of the MUNICIPAL components. This differs from
    # decomposing state-level average productivity as if it were municipal.
    for (st in sort(unique(z$state))) {
      q <- z[z$state == st, , drop = FALSE]
      t <- data.frame(state = st, species = sp, fixed_panel_codes = nrow(q),
        baseline_production_t = sum(q$baseline_production_t), recent_production_t = sum(q$recent_production_t),
        baseline_area_ha = sum(q$baseline_area_ha), recent_area_ha = sum(q$recent_area_ha), stringsAsFactors = FALSE)
      ref <- states08[states08$species == sp & states08$state == st, , drop = FALSE]
      if (nrow(ref) != 1L) stop("Missing state comparison: ", st, " / ", sp, call. = FALSE)
      close_enough(unlist(t[value_cols], use.names = FALSE), unlist(ref[value_cols], use.names = FALSE),
                   paste("State totals:", sp, st))
      t$baseline_aggregate_productivity_kg_ha <- ratio(t$baseline_production_t, t$baseline_area_ha, 1000)
      t$recent_aggregate_productivity_kg_ha <- ratio(t$recent_production_t, t$recent_area_ha, 1000)
      t$production_change_pct <- relative_change(t$baseline_production_t, t$recent_production_t)
      t$area_change_pct <- relative_change(t$baseline_area_ha, t$recent_area_ha)
      t$baseline_production_share_pct <- 100 * t$baseline_production_t / P0
      t$recent_production_share_pct <- 100 * t$recent_production_t / P1
      t$baseline_area_share_pct <- 100 * t$baseline_area_ha / A0
      t$recent_area_share_pct <- 100 * t$recent_area_ha / A1
      for (v in component_cols) t[[v]] <- sum(q[[v]])
      t$components_refer_to_category_aggregate_productivity <- TRUE
      state_results[[paste(sp, st, sep = "_")]] <- t
    }

    # Half of the L1 difference of normalized shares: total variation.
    # A redistribution of statistical shares, NOT a % of farms/land moving.
    for (weighting in c("Production", "Harvested_area")) {
      w0 <- if (weighting == "Production") z$baseline_production_t else z$baseline_area_ha
      w1 <- if (weighting == "Production") z$recent_production_t else z$recent_area_ha
      delta <- w1 / sum(w1) - w0 / sum(w0)
      close_enough(sum(delta), 0, paste("Share balance:", sp, weighting), 1e-10)
      redistribution_results[[paste(sp, weighting, sep = "_")]] <- data.frame(
        species = sp, weighting = weighting, baseline_period = baseline_period, recent_period = recent_period,
        fixed_panel_codes = length(w0), total_variation_share_pct = 50 * sum(abs(delta)),
        positive_share_gains_pp = 100 * sum(delta[delta > 0]),
        absolute_share_losses_pp = -100 * sum(delta[delta < 0]),
        baseline_positive_codes = sum(w0 > 0), recent_positive_codes = sum(w1 > 0),
        actual_farm_relocation_measured = FALSE, climate_attribution_performed = FALSE)
    }
  }

  results <- do.call(rbind, summary_results)
  municipalities <- do.call(rbind, municipal_results)
  states <- do.call(rbind, state_results)
  redistribution <- do.call(rbind, redistribution_results)
  rownames(results) <- rownames(municipalities) <- rownames(states) <- rownames(redistribution) <- NULL
  write_csv(results, "coffee_productivity_decomposition_summary.csv")
  write_csv(municipalities, "coffee_productivity_decomposition_municipal.csv")
  write_csv(states, "coffee_productivity_decomposition_by_state.csv")
  write_csv(redistribution, "coffee_spatial_share_redistribution.csv")

  provenance_files <- c(input_rds, municipal_file, state_file, shift_file, summary_file)
  write_csv(data.frame(file = basename(provenance_files), path = provenance_files,
    md5 = unname(tools::md5sum(provenance_files)), stringsAsFactors = FALSE),
    "coffee_step09_input_provenance.csv")
  write_csv(data.frame(check = c("execution_status", "baseline_period", "recent_period",
    "fixed_panel_codes", "maximum_absolute_identity_residual_kg_ha", "source_input_md5",
    "period_means_match_step08_state_and_category_totals", "undefined_productivities_imputed",
    "original_files_modified", "historical_boundaries_harmonized", "climate_attribution_performed"),
    result = as.character(c("COMPLETED", baseline_period, recent_period, expected_codes,
      max(abs(results$identity_residual_kg_ha)), input_md5, TRUE, FALSE, FALSE, FALSE, FALSE))),
    "coffee_step09_summary.csv")

  notes <- c(
    "COFFE-STUDY | STEP 09 | PRODUCTIVITY AND AREA COMPOSITION",
    paste("Executed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Source audited RDS MD5:", input_md5),
    "The four-year municipal means and fixed-code panel are inherited unchanged from step 08.",
    "Baseline: 2012 to 2015. Recent: 2021 to 2024. Categories always remain separate.",
    "Productivity is 1000 * mean production (t) / mean harvested area (ha).",
    "National/category productivity is the ratio of sums, not an unweighted municipal mean.",
    "Let a_it = A_it / sum_i(A_it); let y_it = 1000*P_it/A_it where A_it>0.",
    "For units with positive area in both periods:",
    "  within component = mean(a_i0,a_i1) * (y_i1-y_i0).",
    "  area-weight component = mean(y_i0,y_i1) * (a_i1-a_i0).",
    "The interaction is shared symmetrically between these two accounting components.",
    "For zero-to-positive area: component = 1000*P_i1/sum_i(A_i1).",
    "For positive-to-zero area: component = -1000*P_i0/sum_i(A_i0).",
    "Undefined productivity remains NA. Zero-to-positive/positive-to-zero cases are separate.",
    "All period denominators include all panel municipalities, including transitions.",
    "The four components sum exactly to the change in aggregate productivity.",
    "Component percentage points use BASELINE aggregate productivity as denominator.",
    "State output sums municipal contributions to category aggregate productivity, not to state productivity.",
    "Within-municipality components are statistical changes inside reporting units.",
    "They can incorporate climate, management, genetics, farm composition, errors and boundary changes.",
    "They are not estimates of technological progress or any other causal mechanism.",
    "Area-weight components describe changing municipal shares of harvested area.",
    "Zero/positive transitions are reported-period patterns, not verified farm openings or closures.",
    "Total variation (%) = 50 * sum_i(abs(share_i1-share_i0)), shares on a 0 to 1 scale.",
    "This measures the change of municipal statistical shares, not the fraction of land physically relocated.",
    "Total variation uses no distances and is sensitive to the reporting-unit geography.",
    "A small centroid change may coexist with changing shares because directional changes can cancel.",
    "Fixed municipal codes do not ensure historically fixed boundaries.",
    "No altitude, weather or farm locations are estimated. No new main figure is created.",
    "Previous production data, step 08 outputs and figures remain unchanged."
  )
  writeLines(notes, file.path(out_dir, "coffee_step09_method_notes.txt"), useBytes = TRUE)
  writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo_step09.txt"))
  cat("\n============================================================\n")
  cat("STEP 09 COMPLETED | ", baseline_period, " vs ", recent_period, "\n", sep = "")
  cat("All municipal and aggregate accounting identities passed.\n\n")
  print(results[, c("species", "baseline_aggregate_productivity_kg_ha",
    "recent_aggregate_productivity_kg_ha", "productivity_change_kg_ha", component_cols)], row.names = FALSE)
  cat("\nMunicipal statistical-share redistribution:\n")
  print(redistribution[, c("species", "weighting", "total_variation_share_pct")], row.names = FALSE)
  cat("\nFiles saved in:\n", out_dir, "\n", sep = "")
  cat("\nSend: coffee_productivity_decomposition_summary.csv\n")
  cat("      coffee_productivity_decomposition_by_state.csv\n")
  invisible(results)
}

main()
