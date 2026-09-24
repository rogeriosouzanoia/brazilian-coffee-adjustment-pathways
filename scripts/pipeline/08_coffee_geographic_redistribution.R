# ======================================================================
# COFFE-STUDY | 08_coffee_geographic_redistribution.R
# Descriptive geographic redistribution and concentration, 2012 to 2024.
# Run the ENTIRE file with Source in RStudio.
#
# Uses the audited municipal data and the coordinate registry from step 05.
# No production, weather, terrain or boundary data are downloaded.
# Existing input files and earlier figures remain unchanged.
#
# IMPORTANT: These are reporting-unit indicators on a 2024 reference map.
# They do NOT establish actual farm relocation, uphill movement, stable
# historical boundaries, climate effects or increased climate risk.
#
# Documentation checked for the implementation:
# https://r-spatial.github.io/sf/reference/st_transform.html
# https://ggplot2.tidyverse.org/reference/ggsave.html
# https://patchwork.data-imaginist.com/reference/wrap_plots.html
# ======================================================================

main <- function() {
  base_dir <- Sys.getenv(
    "COFFEE_STUDY_DIR",
    unset = "."
  )
  years <- 2012:2024
  baseline_years <- 2012:2015
  recent_years <- 2021:2024
  species_levels <- c("Total", "Arabica", "Canephora")
  metric_crs <- "+proj=laea +lat_0=-15 +lon_0=-54 +datum=WGS84 +units=m +no_defs"
  processed_dir <- file.path(base_dir, "02_data_processed")
  out_dir <- file.path(base_dir, "04_outputs", "geographic_redistribution")
  fig_dir <- file.path(base_dir, "05_figures", "Figure02_spatial_draft")
  for (p in c(out_dir, fig_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create directory: ", p, call. = FALSE)
  }
  old_options <- options(scipen = 999)
  on.exit(options(old_options), add = TRUE)

  # Output replacement is restricted to files produced by THIS step.
  write_csv <- function(x, filename) {
    target <- file.path(out_dir, filename)
    tmp <- tempfile("coffee08_", tmpdir = out_dir, fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    if (!file.copy(tmp, target, overwrite = TRUE)) stop("Cannot save: ", target, call. = FALSE)
  }
  read_csv <- function(path) {
    if (!file.exists(path)) stop("Required input not found:\n", path, call. = FALSE)
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                   colClasses = "character", na.strings = c("NA", ""),
                   fileEncoding = "UTF-8")
  }
  require_columns <- function(x, needed, label) {
    missing <- setdiff(needed, names(x))
    if (length(missing)) stop(label, ": missing columns: ",
                             paste(missing, collapse = ", "), call. = FALSE)
  }
  as_numeric_checked <- function(x, label) {
    out <- suppressWarnings(as.numeric(x))
    if (any(!is.na(x) & is.na(out)) || any(!is.na(out) & !is.finite(out))) {
      stop("Non-numeric or non-finite values in ", label, call. = FALSE)
    }
    out
  }
  sum_available <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  safe_pct <- function(a, b) if (is.finite(b) && b > 0) 100 * a / b else NA_real_
  pct_change <- function(a, b) ifelse(a > 0, 100 * (b - a) / a, NA_real_)

  # --------------------------------------------------------------------
  # 1. Load only previously prepared data. Reuse the original RDS hash.
  # --------------------------------------------------------------------
  input_file <- file.path(processed_dir, "coffee_brazil_municipality_audited_1974_2024.rds")
  if (!file.exists(input_file)) stop("Run step 04 first. Expected:\n", input_file, call. = FALSE)
  dat <- as.data.frame(readRDS(input_file), stringsAsFactors = FALSE)
  require_columns(dat, c("municipality_code", "municipality", "state", "year", "species",
                        "production_t", "area_harvested_ha"), "Audited master")
  input_md5 <- unname(tools::md5sum(input_file))
  official_dir <- file.path(base_dir, "04_outputs", "official_validation")
  official_file <- file.path(official_dir, "coffee_official_validation_summary.csv")
  validation <- read_csv(official_file)
  require_columns(validation, c("check", "result"), "Official validation summary")
  get_check <- function(key) {
    z <- validation$result[validation$check == key]
    if (length(z) != 1L || is.na(z)) stop("Missing or ambiguous validation check: ", key, call. = FALSE)
    z
  }
  if (get_check("execution_status") != "COMPLETED" ||
      get_check("mismatch_cells") != "0" || get_check("untestable_cells") != "0") {
    stop("Official source validation is incomplete or has unresolved differences. See step 07.", call. = FALSE)
  }
  for (key in c("national_production_area_totals_reproduced",
                "state_production_area_totals_reproduced",
                "current_official_category_labels_match_input_mapping",
                "current_official_response_units_verified")) {
    if (toupper(get_check(key)) != "TRUE") stop("Step 07 has not confirmed: ", key, call. = FALSE)
  }
  if (!identical(input_md5, get_check("input_md5"))) {
    stop("The municipal input differs from the file verified in step 07. Rerun step 07 first.", call. = FALSE)
  }
  dat$municipality_code <- as.character(dat$municipality_code)
  dat$year <- as_numeric_checked(dat$year, "year")
  if (anyNA(dat$year) || anyNA(dat$municipality_code) ||
      any(!grepl("^[0-9]{7}$", dat$municipality_code))) stop("Invalid years or municipal codes.", call. = FALSE)
  d <- dat[dat$year %in% years & dat$species %in% species_levels, , drop = FALSE]
  rm(dat)
  if (!nrow(d) || anyNA(d$species)) stop("No valid 2012 to 2024 records.", call. = FALSE)
  for (v in c("production_t", "area_harvested_ha")) {
    d[[v]] <- as_numeric_checked(d[[v]], v)
    if (any(d[[v]] < 0, na.rm = TRUE)) stop("Negative measurements in ", v, call. = FALSE)
  }
  if (anyDuplicated(paste(d$municipality_code, d$year, d$species, sep = "|"))) {
    stop("Duplicate municipality/year/species keys.", call. = FALSE)
  }
  if (any(!is.na(d$production_t) & d$production_t > 0 &
          !is.na(d$area_harvested_ha) & d$area_harvested_ha == 0)) {
    stop("Positive production with zero harvested area requires review.", call. = FALSE)
  }
  message("Official totals verified for the unchanged input. Preparing fixed-code panel...")

  # A single panel is used throughout this step: all 13 years, all 3
  # categories, both measurements present. Zero observations are retained.
  codes <- sort(unique(d$municipality_code))
  complete_pair <- !is.na(d$production_t) & !is.na(d$area_harvested_ha)
  complete_counts <- table(factor(d$municipality_code[complete_pair], levels = codes))
  membership <- data.frame(
    municipality_code = codes,
    observed_year_species_pairs = as.integer(complete_counts),
    required_year_species_pairs = length(years) * length(species_levels),
    stringsAsFactors = FALSE
  )
  membership$included <- membership$observed_year_species_pairs == membership$required_year_species_pairs
  panel_codes <- membership$municipality_code[membership$included]
  if (!length(panel_codes)) stop("The complete municipal-code panel is empty.", call. = FALSE)
  write_csv(membership, "coffee_redistribution_panel_membership.csv")
  panel <- d[d$municipality_code %in% panel_codes, , drop = FALSE]
  panel <- panel[order(panel$species, panel$year, panel$municipality_code), , drop = FALSE]

  # --------------------------------------------------------------------
  # 2. Exact-code geography. Coordinates are not inferred from names.
  # --------------------------------------------------------------------
  registry_file <- file.path(processed_dir, "spatial", "coffee_municipality_spatial_registry.csv")
  registry <- read_csv(registry_file)
  require_columns(registry, c("municipality_code", "reference_boundary_year", "spatial_match",
    "municipality_centroid_x_laea_m", "municipality_centroid_y_laea_m",
    "municipality_centroid_longitude", "municipality_centroid_latitude",
    "municipality_reference_longitude", "municipality_reference_latitude"), "Spatial registry")
  if (anyNA(registry$municipality_code) || anyDuplicated(registry$municipality_code)) {
    stop("Invalid or duplicated codes in the spatial registry.", call. = FALSE)
  }
  idx <- match(panel_codes, registry$municipality_code)
  if (anyNA(idx)) stop("Some panel codes have no spatial reference. Rerun step 05.", call. = FALSE)
  reg <- registry[idx, , drop = FALSE]
  if (anyNA(reg$spatial_match) || any(toupper(reg$spatial_match) != "TRUE") ||
      anyNA(reg$reference_boundary_year) || any(reg$reference_boundary_year != "2024")) {
    stop("Spatial reference is not complete or is not the expected 2024 version.", call. = FALSE)
  }
  coord_cols <- c("municipality_centroid_x_laea_m", "municipality_centroid_y_laea_m",
                 "municipality_centroid_longitude", "municipality_centroid_latitude",
                 "municipality_reference_longitude", "municipality_reference_latitude")
  for (v in coord_cols) {
    reg[[v]] <- as_numeric_checked(reg[[v]], v)
    if (anyNA(reg[[v]])) stop("Missing coordinates in: ", v, call. = FALSE)
  }
  packages <- c("sf", "ggplot2", "patchwork")
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages)) {
    utils::install.packages(missing_packages, repos = "https://cloud.r-project.org")
  }
  if (any(!vapply(packages, requireNamespace, logical(1), quietly = TRUE))) {
    stop("Required packages are not available: sf, ggplot2, patchwork.", call. = FALSE)
  }
  projected_points <- function(lon, lat) {
    pts <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = "OGC:CRS84")
    ans <- sf::st_coordinates(sf::st_transform(pts, metric_crs))
    if (nrow(ans) != length(lon) || any(!is.finite(ans))) stop("Coordinate transformation failed.", call. = FALSE)
    ans
  }
  cxy <- cbind(reg$municipality_centroid_x_laea_m, reg$municipality_centroid_y_laea_m)
  check_xy <- projected_points(reg$municipality_centroid_longitude, reg$municipality_centroid_latitude)
  if (max(sqrt(rowSums((check_xy - cxy)^2))) > 5) {
    stop("The stored coordinates do not match the documented metric projection.", call. = FALSE)
  }
  # Sensitivity: another municipal reference point, still NOT a farm point.
  ixy <- projected_points(reg$municipality_reference_longitude, reg$municipality_reference_latitude)
  to_lonlat <- function(x, y) {
    pts <- sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = metric_crs)
    ans <- sf::st_coordinates(sf::st_transform(pts, "OGC:CRS84"))
    if (nrow(ans) != length(x) || any(!is.finite(ans))) stop("Inverse transformation failed.", call. = FALSE)
    ans
  }
  coordinate_definitions <- list(Municipal_polygon_centroid = cxy,
                                Municipal_interior_point = ixy)
  panel$state <- as.character(panel$state)
  if (anyNA(panel$state)) stop("Missing state labels.", call. = FALSE)
  labels <- panel[match(panel_codes, panel$municipality_code), c("municipality_code", "municipality", "state")]

  # --------------------------------------------------------------------
  # 3. Annual metrics and coverage. All quantities retain their meaning.
  # --------------------------------------------------------------------
  centers_list <- concentration_list <- coverage_list <- list()
  ncenter <- nconc <- ncover <- 0L
  center_values <- function(w, xy) {
    den <- sum(w)
    if (!is.finite(den) || den <= 0) stop("Cannot calculate a center without positive total weight.", call. = FALSE)
    c(x_m = sum(w * xy[, 1L]) / den, y_m = sum(w * xy[, 2L]) / den)
  }
  weights_for <- function(q) list(Production = q$production_t, Harvested_area = q$area_harvested_ha)
  for (sp in species_levels) {
    for (yr in years) {
      q <- panel[panel$species == sp & panel$year == yr, , drop = FALSE]
      q <- q[match(panel_codes, q$municipality_code), , drop = FALSE]
      if (nrow(q) != length(panel_codes) || anyNA(q$municipality_code)) stop("Incomplete fixed panel.", call. = FALSE)
      available <- d[d$species == sp & d$year == yr, , drop = FALSE]
      ncover <- ncover + 1L
      coverage_list[[ncover]] <- data.frame(
        year = yr, species = sp, fixed_panel_codes = nrow(q),
        available_production_t = sum_available(available$production_t),
        panel_production_t = sum(q$production_t),
        production_coverage_pct = safe_pct(sum(q$production_t), sum_available(available$production_t)),
        available_harvested_area_ha = sum_available(available$area_harvested_ha),
        panel_harvested_area_ha = sum(q$area_harvested_ha),
        harvested_area_coverage_pct = safe_pct(sum(q$area_harvested_ha), sum_available(available$area_harvested_ha)),
        missing_production_outside_panel = sum(is.na(available$production_t)),
        stringsAsFactors = FALSE
      )
      for (wn in names(weights_for(q))) {
        w <- weights_for(q)[[wn]]
        shares <- w / sum(w)
        sorted_shares <- sort(shares, decreasing = TRUE)
        hhi <- sum(shares^2)
        nconc <- nconc + 1L
        concentration_list[[nconc]] <- data.frame(
          year = yr, species = sp, weighting = wn,
          positive_weight_municipal_codes = sum(w > 0),
          top10_share_pct = 100 * sum(head(sorted_shares, 10L)),
          top50_share_pct = 100 * sum(head(sorted_shares, 50L)),
          hhi = hhi, equivalent_number_of_equal_share_municipalities = 1 / hhi,
          reference_boundary_year = 2024L, historical_boundaries_harmonized = FALSE,
          stringsAsFactors = FALSE
        )
        for (cd in names(coordinate_definitions)) {
          cen <- center_values(w, coordinate_definitions[[cd]])
          ncenter <- ncenter + 1L
          centers_list[[ncenter]] <- data.frame(year = yr, species = sp, weighting = wn,
            coordinate_definition = cd, x_laea_m = unname(cen[1L]), y_laea_m = unname(cen[2L]),
            fixed_panel_codes = nrow(q), stringsAsFactors = FALSE)
        }
      }
    }
  }
  centers <- do.call(rbind, centers_list)
  concentration <- do.call(rbind, concentration_list)
  coverage <- do.call(rbind, coverage_list)
  ll <- to_lonlat(centers$x_laea_m, centers$y_laea_m)
  centers$longitude <- ll[, 1L]
  centers$latitude <- ll[, 2L]
  groups <- interaction(centers$species, centers$weighting, centers$coordinate_definition, drop = TRUE)
  centers$grid_easting_change_from_2012_km <- centers$grid_northing_change_from_2012_km <- NA_real_
  for (ii in split(seq_len(nrow(centers)), groups)) {
    i0 <- ii[centers$year[ii] == years[1L]]
    if (length(i0) != 1L) stop("Missing baseline year for center trajectory.", call. = FALSE)
    centers$grid_easting_change_from_2012_km[ii] <- (centers$x_laea_m[ii] - centers$x_laea_m[i0]) / 1000
    centers$grid_northing_change_from_2012_km[ii] <- (centers$y_laea_m[ii] - centers$y_laea_m[i0]) / 1000
  }
  centers$historical_boundaries_harmonized <- FALSE
  centers$actual_farm_relocation_measured <- FALSE
  write_csv(coverage, "coffee_redistribution_coverage.csv")
  write_csv(concentration, "coffee_concentration_series.csv")
  write_csv(centers, "coffee_geographic_centers_annual.csv")

  # --------------------------------------------------------------------
  # 4. Period means, share changes and period-center displacement.
  #    No percent change is computed when the baseline is zero.
  # --------------------------------------------------------------------
  period_mean <- function(sp, selected_years, variable) {
    z <- panel[panel$species == sp & panel$year %in% selected_years, , drop = FALSE]
    ans <- tapply(z[[variable]], z$municipality_code, mean)
    as.numeric(ans[panel_codes])
  }
  shift_list <- municipal_list <- state_list <- list()
  ns <- 0L
  for (sp in species_levels) {
    m <- labels
    m$species <- sp
    m$baseline_production_t <- period_mean(sp, baseline_years, "production_t")
    m$recent_production_t <- period_mean(sp, recent_years, "production_t")
    m$baseline_area_ha <- period_mean(sp, baseline_years, "area_harvested_ha")
    m$recent_area_ha <- period_mean(sp, recent_years, "area_harvested_ha")
    if (anyNA(m)) stop("Unexpected NA in complete-panel period means.", call. = FALSE)
    m$production_change_t <- m$recent_production_t - m$baseline_production_t
    m$area_change_ha <- m$recent_area_ha - m$baseline_area_ha
    m$production_change_pct <- pct_change(m$baseline_production_t, m$recent_production_t)
    m$area_change_pct <- pct_change(m$baseline_area_ha, m$recent_area_ha)
    m$baseline_production_share_pct <- 100 * m$baseline_production_t / sum(m$baseline_production_t)
    m$recent_production_share_pct <- 100 * m$recent_production_t / sum(m$recent_production_t)
    m$production_share_change_pp <- m$recent_production_share_pct - m$baseline_production_share_pct
    m$baseline_area_share_pct <- 100 * m$baseline_area_ha / sum(m$baseline_area_ha)
    m$recent_area_share_pct <- 100 * m$recent_area_ha / sum(m$recent_area_ha)
    m$area_share_change_pp <- m$recent_area_share_pct - m$baseline_area_share_pct
    m$production_window_status <- ifelse(m$baseline_production_t > 0 & m$recent_production_t > 0,
      "Positive in both windows", ifelse(m$baseline_production_t == 0 & m$recent_production_t > 0,
      "Zero baseline; positive recent", ifelse(m$baseline_production_t > 0 & m$recent_production_t == 0,
      "Positive baseline; zero recent", "Zero in both windows")))
    m$historical_boundaries_harmonized <- FALSE
    municipal_list[[sp]] <- m
    for (st in sort(unique(m$state))) {
      z <- m[m$state == st, , drop = FALSE]
      state_list[[paste(sp, st, sep = "_")]] <- data.frame(
        state = st, species = sp, fixed_panel_codes = nrow(z),
        baseline_production_t = sum(z$baseline_production_t), recent_production_t = sum(z$recent_production_t),
        production_change_t = sum(z$production_change_t),
        contribution_to_species_production_change_pp = 100 * sum(z$production_change_t) / sum(m$baseline_production_t),
        baseline_production_share_pct = sum(z$baseline_production_share_pct),
        recent_production_share_pct = sum(z$recent_production_share_pct),
        production_share_change_pp = sum(z$production_share_change_pp),
        baseline_area_ha = sum(z$baseline_area_ha), recent_area_ha = sum(z$recent_area_ha),
        area_share_change_pp = sum(z$area_share_change_pp), stringsAsFactors = FALSE
      )
    }
    for (wn in c("Production", "Harvested_area")) {
      w0 <- if (wn == "Production") m$baseline_production_t else m$baseline_area_ha
      w1 <- if (wn == "Production") m$recent_production_t else m$recent_area_ha
      for (cd in names(coordinate_definitions)) {
        a <- center_values(w0, coordinate_definitions[[cd]])
        b <- center_values(w1, coordinate_definitions[[cd]])
        loc <- to_lonlat(c(a[1L], b[1L]), c(a[2L], b[2L]))
        ns <- ns + 1L
        shift_list[[ns]] <- data.frame(
          species = sp, weighting = wn, coordinate_definition = cd,
          baseline_period = "2012 to 2015", recent_period = "2021 to 2024",
          fixed_panel_codes = nrow(m), baseline_longitude = loc[1L, 1L], baseline_latitude = loc[1L, 2L],
          recent_longitude = loc[2L, 1L], recent_latitude = loc[2L, 2L],
          longitude_change_deg = loc[2L, 1L] - loc[1L, 1L],
          latitude_change_deg = loc[2L, 2L] - loc[1L, 2L],
          grid_easting_change_km = unname((b[1L] - a[1L]) / 1000),
          grid_northing_change_km = unname((b[2L] - a[2L]) / 1000),
          projected_center_displacement_km = unname(sqrt(sum((b - a)^2)) / 1000),
          baseline_total_weight = sum(w0), recent_total_weight = sum(w1),
          reference_boundary_year = 2024L, historical_boundaries_harmonized = FALSE,
          actual_farm_relocation_measured = FALSE, climate_attribution_performed = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  shifts <- do.call(rbind, shift_list)
  municipal <- do.call(rbind, municipal_list)
  state_changes <- do.call(rbind, state_list)
  for (sp in species_levels) {
    z <- municipal[municipal$species == sp, , drop = FALSE]
    if (abs(sum(z$production_share_change_pp)) > 1e-8 || abs(sum(z$area_share_change_pp)) > 1e-8) {
      stop("Share changes do not sum to zero for ", sp, call. = FALSE)
    }
  }
  rownames(shifts) <- rownames(municipal) <- rownames(state_changes) <- NULL
  write_csv(shifts, "coffee_geographic_shift_by_species.csv")
  write_csv(municipal, "coffee_municipal_redistribution.csv")
  write_csv(state_changes, "coffee_state_redistribution.csv")

  # --------------------------------------------------------------------
  # 5. A four-panel diagnostic, not an altitude figure or causal result.
  #    All panels have their own axes. Comparison axes use common limits.
  # --------------------------------------------------------------------
  font_family <- "Times New Roman"
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    installed_families <- unique(systemfonts::system_fonts()$family)
    if (!font_family %in% installed_families) {
      candidates <- c("Liberation Serif", "DejaVu Serif", "serif")
      available <- candidates[candidates %in% c(installed_families, "serif")]
      font_family <- available[1L]
      message("Times New Roman is unavailable. Figure font: ", font_family)
    }
  } else {
    font_family <- "serif"
    message("Font detection unavailable. Using the device serif family.")
  }
  theme_rogerio <- function(base_size = 14) {
    ggplot2::theme_minimal(base_size = base_size, base_family = font_family) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black", linewidth = 0.45),
        axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.4),
        axis.ticks.length = grid::unit(0.15, "cm"),
        axis.text = ggplot2::element_text(colour = "black"),
        legend.position = "top", legend.title = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(face = "bold"),
        panel.spacing = grid::unit(0.8, "lines"),
        plot.title = ggplot2::element_text(face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11),
        plot.margin = ggplot2::margin(10, 16, 10, 10)
      )
  }
  cplot <- centers[centers$coordinate_definition == "Municipal_polygon_centroid" &
                   centers$species %in% c("Arabica", "Canephora"), , drop = FALSE]
  cplot$weighting <- factor(cplot$weighting, levels = c("Production", "Harvested_area"),
                           labels = c("Production", "Harvested area"))
  qplot <- concentration[concentration$species %in% c("Arabica", "Canephora"), , drop = FALSE]
  qplot$weighting <- factor(qplot$weighting, levels = c("Production", "Harvested_area"),
                           labels = c("Production", "Harvested area"))
  common_limits <- function(x) {
    r <- range(c(0, x), finite = TRUE)
    pad <- max(diff(r) * 0.13, 5)
    r + c(-pad, pad)
  }
  xlim <- common_limits(cplot$grid_easting_change_from_2012_km)
  ylim <- common_limits(cplot$grid_northing_change_from_2012_km)
  line_scale <- function() ggplot2::scale_linetype_manual(
    values = c("Production" = "solid", "Harvested area" = "dashed"), drop = FALSE
  )
  plots <- list()
  for (sp in c("Arabica", "Canephora")) {
    z <- cplot[cplot$species == sp, , drop = FALSE]
    z <- z[order(z$weighting, z$year), , drop = FALSE]
    endpoint <- z[z$year == max(years), , drop = FALSE]
    # Lines follow years, not x order. Closed endpoints indicate 2024.
    plots[[paste0("center_", sp)]] <- ggplot2::ggplot(z,
      ggplot2::aes(grid_easting_change_from_2012_km, grid_northing_change_from_2012_km,
                   group = weighting, linetype = weighting)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, linetype = "dotted") +
      ggplot2::geom_vline(xintercept = 0, linewidth = 0.25, linetype = "dotted") +
      ggplot2::geom_path(linewidth = 0.8) +
      ggplot2::geom_point(data = endpoint, size = 2.3, show.legend = FALSE) +
      ggplot2::annotate("point", x = 0, y = 0, shape = 1, size = 2.6) +
      line_scale() +
      ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      ggplot2::labs(title = sp, subtitle = "Weighted center: open = 2012; filled = 2024",
        x = "Change in grid easting (km)", y = "Change in grid northing (km)") + theme_rogerio()
    v <- qplot[qplot$species == sp, , drop = FALSE]
    plots[[paste0("concentration_", sp)]] <- ggplot2::ggplot(v,
      ggplot2::aes(year, top50_share_pct, linetype = weighting)) +
      ggplot2::geom_line(linewidth = 0.85) + line_scale() +
      ggplot2::scale_x_continuous(breaks = seq(2012, 2024, 3), limits = range(years)) +
      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      ggplot2::labs(title = sp, subtitle = "Concentration across reporting municipalities",
        x = "Year", y = "Share in the 50 largest municipalities (%)") + theme_rogerio()
  }
  fig <- patchwork::wrap_plots(
    plots$center_Arabica, plots$center_Canephora,
    plots$concentration_Arabica, plots$concentration_Canephora,
    ncol = 2, heights = c(1.25, 1), guides = "collect"
  ) + patchwork::plot_annotation(
    title = "Geographic redistribution of Brazilian coffee",
    subtitle = paste0("Fixed panel of ", format(length(panel_codes), big.mark = ","),
      " municipal codes | 2012 to 2024 | Municipal reference points, not farm locations"),
    tag_levels = "a",
    theme = ggplot2::theme(text = ggplot2::element_text(family = font_family),
      plot.title = ggplot2::element_text(face = "bold", size = 17),
      plot.subtitle = ggplot2::element_text(size = 11),
      plot.tag = ggplot2::element_text(face = "bold")))
  fig <- fig & ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank())
  save_plot <- function(ext) {
    target <- file.path(fig_dir, paste0("coffee_geographic_redistribution_draft.", ext))
    tmp <- tempfile("coffee08_", tmpdir = fig_dir, fileext = paste0(".", ext))
    on.exit(unlink(tmp), add = TRUE)
    if (ext == "png") {
      device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else grDevices::png
      ggplot2::ggsave(tmp, plot = fig, device = device, width = 12.5, height = 10.5,
                      units = "in", dpi = 300, bg = "white")
    } else {
      if (!capabilities("cairo")) return("Not exported: Cairo unavailable")
      ggplot2::ggsave(tmp, plot = fig, device = grDevices::cairo_pdf, width = 12.5,
                      height = 10.5, units = "in", family = font_family, bg = "white")
    }
    if (!file.copy(tmp, target, overwrite = TRUE)) stop("Cannot save: ", target, call. = FALSE)
    "Saved"
  }
  png_status <- tryCatch(save_plot("png"), error = function(e) paste("Failed:", conditionMessage(e)))
  pdf_status <- tryCatch(save_plot("pdf"), error = function(e) paste("Failed:", conditionMessage(e)))

  # --------------------------------------------------------------------
  # 6. Provenance, definitions and a concise run summary.
  # --------------------------------------------------------------------
  summary <- data.frame(check = c(
    "execution_status", "analysis_period", "fixed_panel_municipal_codes",
    "minimum_production_coverage_pct", "minimum_area_coverage_pct", "input_md5",
    "spatial_registry_md5", "official_validation_passed_for_input", "reference_boundary_year",
    "historical_boundaries_harmonized", "coffee_field_locations_measured",
    "coffee_field_elevation_estimated", "climate_attribution_performed", "font_family",
    "png_status", "pdf_status", "original_files_modified"),
    result = as.character(c(if (png_status == "Saved") "COMPLETED" else "TABLES_COMPLETE_FIGURE_ERROR",
      "2012 to 2024", length(panel_codes), min(coverage$production_coverage_pct),
      min(coverage$harvested_area_coverage_pct), input_md5,
      unname(tools::md5sum(registry_file)), TRUE, 2024, FALSE, FALSE, FALSE, FALSE,
      font_family, png_status, pdf_status, FALSE)), stringsAsFactors = FALSE)
  write_csv(summary, "coffee_redistribution_summary.csv")
  notes <- c(
    "COFFE-STUDY | DESCRIPTIVE GEOGRAPHIC REDISTRIBUTION, STEP 08",
    paste("Executed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Input:", input_file), paste("Input MD5:", input_md5),
    paste("Metric coordinate system:", metric_crs),
    "The official-total validation is required and linked to the unchanged RDS by MD5.",
    "Scope: 2012 to 2024. Total, Arabica and Canephora are always separate.",
    "A code enters the fixed panel only with production and harvested area observed in all 13 years and all three categories.",
    "Zero is retained. Missing data are never imputed as zero; excluded codes and coverage are exported.",
    "The panel can differ from the eight-year-window panel used for Figure 1 decomposition.",
    "Fixed municipal CODES do not imply stable historical BOUNDARIES.",
    "Centroids from the full 2024 polygons are the primary location proxy.",
    "Interior municipal points provide a proxy sensitivity check, not independent validation or error bars.",
    "All source municipalities and the same panel are used for both species; no farm locations are estimated.",
    "A center is sum(weight * projected reference coordinate) / sum(weight).",
    "Annual weights are municipal production (tonnes) or harvested area (ha).",
    "Period weights are municipal arithmetic means over 2012 to 2015 or 2021 to 2024.",
    "A period center is NOT the unweighted arithmetic mean of annual centers.",
    "Displacements use the stated LAEA grid: easting/northing are projected coordinate changes.",
    "Projected displacement is not an exact geodesic distance, a farm movement distance or a migration rate.",
    "Inverse-transformed longitude and latitude are supplied for context.",
    "The trajectory diagram gives each species and each weighting its own 2012 origin.",
    "Therefore it compares changes, not absolute locations or distances between species.",
    "Annual top-50 shares re-rank municipalities each year. They measure concentration, not performance of a fixed top-50 group.",
    "HHI = sum(s_i^2), with shares on a 0 to 1 scale. Equivalent municipal count = 1/HHI.",
    "Municipal and state share changes are recent share minus baseline share, in percentage points.",
    "Relative production/area changes are undefined when their baseline is zero and remain NA.",
    "Zero-to-positive or positive-to-zero period classifications describe observations, not confirmed farm establishment or abandonment.",
    "Production-weighted centers can shift through productivity changes without cultivated-area relocation.",
    "Harvested-area centers do not cover all planted area or reveal movement within municipalities.",
    "No altitude, climate trend, climate attribution or boundary-harmonized migration is estimated.",
    "Concentration alone does not quantify correlated climate exposure or vulnerability.",
    "The figure is a diagnostic contribution to the spatial analysis, not an additional final main figure.",
    "All original inputs and previously generated figures remain unchanged."
  )
  writeLines(notes, file.path(out_dir, "coffee_redistribution_method_notes.txt"), useBytes = TRUE)
  utils::capture.output(sessionInfo(), file = file.path(out_dir, "sessionInfo_step08.txt"))
  cat("\n============================================================\n")
  cat("COFFEE GEOGRAPHIC REDISTRIBUTION: DESCRIPTIVE OUTPUTS\n")
  cat("============================================================\n")
  cat("Fixed municipal-code panel:", length(panel_codes), "\n")
  cat("Minimum production coverage (%):", min(coverage$production_coverage_pct), "\n")
  cat("Minimum area coverage (%):", min(coverage$harvested_area_coverage_pct), "\n")
  print(shifts[shifts$coordinate_definition == "Municipal_polygon_centroid",
               c("species", "weighting", "longitude_change_deg", "latitude_change_deg",
                 "projected_center_displacement_km")], row.names = FALSE)
  cat("\nPNG:", png_status, "\nPDF:", pdf_status, "\n")
  cat("\nTables:", out_dir, "\nFigure:", fig_dir, "\n")
  cat("\nPlease return coffee_geographic_shift_by_species.csv,\n")
  cat("coffee_redistribution_summary.csv and the draft PNG.\n")
  invisible(list(shifts = shifts, concentration = concentration, summary = summary))
}

invisible(main())
