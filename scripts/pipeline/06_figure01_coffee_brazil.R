# ======================================================================
# COFFE-STUDY | 06_figure01_coffee_brazil.R
# First descriptive figure: municipal distribution, production, area,
# aggregate productivity and an accounting decomposition of production.
# Run the ENTIRE file with Source in RStudio.
#
# Uses the outputs of steps 04 and 05. NO new data download.
# Original data and geographic files are never overwritten.
#
# Scientific scope:
# - This is an exploratory, descriptive figure, not climate attribution.
# - Municipal boundaries locate reporting units, not individual coffee fields.
# - The current species labels and units are inherited from the input.
# - Official totals, original metadata and historical boundaries still
#   require verification before interpreting final spatial trends.
# - Total coffee must NEVER be added to Arabica and Canephora.
#
# Plotting documentation:
# https://ggplot2.tidyverse.org/reference/ggsf.html
# https://patchwork.data-imaginist.com/reference/wrap_plots.html
# https://r-spatial.github.io/sf/reference/geos_unary.html
# ======================================================================

main <- function() {
  # --------------------------------------------------------------------
  # 1. Configuration
  # --------------------------------------------------------------------
  base_dir <- Sys.getenv(
    "COFFEE_STUDY_DIR",
    unset = "."
  )
  first_year <- 2012L
  last_year <- 2024L
  baseline_years <- 2012:2015
  recent_years <- 2021:2024
  species_order <- c("Total", "Arabica", "Canephora")
  # Four-year windows give equal representation to odd/even calendar years.
  # This is not a model of biennial bearing and does not remove all variability.
  plot_simplification_m <- 250
  figure_width <- 13
  figure_height <- 13.5
  png_dpi <- 400

  processed_dir <- file.path(base_dir, "02_data_processed")
  spatial_dir <- file.path(processed_dir, "spatial")
  audit05_dir <- file.path(base_dir, "04_outputs", "spatial_audit")
  table_dir <- file.path(base_dir, "04_outputs", "figure01_brazil")
  figure_dir <- file.path(base_dir, "05_figures", "Figure01_Brazil")
  for (p in c(table_dir, figure_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create directory: ", p, call. = FALSE)
  }
  previous_options <- options(scipen = 999)
  on.exit(options(previous_options), add = TRUE)

  packages <- c("sf", "ggplot2", "patchwork")
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing required package: ", pkg)
      utils::install.packages(pkg, repos = "https://cloud.r-project.org")
    }
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package unavailable: ", pkg, ". Install it and run Source again.", call. = FALSE)
    }
  }

  fail <- function(...) stop(..., call. = FALSE)
  write_csv_safe <- function(x, file) {
    tmp <- tempfile("coffee_", tmpdir = dirname(file), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    if (!file.copy(tmp, file, overwrite = TRUE)) fail("Could not save: ", file)
  }
  read_csv_text <- function(file) {
    utils::read.csv(file, colClasses = "character", stringsAsFactors = FALSE,
                    check.names = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  sum_available <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  safe_ratio <- function(numerator, denominator) {
    if (length(denominator) == 1L && is.finite(denominator) && denominator > 0)
      numerator / denominator else NA_real_
  }
  required_columns <- function(x, required, label) {
    absent <- setdiff(required, names(x))
    if (length(absent)) fail(label, ": missing columns: ", paste(absent, collapse = ", "))
  }
  paired_ok <- function(x) {
    !is.na(x$production_t) & !is.na(x$area_harvested_ha) &
      is.finite(x$production_t) & is.finite(x$area_harvested_ha) &
      x$production_t >= 0 & x$area_harvested_ha >= 0 &
      !(x$production_t > 0 & x$area_harvested_ha == 0)
  }

  # --------------------------------------------------------------------
  # 2. Read existing data and check the link to step 05
  # --------------------------------------------------------------------
  input_file <- file.path(processed_dir, "coffee_brazil_municipality_audited_1974_2024.rds")
  geometry_file <- file.path(spatial_dir, "coffee_municipality_geometries_2024.rds")
  inventory_file <- file.path(audit05_dir, "coffee_reference_boundary_inventory.csv")
  summary05_file <- file.path(audit05_dir, "coffee_spatial_summary.csv")
  needed <- c(input_file, geometry_file, inventory_file, summary05_file)
  if (any(!file.exists(needed))) {
    fail("Missing output from step 04 or 05:\n", paste(needed[!file.exists(needed)], collapse = "\n"))
  }
  message("Reading existing audited production data. No data download.")
  dat <- as.data.frame(readRDS(input_file), stringsAsFactors = FALSE)
  required_columns(dat, c("municipality_code", "municipality", "state", "year", "species",
                          "production_t", "area_harvested_ha"), "Audited data")
  dat$municipality_code <- as.character(dat$municipality_code)
  dat$species <- as.character(dat$species)
  if (!is.numeric(dat$year) || anyNA(dat$year)) fail("Invalid year column.")
  if (!is.numeric(dat$production_t) || !is.numeric(dat$area_harvested_ha)) fail("Non-numeric measurements.")
  if (anyNA(dat$municipality_code) || any(!grepl("^[0-9]{7}$", dat$municipality_code))) fail("Invalid municipal codes.")
  if (anyNA(dat$species) || any(!dat$species %in% species_order)) fail("Unexpected species labels.")
  if (anyDuplicated(paste(dat$municipality_code, dat$year, dat$species, sep = "|"))) fail("Duplicated input keys.")
  if (any(dat$production_t < 0, na.rm = TRUE) || any(dat$area_harvested_ha < 0, na.rm = TRUE)) fail("Negative measurements.")
  if (any(!is.finite(dat$production_t) & !is.na(dat$production_t)) ||
      any(!is.finite(dat$area_harvested_ha) & !is.na(dat$area_harvested_ha))) fail("Infinite measurements.")
  input_md5 <- unname(tools::md5sum(input_file))
  summary05 <- read_csv_text(summary05_file)
  required_columns(summary05, c("check", "result"), "Step 05 summary")
  recorded_hash <- summary05$result[summary05$check == "input_md5"]
  if (length(recorded_hash) != 1L || is.na(recorded_hash) || input_md5 != recorded_hash) {
    fail("The input file differs from the one used in step 05. Rerun step 05 first.")
  }
  dat <- dat[dat$year >= first_year & dat$year <= last_year, , drop = FALSE]
  if (!nrow(dat) || !identical(sort(unique(as.integer(dat$year))), first_year:last_year)) {
    fail("The complete requested year range is not present.")
  }
  counts <- table(dat$year, factor(dat$species, levels = species_order))
  if (any(counts == 0L)) fail("A requested year/species combination is absent.")
  all_input_codes <- sort(unique(dat$municipality_code))
  n_codes <- length(all_input_codes)
  message("Analysis window: ", first_year, " to ", last_year, "; ", n_codes, " municipal reporting codes.")

  # --------------------------------------------------------------------
  # 3. Available-record annual summaries and balanced-panel sensitivity
  #    Aggregate productivity = 1000 * sum(P) / sum(A), using the SAME rows.
  #    Missing measurements are never replaced with zero.
  # --------------------------------------------------------------------
  annual_summary <- function(x, panel_name) {
    out <- vector("list", length(first_year:last_year) * length(species_order))
    k <- 0L
    for (yr in first_year:last_year) for (sp in species_order) {
      k <- k + 1L
      g <- x[x$year == yr & x$species == sp, , drop = FALSE]
      ok <- paired_ok(g)
      pp <- if (any(ok)) sum(g$production_t[ok]) else NA_real_
      aa <- if (any(ok)) sum(g$area_harvested_ha[ok]) else NA_real_
      out[[k]] <- data.frame(
        panel = panel_name, year = yr, species = sp, records = nrow(g),
        paired_records = sum(ok),
        production_t_sum_available = sum_available(g$production_t),
        harvested_area_ha_sum_available = sum_available(g$area_harvested_ha),
        production_t_paired = pp, harvested_area_ha_paired = aa,
        yield_kg_ha_from_paired_sums = 1000 * safe_ratio(pp, aa),
        positive_production_records = sum(g$production_t > 0, na.rm = TRUE),
        missing_production_records = sum(is.na(g$production_t)),
        missing_area_records = sum(is.na(g$area_harvested_ha)),
        official_national_totals_verified = FALSE,
        stringsAsFactors = FALSE
      )
    }
    do.call(rbind, out)
  }
  annual <- annual_summary(dat, "Available municipal records")
  all_year_valid_counts <- table(dat$municipality_code[paired_ok(dat)])
  complete_n <- length(first_year:last_year) * length(species_order)
  full_panel_codes <- names(all_year_valid_counts)[all_year_valid_counts == complete_n]
  if (!length(full_panel_codes)) fail("No complete municipal panel for the sensitivity check.")
  balanced_annual <- annual_summary(dat[dat$municipality_code %in% full_panel_codes, , drop = FALSE],
                                     "Fixed municipal code panel")
  write_csv_safe(rbind(annual, balanced_annual), file.path(table_dir, "figure01_brazil_annual_series.csv"))

  # Compare annual total production/area with the sum of its two species.
  additivity <- do.call(rbind, lapply(first_year:last_year, function(yr) {
    q <- annual[annual$year == yr, ]
    q <- q[match(species_order, q$species), ]
    data.frame(year = yr,
               production_difference_t = q$production_t_sum_available[1] - sum(q$production_t_sum_available[2:3]),
               area_difference_ha = q$harvested_area_ha_sum_available[1] - sum(q$harvested_area_ha_sum_available[2:3]))
  }))
  write_csv_safe(additivity, file.path(table_dir, "figure01_brazil_species_sum_check.csv"))
  if (anyNA(additivity) || any(abs(additivity$production_difference_t) > 0.001) ||
      any(abs(additivity$area_difference_ha) > 0.001)) {
    fail("The aggregate species sum check failed. Inspect figure01_brazil_species_sum_check.csv.")
  }

  # --------------------------------------------------------------------
  # 4. Four-year municipal means for maps
  #    A mean requires all four production observations for that species.
  # --------------------------------------------------------------------
  recent <- dat[dat$year %in% recent_years, , drop = FALSE]
  groups <- split(seq_len(nrow(recent)), paste(recent$municipality_code, recent$species, sep = "|"))
  map_data <- do.call(rbind, lapply(groups, function(ii) {
    g <- recent[ii, , drop = FALSE]
    complete <- nrow(g) == length(recent_years) &&
      setequal(g$year, recent_years) && all(!is.na(g$production_t))
    data.frame(
      municipality_code = g$municipality_code[1], municipality_label = g$municipality[which.max(g$year)],
      state = g$state[1], species = g$species[1], first_year = min(recent_years), last_year = max(recent_years),
      production_observations = sum(!is.na(g$production_t)), complete_window = complete,
      mean_annual_production_t = if (complete) mean(g$production_t) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(map_data) <- NULL
  write_csv_safe(map_data, file.path(table_dir, "figure01_brazil_municipal_map_data.csv"))

  # --------------------------------------------------------------------
  # 5. Exact accounting decomposition, with a common fixed code sample
  #    in BOTH windows and ALL three categories.
  #
  #    P = A * Y / 1000, with P in tonnes and Y in kg/ha.
  #    dP_area  = (A1-A0) * (Y1+Y0) / 2000
  #    dP_yield = (Y1-Y0) * (A1+A0) / 2000
  #    dP_area + dP_yield = P1-P0 exactly (up to floating-point precision).
  #    This is an accounting identity, NOT a causal or climate decomposition.
  # --------------------------------------------------------------------
  selected_years <- c(baseline_years, recent_years)
  z <- dat[dat$year %in% selected_years, , drop = FALSE]
  valid_counts <- table(z$municipality_code[paired_ok(z)])
  expected_records <- length(selected_years) * length(species_order)
  decomposition_codes <- names(valid_counts)[valid_counts == expected_records]
  if (!length(decomposition_codes)) fail("No common complete panel for the two comparison windows.")
  membership <- data.frame(
    municipality_code = all_input_codes,
    included_in_decomposition = all_input_codes %in% decomposition_codes,
    included_in_full_time_series_sensitivity = all_input_codes %in% full_panel_codes,
    stringsAsFactors = FALSE
  )
  write_csv_safe(membership, file.path(table_dir, "figure01_brazil_balanced_panel_membership.csv"))
  decomposition <- do.call(rbind, lapply(species_order, function(sp) {
    q <- z[z$species == sp & z$municipality_code %in% decomposition_codes, , drop = FALSE]
    b <- q[q$year %in% baseline_years, , drop = FALSE]
    r <- q[q$year %in% recent_years, , drop = FALSE]
    p0 <- sum(b$production_t) / length(baseline_years)
    p1 <- sum(r$production_t) / length(recent_years)
    a0 <- sum(b$area_harvested_ha) / length(baseline_years)
    a1 <- sum(r$area_harvested_ha) / length(recent_years)
    if (p0 <= 0 || a0 <= 0 || a1 <= 0) fail("Undefined decomposition for ", sp)
    y0 <- 1000 * p0 / a0
    y1 <- 1000 * p1 / a1
    area_component <- (a1 - a0) * (y1 + y0) / 2000
    yield_component <- (y1 - y0) * (a1 + a0) / 2000
    residual <- area_component + yield_component - (p1 - p0)
    if (abs(residual) > max(0.000001, abs(p1 - p0) * 1e-9)) fail("Accounting identity check failed.")
    data.frame(
      species = sp, baseline_period = paste(range(baseline_years), collapse = " to "),
      recent_period = paste(range(recent_years), collapse = " to "),
      common_municipal_codes = length(decomposition_codes),
      baseline_mean_production_t = p0, recent_mean_production_t = p1,
      baseline_mean_harvested_area_ha = a0, recent_mean_harvested_area_ha = a1,
      baseline_aggregate_yield_kg_ha = y0, recent_aggregate_yield_kg_ha = y1,
      production_change_t = p1 - p0, area_contribution_t = area_component,
      productivity_contribution_t = yield_component, identity_residual_t = residual,
      production_change_pct = 100 * (p1 - p0) / p0,
      area_contribution_pp = 100 * area_component / p0,
      productivity_contribution_pp = 100 * yield_component / p0,
      baseline_coverage_pct_of_available_production = 100 * p0 /
        mean(annual$production_t_sum_available[annual$species == sp & annual$year %in% baseline_years]),
      recent_coverage_pct_of_available_production = 100 * p1 /
        mean(annual$production_t_sum_available[annual$species == sp & annual$year %in% recent_years]),
      historical_boundaries_harmonized = FALSE, causal_attribution = FALSE,
      stringsAsFactors = FALSE
    )
  }))
  write_csv_safe(decomposition, file.path(table_dir, "figure01_brazil_production_decomposition.csv"))
  message("Descriptive tables saved. Preparing the municipal maps...")

  # --------------------------------------------------------------------
  # 6. Cached municipal geography
  #    Add reference municipalities absent from the master as MISSING.
  #    Operational lake areas are not interpreted as municipalities.
  # --------------------------------------------------------------------
  geo <- readRDS(geometry_file)
  if (!inherits(geo, "sf")) fail("The geometry RDS is not an sf object.")
  required_columns(geo, c("municipality_code", "municipality_name_reference"), "Geometry")
  geo <- geo[, c("municipality_code", "municipality_name_reference"), drop = FALSE]
  geo$municipality_code <- as.character(geo$municipality_code)
  if (anyDuplicated(geo$municipality_code)) fail("Duplicate geometry codes.")
  if (is.na(sf::st_crs(geo))) fail("Missing geometry coordinate reference system.")
  if (length(setdiff(all_input_codes, geo$municipality_code))) fail("Unmatched production codes remain.")
  inventory <- read_csv_text(inventory_file)
  required_columns(inventory, c("municipality_code", "municipality_name_reference"), "Reference inventory")
  operational <- inventory$municipality_code %in% c("4300001", "4300002")
  additional_codes <- setdiff(inventory$municipality_code[!operational], geo$municipality_code)
  if (length(additional_codes)) {
    shp_file <- file.path(base_dir, "01_data_raw", "IBGE_geography", "2024", "extracted", "BR_Municipios_2024.shp")
    if (!file.exists(shp_file)) {
      fail("Cached IBGE shapefile missing. Rerun step 05 to restore it:\n", shp_file)
    }
    message("Reading ", length(additional_codes), " additional reference municipalities from the cached shapefile.")
    # Filter the original layer by exact code. Names are not used as join keys.
    sql <- paste0('SELECT CD_MUN, NM_MUN FROM "BR_Municipios_2024" WHERE CD_MUN IN (',
                  paste(sprintf("'%s'", additional_codes), collapse = ","), ")")
    extra <- sf::st_read(shp_file, query = sql, quiet = TRUE, stringsAsFactors = FALSE)
    required_columns(extra, c("CD_MUN", "NM_MUN"), "Additional reference geometries")
    if (!setequal(as.character(extra$CD_MUN), additional_codes)) fail("Additional geometry selection is incomplete.")
    extra <- sf::st_sf(municipality_code = as.character(extra$CD_MUN),
                       municipality_name_reference = as.character(extra$NM_MUN),
                       geometry = sf::st_geometry(extra))
    extra <- sf::st_transform(extra, sf::st_crs(geo))
    geo <- rbind(geo, extra)
  }
  metric_crs <- "+proj=laea +lat_0=-15 +lon_0=-54 +datum=WGS84 +units=m +no_defs"
  geo <- sf::st_transform(geo, metric_crs)
  bad <- is.na(sf::st_is_valid(geo)) | !sf::st_is_valid(geo)
  if (any(bad)) sf::st_geometry(geo)[bad] <- sf::st_make_valid(sf::st_geometry(geo)[bad])
  if (any(sf::st_is_empty(geo)) || any(!sf::st_is_valid(geo))) fail("Invalid plotting geometry.")
  # Simplification is used for display ONLY. No measurement uses this geometry.
  message("Simplifying boundaries for display only...")
  original_geometry <- sf::st_geometry(geo)
  simplified <- sf::st_simplify(original_geometry, dTolerance = plot_simplification_m, preserveTopology = TRUE)
  good <- !sf::st_is_empty(simplified) & !is.na(sf::st_is_valid(simplified)) & sf::st_is_valid(simplified)
  simplified[!good] <- original_geometry[!good]
  sf::st_geometry(geo) <- simplified
  common_bbox <- sf::st_bbox(geo)
  pad_x <- (common_bbox[["xmax"]] - common_bbox[["xmin"]]) * 0.012
  pad_y <- (common_bbox[["ymax"]] - common_bbox[["ymin"]]) * 0.012
  x_limits <- c(common_bbox[["xmin"]] - pad_x, common_bbox[["xmax"]] + pad_x)
  y_limits <- c(common_bbox[["ymin"]] - pad_y, common_bbox[["ymax"]] + pad_y)

  # --------------------------------------------------------------------
  # 7. Theme and plots
  # --------------------------------------------------------------------
  font_family <- "Times New Roman"
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    families <- unique(systemfonts::system_fonts()$family)
    if (!any(tolower(families) == tolower(font_family))) {
      font_family <- "serif"
      message("Times New Roman not found; using the system serif font. Recorded in the notes.")
    }
  }
  theme_rogerio <- function(base_size = 14) {
    ggplot2::theme_minimal(base_size = base_size, base_family = font_family) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black", linewidth = 0.45),
        axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.4),
        axis.ticks.length = grid::unit(0.15, "cm"),
        axis.text = ggplot2::element_text(colour = "black"),
        axis.title = ggplot2::element_text(colour = "black"),
        legend.position = "top", legend.title = ggplot2::element_blank(),
        legend.key.width = grid::unit(1.1, "cm"),
        strip.text = ggplot2::element_text(face = "bold"),
        panel.spacing = grid::unit(0.8, "lines"),
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 11),
        plot.caption = ggplot2::element_text(size = 10, hjust = 0),
        plot.margin = ggplot2::margin(10, 13, 9, 10)
      )
  }
  # Identical production categories, extent and scale in both map panels.
  map_levels <- c("0", "Up to 100", "100 to 1,000", "1,000 to 10,000", "10,000 to 50,000", "Over 50,000", "Missing (x)")
  map_fills <- stats::setNames(c("white", "grey84", "grey67", "grey48", "grey29", "grey8", "grey94"), map_levels)
  map_coverage <- list()
  make_map <- function(sp, letter) {
    m <- geo
    q <- map_data[map_data$species == sp, , drop = FALSE]
    j <- match(m$municipality_code, q$municipality_code)
    m$mean_annual_production_t <- q$mean_annual_production_t[j]
    m$present_in_input <- !is.na(j)
    m$complete_window <- !is.na(m$mean_annual_production_t)
    m$production_class <- as.character(cut(m$mean_annual_production_t,
      breaks = c(-Inf, 0, 100, 1000, 10000, 50000, Inf), labels = map_levels[1:6], right = TRUE))
    m$production_class[is.na(m$production_class)] <- "Missing (x)"
    m$production_class <- factor(m$production_class, levels = map_levels)
    missing_points <- sf::st_point_on_surface(sf::st_geometry(m[!m$complete_window, , drop = FALSE]))
    p <- ggplot2::ggplot(m) +
      ggplot2::geom_sf(ggplot2::aes(fill = production_class), colour = "grey82", linewidth = 0.025) +
      ggplot2::scale_fill_manual(values = map_fills, breaks = map_levels, drop = FALSE) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
      ggplot2::coord_sf(crs = sf::st_crs(metric_crs), datum = sf::st_crs(4326),
                       xlim = x_limits, ylim = y_limits, expand = FALSE) +
      ggplot2::labs(title = paste0(letter, "  ", sp),
                    subtitle = paste0("Mean annual production (t), ", min(recent_years), " to ", max(recent_years)),
                    x = "Longitude", y = "Latitude") + theme_rogerio() +
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.45),
                     axis.line = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10),
                     legend.key.width = grid::unit(0.7, "cm"))
    if (length(missing_points)) {
      p <- p + ggplot2::geom_sf(data = sf::st_sf(geometry = missing_points), inherit.aes = FALSE,
                                shape = 4, size = 0.8, stroke = 0.22, colour = "grey30", show.legend = FALSE)
    }
    map_coverage[[sp]] <<- data.frame(species = sp, reference_municipalities = nrow(m),
      missing_input_municipalities = sum(!m$present_in_input),
      incomplete_input_windows = sum(m$present_in_input & !m$complete_window),
      mapped_complete_windows = sum(m$complete_window),
      mapped_positive_municipalities = sum(m$mean_annual_production_t > 0, na.rm = TRUE),
      stringsAsFactors = FALSE)
    p
  }
  A <- make_map("Arabica", "a")
  B <- make_map("Canephora", "b")
  write_csv_safe(do.call(rbind, map_coverage), file.path(table_dir, "figure01_brazil_map_coverage.csv"))

  annual$species <- factor(annual$species, levels = species_order)
  line_types <- c(Total = "solid", Arabica = "longdash", Canephora = "dotdash")
  make_series <- function(column, divisor, title, y_label) {
    g <- annual
    g$plot_value <- g[[column]] / divisor
    ggplot2::ggplot(g, ggplot2::aes(x = year, y = plot_value, linetype = species, group = species)) +
      ggplot2::geom_line(linewidth = 0.85, colour = "black") +
      ggplot2::scale_linetype_manual(values = line_types, breaks = species_order, drop = FALSE) +
      ggplot2::scale_x_continuous(breaks = seq(first_year, last_year, by = 2),
                                 limits = c(first_year, last_year), expand = ggplot2::expansion(mult = 0.01)) +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0.01, 0.09))) +
      ggplot2::labs(title = title, x = NULL, y = y_label) + theme_rogerio() +
      ggplot2::theme(legend.text = ggplot2::element_text(size = 11))
  }
  C <- make_series("production_t_sum_available", 1e6, "c  Production", "Production (million t)")
  D <- make_series("harvested_area_ha_sum_available", 1e6, "d  Harvested area", "Harvested area (million ha)")
  E <- make_series("yield_kg_ha_from_paired_sums", 1, "e  Aggregate productivity", "Productivity (kg/ha)")

  components <- c("Area", "Productivity", "Observed change")
  variables <- c("area_contribution_pp", "productivity_contribution_pp", "production_change_pct")
  component_data <- do.call(rbind, lapply(seq_along(components), function(i) {
    data.frame(species = decomposition$species, component = components[i],
               contribution = decomposition[[variables[i]]],
               y_position = match(decomposition$species, rev(species_order)) + c(-0.17, 0, 0.17)[i])
  }))
  component_data$component <- factor(component_data$component, levels = components)
  F <- ggplot2::ggplot(component_data, ggplot2::aes(x = contribution, y = y_position, shape = component)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey65", linewidth = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = contribution, yend = y_position),
                          colour = "grey60", linewidth = 0.4, show.legend = FALSE) +
    ggplot2::geom_point(size = 2.7, stroke = 0.8, colour = "black") +
    ggplot2::scale_shape_manual(values = c(Area = 15, Productivity = 17, "Observed change" = 1)) +
    ggplot2::scale_y_continuous(breaks = 1:3, labels = rev(species_order), limits = c(0.5, 3.5)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.10, 0.12))) +
    ggplot2::labs(title = "f  Area and productivity contributions",
                  subtitle = paste0(min(baseline_years), " to ", max(baseline_years), " versus ",
                                    min(recent_years), " to ", max(recent_years)),
                  x = "Change relative to baseline production (%)", y = NULL) +
    theme_rogerio() + ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  top <- patchwork::wrap_plots(A, B, ncol = 2, guides = "collect") &
    ggplot2::theme(legend.position = "top")
  lower <- patchwork::wrap_plots(C, D, E, F, ncol = 2, guides = "collect") &
    ggplot2::theme(legend.position = "top")
  figure <- patchwork::wrap_plots(top, lower, ncol = 1, heights = c(1.15, 1.8)) +
    patchwork::plot_annotation(
      title = "Coffee production in Brazil",
      subtitle = "Municipal distribution and production structure",
      caption = paste0("Exploratory analysis of available PAM records. Reference boundaries: 2024; not coffee-field locations.\n",
                        "Official totals and original metadata remain to be verified. Panel f is an accounting decomposition."),
      theme = ggplot2::theme(
        text = ggplot2::element_text(family = font_family),
        plot.title = ggplot2::element_text(face = "bold", size = 19),
        plot.subtitle = ggplot2::element_text(size = 13),
        plot.caption = ggplot2::element_text(size = 10, hjust = 0),
        plot.margin = ggplot2::margin(12, 12, 12, 12)
      )
    )

  # --------------------------------------------------------------------
  # 8. Exports. Cairo PDF avoids the base-PDF Times New Roman font problem.
  #    A PDF failure does not discard the PNG or numerical results.
  # --------------------------------------------------------------------
  png_file <- file.path(figure_dir, "Figure01_Brazil_coffee_descriptive.png")
  pdf_file <- file.path(figure_dir, "Figure01_Brazil_coffee_descriptive.pdf")
  message("Saving the figure...")
  export_plot <- function(target, kind) {
    tmp <- tempfile("coffee_figure_", tmpdir = dirname(target), fileext = paste0(".", kind))
    on.exit(unlink(tmp), add = TRUE)
    device_id <- NA_integer_
    result <- tryCatch({
      if (kind == "png") {
        if (requireNamespace("ragg", quietly = TRUE)) {
          ragg::agg_png(tmp, width = figure_width, height = figure_height,
                         units = "in", res = png_dpi, background = "white")
        } else if (capabilities("cairo")) {
          grDevices::png(tmp, width = figure_width, height = figure_height,
                         units = "in", res = png_dpi, type = "cairo", bg = "white")
        } else {
          grDevices::png(tmp, width = figure_width, height = figure_height,
                         units = "in", res = png_dpi, bg = "white")
        }
      } else {
        if (!capabilities("cairo")) stop("Cairo is unavailable; PNG was retained.")
        grDevices::cairo_pdf(tmp, width = figure_width, height = figure_height,
                             family = font_family, bg = "white", fallback_resolution = 300)
      }
      device_id <- grDevices::dev.cur()
      print(figure)
      grDevices::dev.off(device_id)
      device_id <- NA_integer_
      if (!file.exists(tmp) || is.na(file.size(tmp)) || file.size(tmp) == 0) stop("Empty figure export.")
      if (!file.copy(tmp, target, overwrite = TRUE)) stop("Cannot copy figure to final path.")
      "Saved"
    }, error = function(e) {
      if (!is.na(device_id) && device_id %in% grDevices::dev.list()) {
        try(grDevices::dev.off(device_id), silent = TRUE)
      }
      paste("Failed:", substr(conditionMessage(e), 1, 350))
    })
    result
  }
  png_status <- export_plot(png_file, "png")
  pdf_status <- if (identical(png_status, "Saved")) export_plot(pdf_file, "pdf") else "Not attempted: PNG export failed"

  # --------------------------------------------------------------------
  # 9. Reproducibility notes, caption and execution summary
  # --------------------------------------------------------------------
  figure_summary <- data.frame(
    check = c("analysis_first_year", "analysis_last_year", "input_municipality_codes", "reference_municipalities_mapped",
              "reference_municipalities_absent_from_input", "operational_reference_areas_excluded",
              "baseline_window", "recent_window", "decomposition_common_municipal_codes",
              "full_time_series_fixed_panel_codes", "font_family", "display_simplification_m",
              "png_status", "pdf_status", "official_national_totals_verified", "historical_boundaries_harmonized",
              "climate_attribution_performed", "input_md5", "original_data_modified"),
    result = as.character(c(first_year, last_year, n_codes, nrow(geo), length(additional_codes), sum(operational),
                            paste(range(baseline_years), collapse = " to "), paste(range(recent_years), collapse = " to "),
                            length(decomposition_codes), length(full_panel_codes), font_family, plot_simplification_m,
                            png_status, pdf_status, FALSE, FALSE, FALSE, input_md5, FALSE)),
    stringsAsFactors = FALSE
  )
  write_csv_safe(figure_summary, file.path(table_dir, "figure01_brazil_summary.csv"))
  notes <- c(
    "COFFE-STUDY: FIRST DESCRIPTIVE FIGURE, STEP 06",
    paste("Executed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Input:", input_file), paste("Input MD5:", input_md5),
    paste("Geometry:", geometry_file), paste("Font:", font_family),
    "No original data or geometry files are modified. No data are downloaded.",
    "Species labels and original unit assumptions are inherited from the audited input, not externally revalidated here.",
    "Panels a and b map municipal reporting units, not coffee-field extent or farm location.",
    "The two map panels use identical geographic extents and identical production-class breaks.",
    "A municipal mean is calculated only when production is observed in all four recent years.",
    "Municipalities with incomplete/absent data have a light fill and an x. Missing is never zero.",
    "Seven-digit codes are the only geographic join key. Operational lake areas are excluded from municipal maps.",
    "Geometries are simplified only for display, with a 250 m tolerance in a metric projection.",
    "Independent simplification may slightly alter shared boundary edges. No area or coordinate measurement uses it.",
    "Panels c and d show sums of available production and area records, separately for each category.",
    "Panel e is 1000 * sum(production_t) / sum(area_harvested_ha), from the same paired non-missing rows.",
    "Annual available-record composition can vary. A complete-code-panel sensitivity series is exported separately.",
    "A fixed code panel does NOT establish fixed historical boundaries.",
    "Panel f uses the same complete municipal-code sample in both windows and all three categories.",
    "Four-year windows include two odd and two even calendar years. Biennial bearing is not otherwise modeled.",
    "Period productivity is the ratio of period production and area sums, not the arithmetic mean of municipal yields.",
    "P=A*Y/1000. Area contribution=(A1-A0)*(Y1+Y0)/2000. Productivity contribution=(Y1-Y0)*(A1+A0)/2000.",
    "The two contributions sum to the observed production difference; their interaction is split symmetrically.",
    "Contributions are expressed as percentage points relative to baseline production.",
    "Accounting contributions are not causal. Aggregate productivity changes can reflect geographic, genotype and management composition.",
    "Official national totals and original metadata still need verification before final interpretation.",
    "Historical boundary harmonization, coffee-field elevation and climate attribution are outside this step.",
    paste("PNG export:", png_status), paste("PDF export:", pdf_status)
  )
  writeLines(notes, file.path(table_dir, "figure01_brazil_method_notes.txt"), useBytes = TRUE)
  caption <- paste0(
    "Figure 1. Municipal distribution and production structure of coffee in Brazil. ",
    "(a, b) Mean annual municipal production of Arabica and Canephora during 2021 to 2024, ",
    "displayed on the 2024 IBGE municipal boundaries. Means require four observed years; ",
    "municipalities with incomplete or absent data are marked with an x. The same map scale is used for both species. ",
    "(c, d) Production and harvested area summed across available records for total coffee, Arabica and Canephora, ",
    "2012 to 2024. (e) Aggregate productivity, calculated from matched production and harvested-area records. ",
    "(f) Exact accounting contributions of harvested-area and aggregate-productivity changes to the change in ",
    "mean production between 2012 to 2015 and 2021 to 2024, using a common complete sample of ",
    length(decomposition_codes), " municipal codes across both periods and all categories. ",
    "Contributions are percentages of baseline production and sum to the observed change. ",
    "These are descriptive estimates, not climate-attributable effects. Municipal polygons do not delineate coffee fields. ",
    "Original metadata, official totals and historical boundary comparability remain subject to verification."
  )
  writeLines(caption, file.path(table_dir, "figure01_brazil_caption_draft.txt"), useBytes = TRUE)
  capture.output(utils::sessionInfo(), file = file.path(table_dir, "sessionInfo_step06.txt"))

  if (unname(tools::md5sum(input_file)) != input_md5) fail("Input changed during execution. Check other running processes.")
  message("\nSTEP 06 completed: descriptive tables saved.")
  message("PNG: ", png_status, "\n", png_file)
  message("PDF: ", pdf_status, "\n", pdf_file)
  message("Tables and notes: ", table_dir)
  message("Return the PNG and figure01_brazil_production_decomposition.csv.")
  if (!identical(png_status, "Saved")) fail("Figure export failed. Send figure01_brazil_summary.csv; numerical tables were retained.")
  if (!identical(pdf_status, "Saved")) warning(pdf_status, call. = FALSE)
  invisible(figure_summary)
}

main()
