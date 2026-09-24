# ======================================================================
# COFFE-STUDY | STEP 10
# Integrate geographic redistribution, concentration and productivity.
# Run the ENTIRE file with Source in RStudio.
#
# Inputs: completed steps 08 and 09. No production or geographic downloads.
# Earlier data, figures and scripts are never overwritten.
# The new six-panel figure is a revised Figure 2, not a new main figure.
#
# Method sources: method notes accompanying steps 08 and 09.
# Plot documentation:
# https://ggplot2.tidyverse.org/reference/ggsave.html
# https://patchwork.data-imaginist.com/articles/guides/layout.html
# ======================================================================

main <- function() {
  base_dir <- Sys.getenv(
    "COFFEE_STUDY_DIR",
    unset = "."
  )
  dir08 <- file.path(base_dir, "04_outputs", "geographic_redistribution")
  dir09 <- file.path(base_dir, "04_outputs", "productivity_redistribution")
  out_dir <- file.path(base_dir, "04_outputs", "figure02_integrated")
  fig_dir <- file.path(base_dir, "05_figures", "Figure02_integrated")
  for (p in c(out_dir, fig_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create directory: ", p, call. = FALSE)
  }
  old_options <- options(scipen = 999)
  on.exit(options(old_options), add = TRUE)

  # Existing user installations are used. Only absent plot packages install.
  packages <- c("ggplot2", "patchwork", "systemfonts", "ragg")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Installing missing plotting packages: ", paste(missing, collapse = ", "))
    utils::install.packages(missing, repos = "https://cloud.r-project.org")
  }
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Packages unavailable: ", paste(missing, collapse = ", "), call. = FALSE)

  read_csv <- function(p) {
    if (!file.exists(p)) stop("Required input not found:\n", p, call. = FALSE)
    utils::read.csv(p, colClasses = "character", check.names = FALSE,
                   stringsAsFactors = FALSE, na.strings = c("NA", ""),
                   fileEncoding = "UTF-8")
  }
  write_csv <- function(d, name) {
    target <- file.path(out_dir, name)
    temp <- tempfile("coffee10_", tmpdir = out_dir, fileext = ".csv")
    on.exit(unlink(temp), add = TRUE)
    utils::write.csv(d, temp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    if (!file.copy(temp, target, overwrite = TRUE)) stop("Cannot write: ", target, call. = FALSE)
  }
  need <- function(d, cols, label) {
    absent <- setdiff(cols, names(d))
    if (length(absent)) stop(label, ": missing columns: ", paste(absent, collapse = ", "), call. = FALSE)
  }
  check_value <- function(d, name) {
    need(d, c("check", "result"), "Summary")
    z <- d$result[d$check == name]
    if (length(z) != 1L || is.na(z)) stop("Missing/ambiguous summary entry: ", name, call. = FALSE)
    z
  }
  numeric_cols <- function(d, cols, label) {
    need(d, cols, label)
    for (v in cols) {
      z <- suppressWarnings(as.numeric(d[[v]]))
      if (anyNA(z) || any(!is.finite(z))) stop(label, ": invalid numbers in ", v, call. = FALSE)
      d[[v]] <- z
    }
    d
  }
  close <- function(x, y, label, tol = 1e-7) {
    if (length(x) != length(y) || any(!is.finite(c(x, y))) ||
        max(abs(x - y)) > tol) stop("Consistency check failed: ", label, call. = FALSE)
  }

  input_files <- c(
    summary08 = file.path(dir08, "coffee_redistribution_summary.csv"),
    shifts = file.path(dir08, "coffee_geographic_shift_by_species.csv"),
    concentration = file.path(dir08, "coffee_concentration_series.csv"),
    summary09 = file.path(dir09, "coffee_step09_summary.csv"),
    decomposition = file.path(dir09, "coffee_productivity_decomposition_summary.csv"),
    municipal = file.path(dir09, "coffee_productivity_decomposition_municipal.csv"),
    shares = file.path(dir09, "coffee_spatial_share_redistribution.csv")
  )
  inputs <- lapply(input_files, read_csv)
  s08 <- inputs$summary08
  s09 <- inputs$summary09
  if (check_value(s09, "execution_status") != "COMPLETED" ||
      !check_value(s08, "execution_status") %in% c("COMPLETED", "TABLES_COMPLETE_FIGURE_ERROR")) {
    stop("Complete steps 08 and 09 first.", call. = FALSE)
  }
  if (check_value(s08, "official_validation_passed_for_input") != "TRUE") {
    stop("Official production/area validation has not passed.", call. = FALSE)
  }
  source_md5 <- check_value(s09, "source_input_md5")
  if (source_md5 != check_value(s08, "input_md5")) {
    stop("Steps 08 and 09 refer to different source inputs.", call. = FALSE)
  }
  expected_codes <- as.integer(check_value(s09, "fixed_panel_codes"))
  if (is.na(expected_codes) || expected_codes < 1L ||
      expected_codes != as.integer(check_value(s08, "fixed_panel_municipal_codes"))) {
    stop("The municipal panels differ.", call. = FALSE)
  }
  source_file <- file.path(base_dir, "02_data_processed", "coffee_brazil_municipality_audited_1974_2024.rds")
  if (!file.exists(source_file) || unname(tools::md5sum(source_file)) != source_md5) {
    stop("Audited source RDS is missing or changed. Check the project directory and provenance.", call. = FALSE)
  }
  baseline <- "2012 to 2015"
  recent <- "2021 to 2024"
  if (check_value(s09, "baseline_period") != baseline || check_value(s09, "recent_period") != recent) {
    stop("Unexpected period definitions.", call. = FALSE)
  }
  species_all <- c("Total", "Arabica", "Canephora")
  species_plot <- c("Arabica", "Canephora")
  components <- c("within_municipality_component_kg_ha", "area_weight_component_kg_ha",
                  "zero_to_positive_area_component_kg_ha", "positive_to_zero_area_component_kg_ha")

  dec <- numeric_cols(inputs$decomposition, c(
    "fixed_panel_codes", "baseline_production_t", "recent_production_t", "baseline_area_ha", "recent_area_ha",
    "baseline_aggregate_productivity_kg_ha", "recent_aggregate_productivity_kg_ha",
    "productivity_change_kg_ha", "productivity_change_pct", components
  ), "Productivity decomposition")
  need(dec, c("species", "baseline_period", "recent_period"), "Decomposition")
  if (nrow(dec) != 3L || !setequal(dec$species, species_all) ||
      any(dec$fixed_panel_codes != expected_codes) || any(dec$baseline_period != baseline) ||
      any(dec$recent_period != recent)) stop("Unexpected decomposition scope.", call. = FALSE)

  municipal <- numeric_cols(inputs$municipal,
    c("baseline_production_t", "recent_production_t", "baseline_area_ha", "recent_area_ha", components),
    "Municipal decomposition")
  need(municipal, c("municipality_code", "species"), "Municipal decomposition")
  if (anyNA(municipal$municipality_code) || anyNA(municipal$species) ||
      anyDuplicated(paste(municipal$municipality_code, municipal$species)) ||
      !setequal(unique(municipal$species), species_all)) stop("Invalid municipal keys.", call. = FALSE)
  for (sp in species_all) {
    d <- dec[dec$species == sp, , drop = FALSE]
    m <- municipal[municipal$species == sp, , drop = FALSE]
    if (nrow(m) != expected_codes) stop("Unexpected municipal count for ", sp, call. = FALSE)
    for (v in components) close(sum(m[[v]]), d[[v]], paste(sp, v))
    for (v in c("baseline_production_t", "recent_production_t", "baseline_area_ha", "recent_area_ha")) {
      close(sum(m[[v]]), d[[v]], paste(sp, v))
    }
    close(sum(as.numeric(unlist(d[1, components], use.names = FALSE))), d$productivity_change_kg_ha, paste(sp, "identity"))
    close(1000 * d$baseline_production_t / d$baseline_area_ha,
          d$baseline_aggregate_productivity_kg_ha, paste(sp, "baseline productivity"))
    close(1000 * d$recent_production_t / d$recent_area_ha,
          d$recent_aggregate_productivity_kg_ha, paste(sp, "recent productivity"))
  }

  shifts <- numeric_cols(inputs$shifts,
    c("grid_easting_change_km", "grid_northing_change_km", "projected_center_displacement_km"), "Period centers")
  need(shifts, c("species", "weighting", "coordinate_definition", "baseline_period", "recent_period"), "Period centers")
  shifts <- shifts[shifts$species %in% species_plot &
      shifts$coordinate_definition == "Municipal_polygon_centroid", , drop = FALSE]
  if (nrow(shifts) != 4L || anyDuplicated(paste(shifts$species, shifts$weighting)) ||
      !setequal(shifts$weighting, c("Production", "Harvested_area")) ||
      any(shifts$baseline_period != baseline) || any(shifts$recent_period != recent)) {
    stop("Unexpected period-center records.", call. = FALSE)
  }
  close(sqrt(shifts$grid_easting_change_km^2 + shifts$grid_northing_change_km^2),
        shifts$projected_center_displacement_km, "Projected distances")
  shifts$weighting <- factor(gsub("_", " ", shifts$weighting), levels = c("Production", "Harvested area"))

  conc <- numeric_cols(inputs$concentration, c("year", "top50_share_pct"), "Concentration")
  need(conc, c("species", "weighting"), "Concentration")
  conc <- conc[conc$species %in% species_plot, , drop = FALSE]
  if (nrow(conc) != 52L || anyDuplicated(paste(conc$species, conc$weighting, conc$year)) ||
      !setequal(conc$year, 2012:2024) || any(conc$top50_share_pct < 0 | conc$top50_share_pct > 100)) {
    stop("Unexpected concentration series.", call. = FALSE)
  }
  conc$weighting <- factor(gsub("_", " ", conc$weighting), levels = c("Production", "Harvested area"))
  if (anyNA(conc$weighting)) stop("Unknown concentration weighting.", call. = FALSE)

  shares <- numeric_cols(inputs$shares, "total_variation_share_pct", "Share redistribution")
  need(shares, c("species", "weighting", "baseline_period", "recent_period"), "Share redistribution")
  if (nrow(shares) != 6L || anyDuplicated(paste(shares$species, shares$weighting)) ||
      any(shares$baseline_period != baseline) || any(shares$recent_period != recent)) {
    stop("Unexpected share-redistribution scope.", call. = FALSE)
  }

  # Figure-ready exact contributions; the observed change is NOT a fifth component.
  labels <- c("Within municipality", "Municipal area weights", "Zero to positive area", "Positive to zero area")
  decomp_plot <- do.call(rbind, lapply(species_all, function(sp) {
    z <- dec[dec$species == sp, , drop = FALSE]
    values <- as.numeric(unlist(z[1, components], use.names = FALSE))
    delta <- z$productivity_change_kg_ha
    data.frame(
      species = sp,
      component = c(labels, "Observed change"),
      component_kg_ha = c(values, delta),
      percentage_of_baseline_productivity = 100 * c(values, delta) / z$baseline_aggregate_productivity_kg_ha,
      share_of_net_productivity_change_pct = if (abs(delta) > 1e-12) 100 * c(values, delta) / delta else NA_real_,
      is_observed_total = c(rep(FALSE, 4), TRUE),
      baseline_period = baseline, recent_period = recent,
      stringsAsFactors = FALSE
    )
  }))
  decomp_plot$position <- rep(5:1, length(species_all))
  decomp_plot$label <- sprintf("%+.2f", decomp_plot$percentage_of_baseline_productivity)
  decomp_plot$label[decomp_plot$is_observed_total] <- paste0(decomp_plot$label[decomp_plot$is_observed_total], "%")
  decomp_plot$label_x <- ifelse(decomp_plot$percentage_of_baseline_productivity >= 0,
    decomp_plot$percentage_of_baseline_productivity + 1.8,
    decomp_plot$percentage_of_baseline_productivity - 1.8)
  decomp_plot$label_hjust <- ifelse(decomp_plot$percentage_of_baseline_productivity >= 0, 0, 1)
  write_csv(decomp_plot, "figure02_productivity_components.csv")
  write_csv(shifts, "figure02_period_center_changes.csv")
  write_csv(conc, "figure02_concentration_series.csv")
  write_csv(shares, "figure02_share_redistribution.csv")

  # User theme. Independent panels ensure axes and ticks in EVERY panel.
  families <- unique(systemfonts::system_fonts()$family)
  requested_font <- "Times New Roman"
  family <- if (requested_font %in% families) requested_font else "serif"
  if (family != requested_font) warning("Times New Roman was not found. Using system serif for this run.", call. = FALSE)
  theme_rogerio <- function(base_size = 14) {
    ggplot2::theme_minimal(base_size = base_size, base_family = family) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black", linewidth = 0.45),
        axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.35),
        axis.ticks.length = grid::unit(0.13, "cm"),
        axis.text = ggplot2::element_text(colour = "black", size = 11),
        axis.title = ggplot2::element_text(colour = "black", size = 12),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 7)),
        legend.position = "top", legend.title = ggplot2::element_blank(),
        legend.key.width = grid::unit(1.05, "cm"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        strip.text = ggplot2::element_text(face = "bold"),
        panel.spacing = grid::unit(0.8, "lines"),
        plot.title = ggplot2::element_text(face = "bold", size = 17, margin = ggplot2::margin(b = 5)),
        plot.subtitle = ggplot2::element_text(size = 11, margin = ggplot2::margin(b = 10)),
        plot.title.position = "plot",
        plot.margin = ggplot2::margin(7, 12, 7, 7)
      )
  }
  line_scale <- function() ggplot2::scale_linetype_manual(
    values = c("Production" = "solid", "Harvested area" = "22"),
    breaks = c("Production", "Harvested area"), drop = FALSE
  )

  # Same metric limits for both species. Equal x/y unit lengths preserve geometry.
  xr <- range(c(0, shifts$grid_easting_change_km))
  yr <- range(c(0, shifts$grid_northing_change_km))
  xlimits <- c(floor((xr[1] - 15) / 50) * 50, ceiling((xr[2] + 15) / 50) * 50)
  ylimits <- c(floor((yr[1] - 15) / 50) * 50, ceiling((yr[2] + 35) / 50) * 50)
  xbreaks <- seq(xlimits[1], xlimits[2], by = 100)
  ybreaks <- seq(ylimits[1], ylimits[2], by = 50)

  center_plot <- function(sp, tag) {
    z <- shifts[shifts$species == sp, , drop = FALSE]
    z <- z[order(z$weighting), , drop = FALSE]
    text <- paste(sprintf("%s: %.1f km", as.character(z$weighting), z$projected_center_displacement_km), collapse = "\n")
    ggplot2::ggplot(z, ggplot2::aes(linetype = weighting)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.3, colour = "grey60") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.3, colour = "grey60") +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = grid_easting_change_km,
                                       yend = grid_northing_change_km), linewidth = 0.8, show.legend = FALSE) +
      ggplot2::geom_point(ggplot2::aes(x = grid_easting_change_km, y = grid_northing_change_km,
                                     shape = weighting), size = 2.7, show.legend = FALSE) +
      ggplot2::annotate("point", x = 0, y = 0, shape = 21, fill = "white", size = 2.7, stroke = 0.7) +
      ggplot2::annotate("text", x = xlimits[2] - 6, y = ylimits[2] - 7,
                        label = text, hjust = 1, vjust = 1, family = family, size = 3.7, lineheight = 1.1) +
      ggplot2::scale_x_continuous(breaks = xbreaks) +
      ggplot2::scale_y_continuous(breaks = ybreaks) +
      ggplot2::scale_shape_manual(values = c("Production" = 16, "Harvested area" = 17), guide = "none") +
      line_scale() +
      ggplot2::coord_fixed(ratio = 1, xlim = xlimits, ylim = ylimits, expand = FALSE) +
      ggplot2::labs(title = paste(tag, sp), subtitle = "Change between period centers",
                    x = "Change in projected easting (km)", y = "Change in projected northing (km)") +
      theme_rogerio()
  }
  concentration_plot <- function(sp, tag, show_legend = FALSE) {
    z <- conc[conc$species == sp, , drop = FALSE]
    ggplot2::ggplot(z, ggplot2::aes(year, top50_share_pct, linetype = weighting)) +
      ggplot2::geom_line(linewidth = 0.8, show.legend = show_legend) +
      line_scale() +
      ggplot2::scale_x_continuous(limits = c(2012, 2024), breaks = c(2012, 2015, 2018, 2021, 2024),
                                 expand = ggplot2::expansion(mult = c(0.02, 0.03))) +
      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                                 expand = ggplot2::expansion(mult = c(0, 0.03))) +
      ggplot2::labs(title = paste(tag, sp), subtitle = "Municipal concentration",
                    x = "Year", y = "Share in the 50 largest municipalities (%)") + theme_rogerio()
  }
  ppvalues <- decomp_plot$percentage_of_baseline_productivity[decomp_plot$species %in% species_plot]
  # Keep the same horizontal scale for both species; print every small component.
  ppmin <- min(-8, floor(min(ppvalues) - 6))
  ppmax <- max(20, ceiling((max(ppvalues) + 10) / 10) * 10)
  component_plot <- function(sp, tag) {
    z <- decomp_plot[decomp_plot$species == sp, , drop = FALSE]
    ggplot2::ggplot(z, ggplot2::aes(x = percentage_of_baseline_productivity, y = position)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.3, colour = "grey60") +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = percentage_of_baseline_productivity,
                                       yend = position), linewidth = 0.65) +
      ggplot2::geom_point(ggplot2::aes(shape = is_observed_total), size = 2.7, show.legend = FALSE) +
      ggplot2::scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 18), guide = "none") +
      ggplot2::geom_text(ggplot2::aes(x = label_x, label = label, hjust = label_hjust),
                         family = family, size = 3.5) +
      ggplot2::scale_y_continuous(breaks = 1:5,
        labels = c("Observed\nchange", "Positive to\nzero area", "Zero to\npositive area",
                   "Municipal\narea weights", "Within\nmunicipality"),
        limits = c(0.5, 5.55), expand = c(0, 0)) +
      ggplot2::scale_x_continuous(limits = c(ppmin, ppmax), breaks = seq(0, ppmax, 20), expand = c(0, 0)) +
      ggplot2::labs(title = paste(tag, sp), subtitle = "Components of aggregate productivity change",
                    x = "Percentage of baseline productivity", y = NULL) + theme_rogerio()
  }

  # Assemble rows separately: long labels in the last row do not shrink the
  # equal-aspect center diagrams or create large blank areas in the first row.
  row1 <- patchwork::wrap_plots(center_plot("Arabica", "a"), center_plot("Canephora", "b"), nrow = 1)
  row2 <- patchwork::wrap_plots(concentration_plot("Arabica", "c", TRUE),
                                concentration_plot("Canephora", "d", FALSE), nrow = 1)
  row3 <- patchwork::wrap_plots(component_plot("Arabica", "e"), component_plot("Canephora", "f"), nrow = 1)
  figure <- patchwork::wrap_plots(row1, row2, row3, ncol = 1,
                                  heights = c(1.05, 0.87, 1.12), guides = "collect") +
    patchwork::plot_annotation(
      title = "Redistribution and productivity of Brazilian coffee",
      subtitle = paste0("Fixed panel of ", format(expected_codes, big.mark = ",", trim = TRUE),
        " municipal codes | ", baseline, " versus ", recent),
      theme = ggplot2::theme(
        text = ggplot2::element_text(family = family),
        plot.title = ggplot2::element_text(face = "bold", size = 21, margin = ggplot2::margin(b = 5)),
        plot.subtitle = ggplot2::element_text(size = 12, margin = ggplot2::margin(b = 6)),
        plot.margin = ggplot2::margin(12, 14, 8, 14)
      )
    )
  figure <- figure & ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank())

  png_file <- file.path(fig_dir, "Figure02_coffee_redistribution_integrated.png")
  pdf_file <- file.path(fig_dir, "Figure02_coffee_redistribution_integrated.pdf")
  message("Saving Figure 2...")
  ggplot2::ggsave(png_file, figure, width = 14, height = 12, units = "in",
                  dpi = 300, device = ragg::agg_png, bg = "white", limitsize = TRUE)
  pdf_status <- "Not saved: Cairo unavailable"
  if (capabilities("cairo")) {
    pdf_status <- tryCatch({
      ggplot2::ggsave(pdf_file, figure, width = 14, height = 12, units = "in",
                      device = grDevices::cairo_pdf, family = family, bg = "white")
      "Saved"
    }, error = function(e) paste("Not saved:", conditionMessage(e)))
  }

  caption <- paste0(
    "Figure 2. Geographic redistribution, municipal concentration and aggregate productivity of Brazilian coffee. ",
    "All panels use the same fixed panel of ", expected_codes, " municipal codes with observed production and harvested area ",
    "in every year from 2012 to 2024 for total coffee and both species. ",
    "(a, b) Changes between centers weighted by municipal mean production or harvested area in 2012 to 2015 and 2021 to 2024. ",
    "Each weighting has its own baseline origin (open point), so the panels compare shifts rather than absolute locations. ",
    "Filled circles denote recent production centers and triangles denote recent harvested area centers. ",
    "Coordinates are centroid proxies from the 2024 municipal polygons projected to the LAEA grid centered at 15 degrees S and 54 degrees W. ",
    "The identical axis limits and equal horizontal/vertical unit lengths permit comparison of projected shifts between species. ",
    "Distances are projected center displacements, not farm relocation distances or exact geodesic distances. ",
    "(c, d) Annual share held by the 50 largest municipalities, ranked separately each year for production and harvested area. ",
    "(e, f) Exact accounting decomposition of the change in aggregate productivity between the two four-year windows. ",
    "Aggregate productivity is the ratio of summed production to summed harvested area. The first two components ",
    "split changes in municipal productivity and municipal area weights symmetrically among units with positive area in both periods. ",
    "Transitions from zero to positive area and positive to zero area are separate. The four components sum to the observed change ",
    "(diamond), which is a reference total and must not be added again. Component values are percentages of baseline aggregate productivity, ",
    "not percentages of the net increase. Concentration is municipality-based rather than farm-based. Official national and state ",
    "production/area totals were reproduced for the underlying 2012 to 2024 source data, but fixed municipal codes do not guarantee ",
    "fixed historical boundaries. These accounting results do not identify climate effects, technological progress, or uphill migration."
  )
  writeLines(caption, file.path(out_dir, "figure02_caption_draft.txt"), useBytes = TRUE)
  notes <- c(
    "COFFE-STUDY | STEP 10 | INTEGRATED FIGURE 2",
    paste("Executed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Audited input MD5:", source_md5),
    "This is an integration of completed steps 08 and 09, not another data download.",
    "The earlier Figure 2 draft and all original data remain unchanged.",
    "TOP ROW: period-center changes, NOT the former 2012-to-2024 annual trajectories.",
    "Both species and both weightings use the same fixed municipal-code panel.",
    "Each weighting has its own baseline origin. Absolute centers cannot be compared in this diagram.",
    "Coordinates are municipal reference proxies, not coffee-field locations.",
    "Same projected x/y limits and equal unit lengths in both center panels.",
    "MIDDLE ROW: top-50 concentration, with a separate ranking by year and weighting.",
    "BOTTOM ROW: symmetric accounting components inherited unchanged from step 09.",
    "Percentages of baseline productivity sum to the observed relative productivity change.",
    "Shares of net change in the exported table have a DIFFERENT denominator.",
    "A negative component means positive shares can sum to more than 100% of the net change.",
    "Zero-to-positive and positive-to-zero area are reported-period patterns, not verified farm entry/exit.",
    "Within-municipality change can include climate, management, genetics and changes of farms inside a municipality.",
    "No causal mechanism is assigned to any component.",
    "The Total category is exported in the component table but never added to the two species.",
    "Share total variation = 50 * sum(abs(recent_share - baseline_share)), shares on a 0-to-1 scale.",
    "Share total variation is not a percentage of land physically moved and uses no geographic distances.",
    "Missing data are not converted to zero. No undefined productivity is imputed.",
    "Historical boundaries are not harmonized. No altitude or climatic exposure is estimated.",
    paste("Requested font:", requested_font), paste("Font used:", family), paste("PDF:", pdf_status)
  )
  writeLines(notes, file.path(out_dir, "figure02_integrated_method_notes.txt"), useBytes = TRUE)
  write_csv(data.frame(role = names(input_files), file = unname(input_files),
                       md5 = unname(tools::md5sum(input_files)), stringsAsFactors = FALSE),
            "figure02_integrated_provenance.csv")
  summary <- data.frame(
    check = c("execution_status", "fixed_panel_codes", "baseline_period", "recent_period",
              "source_input_md5", "component_identity_verified", "source_national_state_totals_verified",
              "historical_boundaries_harmonized", "climate_attribution_performed", "original_inputs_modified",
              "font_requested", "font_used", "png_status", "pdf_status"),
    result = c("COMPLETED", expected_codes, baseline, recent, source_md5, "TRUE", "TRUE", "FALSE", "FALSE", "FALSE",
               requested_font, family, "Saved", pdf_status), stringsAsFactors = FALSE
  )
  write_csv(summary, "figure02_integrated_summary.csv")
  capture.output(utils::sessionInfo(), file = file.path(out_dir, "sessionInfo_step10.txt"))
  cat("\nFigure 2 integration completed.\n\n", png_file, "\n", pdf_file,
      "\n\nTables and caption:\n", out_dir, "\n", sep = "")
  print(dec[, c("species", "baseline_aggregate_productivity_kg_ha", "recent_aggregate_productivity_kg_ha",
                 "productivity_change_pct")], row.names = FALSE)
  invisible(list(figure = figure, decomposition = decomp_plot, summary = summary))
}

result_step10 <- main()
