# ============================================================
# COFFE STUDY
# STEP 28
# FIGURE 5
# ALTITUDINAL AND CLIMATIC REDISTRIBUTION OF BRAZILIAN COFFEE
# ============================================================

rm(list = ls())

packages <- c(
  "dplyr", "tidyr", "readr", "ggplot2", "patchwork",
  "scales", "forcats", "stringr", "tibble"
)

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

# ------------------------------------------------------------
# Directories
# ------------------------------------------------------------

base_dir <- "."

in_dir <- file.path(base_dir, "04_outputs", "figure05_altitude_shift")
out_dir <- file.path(base_dir, "04_outputs", "figure05_altitude_climate")
fig_dir <- file.path(base_dir, "05_figures", "Figure05_altitude_climate")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Inputs
# ------------------------------------------------------------

f_shift <- file.path(in_dir, "coffee_elevation_shift_summary.csv")
f_bands <- file.path(in_dir, "coffee_elevation_band_distribution.csv")
f_up <- file.path(in_dir, "coffee_upslope_indicators.csv")
f_clim <- file.path(in_dir, "coffee_redistribution_climatology_summary.csv")
f_elev <- file.path(in_dir, "coffee_municipality_elevation_reference.csv")
f_summary <- file.path(in_dir, "coffee_figure05_preparation_summary.csv")

required <- c(f_shift, f_bands, f_up, f_clim, f_elev, f_summary)
missing <- required[!file.exists(required)]
if (length(missing) > 0) {
  stop(
    paste0(
      "Required Step 27 file(s) not found:\n",
      paste(missing, collapse = "\n")
    )
  )
}

shift <- readr::read_csv(f_shift, show_col_types = FALSE)
bands <- readr::read_csv(f_bands, show_col_types = FALSE)
upslope <- readr::read_csv(f_up, show_col_types = FALSE)
clim <- readr::read_csv(f_clim, show_col_types = FALSE)
elev <- readr::read_csv(f_elev, show_col_types = FALSE)
prep <- readr::read_csv(f_summary, show_col_types = FALSE)

# ------------------------------------------------------------
# Checks
# ------------------------------------------------------------

stopifnot(
  all(c(
    "species", "weighting",
    "weighted_mean_elevation_m__2012 to 2015",
    "weighted_mean_elevation_m__2021 to 2024",
    "elevation_change_m"
  ) %in% names(shift))
)

stopifnot(
  all(c(
    "species", "weighting", "period",
    "elevation_band", "share_of_weight"
  ) %in% names(bands))
)

stopifnot(
  all(c(
    "species", "weighting", "threshold_m",
    "share_above_threshold__2012 to 2015",
    "share_above_threshold__2021 to 2024",
    "change_share_above_threshold"
  ) %in% names(upslope))
)

stopifnot(
  all(c(
    "species", "weighting", "metric",
    "weighted_mean_climatology__2012 to 2015",
    "weighted_mean_climatology__2021 to 2024",
    "redistribution_only_change"
  ) %in% names(clim))
)

species_order <- c("Arabica", "Canephora")
period_order <- c("2012 to 2015", "2021 to 2024")

# ------------------------------------------------------------
# Style
# ------------------------------------------------------------

theme_rogerio <- function(base_size = 12) {
  theme_minimal(
    base_size = base_size,
    base_family = "Times New Roman"
  ) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(linewidth = 0.35),
      axis.ticks = element_line(linewidth = 0.35),
      axis.ticks.length = unit(0.15, "cm"),
      legend.position = "top",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(size = base_size - 1),
      plot.caption = element_text(size = base_size - 2, hjust = 0),
      panel.spacing = unit(0.8, "lines"),
      plot.margin = margin(7, 8, 7, 8)
    )
}

# ------------------------------------------------------------
# Panel preparation
# ------------------------------------------------------------

# Primary altitude distribution uses harvested area because the question is
# spatial redistribution of cultivated area, not only redistribution of output.

bands_plot <- bands |>
  filter(
    species %in% species_order,
    weighting == "Harvested_area",
    !is.na(elevation_band)
  ) |>
  mutate(
    species = factor(species, levels = species_order),
    period = factor(period, levels = period_order),
    share_pct = 100 * share_of_weight,
    elevation_band = factor(
      elevation_band,
      levels = unique(elevation_band)
    )
  )

# Weighted mean elevation, both area and production weights
shift_plot <- shift |>
  filter(species %in% species_order) |>
  transmute(
    species = factor(species, levels = species_order),
    weighting = recode(
      weighting,
      Harvested_area = "Harvested area",
      Production = "Production"
    ),
    baseline = .data[["weighted_mean_elevation_m__2012 to 2015"]],
    recent = .data[["weighted_mean_elevation_m__2021 to 2024"]],
    change = elevation_change_m
  )

# Threshold summaries use harvested area as the primary land redistribution indicator
upslope_plot <- upslope |>
  filter(
    species %in% species_order,
    weighting == "Harvested_area"
  ) |>
  transmute(
    species = factor(species, levels = species_order),
    threshold_m,
    baseline_pct = 100 * .data[["share_above_threshold__2012 to 2015"]],
    recent_pct = 100 * .data[["share_above_threshold__2021 to 2024"]],
    change_pp = 100 * change_share_above_threshold
  )

# Redistribution only climate metrics
clim_plot <- clim |>
  filter(
    species %in% species_order,
    metric %in% c("tmean_c", "hot_days_ge32")
  ) |>
  mutate(
    species = factor(species, levels = species_order),
    weighting = recode(
      weighting,
      Harvested_area = "Harvested area",
      Production = "Production"
    )
  )

# ------------------------------------------------------------
# Panels a and b
# Harvested area distribution across elevation bands
# ------------------------------------------------------------

make_band_panel <- function(sp, panel_label) {
  d <- bands_plot |> filter(species == sp)

  ggplot(
    d,
    aes(
      x = elevation_band,
      y = share_pct,
      group = period,
      linetype = period,
      shape = period
    )
  ) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2.2) +
    scale_y_continuous(
      labels = label_number(suffix = "%", accuracy = 1),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      title = paste0(panel_label, "  ", sp),
      subtitle = "Harvested area across municipal reference elevations",
      x = "Elevation band (m)",
      y = "Share of harvested area"
    ) +
    theme_rogerio() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

p_a <- make_band_panel("Arabica", "a")
p_b <- make_band_panel("Canephora", "b")

# ------------------------------------------------------------
# Panel c
# Change in weighted mean elevation
# ------------------------------------------------------------

shift_long <- shift_plot |>
  pivot_longer(
    cols = c(baseline, recent),
    names_to = "period_key",
    values_to = "elevation_m"
  ) |>
  mutate(
    period = recode(
      period_key,
      baseline = "2012 to 2015",
      recent = "2021 to 2024"
    ),
    period = factor(period, levels = period_order)
  )

p_c <- ggplot(
  shift_long,
  aes(
    x = elevation_m,
    y = weighting,
    group = weighting
  )
) +
  geom_line(
    aes(group = weighting),
    linewidth = 0.6
  ) +
  geom_point(
    aes(shape = period),
    size = 2.8
  ) +
  facet_wrap(~species, nrow = 1, scales = "free_x") +
  labs(
    title = "c  Weighted mean reference elevation",
    subtitle = "Production and harvested area weights",
    x = "Reference elevation (m)",
    y = NULL,
    shape = NULL
  ) +
  theme_rogerio()

# ------------------------------------------------------------
# Panel d
# Share of harvested area above altitude thresholds
# ------------------------------------------------------------

upslope_long <- upslope_plot |>
  select(species, threshold_m, baseline_pct, recent_pct) |>
  pivot_longer(
    cols = c(baseline_pct, recent_pct),
    names_to = "period_key",
    values_to = "share_pct"
  ) |>
  mutate(
    period = recode(
      period_key,
      baseline_pct = "2012 to 2015",
      recent_pct = "2021 to 2024"
    ),
    period = factor(period, levels = period_order),
    threshold_label = factor(
      paste0("≥ ", threshold_m, " m"),
      levels = paste0("≥ ", sort(unique(threshold_m)), " m")
    )
  )

p_d <- ggplot(
  upslope_long,
  aes(
    x = share_pct,
    y = threshold_label,
    shape = period,
    group = threshold_label
  )
) +
  geom_line(
    aes(group = threshold_label),
    linewidth = 0.55
  ) +
  geom_point(size = 2.7) +
  facet_wrap(~species, nrow = 1, scales = "free_x") +
  labs(
    title = "d  Harvested area above elevation thresholds",
    subtitle = "Municipal reference elevation, not field altitude",
    x = "Share of harvested area (%)",
    y = NULL,
    shape = NULL
  ) +
  theme_rogerio()

# ------------------------------------------------------------
# Panels e and f
# Climate character of redistribution
# ------------------------------------------------------------

make_climate_panel <- function(metric_name, panel_label, subtitle, x_label) {
  d <- clim_plot |>
    filter(metric == metric_name) |>
    mutate(
      species_weight = paste(species, weighting, sep = " | ")
    )

  ggplot(
    d,
    aes(
      x = redistribution_only_change,
      y = weighting,
      shape = species
    )
  ) +
    geom_vline(
      xintercept = 0,
      linewidth = 0.4,
      linetype = 2
    ) +
    geom_point(size = 3) +
    labs(
      title = paste0(panel_label, "  ", subtitle),
      subtitle = "Change caused by redistribution of municipal weights only",
      x = x_label,
      y = NULL,
      shape = NULL
    ) +
    theme_rogerio()
}

p_e <- make_climate_panel(
  "tmean_c",
  "e",
  "Climatological mean temperature",
  "Change in weighted 1981 to 2010 climatology (°C)"
)

p_f <- make_climate_panel(
  "hot_days_ge32",
  "f",
  "Climatological hot day exposure",
  "Change in weighted 1981 to 2010 climatology (days year⁻¹)"
)

# ------------------------------------------------------------
# Integrated figure
# ------------------------------------------------------------

fig <- (
  p_a + p_b
) / (
  p_c + p_d
) / (
  p_e + p_f
) +
  plot_annotation(
    title = "Altitudinal and climatic redistribution of Brazilian coffee",
    subtitle = "Changes between 2012 to 2015 and 2021 to 2024",
    caption = paste(
      "Elevation is sampled at municipal reference points and does not represent observed coffee field altitude.",
      "Panels a and b show harvested area shares across elevation bands.",
      "Panels c and d compare period weighted elevation summaries.",
      "Panels e and f hold the 1981 to 2010 municipal climatology fixed and change only the municipal production or harvested area weights,",
      "so they describe the climatic character of geographic redistribution rather than realized warming between periods.",
      "These results do not demonstrate within municipality uphill migration or causal adaptation to climate change."
    ),
    theme = theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 9, hjust = 0)
    )
  )

# ------------------------------------------------------------
# Export
# ------------------------------------------------------------

png_file <- file.path(fig_dir, "Figure05_altitude_climate_shift.png")
pdf_file <- file.path(fig_dir, "Figure05_altitude_climate_shift.pdf")

ggsave(
  png_file,
  fig,
  width = 13,
  height = 13.5,
  dpi = 400
)

pdf_status <- "NOT_SAVED"

try({
  grDevices::cairo_pdf(
    pdf_file,
    width = 13,
    height = 13.5,
    family = "Times New Roman"
  )
  print(fig)
  grDevices::dev.off()
  pdf_status <- "SAVED"
}, silent = TRUE)

# ------------------------------------------------------------
# Compact result table for manuscript writing
# ------------------------------------------------------------

result_summary <- bind_rows(
  shift_plot |>
    transmute(
      result = "weighted_mean_elevation",
      species = as.character(species),
      weighting,
      indicator = "elevation_m",
      baseline,
      recent,
      change,
      unit = "m"
    ),
  upslope_plot |>
    transmute(
      result = "share_above_elevation_threshold",
      species = as.character(species),
      weighting = "Harvested area",
      indicator = paste0("above_", threshold_m, "m"),
      baseline = baseline_pct,
      recent = recent_pct,
      change = change_pp,
      unit = "percentage points"
    ),
  clim_plot |>
    transmute(
      result = "redistribution_climatology",
      species = as.character(species),
      weighting,
      indicator = metric,
      baseline = .data[["weighted_mean_climatology__2012 to 2015"]],
      recent = .data[["weighted_mean_climatology__2021 to 2024"]],
      change = redistribution_only_change,
      unit = case_when(
        metric == "tmean_c" ~ "degC",
        metric == "hot_days_ge32" ~ "days year-1",
        TRUE ~ NA_character_
      )
    )
)

readr::write_csv(
  result_summary,
  file.path(out_dir, "figure05_summary.csv")
)

# ------------------------------------------------------------
# Manuscript ready caption draft
# ------------------------------------------------------------

caption <- paste(
  "Figure 5. Altitudinal and climatic redistribution of Brazilian coffee between 2012 to 2015 and 2021 to 2024.",
  "Panels a and b show the distribution of harvested area across elevation bands for Arabica and Canephora.",
  "Panel c compares production and harvested area weighted mean municipal reference elevation between periods.",
  "Panel d shows the share of harvested area associated with municipal reference elevations above 600, 800 and 1000 m.",
  "Panels e and f quantify changes in the climatic character of the production geography by applying baseline and recent municipal weights to the same 1981 to 2010 municipal climatology.",
  "Elevation was sampled at municipal reference points and does not represent observed field altitude or within municipality movement of coffee plantations.",
  "Climatological redistribution therefore describes changes in the geographic composition of reporting municipalities and is not an estimate of realized climate change, farm relocation or causal adaptation."
)

writeLines(
  caption,
  file.path(out_dir, "figure05_caption_draft.txt")
)

report <- tibble::tibble(
  check = c(
    "execution_status",
    "figure_version",
    "species",
    "primary_land_weight",
    "baseline_period",
    "recent_period",
    "climatology_period",
    "actual_field_altitude_measured",
    "within_municipality_uphill_movement_measured",
    "causal_adaptation_estimated",
    "png_status",
    "pdf_status"
  ),
  result = c(
    "COMPLETED_FIGURE05",
    "28_altitude_climate_shift_v1",
    "Arabica | Canephora",
    "Harvested area",
    "2012 to 2015",
    "2021 to 2024",
    "1981 to 2010",
    "FALSE",
    "FALSE",
    "FALSE",
    "SAVED",
    pdf_status
  )
)

readr::write_csv(
  report,
  file.path(out_dir, "figure05_report.csv")
)

message("Figure 5 completed.")
message("PNG: ", png_file)
message("PDF: ", pdf_file)
