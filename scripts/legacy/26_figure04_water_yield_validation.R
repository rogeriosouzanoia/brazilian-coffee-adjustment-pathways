# ============================================================
# COFFE STUDY | STEP 26 | FIGURE 4 SYNTHESIS
# ============================================================

rm(list = ls())

packages <- c("dplyr", "tidyr", "readr", "ggplot2", "patchwork", "scales")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

base_dir <- "."
water_dir <- file.path(base_dir, "04_outputs", "water_stress_synthesis")
assoc_dir <- file.path(base_dir, "04_outputs", "seasonal_water_associations")
validation_dir <- file.path(base_dir, "04_outputs", "seasonal_water_temporal_validation")
out_dir <- file.path(base_dir, "04_outputs", "figure04_water_yield")
fig_dir <- file.path(base_dir, "05_figures", "Figure04_water_yield")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

f_water <- file.path(water_dir, "coffee_water_stress_scenario_sensitivity.csv")
f_assoc <- file.path(assoc_dir, "coffee_seasonal_water_plot_data.csv")
f_validation <- file.path(validation_dir, "coffee_water_temporal_model_comparison.csv")
required_files <- c(f_water, f_assoc, f_validation)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) stop(paste("Required file(s) not found:", paste(missing_files, collapse = "\n")))

water <- readr::read_csv(f_water, show_col_types = FALSE)
assoc <- readr::read_csv(f_assoc, show_col_types = FALSE)
validation <- readr::read_csv(f_validation, show_col_types = FALSE)

theme_rogerio <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Times New Roman") +
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

windows <- c("W1", "W2", "W3", "W4")
window_labels <- c(
  W1 = "W1\nSep(t−2) to Mar(t−1)",
  W2 = "W2\nApr to Aug(t−1)",
  W3 = "W3\nSep to Dec(t−1)",
  W4 = "W4\nJan to Mar(t)"
)
species_order <- c("Arabica", "Canephora")

water_plot <- water |>
  filter(species %in% species_order) |>
  mutate(
    species = factor(species, levels = species_order),
    window_id = factor(window_id, levels = windows),
    window_label = factor(window_labels[as.character(window_id)], levels = window_labels[windows])
  )

make_water_panel <- function(sp, panel_label) {
  d <- water_plot |> filter(species == sp)
  ggplot(d, aes(x = window_label, y = central_change_pp)) +
    geom_hline(yintercept = 0, linewidth = 0.4, linetype = 2) +
    geom_linerange(aes(ymin = scenario_minimum_change_pp, ymax = scenario_maximum_change_pp), linewidth = 1.1) +
    geom_point(size = 3) +
    labs(
      title = paste0(panel_label, "  ", sp),
      subtitle = "Change in simulated seasonal water satisfaction",
      x = NULL,
      y = "Change in ETr/ETc\n(percentage points)"
    ) + theme_rogerio()
}

p_a <- make_water_panel("Arabica", "a")
p_b <- make_water_panel("Canephora", "b")

assoc_plot <- assoc |>
  filter(
    species %in% species_order,
    control == "Trend_parity_year",
    family %in% c("P", "W"),
    family == "P" | scenario_id == "CAD100_Kc095",
    predictor %in% c("rain", "water")
  ) |>
  mutate(
    species = factor(species, levels = species_order),
    window_id = factor(window_id, levels = windows),
    indicator = case_when(
      predictor == "rain" ~ "Precipitation: +100 mm",
      predictor == "water" ~ "Water limitation: −0.10 ETr/ETc",
      TRUE ~ predictor
    ),
    ci_available = is.finite(lower95_pct) & is.finite(upper95_pct)
  )

make_assoc_panel <- function(sp, panel_label) {
  d <- assoc_plot |> filter(species == sp)
  ggplot(d, aes(x = association_pct, y = window_id, shape = indicator)) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = 2) +
    geom_errorbar(
      data = d |> filter(ci_available),
      aes(xmin = lower95_pct, xmax = upper95_pct),
      width = 0.16,
      linewidth = 0.55,
      orientation = "y",
      position = position_dodge(width = 0.45)
    ) +
    geom_point(size = 2.8, position = position_dodge(width = 0.45)) +
    labs(
      title = paste0(panel_label, "  ", sp),
      subtitle = "Conditional association with municipal productivity",
      x = "Association with productivity (%)",
      y = NULL,
      shape = NULL
    ) + theme_rogerio()
}

p_c <- make_assoc_panel("Arabica", "c")
p_d <- make_assoc_panel("Canephora", "d")

validation_plot <- validation |>
  filter(
    species %in% species_order,
    test_period == "POOLED_2020_2024",
    family %in% c("N", "H", "P", "W")
  ) |>
  mutate(
    species = factor(species, levels = species_order),
    family = factor(
      family,
      levels = c("N", "H", "P", "W"),
      labels = c("No climate", "Heat", "Heat + precipitation", "Heat + water balance")
    ),
    control_label = case_when(
      control == "Local_trend" ~ "Local trend",
      control == "Local_trend_parity" ~ "Local trend + parity",
      TRUE ~ control
    ),
    mse_skill_pct = 100 * mse_skill_vs_N
  )

make_validation_panel <- function(sp, panel_label) {
  d <- validation_plot |> filter(species == sp)
  ggplot(d, aes(x = mse_skill_pct, y = family, shape = control_label)) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = 2) +
    geom_point(size = 3, position = position_dodge(width = 0.45)) +
    labs(
      title = paste0(panel_label, "  ", sp),
      subtitle = "Chronological holdout, 2020 to 2024",
      x = "MSE skill relative to no climate (%)",
      y = NULL,
      shape = NULL
    ) + theme_rogerio()
}

p_e <- make_validation_panel("Arabica", "e")
p_f <- make_validation_panel("Canephora", "f")

fig <- (p_a + p_b) / (p_c + p_d) / (p_e + p_f) +
  plot_annotation(
    title = "Seasonal water limitation, productivity associations and temporal validation",
    subtitle = "Brazilian coffee, 2012 to 2024. Scenario ranges describe CAD and Kc sensitivity, not confidence intervals.",
    caption = paste(
      "W1 to W4 are calendar windows, not locally observed phenology.",
      "ETr/ETc is simulated under standardized rainfed scenarios.",
      "Association panels use the central CAD 100 mm, Kc 0.95 water scenario and the parity sensitivity specification.",
      "Temporal validation fits each held out year using only earlier reporting years.",
      "Positive MSE skill indicates smaller squared prediction error than the corresponding no climate model.",
      "No panel estimates climate attributable yield loss or adaptation effectiveness."
    ),
    theme = theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 9, hjust = 0)
    )
  )

png_file <- file.path(fig_dir, "Figure04_water_yield_validation.png")
pdf_file <- file.path(fig_dir, "Figure04_water_yield_validation.pdf")

ggsave(png_file, fig, width = 12.5, height = 13.5, dpi = 400)

pdf_status <- "NOT_SAVED"
try({
  grDevices::cairo_pdf(pdf_file, width = 12.5, height = 13.5, family = "Times New Roman")
  print(fig)
  grDevices::dev.off()
  pdf_status <- "SAVED"
}, silent = TRUE)

summary_water <- water_plot |>
  transmute(
    result_type = "water_change",
    species = as.character(species),
    item = as.character(window_id),
    value = central_change_pp,
    lower = scenario_minimum_change_pp,
    upper = scenario_maximum_change_pp,
    unit = "percentage points"
  )

summary_assoc <- assoc_plot |>
  transmute(
    result_type = "association",
    species = as.character(species),
    item = paste(window_id, indicator, sep = " | "),
    value = association_pct,
    lower = lower95_pct,
    upper = upper95_pct,
    unit = "%"
  )

summary_validation <- validation_plot |>
  transmute(
    result_type = "temporal_validation",
    species = as.character(species),
    item = paste(as.character(family), control_label, sep = " | "),
    value = mse_skill_pct,
    lower = NA_real_,
    upper = NA_real_,
    unit = "% MSE skill vs no climate"
  )

readr::write_csv(
  bind_rows(summary_water, summary_assoc, summary_validation),
  file.path(out_dir, "figure04_summary.csv")
)

notes <- c(
  "FIGURE 4 WATER AND PRODUCTIVITY SYNTHESIS",
  "",
  "Panels a and b compare 2021 to 2024 against 2012 to 2015.",
  "Points are the central CAD100 Kc0.95 scenario.",
  "Segments span all 12 CAD and Kc scenarios and are not confidence intervals.",
  "",
  "Panels c and d use the Trend_parity_year specification.",
  "Precipitation increment is +100 mm within the window.",
  "Water increment is 0.10 lower seasonal ETr/ETc.",
  "Intervals are exploratory and are omitted when the primary covariance was invalid.",
  "",
  "Panels e and f are chronological outcome holdouts for 2020 to 2024.",
  "Each held out year is predicted from models trained only on earlier reporting years.",
  "Positive MSE skill means smaller squared error than the matching no climate model.",
  "",
  "The validation pool was selected retrospectively using the full 2012 to 2024 source panel.",
  "W4 includes January to March of the PAM reporting year and may overlap harvest.",
  "Results are not causal climate attribution and are not operational forecasts."
)
writeLines(notes, file.path(out_dir, "figure04_method_notes.txt"))

report <- tibble::tibble(
  check = c(
    "execution_status", "figure_version", "water_rows", "association_rows",
    "validation_rows", "png_status", "pdf_status", "new_models_fitted",
    "new_weather_downloaded", "causal_attribution"
  ),
  result = c(
    "COMPLETED_FIGURE04_SYNTHESIS", "26_water_yield_validation_v1",
    as.character(nrow(water_plot)), as.character(nrow(assoc_plot)),
    as.character(nrow(validation_plot)), "SAVED", pdf_status,
    "FALSE", "FALSE", "FALSE"
  )
)
readr::write_csv(report, file.path(out_dir, "figure04_report.csv"))

message("Figure 4 synthesis completed.")
message("PNG: ", png_file)
message("PDF: ", pdf_file)
