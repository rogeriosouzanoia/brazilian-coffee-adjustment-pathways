# ============================================================
# COFFE-STUDY | STEP 33
# Formal long-term trends in fixed-geography coffee climate exposure
# Theil-Sen slopes + Mann-Kendall tests, 1981-2024
# ============================================================

rm(list = ls())

packages <- c("dplyr", "tidyr", "readr", "ggplot2", "trend", "Kendall", "tibble")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

base_dir <- "."
out_dir <- file.path(base_dir, "04_outputs", "long_term_climate_trends")
fig_dir <- file.path(base_dir, "05_figures", "Long_term_climate_trends")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

find_project_file <- function(name) {
  x <- list.files(base_dir, pattern = paste0("^", name, "$"), recursive = TRUE, full.names = TRUE)
  if (!length(x)) stop("File not found in project: ", name)
  x[1]
}

f <- find_project_file("coffee_power_brazil_weighted_exposure_annual.csv")
dat <- read_csv(f, show_col_types = FALSE) |>
  filter(
    weighting == "Fixed_2012_2015_area",
    species %in% c("Arabica", "Canephora"),
    metric %in% c("tmean_c", "hot_days_ge32", "precipitation_total_mm", "longest_dry_spell_days"),
    year >= 1981, year <= 2024
  )

metric_labels <- c(
  tmean_c = "Mean temperature",
  hot_days_ge32 = "Days with Tmax ≥ 32 °C",
  precipitation_total_mm = "Annual precipitation",
  longest_dry_spell_days = "Longest dry sequence"
)

metric_units <- c(
  tmean_c = "°C decade⁻¹",
  hot_days_ge32 = "days decade⁻¹",
  precipitation_total_mm = "mm decade⁻¹",
  longest_dry_spell_days = "days decade⁻¹"
)

one_trend <- function(d) {
  d <- d |> filter(is.finite(estimate_strict)) |> arrange(year)
  ss <- trend::sens.slope(d$estimate_strict, conf.level = 0.95)
  mk <- Kendall::MannKendall(d$estimate_strict)

  tibble(
    n_years = nrow(d),
    start_year = min(d$year),
    end_year = max(d$year),
    slope_per_year = unname(ss$estimates),
    slope_per_decade = 10 * unname(ss$estimates),
    lower95_per_decade = 10 * unname(ss$conf.int[1]),
    upper95_per_decade = 10 * unname(ss$conf.int[2]),
    kendall_tau = unname(mk$tau),
    mann_kendall_p = unname(mk$sl)
  )
}

trends <- dat |>
  group_by(species, metric, unit) |>
  group_modify(~ one_trend(.x)) |>
  ungroup() |>
  mutate(
    metric_label = unname(metric_labels[metric]),
    trend_unit = unname(metric_units[metric]),
    significant_005 = mann_kendall_p < 0.05
  )

write_csv(trends, file.path(out_dir, "coffee_fixed_geography_climate_trends_1981_2024.csv"))

# Add Sen trend line values for figure
plot_dat <- dat |>
  left_join(trends |> select(species, metric, slope_per_year), by = c("species", "metric")) |>
  group_by(species, metric) |>
  mutate(
    first_year = min(year[is.finite(estimate_strict)]),
    intercept_sen = median(estimate_strict - slope_per_year * year, na.rm = TRUE),
    sen_fit = intercept_sen + slope_per_year * year,
    metric_label = unname(metric_labels[metric])
  ) |>
  ungroup()

p <- ggplot(plot_dat, aes(x = year, y = estimate_strict, colour = species)) +
  geom_line(linewidth = 0.7, alpha = 0.72) +
  geom_line(aes(y = sen_fit), linewidth = 1.25) +
  facet_wrap(~ metric_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c(Arabica = "#315A70", Canephora = "#69784A")) +
  labs(
    title = "Long-term climate exposure at a fixed coffee geography",
    subtitle = "Annual exposure weighted by mean 2012 to 2015 harvested area; thick lines are Theil-Sen trends",
    x = "Year", y = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    plot.title = element_text(face = "bold", size = 18),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "top"
  )

ggsave(file.path(fig_dir, "Long_term_fixed_geography_climate_trends.png"), p, width = 12, height = 9, dpi = 400)
ggsave(file.path(fig_dir, "Long_term_fixed_geography_climate_trends.pdf"), p, width = 12, height = 9)

message("STEP 33 COMPLETED")
print(trends)
