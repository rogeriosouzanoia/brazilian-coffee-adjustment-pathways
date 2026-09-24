# ============================================================
# COFFE-STUDY | STEP 32
# Exact decomposition of production adjustment pathways
# Total harvested area | geographic redistribution | within-municipality productivity | entries/exits
# ============================================================

rm(list = ls())

packages <- c("dplyr", "tidyr", "readr", "purrr", "ggplot2", "tibble", "ragg", "systemfonts")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

base_dir <- "."
out_dir <- file.path(base_dir, "04_outputs", "adjustment_pathway_decomposition")
fig_dir <- file.path(base_dir, "05_figures", "Adjustment_pathway_decomposition")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

find_project_file <- function(name) {
  x <- list.files(base_dir, pattern = paste0("^", name, "$"), recursive = TRUE, full.names = TRUE)
  if (!length(x)) stop("File not found in project: ", name)
  x[1]
}

f <- find_project_file("coffee_brazil_municipality_audited_1974_2024.csv")
dat <- read_csv(f, show_col_types = FALSE) |>
  filter(species %in% c("Arabica", "Canephora"), year %in% c(2012:2015, 2021:2024)) |>
  mutate(period = if_else(year <= 2015, "2012_2015", "2021_2024"))

period <- dat |>
  group_by(species, municipality_code, period) |>
  summarise(
    area = mean(area_harvested_ha, na.rm = TRUE),
    production = mean(production_t, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    area = if_else(is.nan(area), NA_real_, area),
    production = if_else(is.nan(production), NA_real_, production),
    productivity = if_else(area > 0 & is.finite(production), 1000 * production / area, NA_real_)
  ) |>
  pivot_wider(
    names_from = period,
    values_from = c(area, production, productivity),
    names_sep = "__"
  )

perms <- list(
  c("A", "S", "Y"), c("A", "Y", "S"),
  c("S", "A", "Y"), c("S", "Y", "A"),
  c("Y", "A", "S"), c("Y", "S", "A")
)

one_species <- function(d, sp) {
  d <- d |> filter(species == sp)

  cont <- d |>
    filter(
      area__2012_2015 > 0, area__2021_2024 > 0,
      is.finite(production__2012_2015), is.finite(production__2021_2024),
      is.finite(productivity__2012_2015), is.finite(productivity__2021_2024)
    )

  entrant <- d |>
    filter((is.na(area__2012_2015) | area__2012_2015 <= 0), area__2021_2024 > 0, is.finite(production__2021_2024))

  exit <- d |>
    filter(area__2012_2015 > 0, is.finite(production__2012_2015), (is.na(area__2021_2024) | area__2021_2024 <= 0))

  A0 <- sum(cont$area__2012_2015)
  A1 <- sum(cont$area__2021_2024)
  S0 <- cont$area__2012_2015 / A0
  S1 <- cont$area__2021_2024 / A1
  Y0 <- cont$productivity__2012_2015
  Y1 <- cont$productivity__2021_2024

  fprod <- function(A, S, Y) A * sum(S * Y) / 1000

  vals <- list(A = list(A0, A1), S = list(S0, S1), Y = list(Y0, Y1))
  contributions <- setNames(vector("list", 3), c("A", "S", "Y"))

  for (perm in perms) {
    state <- c(A = 1L, S = 1L, Y = 1L)
    old <- fprod(vals$A[[1]], vals$S[[1]], vals$Y[[1]])
    for (factor in perm) {
      state[[factor]] <- 2L
      new <- fprod(vals$A[[state[["A"]]]], vals$S[[state[["S"]]]], vals$Y[[state[["Y"]]]])
      contributions[[factor]] <- c(contributions[[factor]], new - old)
      old <- new
    }
  }

  shap <- sapply(contributions, mean)
  entry_contribution <- sum(entrant$production__2021_2024, na.rm = TRUE)
  exit_contribution <- -sum(exit$production__2012_2015, na.rm = TRUE)

  P0 <- sum(d$production__2012_2015, na.rm = TRUE)
  P1 <- sum(d$production__2021_2024, na.rm = TRUE)
  observed_change <- P1 - P0

  tibble(
    species = sp,
    baseline_production_t = P0,
    recent_production_t = P1,
    observed_change_t = observed_change,
    harvested_area_level_t = shap[["A"]],
    geographic_redistribution_t = shap[["S"]],
    within_municipality_productivity_t = shap[["Y"]],
    entry_t = entry_contribution,
    exit_t = exit_contribution,
    exact_component_sum_t = sum(shap) + entry_contribution + exit_contribution,
    continuing_municipalities = nrow(cont),
    entering_municipalities = nrow(entrant),
    exiting_municipalities = nrow(exit)
  )
}

summary <- bind_rows(
  one_species(period, "Arabica"),
  one_species(period, "Canephora")
) |>
  mutate(
    decomposition_residual_t = exact_component_sum_t - observed_change_t,
    observed_change_pct_baseline = 100 * observed_change_t / baseline_production_t
  )

components <- summary |>
  select(species, baseline_production_t,
         harvested_area_level_t, geographic_redistribution_t,
         within_municipality_productivity_t, entry_t, exit_t) |>
  pivot_longer(
    cols = c(
      harvested_area_level_t,
      geographic_redistribution_t,
      within_municipality_productivity_t,
      entry_t,
      exit_t
    ),
    names_to = "component",
    values_to = "contribution_t"
  ) |>
  mutate(
    component = recode(component,
      harvested_area_level_t = "Total harvested area",
      geographic_redistribution_t = "Geographic redistribution",
      within_municipality_productivity_t = "Within-municipality productivity",
      entry_t = "Entry into production",
      exit_t = "Exit from production"
    ),
    contribution_pct_baseline = 100 * contribution_t / baseline_production_t
  )

# Internal checks
stopifnot(all(is.finite(summary$baseline_production_t)))
stopifnot(all(abs(summary$decomposition_residual_t) < 1e-6))
stopifnot(nrow(components) == 10)

write_csv(summary, file.path(out_dir, "coffee_adjustment_pathway_decomposition_summary.csv"))
write_csv(components, file.path(out_dir, "coffee_adjustment_pathway_decomposition_components.csv"))

# Robust font selection and graphics export
font_table <- tryCatch(systemfonts::system_fonts(), error = function(e) NULL)
font_family <- "serif"
if (!is.null(font_table) && any(tolower(font_table$family) == "times new roman")) {
  font_family <- "Times New Roman"
}
message("Figure font: ", font_family)

save_png_safe <- function(filename, plot, width, height, dpi = 400) {
  ggsave(
    filename = filename, plot = plot, width = width, height = height, dpi = dpi,
    device = ragg::agg_png, bg = "white"
  )
}

save_pdf_safe <- function(filename, plot, width, height) {
  ok <- tryCatch({
    ggsave(
      filename = filename, plot = plot, width = width, height = height,
      device = grDevices::cairo_pdf, bg = "white"
    )
    TRUE
  }, error = function(e) {
    warning("PDF export with Cairo failed: ", conditionMessage(e),
            ". The analysis and PNG output were preserved.")
    FALSE
  })
  ok
}

# Figure
cols <- c(
  "Total harvested area" = "#A6A6A6",
  "Geographic redistribution" = "#6695A8",
  "Within-municipality productivity" = "#315A70",
  "Entry into production" = "#A2AF85",
  "Exit from production" = "#C56F53"
)

p <- ggplot(components, aes(x = species, y = contribution_pct_baseline, fill = component)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_col(position = "stack", width = 0.68) +
  scale_fill_manual(values = cols) +
  labs(
    title = "Contrasting production adjustment pathways",
    subtitle = "Exact Shapley decomposition of production change between 2012 to 2015 and 2021 to 2024",
    x = NULL,
    y = "Contribution relative to baseline production (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14, base_family = font_family) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "top"
  )

png_file <- file.path(fig_dir, "Adjustment_pathway_decomposition.png")
pdf_file <- file.path(fig_dir, "Adjustment_pathway_decomposition.pdf")

save_png_safe(png_file, p, width = 10, height = 7, dpi = 400)
pdf_saved <- save_pdf_safe(pdf_file, p, width = 10, height = 7)

message("STEP 32 COMPLETED")
message("PNG saved: ", png_file)
if (pdf_saved) message("PDF saved: ", pdf_file)
print(summary)
