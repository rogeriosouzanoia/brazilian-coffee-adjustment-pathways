
# ============================================================
# COFFEE STUDY
# STEP 37
# PUBLISHED FUTURE CLIMATE-RISK EVIDENCE FOR BRAZILIAN COFFEE
# ============================================================

rm(list = ls())

packages <- c("dplyr","readr","tibble","ggplot2","stringr","ragg")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

base_dir <- "."
out_dir <- file.path(base_dir, "04_outputs", "future_risk_literature_synthesis")
fig_dir <- file.path(base_dir, "05_figures", "Supplementary_future_risk")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

evidence <- tribble(
  ~study, ~scope, ~framework, ~horizon, ~headline, ~adaptation, ~doi_or_url,
  "Assad et al. 2004",
  "Brazil, Arabica",
  "Agroclimatic zoning",
  "warming scenarios",
  ">95% suitable-area loss in GO, MG and SP under +5.8 C; about 75% in PR",
  "Not represented explicitly",
  "https://doi.org/10.1590/S1678-3921.pab2004.v39.6876",

  "Bunn et al. 2015",
  "Global, Arabica + Canephora",
  "Multi-model climatic suitability",
  "2050",
  "Brazil among regions with largest suitability losses; magnitude strongly scenario dependent",
  "Potential relocation inferred, management not modeled",
  "https://doi.org/10.1007/s10584-014-1306-x",

  "Gomes et al. 2020",
  "Southeast Brazil, Arabica",
  "Spatial suitability plus agroforestry microclimate",
  "2050",
  "About 60% suitable-area reduction unshaded; 50% shade retains about 75% of suitable area",
  "Shade and agroforestry represented explicitly",
  "https://www.sciencedirect.com/science/article/pii/S0167880920300438",

  "CMIP6 multi-risk study 2024",
  "Brazil, Arabica",
  "CMIP6 risk indicators",
  "2041-2100",
  "High and very-high thermal and water risks; about 35-75% of studied area potentially economically unfeasible",
  "Multiple adaptation options evaluated conceptually",
  "https://doi.org/10.1016/j.scitotenv.2023.167753",

  "Agrometeorological yield study 2024",
  "36 Brazilian Arabica locations",
  "Crop model plus CMIP6",
  "2041-2100",
  "Highly heterogeneous response; losses can reach 100% in hot-dry Aw climates under high emissions",
  "Irrigation and CO2 effects represented",
  "https://www.sciencedirect.com/science/article/pii/S0048969724077039",

  "Agricultural zoning study 2025",
  "Brazil, Arabica",
  "Agroclimatic zoning",
  "2041-2080",
  "Mean reduction of about 50% in suitable area; heat and water deficit dominate restrictions",
  "Not represented explicitly",
  "https://doi.org/10.1007/s10668-023-04066-3",

  "Local conditions matter 2025",
  "2,048 Brazilian municipalities",
  "Econometric yield response plus SSP2-4.5",
  "2051-2100",
  "About 8% national production decline under no adaptation; strong local heterogeneity and some gains",
  "Status-quo technology; spatial adjustment discussed",
  "https://doi.org/10.1007/s10584-025-04037-9",

  "AquaCrop Arabica study 2025",
  "Brazil, Arabica",
  "Process-based AquaCrop plus CMIP6",
  "future climate periods",
  "Mixed regional yield response; shorter cycles and quality risks despite yield gains in some regions",
  "Physiology represented; regional responses emphasized",
  "https://www.sciencedirect.com/science/article/pii/S0048969725010551"
)

write_csv(evidence, file.path(out_dir, "Brazil_coffee_future_climate_risk_evidence.csv"))

# Simple evidence matrix as a publication-ready table figure
evidence2 <- evidence |>
  mutate(
    study = factor(study, levels = rev(study)),
    short_framework = case_when(
      str_detect(framework, "zoning") ~ "Zoning",
      str_detect(framework, "suitability") ~ "Suitability",
      str_detect(framework, "Econometric") ~ "Econometric",
      str_detect(framework, "AquaCrop") ~ "Process model",
      str_detect(framework, "Crop model") ~ "Crop model",
      TRUE ~ "Risk indicators"
    ),
    adaptation_class = case_when(
      str_detect(adaptation, "Not represented") ~ "No explicit adaptation",
      TRUE ~ "Adaptation represented or discussed"
    )
  )

p <- ggplot(evidence2, aes(x = short_framework, y = study)) +
  geom_point(aes(shape = adaptation_class), size = 3.3, colour = "#315A70") +
  labs(
    title = "Published projections agree on increasing climate risk but differ in magnitude and endpoint",
    subtitle = "Brazil-focused studies assess suitability, risk, yield or production under different adaptation assumptions",
    x = "Projection framework",
    y = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 13, base_family = "serif") +
  theme(
    panel.grid.major.x = element_line(colour = "grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

ragg::agg_png(
  file.path(fig_dir, "FigureS_future_climate_risk_frameworks.png"),
  width = 3600, height = 2100, res = 300
)
print(p)
dev.off()

message("STEP 37 COMPLETED")
message("Evidence table: ", file.path(out_dir, "Brazil_coffee_future_climate_risk_evidence.csv"))
