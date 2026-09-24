# ============================================================
# DOWNLOAD IBGE PAM COFFEE DATA
# IBGE Aggregate API v3
# Table 5457
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# Package
# ------------------------------------------------------------

if (!requireNamespace("ibger", quietly = TRUE)) {
  install.packages("ibger")
}

library(ibger)

options(timeout = 600)

# ------------------------------------------------------------
# Directory
# ------------------------------------------------------------

base_dir <- "."

raw_dir <- file.path(
  base_dir,
  "01_data_raw",
  "IBGE_PAM"
)

dir.create(
  raw_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

variables <- c(
  8331, # Area planted or destined for harvest
  216,  # Area harvested
  214,  # Production
  112   # Yield
)

# 1974-2011: coffee total
years_total_only <- 1974:2011

# 2012-2024: total + Arabica + Canephora
years_species <- 2012:2024

# ------------------------------------------------------------
# Download function
# ------------------------------------------------------------

download_year <- function(year, categories) {

  file_out <- file.path(
    raw_dir,
    paste0("IBGE_PAM_coffee_", year, ".rds")
  )

  if (file.exists(file_out)) {
    message("Already downloaded: ", year)
    return(invisible(NULL))
  }

  message("Downloading: ", year)

  dat <- ibger::ibge_variables(
    aggregate = 5457,
    variable = variables,
    periods = year,
    localities = "N6",
    classification = list(
      "782" = categories
    ),
    chunk = TRUE
  )

  saveRDS(
    dat,
    file_out
  )

  message("Saved: ", year)
}

# ------------------------------------------------------------
# Historical series: total coffee
# ------------------------------------------------------------

for (year in years_total_only) {

  tryCatch(
    {
      download_year(
        year = year,
        categories = 40139
      )
    },
    error = function(e) {
      message(
        "ERROR in ",
        year,
        ": ",
        conditionMessage(e)
      )
    }
  )
}

# ------------------------------------------------------------
# 2012 onward: total + Arabica + Canephora
# ------------------------------------------------------------

for (year in years_species) {

  tryCatch(
    {
      download_year(
        year = year,
        categories = c(
          40139,
          40140,
          40141
        )
      )
    },
    error = function(e) {
      message(
        "ERROR in ",
        year,
        ": ",
        conditionMessage(e)
      )
    }
  )
}

message("\nIBGE coffee download finished.")
