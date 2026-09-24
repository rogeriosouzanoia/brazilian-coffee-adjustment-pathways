# ============================================================
# DOWNLOAD IBGE PAM COFFEE DATA
# SIDRA TABLE 5457
# ============================================================

rm(list = ls())

packages <- c("sidrar", "tidyverse")

new_packages <- packages[
  !(packages %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

invisible(
  lapply(packages, library, character.only = TRUE)
)

options(
  sidrar.timeout = 180,
  sidrar.retries = 5
)

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

years <- 1974:2024

variables <- c(
  8331,
  216,
  214,
  112
)

coffee_categories <- c(
  40139,
  40140,
  40141
)

for (year in years) {

  file_out <- file.path(
    raw_dir,
    paste0(
      "IBGE_PAM_coffee_",
      year,
      ".rds"
    )
  )

  if (file.exists(file_out)) {
    message("Already downloaded: ", year)
    next
  }

  message("Downloading: ", year)

  tryCatch({

    dat <- sidrar::get_sidra(
      x = 5457,
      variable = variables,
      period = as.character(year),
      geo = "City",
      classific = "c782",
      category = list(
        coffee_categories
      ),
      format = 4,
      value_type = "numeric"
    )

    saveRDS(
      dat,
      file_out
    )

    message("Saved: ", year)

  }, error = function(e) {

    message(
      "ERROR in ",
      year,
      ": ",
      conditionMessage(e)
    )

  })

  Sys.sleep(0.5)
}

message("\nDownload finished.")
