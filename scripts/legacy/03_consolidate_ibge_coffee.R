# ============================================================
# CONSOLIDATE IBGE PAM COFFEE DATA
# Brazil municipalities, 1974 to 2024
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# Packages
# ------------------------------------------------------------

packages <- c(
  "ibger",
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "readr"
)

new_packages <- packages[
  !(packages %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

invisible(
  lapply(packages, library, character.only = TRUE)
)

# ------------------------------------------------------------
# Directories
# ------------------------------------------------------------

base_dir <- "."

raw_dir <- file.path(
  base_dir,
  "01_data_raw",
  "IBGE_PAM"
)

processed_dir <- file.path(
  base_dir,
  "02_data_processed"
)

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Find downloaded files
# ------------------------------------------------------------

files <- list.files(
  raw_dir,
  pattern = "^IBGE_PAM_coffee_[0-9]{4}\\.rds$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No IBGE PAM coffee RDS files were found.")
}

files <- files[
  order(
    as.integer(
      str_extract(
        basename(files),
        "[0-9]{4}"
      )
    )
  )
]

# ------------------------------------------------------------
# Helper: identify coffee species
# ------------------------------------------------------------

identify_species <- function(dat, year) {

  # Before 2012 only total coffee was requested
  if (year < 2012) {
    return(
      rep(
        "Total",
        nrow(dat)
      )
    )
  }

  # Search for a column containing category IDs
  code_candidates <- names(dat)[
    vapply(
      dat,
      function(x) {
        vals <- unique(as.character(x))
        any(
          vals %in% c(
            "40139",
            "40140",
            "40141"
          ),
          na.rm = TRUE
        )
      },
      logical(1)
    )
  ]

  if (length(code_candidates) > 0) {

    x <- as.character(
      dat[[code_candidates[1]]]
    )

    species <- case_when(
      x == "40139" ~ "Total",
      x == "40140" ~ "Arabica",
      x == "40141" ~ "Canephora",
      TRUE ~ NA_character_
    )

    if (any(!is.na(species))) {
      return(species)
    }
  }

  # Fallback: search text labels across all columns
  text_matrix <- dat |>
    mutate(
      across(
        everything(),
        as.character
      )
    )

  species <- rep(
    NA_character_,
    nrow(dat)
  )

  for (i in seq_len(nrow(text_matrix))) {

    row_text <- paste(
      unlist(
        text_matrix[i, ],
        use.names = FALSE
      ),
      collapse = " | "
    )

    species[i] <- case_when(
      str_detect(
        row_text,
        regex(
          "arábica|arabica",
          ignore_case = TRUE
        )
      ) ~ "Arabica",

      str_detect(
        row_text,
        regex(
          "canephora|conilon|robusta",
          ignore_case = TRUE
        )
      ) ~ "Canephora",

      str_detect(
        row_text,
        regex(
          "café.*total|cafe.*total",
          ignore_case = TRUE
        )
      ) ~ "Total",

      TRUE ~ NA_character_
    )
  }

  species
}

# ------------------------------------------------------------
# Read and standardize one year
# ------------------------------------------------------------

read_one_year <- function(file) {

  year <- as.integer(
    str_extract(
      basename(file),
      "[0-9]{4}"
    )
  )

  message(
    "Reading ",
    year
  )

  dat <- readRDS(file)

  required_cols <- c(
    "variable_id",
    "variable_name",
    "variable_unit",
    "locality_id",
    "locality_name",
    "period",
    "value"
  )

  missing_cols <- setdiff(
    required_cols,
    names(dat)
  )

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Unexpected structure in year ",
        year,
        ". Missing columns: ",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }

  dat |>
    mutate(
      year = year,
      species = identify_species(
        dat,
        year
      ),
      value_numeric = ibger::parse_ibge_value(
        value
      ),
      municipality_code = as.character(
        locality_id
      ),
      municipality = as.character(
        locality_name
      ),
      variable_id = as.character(
        variable_id
      )
    ) |>
    select(
      municipality_code,
      municipality,
      year,
      species,
      variable_id,
      variable_name,
      variable_unit,
      value_numeric
    )
}

# ------------------------------------------------------------
# Combine all years
# ------------------------------------------------------------

coffee_long <- map_dfr(
  files,
  read_one_year
)

# ------------------------------------------------------------
# Check species recognition
# ------------------------------------------------------------

species_check <- coffee_long |>
  count(
    year,
    species,
    name = "n"
  )

write_csv(
  species_check,
  file.path(
    processed_dir,
    "coffee_brazil_species_check.csv"
  )
)

if (
  any(
    coffee_long$year >= 2012 &
      is.na(coffee_long$species)
  )
) {

  bad_years <- coffee_long |>
    filter(
      year >= 2012,
      is.na(species)
    ) |>
    distinct(year) |>
    pull(year)

  stop(
    paste0(
      "Species could not be identified for years: ",
      paste(
        bad_years,
        collapse = ", "
      ),
      ". See coffee_brazil_species_check.csv."
    )
  )
}

# ------------------------------------------------------------
# Convert variables to wide format
# ------------------------------------------------------------

coffee_master <- coffee_long |>
  mutate(
    variable = case_when(
      variable_id == "8331" ~ "area_destined_ha",
      variable_id == "216"  ~ "area_harvested_ha",
      variable_id == "214"  ~ "production_t",
      variable_id == "112"  ~ "yield_kg_ha",
      TRUE ~ paste0(
        "variable_",
        variable_id
      )
    )
  ) |>
  select(
    municipality_code,
    municipality,
    year,
    species,
    variable,
    value_numeric
  ) |>
  distinct() |>
  pivot_wider(
    names_from = variable,
    values_from = value_numeric
  ) |>
  arrange(
    municipality_code,
    year,
    species
  )

# ------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------

cat("\n====================================\n")
cat("BRAZIL COFFEE MASTER DATASET\n")
cat("====================================\n")

cat(
  "\nRows:",
  format(
    nrow(coffee_master),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Municipal codes:",
  dplyr::n_distinct(
    coffee_master$municipality_code
  ),
  "\n"
)

cat(
  "Years:",
  min(
    coffee_master$year,
    na.rm = TRUE
  ),
  "to",
  max(
    coffee_master$year,
    na.rm = TRUE
  ),
  "\n"
)

cat("\nSpecies by period:\n")

print(
  coffee_master |>
    group_by(species) |>
    summarise(
      first_year = min(
        year,
        na.rm = TRUE
      ),
      last_year = max(
        year,
        na.rm = TRUE
      ),
      municipalities = n_distinct(
        municipality_code
      ),
      .groups = "drop"
    )
)

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

csv_file <- file.path(
  processed_dir,
  "coffee_brazil_municipality_1974_2024.csv"
)

rds_file <- file.path(
  processed_dir,
  "coffee_brazil_municipality_1974_2024.rds"
)

write_csv(
  coffee_master,
  csv_file
)

saveRDS(
  coffee_master,
  rds_file
)

cat("\nSaved:\n")
cat(csv_file, "\n")
cat(rds_file, "\n")

message("\nConsolidation finished successfully.")
