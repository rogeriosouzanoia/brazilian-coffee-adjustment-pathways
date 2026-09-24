# ============================================================
# CHECK IBGE PAM COFFEE DOWNLOAD
# ============================================================

rm(list = ls())

base_dir <- "."

raw_dir <- file.path(
  base_dir,
  "01_data_raw",
  "IBGE_PAM"
)

files <- list.files(
  raw_dir,
  pattern = "^IBGE_PAM_coffee_[0-9]{4}\\.rds$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No IBGE coffee RDS files were found.")
}

years_found <- as.integer(
  sub(
    "^IBGE_PAM_coffee_([0-9]{4})\\.rds$",
    "\\1",
    basename(files)
  )
)

expected_years <- 1974:2024
missing_years <- setdiff(expected_years, years_found)

cat("\n====================================\n")
cat("IBGE COFFEE DOWNLOAD CHECK\n")
cat("====================================\n")

cat("\nFiles found:", length(files), "\n")
cat("First year:", min(years_found), "\n")
cat("Last year:", max(years_found), "\n")

if (length(missing_years) == 0) {
  cat("Missing years: NONE\n")
} else {
  cat("Missing years:", paste(missing_years, collapse = ", "), "\n")
}

inspect_years <- c(1974, 2011, 2012, 2024)
inspect_years <- inspect_years[inspect_years %in% years_found]

for (yr in inspect_years) {

  cat("\n\n------------------------------------\n")
  cat("YEAR:", yr, "\n")
  cat("------------------------------------\n")

  f <- file.path(
    raw_dir,
    paste0("IBGE_PAM_coffee_", yr, ".rds")
  )

  dat <- readRDS(f)

  cat("Rows:", nrow(dat), "\n")
  cat("Columns:", ncol(dat), "\n")

  cat("\nColumn names:\n")
  print(names(dat))

  char_cols <- names(dat)[
    vapply(
      dat,
      function(x) is.character(x) || is.factor(x),
      logical(1)
    )
  ]

  if (length(char_cols) > 0) {

    matches <- unique(
      unlist(
        lapply(
          dat[char_cols],
          function(x) {
            vals <- unique(as.character(x))
            vals[
              grepl(
                "caf|arab|caneph|conilon|robusta",
                vals,
                ignore.case = TRUE
              )
            ]
          }
        )
      )
    )

    matches <- matches[
      !is.na(matches) &
      nzchar(matches)
    ]

    cat("\nCoffee-related labels found:\n")

    if (length(matches) == 0) {
      cat("None found in character columns.\n")
    } else {
      print(matches)
    }
  }

  cat("\nFirst rows:\n")
  print(utils::head(dat, 3))
}

audit <- data.frame(
  expected_year = expected_years,
  downloaded = expected_years %in% years_found
)

audit_file <- file.path(
  raw_dir,
  "IBGE_PAM_download_audit.csv"
)

write.csv(
  audit,
  audit_file,
  row.names = FALSE
)

cat("\n\nAudit saved to:\n")
cat(audit_file, "\n")
