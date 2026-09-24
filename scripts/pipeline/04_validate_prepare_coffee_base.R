# ======================================================================
# COFFE-STUDY | 04_validate_prepare_coffee_base.R
# Audit and prepare the supplied consolidated IBGE PAM coffee data.
# No download. No package installation. Original input files are preserved.
# Run this entire script with Source in RStudio.
# ======================================================================

main <- function() {

  base_dir <- Sys.getenv(
    "COFFEE_STUDY_DIR",
    unset = "."
  )
  processed_dir <- file.path(base_dir, "02_data_processed")
  audit_dir <- file.path(base_dir, "04_outputs", "audit")
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(audit_dir)) stop("Could not create: ", audit_dir, call. = FALSE)

  input_stub <- file.path(processed_dir, "coffee_brazil_municipality_1974_2024")
  csv_input <- paste0(input_stub, ".csv")
  rds_input <- paste0(input_stub, ".rds")
  expected_years <- 1974:2024
  species_order <- c("Total", "Arabica", "Canephora")
  numeric_columns <- c("area_destined_ha", "area_harvested_ha", "production_t", "yield_kg_ha")
  required_columns <- c("municipality_code", "municipality", "year", "species", numeric_columns)

  # Analyst-selected tolerance for comparing integer published yield with P/A.
  # This is an audit threshold, not a statement about measurement precision.
  yield_tolerance_kg_ha <- 1 + 1e-6
  additive_tolerance <- 1e-8

  # The split at 2002 is a conservative temporal comparison rule.
  # Historical metadata report a change from cafe em coco to beneficiado.
  # No conversion factor is applied; all supplied numbers remain unchanged.
  # Archive the primary IBGE methodological note before publication.
  # Metadata reference located in the public BDE/PE catalogue:
  # https://www.bde.pe.gov.br/
  # Primary source to archive: https://sidra.ibge.gov.br/tabela/5457
  comparison_start_year <- 2002L

  write_csv_safe <- function(x, path) {
    tmp <- tempfile(pattern = "coffee_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not write: ", path, call. = FALSE)
  }
  save_rds_safe <- function(x, path) {
    tmp <- tempfile(pattern = "coffee_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not write: ", path, call. = FALSE)
  }
  positive <- function(x) !is.na(x) & x > 0
  negative <- function(x) !is.na(x) & x < 0
  sum_available <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  key_for <- function(x) paste(x$municipality_code, x$year, x$species, sep = "|")
  normalize_input <- function(x) {
    if (!is.data.frame(x)) stop("The input is not a data frame.", call. = FALSE)
    missing <- setdiff(required_columns, names(x))
    if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
    x <- as.data.frame(x[, required_columns, drop = FALSE], stringsAsFactors = FALSE)
    x$municipality_code <- as.character(x$municipality_code)
    x$municipality <- as.character(x$municipality)
    x$species <- as.character(x$species)
    if (!is.numeric(x$year) || anyNA(x$year) || any(x$year != floor(x$year))) {
      stop("year must contain nonmissing integer years.", call. = FALSE)
    }
    x$year <- as.integer(x$year)
    for (nm in numeric_columns) {
      if (!is.numeric(x[[nm]])) stop("Expected numeric column: ", nm, call. = FALSE)
      if (any(!is.finite(x[[nm]]) & !is.na(x[[nm]]))) stop("Infinite values in: ", nm, call. = FALSE)
    }
    x
  }

  message("Reading the existing consolidated files...")
  read_csv_input <- function() utils::read.csv(
    csv_input, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c("character", "character", "integer", "character", rep("numeric", 4)),
    na.strings = c("NA", ""), fileEncoding = "UTF-8"
  )
  if (file.exists(rds_input)) {
    dat <- normalize_input(readRDS(rds_input))
  } else if (file.exists(csv_input)) {
    dat <- normalize_input(read_csv_input())
  } else {
    stop("Neither input file was found in:\n", processed_dir, call. = FALSE)
  }
  if (!nrow(dat)) stop("The consolidated input is empty.", call. = FALSE)

  csv_rds_status <- "Not compared: only one format was available"
  if (file.exists(csv_input) && file.exists(rds_input)) {
    csv_dat <- normalize_input(read_csv_input())
    equal <- isTRUE(all.equal(dat, csv_dat, check.attributes = FALSE, tolerance = 1e-12))
    if (!equal) stop("CSV and RDS differ. Original files were left unchanged.", call. = FALSE)
    csv_rds_status <- "Same row order and cell values"
    rm(csv_dat)
  }

  if (anyNA(dat$municipality_code) || any(!grepl("^[0-9]{7}$", dat$municipality_code))) {
    stop("Invalid municipality codes. Seven digits are required.", call. = FALSE)
  }
  if (anyNA(dat$species) || any(!dat$species %in% species_order)) {
    stop("Unexpected or missing species labels.", call. = FALSE)
  }
  if (any(!dat$year %in% expected_years)) stop("Unexpected years in the input.", call. = FALSE)
  missing_years <- setdiff(expected_years, unique(dat$year))
  if (length(missing_years)) stop("Missing years: ", paste(missing_years, collapse = ", "), call. = FALSE)
  if (any(dat$year < 2012 & dat$species != "Total")) {
    stop("Species-specific records occur before 2012; inspect the source.", call. = FALSE)
  }
  expected_groups <- c(paste(1974:2011, "Total"), as.vector(outer(2012:2024, species_order, paste)))
  missing_groups <- setdiff(expected_groups, paste(dat$year, dat$species))
  if (length(missing_groups)) stop("Missing year/species groups: ", paste(missing_groups, collapse = ", "), call. = FALSE)

  key <- key_for(dat)
  duplicate <- duplicated(key) | duplicated(key, fromLast = TRUE)
  write_csv_safe(dat[duplicate, , drop = FALSE], file.path(audit_dir, "coffee_duplicate_keys.csv"))
  if (any(duplicate)) stop("Duplicate keys found. See coffee_duplicate_keys.csv.", call. = FALSE)

  # All original columns, including reported yield, are retained unchanged.
  dat$state_code <- substr(dat$municipality_code, 1, 2)
  state_lookup <- c(
    "11"="RO", "12"="AC", "13"="AM", "14"="RR", "15"="PA", "16"="AP", "17"="TO",
    "21"="MA", "22"="PI", "23"="CE", "24"="RN", "25"="PB", "26"="PE", "27"="AL", "28"="SE", "29"="BA",
    "31"="MG", "32"="ES", "33"="RJ", "35"="SP", "41"="PR", "42"="SC", "43"="RS",
    "50"="MS", "51"="MT", "52"="GO", "53"="DF"
  )
  dat$state <- unname(state_lookup[dat$state_code])
  if (anyNA(dat$state)) stop("An unknown state prefix was found.", call. = FALSE)
  dat$comparison_period <- ifelse(dat$year < comparison_start_year, "1974_2001", "2002_2024")
  dat$yield_calculated_kg_ha <- NA_real_
  valid_ratio <- !is.na(dat$production_t) & positive(dat$area_harvested_ha)
  dat$yield_calculated_kg_ha[valid_ratio] <- dat$production_t[valid_ratio] * 1000 / dat$area_harvested_ha[valid_ratio]
  dat$yield_difference_kg_ha <- dat$yield_kg_ha - dat$yield_calculated_kg_ha
  dat$flag_yield_difference <- !is.na(dat$yield_difference_kg_ha) & abs(dat$yield_difference_kg_ha) > yield_tolerance_kg_ha
  dat$flag_reported_yield_without_area <- positive(dat$yield_kg_ha) & (is.na(dat$area_harvested_ha) | dat$area_harvested_ha <= 0)
  dat$flag_production_without_area <- positive(dat$production_t) & (is.na(dat$area_harvested_ha) | dat$area_harvested_ha <= 0)
  dat$flag_area_exceeds_destined <- !is.na(dat$area_harvested_ha) & !is.na(dat$area_destined_ha) & dat$area_harvested_ha > dat$area_destined_ha
  dat$flag_negative_value <- Reduce(`|`, lapply(dat[numeric_columns], negative))
  dat$all_original_values_missing <- rowSums(!is.na(dat[numeric_columns])) == 0
  dat$record_status <- "Other combination: inspect"
  complete_pa <- !is.na(dat$production_t) & !is.na(dat$area_harvested_ha)
  dat$record_status[!complete_pa] <- "Production or harvested area missing"
  dat$record_status[complete_pa & dat$production_t == 0 & dat$area_harvested_ha == 0] <- "Zero production and zero harvested area"
  dat$record_status[complete_pa & dat$production_t == 0 & dat$area_harvested_ha > 0] <- "Zero production with positive harvested area"
  dat$record_status[complete_pa & dat$production_t > 0 & dat$area_harvested_ha > 0] <- "Positive production and harvested area"
  dat$record_status[dat$all_original_values_missing] <- "All original values missing"

  flag_columns <- grep("^flag_", names(dat), value = TRUE)
  flagged <- Reduce(`|`, dat[flag_columns])
  write_csv_safe(dat[flagged, , drop = FALSE], file.path(audit_dir, "coffee_flagged_records.csv"))

  # Total, Arabica and Canephora are compared, never summed together.
  message("Checking Total = Arabica + Canephora...")
  total <- dat[dat$species == "Total" & dat$year >= 2012, , drop = FALSE]
  arabica <- dat[dat$species == "Arabica", , drop = FALSE]
  canephora <- dat[dat$species == "Canephora", , drop = FALSE]
  pairkey <- function(x) paste(x$municipality_code, x$year, sep = "|")
  ia <- match(pairkey(total), pairkey(arabica))
  ic <- match(pairkey(total), pairkey(canephora))
  additive_vars <- c("production_t", "area_harvested_ha", "area_destined_ha")
  additive <- do.call(rbind, lapply(additive_vars, function(nm) {
    tval <- total[[nm]]
    aval <- arabica[[nm]][ia]
    cval <- canephora[[nm]][ic]
    complete <- !is.na(tval) & !is.na(aval) & !is.na(cval)
    difference <- rep(NA_real_, length(tval))
    difference[complete] <- tval[complete] - aval[complete] - cval[complete]
    status <- rep("Not testable: missing value or species row", length(tval))
    status[complete] <- "Match"
    status[complete & abs(difference) > additive_tolerance] <- "Mismatch"
    data.frame(municipality_code = total$municipality_code, year = total$year,
      variable = nm, total = tval, arabica = aval, canephora = cval,
      difference = difference, status = status, stringsAsFactors = FALSE)
  }))
  write_csv_safe(additive, file.path(audit_dir, "coffee_species_additivity.csv"))

  # Coverage means coverage WITHIN THE SUPPLIED FILE, not national completeness.
  group_id <- interaction(dat$year, dat$species, drop = TRUE, lex.order = TRUE)
  groups <- split(seq_len(nrow(dat)), group_id)
  coverage <- do.call(rbind, lapply(groups, function(idx) {
    x <- dat[idx, , drop = FALSE]
    data.frame(year = x$year[1], species = x$species[1],
      records = nrow(x), municipality_codes_in_file = length(unique(x$municipality_code)),
      positive_production_records = sum(positive(x$production_t)),
      zero_production_records = sum(!is.na(x$production_t) & x$production_t == 0),
      missing_production_records = sum(is.na(x$production_t)),
      missing_area_records = sum(is.na(x$area_harvested_ha)),
      all_original_values_missing = sum(x$all_original_values_missing),
      stringsAsFactors = FALSE)
  }))
  coverage <- coverage[order(coverage$year, match(coverage$species, species_order)), ]
  rownames(coverage) <- NULL
  write_csv_safe(coverage, file.path(audit_dir, "coffee_coverage_by_year_species.csv"))

  # Sums of available records are explicitly labelled as such.
  sums <- do.call(rbind, lapply(groups, function(idx) {
    x <- dat[idx, , drop = FALSE]
    paired <- !is.na(x$production_t) & !is.na(x$area_harvested_ha)
    p <- sum_available(x$production_t)
    a <- sum_available(x$area_harvested_ha)
    pp <- sum_available(x$production_t[paired])
    aa <- sum_available(x$area_harvested_ha[paired])
    y <- if (!is.na(aa) && aa > 0 && !is.na(pp)) pp * 1000 / aa else NA_real_
    data.frame(year = x$year[1], species = x$species[1], comparison_period = x$comparison_period[1],
      production_t_sum_available = p, harvested_area_ha_sum_available = a,
      yield_kg_ha_from_paired_sums = y,
      paired_records = sum(paired), missing_production_records = sum(is.na(x$production_t)),
      missing_area_records = sum(is.na(x$area_harvested_ha)),
      official_national_total_verified = FALSE, stringsAsFactors = FALSE)
  }))
  sums <- sums[order(sums$year, match(sums$species, species_order)), ]
  rownames(sums) <- NULL
  write_csv_safe(sums, file.path(processed_dir, "coffee_brazil_sums_available_records.csv"))

  # The original species-check file counts long-format rows (four variables).
  long_count_status <- "Not checked: species-check file not found"
  long_check_path <- file.path(processed_dir, "coffee_brazil_species_check.csv")
  if (file.exists(long_check_path)) {
    ck <- utils::read.csv(long_check_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    if (all(c("year", "species", "n") %in% names(ck))) {
      k1 <- paste(coverage$year, coverage$species)
      k2 <- paste(ck$year, ck$species)
      im <- match(k1, k2)
      ok <- !anyNA(im) && !anyDuplicated(k2) && length(k1) == length(k2) && all(ck$n[im] == 4 * coverage$records)
      long_count_status <- if (ok) "Matches: four long-format variables per master row" else "Mismatch: inspect species-check counts"
    } else long_count_status <- "Unexpected schema: species-check file not evaluated"
  }

  # Original geography is retained. No historical boundary harmonization yet.
  by_municipality <- split(seq_len(nrow(dat)), dat$municipality_code)
  geo_targets <- do.call(rbind, lapply(by_municipality, function(idx) {
    x <- dat[idx, , drop = FALSE]
    productive <- positive(x$production_t) | positive(x$area_harvested_ha)
    if (!any(productive)) return(NULL)
    latest <- which.max(x$year)
    years_positive <- sort(unique(x$year[productive]))
    data.frame(municipality_code = x$municipality_code[1],
      municipality_label_latest_in_file = x$municipality[latest],
      state = x$state[1], first_year_with_positive_production_or_area = min(years_positive),
      last_year_with_positive_production_or_area = max(years_positive),
      years_with_positive_production_or_area = length(years_positive),
      boundary_harmonization_done = FALSE, stringsAsFactors = FALSE)
  }))
  if (is.null(geo_targets)) stop("No positive coffee area or production was found.", call. = FALSE)
  rownames(geo_targets) <- NULL
  write_csv_safe(geo_targets, file.path(processed_dir, "coffee_municipality_spatial_targets.csv"))

  audited_stub <- file.path(processed_dir, "coffee_brazil_municipality_audited_1974_2024")
  write_csv_safe(dat, paste0(audited_stub, ".csv"))
  save_rds_safe(dat, paste0(audited_stub, ".rds"))
  recent_total <- dat[dat$species == "Total" & dat$year >= comparison_start_year, , drop = FALSE]
  recent_species <- dat[dat$species %in% c("Arabica", "Canephora") & dat$year >= 2012, , drop = FALSE]
  save_rds_safe(recent_total, file.path(processed_dir, "coffee_brazil_total_2002_2024.rds"))
  save_rds_safe(recent_species, file.path(processed_dir, "coffee_brazil_species_2012_2024.rds"))

  summary_values <- c(
    rows = nrow(dat), municipality_codes_in_file = length(unique(dat$municipality_code)),
    first_year = min(dat$year), last_year = max(dat$year), duplicate_keys = sum(duplicate),
    all_original_values_missing_rows = sum(dat$all_original_values_missing),
    yield_discrepancy_rows = sum(dat$flag_yield_difference),
    reported_yield_without_area_rows = sum(dat$flag_reported_yield_without_area),
    production_without_area_rows = sum(dat$flag_production_without_area),
    area_exceeds_destined_rows = sum(dat$flag_area_exceeds_destined),
    negative_value_rows = sum(dat$flag_negative_value), total_flagged_rows = sum(flagged),
    additivity_tested_cells = sum(additive$status != "Not testable: missing value or species row"),
    additivity_mismatch_cells = sum(additive$status == "Mismatch"),
    additivity_untestable_cells = sum(additive$status == "Not testable: missing value or species row"),
    spatial_target_municipalities = nrow(geo_targets),
    csv_rds_comparison = csv_rds_status, long_format_count_check = long_count_status,
    original_unit_metadata_verified = "No: original variable_unit columns are absent from the consolidated input",
    original_species_labels_verified = "No: original API category labels are absent from the consolidated input",
    official_national_totals_verified = "No", historical_boundaries_harmonized = "No"
  )
  audit_summary <- data.frame(check = names(summary_values), result = unname(as.character(summary_values)), stringsAsFactors = FALSE)
  summary_file <- file.path(audit_dir, "coffee_validation_summary.csv")
  write_csv_safe(audit_summary, summary_file)

  inputs <- c(csv_input, rds_input, long_check_path)
  inputs <- inputs[file.exists(inputs)]
  provenance <- data.frame(file = basename(inputs), md5 = unname(tools::md5sum(inputs)),
    checked_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), stringsAsFactors = FALSE)
  write_csv_safe(provenance, file.path(audit_dir, "coffee_input_provenance.csv"))

  notes <- c(
    "COFFE-STUDY: AUDIT AND ANALYSIS PREPARATION",
    "",
    paste("CSV/RDS comparison:", csv_rds_status),
    "All original input files and original measurement columns are preserved.",
    "yield_calculated_kg_ha = production_t * 1000 / area_harvested_ha, only where area > 0 and production is reported.",
    "An undefined yield (zero/missing denominator) stays NA. It is never forced to zero.",
    "Reported yield is retained in yield_kg_ha. Flagged differences are not silently corrected.",
    "NA remains missing. Existing zeros are retained as supplied; original API symbols must be checked in the annual raw files.",
    "Never sum Total + Arabica + Canephora: that would count coffee twice.",
    "Sum-of-available-records outputs are not validated official national totals.",
    "",
    "TEMPORAL COMPARABILITY",
    "The 1974-2001 and 2002-2024 periods are kept separate as a conservative comparison rule.",
    "The BDE/PE agricultural metadata note reports cafe em coco through 2001 and beneficiado from 2002.",
    "The primary IBGE methodological note still needs to be archived and checked against this series before publication.",
    "No assumed conversion between product forms is applied. Archived data are kept in their reported basis.",
    "Starting an analysis in 2002 does not itself validate original units or harmonize historical boundaries.",
    "",
    "SPATIAL COMPARABILITY",
    "Municipality codes identify supplied records; they do not demonstrate constant geographic boundaries over time.",
    "The spatial-target file is a list for a subsequent geographic join, not coffee-field coordinates.",
    "No coordinates, terrain elevation, coffee-field elevation or climate effects are estimated in this step.",
    "A change in production-weighted municipality elevation alone would not demonstrate within-municipality uphill movement.",
    "",
    "SOURCE AND VALIDATION LIMITS",
    "The original category labels, original variable-unit labels and original missing-value symbols are not in this master.",
    "This audit establishes internal numerical consistency, not external completeness or causal attribution.",
    "Archive source metadata, verify national/state totals and harmonize boundaries before final spatial trends.",
    "https://sidra.ibge.gov.br/tabela/5457",
    "https://servicodados.ibge.gov.br/api/v3/agregados/5457/metadados",
    "https://www.bde.pe.gov.br/"
  )
  writeLines(enc2utf8(notes), file.path(audit_dir, "coffee_audit_notes.txt"), useBytes = TRUE)

  cat("\n=====================================================\n")
  cat("COFFEE DATA AUDIT COMPLETED\n")
  cat("=====================================================\n")
  cat("Rows: ", format(nrow(dat), big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("Municipality codes in the file: ", length(unique(dat$municipality_code)), "\n", sep = "")
  cat("CSV/RDS: ", csv_rds_status, "\n", sep = "")
  cat("Flagged records: ", sum(flagged), "\n", sep = "")
  cat("Species additivity mismatches: ", sum(additive$status == "Mismatch"), "\n", sep = "")
  cat("Geographic targets: ", nrow(geo_targets), "\n", sep = "")
  cat("\nOriginal files preserved. No data were downloaded.\n")
  cat("\nAudit summary:\n", summary_file, "\n", sep = "")
  cat("\nAudited master:\n", paste0(audited_stub, ".rds"), "\n", sep = "")
  cat("\nIMPORTANT: This is an internal audit; national completeness and historical geography remain to be validated.\n")
  invisible(audit_summary)
}

main()
