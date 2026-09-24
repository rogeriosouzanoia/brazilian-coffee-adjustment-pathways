# ======================================================================
# COFFE-STUDY | 07_check_ibge_official_totals.R
# Validate Figure 1 inputs against official IBGE national/state aggregates.
# Run this ENTIRE file with Source in RStudio.
#
# Reads the existing audited municipality data. It does NOT download
# municipality production or geographic boundaries again.
# Only metadata and small national/state aggregate responses are requested.
# Uses the official Aggregates API v3, NOT apisidra or the sidrar package.
#
# Source and API contract:
# https://servicodados.ibge.gov.br/api/docs/agregados?versao=3
# https://servicodados.ibge.gov.br/api/v3/agregados/5457/metadados
# https://sidra.ibge.gov.br/tabela/5457
#
# Scope: 2012 to 2024, matching Figure 1.
# No original measurement is changed; differences are reported, not fixed.
# Agreement of sums does not validate historical boundaries, individual
# municipality values, causal climate attribution, or field elevations.
# Live API execution must be checked in the user's R environment.
# ======================================================================

main <- function() {
  base_dir <- Sys.getenv(
    "COFFEE_STUDY_DIR",
    unset = "."
  )
  years <- 2012:2024
  force_refresh <- FALSE
  max_attempts <- 3L
  timeout_seconds <- 120
  pause_seconds <- 0.5
  # Inputs are reported as integer tonnes/hectares. Do not hide a one-unit
  # discrepancy. The separate yield allowance is one kg/ha for rounding.
  additive_tolerance <- 1e-6
  yield_rounding_allowance <- 1.000001
  api_base <- "https://servicodados.ibge.gov.br/api/v3/agregados/5457"

  processed_dir <- file.path(base_dir, "02_data_processed")
  input_file <- file.path(processed_dir, "coffee_brazil_municipality_audited_1974_2024.rds")
  cache_dir <- file.path(base_dir, "01_data_raw", "IBGE_PAM", "official_checks_step07")
  out_dir <- file.path(base_dir, "04_outputs", "official_validation")
  for (p in c(cache_dir, out_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create folder: ", p, call. = FALSE)
  }
  report_path <- file.path(out_dir, "coffee_official_validation_summary.csv")
  state <- new.env(parent = emptyenv())
  state$completed <- FALSE
  state$error <- "Execution interrupted before completion."
  state$checks <- data.frame(check = character(), result = character())
  state$requests <- list()

  fail <- function(...) stop(..., call. = FALSE)
  add_check <- function(check, result) {
    state$checks <- rbind(state$checks, data.frame(
      check = check, result = paste(result, collapse = "; "), stringsAsFactors = FALSE
    ))
  }
  write_csv <- function(x, path) {
    tmp <- tempfile("coffee07_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    if (!file.copy(tmp, path, overwrite = TRUE)) fail("Cannot write: ", path)
  }
  on.exit({
    final_report <- rbind(data.frame(
      check = c("execution_status", "execution_time", "error_if_incomplete"),
      result = c(if (state$completed) "COMPLETED" else "INCOMPLETE",
                 format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                 if (state$completed) "None" else state$error)
    ), state$checks)
    try(write_csv(final_report, report_path), silent = TRUE)
    if (length(state$requests)) try(write_csv(do.call(rbind, state$requests),
      file.path(out_dir, "coffee_official_request_manifest.csv")), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()),
      file.path(out_dir, "sessionInfo_step07.txt")), silent = TRUE)
  }, add = TRUE)

  # --------------------------------------------------------------------
  # Helpers: parsing, source labels and safe caching
  # --------------------------------------------------------------------
  scalar <- function(x, default = NA_character_) {
    if (is.null(x) || length(x) != 1L) return(default)
    as.character(x)
  }
  plain <- function(x) {
    x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
    x[is.na(x)] <- ""
    tolower(gsub("[^[:alnum:]]+", " ", x))
  }
  join_rows <- function(x) {
    if (!length(x)) return(data.frame())
    rownames_out <- do.call(rbind, x)
    rownames(rownames_out) <- NULL
    rownames_out
  }
  numeric_source <- function(x) {
    x <- trimws(as.character(x))
    is_missing <- is.na(x) | x %in% c("", "NA", "..", "...", "X", "x")
    is_zero_symbol <- !is.na(x) & x == "-"
    is_number <- !is.na(x) & grepl("^[+-]?[0-9]+([.,][0-9]+)?$", x)
    value <- rep(NA_real_, length(x))
    value[is_number] <- as.numeric(sub(",", ".", x[is_number], fixed = TRUE))
    value[is_zero_symbol] <- 0
    status <- rep("UNRECOGNIZED_SYMBOL", length(x))
    status[is_missing] <- "Missing or suppressed source value"
    status[is_zero_symbol] <- "Source dash interpreted as zero"
    status[is_number] <- "Numeric source value"
    data.frame(value = value, parse_status = status, stringsAsFactors = FALSE)
  }
  same_name_and_unit <- function(variable_id, name, unit) {
    n <- plain(name)
    u <- trimws(plain(unit))
    name_ok <- switch(variable_id,
      "8331" = grepl("area", n) && grepl("plantada|destinada", n),
      "216" = grepl("area", n) && grepl("colhida", n),
      "214" = grepl("quantidade", n) && grepl("produzida", n),
      "112" = grepl("rendimento|produtividade", n), FALSE)
    unit_ok <- switch(variable_id,
      "8331" = grepl("hectare", u) || u == "ha",
      "216" = grepl("hectare", u) || u == "ha",
      "214" = grepl("tonelada|tonne", u) || u == "t",
      "112" = (grepl("quilograma|kilograma|kg", u) && grepl("hectare|ha", u)), FALSE)
    isTRUE(name_ok) && isTRUE(unit_ok)
  }
  same_species_label <- function(category, label) {
    p <- plain(label)
    coffee <- grepl("cafe|coffee", p)
    arabica <- grepl("arabica", p)
    canephora <- grepl("canephora|canefora|conilon|robusta", p)
    switch(category,
      "40139" = coffee && !arabica && !canephora,
      "40140" = coffee && arabica && !canephora,
      "40141" = coffee && canephora && !arabica,
      FALSE)
  }

  work <- function() {
    # No sidrar/ibger compatibility dependency is needed for this step.
    for (pkg in c("httr", "jsonlite")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        utils::install.packages(pkg, repos = "https://cloud.r-project.org")
      }
      if (!requireNamespace(pkg, quietly = TRUE)) fail("Package unavailable: ", pkg)
    }
    fetch_json <- function(url, file, validator) {
      object <- NULL
      cached <- FALSE
      if (file.exists(file) && !force_refresh) {
        object <- tryCatch(jsonlite::fromJSON(file, simplifyVector = FALSE),
                           error = function(e) NULL)
        if (!is.null(object) && isTRUE(validator(object))) cached <- TRUE
        else object <- NULL
      }
      if (!cached) {
        for (attempt in seq_len(max_attempts)) {
          response <- tryCatch(httr::GET(
            url, httr::timeout(timeout_seconds),
            httr::accept_json(),
            httr::user_agent("coffe-study/step07 scientific source validation")
          ), error = function(e) e)
          if (inherits(response, "error")) {
            detail <- conditionMessage(response)
            if (attempt == max_attempts) fail("Connection failed. Cached requests are retained.\n", detail)
            Sys.sleep(2^attempt)
            next
          }
          status <- httr::status_code(response)
          text <- httr::content(response, as = "text", encoding = "UTF-8")
          challenge <- grepl("<html|<!doctype|cf-chl|just a moment", substr(text, 1, 3000), ignore.case = TRUE)
          if (status %in% c(401L, 403L) || challenge) {
            fail("IBGE access blocked or HTML returned (HTTP ", status, ").\n",
                 "Stopped instead of repeating the same failure for every year.\n",
                 "No access-control challenge is bypassed; completed requests remain cached.\n", url)
          }
          if (status == 429L || status >= 500L) {
            if (attempt == max_attempts) fail("IBGE unavailable after retries (HTTP ", status, ").\n", url)
            retry_after <- suppressWarnings(as.numeric(httr::headers(response)[["retry-after"]]))
            if (!length(retry_after) || !is.finite(retry_after)) retry_after <- 2^attempt
            # A very long Retry-After is a request to stop this run, not retry sooner.
            if (retry_after > 300) fail("IBGE requested a long retry interval. Run again later; cache is retained.")
            Sys.sleep(max(2^attempt, retry_after))
            next
          }
          if (status != 200L) fail("IBGE HTTP ", status, ": ", substr(gsub("[\r\n]", " ", text), 1, 300), "\n", url)
          object <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
          if (is.null(object) || !isTRUE(validator(object))) {
            fail("Unexpected IBGE response structure; no valid-data cache was written.\n", url)
          }
          tmp <- tempfile("ibge07_", tmpdir = dirname(file), fileext = ".json")
          writeLines(enc2utf8(text), tmp, useBytes = TRUE)
          if (!file.copy(tmp, file, overwrite = TRUE)) { unlink(tmp); fail("Cannot save cache: ", file) }
          unlink(tmp)
          Sys.sleep(pause_seconds)
          break
        }
      }
      state$requests[[length(state$requests) + 1L]] <- data.frame(
        url = url, local_file = file, used_cache = cached,
        cache_file_time = format(file.info(file)$mtime, "%Y-%m-%d %H:%M:%S %Z"),
        md5 = unname(tools::md5sum(file)), stringsAsFactors = FALSE
      )
      object
    }

    # ------------------------------------------------------------------
    # Input: complete available municipal records, NOT the balanced subset
    # ------------------------------------------------------------------
    if (!file.exists(input_file)) fail("Input not found:\n", input_file)
    dat <- as.data.frame(readRDS(input_file), stringsAsFactors = FALSE)
    needed <- c("municipality_code", "year", "species", "area_destined_ha",
                "area_harvested_ha", "production_t")
    if (!all(needed %in% names(dat))) fail("Audited input missing columns: ", paste(setdiff(needed, names(dat)), collapse = ", "))
    dat$municipality_code <- as.character(dat$municipality_code)
    dat$year <- as.integer(dat$year)
    dat$species <- as.character(dat$species)
    species_map <- c("40139" = "Total", "40140" = "Arabica", "40141" = "Canephora")
    variable_map <- c("8331" = "area_destined_ha", "216" = "area_harvested_ha",
                      "214" = "production_t", "112" = "yield_calculated_kg_ha")
    if (anyNA(dat$year) || anyNA(dat$species)) fail("Missing year/species keys in audited input.")
    dat <- dat[dat$year %in% years & dat$species %in% species_map, , drop = FALSE]
    if (!nrow(dat) || !setequal(unique(dat$year), years)) fail("Not all Figure 1 years are present.")
    if (anyNA(dat$municipality_code) || any(!grepl("^[0-9]{7}$", dat$municipality_code))) fail("Invalid municipality codes.")
    if (anyDuplicated(dat[c("municipality_code", "year", "species")])) fail("Duplicate input keys.")
    for (column in c("area_destined_ha", "area_harvested_ha", "production_t")) {
      if (!is.numeric(dat[[column]])) fail("Expected numeric input column: ", column)
      if (any(!is.na(dat[[column]]) & (!is.finite(dat[[column]]) | dat[[column]] < 0))) fail("Invalid numeric values: ", column)
    }
    dat$state_code <- substr(dat$municipality_code, 1, 2)
    dat$category_id <- names(species_map)[match(dat$species, unname(species_map))]
    add_check("input_file", input_file)
    add_check("input_md5", unname(tools::md5sum(input_file)))
    add_check("analysis_period", paste(range(years), collapse = " to "))
    add_check("municipalities_in_input", length(unique(dat$municipality_code)))

    # ------------------------------------------------------------------
    # Verify the LIVE official category mapping, not our own earlier labels
    # ------------------------------------------------------------------
    message("Checking official IBGE metadata...")
    metadata <- fetch_json(paste0(api_base, "/metadados"),
      file.path(cache_dir, "5457_metadata.json"), function(x) {
        is.list(x) && !is.null(x$variaveis) && !is.null(x$classificacoes)
      })
    class_pos <- which(vapply(metadata$classificacoes,
      function(x) identical(scalar(x$id), "782"), logical(1)))
    if (length(class_pos) != 1L) fail("Classification 782 was not uniquely identified in official metadata.")
    category_list <- metadata$classificacoes[[class_pos]]$categorias
    if (is.null(category_list)) fail("Official metadata lacks category definitions.")
    cat_rows <- lapply(category_list, function(x) data.frame(
      category_id = scalar(x$id), official_category_label = scalar(x$nome), stringsAsFactors = FALSE))
    cats <- join_rows(cat_rows)
    cats <- cats[cats$category_id %in% names(species_map), , drop = FALSE]
    if (anyDuplicated(cats$category_id) || !setequal(cats$category_id, names(species_map))) fail("Expected coffee category IDs missing or duplicated in official metadata.")
    cats$input_species <- unname(species_map[cats$category_id])
    cats$label_verified <- mapply(same_species_label, cats$category_id, cats$official_category_label)
    write_csv(cats, file.path(out_dir, "coffee_official_category_check.csv"))
    add_check("current_official_category_labels_match_input_mapping", all(cats$label_verified))
    if (!all(cats$label_verified)) fail("Official coffee labels do not agree with our earlier mapping. See coffee_official_category_check.csv. No automatic relabeling was applied.")
    metadata_vars <- join_rows(lapply(metadata$variaveis, function(x) data.frame(
      variable_id = scalar(x$id), official_name = scalar(x$nome),
      metadata_unit = scalar(x$unidade), stringsAsFactors = FALSE)))
    metadata_vars <- metadata_vars[metadata_vars$variable_id %in% names(variable_map), , drop = FALSE]
    write_csv(metadata_vars, file.path(out_dir, "coffee_official_variable_metadata.csv"))
    if (!setequal(metadata_vars$variable_id, names(variable_map))) fail("Required variable IDs missing in official metadata.")

    # ------------------------------------------------------------------
    # Official data: national and state totals, one small request per year
    # and geographic level. Raw JSON and original symbols are preserved.
    # ------------------------------------------------------------------
    unpack <- function(object, expected_year, expected_level) {
      rows <- list()
      for (v in object) {
        vid <- scalar(v$id)
        if (!(vid %in% names(variable_map))) fail("Unexpected official variable ID: ", vid)
        if (!same_name_and_unit(vid, scalar(v$variavel), scalar(v$unidade))) {
          fail("Unexpected variable name/unit: ", vid, " | ", scalar(v$variavel), " | ", scalar(v$unidade), ". No unit conversion assumed.")
        }
        for (r in v$resultados) {
          selected <- r$classificacoes[vapply(r$classificacoes, function(z) identical(scalar(z$id), "782"), logical(1))]
          if (length(selected) != 1L) fail("Cannot uniquely identify result classification 782.")
          cat_obj <- selected[[1]]$categoria
          category <- names(cat_obj)
          if (length(category) != 1L || !(category %in% names(species_map))) fail("Unexpected coffee result category.")
          label <- scalar(cat_obj[[1]])
          if (!same_species_label(category, label)) fail("Coffee result label disagrees with category mapping: ", label)
          for (s in r$series) {
            local <- s$localidade
            level <- scalar(local$nivel$id)
            if (is.na(level)) level <- expected_level
            if (!identical(level, expected_level)) fail("Unexpected geographic level in official response.")
            period <- names(s$serie)
            if (length(period) != 1L || period != as.character(expected_year)) fail("Unexpected period in official response.")
            rows[[length(rows) + 1L]] <- data.frame(
              geo_level = level, locality_id = scalar(local$id), locality_name = scalar(local$nome),
              year = expected_year, category_id = category, species = unname(species_map[category]),
              official_category_label = label, variable_id = vid,
              official_variable_name = scalar(v$variavel), official_unit = scalar(v$unidade),
              official_value_raw = scalar(s$serie[[1]]), stringsAsFactors = FALSE)
          }
        }
      }
      result <- join_rows(rows)
      if (!nrow(result)) fail("Official response contains no observations.")
      expected_localities <- if (expected_level == "N1") "1" else c(
        "11", "12", "13", "14", "15", "16", "17", "21", "22", "23", "24", "25", "26", "27", "28", "29",
        "31", "32", "33", "35", "41", "42", "43", "50", "51", "52", "53")
      if (anyNA(result$locality_id) || any(!result$locality_id %in% expected_localities)) fail("Unexpected national/state geographic code.")
      expected <- expand.grid(locality_id = expected_localities, category_id = names(species_map),
                              variable_id = names(variable_map), stringsAsFactors = FALSE)
      key <- function(x) paste(x$locality_id, x$category_id, x$variable_id, sep = "|")
      if (anyDuplicated(key(result)) || !setequal(key(result), key(expected))) {
        fail("Incomplete official response for ", expected_year, " ", expected_level,
             ". Expected all category-variable-locality combinations, including source missing symbols.")
      }
      parsed <- numeric_source(result$official_value_raw)
      result$official_value <- parsed$value
      result$source_value_status <- parsed$parse_status
      if (any(parsed$parse_status == "UNRECOGNIZED_SYMBOL")) {
        write_csv(result, file.path(out_dir, "coffee_unrecognized_source_symbols.csv"))
        fail("Unexpected official value symbol. See coffee_unrecognized_source_symbols.csv.")
      }
      result
    }
    official_parts <- list()
    for (yr in years) for (level in c("N1", "N3")) {
      message("Official aggregates: ", yr, " | ", if (level == "N1") "Brazil" else "states")
      url <- paste0(api_base, "/periodos/", yr, "/variaveis/",
        utils::URLencode(paste(names(variable_map), collapse = "|"), reserved = TRUE),
        "?localidades=", utils::URLencode(paste0(level, "[all]"), reserved = TRUE),
        "&classificacao=", utils::URLencode("782[40139,40140,40141]", reserved = TRUE))
      object <- fetch_json(url, file.path(cache_dir, paste0("5457_", yr, "_", level, ".json")),
        function(x) is.list(x) && length(x) > 0L && all(vapply(x,
          function(z) is.list(z) && !is.null(z$id) && !is.null(z$resultados), logical(1))))
      official_parts[[length(official_parts) + 1L]] <- unpack(object, yr, level)
    }
    official <- join_rows(official_parts)
    write_csv(official, file.path(out_dir, "coffee_official_reference_values.csv"))
    unit_check <- unique(official[c("variable_id", "official_variable_name", "official_unit")])
    unit_check$variable_and_unit_verified <- mapply(same_name_and_unit,
      unit_check$variable_id, unit_check$official_variable_name, unit_check$official_unit)
    write_csv(unit_check, file.path(out_dir, "coffee_official_unit_check.csv"))
    add_check("current_official_response_units_verified", all(unit_check$variable_and_unit_verified))
    add_check("original_raw_municipal_labels_units_symbols_rechecked", FALSE)

    # ------------------------------------------------------------------
    # Reproduce aggregated values from available municipal measurements.
    # Yield is a ratio of paired sums, never a sum/mean of municipal yields.
    # ------------------------------------------------------------------
    message("Comparing official aggregates with existing municipality records...")
    aggregate_municipal <- function(level) {
      locality <- if (level == "N1") rep("1", nrow(dat)) else dat$state_code
      groups <- split(seq_len(nrow(dat)), paste(locality, dat$year, dat$category_id, sep = "|"))
      join_rows(lapply(groups, function(idx) {
        d <- dat[idx, , drop = FALSE]
        result <- lapply(names(variable_map), function(vid) {
          if (vid == "112") {
            good <- !is.na(d$production_t) & !is.na(d$area_harvested_ha) &
              !(d$production_t > 0 & d$area_harvested_ha == 0)
            denominator <- sum(d$area_harvested_ha[good])
            value <- if (any(good) && denominator > 0) 1000 * sum(d$production_t[good]) / denominator else NA_real_
            observed <- sum(good)
            method <- "1000 * sum(production) / sum(area), same paired rows"
          } else {
            x <- d[[variable_map[[vid]]]]
            observed <- sum(!is.na(x))
            value <- if (observed) sum(x, na.rm = TRUE) else NA_real_
            method <- "Sum of available municipal records"
          }
          data.frame(geo_level = level, locality_id = locality[idx[1]], year = d$year[1],
            category_id = d$category_id[1], variable_id = vid, municipal_value = value,
            municipal_records = nrow(d), observed_municipal_records = observed,
            missing_municipal_records = nrow(d) - observed, municipal_aggregation_method = method,
            stringsAsFactors = FALSE)
        })
        join_rows(result)
      }))
    }
    municipal <- rbind(aggregate_municipal("N1"), aggregate_municipal("N3"))
    by <- c("geo_level", "locality_id", "year", "category_id", "variable_id")
    if (anyDuplicated(municipal[by]) || anyDuplicated(official[by])) fail("Duplicate comparison keys.")
    comparison <- merge(official, municipal, by = by, all = TRUE, sort = TRUE)
    comparison$difference <- comparison$municipal_value - comparison$official_value
    comparison$tolerance <- ifelse(comparison$variable_id == "112", yield_rounding_allowance, additive_tolerance)
    paired <- is.finite(comparison$official_value) & is.finite(comparison$municipal_value)
    comparison$status <- "UNTESTABLE_MISSING_VALUE"
    comparison$status[paired & abs(comparison$difference) > comparison$tolerance] <- "MISMATCH"
    comparison$status[paired & abs(comparison$difference) <= comparison$tolerance] <- "MATCH_WITHIN_TOLERANCE"
    comparison$status[paired & abs(comparison$difference) <= additive_tolerance] <- "MATCH_EXACT"
    zero_area <- comparison$variable_id == "216" & !is.na(comparison$municipal_value) & comparison$municipal_value == 0
    zero_area_keys <- paste(comparison$geo_level[zero_area], comparison$locality_id[zero_area],
      comparison$year[zero_area], comparison$category_id[zero_area], sep = "|")
    zero_yield <- comparison$variable_id == "112" & is.na(comparison$municipal_value) &
      paste(comparison$geo_level, comparison$locality_id, comparison$year, comparison$category_id, sep = "|") %in% zero_area_keys
    comparison$status[zero_yield] <- "NOT_APPLICABLE_ZERO_HARVESTED_AREA"
    comparison$historical_boundaries_harmonized <- FALSE
    comparison$climate_attribution_performed <- FALSE
    write_csv(comparison, file.path(out_dir, "coffee_official_comparison_all.csv"))
    write_csv(comparison[comparison$geo_level == "N1", ], file.path(out_dir, "coffee_official_national_comparison.csv"))
    write_csv(comparison[comparison$geo_level == "N3", ], file.path(out_dir, "coffee_official_state_comparison.csv"))
    write_csv(comparison[comparison$status == "MISMATCH", ], file.path(out_dir, "coffee_official_mismatches.csv"))
    write_csv(comparison[!grepl("^MATCH", comparison$status), ], file.path(out_dir, "coffee_official_cases_for_review.csv"))
    matched <- grepl("^MATCH", comparison$status)
    additive <- comparison$variable_id != "112"
    national <- comparison$geo_level == "N1"
    # All figures retain their original flags. These are separate audit findings.
    add_check("official_comparison_cells", nrow(comparison))
    add_check("matched_cells", sum(matched))
    add_check("mismatch_cells", sum(comparison$status == "MISMATCH"))
    add_check("untestable_cells", sum(comparison$status == "UNTESTABLE_MISSING_VALUE"))
    add_check("yield_not_applicable_zero_area_cells", sum(zero_yield))
    add_check("national_additive_totals_reproduced", all(matched[national & additive]))
    add_check("state_additive_totals_reproduced", all(matched[!national & additive]))
    add_check("national_production_area_totals_reproduced", all(matched[national & comparison$variable_id %in% c("214", "216")]))
    add_check("state_production_area_totals_reproduced", all(matched[!national & comparison$variable_id %in% c("214", "216")]))
    add_check("national_observed_value_comparisons_match", all(matched[national & paired]))
    add_check("historical_boundaries_harmonized", FALSE)
    add_check("pre2002_measurement_basis_verified", FALSE)
    add_check("original_input_modified", FALSE)
    add_check("climate_attribution_performed", FALSE)

    notes <- c(
      "COFFE-STUDY | OFFICIAL SOURCE VALIDATION, STEP 07",
      paste("Executed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      "Scope: 2012 to 2024. Data from Table 5457, classification 782.",
      "Metadata and raw national/state API responses are archived with URL and MD5.",
      "This checks the CURRENT official category mapping and response units.",
      "It does not re-read original municipal API labels, units or missing symbols.",
      "All available municipal records are used, NOT the balanced Figure 1 sample.",
      "Total is checked independently from Arabica and Canephora; they are never added together.",
      "Production and harvested/destined areas are summed across available records.",
      "Aggregate productivity uses production and harvested area from the same paired rows.",
      "Municipalities with zero area have undefined productivity, not an imputed zero.",
      "The API dash is retained as a raw symbol and interpreted as zero for additive comparison.",
      "Other source missing/suppressed symbols remain NA. Unknown symbols stop execution.",
      "Differences in additive variables must be less than 1e-6 source units to match.",
      "Reported productivity is allowed one kg/ha difference due to integer rounding.",
      "Matching an available-record sum does not prove every municipality is observed or correct.",
      "Differences can reflect coverage, source revisions or other issues: no automatic correction.",
      "Historical boundaries, pre-2002 product basis and climate attribution are outside this step.",
      "Original files and all earlier figures/flags remain unchanged.",
      paste("API documentation:", "https://servicodados.ibge.gov.br/api/docs/agregados?versao=3")
    )
    writeLines(enc2utf8(notes), file.path(out_dir, "coffee_official_validation_notes.txt"), useBytes = TRUE)
    state$completed <- TRUE
    state$error <- ""
    cat("\nOFFICIAL CHECK COMPLETED\n")
    print(as.data.frame(table(comparison$geo_level, comparison$status)), row.names = FALSE)
    cat("\nSend these two files:\n",
      file.path(out_dir, "coffee_official_validation_summary.csv"), "\n",
      file.path(out_dir, "coffee_official_mismatches.csv"), "\n", sep = "")
    if (any(comparison$status == "MISMATCH")) {
      warning("Differences found. They were exported and NOT corrected.", call. = FALSE)
    }
    invisible(comparison)
  }
  tryCatch(work(), error = function(e) {
    state$error <- conditionMessage(e)
    fail("STEP 07 INCOMPLETE: ", state$error, "\nSummary saved to: ", report_path)
  })
}

main()
