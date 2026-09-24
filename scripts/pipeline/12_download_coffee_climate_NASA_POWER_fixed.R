# ======================================================================
# COFFE-STUDY | STEP 12 | NASA POWER DAILY HISTORY, SIX EXISTING POINTS
# Run this COMPLETE file using Source in RStudio.
# COMPLETE REPLACEMENT, version 2: source-label and cache-recovery correction.
# Replaces the previous NASA POWER step 12. Steps 01-11 need not be rerun.
# Recognizes CERES SYN1deg identifiers as well as the family name CERES.
# Revalidates an existing REVIEW_ response before making another HTTP request.
# Original JSON files, including REVIEW_ files, are never overwritten/deleted.
# Unknown or conflicting source labels still stop processing with a clear audit.
# The provider-reported labels are kept separately from documented epochs.
# The raw JSON that caused the user's error was not attached to this chat;
# this script will inspect and revalidate it locally. No label is guessed.
#
# Scope: the six municipal reference points selected in step 11.
# Meteorology: 1981-2024. Solar radiation: 1984-2024.
# NASA POWER is queried directly; no API key or paid service is used.
# Earlier files, weights, figures and Open-Meteo data remain unchanged.
#
# IMPORTANT
# * Meteorology is MERRA-2 at native 0.5 latitude x 0.625 longitude degrees.
# * Solar uses a different grid (primary resolution 1 x 1 degree) and sources:
#   SRB for 1984-2000 and CERES from 2001. Source epochs stay identified.
# * NASA cautions against solar-trend analyses spanning source changes.
# * Returned geometry/elevation is response metadata, NOT farm coordinates,
#   coffee-field altitude or a verified identity of the native grid cell.
# * UTC days are requested explicitly. These are not local civil days.
# * Missing data stay NA. Radiation before 1984 stays structurally missing.
# * VPD maximum, ET0 and water balance are NOT computed in this download step.
#   Mean daily temperature/dewpoint are not used to fabricate maximum VPD.
# * WS2M (not WS10M), dewpoint and pressure are downloaded for later ET0 work.
# * Six points do not constitute a representative sample of national climate.
# * A subsequent national download must avoid redundant requests within the
#   same source cells; this script intentionally does not request all targets.
#
# Official documentation checked 2026-09-20:
# https://power.larc.nasa.gov/
# https://power.larc.nasa.gov/docs/services/api/temporal/daily/
# https://power.larc.nasa.gov/docs/methodology/data/sources/
# https://power.larc.nasa.gov/docs/methodology/meteorology/
# https://power.larc.nasa.gov/docs/faqs/data/
# ======================================================================

coffee_power_step12 <- function() {
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  first_year <- 1981L
  last_year <- 2024L
  pause_seconds <- 2
  max_attempts <- 3L
  request_timeout_seconds <- 300
  script_version <- "12_NASA_POWER_v2_source_labels_and_review_recovery"
  prepare_only <- toupper(Sys.getenv("COFFEE_POWER_PREPARE_ONLY", "FALSE")) == "TRUE"
  api <- "https://power.larc.nasa.gov/api/temporal/daily/point"

  pilot_dir <- file.path(base_dir, "04_outputs", "climate_pilot")
  out_dir <- file.path(base_dir, "04_outputs", "climate_nasa_power")
  raw_dir <- file.path(base_dir, "01_data_raw", "climate", "NASA_POWER", "history6")
  daily_dir <- file.path(base_dir, "02_data_processed", "climate", "NASA_POWER", "history6")
  for (p in c(out_dir, raw_dir, daily_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create directory: ", p, call. = FALSE)
  }
  old_options <- options(scipen = 999, timeout = max(300, getOption("timeout", 60)))
  on.exit(options(old_options), add = TRUE)
  lock <- file.path(out_dir, ".power12_running")
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("Another NASA POWER run may be active. After confirming it has stopped, remove:\n",
         lock, call. = FALSE)
  }
  writeLines(paste(Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)

  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  hash <- function(path) unname(tools::md5sum(path))
  need <- function(x, cols, label) {
    missing <- setdiff(cols, names(x))
    assert(!length(missing), paste(label, "is missing:", paste(missing, collapse = ", ")))
  }
  read_csv <- function(path) {
    assert(file.exists(path), paste("Required file not found:", path))
    utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE,
      check.names = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  write_csv <- function(x, path) {
    tmp <- tempfile("power12_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  write_rds <- function(x, path) {
    tmp <- tempfile("power12_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  num <- function(x, label, allow_na = FALSE) {
    old <- as.character(x)
    z <- suppressWarnings(as.numeric(old))
    assert(!any(!is.na(old) & is.na(z)) && all(is.na(z) | is.finite(z)) &&
      (allow_na || !anyNA(z)), paste("Invalid numeric values:", label))
    z
  }
  checked <- function(x, key) {
    need(x, c("check", "result"), "Summary")
    z <- x$result[x$check == key]
    assert(length(z) == 1L && !is.na(z), paste("Missing/ambiguous check:", key))
    z
  }
  bind <- function(x) {
    if (!length(x)) return(data.frame())
    z <- do.call(rbind, x)
    rownames(z) <- NULL
    z
  }
  scalar <- function(x, fallback = "not supplied") {
    z <- unlist(x, use.names = FALSE)
    if (!length(z)) return(fallback)
    paste(as.character(z), collapse = " | ")
  }
  stop_kind <- function(msg, kind) {
    stop(structure(list(message = msg, call = NULL),
                   class = c(kind, "error", "condition")))
  }
  requests <- quality_parts <- annual_parts <- coverage_parts <- source_parts <- list()
  provenance <- plan <- previous_plan <- data.frame()
  # Previous request plans are used only to recover the exact URL-to-file
  # association of legacy REVIEW_ files whose old code did not save a sidecar.
  report <- list(execution_status = "INCOMPLETE", script_version = script_version, source = "NASA POWER",
    scope = "SIX_EXISTING_PILOT_POINTS", meteorological_period = "1981 to 2024",
    radiation_period = "1984 to 2024", time_standard = "UTC", selected_points = 0,
    planned_requests = 0, validated_requests = 0, cache_requests = 0,
    recovered_review_requests = 0, source_metadata_review_rows = 0,
    previous_reports_backup = "None",
    http_attempts_this_run = 0, processed_histories = 0, site_days = 0,
    quality_flag_rows = 0, combined_with_openmeteo = FALSE,
    nationwide_weather_downloaded = FALSE, et0_calculated = FALSE,
    vpd_maximum_calculated = FALSE, soil_water_balance_calculated = FALSE,
    climate_trends_calculated = FALSE, climate_attribution_performed = FALSE,
    farm_coordinates_measured = FALSE, original_inputs_modified = FALSE,
    last_stage = "Loading inputs", error_if_incomplete = "None")
  save_progress <- function() {
    write_csv(data.frame(check = names(report), result = vapply(report, as.character, character(1))),
      file.path(out_dir, "coffee_power_history_summary.csv"))
    for (entry in list(c("requests", "coffee_power_requests.csv"),
                       c("quality_parts", "coffee_power_quality.csv"),
                       c("annual_parts", "coffee_power_annual.csv"),
                       c("coverage_parts", "coffee_power_history_coverage.csv"),
                       c("source_parts", "coffee_power_source_metadata.csv"))) {
      x <- get(entry[1L], inherits = TRUE)
      if (length(x)) write_csv(bind(x), file.path(out_dir, entry[2L]))
    }
  }
  on.exit({
    try(save_progress(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()),
      file.path(out_dir, "sessionInfo_step12_NASA_POWER.txt")), silent = TRUE)
  }, add = TRUE)

  # The unit in the actual response is verified before any conversion.
  dictionary <- data.frame(
    parameter = c("T2M", "T2M_MIN", "T2M_MAX", "T2MDEW", "RH2M", "PRECTOTCORR", "WS2M", "PS", "ALLSKY_SFC_SW_DWN"),
    output_column = c("tmean_c", "tmin_c", "tmax_c", "dewpoint_mean_c", "rhmean_pct", "precipitation_mm", "wind2m_mean_m_s", "surface_pressure_kpa", "shortwave_mj_m2"),
    output_unit = c(rep("degC", 4), "%", "mm/day", "m/s", "kPa", "MJ/m2/day"),
    first_available_year = c(rep(1981L, 8), 1984L),
    lower_screen = c(-60, -60, -60, -90, 0, 0, 0, 40, 0),
    upper_screen = c(60, 60, 60, 60, 100, 1500, 100, 110, 60),
    stringsAsFactors = FALSE)
  dictionary$source_family <- c(rep("MERRA-2 meteorology", 8), "SRB before 2001; CERES from 2001")
  dictionary$native_resolution <- c(rep("0.5 latitude x 0.625 longitude degrees", 8), "Primary solar grid 1 x 1 degree")
  dictionary$screening_note <- "Broad physical screening, not crop stress thresholds"
  met_params <- dictionary$parameter[1:8]

  conversion <- function(parameter, unit) {
    u <- tolower(unit)
    u <- gsub("\u00b2", "2", u, fixed = TRUE)
    u <- gsub("\u00b0", "", u, fixed = TRUE)
    u <- gsub("[[:space:]_*^()]", "", u)
    if (parameter %in% c("T2M", "T2M_MIN", "T2M_MAX", "T2MDEW") &&
        u %in% c("c", "degc", "degrees celsius", "degreescelsius", "celsius")) return(1)
    if (parameter == "RH2M" && u %in% c("%", "percent")) return(1)
    if (parameter == "PRECTOTCORR" && u %in% c("mm/day", "mmd-1", "mmday-1", "mm")) return(1)
    if (parameter == "WS2M" && u %in% c("m/s", "ms-1", "m/sec")) return(1)
    if (parameter == "PS" && u == "kpa") return(1)
    if (parameter == "PS" && u %in% c("hpa", "mbar")) return(0.1)
    if (parameter == "PS" && u == "pa") return(0.001)
    if (parameter == "ALLSKY_SFC_SW_DWN") {
      if (u %in% c("mj/m2/day", "mj/m2/d", "mjm-2day-1", "mj/m2")) return(1)
      if (u %in% c("kw-hr/m2/day", "kwhr/m2/day", "kwh/m2/day", "kwh/m2")) return(3.6)
      if (u %in% c("w/m2", "wm-2")) return(0.0864)
    }
    stop("Unrecognized unit for ", parameter, ": ", unit, ". No conversion guessed.", call. = FALSE)
  }

  # Source identifiers and provider family names are not the same field.
  # NASA documents SYN1deg as a CERES product. The raw labels remain intact.
  source_classification <- function(group, sources) {
    raw <- as.character(unlist(sources, use.names = FALSE))
    raw <- raw[!is.na(raw) & nzchar(trimws(raw))]
    token <- unique(toupper(gsub("[^A-Za-z0-9]", "", raw)))
    derived <- token == "POWER"
    is_merra <- grepl("^MERRA2($|V|VERSION|[0-9])", token)
    is_srb <- grepl("^(GEWEX)?SRB($|[0-9]|R[0-9]|V[0-9]|RELEASE[0-9])", token)
    is_syn <- grepl("^(CERES)?SYN1DEG($|ED|V|[0-9])", token)
    is_ceres_family <- token == "CERES"
    blocked <- grepl("FLASH|GEOS|IMERG", token)
    ok <- FALSE
    family <- product <- "UNRESOLVED"
    status <- "REVIEW_UNKNOWN_OR_CONFLICTING_SOURCE"
    if (identical(as.character(group), "meteorology")) {
      ok <- length(token) > 0L && any(is_merra) &&
        all(is_merra | derived) && !any(blocked)
      if (ok) { family <- "MERRA-2"; product <- "MERRA-2" }
    } else if (identical(as.character(group), "solar_1984_2000")) {
      ok <- length(token) > 0L && any(is_srb) &&
        all(is_srb | derived) && !any(blocked)
      if (ok) { family <- "SRB"; product <- "SRB" }
    } else if (identical(as.character(group), "solar_2001_2024")) {
      ok <- length(token) > 0L && any(is_syn | is_ceres_family) &&
        all(is_syn | is_ceres_family | derived) && !any(blocked)
      if (ok) {
        family <- "CERES"
        product <- if (any(is_syn)) "CERES SYN1deg" else "CERES (product unspecified in header)"
      }
    }
    if (ok) status <- if (family == "CERES" && !any(is_syn))
      "RECOGNIZED_FAMILY_PRODUCT_UNSPECIFIED" else "RECOGNIZED_SOURCE_IDENTIFIER"
    note <- if (ok && family == "CERES" && any(is_syn)) {
      "SYN1deg is the documented CERES product; identifier recognized without changing the raw label."
    } else if (ok && family == "CERES") {
      "CERES family explicitly reported; product version is not inferred from the calendar year."
    } else if (ok) {
      "Expected source explicitly recognized from provider metadata."
    } else {
      "A source label is absent, unknown, or inconsistent with the requested historical block."
    }
    list(ok = ok, family = family, product = product, status = status, note = note)
  }

  # Small deterministic checks executed locally before reading/downloading data.
  # These are software checks with constructed labels, not validation of NASA data.
  source_rule_tests <- function() {
    cases <- list(
      list("meteorology", c("MERRA2", "POWER"), TRUE),
      list("meteorology", "MERRA-2", TRUE),
      list("solar_1984_2000", "SRB", TRUE),
      list("solar_2001_2024", "SYN1DEG", TRUE),
      list("solar_2001_2024", "CERES SYN1deg Edition 4.2", TRUE),
      list("solar_2001_2024", "CERES", TRUE),
      list("solar_2001_2024", c("SYN1DEG", "POWER"), TRUE),
      list("solar_2001_2024", "FLASHFLUX", FALSE),
      list("solar_2001_2024", c("SYN1DEG", "SRB"), FALSE),
      list("solar_2001_2024", c("CERES", "FLASHFlux"), FALSE),
      list("solar_2001_2024", "unknown_provider", FALSE),
      list("solar_2001_2024", NULL, FALSE))
    for (test in cases) {
      result <- source_classification(test[[1L]], test[[2L]])
      assert(identical(result$ok, test[[3L]]), "Internal source-label test failed.")
    }
    assert(conversion("ALLSKY_SFC_SW_DWN", "MJ/m^2/day") == 1,
      "Internal MJ radiation unit test failed.")
    assert(conversion("ALLSKY_SFC_SW_DWN", "kW-hr/m^2/day") == 3.6,
      "Internal kWh radiation unit test failed.")
    invisible(TRUE)
  }

  # Validate the complete payload, including dates and response metadata.
  parse_power <- function(path, job) {
    j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    assert(identical(j$type, "Feature") && is.list(j$properties$parameter),
      paste("Not a NASA POWER point response:", basename(path)))
    header <- j$header
    assert(toupper(scalar(header$time_standard)) == "UTC", "Response time standard is not confirmed UTC.")
    fill <- num(unlist(header$fill_value), "header fill value")
    assert(length(fill) == 1L, "Missing/ambiguous fill value.")
    source_text <- scalar(header$sources)
    source_info <- source_classification(job$group, header$sources)
    # This audit is written even when a source is unknown. It concerns source
    # metadata only; accepted labels do not imply that daily values passed QA.
    source_parts[[length(source_parts) + 1L]] <<- data.frame(
      municipality_code = job$municipality_code, group = job$group,
      start = job$start, end = job$end, actual_file_read = path,
      raw_md5 = hash(path), sources_reported = source_text,
      recognized_source_family = source_info$family,
      recognized_source_product = source_info$product,
      source_label_status = source_info$status,
      source_labels_verified = source_info$ok,
      source_verification_note = source_info$note,
      api_version = scalar(header$api$version),
      documentation = "https://power.larc.nasa.gov/docs/methodology/data/sources/",
      stringsAsFactors = FALSE)
    report$source_metadata_review_rows <<- sum(!bind(source_parts)$source_labels_verified)
    if (!source_info$ok) {
      stop("Source metadata require review for ", job$group,
        ". Actual header$sources: ", source_text, ". ", source_info$note,
        " No source has been relabelled from dates alone. See coffee_power_source_metadata.csv.",
        call. = FALSE)
    }
    # Where supplied, header dates must agree with the requested interval.
    # Independently, every parameter must contain the complete exact date set.
    for (edge in c("start", "end")) {
      if (!is.null(header[[edge]])) {
        hdate <- gsub("-", "", scalar(header[[edge]]), fixed = TRUE)
        qdate <- gsub("-", "", as.character(job[[edge]]), fixed = TRUE)
        assert(identical(hdate, qdate), paste("Header", edge, "differs from requested interval."))
      }
    }
    expected <- seq(as.Date(job$start), as.Date(job$end), by = "day")
    keys <- format(expected, "%Y%m%d")
    pars <- strsplit(job$parameters, ",", fixed = TRUE)[[1L]]
    assert(all(pars %in% names(j$properties$parameter)), "One or more requested parameters were omitted.")
    d <- data.frame(date = expected)
    units <- list()
    for (p in pars) {
      series <- j$properties$parameter[[p]]
      assert(!is.null(names(series)) && !anyDuplicated(names(series)) && setequal(names(series), keys),
        paste("Missing, extra or duplicated dates in", p, basename(path)))
      values <- vapply(series[match(keys, names(series))], function(v) {
        if (is.null(v)) return(NA_real_)
        assert(length(v) == 1L && is.numeric(v), paste("Non-numeric API value in", p))
        as.numeric(v)
      }, numeric(1))
      values[!is.na(values) & values == fill] <- NA_real_
      assert(all(is.na(values) | is.finite(values)), paste("Nonfinite API value in", p))
      unit <- scalar(j$parameters[[p]]$units)
      factor <- conversion(p, unit)
      col <- dictionary$output_column[match(p, dictionary$parameter)]
      d[[col]] <- values * factor
      units[[p]] <- data.frame(parameter = p, response_unit = unit,
        output_unit = dictionary$output_unit[match(p, dictionary$parameter)],
        conversion_factor = factor, missing_values = sum(is.na(values)))
    }
    geom <- num(unlist(j$geometry$coordinates), "response geometry", allow_na = TRUE)
    assert(length(geom) >= 2L && all(is.finite(geom[1:2])), "Missing response coordinates.")
    assert(identical(j$geometry$type, "Point"), "Response geometry is not a point.")
    # POWER returns point coordinates rounded in the metadata in the observed
    # API format. This check is about response identity, not native grid cells.
    assert(max(abs(geom[1:2] - c(job$requested_longitude, job$requested_latitude))) <= 0.01,
      "Response coordinates do not match the requested municipal point within 0.01 degree. Review before reuse.")
    list(data = d, units = bind(units), header = header, geometry = geom,
      messages = j$messages, api_version = scalar(header$api$version), sources = source_text,
      source_info = source_info, fill_value = fill)
  }

  # The old code saved failed responses as REVIEW_<canonical basename>, but
  # did not attach their URL. The previous request plan supplies that link.
  # Recovery still requires dates, parameters, units, UTC, coordinates and
  # explicitly recognized sources to pass validation. No failed check is waived.
  cache_identity <- function(path, job) {
    sidecar <- paste0(path, ".url.txt")
    if (file.exists(sidecar)) {
      old_url <- paste(readLines(sidecar, warn = FALSE), collapse = "")
      assert(identical(old_url, as.character(job$url)),
        paste("Cached URL differs from requested URL:", path))
      return("EXACT_URL_SIDECAR")
    }
    if (nrow(previous_plan) > 0L) {
      k <- which(previous_plan$url == job$url)
      if (length(k) == 1L) {
        prior_name <- basename(gsub("\\", "/", previous_plan$raw_file[k], fixed = TRUE))
        this_name <- sub("^REVIEW_", "", basename(path))
        if (identical(prior_name, this_name)) return("PRIOR_REQUEST_PLAN_AND_EXACT_FILENAME")
      }
    }
    stop("Cannot verify the URL associated with cached file:\n", path,
      "\nThe file was retained. Send its metadata for review rather than renaming it.", call. = FALSE)
  }

  # Do not overwrite a previously saved REVIEW_ file with a new payload.
  retain_review <- function(tmp, dest, url, reason) {
    path <- file.path(raw_dir, paste0("REVIEW_", basename(dest)))
    if (file.exists(path)) {
      path <- tempfile(paste0("REVIEW_EXTRA_", sub("\\.json$", "", basename(dest)), "_"),
        tmpdir = raw_dir, fileext = ".json")
    }
    assert(file.copy(tmp, path, overwrite = FALSE), paste("Cannot retain response:", path))
    writeLines(as.character(url), paste0(path, ".url.txt"), useBytes = TRUE)
    writeLines(reason, paste0(path, ".reason.txt"), useBytes = TRUE)
    path
  }

  last_http_end <- as.POSIXct("1970-01-01", tz = "UTC")
  fetch_power <- function(job) {
    dest <- as.character(job$raw_file)
    review <- file.path(raw_dir, paste0("REVIEW_", basename(dest)))
    if (file.exists(dest)) {
      evidence <- cache_identity(dest, job)
      ans <- parse_power(dest, job)
      if (!file.exists(paste0(dest, ".url.txt")))
        writeLines(as.character(job$url), paste0(dest, ".url.txt"), useBytes = TRUE)
      message("    Reusing validated NASA cache.")
      return(list(parsed = ans, cached = TRUE, recovered_review = FALSE,
        reuse_status = "VALIDATED_CACHE", cache_identity_evidence = evidence))
    }
    if (file.exists(review)) {
      evidence <- cache_identity(review, job)
      ans <- tryCatch(parse_power(review, job), error = function(e) {
        stop("Existing REVIEW_ file still needs review:\n", review, "\n",
          conditionMessage(e), "\nNo replacement was downloaded and the original file was retained.",
          call. = FALSE)
      })
      assert(file.copy(review, dest, overwrite = FALSE), paste("Cannot recover response:", dest))
      assert(identical(hash(review), hash(dest)), "Recovered response copy differs from original REVIEW_ file.")
      writeLines(as.character(job$url), paste0(dest, ".url.txt"), useBytes = TRUE)
      message("    Recovered existing REVIEW_ file after validation; no new download.")
      return(list(parsed = ans, cached = TRUE, recovered_review = TRUE,
        reuse_status = "REVALIDATED_REVIEW_FILE", cache_identity_evidence = evidence))
    }
    for (attempt in seq_len(max_attempts)) {
      elapsed <- as.numeric(difftime(Sys.time(), last_http_end, units = "secs"))
      if (elapsed < pause_seconds) Sys.sleep(pause_seconds - elapsed)
      report$http_attempts_this_run <<- report$http_attempts_this_run + 1L
      r <- tryCatch(httr::GET(job$url, httr::timeout(request_timeout_seconds),
        httr::user_agent("coffee-climate-research/1.1 (NASA POWER daily data)"),
        httr::add_headers(Accept = "application/json")), error = function(e) e)
      last_http_end <<- Sys.time()
      if (inherits(r, "error")) {
        if (attempt == max_attempts) stop("NASA request failed: ", conditionMessage(r), call. = FALSE)
        Sys.sleep(5 * 2^(attempt - 1L))
        next
      }
      status <- httr::status_code(r)
      body <- httr::content(r, as = "text", encoding = "UTF-8")
      if (status == 429L) {
        retry <- httr::headers(r)[["retry-after"]]
        stop_kind(paste("NASA returned HTTP 429. Successful downloads are retained.",
          "Retry after:", if (is.null(retry)) "provider rate limit has cleared" else retry), "power_rate_limit")
      }
      if (status %in% c(401L, 403L))
        stop_kind(paste("NASA returned HTTP", status, ". Run stopped without bypassing access controls."), "power_access_block")
      if (status >= 500L && attempt < max_attempts) {
        Sys.sleep(5 * 2^(attempt - 1L))
        next
      }
      if (status != 200L) stop("NASA HTTP ", status, ": ",
        substr(gsub("[\r\n]+", " ", body), 1, 600), call. = FALSE)
      assert(grepl("^[[:space:]]*\\{", body), "Server returned non-JSON content; nothing saved as weather.")
      tmp <- tempfile("power_payload_", tmpdir = raw_dir, fileext = ".json")
      on.exit(unlink(tmp), add = TRUE)
      writeLines(body, tmp, useBytes = TRUE)
      ans <- tryCatch(parse_power(tmp, job), error = function(e) {
        rejected <- retain_review(tmp, dest, job$url, conditionMessage(e))
        # Update the diagnostic path to the retained file rather than the temporary file.
        if (length(source_parts)) {
          k <- length(source_parts)
          if (identical(as.character(source_parts[[k]]$actual_file_read), tmp))
            source_parts[[k]]$actual_file_read <<- rejected
        }
        stop("Response needs review. Saved to ", rejected, "\n", conditionMessage(e), call. = FALSE)
      })
      assert(file.copy(tmp, dest, overwrite = FALSE), paste("Cannot save raw response:", dest))
      if (length(source_parts)) source_parts[[length(source_parts)]]$actual_file_read <<- dest
      unlink(tmp)
      writeLines(as.character(job$url), paste0(dest, ".url.txt"), useBytes = TRUE)
      return(list(parsed = ans, cached = FALSE, recovered_review = FALSE,
        reuse_status = "NEW_DOWNLOAD", cache_identity_evidence = "DIRECT_EXACT_URL_REQUEST"))
    }
    stop("Download attempts exhausted.", call. = FALSE)
  }

  worker <- function() {
    source_rule_tests()
    # Read the previous manifest BEFORE creating a new plan. Keep its exact
    # filenames so cached Windows line-ending hashes are not changed.
    previous_plan_path <- file.path(out_dir, "coffee_power_request_plan.csv")
    if (file.exists(previous_plan_path)) {
      previous_plan <<- read_csv(previous_plan_path)
      need(previous_plan, c("url", "raw_file"), "Previous NASA request plan")
      assert(!anyNA(previous_plan$url) && !anyDuplicated(previous_plan$url),
        "Previous NASA plan contains missing or duplicate URLs.")
    }
    prior_reports <- list.files(out_dir, pattern = "^coffee_power_.*\\.(csv|txt)$", full.names = TRUE)
    if (length(prior_reports)) {
      backups <- file.path(out_dir, "previous_runs")
      dir.create(backups, recursive = TRUE, showWarnings = FALSE)
      backup <- tempfile(paste0("before_v2_", format(Sys.time(), "%Y%m%dT%H%M%S"), "_"), tmpdir = backups)
      assert(dir.create(backup), "Cannot create previous-report backup folder.")
      assert(all(file.copy(prior_reports, backup, overwrite = FALSE)), "Cannot archive previous reports.")
      report$previous_reports_backup <<- backup
    }
    paths <- c(pilot_sites = file.path(pilot_dir, "coffee_climate_pilot_sites.csv"),
      pilot_summary = file.path(pilot_dir, "coffee_climate_pilot_summary.csv"),
      targets = file.path(base_dir, "02_data_processed", "climate", "coffee_climate_target_registry.csv"))
    assert(all(file.exists(paths)), paste("Missing step 11 file(s):", paste(paths[!file.exists(paths)], collapse = "\n")))
    provenance <<- data.frame(role = names(paths), file = unname(paths),
      md5 = vapply(paths, hash, character(1)), stringsAsFactors = FALSE)
    write_csv(provenance, file.path(out_dir, "coffee_power_input_provenance.csv"))
    pilot <- read_csv(paths[["pilot_sites"]])
    summary <- read_csv(paths[["pilot_summary"]])
    targets <- read_csv(paths[["targets"]])
    assert(checked(summary, "baseline_area_weights_verified") == "TRUE" &&
      checked(summary, "dynamic_area_weights_verified") == "TRUE", "Step 11 climate weights have not passed validation.")
    need(pilot, c("municipality_code", "municipality", "state", "species", "longitude", "latitude"), "Pilot sites")
    need(targets, c("municipality_code", "longitude", "latitude"), "Target registry")
    assert(!anyDuplicated(pilot$municipality_code) && !anyDuplicated(targets$municipality_code), "Duplicate sampling codes.")
    assert(nrow(pilot) == 6L && all(grepl("^[0-9]{7}$", pilot$municipality_code)), "Expected the six original pilot points.")
    for (v in c("latitude", "longitude")) {
      pilot[[v]] <- num(pilot[[v]], paste("pilot", v))
      targets[[v]] <- num(targets[[v]], paste("target", v))
    }
    k <- match(pilot$municipality_code, targets$municipality_code)
    assert(!anyNA(k) && max(abs(pilot$longitude - targets$longitude[k])) < 1e-7 &&
      max(abs(pilot$latitude - targets$latitude[k])) < 1e-7, "Pilot points do not match the existing target registry.")
    assert(all(pilot$longitude >= -75 & pilot$longitude <= -30 & pilot$latitude >= -35 & pilot$latitude <= 6), "Invalid Brazil coordinates.")
    report$selected_points <<- nrow(pilot)
    write_csv(pilot, file.path(out_dir, "coffee_power_sampling_points.csv"))
    write_csv(dictionary, file.path(out_dir, "coffee_power_variable_dictionary.csv"))

    plan_parts <- list()
    for (i in seq_len(nrow(pilot))) {
      site <- pilot[i, ]
      groups <- c("meteorology", "solar_1984_2000", "solar_2001_2024")
      starts <- c("1981-01-01", "1984-01-01", "2001-01-01")
      ends <- c("2024-12-31", "2000-12-31", "2024-12-31")
      for (b in seq_along(groups)) {
        pars <- if (b == 1L) paste(met_params, collapse = ",") else "ALLSKY_SFC_SW_DWN"
        params <- c(parameters = pars, community = "AG", longitude = sprintf("%.7f", site$longitude),
          latitude = sprintf("%.7f", site$latitude), start = gsub("-", "", starts[b]),
          end = gsub("-", "", ends[b]), format = "JSON", `time-standard` = "UTC")
        url <- paste0(api, "?", paste(names(params),
          vapply(params, utils::URLencode, character(1), reserved = TRUE), sep = "=", collapse = "&"))
        tf <- tempfile()
        writeLines(url, tf, useBytes = TRUE)
        url_hash <- hash(tf)
        unlink(tf)
        file <- file.path(raw_dir, paste0("M_", site$municipality_code, "_", groups[b], "_", url_hash, ".json"))
        if (nrow(previous_plan) > 0L) {
          previous <- which(previous_plan$url == url)
          if (length(previous) == 1L) {
            old_name <- basename(gsub("\\", "/", previous_plan$raw_file[previous], fixed = TRUE))
            expected_prefix <- paste0("M_", site$municipality_code, "_", groups[b], "_")
            assert(startsWith(old_name, expected_prefix) && grepl("[[:xdigit:]]{32}\\.json$", old_name),
              "Previous NASA cache filename is incompatible with the point/group.")
            file <- file.path(raw_dir, old_name)
          }
        }
        plan_parts[[length(plan_parts) + 1L]] <- data.frame(municipality_code = site$municipality_code,
          municipality = site$municipality, group = groups[b], start = starts[b], end = ends[b],
          parameters = pars, requested_longitude = site$longitude, requested_latitude = site$latitude,
          url = url, raw_file = file, stringsAsFactors = FALSE)
      }
    }
    plan <<- bind(plan_parts)
    assert(!anyDuplicated(plan$url), "Duplicate public requests.")
    report$planned_requests <<- nrow(plan)
    write_csv(plan, file.path(out_dir, "coffee_power_request_plan.csv"))
    notes <- c("COFFE-STUDY | NASA POWER STEP 12", paste("Executed:", format(Sys.time(), usetz = TRUE)),
      "Six existing municipal reference points. No nationwide acquisition or national climate mean.",
      "The step 11 municipal target list and fixed/dynamic weights are left unchanged.",
      "ERA5/Open-Meteo raw files are neither reused as NASA observations nor overwritten.",
      "Documented epochs: MERRA-2 meteorology 1981-2024; SRB radiation 1984-2000; CERES radiation 2001-2024.",
      "Version 2 accepts documented CERES SYN1deg identifiers in header$sources, not only the literal CERES name.",
      "Raw reported source labels and recognized source products are archived without inferring them solely from dates.",
      "Unknown or conflicting source labels stop the run and are written to coffee_power_source_metadata.csv.",
      "Legacy REVIEW_ files are revalidated locally; their exact query is linked by prior plan/filename or URL sidecar.",
      "Recovery requires all payload checks to pass. REVIEW_ originals are retained unchanged.",
      "Earlier reports from this step are copied into previous_runs before replacement. Earlier steps are unchanged.",
      "Documented native meteorological grid: 0.5 latitude x 0.625 longitude degrees; primary solar grid: 1 x 1 degree.",
      "NASA cautions against interpreting radiation trends that cross solar source changes.",
      "Daily UTC is explicit. Civil/solar local-day summaries may differ.",
      "API geometry coordinates are preserved as response coordinates, not asserted to be native grid-cell centers.",
      "Source-grid average elevation is not coffee-field altitude. Municipal points are not coffee masks.",
      "Requested parameters: T2M,T2M_MIN,T2M_MAX,T2MDEW,RH2M,PRECTOTCORR,WS2M,PS,ALLSKY_SFC_SW_DWN.",
      "Actual parameter units are read from response metadata before any documented conversion.",
      "Daily precipitation in mm/day is the depth for one daily interval; no monthly conversion applied.",
      "Solar is normalized to MJ/m2/day. Multipliers: kWh/m2/day *3.6; daily-mean W/m2 *0.0864.",
      "Missing fill values remain NA. Radiation 1981-1983 is structurally unavailable, never set to zero.",
      "Annual metrics require every expected day observed and no broad-range flags for that variable/year.",
      "Tmin <= Tmean <= Tmax is checked. Failed checks flag the corresponding annual temperature summaries.",
      "Broad range checks are not validation against stations, not coffee thresholds and not bias correction.",
      "No daily-maximum VPD, ET0, crop ET or root-zone water balance is estimated here.",
      "No interpolation, source splicing with ERA5, artificial weather replication or altitude adjustment.",
      "There is one sequential HTTP request at a time. HTTP 429,401,403 stop the run.",
      "Validated raw JSON with identical public URL is reused on restart, including missing-data flags.",
      "Later large-scale access must be designed around source cells to avoid redundant point downloads.",
      "https://power.larc.nasa.gov/docs/services/api/temporal/daily/",
      "https://power.larc.nasa.gov/docs/methodology/data/sources/",
      "https://power.larc.nasa.gov/docs/faqs/data/")
    writeLines(notes, file.path(out_dir, "coffee_power_method_notes.txt"))
    if (prepare_only) {
      report$execution_status <<- "PREPARED_NO_DOWNLOAD"
      report$last_stage <<- "Request plan prepared; no HTTP calls made"
      return(invisible(NULL))
    }

    for (pkg in c("httr", "jsonlite")) {
      if (!requireNamespace(pkg, quietly = TRUE)) utils::install.packages(pkg, repos = "https://cloud.r-project.org")
      assert(requireNamespace(pkg, quietly = TRUE), paste("Package unavailable:", pkg))
    }
    dates <- seq(as.Date(paste0(first_year, "-01-01")), as.Date(paste0(last_year, "-12-31")), by = "day")
    for (i in seq_len(nrow(pilot))) {
      site <- pilot[i, ]
      code <- site$municipality_code
      message(sprintf("\n[%d/%d] %s | NASA POWER, 1981-2024", i, nrow(pilot), site$municipality))
      d <- data.frame(municipality_code = code, date = dates, stringsAsFactors = FALSE)
      for (v in dictionary$output_column) d[[v]] <- NA_real_
      d$meteorology_sources_reported <- NA_character_
      d$solar_sources_reported <- NA_character_
      d$meteorology_product_recognized <- NA_character_
      d$solar_product_recognized <- NA_character_
      rows <- which(plan$municipality_code == code)
      met_missing <- solar_missing <- 0L
      for (r in rows) {
        job <- plan[r, , drop = FALSE]
        report$last_stage <<- paste(code, job$group)
        message("  ", job$group, ": ", job$start, " to ", job$end)
        response <- fetch_power(job)
        a <- response$parsed
        j <- match(a$data$date, d$date)
        assert(!anyNA(j), "Response dates outside target history.")
        for (v in setdiff(names(a$data), "date")) d[[v]][j] <- a$data[[v]]
        if (job$group == "meteorology") {
          d$meteorology_sources_reported[j] <- a$sources
          d$meteorology_product_recognized[j] <- a$source_info$product
        } else {
          d$solar_sources_reported[j] <- a$sources
          d$solar_product_recognized[j] <- a$source_info$product
        }
        meta <- list(public_url = job$url, raw_md5 = hash(job$raw_file), header = a$header,
          response_geometry = a$geometry, parameter_units = a$units, messages = a$messages,
          sources_reported = a$sources, source_recognition = a$source_info,
          reuse_status = response$reuse_status, cache_identity_evidence = response$cache_identity_evidence,
          script_version = script_version,
          requested_municipal_longitude = site$longitude, requested_municipal_latitude = site$latitude)
        write_rds(meta, paste0(job$raw_file, ".metadata.rds"))
        status <- if (sum(a$units$missing_values) == 0L) "VALID_RESPONSE" else "VALID_RESPONSE_WITH_MISSING_VALUES"
        requests[[length(requests) + 1L]] <<- data.frame(municipality_code = code,
          group = job$group, start = job$start, end = job$end, public_url = job$url,
          raw_file = job$raw_file, raw_md5 = hash(job$raw_file), cached = response$cached,
          recovered_review = response$recovered_review, reuse_status = response$reuse_status,
          cache_identity_evidence = response$cache_identity_evidence,
          source_family_recognized = a$source_info$family, source_product_recognized = a$source_info$product,
          source_label_status = a$source_info$status,
          received_days = nrow(a$data), status = status, api_version = a$api_version, sources = a$sources,
          requested_longitude = site$longitude, requested_latitude = site$latitude,
          response_longitude = a$geometry[1L], response_latitude = a$geometry[2L],
          response_elevation_m = if (length(a$geometry) >= 3L) a$geometry[3L] else NA_real_,
          units_and_factors = paste(a$units$parameter, a$units$response_unit,
            a$units$conversion_factor, sep = ":", collapse = " | "),
          fill_values_as_na = sum(a$units$missing_values), stringsAsFactors = FALSE)
        report$validated_requests <<- length(requests)
        report$cache_requests <<- report$cache_requests + as.integer(response$cached)
        report$recovered_review_requests <<- report$recovered_review_requests + as.integer(response$recovered_review)
        save_progress()
      }
      d$year <- as.integer(format(d$date, "%Y"))
      d$source <- "NASA POWER"
      # Epoch labels describe the documented coverage. The *_sources_reported
      # and *_product_recognized columns above retain actual response evidence.
      d$meteorological_source_epoch <- "MERRA-2"
      d$solar_source_epoch <- ifelse(d$year < 1984, "NOT_AVAILABLE_BEFORE_1984",
        ifelse(d$year <= 2000, "SRB_1984_2000", "CERES_2001_2024"))
      d$time_standard <- "UTC"
      d$requested_longitude <- site$longitude
      d$requested_latitude <- site$latitude
      d$temperature_order_flag <- with(d,
        (!is.na(tmin_c) & !is.na(tmean_c) & tmin_c > tmean_c + 0.05) |
        (!is.na(tmean_c) & !is.na(tmax_c) & tmean_c > tmax_c + 0.05) |
        (!is.na(tmin_c) & !is.na(tmax_c) & tmin_c > tmax_c + 0.05))
      for (yr in first_year:last_year) {
        z <- d[d$year == yr, , drop = FALSE]
        annual <- data.frame(municipality_code = code, municipality = site$municipality,
          state = site$state, pilot_species_selection = site$species, year = yr,
          days_expected = nrow(z), time_standard = "UTC", source = "NASA POWER",
          solar_source_epoch = unique(z$solar_source_epoch),
          meteorology_sources_reported = scalar(unique(z$meteorology_sources_reported)),
          solar_sources_reported = scalar(unique(stats::na.omit(z$solar_sources_reported)),
            fallback = "STRUCTURALLY_UNAVAILABLE_BEFORE_1984"), stringsAsFactors = FALSE)
        for (v in seq_len(nrow(dictionary))) {
          def <- dictionary[v, ]
          x <- z[[def$output_column]]
          available <- yr >= def$first_available_year
          bad <- !is.na(x) & (x < def$lower_screen | x > def$upper_screen)
          n_order <- if (def$parameter %in% c("T2M", "T2M_MIN", "T2M_MAX")) sum(z$temperature_order_flag) else 0L
          complete <- available && !anyNA(x) && !any(bad) && n_order == 0L
          quality_parts[[length(quality_parts) + 1L]] <<- data.frame(municipality_code = code,
            year = yr, parameter = def$parameter, days_expected = if (available) nrow(z) else 0L,
            valid_values = sum(!is.na(x)), missing_values = if (available) sum(is.na(x)) else 0L,
            structurally_unavailable_days = if (!available) nrow(z) else 0L,
            outside_broad_bounds_days = sum(bad), temperature_order_failures = n_order,
            minimum = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
            maximum = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE),
            status = if (!available) "NOT_AVAILABLE_BEFORE_1984" else if (complete) "PASS" else "REVIEW", stringsAsFactors = FALSE)
          annual[[paste0(def$output_column, "_annual_mean")]] <- if (complete) mean(x) else NA_real_
          if (def$parameter %in% c("PRECTOTCORR", "ALLSKY_SFC_SW_DWN"))
            annual[[paste0(def$output_column, "_annual_sum")]] <- if (complete) sum(x) else NA_real_
          if (def$parameter == "T2M_MIN") annual$tmin_c_annual_min <- if (complete) min(x) else NA_real_
          if (def$parameter == "T2M_MAX") annual$tmax_c_annual_max <- if (complete) max(x) else NA_real_
        }
        annual_parts[[length(annual_parts) + 1L]] <<- annual
      }
      output_rds <- file.path(daily_dir, paste0("NASA_POWER_M_", code, "_1981_2024.rds"))
      output_csv <- file.path(daily_dir, paste0("NASA_POWER_M_", code, "_1981_2024.csv"))
      write_rds(d, output_rds)
      write_csv(d, output_csv)
      met_ok <- stats::complete.cases(d[dictionary$output_column[1:8]])
      solar_period <- d$year >= 1984L
      coverage_parts[[length(coverage_parts) + 1L]] <<- data.frame(municipality_code = code,
        municipality = site$municipality, first_date = as.character(min(d$date)), last_date = as.character(max(d$date)),
        rows = nrow(d), expected_rows = length(dates), meteorology_complete_value_days = sum(met_ok),
        solar_expected_days = sum(solar_period), solar_observed_days = sum(!is.na(d$shortwave_mj_m2[solar_period])),
        radiation_unavailable_before_1984_days = sum(!solar_period), temperature_order_flag_days = sum(d$temperature_order_flag),
        output_csv = output_csv, output_rds = output_rds, output_rds_md5 = hash(output_rds), stringsAsFactors = FALSE)
      report$processed_histories <<- report$processed_histories + 1L
      report$site_days <<- report$site_days + nrow(d)
      report$quality_flag_rows <<- sum(bind(quality_parts)$status == "REVIEW")
      save_progress()
      message("  Saved history: ", nrow(d), " daily records.")
    }
    assert(all(vapply(provenance$file, hash, character(1)) == provenance$md5), "An input changed during this run.")
    report$execution_status <<- if (report$quality_flag_rows > 0L) "COMPLETED_SIX_POINT_HISTORY_WITH_QA_FLAGS" else "COMPLETED_SIX_POINT_HISTORY"
    report$last_stage <<- "Six NASA POWER histories saved. No national climate average or trends calculated."
    save_progress()
    message("\n", report$execution_status, "\nReports: ", out_dir, "\nDaily series: ", daily_dir)
  }
  tryCatch(worker(), error = function(e) {
    report$error_if_incomplete <<- substr(conditionMessage(e), 1, 1000)
    report$execution_status <<- if (inherits(e, "power_rate_limit")) "PAUSED_RATE_LIMIT" else
      if (inherits(e, "power_access_block")) "STOPPED_ACCESS_BLOCK" else "INCOMPLETE"
    try(save_progress(), silent = TRUE)
    stop(conditionMessage(e), "\nPreviously downloaded NASA files were retained.\nSummary: ",
      file.path(out_dir, "coffee_power_history_summary.csv"), call. = FALSE)
  }, interrupt = function(e) {
    report$execution_status <<- "INTERRUPTED_RESUMABLE"
    report$error_if_incomplete <<- "Interrupted by user; successful NASA responses retained."
    try(save_progress(), silent = TRUE)
    message("\nInterrupted. Rerun the same file to reuse completed requests.")
    invisible(NULL)
  })
}

if (!isTRUE(getOption("coffee.power.skip_auto_run", FALSE))) coffee_power_step12()
