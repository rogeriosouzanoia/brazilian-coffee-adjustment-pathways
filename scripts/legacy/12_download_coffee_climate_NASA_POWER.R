# ======================================================================
# COFFE-STUDY | STEP 12 | NASA POWER DAILY HISTORY, SIX EXISTING POINTS
# Run this COMPLETE file using Source in RStudio.
# Replaces the Open-Meteo step 12. Steps 01-11 need not be rerun.
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
  requests <- quality_parts <- annual_parts <- coverage_parts <- list()
  provenance <- plan <- data.frame()
  report <- list(execution_status = "INCOMPLETE", source = "NASA POWER",
    scope = "SIX_EXISTING_PILOT_POINTS", meteorological_period = "1981 to 2024",
    radiation_period = "1984 to 2024", time_standard = "UTC", selected_points = 0,
    planned_requests = 0, validated_requests = 0, cache_requests = 0,
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
                       c("coverage_parts", "coffee_power_history_coverage.csv"))) {
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
    if (job$group == "meteorology") {
      assert(grepl("MERRA", source_text, ignore.case = TRUE), "MERRA-2 not identified in historical meteorology response.")
      assert(!grepl("GEOS", source_text, ignore.case = TRUE), "Unexpected GEOS source in fixed 1981-2024 history. Review metadata.")
    }
    if (job$group == "solar_1984_2000")
      assert(grepl("SRB", source_text, ignore.case = TRUE), "SRB not identified in 1984-2000 radiation response.")
    if (job$group == "solar_2001_2024")
      assert(grepl("CERES", source_text, ignore.case = TRUE), "CERES not identified in 2001-2024 radiation response.")
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
    list(data = d, units = bind(units), header = header, geometry = geom,
      messages = j$messages, api_version = scalar(header$api$version), sources = source_text,
      fill_value = fill)
  }

  last_http_end <- as.POSIXct("1970-01-01", tz = "UTC")
  fetch_power <- function(job) {
    dest <- job$raw_file
    if (file.exists(dest)) {
      # Never silently reuse a different query or an unverified raw file.
      assert(file.exists(paste0(dest, ".url.txt")), paste("Cache URL sidecar missing:", dest))
      cached_url <- paste(readLines(paste0(dest, ".url.txt"), warn = FALSE), collapse = "")
      assert(identical(cached_url, job$url), "Cached query differs from the requested query.")
      ans <- parse_power(dest, job)
      return(list(parsed = ans, cached = TRUE))
    }
    for (attempt in seq_len(max_attempts)) {
      elapsed <- as.numeric(difftime(Sys.time(), last_http_end, units = "secs"))
      if (elapsed < pause_seconds) Sys.sleep(pause_seconds - elapsed)
      report$http_attempts_this_run <<- report$http_attempts_this_run + 1L
      r <- tryCatch(httr::GET(job$url, httr::timeout(request_timeout_seconds),
        httr::user_agent("coffee-climate-research/1.0 (NASA POWER daily data)"),
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
      writeLines(body, tmp, useBytes = TRUE)
      ans <- tryCatch(parse_power(tmp, job), error = function(e) {
        rejected <- file.path(raw_dir, paste0("REVIEW_", basename(dest)))
        file.copy(tmp, rejected, overwrite = TRUE)
        unlink(tmp)
        stop("Response needs review. Saved to ", rejected, "\n", conditionMessage(e), call. = FALSE)
      })
      assert(file.copy(tmp, dest, overwrite = FALSE), paste("Cannot save raw response:", dest))
      unlink(tmp)
      writeLines(job$url, paste0(dest, ".url.txt"))
      return(list(parsed = ans, cached = FALSE))
    }
    stop("Download attempts exhausted.", call. = FALSE)
  }

  worker <- function() {
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
      "MERRA-2 meteorology 1981-2024; SRB radiation 1984-2000; CERES radiation 2001-2024.",
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
        meta <- list(public_url = job$url, raw_md5 = hash(job$raw_file), header = a$header,
          response_geometry = a$geometry, parameter_units = a$units, messages = a$messages,
          requested_municipal_longitude = site$longitude, requested_municipal_latitude = site$latitude)
        write_rds(meta, paste0(job$raw_file, ".metadata.rds"))
        status <- if (sum(a$units$missing_values) == 0L) "VALID_RESPONSE" else "VALID_RESPONSE_WITH_MISSING_VALUES"
        requests[[length(requests) + 1L]] <<- data.frame(municipality_code = code,
          group = job$group, start = job$start, end = job$end, public_url = job$url,
          raw_file = job$raw_file, raw_md5 = hash(job$raw_file), cached = response$cached,
          received_days = nrow(a$data), status = status, api_version = a$api_version, sources = a$sources,
          requested_longitude = site$longitude, requested_latitude = site$latitude,
          response_longitude = a$geometry[1L], response_latitude = a$geometry[2L],
          response_elevation_m = if (length(a$geometry) >= 3L) a$geometry[3L] else NA_real_,
          units_and_factors = paste(a$units$parameter, a$units$response_unit,
            a$units$conversion_factor, sep = ":", collapse = " | "),
          fill_values_as_na = sum(a$units$missing_values), stringsAsFactors = FALSE)
        report$validated_requests <<- length(requests)
        report$cache_requests <<- report$cache_requests + as.integer(response$cached)
        save_progress()
      }
      d$year <- as.integer(format(d$date, "%Y"))
      d$source <- "NASA POWER"
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
          solar_source_epoch = unique(z$solar_source_epoch), stringsAsFactors = FALSE)
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

coffee_power_step12()
