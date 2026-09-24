# ======================================================================
# COFFE-STUDY | STEP 16 | NASA POWER HISTORY AT THE PLANNED SOURCE CELLS
# Run this COMPLETE file using Source in RStudio.
#
# Default: execute the ENTIRE step15 plan, not another six-site pilot.
# Sources/periods: MET 1981-2024; SRB 1984-2000; CERES 2001-2024.
# One serial request per planned source cell. No parallel workers or keys.
# Successful raw responses and processed cell files are resumable.
# Production, boundaries, weights, pilot files and Open-Meteo are unchanged.
#
# IMPORTANT: the new requests are at source-grid sampling coordinates.
# Earlier pilot coordinates are different; proximity alone never authorizes
# replacing a grid query with a pilot file. Exact requests may be reused.
# Otherwise pilot series are compared with downloaded grid series, and any
# difference is reported without changing either dataset.
#
# There are no municipal climate means, ET0, trends, yield models or figures
# in this step. Source-specific daily series are stored once per cell.
# The municipality-to-cell index points to them without duplicating values.
#
# Optional environment settings, not needed for the normal full run:
# COFFEE_STUDY_DIR                 : project directory override
# COFFEE_POWER16_PREPARE_ONLY=TRUE  : validate inputs; do not call NASA
# COFFEE_POWER16_TEST_ONLY=TRUE     : run software tests only
# COFFEE_POWER16_MAX_REQUESTS=20    : optional limit on new jobs per run
# COFFEE_POWER16_AUTORUN=FALSE      : define the function without running it
#
# NASA documentation checked 2026-09-20:
# https://power.larc.nasa.gov/docs/services/api/temporal/daily/
# https://power.larc.nasa.gov/docs/methodology/data/sources/
# https://power.larc.nasa.gov/docs/faqs/data/
# ======================================================================

coffee_power_step16 <- function(base_dir = Sys.getenv("COFFEE_STUDY_DIR", unset =
  ".")) {
  version <- "16_source_grid_history_v1"
  endpoint <- "https://power.larc.nasa.gov/api/temporal/daily/point"
  wait_seconds <- 2
  timeout_seconds <- 300
  max_attempts <- 3L
  max_consecutive_failures <- 5L
  prepare_only <- toupper(Sys.getenv("COFFEE_POWER16_PREPARE_ONLY", "FALSE")) == "TRUE"
  test_only <- toupper(Sys.getenv("COFFEE_POWER16_TEST_ONLY", "FALSE")) == "TRUE"
  max_new_jobs <- suppressWarnings(as.numeric(Sys.getenv("COFFEE_POWER16_MAX_REQUESTS", "Inf")))
  if (length(max_new_jobs) != 1L || is.na(max_new_jobs) || max_new_jobs < 0)
    stop("COFFEE_POWER16_MAX_REQUESTS must be a nonnegative number or Inf.", call. = FALSE)

  plan_dir <- file.path(base_dir, "02_data_processed", "climate", "NASA_POWER", "spatial_plan")
  step15_dir <- file.path(base_dir, "04_outputs", "climate_power_spatial_expansion")
  out_dir <- file.path(base_dir, "04_outputs", "climate_power_brazil_download")
  raw_dir <- file.path(base_dir, "01_data_raw", "climate", "NASA_POWER", "brazil_grid")
  data_dir <- file.path(base_dir, "02_data_processed", "climate", "NASA_POWER", "brazil_grid")
  for (p in c(out_dir, raw_dir, data_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create directory: ", p, call. = FALSE)
  }
  old_options <- options(scipen = 999, timeout = max(timeout_seconds, getOption("timeout", 60)))
  on.exit(options(old_options), add = TRUE)
  lock <- file.path(out_dir, ".step16_running")
  if (!dir.create(lock, showWarnings = FALSE))
    stop("A step16 run may already be active. After confirming that it has stopped, remove only:\n", lock,
         "\nDo not remove downloaded weather files.", call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  writeLines(paste("PID", Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))

  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(x, cols, label) {
    absent <- setdiff(cols, names(x))
    assert(!length(absent), paste(label, "missing column(s):", paste(absent, collapse = ", ")))
  }
  hash <- function(path) {
    z <- unname(tools::md5sum(path))
    assert(length(z) == 1L && !is.na(z), paste("Cannot hash file:", path)); z
  }
  bind <- function(x) {
    x <- Filter(function(z) is.data.frame(z) && nrow(z) > 0L, x)
    if (!length(x)) return(data.frame())
    z <- do.call(rbind, x); rownames(z) <- NULL; z
  }
  number <- function(x, label, allow_na = FALSE) {
    text <- as.character(x); z <- suppressWarnings(as.numeric(text))
    assert(!any(!is.na(text) & is.na(z)) && all(is.na(z) | is.finite(z)) &&
             (allow_na || !anyNA(z)), paste("Invalid numeric field:", label)); z
  }
  scalar <- function(x, fallback = "NOT_REPORTED") {
    z <- as.character(unlist(x, use.names = FALSE))
    z <- z[!is.na(z) & nzchar(z)]
    if (!length(z)) fallback else paste(unique(z), collapse = " | ")
  }
  checked <- function(x, key) {
    need(x, c("check", "result"), "Summary")
    z <- x$result[x$check == key]
    assert(length(z) == 1L && !is.na(z), paste("Missing/ambiguous summary key:", key)); z
  }
  read_csv <- function(path) {
    assert(file.exists(path), paste("Required file not found:", path))
    utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE,
      check.names = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  write_csv <- function(x, path) {
    tmp <- tempfile("power16_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  write_rds <- function(x, path) {
    tmp <- tempfile("power16_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  make_url <- function(args) paste0(endpoint, "?", paste(names(args),
    vapply(as.character(args), utils::URLencode, character(1), reserved = TRUE), sep = "=", collapse = "&"))
  hash_text <- function(text) {
    f <- tempfile(); on.exit(unlink(f), add = TRUE)
    con <- file(f, "wb"); writeBin(charToRaw(enc2utf8(text)), con); close(con); hash(f)
  }
  stop_kind <- function(msg, cls) stop(structure(list(message = msg, call = NULL),
    class = c(cls, "error", "condition")))
  # Rebase ONLY the known project-relative suffix; never search by filename.
  local_path <- function(path) {
    path <- gsub("\\", "/", path, fixed = TRUE)
    if (file.exists(path)) return(path)
    tag <- "/coffe-study/"
    k <- regexpr(tag, path, fixed = TRUE)[1L]
    if (k > 0L) {
      candidate <- file.path(base_dir, substring(path, k + nchar(tag)))
      if (file.exists(candidate)) return(candidate)
    }
    path
  }
  near_axis <- function(x, origin, step) {
    t <- (x - origin) / step; lo <- floor(t)
    origin + ifelse(abs(t - lo - 0.5) < 1e-9, lo, floor(t + 0.5)) * step
  }
  cell_name <- function(family, lon, lat) paste0(family, "_Y", ifelse(lat < 0, "m", "p"),
    sprintf("%.0f", abs(round(lat * 1e6))), "_X", ifelse(lon < 0, "m", "p"),
    sprintf("%.0f", abs(round(lon * 1e6))))

  dictionary <- data.frame(
    parameter = c("T2M", "T2M_MIN", "T2M_MAX", "T2MDEW", "RH2M", "PRECTOTCORR", "WS2M", "PS", "ALLSKY_SFC_SW_DWN"),
    output_column = c("tmean_c", "tmin_c", "tmax_c", "dewpoint_mean_c", "rhmean_pct", "precipitation_mm", "wind2m_mean_m_s", "surface_pressure_kpa", "shortwave_mj_m2"),
    output_unit = c(rep("degC", 4), "%", "mm/day", "m/s", "kPa", "MJ/m2/day"),
    lower_screen = c(-60, -60, -60, -90, 0, 0, 0, 40, 0),
    upper_screen = c(60, 60, 60, 60, 100, 1500, 100, 110, 60), stringsAsFactors = FALSE)
  catalogue <- data.frame(family = c("MET", "SRB", "CERES"),
    start = c("1981-01-01", "1984-01-01", "2001-01-01"),
    end = c("2024-12-31", "2000-12-31", "2024-12-31"),
    parameters = c(paste(dictionary$parameter[1:8], collapse = ","), rep("ALLSKY_SFC_SW_DWN", 2)),
    stringsAsFactors = FALSE)
  conversion <- function(parameter, unit) {
    u <- tolower(unit)
    u <- gsub("\u00b2", "2", u, fixed = TRUE); u <- gsub("\u00b0", "", u, fixed = TRUE)
    u <- gsub("[[:space:]_*^()]", "", u)
    if (parameter %in% dictionary$parameter[1:4] && u %in% c("c", "degc", "degreescelsius", "celsius")) return(1)
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
    stop("Unknown response unit for ", parameter, ": ", unit, ". No conversion guessed.", call. = FALSE)
  }
  source_info <- function(family, sources) {
    raw <- as.character(unlist(sources, use.names = FALSE))
    raw <- raw[!is.na(raw) & nzchar(trimws(raw))]
    token <- unique(toupper(gsub("[^A-Za-z0-9]", "", raw)))
    derived <- token == "POWER"
    merra <- grepl("^MERRA2($|V|VERSION|[0-9])", token)
    srb <- grepl("^(GEWEX)?SRB($|[0-9]|R[0-9]|V[0-9]|RELEASE[0-9])", token)
    syn <- grepl("^(CERES)?SYN1DEG($|ED|V|[0-9])", token)
    ceres <- token == "CERES"
    allowed <- switch(as.character(family), MET = merra, SRB = srb, CERES = syn | ceres, rep(FALSE, length(token)))
    ok <- length(token) > 0L && any(allowed) && all(allowed | derived)
    product <- if (!ok) "UNRESOLVED" else if (family == "MET") "MERRA-2" else if (family == "SRB") "SRB" else
      if (any(syn)) "CERES SYN1deg" else "CERES (product not specified in header)"
    list(ok = ok, reported = scalar(sources), product = product)
  }
  parse_payload <- function(obj, job) {
    assert(identical(obj$type, "Feature") && identical(obj$geometry$type, "Point") &&
      is.list(obj$properties$parameter), "Expected a NASA POWER point Feature with parameter series.")
    h <- obj$header
    assert(toupper(scalar(h$time_standard)) == "UTC", "Response time standard is not confirmed UTC.")
    si <- source_info(job$family, h$sources)
    assert(si$ok, paste("Unknown/conflicting source identifiers:", si$reported, "for", job$family))
    fill <- number(unlist(h$fill_value), "header fill value")
    assert(length(fill) == 1L, "Missing or ambiguous fill value.")
    for (edge in c("start", "end")) if (!is.null(h[[edge]])) {
      assert(identical(gsub("-", "", scalar(h[[edge]]), fixed = TRUE),
                       gsub("-", "", as.character(job[[edge]]), fixed = TRUE)), paste("Header", edge, "differs from request."))
    }
    geom <- number(unlist(obj$geometry$coordinates, use.names = FALSE), "response geometry", TRUE)
    assert(length(geom) >= 2L && all(is.finite(geom[1:2])), "Missing response longitude/latitude.")
    assert(max(abs(geom[1:2] - c(job$longitude, job$latitude))) <= 0.01,
      "Response coordinates differ from the requested sampling point by more than 0.01 degree.")
    dates <- seq(as.Date(job$start), as.Date(job$end), by = "day")
    keys <- format(dates, "%Y%m%d"); pars <- strsplit(job$parameters, ",", fixed = TRUE)[[1L]]
    assert(all(pars %in% names(obj$properties$parameter)), "Response omitted a requested parameter.")
    d <- data.frame(date = dates); unit_parts <- list()
    for (p in pars) {
      s <- obj$properties$parameter[[p]]
      assert(!is.null(names(s)) && !anyDuplicated(names(s)) && setequal(names(s), keys),
        paste("Missing, duplicate or extra dates:", p))
      x <- vapply(s[match(keys, names(s))], function(v) {
        if (is.null(v)) return(NA_real_)
        assert(length(v) == 1L && is.numeric(v), paste("Non-numeric daily value:", p)); as.numeric(v)
      }, numeric(1))
      x[!is.na(x) & x == fill] <- NA_real_
      assert(all(is.na(x) | is.finite(x)), paste("Nonfinite values:", p))
      unit <- scalar(obj$parameters[[p]]$units); factor <- conversion(p, unit)
      def <- dictionary[match(p, dictionary$parameter), , drop = FALSE]
      d[[def$output_column]] <- x * factor
      unit_parts[[p]] <- data.frame(parameter = p, response_unit = unit,
        output_unit = def$output_unit, factor = factor, stringsAsFactors = FALSE)
    }
    list(data = d, units = bind(unit_parts), header = h, geometry = geom,
      sources = si$reported, source_product = si$product, api_version = scalar(h$api$version),
      fill_value = fill, messages = obj$messages)
  }
  parse_file <- function(path, job) parse_payload(jsonlite::fromJSON(path, simplifyVector = FALSE), job)

  # Self-tests use constructed in-memory examples, not invented observations.
  tests <- list()
  add_test <- function(label, ok) tests[[length(tests) + 1L]] <<- data.frame(test = label, pass = isTRUE(ok))
  errors <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
  make_fixture <- function() list(type = "Feature", geometry = list(type = "Point", coordinates = c(-46.875, -19, 900)),
    header = list(time_standard = "UTC", sources = list("MERRA2", "POWER"), fill_value = -999,
      start = "20200101", end = "20200103", api = list(version = "test")),
    properties = list(parameter = list(T2M = list("20200101" = 20, "20200102" = -999, "20200103" = 22))),
    parameters = list(T2M = list(units = "C")))
  tj <- data.frame(family = "MET", longitude = -46.875, latitude = -19,
    start = "2020-01-01", end = "2020-01-03", parameters = "T2M", stringsAsFactors = FALSE)
  tf <- make_fixture(); parsed_test <- parse_payload(tf, tj)
  add_test("Documented SYN1DEG identifier accepted", source_info("CERES", "SYN1DEG")$ok)
  add_test("MERRA2 plus POWER accepted", source_info("MET", c("MERRA2", "POWER"))$ok)
  add_test("Mixed solar epochs rejected", !source_info("CERES", c("SYN1DEG", "SRB"))$ok)
  add_test("Unknown source rejected", !source_info("MET", "unknown" )$ok)
  add_test("Fill value remains missing", is.na(parsed_test$data$tmean_c[2L]))
  bad <- make_fixture(); bad$header$time_standard <- "LST"
  add_test("Wrong time standard rejected", errors(parse_payload(bad, tj)))
  bad <- make_fixture(); bad$geometry$coordinates[1] <- -40
  add_test("Wrong coordinates rejected", errors(parse_payload(bad, tj)))
  bad <- make_fixture(); bad$properties$parameter$T2M <- bad$properties$parameter$T2M[-1L]
  add_test("Missing dates rejected", errors(parse_payload(bad, tj)))
  bad <- make_fixture(); names(bad$properties$parameter$T2M)[2L] <- "20200101"
  add_test("Duplicate dates rejected", errors(parse_payload(bad, tj)))
  add_test("kWh converted to MJ", conversion("ALLSKY_SFC_SW_DWN", "kW-hr/m^2/day") == 3.6)
  add_test("Daily mean W converted to MJ", abs(conversion("ALLSKY_SFC_SW_DWN", "W/m^2") - 0.0864) < 1e-12)
  add_test("Pressure conversion", conversion("PS", "Pa") == 0.001)
  add_test("Unknown units rejected", errors(conversion("T2M", "unknown")))
  add_test("Longitude lookup and source distinction", near_axis(-47.0559336, 0, 0.625) == -46.875 &&
    near_axis(-47.0559336, 0.5, 1) == -47.5)
  add_test("Period leap-day coverage", length(seq(as.Date("1981-01-01"), as.Date("2024-12-31"), by = "day")) == 16071)
  write_csv(bind(tests), file.path(out_dir, "coffee_power_brazil_software_tests.csv"))
  assert(all(bind(tests)$pass), "A software self-test failed. No network request was made.")
  if (test_only) { message("Step16 software tests passed. No network request made."); return(invisible(bind(tests))) }

  report <- list(execution_status = "INCOMPLETE", script_version = version, source = "NASA POWER",
    municipal_targets = 0L, planned_source_cells = 0L, completed_source_cells = 0L,
    cells_with_quality_flags = 0L, failed_cells = 0L, pending_cells = 0L,
    http_attempts_this_run = 0L, new_jobs_attempted_this_run = 0L,
    exact_cache_reused_this_run = 0L, pilot_exact_requests_reused = 0L,
    regional_probe_checks = 0L, regional_probe_mismatches = 0L,
    all_source_histories_acquired = FALSE, complete_values_in_all_histories = FALSE,
    municipal_daily_series_materialized = FALSE, national_climate_mean_calculated = FALSE,
    et0_calculated = FALSE, climate_trends_estimated = FALSE, climate_attribution_performed = FALSE,
    independent_municipal_weather_assumed = FALSE, historical_boundaries_harmonized = FALSE,
    previous_data_modified = FALSE, last_stage = "Reading plan", error_if_incomplete = "None")
  manifest <- data.frame(); quality_parts <- probe_parts <- pilot_parts <- list()
  input_paths <- character(); input_hashes <- character(); crosswalk <- data.frame()
  valid_states <- c("VALID_COMPLETE", "VALID_WITH_QA_FLAGS")
  full_save <- function(all_tables = TRUE) {
    if (nrow(manifest)) {
      ok <- manifest$status %in% valid_states
      report$completed_source_cells <<- sum(ok)
      report$cells_with_quality_flags <<- sum(manifest$status == "VALID_WITH_QA_FLAGS")
      report$failed_cells <<- sum(manifest$status %in% c("FAILED_REQUEST", "REVIEW_REQUIRED"))
      report$pending_cells <<- sum(!ok & !manifest$status %in% c("FAILED_REQUEST", "REVIEW_REQUIRED"))
      if (all_tables) {
        write_csv(manifest, file.path(out_dir, "coffee_power_brazil_download_manifest.csv"))
        write_csv(manifest[manifest$status %in% c("FAILED_REQUEST", "REVIEW_REQUIRED"), , drop = FALSE],
          file.path(out_dir, "coffee_power_brazil_failures.csv"))
      }
      coverage <- bind(lapply(catalogue$family, function(fam) {
        z <- manifest[manifest$family == fam, , drop = FALSE]
        good <- z$status %in% valid_states
        data.frame(family = fam, planned_cells = nrow(z), acquired_cells = sum(good),
          cells_without_quality_flags = sum(z$status == "VALID_COMPLETE"),
          cells_with_quality_flags = sum(z$status == "VALID_WITH_QA_FLAGS"),
          expected_cell_days = sum(z$expected_daily_rows),
          received_cell_days = sum(z$received_days[good], na.rm = TRUE),
          fully_observed_cell_days = sum(z$complete_value_days[good], na.rm = TRUE),
          downloaded_pct = if (nrow(z)) 100 * mean(good) else NA_real_)
      }))
      write_csv(coverage, file.path(out_dir, "coffee_power_brazil_download_coverage.csv"))
      if (all_tables && nrow(crosswalk)) {
        idx <- match(crosswalk$cell_id, manifest$cell_id)
        index <- crosswalk
        index$history_status <- manifest$status[idx]
        index$daily_rds <- ifelse(manifest$status[idx] %in% valid_states, manifest$daily_file[idx], NA_character_)
        index$metadata_rds <- ifelse(manifest$status[idx] %in% valid_states, manifest$metadata_file[idx], NA_character_)
        index$history_start <- manifest$start[idx]; index$history_end <- manifest$end[idx]
        write_csv(index, file.path(out_dir, "coffee_power_brazil_municipality_series_index.csv"))
      }
    }
    probes_done <- bind(probe_parts)
    report$regional_probe_checks <<- nrow(probes_done)
    report$regional_probe_mismatches <<- if (nrow(probes_done)) sum(probes_done$status == "MISMATCH") else 0L
    write_csv(data.frame(check = names(report), result = vapply(report, as.character, character(1))),
      file.path(out_dir, "coffee_power_brazil_download_summary.csv"))
    if (all_tables) {
      for (pair in list(c("quality_parts", "coffee_power_brazil_quality.csv"),
                        c("probe_parts", "coffee_power_brazil_probe_checks.csv"),
                        c("pilot_parts", "coffee_power_brazil_pilot_comparison.csv"))) {
        z <- bind(get(pair[1L], inherits = TRUE))
        if (nrow(z)) write_csv(z, file.path(out_dir, pair[2L]))
      }
    }
  }
  on.exit({
    try(full_save(TRUE), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step16.txt")), silent = TRUE)
  }, add = TRUE)

  worker <- function() {
    # ---- Verify the existing plan, not a guessed set of points ----------
    paths <- c(summary = file.path(step15_dir, "coffee_power_spatial_expansion_summary.csv"),
      plan = file.path(plan_dir, "coffee_power_brazil_request_plan.csv"),
      cells = file.path(plan_dir, "coffee_power_unique_sampling_cells.csv"),
      crosswalk = file.path(plan_dir, "coffee_municipality_power_grid_crosswalk.csv"),
      definitions = file.path(plan_dir, "coffee_power_source_grid_definitions.csv"),
      weights = file.path(plan_dir, "coffee_power_source_cell_weights.csv"),
      provenance = file.path(step15_dir, "coffee_power_spatial_expansion_provenance.csv"),
      discovery = file.path(step15_dir, "coffee_power_grid_discovery.csv"),
      probes = file.path(step15_dir, "coffee_power_grid_probe_points.csv"),
      candidates = file.path(step15_dir, "coffee_power_pilot_cache_candidates.csv"))
    assert(all(file.exists(paths)), paste("Missing step15 input(s):", paste(paths[!file.exists(paths)], collapse = "\n")))
    s15 <- read_csv(paths[["summary"]])
    assert(checked(s15, "execution_status") == "COMPLETED_SPATIAL_EXPANSION_PLAN" &&
      checked(s15, "grid_checks_passed") == "TRUE", "The step15 plan has not passed its checks.")
    origin <- read_csv(paths[["provenance"]]); need(origin, c("role", "file", "md5"), "Step15 provenance")
    origin_paths <- vapply(origin$file, local_path, character(1))
    assert(all(file.exists(origin_paths)), "An original step15 input is missing; no download was started.")
    assert(all(vapply(origin_paths, hash, character(1)) == origin$md5), "An original step15 input changed. Revalidate the plan first.")
    input_paths <<- unique(c(unname(paths), unname(origin_paths)))
    input_hashes <<- vapply(input_paths, hash, character(1))
    prior <- list.files(out_dir, pattern = "^coffee_power_brazil_.*\\.(csv|txt)$", full.names = TRUE)
    prior <- prior[!grepl("software_tests", prior)]
    if (length(prior)) {
      root <- file.path(out_dir, "previous_runs"); dir.create(root, showWarnings = FALSE)
      archive <- tempfile(paste0("run_", format(Sys.time(), "%Y%m%dT%H%M%S"), "_"), tmpdir = root)
      assert(dir.create(archive) && all(file.copy(prior, archive)), "Cannot retain previous reports.")
    }
    write_csv(data.frame(file = input_paths, md5 = unname(input_hashes)), file.path(out_dir, "coffee_power_brazil_input_provenance.csv"))
    jobs <- read_csv(paths[["plan"]]); cells <- read_csv(paths[["cells"]])
    cr <- read_csv(paths[["crosswalk"]]); definitions <- read_csv(paths[["definitions"]])
    weights <- read_csv(paths[["weights"]]); discovery <- read_csv(paths[["discovery"]]); probes <- read_csv(paths[["probes"]])
    need(jobs, c("cell_id", "family", "longitude", "latitude", "start", "end", "parameters", "community", "time_standard", "expected_daily_rows", "url"), "Request plan")
    need(cr, c("cell_id", "family", "municipality_code", "sampling_longitude", "sampling_latitude", "requested_municipal_longitude", "requested_municipal_latitude"), "Municipality crosswalk")
    need(definitions, c("family", "longitude_step", "latitude_step", "longitude_registration", "latitude_registration", "grid_checks_passed"), "Grid definitions")
    need(weights, c("cell_id", "family", "species", "year", "baseline_area_weight", "dynamic_area_weight", "baseline_production_weight", "dynamic_production_weight"), "Cell weights")
    need(probes, c("family", "anchor", "longitude", "latitude", "probe_date", "parameter", "value_raw"), "Regional probes")
    need(discovery, c("family", "anchor", "raw_file", "raw_md5", "source_recognized"), "Grid discovery")
    assert(!anyNA(jobs$cell_id) && !anyDuplicated(jobs$cell_id) && !anyDuplicated(jobs$url), "Duplicate/missing planned cell or URL.")
    assert(nrow(jobs) == as.integer(checked(s15, "planned_historical_requests")), "Plan count differs from step15 summary.")
    assert(setequal(jobs$cell_id, cells$cell_id) && !anyDuplicated(cells$cell_id), "Unique-cell list and jobs differ.")
    assert(setequal(unique(jobs$family), catalogue$family) && all(definitions$grid_checks_passed == "TRUE"), "Grid families are unverified.")
    assert(!anyDuplicated(definitions$family) && setequal(definitions$family, catalogue$family), "Grid definitions are ambiguous.")
    for (v in c("longitude", "latitude", "expected_daily_rows")) jobs[[v]] <- number(jobs[[v]], paste("plan", v))
    for (v in c("longitude_step", "latitude_step", "longitude_registration", "latitude_registration")) definitions[[v]] <- number(definitions[[v]], v)
    for (v in c("sampling_longitude", "sampling_latitude", "requested_municipal_longitude", "requested_municipal_latitude")) cr[[v]] <- number(cr[[v]], v)
    assert(all(jobs$community == "AG" & jobs$time_standard == "UTC"), "The plan must use AG and UTC.")
    assert(all(jobs$longitude >= -75 & jobs$longitude <= -30 & jobs$latitude >= -35 & jobs$latitude <= 6), "Grid coordinates outside the Brazil study envelope.")
    assert(!anyDuplicated(paste(cr$municipality_code, cr$family)) && setequal(cr$cell_id, jobs$cell_id), "Crosswalk key mismatch.")
    target_count <- as.integer(checked(s15, "municipal_targets"))
    assert(length(unique(cr$municipality_code)) == target_count && nrow(cr) == target_count * 3L, "Incomplete crosswalk.")
    for (fam in catalogue$family) {
      spec <- catalogue[catalogue$family == fam, , drop = FALSE]
      def <- definitions[definitions$family == fam, , drop = FALSE]
      jj <- jobs[jobs$family == fam, , drop = FALSE]; cc <- cr[cr$family == fam, , drop = FALSE]
      assert(all(jj$start == spec$start & jj$end == spec$end & jj$parameters == spec$parameters), paste("Unexpected parameters or period:", fam))
      n_days <- length(seq(as.Date(spec$start), as.Date(spec$end), by = "day"))
      assert(all(jj$expected_daily_rows == n_days), paste("Wrong expected date count:", fam))
      assert(all(cell_name(fam, jj$longitude, jj$latitude) == jj$cell_id), "Cell identifiers and coordinates disagree.")
      assert(max(abs(near_axis(jj$longitude, def$longitude_registration, def$longitude_step) - jj$longitude)) < 1e-7 &&
        max(abs(near_axis(jj$latitude, def$latitude_registration, def$latitude_step) - jj$latitude)) < 1e-7, "A request is off the inspected lattice.")
      assert(max(abs(near_axis(cc$requested_municipal_longitude, def$longitude_registration, def$longitude_step) - cc$sampling_longitude)) < 1e-7 &&
        max(abs(near_axis(cc$requested_municipal_latitude, def$latitude_registration, def$latitude_step) - cc$sampling_latitude)) < 1e-7,
        "Crosswalk differs from the step15 sampling rule.")
      k <- match(cc$cell_id, jj$cell_id)
      assert(!anyNA(k) && max(abs(cc$sampling_longitude - jj$longitude[k])) < 1e-7 &&
        max(abs(cc$sampling_latitude - jj$latitude[k])) < 1e-7, "Crosswalk coordinates disagree with jobs.")
    }
    wcols <- c("baseline_area_weight", "dynamic_area_weight", "baseline_production_weight", "dynamic_production_weight")
    assert(!anyDuplicated(paste(weights$cell_id, weights$species, weights$year)), "Duplicate cell-weight keys.")
    assert(setequal(weights$cell_id, jobs$cell_id) && nrow(weights) == nrow(jobs) * 3L * 13L &&
      setequal(weights$year, as.character(2012:2024)) && setequal(weights$species, c("Total", "Arabica", "Canephora")), "Incomplete cell-weight keys.")
    assert(all(weights$family == jobs$family[match(weights$cell_id, jobs$cell_id)]), "Source labels differ in cell weights.")
    for (v in wcols) {
      z <- number(weights[[v]], v); assert(all(z >= 0), paste("Negative weights:", v))
      s <- tapply(z, list(weights$family, weights$species, weights$year), sum)
      assert(all(is.finite(s)) && max(abs(s - 1)) < 1e-8, paste("Weights not conserved:", v))
    }
    expected_urls <- vapply(seq_len(nrow(jobs)), function(i) {
      j <- jobs[i, , drop = FALSE]
      make_url(c(parameters = j$parameters, community = "AG", longitude = sprintf("%.7f", j$longitude),
        latitude = sprintf("%.7f", j$latitude), start = gsub("-", "", j$start, fixed = TRUE),
        end = gsub("-", "", j$end, fixed = TRUE), format = "JSON", `time-standard` = "UTC"))
    }, character(1))
    assert(all(jobs$url == expected_urls), "Plan URLs do not exactly encode the validated requests.")
    # Keep the original step15 plan untouched; replace its planning-only label
    # in this execution ledger so a completed job is never labelled unexecuted.
    jobs$request_status <- NULL
    jobs$query_md5 <- vapply(jobs$url, hash_text, character(1))
    for (fam in catalogue$family) {
      dir.create(file.path(raw_dir, fam), showWarnings = FALSE)
      dir.create(file.path(data_dir, fam), showWarnings = FALSE)
    }
    stub <- paste0(jobs$cell_id, "_", substr(jobs$query_md5, 1L, 12L))
    jobs$raw_file <- file.path(raw_dir, jobs$family, paste0(stub, ".json"))
    jobs$daily_file <- file.path(data_dir, jobs$family, paste0(stub, ".rds"))
    jobs$metadata_file <- file.path(data_dir, jobs$family, paste0(stub, "_meta.rds"))
    jobs$receipt_file <- file.path(data_dir, jobs$family, paste0(stub, "_receipt.rds"))
    crosswalk <<- cr
    report$municipal_targets <<- target_count; report$planned_source_cells <<- nrow(jobs)

    # Prioritize a request in each family/anchor window before the rest.
    for (v in c("longitude", "latitude", "value_raw")) probes[[v]] <- number(probes[[v]], paste("probe", v))
    probes$cell_id <- cell_name(probes$family, probes$longitude, probes$latitude)
    first <- integer()
    for (fam in catalogue$family) for (a in unique(probes$anchor)) {
      p <- probes[probes$family == fam & probes$anchor == a, , drop = FALSE]
      k <- which(jobs$cell_id %in% p$cell_id)
      assert(length(k) > 0L, paste("No planned cell overlaps the inspected window:", fam, a))
      first <- c(first, k[1L])
    }
    sequence <- c(unique(first), setdiff(seq_len(nrow(jobs)), first))
    jobs <- jobs[sequence, , drop = FALSE]; rownames(jobs) <- NULL
    jobs$execution_order <- seq_len(nrow(jobs))
    jobs$status <- "PENDING"; jobs$error <- ""
    for (v in c("raw_md5", "daily_md5", "reuse_mode", "sources_reported", "source_product", "api_version", "validated_at_utc")) jobs[[v]] <- NA_character_
    for (v in c("received_days", "complete_value_days", "quality_flag_variables", "response_longitude", "response_latitude", "response_elevation_m")) jobs[[v]] <- NA_real_
    manifest <<- jobs
    write_csv(jobs, file.path(out_dir, "coffee_power_brazil_execution_plan.csv"))
    write_csv(dictionary, file.path(out_dir, "coffee_power_brazil_variable_dictionary.csv"))
    notes <- c("COFFE-STUDY | STEP16 | COMPLETE SOURCE-CELL HISTORY ACQUISITION",
      paste("Script:", version), paste("Executed:", format(Sys.time(), usetz = TRUE)),
      "All step15 source-cell jobs are eligible by default. This is not another pilot.",
      "Source grids: independent MET, SRB and CERES query/cache namespaces.",
      "Raw observations, original pilot files, weights and Open-Meteo are never modified or merged.",
      "Existing pilot files can replace a job ONLY for the exact same validated request.",
      "Otherwise new cell histories are compared with candidate pilot histories without substituting values.",
      "The source-cell sampling coordinates differ from municipal interior reference points.",
      "Two-window regional inspection supports a sampling lattice, not worldwide point-service equivalence.",
      "Where discovery overlaps a planned cell, its one-date value is checked against the point response.",
      "Unknown units, wrong dates, coordinates, time standard or source labels stop processing for review.",
      "MERRA2 and POWER metadata accepted for meteorology; SYN1DEG is recognized as a CERES product.",
      "NA/fill values stay missing. Broad value-range checks are screening, not station validation.",
      "No extra smoothing, altitude correction or values taken from another weather source are used.",
      "The declared periods are MET 1981-2024, SRB 1984-2000, CERES 2001-2024.",
      "Radiation before 1984 is not requested or filled; solar epochs remain separate.",
      "One sequential HTTP job at a time, with a pause and bounded retries for temporary failures.",
      "HTTP 429, 401 and 403 halt the run. No bypass, concurrent workers or quota reset.",
      "Cache receipts bind the exact query, raw hash, processed hash and validator version.",
      "Some municipalities share weather samples. They must not be treated as independent climatic observations.",
      "No ET0, soil water, municipal exposure mean, trend or climate attribution is computed in this step.",
      "Future nonlinear indices require the proper municipality/source combination before weighted aggregation.",
      "https://power.larc.nasa.gov/docs/services/api/temporal/daily/",
      "https://power.larc.nasa.gov/docs/methodology/data/sources/")
    writeLines(notes, file.path(out_dir, "coffee_power_brazil_download_method_notes.txt"))
    full_save(TRUE)
    if (prepare_only) {
      report$execution_status <<- "PREPARED_NO_DOWNLOAD"; report$last_stage <<- "Plan verified; no NASA calls made"
      full_save(TRUE); message("Plan verified. PREPARED_NO_DOWNLOAD"); return(invisible(NULL))
    }
    for (pkg in c("httr", "jsonlite")) {
      if (!requireNamespace(pkg, quietly = TRUE)) utils::install.packages(pkg, repos = "https://cloud.r-project.org")
      assert(requireNamespace(pkg, quietly = TRUE), paste("Package unavailable:", pkg))
    }

    # Recover the actual unit and missing-value code of each regional probe.
    probes$value_converted <- NA_real_; probes$unit <- NA_character_
    for (i in seq_len(nrow(discovery))) {
      path <- local_path(discovery$raw_file[i])
      assert(file.exists(path) && hash(path) == discovery$raw_md5[i], "A regional discovery payload is missing or changed.")
      obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
      assert(identical(obj$type, "FeatureCollection"), "Unexpected cached regional schema.")
      ii <- which(probes$family == discovery$family[i] & probes$anchor == discovery$anchor[i])
      for (r in ii) {
        p <- probes$parameter[r]
        u <- obj$parameters[[p]]$units
        if (is.null(u) && length(obj$features)) u <- obj$features[[1L]]$parameters[[p]]$units
        unit <- scalar(u)
        fill <- obj$header$fill_value
        if (is.null(fill) && length(obj$features)) fill <- obj$features[[1L]]$header$fill_value
        fill <- number(unlist(fill), "regional fill value")
        assert(length(fill) == 1L, "Regional fill-value metadata absent or ambiguous.")
        probes$unit[r] <- unit
        probes$value_converted[r] <- if (probes$value_raw[r] == fill) NA_real_ else probes$value_raw[r] * conversion(p, unit)
      }
    }
    assert(!anyNA(probes$unit), "Some regional reference units were not resolved.")
    # Signatures prevent fast reuse of receipts against changed QA references.
    validation_signature <- hash_text(paste(version, hash(paths[["probes"]]), hash(paths[["discovery"]]), sep = "|"))
    pilot_path <- file.path(base_dir, "04_outputs", "climate_nasa_power", "coffee_power_requests.csv")
    pilot_candidates <- read_csv(paths[["candidates"]])
    pilot_requests <- if (file.exists(pilot_path)) read_csv(pilot_path) else data.frame()
    if (nrow(pilot_requests)) need(pilot_requests, c("municipality_code", "group", "public_url", "raw_file", "raw_md5", "requested_longitude", "requested_latitude"), "Pilot requests")
    if (nrow(pilot_candidates)) need(pilot_candidates, c("municipality_code", "family", "candidate_cell_id"), "Pilot cache candidates")

    qa_for <- function(a, job) {
      d <- a$data; parts <- list()
      order_flag <- rep(FALSE, nrow(d))
      if (job$family == "MET") order_flag <- with(d,
        (!is.na(tmin_c) & !is.na(tmean_c) & tmin_c > tmean_c + 0.05) |
        (!is.na(tmean_c) & !is.na(tmax_c) & tmean_c > tmax_c + 0.05) |
        (!is.na(tmin_c) & !is.na(tmax_c) & tmin_c > tmax_c + 0.05))
      for (p in a$units$parameter) {
        def <- dictionary[match(p, dictionary$parameter), , drop = FALSE]
        x <- d[[def$output_column]]; outside <- !is.na(x) & (x < def$lower_screen | x > def$upper_screen)
        ordering <- if (p %in% c("T2M", "T2M_MIN", "T2M_MAX")) sum(order_flag) else 0L
        bad <- anyNA(x) || any(outside) || ordering > 0L
        parts[[p]] <- data.frame(cell_id = job$cell_id, family = job$family, parameter = p,
          start = job$start, end = job$end, expected_days = job$expected_daily_rows,
          observed_days = sum(!is.na(x)), missing_days = sum(is.na(x)),
          outside_broad_bounds_days = sum(outside), temperature_order_failures = ordering,
          minimum = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
          maximum = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE),
          status = if (bad) "REVIEW_VALUES" else "PASS", stringsAsFactors = FALSE)
      }
      bind(parts)
    }
    probe_check <- function(a, job) {
      z <- probes[probes$cell_id == job$cell_id, , drop = FALSE]
      if (!nrow(z)) return(data.frame())
      out <- lapply(seq_len(nrow(z)), function(i) {
        col <- dictionary$output_column[match(z$parameter[i], dictionary$parameter)]
        j <- match(as.Date(z$probe_date[i]), a$data$date)
        assert(!is.na(j) && col %in% names(a$data), "Regional probe is outside the validated cell payload.")
        observed <- a$data[[col]][j]; ref <- z$value_converted[i]
        comparable <- is.finite(ref) && is.finite(observed)
        diff <- if (comparable) observed - ref else NA_real_
        status <- if (!comparable) "NOT_TESTABLE_MISSING_VALUE" else if (abs(diff) <= 0.02) "MATCH" else "MISMATCH"
        data.frame(cell_id = job$cell_id, family = job$family, anchor = z$anchor[i],
          date = z$probe_date[i], parameter = z$parameter[i], regional_value = ref,
          point_value = observed, difference = diff, absolute_tolerance = 0.02, status = status)
      })
      bind(out)
    }
    pilot_compare <- function(a, job) {
      candidates <- pilot_candidates[pilot_candidates$candidate_cell_id == job$cell_id, , drop = FALSE]
      if (!nrow(candidates) || !nrow(pilot_requests)) return(data.frame())
      out <- list(); group_map <- c(MET = "meteorology", SRB = "solar_1984_2000", CERES = "solar_2001_2024")
      for (i in seq_len(nrow(candidates))) {
        cc <- candidates[i, , drop = FALSE]
        q <- pilot_requests[pilot_requests$municipality_code == cc$municipality_code & pilot_requests$group == group_map[[job$family]], , drop = FALSE]
        comparison <- tryCatch({
          assert(nrow(q) == 1L, "Pilot query record missing/ambiguous.")
          path <- local_path(q$raw_file)
          assert(file.exists(path) && hash(path) == q$raw_md5, "Pilot raw payload missing or changed.")
          pj <- job; pj$longitude <- number(q$requested_longitude, "pilot longitude")
          pj$latitude <- number(q$requested_latitude, "pilot latitude")
          b <- parse_file(path, pj)
          assert(identical(a$data$date, b$data$date), "Pilot and cell dates differ.")
          bind(lapply(a$units$parameter, function(p) {
            v <- dictionary$output_column[match(p, dictionary$parameter)]
            x <- a$data[[v]]; y <- b$data[[v]]
            paired <- is.finite(x) & is.finite(y); same_missing <- identical(is.na(x), is.na(y))
            md <- if (any(paired)) max(abs(x[paired] - y[paired])) else NA_real_
            same <- same_missing && is.finite(md) && md < 1e-8
            data.frame(cell_id = job$cell_id, family = job$family, municipality_code = cc$municipality_code,
              parameter = p, comparable_days = sum(paired), missing_pattern_equal = same_missing,
              maximum_absolute_difference = md, pilot_query_equals_grid_query = identical(q$public_url, job$url),
              status = if (same) "VALUES_MATCH_ON_REQUESTED_DATES" else "VALUES_DIFFER_BOTH_SERIES_PRESERVED", note = "No coordinate-based substitution made.")
          }))
        }, error = function(e) data.frame(cell_id = job$cell_id, family = job$family,
          municipality_code = cc$municipality_code, parameter = "ALL", comparable_days = 0L,
          missing_pattern_equal = NA, maximum_absolute_difference = NA_real_, pilot_query_equals_grid_query = FALSE,
          status = "PILOT_NOT_COMPARABLE", note = substr(conditionMessage(e), 1, 600)))
        out[[length(out) + 1L]] <- comparison
      }
      bind(out)
    }

    last_http_end <- as.POSIXct("1970-01-01", tz = "UTC")
    cache_url <- function(path, url) {
      side <- paste0(path, ".url.txt")
      assert(file.exists(side), paste("Cached raw response has no URL sidecar:", path))
      assert(identical(paste(readLines(side, warn = FALSE), collapse = ""), url), "Cached URL differs from planned URL.")
    }
    fetch <- function(job) {
      dest <- job$raw_file
      if (file.exists(dest)) {
        cache_url(dest, job$url)
        return(list(parsed = parse_file(dest, job), mode = "REVALIDATED_GRID_RAW_CACHE"))
      }
      # Reuse only an EXACT pilot request, not merely a nearby coordinate.
      ix <- if (nrow(pilot_requests)) which(pilot_requests$public_url == job$url) else integer()
      if (length(ix) == 1L) {
        p <- local_path(pilot_requests$raw_file[ix])
        assert(file.exists(p) && hash(p) == pilot_requests$raw_md5[ix], "Exact pilot cache changed or missing.")
        a <- parse_file(p, job)
        writeLines(job$url, paste0(dest, ".url.txt"), useBytes = TRUE)
        assert(file.copy(p, dest, overwrite = FALSE), "Cannot retain exact-query cache copy.")
        report$pilot_exact_requests_reused <<- report$pilot_exact_requests_reused + 1L
        return(list(parsed = a, mode = "EXACT_PILOT_QUERY_CACHE"))
      }
      review <- paste0(dest, ".review")
      if (file.exists(review)) {
        cache_url(review, job$url)
        a <- parse_file(review, job)
        writeLines(job$url, paste0(dest, ".url.txt"), useBytes = TRUE)
        assert(file.copy(review, dest, overwrite = FALSE), "Cannot recover reviewed payload.")
        return(list(parsed = a, mode = "REVALIDATED_REVIEW_CACHE"))
      }
      if (report$new_jobs_attempted_this_run >= max_new_jobs)
        stop_kind("Configured new-request limit reached. Run again to continue from saved files.", "power16_job_limit")
      report$new_jobs_attempted_this_run <<- report$new_jobs_attempted_this_run + 1L
      for (attempt in seq_len(max_attempts)) {
        delay <- wait_seconds - as.numeric(difftime(Sys.time(), last_http_end, units = "secs"))
        if (delay > 0) Sys.sleep(delay)
        report$http_attempts_this_run <<- report$http_attempts_this_run + 1L
        r <- tryCatch(httr::GET(job$url, httr::timeout(timeout_seconds),
          httr::user_agent("coffee-climate-research/2.0 (serial NASA POWER source-grid histories)"),
          httr::add_headers(Accept = "application/json")), error = function(e) e)
        last_http_end <<- Sys.time()
        if (inherits(r, "error")) {
          if (attempt == max_attempts) stop_kind(conditionMessage(r), "power16_temporary")
          Sys.sleep(5 * 2^(attempt - 1L)); next
        }
        sc <- httr::status_code(r)
        if (sc == 429L) stop_kind(paste("NASA HTTP 429. Retry-After:", scalar(httr::headers(r)[["retry-after"]]),
          "All successful downloads are retained. No automatic retry on a rate limit."), "power16_rate_limit")
        if (sc %in% c(401L, 403L)) stop_kind(paste("NASA HTTP", sc, ": stopped without bypassing access controls."), "power16_access")
        body <- httr::content(r, as = "text", encoding = "UTF-8")
        if (sc >= 500L || sc == 408L) {
          if (attempt == max_attempts) stop_kind(paste("NASA HTTP", sc, substr(body, 1, 500)), "power16_temporary")
          Sys.sleep(5 * 2^(attempt - 1L)); next
        }
        assert(sc == 200L, paste("NASA HTTP", sc, substr(body, 1, 1000)))
        assert(grepl("^[[:space:]]*\\{", body), "Non-JSON response was not accepted as weather.")
        tmp <- tempfile("payload16_", tmpdir = dirname(dest), fileext = ".json")
        on.exit(unlink(tmp), add = TRUE)
        writeLines(body, tmp, useBytes = TRUE)
        a <- tryCatch(parse_file(tmp, job), error = function(e) {
          writeLines(job$url, paste0(review, ".url.txt"), useBytes = TRUE)
          assert(file.copy(tmp, review, overwrite = FALSE), "Cannot retain review payload.")
          writeLines(conditionMessage(e), paste0(review, ".reason.txt"), useBytes = TRUE)
          stop("Response requires review: ", review, "\n", conditionMessage(e), call. = FALSE)
        })
        writeLines(job$url, paste0(dest, ".url.txt"), useBytes = TRUE)
        assert(file.copy(tmp, dest, overwrite = FALSE), paste("Cannot save:", dest))
        unlink(tmp)
        return(list(parsed = a, mode = "NEW_GRID_QUERY"))
      }
      stop_kind("Request attempts exhausted.", "power16_temporary")
    }
    load_receipt <- function(job) {
      if (!file.exists(job$receipt_file)) return(NULL)
      rc <- tryCatch(readRDS(job$receipt_file), error = function(e) NULL)
      if (!is.list(rc) || !identical(rc$version, version) || !identical(rc$signature, validation_signature) ||
          !identical(rc$url, job$url) || !is.data.frame(rc$row) || nrow(rc$row) != 1L ||
          !all(names(job) %in% names(rc$row))) return(NULL)
      if (!all(file.exists(c(job$raw_file, job$daily_file, job$metadata_file)))) return(NULL)
      cache_url(job$raw_file, job$url)
      if (!identical(hash(job$raw_file), rc$row$raw_md5) || !identical(hash(job$daily_file), rc$row$daily_md5) ||
          !identical(hash(job$metadata_file), rc$metadata_md5)) return(NULL)
      if (!rc$row$status %in% valid_states || !is.data.frame(rc$quality)) return(NULL)
      # Refresh plan-only fields (e.g. linked municipality count/order) while
      # retaining the validated weather and its validation timestamp.
      plan_fields <- setdiff(names(job), c("status", "error", "raw_md5", "daily_md5", "reuse_mode",
        "sources_reported", "source_product", "api_version", "validated_at_utc", "received_days",
        "complete_value_days", "quality_flag_variables", "response_longitude", "response_latitude", "response_elevation_m"))
      rc$row[plan_fields] <- job[plan_fields]
      rc$row$reuse_mode <- "VERIFIED_GRID_RECEIPT_CACHE"
      rc
    }
    completed_this_run <- 0L; consecutive <- 0L
    message("\nNASA POWER: ", nrow(jobs), " planned source-cell histories for ", target_count, " municipal targets.",
      "\nFull execution. Existing successful grid requests will be reused.\n")
    for (i in seq_len(nrow(jobs))) {
      job <- jobs[i, , drop = FALSE]; id <- job$cell_id
      report$last_stage <<- paste(i, "of", nrow(jobs), id)
      message(sprintf("[%d/%d] %s | %s to %s", i, nrow(jobs), id, job$start, job$end))
      answer <- tryCatch({
        rc <- load_receipt(job)
        if (is.null(rc)) {
          fetched <- fetch(job); a <- fetched$parsed
          pc <- probe_check(a, job); probe_parts[[id]] <<- pc
          if (nrow(pc)) assert(!any(pc$status == "MISMATCH"),
            paste("Grid point and cached regional probe differ. See coffee_power_brazil_probe_checks.csv; raw data retained:", id))
          qa <- qa_for(a, job); pilot_check <- pilot_compare(a, job)
          d <- a$data; d$cell_id <- id; d$family <- job$family
          d$source <- "NASA POWER"; d$source_product <- a$source_product; d$time_standard <- "UTC"
          meta <- list(script_version = version, query = job$url, cell_id = id, family = job$family,
            sampling_longitude = job$longitude, sampling_latitude = job$latitude,
            response_geometry = a$geometry, header = a$header, parameter_units = a$units,
            sources_reported = a$sources, source_product = a$source_product,
            raw_file = job$raw_file, raw_md5 = hash(job$raw_file), messages = a$messages,
            note = "Source-grid sample; not farm location or municipal polygon mean.")
          write_rds(d, job$daily_file); write_rds(meta, job$metadata_file)
          row <- job
          row$status <- if (any(qa$status != "PASS")) "VALID_WITH_QA_FLAGS" else "VALID_COMPLETE"
          row$raw_md5 <- meta$raw_md5; row$daily_md5 <- hash(job$daily_file)
          row$received_days <- nrow(a$data)
          row$complete_value_days <- sum(stats::complete.cases(a$data[setdiff(names(a$data), "date")]))
          row$quality_flag_variables <- sum(qa$status != "PASS")
          row$reuse_mode <- fetched$mode; row$sources_reported <- a$sources
          row$source_product <- a$source_product; row$api_version <- a$api_version
          row$response_longitude <- a$geometry[1L]; row$response_latitude <- a$geometry[2L]
          row$response_elevation_m <- if (length(a$geometry) >= 3L) a$geometry[3L] else NA_real_
          row$validated_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          rc <- list(version = version, signature = validation_signature, url = job$url,
            row = row, quality = qa, probe = pc, pilot = pilot_check, metadata_md5 = hash(job$metadata_file))
          write_rds(rc, job$receipt_file)
        }
        rc
      }, error = function(e) e)
      if (inherits(answer, "error")) {
        manifest$error[i] <<- substr(conditionMessage(answer), 1, 1400)
        if (inherits(answer, "power16_temporary")) {
          manifest$status[i] <<- "FAILED_REQUEST"; consecutive <- consecutive + 1L
          full_save(TRUE); message("  Temporary failure retained for next run: ", conditionMessage(answer))
          if (consecutive >= max_consecutive_failures) stop_kind("Five consecutive temporary failures. Rerun after the service/connection recovers.", "power16_temporary")
          next
        }
        manifest$status[i] <<- if (inherits(answer, c("power16_rate_limit", "power16_access", "power16_job_limit"))) "PENDING" else "REVIEW_REQUIRED"
        full_save(TRUE); stop(answer)
      }
      consecutive <- 0L
      manifest[i, ] <<- answer$row[, names(manifest), drop = FALSE]
      quality_parts[[id]] <<- answer$quality; probe_parts[[id]] <<- answer$probe; pilot_parts[[id]] <<- answer$pilot
      if (answer$row$reuse_mode != "NEW_GRID_QUERY") report$exact_cache_reused_this_run <<- report$exact_cache_reused_this_run + 1L
      completed_this_run <- completed_this_run + 1L
      full_save(i %% 10L == 0L)
      message("  ", answer$row$status, " | ", answer$row$received_days, " days | ", answer$row$reuse_mode,
        " | completed ", report$completed_source_cells, "/", nrow(jobs))
    }
    assert(all(vapply(input_paths, hash, character(1)) == input_hashes), "An original input changed during execution.")
    all_done <- all(manifest$status %in% valid_states)
    all_clean <- all(manifest$status == "VALID_COMPLETE")
    report$all_source_histories_acquired <<- all_done
    report$complete_values_in_all_histories <<- all_done && all(manifest$complete_value_days == manifest$expected_daily_rows)
    report$execution_status <<- if (!all_done) "INCOMPLETE_RESUMABLE" else if (!all_clean)
      "COMPLETED_GRID_HISTORY_WITH_QA_FLAGS" else "COMPLETED_GRID_HISTORY"
    report$last_stage <<- if (all_done) "All planned cell histories saved; municipal climate aggregation is a later step" else
      "Some requests failed. Rerun this same script; successful cache files will be reused"
    full_save(TRUE)
    message("\n", report$execution_status, "\nReports: ", out_dir, "\nCell daily files: ", data_dir)
    invisible(manifest)
  }
  tryCatch(worker(), error = function(e) {
    report$error_if_incomplete <<- substr(conditionMessage(e), 1, 1600)
    report$execution_status <<- if (inherits(e, "power16_rate_limit")) "PAUSED_RATE_LIMIT" else
      if (inherits(e, "power16_access")) "STOPPED_ACCESS_BLOCK" else
      if (inherits(e, "power16_job_limit")) "PAUSED_CONFIGURED_LIMIT" else
      if (inherits(e, "power16_temporary")) "INCOMPLETE_RESUMABLE" else "REVIEW_REQUIRED"
    try(full_save(TRUE), silent = TRUE)
    stop(conditionMessage(e), "\nSuccessful downloads and all previous-step data were retained.\nSummary: ",
      file.path(out_dir, "coffee_power_brazil_download_summary.csv"), call. = FALSE)
  }, interrupt = function(e) {
    report$execution_status <<- "INTERRUPTED_RESUMABLE"
    report$error_if_incomplete <<- "Interrupted by user. Run this same script to continue."
    try(full_save(TRUE), silent = TRUE)
    message("\nInterrupted. Completed cell files were retained; rerun step16 to continue.")
    invisible(NULL)
  })
}

if (toupper(Sys.getenv("COFFEE_POWER16_AUTORUN", "TRUE")) != "FALSE") coffee_power_step16()
