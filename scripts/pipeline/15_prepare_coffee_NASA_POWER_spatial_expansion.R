# ======================================================================
# COFFE-STUDY | STEP 15 | SOURCE-GRID PLAN FOR SPATIAL EXPANSION
# Run this COMPLETE file using Source in RStudio.
#
# Reads ALL municipal climate targets and weights prepared in step 11.
# Makes only SIX SMALL regional queries: one date, one variable, two
# geographically separated windows for each of MERRA-2, SRB and CERES.
# These queries inspect the coordinates actually returned by POWER.
# DOES NOT start the full historical download and DOES NOT alter the pilot.
#
# Output: municipality/source-grid crosswalks, unique sampling cells,
# aggregated weights, a historical request plan and a cache-candidate list.
# The number of unique cells is CALCULATED from the user's registry.
#
# A matching regular regional grid supports the explicit nearest-axis
# sampling rule used here. It is not a coffee-location map or proof that
# all point/API products share identical spatial processing. Pilot cache
# candidates require revalidation before any future reuse. No weather
# values are reassigned to other municipalities by this planning script.
#
# Official documentation consulted 2026-09-20:
# https://power.larc.nasa.gov/docs/services/api/temporal/daily/
# https://power.larc.nasa.gov/docs/methodology/data/sources/
# https://power.larc.nasa.gov/docs/faqs/data/
#
# To run the local tests only, set COFFEE_POWER_GRID_TEST_ONLY=TRUE.
# ======================================================================

coffee_power_step15 <- function() {
  base_dir <- Sys.getenv("COFFEE_STUDY_DIR", unset =
    ".")
  out_dir <- file.path(base_dir, "04_outputs", "climate_power_spatial_expansion")
  plan_dir <- file.path(base_dir, "02_data_processed", "climate", "NASA_POWER", "spatial_plan")
  raw_dir <- file.path(base_dir, "01_data_raw", "climate", "NASA_POWER", "grid_discovery")
  for (p in c(out_dir, plan_dir, raw_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Cannot create: ", p, call. = FALSE)
  }
  old_options <- options(timeout = max(240, getOption("timeout", 60)), scipen = 999)
  on.exit(options(old_options), add = TRUE)
  lock <- file.path(out_dir, ".step15_running")
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("Another step15 run may be active. Confirm that it has stopped before removing:\n",
         lock, call. = FALSE)
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  writeLines(paste("PID", Sys.getpid(), format(Sys.time(), usetz = TRUE)), file.path(lock, "owner.txt"))

  assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  need <- function(x, cols, label) {
    absent <- setdiff(cols, names(x))
    assert(!length(absent), paste(label, "missing column(s):", paste(absent, collapse = ", ")))
  }
  hash <- function(path) unname(tools::md5sum(path))
  bind <- function(x) {
    if (!length(x)) return(data.frame())
    y <- do.call(rbind, x); rownames(y) <- NULL; y
  }
  number <- function(x, label, allow_na = FALSE) {
    y <- suppressWarnings(as.numeric(as.character(x)))
    assert(!any(!is.na(x) & is.na(y)) && all(is.na(y) | is.finite(y)) &&
             (allow_na || !anyNA(y)), paste("Invalid numeric data:", label))
    y
  }
  scalar <- function(x, fallback = "NOT_REPORTED") {
    x <- unlist(x, use.names = FALSE)
    if (!length(x)) fallback else paste(unique(as.character(x)), collapse = " | ")
  }
  read_csv <- function(path) {
    assert(file.exists(path), paste("Required file not found:", path))
    utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE,
      check.names = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
  }
  write_csv <- function(x, path) {
    tmp <- tempfile("power15_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  write_rds <- function(x, path) {
    tmp <- tempfile("power15_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    assert(file.copy(tmp, path, overwrite = TRUE), paste("Cannot save:", path))
  }
  checked <- function(x, key) {
    need(x, c("check", "result"), "Summary")
    y <- x$result[x$check == key]
    assert(length(y) == 1L && !is.na(y), paste("Missing/ambiguous summary key:", key))
    y
  }
  make_url <- function(endpoint, parameters) {
    paste0(endpoint, "?", paste(names(parameters), vapply(as.character(parameters),
      utils::URLencode, character(1), reserved = TRUE), sep = "=", collapse = "&"))
  }
  url_hash <- function(url) {
    p <- tempfile(); on.exit(unlink(p), add = TRUE)
    con <- file(p, "wb"); writeBin(charToRaw(enc2utf8(url)), con); close(con)
    hash(p)
  }
  stop_kind <- function(msg, cls) {
    stop(structure(list(message = msg, call = NULL), class = c(cls, "error", "condition")))
  }
  near_axis <- function(x, origin, step) {
    assert(length(origin) == 1L && length(step) == 1L && is.finite(step) && step > 0,
           "Invalid grid spacing.")
    scaled <- (x - origin) / step
    lo <- floor(scaled)
    tie <- abs(scaled - lo - 0.5) < 1e-9
    # Exact ties go to the lower coordinate and are exported for review.
    index <- ifelse(tie, lo, floor(scaled + 0.5))
    list(coordinate = origin + index * step, tie = tie)
  }
  haversine <- function(lon1, lat1, lon2, lat2) {
    rad <- pi / 180
    a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) *
         sin((lon2 - lon1) * rad / 2)^2
    6371.0088 * 2 * atan2(sqrt(pmin(1, pmax(0, a))), sqrt(pmax(0, 1 - a)))
  }
  cell_id <- function(family, lon, lat) {
    paste0(family, "_Y", ifelse(lat < 0, "m", "p"), sprintf("%.0f", abs(round(lat * 1e6))),
           "_X", ifelse(lon < 0, "m", "p"), sprintf("%.0f", abs(round(lon * 1e6))))
  }

  # Local mathematical tests. These do not make network calls.
  tests <- list()
  add_test <- function(label, ok) {
    tests[[length(tests) + 1L]] <<- data.frame(test = label, pass = isTRUE(ok))
  }
  add_test("Nearest axis with negative coordinates",
           abs(near_axis(-47.0559336, 0, 0.625)$coordinate - (-46.875)) < 1e-10)
  add_test("Different grid registration retained",
           abs(near_axis(-47.0559336, 0.5, 1)$coordinate - (-47.5)) < 1e-10)
  add_test("Exact tie flagged and sent to lower coordinate",
           near_axis(-47, 0.5, 1)$tie && near_axis(-47, 0.5, 1)$coordinate == -47.5)
  add_test("Coincident locations have zero distance", haversine(-47, -19, -47, -19) == 0)
  add_test("Sharing a cell preserves sum of weights",
           abs(sum(tapply(c(0.2, 0.3, 0.5), c("a", "a", "b"), sum)) - 1) < 1e-12)
  add_test("Source families have separate cell IDs",
           cell_id("SRB", -47.5, -19.5) != cell_id("CERES", -47.5, -19.5))
  write_csv(bind(tests), file.path(out_dir, "coffee_power_grid_numerical_tests.csv"))
  assert(all(bind(tests)$pass), "A local grid test failed. No API query was made.")
  if (toupper(Sys.getenv("COFFEE_POWER_GRID_TEST_ONLY", "FALSE")) == "TRUE") {
    message("Grid tests passed. Test-only mode: no inputs read and no requests made.")
    return(invisible(bind(tests)))
  }

  report <- list(execution_status = "INCOMPLETE", script_version = "15_source_grid_plan_v1",
    source = "NASA POWER", municipal_targets = 0, discovery_requests_planned = 6,
    discovery_requests_validated = 0, new_http_attempts = 0, cached_discovery_requests = 0,
    source_grids_inspected = 0, unique_meteorology_cells = NA,
    unique_srb_cells = NA, unique_ceres_cells = NA, planned_historical_requests = NA,
    equivalent_point_by_source_requests = NA, request_reduction_pct = NA,
    maximum_weight_sum_error = NA, grid_checks_passed = FALSE,
    municipal_weather_values_assigned = FALSE, full_history_download_started = FALSE,
    national_mean_calculated = FALSE, climate_attribution_performed = FALSE,
    farm_locations_measured = FALSE, historical_boundaries_harmonized = FALSE,
    original_inputs_modified = FALSE, last_stage = "Reading input contracts", error_if_incomplete = "None")
  request_log <- list()
  save_progress <- function() {
    write_csv(data.frame(check = names(report),
      result = vapply(report, as.character, character(1))),
      file.path(out_dir, "coffee_power_spatial_expansion_summary.csv"))
    if (length(request_log)) write_csv(bind(request_log), file.path(out_dir, "coffee_power_grid_requests.csv"))
  }
  on.exit({
    try(save_progress(), silent = TRUE)
    try(writeLines(capture.output(utils::sessionInfo()), file.path(out_dir, "sessionInfo_step15.txt")), silent = TRUE)
  }, add = TRUE)

  last_request_end <- as.POSIXct("1970-01-01", tz = "UTC")
  get_raw <- function(url, label) {
    dest <- file.path(raw_dir, paste0(label, "_", url_hash(url), ".json"))
    cached <- file.exists(dest)
    if (cached) {
      side <- paste0(dest, ".url.txt")
      assert(file.exists(side) && identical(paste(readLines(side, warn = FALSE), collapse = ""), url),
             paste("Cached request provenance does not match:", dest))
      report$cached_discovery_requests <<- report$cached_discovery_requests + 1L
    } else {
      for (attempt in 1:3) {
        wait <- 2 - as.numeric(difftime(Sys.time(), last_request_end, units = "secs"))
        if (wait > 0) Sys.sleep(wait)
        report$new_http_attempts <<- report$new_http_attempts + 1L
        response <- tryCatch(httr::GET(url, httr::timeout(240),
          httr::user_agent("coffee-climate-research/1.0 (source-grid discovery)"),
          httr::add_headers(Accept = "application/json")), error = function(e) e)
        last_request_end <<- Sys.time()
        if (inherits(response, "error")) {
          if (attempt == 3L) stop(conditionMessage(response), call. = FALSE)
          Sys.sleep(5 * 2^(attempt - 1L)); next
        }
        status <- httr::status_code(response)
        if (status == 429L) stop_kind(paste("NASA returned HTTP 429. No retry loop or bypass is used.",
          "Retry-After:", scalar(httr::headers(response)[["retry-after"]])), "power15_rate_limit")
        if (status %in% c(401L, 403L)) stop_kind(paste("NASA HTTP", status, "access stop."), "power15_access")
        body <- httr::content(response, as = "text", encoding = "UTF-8")
        if (status >= 500L && attempt < 3L) { Sys.sleep(5 * 2^(attempt - 1L)); next }
        assert(status == 200L, paste("NASA HTTP", status, substr(body, 1, 700)))
        assert(grepl("^[[:space:]]*\\{", body), "Non-JSON response; not saved as weather.")
        tmp <- tempfile("discovery_", tmpdir = raw_dir, fileext = ".json")
        writeLines(body, tmp, useBytes = TRUE)
        assert(file.copy(tmp, dest, overwrite = FALSE), paste("Cannot save raw file:", dest))
        unlink(tmp)
        writeLines(url, paste0(dest, ".url.txt"), useBytes = TRUE)
        break
      }
    }
    request_log[[length(request_log) + 1L]] <<- data.frame(label = label, url = url,
      raw_file = dest, raw_md5 = hash(dest), cached = cached, parse_status = "PENDING")
    save_progress()
    list(path = dest, row = length(request_log))
  }

  parse_regional <- function(path, def, anchor, url) {
    obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    assert(identical(obj$type, "FeatureCollection") && is.list(obj$features) && length(obj$features) > 3L,
      paste("Regional JSON schema needs review. Expected FeatureCollection with point features. Raw file:", path))
    feature_headers <- lapply(obj$features, function(f) f$header)
    headers <- c(list(obj$header), feature_headers)
    sources <- scalar(lapply(headers, function(h) h$sources))
    times <- unique(toupper(as.character(unlist(lapply(headers, function(h) h$time_standard), use.names = FALSE))))
    # If the API omits a time-standard header, the exact UTC URL remains
    # recorded, but this is flagged separately rather than invented.
    if (length(times)) assert(all(times == "UTC"), "Regional response reports non-UTC data.")
    source_norm <- toupper(gsub("[^A-Z0-9]", "", toupper(sources)))
    seen_met <- grepl("MERRA", source_norm)
    seen_srb <- grepl("SRB", source_norm)
    seen_ceres <- grepl("CERES|SYN1DEG", source_norm)
    seen_recent <- grepl("GEOS|FLASH", source_norm)
    recognized <- switch(def$family,
      MET = seen_met && !seen_srb && !seen_ceres && !seen_recent,
      SRB = seen_srb && !seen_met && !seen_ceres && !seen_recent,
      CERES = seen_ceres && !seen_met && !seen_srb && !seen_recent, FALSE)
    key <- gsub("-", "", def$probe_date, fixed = TRUE)
    entries <- lapply(seq_along(obj$features), function(i) {
      f <- obj$features[[i]]
      assert(identical(f$geometry$type, "Point"), "A regional feature is not a point.")
      coord <- number(unlist(f$geometry$coordinates, use.names = FALSE), "regional coordinates", TRUE)
      assert(length(coord) >= 2L && all(is.finite(coord[1:2])), "Invalid regional point coordinates.")
      series <- f$properties$parameter[[def$probe_parameter]]
      assert(is.list(series) && !is.null(names(series)) && identical(names(series), key),
             "Regional response dates/parameter differ from the one-day request.")
      value <- unlist(series[[key]], use.names = FALSE)
      assert(length(value) == 1L && is.numeric(value), "Non-numeric grid probe value.")
      data.frame(family = def$family, anchor = anchor, longitude = coord[1L], latitude = coord[2L],
        probe_date = def$probe_date, parameter = def$probe_parameter, value_raw = value,
        response_elevation_m = if (length(coord) >= 3L) coord[3L] else NA_real_)
    })
    points <- bind(entries)
    assert(!anyDuplicated(paste(points$longitude, points$latitude)), "Duplicate regional coordinates.")
    lon <- sort(unique(round(points$longitude, 6)))
    lat <- sort(unique(round(points$latitude, 6)))
    assert(length(lon) >= 3L && length(lat) >= 3L, "Too few coordinates to inspect the regional grid.")
    dx <- stats::median(diff(lon)); dy <- stats::median(diff(lat))
    regular <- max(abs(diff(lon) - dx)) < 1e-5 && max(abs(diff(lat) - dy)) < 1e-5
    rectangular <- nrow(points) == length(lon) * length(lat)
    native_match <- abs(dx - def$native_lon_step) < 1e-5 && abs(dy - def$native_lat_step) < 1e-5
    registration <- function(first, step) {
      x <- first - floor(first / step) * step
      if (abs(x) < 1e-6 || abs(x - step) < 1e-6) 0 else round(x, 6)
    }
    metadata <- data.frame(family = def$family, anchor = anchor, parameter = def$probe_parameter,
      probe_date = def$probe_date, sources_reported = sources, source_recognized = recognized,
      time_standard_reported = if (length(times)) paste(times, collapse = "|") else "NOT_REPORTED_QUERY_REQUESTED_UTC",
      api_version = scalar(obj$header$api$version), n_points = nrow(points),
      longitude_step = dx, latitude_step = dy,
      longitude_registration = registration(lon[1L], dx), latitude_registration = registration(lat[1L], dy),
      regular_grid = regular, rectangular_grid = rectangular,
      expected_native_longitude_step = def$native_lon_step, expected_native_latitude_step = def$native_lat_step,
      matches_documented_native_spacing = native_match, raw_file = path, raw_md5 = hash(path), url = url)
    list(points = points, metadata = metadata)
  }

  worker <- function() {
    paths <- c(
      targets = file.path(base_dir, "02_data_processed", "climate", "coffee_climate_target_registry.csv"),
      weights = file.path(base_dir, "02_data_processed", "climate", "coffee_climate_municipal_weights.csv"),
      pilot = file.path(base_dir, "04_outputs", "climate_pilot", "coffee_climate_pilot_sites.csv"),
      pilot_synthesis = file.path(base_dir, "04_outputs", "climate_pilot_synthesis", "coffee_climate_pilot_synthesis_summary.csv")
    )
    assert(all(file.exists(paths)), paste("Missing input(s):", paste(paths[!file.exists(paths)], collapse = "\n")))
    provenance <- data.frame(role = names(paths), file = unname(paths), md5 = vapply(paths, hash, character(1)))
    write_csv(provenance, file.path(out_dir, "coffee_power_spatial_expansion_provenance.csv"))
    s14 <- read_csv(paths[["pilot_synthesis"]])
    assert(checked(s14, "execution_status") == "COMPLETED_PILOT_SYNTHESIS" &&
             checked(s14, "numerical_tests_passed") == "TRUE", "Step14 pilot synthesis has not passed.")
    targets <- read_csv(paths[["targets"]]); weights <- read_csv(paths[["weights"]]); pilot <- read_csv(paths[["pilot"]])
    need(targets, c("municipality_code", "municipality", "longitude", "latitude"), "Climate target registry")
    need(weights, c("municipality_code", "species", "year", "production_t", "area_harvested_ha",
                   "baseline_area_weight", "dynamic_area_weight", "baseline_production_weight", "dynamic_production_weight"), "Municipal weights")
    need(pilot, c("municipality_code", "longitude", "latitude"), "Pilot sites")
    assert(!anyDuplicated(targets$municipality_code) && !anyNA(targets$municipality_code) &&
             all(grepl("^[0-9]{7}$", targets$municipality_code)), "Invalid municipal target keys.")
    for (v in c("longitude", "latitude")) {
      targets[[v]] <- number(targets[[v]], v); pilot[[v]] <- number(pilot[[v]], v)
    }
    assert(all(targets$longitude >= -75 & targets$longitude <= -30 & targets$latitude >= -35 & targets$latitude <= 6),
           "Target coordinates outside the broad Brazil envelope.")
    wcols <- c("baseline_area_weight", "dynamic_area_weight", "baseline_production_weight", "dynamic_production_weight")
    for (v in c("year", "production_t", "area_harvested_ha", wcols)) weights[[v]] <- number(weights[[v]], v)
    assert(setequal(unique(weights$municipality_code), targets$municipality_code), "Weights and target codes differ.")
    assert(!anyDuplicated(paste(weights$municipality_code, weights$species, weights$year)), "Duplicate municipal weights.")
    assert(setequal(unique(weights$year), 2012:2024) &&
           setequal(unique(weights$species), c("Total", "Arabica", "Canephora")) &&
           nrow(weights) == nrow(targets) * 13L * 3L, "Incomplete weight-key coverage.")
    for (v in wcols) {
      assert(all(weights[[v]] >= 0), paste("Negative", v))
      sums <- tapply(weights[[v]], list(weights$species, weights$year), sum)
      assert(all(is.finite(sums)) && max(abs(sums - 1)) < 1e-8, paste(v, "does not sum to one."))
    }
    report$municipal_targets <<- nrow(targets)
    for (pkg in c("httr", "jsonlite")) {
      if (!requireNamespace(pkg, quietly = TRUE)) utils::install.packages(pkg, repos = "https://cloud.r-project.org")
      assert(requireNamespace(pkg, quietly = TRUE), paste("Package unavailable:", pkg))
    }

    # Use two existing pilot anchors, not newly inferred farm locations.
    anchor_codes <- c("3148103", "1100320")
    j <- match(anchor_codes, pilot$municipality_code)
    assert(!anyNA(j), "The original two pilot anchors are missing.")
    anchors <- pilot[j, , drop = FALSE]
    catalogue <- data.frame(family = c("MET", "SRB", "CERES"),
      probe_parameter = c("T2M", "ALLSKY_SFC_SW_DWN", "ALLSKY_SFC_SW_DWN"),
      probe_date = c("2020-07-01", "1990-07-01", "2020-07-01"),
      start = c("1981-01-01", "1984-01-01", "2001-01-01"),
      end = c("2024-12-31", "2000-12-31", "2024-12-31"),
      parameters = c("T2M,T2M_MIN,T2M_MAX,T2MDEW,RH2M,PRECTOTCORR,WS2M,PS", "ALLSKY_SFC_SW_DWN", "ALLSKY_SFC_SW_DWN"),
      native_lon_step = c(0.625, 1, 1), native_lat_step = c(0.5, 1, 1),
      stringsAsFactors = FALSE)
    points_parts <- metadata_parts <- list()
    for (f in seq_len(nrow(catalogue))) {
      def <- catalogue[f, , drop = FALSE]
      for (i in seq_len(nrow(anchors))) {
        a <- anchors[i, , drop = FALSE]
        # A small 3-by-3-degree region, well below broad API limits.
        cx <- floor(a$longitude) + 0.5; cy <- floor(a$latitude) + 0.5
        args <- c(`latitude-min` = sprintf("%.6f", cy - 1.5), `latitude-max` = sprintf("%.6f", cy + 1.5),
          `longitude-min` = sprintf("%.6f", cx - 1.5), `longitude-max` = sprintf("%.6f", cx + 1.5),
          parameters = def$probe_parameter, community = "AG", start = gsub("-", "", def$probe_date, fixed = TRUE),
          end = gsub("-", "", def$probe_date, fixed = TRUE), format = "JSON", `time-standard` = "UTC")
        url <- make_url("https://power.larc.nasa.gov/api/temporal/daily/regional", args)
        label <- paste0("GRID_", def$family, "_", a$municipality_code)
        report$last_stage <<- label
        message("Inspecting regional grid: ", def$family, " | anchor ", a$municipality_code, " | one date")
        raw <- get_raw(url, label)
        parsed <- parse_regional(raw$path, def, a$municipality_code, url)
        points_parts[[length(points_parts) + 1L]] <- parsed$points
        metadata_parts[[length(metadata_parts) + 1L]] <- parsed$metadata
        request_log[[raw$row]]$parse_status <<- "PARSED"
        report$discovery_requests_validated <<- report$discovery_requests_validated + 1L
        write_csv(bind(metadata_parts), file.path(out_dir, "coffee_power_grid_discovery.csv"))
        write_csv(bind(points_parts), file.path(out_dir, "coffee_power_grid_probe_points.csv"))
        save_progress()
      }
    }
    meta <- bind(metadata_parts)
    definitions <- bind(lapply(catalogue$family, function(fam) {
      q <- meta[meta$family == fam, , drop = FALSE]
      same <- nrow(q) == 2L && abs(diff(q$longitude_step)) < 1e-5 && abs(diff(q$latitude_step)) < 1e-5 &&
        abs(diff(q$longitude_registration)) < 1e-5 && abs(diff(q$latitude_registration)) < 1e-5
      valid <- same && all(q$regular_grid & q$rectangular_grid & q$source_recognized & q$matches_documented_native_spacing)
      data.frame(family = fam, longitude_step = q$longitude_step[1L], latitude_step = q$latitude_step[1L],
        longitude_registration = q$longitude_registration[1L], latitude_registration = q$latitude_registration[1L],
        two_anchors_agree = same, grid_checks_passed = valid,
        sampling_rule = "Nearest axis on inspected regular source grid; exact ties lower and flagged",
        grid_geometry_verified_at_all_targets = FALSE,
        source_note = "Native-spacing cross-check plus two regional coordinate inspections; no farm-scale validation")
    }))
    report$source_grids_inspected <<- nrow(definitions)
    write_csv(definitions, file.path(out_dir, "coffee_power_source_grid_definitions.csv"))
    write_csv(definitions, file.path(plan_dir, "coffee_power_source_grid_definitions.csv"))
    if (!all(definitions$grid_checks_passed)) {
      report$execution_status <<- "GRID_REVIEW_REQUIRED_NO_HISTORY_DOWNLOAD"
      report$last_stage <<- "Actual regional grid differs from a checked source assumption; no municipality-to-cell assignment made"
      report$error_if_incomplete <<- "Review coffee_power_grid_discovery.csv and raw regional responses. No fallback grid guessed."
      save_progress()
      message("\nGRID_REVIEW_REQUIRED_NO_HISTORY_DOWNLOAD\nGrid metadata saved in: ", out_dir)
      return(invisible(NULL))
    }
    report$grid_checks_passed <<- TRUE
    report$last_stage <<- "Preparing municipality-to-grid sampling plan"

    crosswalk <- bind(lapply(seq_len(nrow(definitions)), function(f) {
      def <- definitions[f, , drop = FALSE]
      x <- near_axis(targets$longitude, def$longitude_registration, def$longitude_step)
      y <- near_axis(targets$latitude, def$latitude_registration, def$latitude_step)
      data.frame(municipality_code = targets$municipality_code, municipality = targets$municipality,
        family = def$family, requested_municipal_longitude = targets$longitude,
        requested_municipal_latitude = targets$latitude,
        cell_id = cell_id(def$family, x$coordinate, y$coordinate),
        sampling_longitude = round(x$coordinate, 6), sampling_latitude = round(y$coordinate, 6),
        point_to_sampling_center_km = haversine(targets$longitude, targets$latitude, x$coordinate, y$coordinate),
        longitude_tie = x$tie, latitude_tie = y$tie, grid_checks_passed = TRUE,
        climate_representation = "Source-grid sample assigned to a municipal reference point, not polygon mean",
        coffee_field_coordinates = FALSE, stringsAsFactors = FALSE)
    }))
    assert(!anyDuplicated(paste(crosswalk$family, crosswalk$municipality_code)), "Duplicate crosswalk keys.")
    write_csv(crosswalk, file.path(plan_dir, "coffee_municipality_power_grid_crosswalk.csv"))
    write_rds(crosswalk, file.path(plan_dir, "coffee_municipality_power_grid_crosswalk.rds"))
    cell_parts <- split(crosswalk, crosswalk$cell_id)
    cells <- bind(lapply(cell_parts, function(z) data.frame(
      cell_id = z$cell_id[1L], family = z$family[1L], longitude = z$sampling_longitude[1L],
      latitude = z$sampling_latitude[1L], linked_municipalities = nrow(z),
      maximum_point_offset_km = max(z$point_to_sampling_center_km),
      any_exact_axis_tie = any(z$longitude_tie | z$latitude_tie))))
    cells <- cells[order(match(cells$family, catalogue$family), cells$cell_id), , drop = FALSE]
    write_csv(cells, file.path(plan_dir, "coffee_power_unique_sampling_cells.csv"))

    weighted <- weight_checks <- list()
    for (fam in catalogue$family) {
      cr <- crosswalk[crosswalk$family == fam, , drop = FALSE]
      k <- match(weights$municipality_code, cr$municipality_code)
      assert(!anyNA(k), "A weight has no source-grid match.")
      by <- list(cell_id = cr$cell_id[k], species = weights$species, year = weights$year)
      z <- stats::aggregate(weights[c("production_t", "area_harvested_ha", wcols)], by, sum)
      n <- stats::aggregate(rep(1L, nrow(weights)), by, sum)
      z$municipal_codes <- n$x[match(paste(z$cell_id, z$species, z$year), paste(n$cell_id, n$species, n$year))]
      z$family <- fam
      weighted[[fam]] <- z
      for (sp in c("Total", "Arabica", "Canephora")) for (yr in 2012:2024) {
        p <- z[z$species == sp & z$year == yr, , drop = FALSE]
        sums <- vapply(wcols, function(v) sum(p[[v]]), numeric(1))
        weight_checks[[length(weight_checks) + 1L]] <- data.frame(family = fam, species = sp, year = yr,
          baseline_area_weight_sum = sums[1L], dynamic_area_weight_sum = sums[2L],
          baseline_production_weight_sum = sums[3L], dynamic_production_weight_sum = sums[4L],
          max_absolute_error = max(abs(sums - 1)), verified = max(abs(sums - 1)) < 1e-8)
      }
    }
    wc <- bind(weight_checks)
    assert(all(wc$verified), "Source-grid aggregation did not conserve all weights.")
    report$maximum_weight_sum_error <<- max(wc$max_absolute_error)
    write_csv(bind(weighted), file.path(plan_dir, "coffee_power_source_cell_weights.csv"))
    write_rds(bind(weighted), file.path(plan_dir, "coffee_power_source_cell_weights.rds"))
    write_csv(wc, file.path(out_dir, "coffee_power_grid_weight_checks.csv"))

    jobs <- bind(lapply(seq_len(nrow(cells)), function(i) {
      cell <- cells[i, , drop = FALSE]; def <- catalogue[catalogue$family == cell$family, , drop = FALSE]
      args <- c(parameters = def$parameters, community = "AG", longitude = sprintf("%.7f", cell$longitude),
        latitude = sprintf("%.7f", cell$latitude), start = gsub("-", "", def$start, fixed = TRUE),
        end = gsub("-", "", def$end, fixed = TRUE), format = "JSON", `time-standard` = "UTC")
      data.frame(cell_id = cell$cell_id, family = cell$family, longitude = cell$longitude, latitude = cell$latitude,
        start = def$start, end = def$end, parameters = def$parameters, community = "AG", time_standard = "UTC",
        linked_municipalities = cell$linked_municipalities,
        expected_daily_rows = length(seq(as.Date(def$start), as.Date(def$end), by = "day")),
        url = make_url("https://power.larc.nasa.gov/api/temporal/daily/point", args),
        request_status = "PLANNED_NOT_EXECUTED")
    }))
    assert(!anyDuplicated(jobs$url), "Historical plan contains duplicate queries.")
    write_csv(jobs, file.path(plan_dir, "coffee_power_brazil_request_plan.csv"))
    write_rds(jobs, file.path(plan_dir, "coffee_power_brazil_request_plan.rds"))

    # Existing pilot files are inventoried, never moved, overwritten or
    # assigned to other municipalities without subsequent revalidation.
    req_path <- file.path(base_dir, "04_outputs", "climate_nasa_power", "coffee_power_requests.csv")
    if (file.exists(req_path)) {
      old <- read_csv(req_path)
      need(old, c("municipality_code", "group", "raw_file", "raw_md5"), "Pilot request manifest")
      fam <- c(meteorology = "MET", solar_1984_2000 = "SRB", solar_2001_2024 = "CERES")[old$group]
      kk <- match(paste(fam, old$municipality_code), paste(crosswalk$family, crosswalk$municipality_code))
      candidates <- data.frame(municipality_code = old$municipality_code, family = unname(fam),
        candidate_cell_id = crosswalk$cell_id[kk], raw_file = old$raw_file, raw_md5 = old$raw_md5,
        raw_file_exists = file.exists(old$raw_file), reuse_verified = FALSE,
        status = "CANDIDATE_ONLY_REVALIDATE_QUERY_AND_SPATIAL_EQUIVALENCE_BEFORE_REUSE")
      write_csv(candidates, file.path(out_dir, "coffee_power_pilot_cache_candidates.csv"))
    }
    counts <- bind(lapply(catalogue$family, function(fam) {
      q <- cells[cells$family == fam, , drop = FALSE]
      data.frame(family = fam, municipal_targets = nrow(targets), unique_sampling_cells = nrow(q),
        maximum_municipalities_sharing_cell = max(q$linked_municipalities),
        historical_requests_planned = sum(jobs$family == fam),
        maximum_reference_point_to_cell_center_km = max(q$maximum_point_offset_km))
    }))
    write_csv(counts, file.path(out_dir, "coffee_power_spatial_expansion_counts.csv"))
    write_csv(crosswalk[crosswalk$longitude_tie | crosswalk$latitude_tie, , drop = FALSE],
              file.path(out_dir, "coffee_power_grid_boundary_ties.csv"))
    notes <- c(
      "COFFE-STUDY | STEP15 | NASA POWER SPATIAL EXPANSION PLAN",
      paste("Executed:", format(Sys.time(), usetz = TRUE)),
      "No full historical acquisition starts in this step. Six small one-date regional queries inspect grids.",
      "Each source is inspected near Patrocinio and Sao Miguel do Guapore; source metadata and raw responses are retained.",
      "Grid spacing and registration are measured from regional coordinates, then checked for agreement across the two windows.",
      "The measured regular lattice is extended to all target points as an explicit spatial sampling assumption.",
      "Meteorology, SRB and CERES have separate source-grid identifiers and separate request plans.",
      "Unexpected/unknown/irregular regional grids are saved for review, with no fallback lattice silently invented.",
      "Nearest-axis lookup is used for rectangular grids. Exact ties select the lower coordinate and are listed.",
      "A source-grid sample is not a field measurement or a municipal polygon/crop-area average.",
      "A regionally inspected grid does not validate every point API's spatial processing or cell identity worldwide.",
      "The same municipal target set and original municipal area/production weights from step11 are retained.",
      "Both fixed baseline and dynamic weights sum to one within each year/species/source, including zero weights.",
      "Do not add Total to Arabica+Canephora. Do not add weights across meteorology/SRB/CERES source tables.",
      "Source-specific weight tables are a bookkeeping plan, not climate averages or independent observations.",
      "Derived nonlinear indices must be calculated at the appropriate source/municipal combination before aggregation.",
      "Cloud and meteorology grids can differ. ET0 must not be computed by blindly averaging already-aggregated inputs.",
      "Existing pilot raw files are potential cache candidates only; no cross-site reuse occurs in this script.",
      "Existing NASA pilot and Open-Meteo files, production data and all previous figures remain untouched.",
      "Solar dates: SRB 1984-2000 and CERES 2001-2024. No radiation fill before 1984.",
      "No climate/ET0 trend crossing solar sources, national climate estimate, attribution or altitude effect is estimated.",
      "Municipal code panels do not ensure constant historical municipal boundaries.",
      "Regional discovery uses one parameter and small windows, with sequential requests. HTTP 429/401/403 stop the run.",
      "https://power.larc.nasa.gov/docs/services/api/temporal/daily/",
      "https://power.larc.nasa.gov/docs/methodology/data/sources/",
      "https://power.larc.nasa.gov/docs/faqs/data/"
    )
    writeLines(notes, file.path(out_dir, "coffee_power_spatial_expansion_method_notes.txt"))
    assert(all(vapply(paths, hash, character(1)) == provenance$md5), "An input changed while planning. Review before proceeding.")
    report$unique_meteorology_cells <<- sum(cells$family == "MET")
    report$unique_srb_cells <<- sum(cells$family == "SRB")
    report$unique_ceres_cells <<- sum(cells$family == "CERES")
    report$planned_historical_requests <<- nrow(jobs)
    report$equivalent_point_by_source_requests <<- nrow(targets) * 3L
    report$request_reduction_pct <<- 100 * (1 - nrow(jobs) / (nrow(targets) * 3L))
    report$execution_status <<- "COMPLETED_SPATIAL_EXPANSION_PLAN"
    report$last_stage <<- "Unique source-grid plan and conserved weights saved. Historical requests not launched."
    save_progress()
    print(counts, row.names = FALSE)
    message("\nCOMPLETED_SPATIAL_EXPANSION_PLAN\nReports: ", out_dir,
            "\nPlan: ", plan_dir, "\nFull historical download has NOT started.")
  }
  tryCatch(worker(), error = function(e) {
    report$error_if_incomplete <<- substr(conditionMessage(e), 1, 1400)
    report$execution_status <<- if (inherits(e, "power15_rate_limit")) "PAUSED_RATE_LIMIT" else
      if (inherits(e, "power15_access")) "STOPPED_ACCESS_BLOCK" else "INCOMPLETE"
    try(save_progress(), silent = TRUE)
    stop(conditionMessage(e), "\nAll original data and successful grid responses were retained.\nSummary: ",
         file.path(out_dir, "coffee_power_spatial_expansion_summary.csv"), call. = FALSE)
  }, interrupt = function(e) {
    report$execution_status <<- "INTERRUPTED_RESUMABLE"
    report$error_if_incomplete <<- "Interrupted. Rerun to reuse completed grid inspections."
    try(save_progress(), silent = TRUE)
    message("Interrupted; successful grid responses were preserved.")
    invisible(NULL)
  })
}

coffee_power_step15()
