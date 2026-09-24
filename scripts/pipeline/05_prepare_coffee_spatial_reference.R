# ======================================================================
# COFFE-STUDY | 05_prepare_coffee_spatial_reference.R
# Municipal geography: official IBGE 2024 boundaries and reference points.
# Run the ENTIRE file with Source in RStudio.
#
# This step does NOT download production again, impute missing values,
# harmonize historical boundaries, estimate coffee-field elevation,
# estimate climate effects, or calculate geographic/altitudinal trends.
#
# Official source, checked on 2026-09-19:
# https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2024/Brasil/
# The national municipal ZIP is listed as approximately 199 MB.
# Geometric methods:
# https://r-spatial.github.io/sf/reference/geos_unary.html
# ======================================================================

main <- function() {
  base_dir <- Sys.getenv(
    "COFFEE_STUDY_DIR",
    unset = "."
  )
  reference_year <- 2024L
  processed_dir <- file.path(base_dir, "02_data_processed")
  spatial_dir <- file.path(processed_dir, "spatial")
  raw_geo_dir <- file.path(base_dir, "01_data_raw", "IBGE_geography", "2024")
  audit_dir <- file.path(base_dir, "04_outputs", "spatial_audit")
  for (p in c(spatial_dir, raw_geo_dir, audit_dir)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(p)) stop("Could not create directory: ", p, call. = FALSE)
  }
  old_options <- options(timeout = max(1800, getOption("timeout", 60)), scipen = 999)
  on.exit(options(old_options), add = TRUE)

  # Only sf is required in addition to base R. No tidyverse data masking.
  if (!requireNamespace("sf", quietly = TRUE)) {
    message("Installing the required spatial package: sf")
    utils::install.packages("sf", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package sf could not be installed. Restart R after installation.", call. = FALSE)
  }

  write_csv_safe <- function(x, path) {
    tmp <- tempfile("coffee_", tmpdir = dirname(path), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(x, tmp, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not save: ", path, call. = FALSE)
  }
  save_rds_safe <- function(x, path) {
    tmp <- tempfile("coffee_", tmpdir = dirname(path), fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(x, tmp, compress = "gzip")
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not save: ", path, call. = FALSE)
  }
  positive <- function(x) !is.na(x) & x > 0
  sum_available <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  ratio_pct <- function(a, b) if (is.finite(b) && b > 0) 100 * a / b else NA_real_

  # --------------------------------------------------------------------
  # 1. Read the audited master and the target list from step 04.
  # --------------------------------------------------------------------
  input_stub <- file.path(processed_dir, "coffee_brazil_municipality_audited_1974_2024")
  input_file <- paste0(input_stub, ".rds")
  if (file.exists(input_file)) {
    dat <- readRDS(input_file)
  } else if (file.exists(paste0(input_stub, ".csv"))) {
    input_file <- paste0(input_stub, ".csv")
    dat <- utils::read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE,
                           na.strings = c("NA", ""), fileEncoding = "UTF-8")
  } else {
    stop("Audited master not found. Run step 04 first. Expected:\n", input_file, call. = FALSE)
  }
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  required <- c("municipality_code", "municipality", "state", "year", "species",
                "area_harvested_ha", "production_t", "comparison_period")
  if (length(setdiff(required, names(dat)))) {
    stop("Missing input columns: ", paste(setdiff(required, names(dat)), collapse = ", "), call. = FALSE)
  }
  dat$municipality_code <- as.character(dat$municipality_code)
  if (!nrow(dat) || anyNA(dat$municipality_code) ||
      any(!grepl("^[0-9]{7}$", dat$municipality_code))) {
    stop("The master must contain valid seven-digit municipality codes.", call. = FALSE)
  }
  if (!is.numeric(dat$year) || anyNA(dat$year) ||
      !is.numeric(dat$production_t) || !is.numeric(dat$area_harvested_ha)) {
    stop("Invalid year, production or harvested-area columns.", call. = FALSE)
  }
  key <- paste(dat$municipality_code, dat$year, dat$species, sep = "|")
  if (anyDuplicated(key)) stop("Duplicated municipality/year/species keys.", call. = FALSE)
  if (anyNA(dat$species) || any(!dat$species %in% c("Total", "Arabica", "Canephora"))) {
    stop("Unexpected species labels in the audited master.", call. = FALSE)
  }
  target_file <- file.path(processed_dir, "coffee_municipality_spatial_targets.csv")
  if (!file.exists(target_file)) stop("Target list not found: ", target_file, call. = FALSE)
  targets <- utils::read.csv(target_file, stringsAsFactors = FALSE, check.names = FALSE,
                             colClasses = "character", na.strings = c("NA", ""),
                             fileEncoding = "UTF-8")
  if (!"municipality_code" %in% names(targets) || !nrow(targets) ||
      anyNA(targets$municipality_code) || anyDuplicated(targets$municipality_code)) {
    stop("Invalid or duplicated municipality codes in the target list.", call. = FALSE)
  }
  if (length(setdiff(targets$municipality_code, dat$municipality_code))) {
    stop("The target list and audited master contain incompatible codes.", call. = FALSE)
  }
  input_md5 <- unname(tools::md5sum(input_file))
  message("Input: ", nrow(dat), " records; ", nrow(targets), " spatial targets.")

  # --------------------------------------------------------------------
  # 2. Download and cache the official municipal boundaries, once.
  #    A different year or a different provider is never substituted.
  # --------------------------------------------------------------------
  source_url <- paste0(
    "https://geoftp.ibge.gov.br/organizacao_do_territorio/",
    "malhas_territoriais/malhas_municipais/municipio_2024/Brasil/",
    "BR_Municipios_2024.zip"
  )
  zip_file <- file.path(raw_geo_dir, "BR_Municipios_2024.zip")
  expected_shape <- "BR_Municipios_2024.shp"
  zip_inventory <- function(f) {
    if (!file.exists(f) || is.na(file.size(f)) || file.size(f) <= 0) return(NULL)
    z <- tryCatch(suppressWarnings(utils::unzip(f, list = TRUE)), error = function(e) NULL)
    if (is.null(z) || !nrow(z)) return(NULL)
    if (!any(tolower(basename(z$Name)) == tolower(expected_shape))) return(NULL)
    z
  }
  download_zip <- function() {
    if (!is.null(zip_inventory(zip_file))) {
      message("Using the existing validated ZIP: ", basename(zip_file))
      return(invisible(NULL))
    }
    if (file.exists(zip_file)) {
      backup <- paste0(zip_file, ".invalid_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      if (!file.rename(zip_file, backup)) stop("Cannot archive invalid ZIP: ", zip_file, call. = FALSE)
    }
    message("Downloading official IBGE municipal boundaries (approximately 199 MB).")
    message("No production data will be downloaded again.")
    tmp <- tempfile("ibge_download_", tmpdir = raw_geo_dir, fileext = ".zip")
    on.exit(unlink(tmp), add = TRUE)
    last_error <- "unknown download error"
    for (attempt in seq_len(3L)) {
      if (file.exists(tmp)) unlink(tmp)
      ok <- tryCatch({
        status <- utils::download.file(source_url, tmp, mode = "wb", method = "libcurl", quiet = FALSE)
        if (!identical(as.integer(status), 0L) || is.null(zip_inventory(tmp))) {
          stop("The response is not a valid IBGE municipal ZIP.")
        }
        TRUE
      }, error = function(e) {
        last_error <<- substr(conditionMessage(e), 1L, 400L)
        FALSE
      })
      if (ok) {
        if (!file.copy(tmp, zip_file, overwrite = TRUE)) stop("Could not save downloaded ZIP.", call. = FALSE)
        return(invisible(NULL))
      }
      message("Download attempt ", attempt, " failed: ", last_error)
      if (attempt < 3L) Sys.sleep(5 * attempt)
    }
    stop("Boundary download failed. Existing production data are unchanged.\n",
         "Rerun this script when the public IBGE server is accessible.\n", last_error,
         call. = FALSE)
  }
  download_zip()
  z <- zip_inventory(zip_file)
  shp_member <- z$Name[tolower(basename(z$Name)) == tolower(expected_shape)]
  if (length(shp_member) != 1L) stop("Ambiguous shapefile inside the ZIP.", call. = FALSE)
  extract_dir <- file.path(raw_geo_dir, "extracted")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  # Check extracted file sizes against the ZIP inventory before reusing them.
  extracted <- file.path(extract_dir, z$Name)
  regular <- !grepl("/$", z$Name)
  sizes <- file.info(extracted[regular])$size
  if (length(sizes) != sum(regular) || anyNA(sizes) || any(sizes != z$Length[regular])) {
    message("Extracting the official boundary files...")
    utils::unzip(zip_file, exdir = extract_dir, overwrite = TRUE)
  }
  shp_file <- file.path(extract_dir, shp_member)
  sidecars <- paste0(sub("\\.shp$", "", shp_file, ignore.case = TRUE), c(".shp", ".shx", ".dbf", ".prj"))
  if (!all(file.exists(sidecars))) stop("Incomplete shapefile extraction.", call. = FALSE)
  zip_md5 <- unname(tools::md5sum(zip_file))
  message("Reading the 2024 municipal boundaries...")
  geo <- sf::st_read(shp_file, quiet = TRUE, stringsAsFactors = FALSE)
  code_col <- names(geo)[toupper(names(geo)) == "CD_MUN"]
  name_col <- names(geo)[toupper(names(geo)) == "NM_MUN"]
  if (length(code_col) != 1L || length(name_col) != 1L) {
    stop("Unexpected IBGE fields. Expected CD_MUN and NM_MUN; found: ",
         paste(names(geo), collapse = ", "), call. = FALSE)
  }
  if (is.na(sf::st_crs(geo))) stop("The shapefile has no coordinate reference system.", call. = FALSE)
  names(geo)[names(geo) == code_col] <- "municipality_code"
  names(geo)[names(geo) == name_col] <- "municipality_name_reference"
  geo$municipality_code <- as.character(geo$municipality_code)
  if (anyNA(geo$municipality_code) || anyDuplicated(geo$municipality_code)) {
    stop("Missing or duplicated codes in the reference boundaries. No automatic merging was done.", call. = FALSE)
  }
  geo <- geo[, c("municipality_code", "municipality_name_reference"), drop = FALSE]
  reference_inventory <- sf::st_drop_geometry(geo)
  reference_inventory$reference_boundary_year <- reference_year
  reference_inventory$present_in_coffee_master <- reference_inventory$municipality_code %in% dat$municipality_code
  write_csv_safe(reference_inventory, file.path(audit_dir, "coffee_reference_boundary_inventory.csv"))
  write_csv_safe(reference_inventory[!reference_inventory$present_in_coffee_master, , drop = FALSE],
                 file.path(audit_dir, "coffee_reference_codes_not_in_master.csv"))

  # Keep every input code in the registry. Unmatched codes remain unmatched.
  # Never use nearest neighbours or municipality names as replacement keys.
  geo <- geo[geo$municipality_code %in% dat$municipality_code, , drop = FALSE]
  if (!nrow(geo)) stop("No exact municipal-code matches with IBGE boundaries.", call. = FALSE)

  # --------------------------------------------------------------------
  # 3. Reference coordinates, calculated in a metric equal-area projection.
  #    Both full-polygon centroids and interior reference points are stored.
  #    Interior reference points are NOT coffee-field locations.
  # --------------------------------------------------------------------
  metric_crs <- "+proj=laea +lat_0=-15 +lon_0=-54 +datum=WGS84 +units=m +no_defs"
  message("Calculating municipal reference coordinates by state...")
  state_prefixes <- sort(unique(substr(geo$municipality_code, 1L, 2L)))
  coord_parts <- vector("list", length(state_prefixes))
  repair_parts <- vector("list", length(state_prefixes))
  for (i in seq_along(state_prefixes)) {
    state_id <- state_prefixes[i]
    ix <- which(substr(geo$municipality_code, 1L, 2L) == state_id)
    message("  State code ", state_id, ": ", length(ix), " reference polygons")
    projected <- sf::st_transform(sf::st_geometry(geo[ix, ]), metric_crs)
    before <- sf::st_is_valid(projected)
    needs_repair <- is.na(before) | !before
    if (any(needs_repair)) projected[needs_repair] <- sf::st_make_valid(projected[needs_repair])
    after <- sf::st_is_valid(projected)
    empty <- sf::st_is_empty(projected)
    types <- as.character(sf::st_geometry_type(projected))
    valid_polygon <- !is.na(after) & after & !empty & types %in% c("POLYGON", "MULTIPOLYGON")
    repairs <- data.frame(
      municipality_code = geo$municipality_code[ix],
      geometry_repaired = needs_repair,
      geometry_valid_after_check = valid_polygon,
      stringsAsFactors = FALSE
    )
    repair_parts[[i]] <- repairs
    if (any(!valid_polygon)) {
      write_csv_safe(do.call(rbind, repair_parts[seq_len(i)]),
                     file.path(audit_dir, "coffee_geometry_audit.csv"))
      stop("Non-polygonal, empty or invalid geometries remain. See coffee_geometry_audit.csv.", call. = FALSE)
    }
    if (any(needs_repair)) {
      g <- sf::st_geometry(geo)
      g[ix[needs_repair]] <- sf::st_transform(projected[needs_repair], sf::st_crs(geo))
      sf::st_geometry(geo) <- g
      rm(g)
    }
    centroids <- sf::st_centroid(projected)
    interior <- sf::st_point_on_surface(projected)
    xy <- sf::st_coordinates(centroids)
    ll_centroid <- sf::st_coordinates(sf::st_transform(centroids, 4326))
    ll_reference <- sf::st_coordinates(sf::st_transform(interior, 4326))
    inside <- sf::st_intersects(centroids, projected, sparse = TRUE)
    centroid_within <- vapply(seq_along(inside), function(j) j %in% inside[[j]], logical(1))
    if (nrow(ll_reference) != length(ix) || nrow(ll_centroid) != length(ix) ||
        any(!is.finite(ll_reference)) || any(!is.finite(ll_centroid))) {
      stop("Invalid coordinate results for state ", state_id, call. = FALSE)
    }
    if (any(ll_reference[, 1L] < -75 | ll_reference[, 1L] > -25 |
            ll_reference[, 2L] < -35 | ll_reference[, 2L] > 6)) {
      stop("Reference coordinates outside a broad Brazil bounding box.", call. = FALSE)
    }
    coord_parts[[i]] <- data.frame(
      municipality_code = geo$municipality_code[ix],
      municipality_name_reference = geo$municipality_name_reference[ix],
      reference_boundary_year = reference_year,
      municipality_centroid_longitude = ll_centroid[, 1L],
      municipality_centroid_latitude = ll_centroid[, 2L],
      municipality_centroid_x_laea_m = xy[, 1L],
      municipality_centroid_y_laea_m = xy[, 2L],
      municipality_reference_longitude = ll_reference[, 1L],
      municipality_reference_latitude = ll_reference[, 2L],
      centroid_within_municipality = centroid_within,
      municipality_area_reference_km2 = as.numeric(sf::st_area(projected)) / 1e6,
      geometry_repaired = needs_repair,
      stringsAsFactors = FALSE
    )
    rm(projected, centroids, interior)
  }
  coordinates <- do.call(rbind, coord_parts)
  rownames(coordinates) <- NULL
  write_csv_safe(do.call(rbind, repair_parts), file.path(audit_dir, "coffee_geometry_audit.csv"))

  # --------------------------------------------------------------------
  # 4. Exact-code registry and non-destructive geographic joins.
  # --------------------------------------------------------------------
  latest <- order(dat$municipality_code, -dat$year)
  latest <- latest[!duplicated(dat$municipality_code[latest])]
  registry <- dat[latest, c("municipality_code", "municipality", "state"), drop = FALSE]
  names(registry)[names(registry) == "municipality"] <- "municipality_label_in_master"
  m <- match(registry$municipality_code, coordinates$municipality_code)
  added_names <- setdiff(names(coordinates), "municipality_code")
  registry <- cbind(registry, coordinates[m, added_names, drop = FALSE])
  registry$spatial_match <- !is.na(m)
  registry$spatial_target_from_step04 <- registry$municipality_code %in% targets$municipality_code
  registry$historical_boundaries_harmonized <- FALSE
  registry$coffee_field_coordinates_available <- FALSE
  registry$elevation_estimated <- FALSE
  rownames(registry) <- NULL
  write_csv_safe(registry, file.path(spatial_dir, "coffee_municipality_spatial_registry.csv"))
  write_csv_safe(registry[!registry$spatial_match, , drop = FALSE],
                 file.path(audit_dir, "coffee_unmatched_municipality_codes.csv"))
  target_idx <- match(targets$municipality_code, registry$municipality_code)
  target_extra <- setdiff(names(registry), c("municipality_code", names(targets)))
  targets_geo <- cbind(targets, registry[target_idx, target_extra, drop = FALSE])
  rownames(targets_geo) <- NULL
  write_csv_safe(targets_geo, file.path(spatial_dir, "coffee_spatial_targets_georeferenced.csv"))

  geo$reference_boundary_year <- reference_year
  geo$historical_boundaries_harmonized <- FALSE
  geo$coffee_field_geometry <- FALSE
  attr(geo, "source_url") <- source_url
  attr(geo, "source_zip_md5") <- zip_md5
  attr(geo, "coordinate_methods") <- c("Centroid of complete municipal polygon",
                                       "Point on municipal polygon surface", metric_crs)
  save_rds_safe(geo, file.path(spatial_dir, "coffee_municipality_geometries_2024.rds"))

  keep_geo <- c(added_names, "spatial_match", "historical_boundaries_harmonized",
                "coffee_field_coordinates_available", "elevation_estimated")
  # All original data columns and their order are retained.
  append_geography <- function(x) {
    new_cols <- setdiff(keep_geo, names(x))
    mm <- match(x$municipality_code, registry$municipality_code)
    result <- cbind(x, registry[mm, new_cols, drop = FALSE])
    rownames(result) <- NULL
    if (nrow(result) != nrow(x) ||
        !isTRUE(all.equal(result[, names(x), drop = FALSE], x, check.attributes = FALSE))) {
      stop("Geographic join altered original data. Outputs blocked.", call. = FALSE)
    }
    result
  }
  total <- dat[dat$species == "Total" & dat$year >= 2002 & dat$year <= 2024, , drop = FALSE]
  species <- dat[dat$species %in% c("Arabica", "Canephora") & dat$year >= 2012 & dat$year <= 2024, , drop = FALSE]
  save_rds_safe(append_geography(total),
                file.path(spatial_dir, "coffee_brazil_total_georeferenced_2002_2024.rds"))
  save_rds_safe(append_geography(species),
                file.path(spatial_dir, "coffee_brazil_species_georeferenced_2012_2024.rds"))

  # --------------------------------------------------------------------
  # 5. Report spatial coverage without treating missing measurements as 0.
  #    These sums are input-coverage diagnostics, NOT validated national totals.
  # --------------------------------------------------------------------
  matched <- registry$spatial_match[match(dat$municipality_code, registry$municipality_code)]
  groups <- split(seq_len(nrow(dat)), paste(dat$year, dat$species, sep = "|"))
  coverage <- do.call(rbind, lapply(groups, function(j) {
    x <- dat[j, , drop = FALSE]
    ok <- matched[j]
    p <- sum_available(x$production_t)
    a <- sum_available(x$area_harvested_ha)
    pm <- sum_available(x$production_t[ok])
    am <- sum_available(x$area_harvested_ha[ok])
    data.frame(
      year = x$year[1L], species = x$species[1L], records = nrow(x),
      matched_records = sum(ok), unmatched_records = sum(!ok),
      positive_production_unmatched = sum(positive(x$production_t) & !ok),
      positive_harvested_area_unmatched = sum(positive(x$area_harvested_ha) & !ok),
      production_sum_available = p, harvested_area_sum_available = a,
      production_spatially_matched_pct = ratio_pct(pm, p),
      harvested_area_spatially_matched_pct = ratio_pct(am, a),
      missing_production_records = sum(is.na(x$production_t)),
      reference_boundary_year = reference_year,
      historical_boundaries_harmonized = FALSE,
      official_national_total_verified = FALSE,
      stringsAsFactors = FALSE
    )
  }))
  rownames(coverage) <- NULL
  coverage <- coverage[order(coverage$year, match(coverage$species, c("Total", "Arabica", "Canephora"))), ]
  write_csv_safe(coverage, file.path(audit_dir, "coffee_spatial_coverage_by_year_species.csv"))
  unmatched_targets <- sum(!targets_geo$spatial_match)
  summary <- data.frame(
    check = c("input_records", "input_municipality_codes", "spatial_targets_step04",
              "reference_boundary_year", "reference_features_total", "input_codes_matched",
              "input_codes_unmatched", "spatial_targets_matched", "spatial_targets_unmatched",
              "reference_codes_not_in_master", "geometries_repaired",
              "historical_boundaries_harmonized", "coffee_field_coordinates_estimated",
              "coffee_field_elevation_estimated", "original_measurements_changed",
              "official_national_totals_verified", "input_file", "input_md5", "source_zip_md5"),
    result = as.character(c(nrow(dat), nrow(registry), nrow(targets), reference_year,
                            nrow(reference_inventory), sum(registry$spatial_match),
                            sum(!registry$spatial_match), sum(targets_geo$spatial_match),
                            unmatched_targets, sum(!reference_inventory$present_in_coffee_master),
                            sum(coordinates$geometry_repaired), FALSE, FALSE, FALSE, FALSE,
                            FALSE, input_file, input_md5, zip_md5)),
    stringsAsFactors = FALSE
  )
  write_csv_safe(summary, file.path(audit_dir, "coffee_spatial_summary.csv"))
  writeLines(c(
    "COFFE-STUDY: SPATIAL REFERENCE, STEP 05",
    paste("Execution time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Official source:", source_url),
    paste("Reference boundary year:", reference_year),
    paste("Original input MD5:", input_md5),
    paste("Official ZIP MD5:", zip_md5),
    paste("Metric projection:", metric_crs),
    "Geographic coordinates are WGS84 decimal degrees (EPSG:4326).",
    "Centroids use the complete polygon, including all component polygons.",
    "Interior reference points use st_point_on_surface; they are not farm coordinates.",
    "A centroid can be outside a concave or multipart municipality; this is explicitly flagged.",
    "Calculated polygon areas are reference-geometry areas, not planted or harvested coffee areas.",
    "Exact seven-digit codes are the only join key. Names and nearest neighbours are never used.",
    "Unmatched codes and missing measurements remain in tabular outputs.",
    "Reference codes absent from the master require source-coverage investigation; absence is not zero coffee.",
    "The spatial targets from step 04 use positive production or harvested area in at least one year.",
    "All input codes are included in the registry, including codes outside the target list.",
    "Matching a code does NOT demonstrate that its boundaries were constant through time.",
    "The 2024 boundaries provide a reference join, not a historically harmonized panel.",
    "No longitudinal map or production-weighted migration trend is estimated in this step.",
    "Coffee-field elevation requires appropriate crop-location data and a terrain model.",
    "Municipal terrain elevation alone cannot establish within-municipality uphill movement.",
    "Species labels, original units and national totals still require primary-source checks.",
    "The temporal split at 2002 is inherited unchanged from step 04; no conversion is applied.",
    "Original production files and measurement columns are unchanged.",
    paste("Unmatched spatial targets:", unmatched_targets)
  ), file.path(audit_dir, "coffee_spatial_method_notes.txt"), useBytes = TRUE)
  capture.output(utils::sessionInfo(), file = file.path(audit_dir, "sessionInfo_step05.txt"))

  message("\nSTEP 05: spatial reference files saved.")
  message("Matched input codes: ", sum(registry$spatial_match), " / ", nrow(registry))
  message("Matched spatial targets: ", sum(targets_geo$spatial_match), " / ", nrow(targets))
  message("Spatial outputs: ", spatial_dir)
  message("Please return: ", file.path(audit_dir, "coffee_spatial_summary.csv"))
  message("and: ", file.path(audit_dir, "coffee_unmatched_municipality_codes.csv"))
  if (unmatched_targets > 0L) {
    warning("Spatial reference is INCOMPLETE for ", unmatched_targets,
            " targets. Inspect unmatched codes before mapping or trend analysis.", call. = FALSE)
  }
  invisible(summary)
}

main()
