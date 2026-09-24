# ============================================================
# COFFE-STUDY | STEP 35
# Field-level spatial sensitivity for altitude using MapBiomas coffee pixels
# MapBiomas Collection 11 class 46 + SRTM 90 m elevation
# IMPORTANT: MapBiomas maps coffee as one class, not Arabica/Canephora.
# Species are handled with two transparent municipal allocation strategies:
# (1) dominant-species municipalities >=80% species share (primary sensitivity)
# (2) proportional municipal species weights (secondary sensitivity)
# ============================================================

rm(list = ls())

packages <- c("terra", "sf", "geobr", "geodata", "dplyr", "tidyr", "readr", "purrr", "ggplot2", "tibble")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

terraOptions(progress = 1, memfrac = 0.65)

base_dir <- "."
raw_dir <- file.path(base_dir, "01_data_raw", "MapBiomas_coffee")
proc_dir <- file.path(base_dir, "02_data_processed", "MapBiomas_coffee_altitude")
out_dir <- file.path(base_dir, "04_outputs", "MapBiomas_field_altitude")
fig_dir <- file.path(base_dir, "05_figures", "MapBiomas_field_altitude")
for (x in c(raw_dir, proc_dir, out_dir, fig_dir)) dir.create(x, recursive = TRUE, showWarnings = FALSE)

find_project_file <- function(name) {
  x <- list.files(base_dir, pattern = paste0("^", name, "$"), recursive = TRUE, full.names = TRUE)
  if (!length(x)) stop("File not found in project: ", name)
  x[1]
}

coffee <- read_csv(find_project_file("coffee_brazil_municipality_audited_1974_2024.csv"), show_col_types = FALSE) |>
  filter(species %in% c("Arabica", "Canephora"), year %in% c(2012:2015, 2021:2024)) |>
  mutate(period = if_else(year <= 2015, "2012_2015", "2021_2024"))

# Municipal species composition per period
shares <- coffee |>
  group_by(municipality_code, period, species) |>
  summarise(area = mean(area_harvested_ha, na.rm = TRUE), .groups = "drop") |>
  mutate(area = if_else(is.nan(area), 0, pmax(area, 0))) |>
  pivot_wider(names_from = species, values_from = area, values_fill = 0) |>
  mutate(
    total_species_area = Arabica + Canephora,
    arabica_share = if_else(total_species_area > 0, Arabica / total_species_area, NA_real_),
    canephora_share = if_else(total_species_area > 0, Canephora / total_species_area, NA_real_),
    dominant_species = case_when(
      arabica_share >= 0.80 ~ "Arabica",
      canephora_share >= 0.80 ~ "Canephora",
      total_species_area > 0 ~ "Mixed",
      TRUE ~ NA_character_
    )
  )

# 2024 municipalities
mun <- geobr::read_municipality(code_muni = "all", year = 2024, simplified = FALSE, showProgress = FALSE) |>
  st_make_valid() |>
  mutate(municipality_code = as.integer(code_muni))

# Restrict processing to municipalities with coffee in either period
coffee_codes <- shares |> filter(total_species_area > 0) |> distinct(municipality_code) |> pull()
mun_coffee <- mun |> filter(municipality_code %in% coffee_codes)

# MapBiomas Collection 11 annual national map URL
mapbiomas_url <- function(year) {
  paste0(
    "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection11/lulc/coverage/brazil_coverage/",
    "brazil_coverage-col11_", year, ".tif"
  )
}

get_mapbiomas <- function(year) {
  f <- file.path(raw_dir, paste0("brazil_coverage-col11_", year, ".tif"))
  if (!file.exists(f)) {
    message("Downloading MapBiomas Collection 11: ", year)
    download.file(mapbiomas_url(year), f, mode = "wb", quiet = FALSE)
  }
  rast(f)
}

# Build persistent coffee mask: class 46 in at least 3 of 4 years
build_period_mask <- function(years, label) {
  out <- file.path(proc_dir, paste0("coffee_persistent_", label, ".tif"))
  if (file.exists(out)) return(rast(out))

  rlist <- lapply(years, function(y) {
    r <- get_mapbiomas(y)
    # class 46 = coffee
    classify(r, rcl = matrix(c(-Inf,45.5,0, 45.5,46.5,1, 46.5,Inf,0), ncol = 3, byrow = TRUE))
  })
  s <- do.call(c, rlist)
  persistence <- sum(s, na.rm = TRUE)
  mask <- persistence >= 3
  writeRaster(mask, out, overwrite = TRUE, datatype = "INT1U")
  mask
}

mask0 <- build_period_mask(2012:2015, "2012_2015")
mask1 <- build_period_mask(2021:2024, "2021_2024")

# Crop to coffee-producing municipality extent
mun_v <- vect(st_transform(mun_coffee, crs(mask0)))
ext_coffee <- ext(mun_v)
mask0 <- crop(mask0, ext_coffee)
mask1 <- crop(mask1, ext_coffee)

# Aggregate 30 m coffee mask to ~90 m fractions to match SRTM scale
frac0 <- aggregate(mask0, fact = 3, fun = mean, na.rm = TRUE)
frac1 <- aggregate(mask1, fact = 3, fun = mean, na.rm = TRUE)

# SRTM 3 arc-second tiles covering coffee extent
# geodata::elevation_3s downloads 5 x 5 degree tiles by lon/lat.
bb <- as.vector(ext(frac0))
lon_centers <- seq(floor(bb[1]/5)*5 + 2.5, ceiling(bb[2]/5)*5 - 2.5, by = 5)
lat_centers <- seq(floor(bb[3]/5)*5 + 2.5, ceiling(bb[4]/5)*5 - 2.5, by = 5)
centers <- expand.grid(lon = lon_centers, lat = lat_centers)

dem_tiles <- pmap(centers, function(lon, lat) {
  tryCatch(geodata::elevation_3s(lon = lon, lat = lat, path = raw_dir), error = function(e) NULL)
})
dem_tiles <- compact(dem_tiles)
if (!length(dem_tiles)) stop("Could not obtain SRTM elevation tiles.")
dem <- do.call(mosaic, dem_tiles)
dem <- crop(dem, ext(frac0))
dem <- resample(dem, frac0, method = "bilinear")

# Municipality raster for zonal summaries
mun_index <- mun_coffee |> select(municipality_code) |> st_transform(crs(frac0))
mun_index$zone <- seq_len(nrow(mun_index))
zone_lookup <- st_drop_geometry(mun_index) |> select(zone, municipality_code)
zone_r <- rasterize(vect(mun_index), frac0, field = "zone", touches = TRUE)

# Helper: municipal mapped coffee area and coffee-pixel mean altitude
municipal_stats <- function(frac, period_label) {
  cell_ha <- cellSize(frac, unit = "ha")
  w_area <- frac * cell_ha
  elev_num <- frac * dem * cell_ha

  za <- zonal(w_area, zone_r, fun = "sum", na.rm = TRUE) |> as.data.frame()
  ze <- zonal(elev_num, zone_r, fun = "sum", na.rm = TRUE) |> as.data.frame()
  names(za) <- c("zone", "mapped_coffee_ha")
  names(ze) <- c("zone", "elevation_area_sum")

  left_join(za, ze, by = "zone") |>
    left_join(zone_lookup, by = "zone") |>
    mutate(
      period = period_label,
      mapped_coffee_mean_elevation_m = if_else(mapped_coffee_ha > 0, elevation_area_sum / mapped_coffee_ha, NA_real_)
    )
}

mun0 <- municipal_stats(frac0, "2012_2015")
mun1 <- municipal_stats(frac1, "2021_2024")
munstats <- bind_rows(mun0, mun1) |>
  left_join(shares, by = c("municipality_code", "period"))

write_csv(munstats, file.path(out_dir, "MapBiomas_municipal_coffee_altitude_summary.csv"))

# Primary sensitivity: dominant-species municipalities only
primary <- munstats |>
  filter(dominant_species %in% c("Arabica", "Canephora"), mapped_coffee_ha > 0) |>
  group_by(species = dominant_species, period) |>
  summarise(
    mapped_coffee_ha = sum(mapped_coffee_ha),
    weighted_field_elevation_m = weighted.mean(mapped_coffee_mean_elevation_m, mapped_coffee_ha, na.rm = TRUE),
    municipalities = n(),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = period, values_from = c(mapped_coffee_ha, weighted_field_elevation_m, municipalities), names_sep = "__") |>
  mutate(
    field_elevation_change_m = weighted_field_elevation_m__2021_2024 - weighted_field_elevation_m__2012_2015
  )

# Secondary sensitivity: proportional species allocation within each municipality
prop <- bind_rows(
  munstats |> mutate(species = "Arabica", species_weight = arabica_share),
  munstats |> mutate(species = "Canephora", species_weight = canephora_share)
) |>
  filter(is.finite(species_weight), species_weight > 0, mapped_coffee_ha > 0) |>
  mutate(weighted_mapped_area = mapped_coffee_ha * species_weight) |>
  group_by(species, period) |>
  summarise(
    mapped_coffee_ha_species_weighted = sum(weighted_mapped_area),
    weighted_field_elevation_m = weighted.mean(mapped_coffee_mean_elevation_m, weighted_mapped_area, na.rm = TRUE),
    municipalities = n(),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = period, values_from = c(mapped_coffee_ha_species_weighted, weighted_field_elevation_m, municipalities), names_sep = "__") |>
  mutate(
    field_elevation_change_m = weighted_field_elevation_m__2021_2024 - weighted_field_elevation_m__2012_2015
  )

write_csv(primary, file.path(out_dir, "MapBiomas_field_elevation_dominant_species_primary.csv"))
write_csv(prop, file.path(out_dir, "MapBiomas_field_elevation_proportional_species_sensitivity.csv"))

# Coverage diagnostic against IBGE total coffee area
coverage <- munstats |>
  mutate(ibge_total_species_area = total_species_area) |>
  group_by(period) |>
  summarise(
    MapBiomas_mapped_coffee_ha = sum(mapped_coffee_ha, na.rm = TRUE),
    IBGE_Arabica_plus_Canephora_ha = sum(ibge_total_species_area, na.rm = TRUE),
    MapBiomas_to_IBGE_ratio = MapBiomas_mapped_coffee_ha / IBGE_Arabica_plus_Canephora_ha,
    .groups = "drop"
  )
write_csv(coverage, file.path(out_dir, "MapBiomas_IBGE_area_coverage_diagnostic.csv"))

# Figure
plotdat <- primary |>
  select(species, weighted_field_elevation_m__2012_2015, weighted_field_elevation_m__2021_2024, field_elevation_change_m) |>
  pivot_longer(starts_with("weighted_field"), names_to = "period", values_to = "elevation_m") |>
  mutate(period = if_else(grepl("2012", period), "2012 to 2015", "2021 to 2024"))

p <- ggplot(plotdat, aes(x = elevation_m, y = species, shape = period, colour = species)) +
  geom_line(aes(group = species), colour = "grey55", linewidth = 0.8) +
  geom_point(size = 4) +
  scale_colour_manual(values = c(Arabica = "#315A70", Canephora = "#69784A")) +
  labs(
    title = "Mapped coffee pixels provide an independent altitude sensitivity",
    subtitle = "Primary sensitivity uses MapBiomas coffee pixels in municipalities where one species represents at least 80% of IBGE harvested area",
    x = "Coffee-pixel area-weighted elevation (m)", y = NULL, colour = NULL, shape = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "top"
  )

ggsave(file.path(fig_dir, "MapBiomas_field_elevation_sensitivity.png"), p, width = 10, height = 6.5, dpi = 400)
ggsave(file.path(fig_dir, "MapBiomas_field_elevation_sensitivity.pdf"), p, width = 10, height = 6.5)

message("STEP 35 COMPLETED")
print(coverage)
print(primary)
print(prop)
