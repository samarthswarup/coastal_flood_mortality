##########################################
# Step 6 - Calculate exposed pops        #
#                                        #
# 2026-06-24                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# This script calculates "exposed" populations for every event in our subset of
# EMDAT data using various definitions of exposure. It loops through EMDAT
# events and tabulates national and subnational estimates of coastal populations
# based on the SLIIDERS population data (adjusted to the year of the event).
#
# Three spatial scales of exposure are computed:
#   ADM0  — entire country
#   local — SLIIDERS tiles intersecting the geocoded event polygon(s)
#   ADM1  — SLIIDERS tiles intersecting the ADM1 region(s) containing the event
#
# For events with no geocoded location, local and ADM1 fall back to ADM0.
# For multi-country events, all countries' tiles are summed into "pooled" cols.
# A composite "local.else.adm1.else.national" column is also built, which uses
# local where available, ADM1 where local is zero, and ADM0 as final fallback.

rm(list = ls())

library(openxlsx)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(lwgeom)
library(dplyr)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

step3_date_append_output <- "2026-07-04"
step4_date_append_output <- "2026-06-24"
step5_date_append_output <- "2026-06-24"
date_append_output       <- "2026-07-04"

# Set TRUE to save PNG maps of tile intersections for QC (slow)
plot_tiles <- FALSE

dir.create(paste0("Intermediate outputs/Output from Step 6/",
                  "Maps of SLIIDERS tiles covered by each disaster"),
           recursive = TRUE, showWarnings = FALSE)
dir.create("Output", recursive = TRUE, showWarnings = FALSE)


# I. Read inputs ---------------------------------------------------------------

unzip(zipfile = "Inputs/SLIIDERS data/SLIIDERS_SHP.zip",
      exdir   = "Inputs/SLIIDERS data/",
      overwrite = FALSE)

tile.shapefile.data <- st_read(
  "Inputs/SLIIDERS data/SLIIDERS_SHP/sliiders_v1_1_seg_voronoi.shp",
  quiet = TRUE) |> st_make_valid()

seg_adm_pop_exposed.df <- read.csv(
  paste0("Intermediate outputs/Output from Step 3/",
         "All pops by year (past and future) by SSP by return period by ",
         "tile with pop growth ", step3_date_append_output, ".csv"))

emdat.data.filtered <- read.xlsx(
  paste0("Intermediate outputs/Output from Step 4/",
         "EMDAT filtered events with deaths ", step4_date_append_output, ".xlsx"))

geocode.data <- read.xlsx(
  paste0("Intermediate outputs/Output from Step 5/",
         "Geocode database with events ", step5_date_append_output, ".xlsx"))

options(timeout = max(600, getOption("timeout")))

# Download the GADM geopackage if it isn't already present locally. GADM's
# license doesn't permit redistribution, so this file is never committed to
# the repo -- each user's first run fetches it directly from GADM instead.
if (!file.exists("Inputs/GADM data/gadm_410-levels.gpkg")) {
  dir.create("Inputs/GADM data", recursive = TRUE, showWarnings = FALSE)
  download.file(
    url      = "https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gpkg.zip",
    destfile = "Inputs/GADM data/gadm_410-gpkg.zip",
    mode     = "wb")
  unzip(zipfile   = "Inputs/GADM data/gadm_410-gpkg.zip",
        exdir     = "Inputs/GADM data/",
        overwrite = FALSE)
}

gadm.data.adm1 <- st_read("Inputs/GADM data/gadm_410-levels.gpkg",
                           layer = "ADM_1", quiet = TRUE)
gadm.data.adm1_simplified <-
  st_simplify(st_make_valid(gadm.data.adm1),
              preserveTopology = TRUE, dTolerance = 0.05)

country.data <- read.csv(
  paste0("Inputs/Identify mismatched and missing countries - step 3/",
         "SLIIDERS country codes with names and territorial mapping.csv"))


# II. Set up scenario vectors --------------------------------------------------

surge.period.vec <- c("10yr", "100yr", "1000yr", "10000yr")

# Identify which surge periods have SLR+popscale columns in Step 3 output
# (currently only "100yr" — the SLR scenarios are only run for that period)
slr.popscale.colnames <-
  colnames(seg_adm_pop_exposed.df)[
    grepl("\\.popscale\\.", colnames(seg_adm_pop_exposed.df))]

slr.colnames.root <- unique(sub("\\.pop.*", "", slr.popscale.colnames))

surge.period.vec.future <-
  surge.period.vec[surge.period.vec %in%
    unique(sub(".*\\.pop\\.", "",
               sub("\\.popscale.*", "", slr.popscale.colnames)))]

# If none detected, default to 100yr
if (length(surge.period.vec.future) == 0) surge.period.vec.future <- "100yr"


# III. Helper: look up tile-level population for a given column ----------------
#
# Handles:
#  - Single or multiple SLIIDERS country codes
#  - Canary Islands (SPI) special case
#  - Fallback to grep-based country matching for overseas territories
#
# Fix vs original: used == for country matching which breaks for multi-country
# events; now uses %in% throughout.

spi_bbox <- st_as_sf(data.frame(
  id = 1L,
  geometry = st_sfc(st_polygon(list(matrix(
    c(-18.2, 27.5, -18.2, 29.6, -13.3, 29.6, -13.3, 27.5, -18.2, 27.5),
    ncol = 2, byrow = TRUE))), crs = 4326)))

spi_segments <- tile.shapefile.data |> st_filter(spi_bbox)

get_adm0_pop <- function(country_codes, col_name, iso_code) {
  # Primary: exact country code match
  rows <- which(seg_adm_pop_exposed.df$country_code %in% country_codes)

  # Canary Islands override
  if ("SPI" %in% iso_code) {
    rows <- which(substr(seg_adm_pop_exposed.df$seg_adm, 1, 9) %in%
                    spi_segments$seg)
  }

  pop <- sum(seg_adm_pop_exposed.df[rows,
             which(colnames(seg_adm_pop_exposed.df) == col_name)],
             na.rm = TRUE)

  # Fallback: grep-based match for territories (e.g. ANT → CUW, ABW, ...)
  if (pop == 0 && !("SPI" %in% iso_code)) {
    rows_fb <- which(unlist(lapply(
      seg_adm_pop_exposed.df$country_code,
      function(x) length(grep(x, country_codes)) > 0)))
    pop <- sum(seg_adm_pop_exposed.df[rows_fb,
               which(colnames(seg_adm_pop_exposed.df) == col_name)],
               na.rm = TRUE)
  }
  pop
}


# IV. Initialise ADM0 exposure columns -----------------------------------------

init_cols <- function(df, pattern_fn, surges, extras = NULL) {
  for (surge in surges) {
    df[[pattern_fn(surge)]] <- NA_real_
  }
  df
}

for (surge in surge.period.vec) {
  emdat.data.filtered[[paste0("pop.exposed.adm0.", surge)]] <- NA_real_
  for (j in 1:5) {
    emdat.data.filtered[[paste0("base.2050.SSP", j, ".pop.exposed.adm0.", surge)]] <- NA_real_
    emdat.data.filtered[[paste0("base.2100.SSP", j, ".pop.exposed.adm0.", surge)]] <- NA_real_
  }
}

for (root in slr.colnames.root) {
  for (surge in surge.period.vec.future) {
    emdat.data.filtered[[paste0(root, ".pop.exposed.adm0.", surge)]] <- NA_real_
    for (pscol in slr.popscale.colnames[grepl(root, slr.popscale.colnames)]) {
      emdat.data.filtered[[gsub("\\.pop\\.", ".pop.exposed.adm0.", pscol)]] <- NA_real_
    }
  }
}


# V. Loop I: fill ADM0 exposure columns ----------------------------------------

for (i in seq_len(nrow(emdat.data.filtered))) {

  country_codes <- unlist(strsplit(
    emdat.data.filtered$SLIIDERS.country.code.flat[i], ","))
  iso_code  <- emdat.data.filtered$ISO[i]
  emdat.year <- emdat.data.filtered$Start.Year[i]

  for (surge in surge.period.vec) {

    # Year-of-event population
    col_lkp <- if (emdat.year < 2000)
      paste0("base.", emdat.year, ".pop.", surge) else
      paste0("base.", emdat.year, ".SSP1.pop.", surge)

    emdat.data.filtered[i, paste0("pop.exposed.adm0.", surge)] <-
      get_adm0_pop(country_codes, col_lkp, iso_code)

    # SSP projections 2050 and 2100
    for (j in 1:5) {
      for (yr in c(2050, 2100)) {
        col_lkp  <- paste0("base.", yr, ".SSP", j, ".pop.", surge)
        col_fill <- paste0("base.", yr, ".SSP", j, ".pop.exposed.adm0.", surge)
        emdat.data.filtered[i, col_fill] <-
          get_adm0_pop(country_codes, col_lkp, iso_code)
      }
    }

    # SLR scenarios (geometry only)
    for (root in slr.colnames.root) {
      col_lkp  <- paste0(root, ".pop.", surge)
      col_fill <- paste0(root, ".pop.exposed.adm0.", surge)
      emdat.data.filtered[i, col_fill] <-
        get_adm0_pop(country_codes, col_lkp, iso_code)
    }

    # SLR + popscale composite
    if (surge %in% surge.period.vec.future) {
      for (pscol in slr.popscale.colnames[grepl(surge, slr.popscale.colnames)]) {
        col_fill <- gsub("\\.pop\\.", ".pop.exposed.adm0.", pscol)
        emdat.data.filtered[i, col_fill] <-
          get_adm0_pop(country_codes, pscol, iso_code)
      }
    }
  }

  message("I. ", i, " / ", nrow(emdat.data.filtered))
}

save(emdat.data.filtered,
     file = paste0("Intermediate outputs/Output from Step 6/",
                   "emdat.data.filtered after Step I ", date_append_output))


# VI. Pool multi-country events ------------------------------------------------
#
# For events spanning multiple countries, sum all countries' ADM0 values
# into a single "pooled" row (first occurrence of that DisNo); subsequent
# rows for the same event get NA so they're excluded from analysis.

all_exposure_cols <- colnames(emdat.data.filtered)[
  grepl("pop\\.exposed\\.adm0", colnames(emdat.data.filtered))]

for (col in all_exposure_cols)
  emdat.data.filtered[[paste0(col, ".pooled")]] <- NA_real_

emdat.data.filtered$total.deaths.na.as.0.pooled  <- NA_real_
emdat.data.filtered$total.deaths.reported.pooled  <- NA_real_

multi.country.events <- list()
k <- 0L

for (i in seq_len(nrow(emdat.data.filtered))) {

  dis_root    <- substr(emdat.data.filtered$DisNo.[i], 1, 9)
  dis_rows    <- grep(dis_root, emdat.data.filtered$DisNo.)
  already_seen <- any(sapply(multi.country.events,
                              function(x) any(grepl(dis_root, x))))

  if (length(dis_rows) > 1 && !already_seen) {
    k <- k + 1L
    multi.country.events[[k]] <- emdat.data.filtered$DisNo.[dis_rows]

    emdat.data.filtered$total.deaths.na.as.0.pooled[i] <-
      sum(emdat.data.filtered$total.deaths.na.as.0[dis_rows])
    emdat.data.filtered$total.deaths.reported.pooled[i] <-
      sum(emdat.data.filtered$total.deaths.reported[dis_rows])

    matched_rows <- match(unlist(multi.country.events[[k]]),
                          emdat.data.filtered$DisNo.)
    for (col in all_exposure_cols) {
      emdat.data.filtered[i, paste0(col, ".pooled")] <-
        sum(emdat.data.filtered[matched_rows, col], na.rm = TRUE)
    }

  } else if (length(dis_rows) == 1) {

    emdat.data.filtered$total.deaths.na.as.0.pooled[i] <-
      emdat.data.filtered$total.deaths.na.as.0[i]
    emdat.data.filtered$total.deaths.reported.pooled[i] <-
      emdat.data.filtered$total.deaths.reported[i]

    for (col in all_exposure_cols)
      emdat.data.filtered[i, paste0(col, ".pooled")] <-
        emdat.data.filtered[i, col]
  }
}


# VII. Loop III: identify SLIIDERS tiles covered by local geocode polygons -----
#
# For each event, find which SLIIDERS tiles intersect the geocoded polygon(s).
# Tiles from a different country than the event are recorded but excluded.
# Country adjacency exceptions handle cases where tiles straddle borders.

emdat.data.filtered$tiles_covered.local  <- vector("list", nrow(emdat.data.filtered))
emdat.data.filtered$tiles_excluded.local <- vector("list", nrow(emdat.data.filtered))

world <- ne_countries(scale = "medium", returnclass = "sf")

expand_bbox_square <- function(bbox, expand_factor) {
  x_range  <- bbox["xmax"] - bbox["xmin"]
  y_range  <- bbox["ymax"] - bbox["ymin"]
  max_range <- max(x_range, y_range)
  bbox["xmin"] <- bbox["xmin"] - (max_range - x_range) / 2 - max_range * expand_factor
  bbox["xmax"] <- bbox["xmax"] + (max_range - x_range) / 2 + max_range * expand_factor
  bbox["ymin"] <- bbox["ymin"] - (max_range - y_range) / 2 - max_range * expand_factor
  bbox["ymax"] <- bbox["ymax"] + (max_range - y_range) / 2 + max_range * expand_factor
  bbox
}

# Country adjacency exceptions: when no tiles found in the event country,
# allow neighbouring country tiles to fill in. Order matters: only the first
# matching rule fires.
adjacency_exceptions <- list(
  IND = c("IND", "BGD"),
  TON = c("TON", "WSM", "WLF"),
  CHN = c("CHN", "RUS"),
  SLV = c("SLV", "NIC"),
  ATG = c("ATG", "MSR"),
  BEL = c("BEL", "NLD"),
  COD = c("COD", "COG"),
  EST = c("EST", "LVA"),
  COK = c("ASM", "TKL"),
  THA = c("THA", "MMR", "VNM"),
  KHM = c("KHM", "VNM", "THA"),
  ARG = c("ARG", "BRA", "URY")
)

for (i in seq_len(nrow(emdat.data.filtered))) {

  disaster.number  <- emdat.data.filtered$DisNo.[i]
  geocode.rows     <- grep(disaster.number, geocode.data$disaster_number)
  sliiders.countries <- unlist(strsplit(
    emdat.data.filtered$SLIIDERS.country.code.flat[i], ","))

  tile_names          <- character(0)
  geocode.polygon.cumul <- NULL

  if (length(geocode.rows) > 0) {

    for (j in seq_along(geocode.rows)) {
      path <- geocode.data$local.polygon.path[geocode.rows[j]]
      if (is.na(path) || !nzchar(path)) next
      gpoly <- tryCatch(st_read(path, quiet = TRUE), error = function(e) NULL)
      if (is.null(gpoly)) next

      geocode.polygon.cumul <- rbind(geocode.polygon.cumul, gpoly)

      sf::sf_use_s2(TRUE)
      hits <- st_intersects(tile.shapefile.data, st_make_valid(gpoly),
                            sparse = FALSE)
      tile_names <- c(tile_names,
                      tile.shapefile.data$seg[which(hits)])
    }

    tile_names <- unique(tile_names)

    tile_names_country <-
      seg_adm_pop_exposed.df$country_code[
        match(tile_names,
              substr(seg_adm_pop_exposed.df$seg_adm, 1, 9))]

    # Marshall Islands / Yakushima special cases
    if (sum(tile_names_country == "MHL", na.rm = TRUE) > 0) {
      tile_names_country[!is.na(tile_names_country) &
                           tile_names_country == "MHL"] <- "USA"
      emdat.data.filtered$ISO[i]     <- "MHL"
      emdat.data.filtered$Country[i] <- "Marshall Islands"
    }

    if (length(grep("Yakushuma Island", emdat.data.filtered$Location[i])) > 0) {
      tile_names_country[!is.na(tile_names_country) &
                           tile_names_country == "TWN"] <- "JPN"
      emdat.data.filtered$ISO[i]     <- "JPN"
      emdat.data.filtered$Country[i] <- "Japan"
      sliiders.countries <- "JPN"
    }

    # Fix: use %in% not == so multi-country events work correctly
    tiles_include <- which(tile_names_country %in% sliiders.countries)
    tiles_exclude <- setdiff(seq_along(tile_names_country), tiles_include)

    # If no tiles found, try adjacency exceptions
    if (length(tiles_include) == 0) {
      for (base_code in names(adjacency_exceptions)) {
        if (base_code %in% sliiders.countries) {
          sliiders.countries <- adjacency_exceptions[[base_code]]
          tiles_include <- which(tile_names_country %in% sliiders.countries)
          tiles_exclude <- setdiff(seq_along(tile_names_country), tiles_include)
          break
        }
      }
    }

    emdat.data.filtered$tiles_covered.local[[i]]  <- tile_names[tiles_include]
    emdat.data.filtered$tiles_excluded.local[[i]] <- tile_names[tiles_exclude]

    if (plot_tiles && length(tiles_include) > 0) {
      plot_data    <- tile.shapefile.data[
        match(emdat.data.filtered$tiles_covered.local[[i]],
              tile.shapefile.data$seg), ]
      expanded_bbox <- expand_bbox_square(st_bbox(plot_data), 3)

      gg <- ggplot() +
        geom_sf(data = world, fill = "gray80") +
        geom_sf(data = geocode.polygon.cumul, fill = "blue", alpha = 0.5) +
        geom_sf(data = plot_data, fill = "red", alpha = 0.5) +
        coord_sf(xlim = c(expanded_bbox["xmin"], expanded_bbox["xmax"]),
                 ylim = c(expanded_bbox["ymin"], expanded_bbox["ymax"]),
                 expand = FALSE) +
        ggtitle(paste("Disaster:", disaster.number)) +
        theme_minimal()

      ggsave(paste0("Intermediate outputs/Output from Step 6/",
                    "Maps of SLIIDERS tiles covered by each disaster/",
                    disaster.number, "_combined_local_polygons.png"),
             plot = gg, width = 10, height = 8)
    }
  }

  message("III. ", i, " / ", nrow(emdat.data.filtered))
}

# Log events with excluded tiles for QC
disasters.with.excluded.tiles <- data.frame(
  dis.no = emdat.data.filtered$DisNo.[
    which(lengths(emdat.data.filtered$tiles_excluded.local) > 0)],
  location = emdat.data.filtered$Location[
    which(lengths(emdat.data.filtered$tiles_excluded.local) > 0)],
  excluded.tiles = sapply(
    emdat.data.filtered$tiles_excluded.local[
      which(lengths(emdat.data.filtered$tiles_excluded.local) > 0)],
    paste, collapse = ","),
  included.tiles = sapply(
    emdat.data.filtered$tiles_covered.local[
      which(lengths(emdat.data.filtered$tiles_excluded.local) > 0)],
    paste, collapse = ","),
  excluded.tile.countries = sapply(
    emdat.data.filtered$tiles_excluded.local[
      which(lengths(emdat.data.filtered$tiles_excluded.local) > 0)],
    function(y) paste(unique(
      tile.shapefile.data$seg_countr[match(y, tile.shapefile.data$seg)]),
      collapse = ","))
)

disasters.with.excluded.tiles$emdat.row <-
  match(disasters.with.excluded.tiles$dis.no, emdat.data.filtered$DisNo.)

write.csv(disasters.with.excluded.tiles,
          paste0("Intermediate outputs/Output from Step 6/Disasters with excluded location tiles ",
                 date_append_output, ".csv"))

save(emdat.data.filtered,
     file = paste0("Intermediate outputs/Output from Step 6/",
                   "emdat.data.filtered after Step III ", date_append_output))


# VIII. Initialise and fill local exposure columns -----------------------------
#
# Two variants per column:
#   local.if.available  — NA when no geocode data (not zero)
#   local.else.national — falls back to ADM0 when no geocode data

for (surge in surge.period.vec) {
  for (sfx in c("local.if.available", "local.else.national")) {
    emdat.data.filtered[[paste0("pop.exposed.", sfx, ".", surge)]] <- NA_real_
    for (j in 1:5) {
      for (yr in c(2050, 2100))
        emdat.data.filtered[[paste0("base.", yr, ".SSP", j,
                                    ".pop.exposed.", sfx, ".", surge)]] <- NA_real_
    }
  }
  if (surge %in% surge.period.vec.future) {
    for (root in slr.colnames.root) {
      for (sfx in c("local.if.available", "local.else.national"))
        emdat.data.filtered[[paste0(root, ".pop.exposed.", sfx, ".", surge)]] <- NA_real_
    }
    for (pscol in slr.popscale.colnames[grepl(surge, slr.popscale.colnames)]) {
      for (sfx in c("local.if.available", "local.else.national"))
        emdat.data.filtered[[gsub("\\.pop\\.",
                                   paste0(".pop.exposed.", sfx, "."), pscol)]] <- NA_real_
    }
  }
}

tile_match_rows <- function(tile_names, iso_code) {
  which(
    substr(seg_adm_pop_exposed.df$seg_adm, 1, 9) %in% tile_names &
      (seg_adm_pop_exposed.df$country_code == iso_code |
         seg_adm_pop_exposed.df$country_code %in%
         country.data$SLIIDERS.country.code[
           grep(iso_code, country.data$Alternate.ISO.code)])
  )
}

for (i in seq_len(nrow(emdat.data.filtered))) {

  tiles_covered <- unlist(emdat.data.filtered$tiles_covered.local[i])
  iso_code      <- emdat.data.filtered$ISO[i]
  emdat.year    <- emdat.data.filtered$Start.Year[i]
  country_codes <- unlist(strsplit(
    emdat.data.filtered$SLIIDERS.country.code.flat[i], ","))

  has_local_tiles <- length(tiles_covered) > 0 &&
    length(tile_match_rows(tiles_covered, iso_code)) > 0

  for (surge in surge.period.vec) {

    if (!has_local_tiles) {
      # No local data — copy ADM0 to else.national, NA to if.available
      col_adm0 <- paste0("pop.exposed.adm0.", surge)
      emdat.data.filtered[i, paste0("pop.exposed.local.else.national.", surge)] <-
        emdat.data.filtered[i, col_adm0]
      emdat.data.filtered[i, paste0("pop.exposed.local.if.available.", surge)] <- NA_real_

      for (j in 1:5) {
        for (yr in c(2050, 2100)) {
          col_adm0 <- paste0("base.", yr, ".SSP", j, ".pop.exposed.adm0.", surge)
          emdat.data.filtered[i, paste0("base.", yr, ".SSP", j,
                                         ".pop.exposed.local.else.national.", surge)] <-
            emdat.data.filtered[i, col_adm0]
          emdat.data.filtered[i, paste0("base.", yr, ".SSP", j,
                                         ".pop.exposed.local.if.available.", surge)] <- NA_real_
        }
      }
      if (surge %in% surge.period.vec.future) {
        for (root in slr.colnames.root) {
          col_adm0 <- paste0(root, ".pop.exposed.adm0.", surge)
          emdat.data.filtered[i, paste0(root, ".pop.exposed.local.else.national.", surge)] <-
            emdat.data.filtered[i, col_adm0]
          emdat.data.filtered[i, paste0(root, ".pop.exposed.local.if.available.", surge)] <- NA_real_
        }
        for (pscol in slr.popscale.colnames[grepl(surge, slr.popscale.colnames)]) {
          col_adm0 <- gsub("\\.pop\\.", ".pop.exposed.adm0.", pscol)
          emdat.data.filtered[i, gsub("\\.pop\\.", ".pop.exposed.local.else.national.", pscol)] <-
            emdat.data.filtered[i, col_adm0]
          emdat.data.filtered[i, gsub("\\.pop\\.", ".pop.exposed.local.if.available.", pscol)] <- NA_real_
        }
      }

    } else {
      # Has local tiles — sum tile populations
      t_rows <- tile_match_rows(tiles_covered, iso_code)

      col_lkp <- if (emdat.year < 2000)
        paste0("base.", emdat.year, ".pop.", surge) else
        paste0("base.", emdat.year, ".SSP1.pop.", surge)

      pop <- sum(seg_adm_pop_exposed.df[t_rows,
                 which(colnames(seg_adm_pop_exposed.df) == col_lkp)], na.rm = TRUE)

      emdat.data.filtered[i, paste0("pop.exposed.local.else.national.", surge)] <- pop
      emdat.data.filtered[i, paste0("pop.exposed.local.if.available.", surge)]  <- pop

      for (j in 1:5) {
        for (yr in c(2050, 2100)) {
          col_lkp <- paste0("base.", yr, ".SSP", j, ".pop.", surge)
          pop <- sum(seg_adm_pop_exposed.df[t_rows,
                     which(colnames(seg_adm_pop_exposed.df) == col_lkp)], na.rm = TRUE)
          emdat.data.filtered[i, paste0("base.", yr, ".SSP", j,
                                         ".pop.exposed.local.else.national.", surge)] <- pop
          emdat.data.filtered[i, paste0("base.", yr, ".SSP", j,
                                         ".pop.exposed.local.if.available.", surge)]  <- pop
        }
      }
      if (surge %in% surge.period.vec.future) {
        for (root in slr.colnames.root) {
          col_lkp <- paste0(root, ".pop.", surge)
          pop <- sum(seg_adm_pop_exposed.df[t_rows,
                     which(colnames(seg_adm_pop_exposed.df) == col_lkp)], na.rm = TRUE)
          emdat.data.filtered[i, paste0(root, ".pop.exposed.local.else.national.", surge)] <- pop
          emdat.data.filtered[i, paste0(root, ".pop.exposed.local.if.available.", surge)]  <- pop
        }
        for (pscol in slr.popscale.colnames[grepl(surge, slr.popscale.colnames)]) {
          pop <- sum(seg_adm_pop_exposed.df[t_rows,
                     which(colnames(seg_adm_pop_exposed.df) == pscol)], na.rm = TRUE)
          emdat.data.filtered[i, gsub("\\.pop\\.", ".pop.exposed.local.else.national.", pscol)] <- pop
          emdat.data.filtered[i, gsub("\\.pop\\.", ".pop.exposed.local.if.available.", pscol)]  <- pop
        }
      }
    }
  }
  message("IV. ", i, " / ", nrow(emdat.data.filtered))
}

save(emdat.data.filtered,
     file = paste0("Intermediate outputs/Output from Step 6/",
                   "emdat.data.filtered after Step V ", date_append_output))


# IX. Pool local exposure columns for multi-country events ---------------------

local_exposure_cols <- colnames(emdat.data.filtered)[
  grepl("pop\\.exposed\\.local", colnames(emdat.data.filtered))]

for (col in local_exposure_cols)
  emdat.data.filtered[[paste0(col, ".pooled")]] <- NA_real_

multi.country.events <- list()
k <- 0L

for (i in seq_len(nrow(emdat.data.filtered))) {

  dis_root     <- substr(emdat.data.filtered$DisNo.[i], 1, 9)
  dis_rows     <- grep(dis_root, emdat.data.filtered$DisNo.)
  already_seen <- any(sapply(multi.country.events,
                              function(x) any(grepl(dis_root, x))))

  if (length(dis_rows) > 1 && !already_seen) {
    k <- k + 1L
    multi.country.events[[k]] <- emdat.data.filtered$DisNo.[dis_rows]
    matched_rows <- match(unlist(multi.country.events[[k]]),
                          emdat.data.filtered$DisNo.)

    for (col in local_exposure_cols) {
      if (grepl("if\\.available", col)) {
        # NA if any country lacks local data
        vals <- emdat.data.filtered[matched_rows, col]
        emdat.data.filtered[i, paste0(col, ".pooled")] <-
          if (any(is.na(vals))) NA_real_ else sum(vals, na.rm = TRUE)
      } else {
        emdat.data.filtered[i, paste0(col, ".pooled")] <-
          sum(emdat.data.filtered[matched_rows, col], na.rm = TRUE)
      }
    }

  } else if (length(dis_rows) == 1) {
    for (col in local_exposure_cols)
      emdat.data.filtered[i, paste0(col, ".pooled")] <-
        emdat.data.filtered[i, col]
  }
}


# X. ADM1 tile intersection ---------------------------------------------------
#
# For each event, find ADM1 regions intersecting the geocoded polygon(s),
# then find SLIIDERS tiles within those ADM1 regions.
# Fix vs original: plot_tiles = TRUE was hardcoded at line 1641 overriding
# the user setting at the top; now respects the user's choice.

emdat.data.filtered$tiles_covered.adm1.alt <- vector("list", nrow(emdat.data.filtered))
emdat.data.filtered$adm1.gid               <- vector("list", nrow(emdat.data.filtered))
emdat.data.filtered$adm1.name              <- vector("list", nrow(emdat.data.filtered))
emdat.data.filtered$geocode.rows           <-
  lapply(emdat.data.filtered$DisNo.,
         function(x) grep(x, geocode.data$disaster_number))

adm1_full <- st_make_valid(gadm.data.adm1_simplified)

# Pre-compute global intersections: all local polygons × all ADM1
all_paths <- na.omit(geocode.data$local.polygon.path)
all_rows  <- which(!is.na(geocode.data$local.polygon.path))

locals_list <- lapply(all_paths, function(p)
  tryCatch(st_read(p, quiet = TRUE) |> st_make_valid(), error = function(e) NULL))

# Track which paths successfully loaded (NULL = failed read)
loaded_idx  <- which(!sapply(locals_list, is.null))
locals_list <- locals_list[loaded_idx]

# Normalise CRS and geometry column name before binding so rbind doesn't fail
target_crs <- 4326
locals_list <- lapply(locals_list, function(x) {
  if (is.na(st_crs(x)) || st_crs(x)$epsg != target_crs)
    x <- st_transform(x, target_crs)
  # Rename geometry column to "geometry" if it differs (e.g. "geom")
  geom_col <- attr(x, "sf_column")
  if (geom_col != "geometry") {
    x <- x |> rename(geometry = !!geom_col) |> st_set_geometry("geometry")
  }
  # Keep only columns present in all files to avoid bind failures
  x[, c("geometry"), drop = FALSE]
})

locals <- do.call(rbind, locals_list)
# Reattach disaster numbers using the successfully loaded indices
locals$disaster <- geocode.data$disaster_number[all_rows[loaded_idx]]

if (!inherits(locals, "sf"))
  stop("locals failed to build as sf object — check .gpkg files")

if (is.na(st_crs(locals)) || st_crs(locals) != st_crs(adm1_full))
  locals <- st_transform(locals, st_crs(adm1_full))

LxA <- st_intersects(locals, adm1_full, sparse = TRUE)

by_dis              <- split(seq_len(nrow(locals)), locals$disaster)
adm1_cands_by_dis   <- lapply(by_dis, function(ix) unique(unlist(LxA[ix])))

ea         <- 6933L
locals_ea  <- st_transform(locals, ea)
adm1_ea    <- st_transform(adm1_full, ea)

keep_adm_by_dis <- lapply(names(by_dis), function(dn) {
  i_locals   <- by_dis[[dn]]
  i_adm_cand <- adm1_cands_by_dis[[dn]]
  if (!length(i_adm_cand) || !length(i_locals)) return(integer(0))

  keep_sub <- integer(0)
  for (iL in i_locals) {
    Aloc <- as.numeric(st_area(locals_ea[iL, ]))
    cand_for_local <- LxA[[iL]]
    if (!length(cand_for_local)) next

    fracs <- sapply(cand_for_local, function(k) {
      tryCatch({
        ints <- suppressWarnings(st_intersection(
          st_geometry(adm1_ea[k, ]), st_geometry(locals_ea[iL, ])))
        if (!length(ints)) return(0)
        as.numeric(sum(st_area(ints)) / Aloc)
      }, error = function(e) NA_real_)
    })
    keep_sub <- c(keep_sub, cand_for_local[!is.na(fracs) & fracs > 0.01])
  }
  unique(keep_sub)
})
names(keep_adm_by_dis) <- names(by_dis)

# Find SLIIDERS tiles within each disaster's ADM1 set
keep_tiles_adm_by_dis <- lapply(keep_adm_by_dis, function(adm_idx) {
  if (!length(adm_idx)) return(integer(0))
  hits <- st_intersects(tile.shapefile.data,
                        gadm.data.adm1_simplified[adm_idx, , drop = FALSE],
                        sparse = TRUE)
  which(lengths(hits) > 0L)
})
names(keep_tiles_adm_by_dis) <- names(by_dis)

as_sf_geom <- function(x, crs) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "sf"))  return(x)
  if (inherits(x, "sfc")) return(sf::st_sf(geometry = x, crs = crs))
  xx <- try(do.call(c, as.list(x)), silent = TRUE)
  if (inherits(xx, "try-error")) return(NULL)
  sf::st_sf(geometry = sf::st_sfc(xx, crs = crs))
}

for (i in seq_len(nrow(emdat.data.filtered))) {

  disaster.number    <- emdat.data.filtered$DisNo.[i]
  sliiders.countries <- unlist(strsplit(
    emdat.data.filtered$SLIIDERS.country.code.flat[i], ","))

  # Find which pre-computed disasters match this DisNo
  matching_dis <- names(by_dis)[sapply(names(by_dis),
    function(x) length(grep(disaster.number, x)) > 0)]

  if (length(matching_dis) == 0) {
    message("VI. ", i, " / ", nrow(emdat.data.filtered), " (no geocode rows)")
    next
  }

  tile_idx_all <- unique(unlist(keep_tiles_adm_by_dis[matching_dis]))

  if (length(tile_idx_all) == 0) {
    message("VI. ", i, " / ", nrow(emdat.data.filtered), " (no ADM1 tiles)")
    next
  }

  tiles_cand <- tile.shapefile.data$seg[tile_idx_all]
  # Fix vs original: used == which breaks for multi-country; now uses %in%
  country_match <- tile.shapefile.data$seg_countr[tile_idx_all] %in% sliiders.countries

  emdat.data.filtered$tiles_covered.adm1.alt[[i]] <- tiles_cand[country_match]

  adm_idx_all <- unique(unlist(keep_adm_by_dis[matching_dis]))
  emdat.data.filtered$adm1.gid[[i]]  <- gadm.data.adm1$GID_1[adm_idx_all]
  emdat.data.filtered$adm1.name[[i]] <- gadm.data.adm1$NAME_1[adm_idx_all]

  if (plot_tiles && length(emdat.data.filtered$tiles_covered.adm1.alt[[i]]) > 0) {

    geocode_paths <- geocode.data$local.polygon.path[
      unlist(emdat.data.filtered$geocode.rows[i])]
    geocode_polys <- lapply(na.omit(geocode_paths), function(p)
      tryCatch(st_read(p, layer = "polygon", quiet = TRUE) |> st_make_valid(),
               error = function(e) NULL))
    geocode_polys <- Filter(Negate(is.null), geocode_polys)
    geocode.polygon.cumul <- if (length(geocode_polys) > 0)
      st_sf(geometry = st_union(do.call(rbind, geocode_polys)), crs = 4326) else NULL

    plot_data    <- tile.shapefile.data[
      match(emdat.data.filtered$tiles_covered.adm1.alt[[i]],
            tile.shapefile.data$seg), ]
    expanded_bbox <- expand_bbox_square(st_bbox(plot_data), 3)

    adm1_geoms <- gadm.data.adm1$geom[
      match(unlist(emdat.data.filtered$adm1.gid[i]), gadm.data.adm1$GID_1)]

    gg <- ggplot() +
      geom_sf(data = world, fill = "gray80") +
      { if (!is.null(as_sf_geom(adm1_geoms, st_crs(world))))
          geom_sf(data = as_sf_geom(adm1_geoms, st_crs(world)),
                  fill = "green", alpha = 0.5) } +
      { if (!is.null(geocode.polygon.cumul))
          geom_sf(data = geocode.polygon.cumul, fill = "blue", alpha = 0.5) } +
      geom_sf(data = plot_data, fill = "red", alpha = 0.5) +
      coord_sf(xlim = c(expanded_bbox["xmin"], expanded_bbox["xmax"]),
               ylim = c(expanded_bbox["ymin"], expanded_bbox["ymax"]),
               expand = FALSE) +
      ggtitle(paste("Disaster:", disaster.number)) +
      theme_minimal()

    ggsave(paste0("Intermediate outputs/Output from Step 6/",
                  "Maps of SLIIDERS tiles covered by each disaster/",
                  disaster.number, "_combined_ADM1_polygons.png"),
           plot = gg, width = 10, height = 8)
  }

  message("VI. ", i, " / ", nrow(emdat.data.filtered))
}

save(emdat.data.filtered,
     file = paste0("Intermediate outputs/Output from Step 6/",
                   "emdat.data.filtered after Step VI ", date_append_output))


# XI. Fill ADM1 exposure columns -----------------------------------------------

for (surge in surge.period.vec) {
  for (sfx in c("adm1.if.available", "adm1.else.national")) {
    emdat.data.filtered[[paste0("pop.exposed.", sfx, ".", surge)]] <- NA_real_
    for (j in 1:5)
      for (yr in c(2050, 2100))
        emdat.data.filtered[[paste0("base.", yr, ".SSP", j,
                                    ".pop.exposed.", sfx, ".", surge)]] <- NA_real_
  }
  if (surge %in% surge.period.vec.future) {
    for (root in slr.colnames.root)
      for (sfx in c("adm1.if.available", "adm1.else.national"))
        emdat.data.filtered[[paste0(root, ".pop.exposed.", sfx, ".", surge)]] <- NA_real_
    for (pscol in slr.popscale.colnames[grepl(surge, slr.popscale.colnames)])
      for (sfx in c("adm1.if.available", "adm1.else.national"))
        emdat.data.filtered[[gsub("\\.pop\\.",
                                   paste0(".pop.exposed.", sfx, "."), pscol)]] <- NA_real_
  }
}

for (i in seq_len(nrow(emdat.data.filtered))) {

  tiles_covered <- unlist(emdat.data.filtered$tiles_covered.adm1.alt[i])
  iso_code      <- emdat.data.filtered$ISO[i]
  emdat.year    <- emdat.data.filtered$Start.Year[i]

  for (surge in surge.period.vec) {

    if (length(tiles_covered) == 0) {
      # Fall back to ADM0
      col_adm0 <- paste0("pop.exposed.adm0.", surge)
      emdat.data.filtered[i, paste0("pop.exposed.adm1.else.national.", surge)] <-
        emdat.data.filtered[i, col_adm0]
      emdat.data.filtered[i, paste0("pop.exposed.adm1.if.available.", surge)] <- NA_real_

      for (j in 1:5) for (yr in c(2050, 2100)) {
        col_adm0 <- paste0("base.", yr, ".SSP", j, ".pop.exposed.adm0.", surge)
        emdat.data.filtered[i, paste0("base.", yr, ".SSP", j,
                                       ".pop.exposed.adm1.else.national.", surge)] <-
          emdat.data.filtered[i, col_adm0]
        emdat.data.filtered[i, paste0("base.", yr, ".SSP", j,
                                       ".pop.exposed.adm1.if.available.", surge)] <- NA_real_
      }

      if (surge %in% surge.period.vec.future) {
        for (root in slr.colnames.root) {
          col_adm0 <- paste0(root, ".pop.exposed.adm0.", surge)
          emdat.data.filtered[i, paste0(root, ".pop.exposed.adm1.else.national.", surge)] <-
            emdat.data.filtered[i, col_adm0]
          emdat.data.filtered[i, paste0(root, ".pop.exposed.adm1.if.available.", surge)] <- NA_real_
        }
        for (pscol in slr.popscale.colnames[grepl(surge, slr.popscale.colnames)]) {
          col_adm0 <- gsub("\\.pop\\.", ".pop.exposed.adm0.", pscol)
          emdat.data.filtered[i, gsub("\\.pop\\.", ".pop.exposed.adm1.else.national.", pscol)] <-
            emdat.data.filtered[i, col_adm0]
          emdat.data.filtered[i, gsub("\\.pop\\.", ".pop.exposed.adm1.if.available.", pscol)] <- NA_real_
        }
      }

    } else {
      # Sum tiles within ADM1 region
      t_rows <- tile_match_rows(tiles_covered, iso_code)

      col_lkp <- if (emdat.year < 2000)
        paste0("base.", emdat.year, ".pop.", surge) else
        paste0("base.", emdat.year, ".SSP1.pop.", surge)

      pop <- sum(seg_adm_pop_exposed.df[t_rows,
                 which(colnames(seg_adm_pop_exposed.df) == col_lkp)], na.rm = TRUE)
      emdat.data.filtered[i, paste0("pop.exposed.adm1.else.national.", surge)] <- pop
      emdat.data.filtered[i, paste0("pop.exposed.adm1.if.available.", surge)]  <- pop

      for (j in 1:5) for (yr in c(2050, 2100)) {
        col_lkp <- paste0("base.", yr, ".SSP", j, ".pop.", surge)
        pop <- sum(seg_adm_pop_exposed.df[t_rows,
                   which(colnames(seg_adm_pop_exposed.df) == col_lkp)], na.rm = TRUE)
        emdat.data.filtered[i, paste0("base.", yr, ".SSP", j,
                                       ".pop.exposed.adm1.else.national.", surge)] <- pop
        emdat.data.filtered[i, paste0("base.", yr, ".SSP", j,
                                       ".pop.exposed.adm1.if.available.", surge)]  <- pop
      }

      if (surge %in% surge.period.vec.future) {
        for (root in slr.colnames.root) {
          col_lkp <- paste0(root, ".pop.", surge)
          pop <- sum(seg_adm_pop_exposed.df[t_rows,
                     which(colnames(seg_adm_pop_exposed.df) == col_lkp)], na.rm = TRUE)
          emdat.data.filtered[i, paste0(root, ".pop.exposed.adm1.else.national.", surge)] <- pop
          emdat.data.filtered[i, paste0(root, ".pop.exposed.adm1.if.available.", surge)]  <- pop
        }
        for (pscol in slr.popscale.colnames[grepl(surge, slr.popscale.colnames)]) {
          pop <- sum(seg_adm_pop_exposed.df[t_rows,
                     which(colnames(seg_adm_pop_exposed.df) == pscol)], na.rm = TRUE)
          emdat.data.filtered[i, gsub("\\.pop\\.", ".pop.exposed.adm1.else.national.", pscol)] <- pop
          emdat.data.filtered[i, gsub("\\.pop\\.", ".pop.exposed.adm1.if.available.", pscol)]  <- pop
        }
      }
    }
  }
  message("VII. ", i, " / ", nrow(emdat.data.filtered))
}


# XII. Pool ADM1 exposure columns for multi-country events --------------------

adm1_exposure_cols <- colnames(emdat.data.filtered)[
  grepl("pop\\.exposed\\.adm1", colnames(emdat.data.filtered))]

for (col in adm1_exposure_cols)
  emdat.data.filtered[[paste0(col, ".pooled")]] <- NA_real_

multi.country.events <- list()
k <- 0L

for (i in seq_len(nrow(emdat.data.filtered))) {

  dis_root     <- substr(emdat.data.filtered$DisNo.[i], 1, 9)
  dis_rows     <- grep(dis_root, emdat.data.filtered$DisNo.)
  already_seen <- any(sapply(multi.country.events,
                              function(x) any(grepl(dis_root, x))))

  if (length(dis_rows) > 1 && !already_seen) {
    k <- k + 1L
    multi.country.events[[k]] <- emdat.data.filtered$DisNo.[dis_rows]
    matched_rows <- match(unlist(multi.country.events[[k]]),
                          emdat.data.filtered$DisNo.)

    for (col in adm1_exposure_cols) {
      if (grepl("if\\.available", col)) {
        vals <- emdat.data.filtered[matched_rows, col]
        emdat.data.filtered[i, paste0(col, ".pooled")] <-
          if (any(is.na(vals))) NA_real_ else sum(vals, na.rm = TRUE)
      } else {
        emdat.data.filtered[i, paste0(col, ".pooled")] <-
          sum(emdat.data.filtered[matched_rows, col], na.rm = TRUE)
      }
    }

  } else if (length(dis_rows) == 1) {
    for (col in adm1_exposure_cols)
      emdat.data.filtered[i, paste0(col, ".pooled")] <-
        emdat.data.filtered[i, col]
  }
}


# XIII. Flatten list columns and prepare final output -------------------------
#
# Fix vs original: paste(unlist(lapply(...)), collapse=",") collapses all rows
# into one string. Now uses sapply to get one string per row.

emdat.data.filtered$tiles_covered.adm1.flat <-
  sapply(emdat.data.filtered$tiles_covered.adm1.alt,
         function(x) paste(unlist(x), collapse = ","))

emdat.data.filtered$tiles_covered.local.flat <-
  sapply(emdat.data.filtered$tiles_covered.local,
         function(x) paste(unlist(x), collapse = ","))

emdat.data.filtered$tiles_excluded.local.flat <-
  sapply(emdat.data.filtered$tiles_excluded.local,
         function(x) paste(unlist(x), collapse = ","))

emdat.data.filtered$adm1.gid.flat <-
  sapply(emdat.data.filtered$adm1.gid,
         function(x) paste(unlist(x), collapse = ","))

emdat.data.filtered$adm1.name.flat <-
  sapply(emdat.data.filtered$adm1.name,
         function(x) paste(unlist(x), collapse = ","))

emdat.data.filtered$geocode.rows.flat <-
  sapply(emdat.data.filtered$geocode.rows,
         function(x) paste(unlist(x), collapse = ","))

# Drop list columns before saving (can't go into xlsx)
list_cols <- c("tiles_covered.adm1", "tiles_covered.adm1.alt",
               "tiles_covered.local", "tiles_excluded.local",
               "adm1.name", "adm1.gid", "geocode.rows", "local.bbox")
emdat.data.filtered.to.save <-
  emdat.data.filtered[, setdiff(colnames(emdat.data.filtered), list_cols)]

# Rename base.* → popscale.* for clarity
colnames(emdat.data.filtered.to.save) <-
  gsub("^base\\.", "popscale.",
       colnames(emdat.data.filtered.to.save))


# XIV. Build composite local.else.adm1.else.national columns ------------------
#
# Priority: local > ADM1 > ADM0.
# Uses ADM1 when local < total deaths (implausibly small), then ADM0 if still
# implausibly small.

cols_to_fix <- grep("local\\.else\\.national",
                    colnames(emdat.data.filtered.to.save))

for (col_no in cols_to_fix) {

  col_name  <- colnames(emdat.data.filtered.to.save)[col_no]
  new_name  <- gsub("local\\.else\\.national",
                    "local.else.adm1.else.national", col_name)
  adm1_name <- gsub("local\\.else\\.national",
                    "adm1.else.national", col_name)
  adm0_name <- gsub("local\\.else\\.national",
                    "adm0", col_name)

  emdat.data.filtered.to.save[[new_name]] <-
    emdat.data.filtered.to.save[[col_name]]

  # Step 1: upgrade to ADM1 where local < deaths
  if (adm1_name %in% colnames(emdat.data.filtered.to.save)) {
    rows_fix <- which(
      emdat.data.filtered.to.save[[new_name]] <
        emdat.data.filtered.to.save$total.deaths.na.as.0)
    emdat.data.filtered.to.save[rows_fix, new_name] <-
      emdat.data.filtered.to.save[rows_fix, adm1_name]
  }

  # Step 2: upgrade to ADM0 where still < deaths
  if (adm0_name %in% colnames(emdat.data.filtered.to.save)) {
    rows_fix2 <- which(
      emdat.data.filtered.to.save[[new_name]] <
        emdat.data.filtered.to.save$total.deaths.na.as.0)
    emdat.data.filtered.to.save[rows_fix2, new_name] <-
      emdat.data.filtered.to.save[rows_fix2, adm0_name]
  }
}

# Summary: how many events still have zero exposure in composite columns?
# Fix vs original: loop variable was `col` not `col.no`
else_national_cols <- grep("else\\.national",
                            colnames(emdat.data.filtered.to.save))
else_zero_counts <- sapply(else_national_cols, function(col_no) {
  col_name <- colnames(emdat.data.filtered.to.save)[col_no]
  sum(emdat.data.filtered.to.save[[col_name]] == 0, na.rm = TRUE)
})
message("Columns with zero-exposure events: ",
        sum(else_zero_counts > 0), " / ", length(else_zero_counts))


# XV. Write output -------------------------------------------------------------

write.xlsx(
  emdat.data.filtered.to.save,
  paste0("Intermediate outputs/Output from Step 6/",
         "EMDAT events with exposed pops ", date_append_output, ".xlsx")
)
