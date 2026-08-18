##########################################
# Step 5a - EMDAT Geocoding              #
#                                        #
# 2026-06-24                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# Geocodes all unique locations produced by Step 4, using the geocode_range()
# and local.polygon() functions defined in 5x_shared_code.R.
#
# This script runs from scratch — geocoding every location. It is time-intensive
# (one OSM API call per location). For incremental updates after osm_replace.csv
# changes, use 5b_EMDAT_update_geocoding.R instead.
#
# Workflow after running:
#   1. Inspect output: table(geocode_results$type) to find unexpected place types
#      (e.g. bus stop, bar, bank) and blank rows.
#   2. Add corrections to Inputs/EM-DAT data/osm_replace.csv.
#   3. Re-run Step 4, then run 5b to update only the changed locations.

date_append_output <- "2026-06-24"

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

source("5x_shared_code.R")

dir.create("Intermediate outputs/Output from Step 5",
           recursive = TRUE, showWarnings = FALSE)
dir.create("Intermediate outputs/Output from Step 5/Local polygons",
           recursive = TRUE, showWarnings = FALSE)


# I. Geocode all locations in batches -----------------------------------------
#
# Split into batches so that if a batch fails or is interrupted, earlier results
# are not lost. range.test is a debugging tool — comment it out for production
# runs; those indices are already covered by range4.

n_locs <- length(location_split_no_generic_no_parentheses_country.fixed.unique)
message("Total unique locations to geocode: ", n_locs)

range1 <- 1:10
range2 <- 11:500
range3 <- 501:1000
range4 <- 1001:n_locs

# range.test <- 2945:3000  # debugging only — indices covered by range4

geocode_results_df1 <- geocode_range(range1)
geocode_results_df1$local.polygon.path <- local.polygon(geocode_results_df1)
message("Batch 1 complete (", length(range1), " locations)")

geocode_results_df2 <- geocode_range(range2)
geocode_results_df2$local.polygon.path <- local.polygon(geocode_results_df2)
message("Batch 2 complete (", length(range2), " locations)")

geocode_results_df3 <- geocode_range(range3)
geocode_results_df3$local.polygon.path <- local.polygon(geocode_results_df3)
message("Batch 3 complete (", length(range3), " locations)")

geocode_results_df4 <- geocode_range(range4)
geocode_results_df4$local.polygon.path <- local.polygon(geocode_results_df4)
message("Batch 4 complete (", length(range4), " locations)")

geocode_results <- bind_rows(
  geocode_results_df1,
  geocode_results_df2,
  geocode_results_df3,
  geocode_results_df4
)

message("Total geocoded rows: ", nrow(geocode_results))


# II. Flatten bounding box list column for XLSX export ------------------------

geocode_results$boundingbox.flat <-
  sapply(geocode_results$boundingbox,
         function(x) paste(unlist(x), collapse = ","))


# III. Ensure every disaster number is recorded for its locations --------------
#
# geocode_range() assigns disaster numbers at geocoding time, but if the same
# location appears in multiple disasters, later ones may have been missed.
# This loop audits every event and appends any missing disaster numbers.
#
# Matching uses exact comma-separated tokens (not a plain substring search),
# so a disaster number that happens to be a substring of another one isn't
# falsely treated as already present.

add_disaster_if_missing <- function(current_str, disaster_num) {
  # Check for exact match as a comma-separated token
  already_present <- grepl(
    paste0("(^|,)", disaster_num, "(,|$)"),
    current_str
  )
  if (already_present) return(current_str)
  paste0(current_str, ",", disaster_num)
}

for (i in seq_len(nrow(emdat.data.filtered))) {
  disaster.number <- emdat.data.filtered$DisNo.[i]
  locations.fixed <- unlist(location_split_no_generic_no_parentheses_country.fixed[[i]])

  for (loc in locations.fixed) {
    geocode.row <- match(
      normalize_spaces(loc),
      normalize_spaces(geocode_results$original_location)
    )
    if (is.na(geocode.row)) next  # location didn't geocode successfully

    geocode_results$disaster_number[geocode.row] <-
      add_disaster_if_missing(geocode_results$disaster_number[geocode.row],
                              disaster.number)
  }
}


# IV. Consolidate duplicate address rows --------------------------------------
#
# Duplicate addresses can arise when the same place name appears across multiple
# events that were geocoded in different batches. Keep the first occurrence,
# merge all associated disaster numbers into it, and drop the rest.
#
# Note: duplicate *geometries* under different names (e.g. "Boston, USA" vs
# "Boston, Massachusetts, USA") are intentionally kept — Step 6 handles
# overlapping bounding boxes.

geocode_results <- geocode_results %>%
  mutate(address_trim = str_trim(address)) %>%
  group_by(address_trim) %>%
  mutate(
    disaster_number = paste(
      unique(unlist(strsplit(paste(disaster_number, collapse = ","), ","))),
      collapse = ","
    )
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(-address_trim)

message("Rows after deduplication: ", nrow(geocode_results))


# V. Save outputs -------------------------------------------------------------

# With-events file: passed to Step 6 for exposed population calculations
write.xlsx(
  geocode_results[, setdiff(colnames(geocode_results), "boundingbox")],
  paste0("Intermediate outputs/Output from Step 5/",
         "Geocode database with events ", date_append_output, ".xlsx")
)

# No-events file: used as the starting point for 5b incremental updates.
# Disaster numbers are intentionally excluded because they may change if the
# EMDAT database is updated and Step 4 is re-run.
write.xlsx(
  geocode_results[, setdiff(colnames(geocode_results),
                             c("boundingbox", "disaster_number", "item_number"))],
  paste0("Intermediate outputs/Output from Step 5/",
         "Geocode database no events ", date_append_output, ".xlsx")
)


# VI. Interactive QC helper ---------------------------------------------------
#
# Use this block interactively to spot-check a specific location.
# Not run automatically.

# table(geocode_results$type)  # look for unexpected place types
#
# test_location <- "Mananara Avaratra, Madagascar"
# temp_df_test  <- data.frame(address = test_location, stringsAsFactors = FALSE)
# test_geocode  <- tidygeocoder::geocode(temp_df_test, address = address,
#                                        method = "osm", full_results = TRUE)
# world_map     <- map_data("world")
# lat_center    <- test_geocode$lat
# long_center   <- test_geocode$long
# ggplot() +
#   geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
#                fill = "gray90", color = "white") +
#   geom_point(data = test_geocode, aes(x = long, y = lat), color = "red", size = 3) +
#   coord_fixed(1.3,
#               xlim = c(long_center - 5, long_center + 5),
#               ylim = c(lat_center - 5, lat_center + 5)) +
#   theme_minimal() +
#   labs(title = paste("Location of", test_location))
