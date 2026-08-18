##########################################
# Step 5b - Update geocoding             #
#                                        #
# 2026-06-24                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# Incremental update to the geocode database after changes to osm_replace.csv
# or new events added to the EMDAT database.
#
# Workflow:
#   1. Make corrections/additions to Inputs/EM-DAT data/osm_replace.csv.
#   2. Re-run Step 4 (to regenerate the location lists with the new overrides).
#   3. Run this script. It reads the existing "no events" geocode database,
#      identifies locations that are new (need geocoding) or removed (need
#      dropping), and only calls the OSM API for new ones.
#
# Alternatively, start here directly to use the 2026-06-24 geocode database
# that has already been developed, without re-running Step 5a.

date_append_output <- "2026-06-24"

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

source("5x_shared_code.R")

dir.create("Intermediate outputs/Output from Step 5",
           recursive = TRUE, showWarnings = FALSE)


# I. Load existing geocode database and compare to current location list -------

old.geocode.results <-
  read_excel("Intermediate outputs/Output from Step 5/Geocode database no events 2026-06-24.xlsx")

current_locs <- trimws(location_split_no_generic_no_parentheses_country.fixed.unique)
old_locs     <- trimws(old.geocode.results$original_location)

# Locations in current list but not in old database → need geocoding
geocode.add.index <- which(!current_locs %in% old_locs)

# Locations in old database but no longer in current list → drop
geocode.subtract.index <- which(!old_locs %in% current_locs)

message(length(geocode.add.index),   " locations to add")
message(length(geocode.subtract.index), " locations to remove")

# Start from the retained subset of the old database
new.geocode.results <-
  old.geocode.results[-geocode.subtract.index, , drop = FALSE]


# II. Geocode new locations ---------------------------------------------------

if (length(geocode.add.index) > 0) {
  geocode_results_df_new <- geocode_range(geocode.add.index)
  geocode_results_df_new$local.polygon.path <- local.polygon(geocode_results_df_new)
  message("New locations geocoded: ", nrow(geocode_results_df_new))
} else {
  geocode_results_df_new <- NULL
  message("No new locations to geocode.")
}


# III. Helper: assign item_number and disaster_number to a results data frame -
#
# For each row, finds the matching unique location index and looks up which
# EMDAT events reference it via location_index_map.
#
# If a location has no match in the unique location list, that row is skipped
# (item_number/disaster_number left as NA) with a warning, rather than halting
# the loop -- every other row still gets processed.

assign_disaster_metadata <- function(results_df) {
  results_df$item_number    <- NA_character_
  results_df$disaster_number <- NA_character_

  for (i in seq_len(nrow(results_df))) {
    location <- results_df$original_location[i]

    matches <- which(
      trimws(location_split_no_generic_no_parentheses_country.fixed.unique) ==
        trimws(location)
    )

    if (length(matches) == 0) {
      warning("No match in unique location list for row ", i, ": '", location,
              "' — item_number and disaster_number left as NA")
      next
    }

    event_indices <- unlist(location_index_map[matches])

    results_df$item_number[i]    <- paste(event_indices, collapse = ",")
    results_df$disaster_number[i] <-
      paste(emdat.data.filtered$DisNo.[event_indices], collapse = ",")
  }

  results_df
}

# Reassign metadata for both retained old rows and newly geocoded rows
new.geocode.results    <- assign_disaster_metadata(new.geocode.results)

if (!is.null(geocode_results_df_new)) {
  geocode_results_df_new <- assign_disaster_metadata(geocode_results_df_new)
}


# IV. Merge old (retained) and new results ------------------------------------

if (!is.null(geocode_results_df_new) && nrow(geocode_results_df_new) > 0) {

  # Flatten bounding box list column before binding (only needed for new rows;
  # old rows loaded from XLSX already have it as a flat string)
  geocode_results_df_new$boundingbox.flat <-
    sapply(geocode_results_df_new$boundingbox,
           function(x) paste(unlist(x), collapse = ","))

  geocode_results_df_new <-
    geocode_results_df_new[, setdiff(colnames(geocode_results_df_new), "boundingbox")]

  geocode.results.merged <- bind_rows(new.geocode.results, geocode_results_df_new)

} else {
  geocode.results.merged <- new.geocode.results
}

message("Total rows in merged database: ", nrow(geocode.results.merged))


# V. Save outputs -------------------------------------------------------------

write.xlsx(
  geocode.results.merged,
  paste0("Intermediate outputs/Output from Step 5/",
         "Geocode database with events ", date_append_output, ".xlsx")
)

write.xlsx(
  geocode.results.merged[,
    setdiff(colnames(geocode.results.merged), c("disaster_number", "item_number"))],
  paste0("Intermediate outputs/Output from Step 5/",
         "Geocode database no events ", date_append_output, ".xlsx")
)


# VI. Debugging helpers (run interactively as needed) -------------------------

# Find geocoded locations associated with a specific disaster number:
# geocode.results.merged$address[
#   grep("2020-0051-SWE", geocode.results.merged$disaster_number)]
#
# geocode.results.merged$address[
#   grep("2018-0170-USA", geocode.results.merged$disaster_number)]
#
# geocode.results.merged$address[
#   grep("2018-0194-FJI", geocode.results.merged$disaster_number)]
