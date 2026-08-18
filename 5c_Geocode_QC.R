##########################################
# Step 5c - Geocode QC                   #
#                                        #
# 2026-06-24                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# Quality-control checks on the geocode database produced by 5a/5b.
# Meant to be run interactively, in the same R session right after 5a or 5b,
# since it relies on objects already in memory (location_split_no_generic_no_
# parentheses_country.fixed, emdat.data.filtered) rather than re-reading them
# from disk. It does not set its own working directory for this reason --
# it inherits whatever 5a/5b already set.

# Load the geocode database
geocode_db <- read_excel(
  "Intermediate outputs/Output from Step 5/Geocode database with events 2026-06-24.xlsx"
)

# Flatten all location strings from Step 4 output
all_locations <- trimws(unlist(
  location_split_no_generic_no_parentheses_country.fixed
))
all_locations_unique <- unique(all_locations)

# 1. Strings with no match in geocode DB
not_geocoded <- all_locations_unique[
  !all_locations_unique %in% trimws(geocode_db$original_location)
]
message("Strings not in geocode DB: ", length(not_geocoded))
print(not_geocoded)

# 2. Strings that geocoded to a bad type
bad_types <- c("theatre","car_rental","bank","ngo","government","insurance",
               "artwork","psychotherapist","post_office","pawnbroker","board",
               "defibrillator","information","estate_agent","motorway_junction",
               "halt","farm","place_of_worship","engineer","company","bar",
               "viewpoint","map","hotel","restaurant","aerodrome","bus_stop",
               "station","service_station")

bad_matches <- geocode_db %>%
  filter(type %in% bad_types) %>%
  select(original_location, type, display_name)

message("Bad type matches: ", nrow(bad_matches))
print(bad_matches, n = Inf)

# 3. Encoding mismatches -- strings in location list that look like
#    they should match but don't (contain non-ASCII)
non_ascii <- all_locations_unique[
  grepl("[^\x01-\x7F]", all_locations_unique) &
    !all_locations_unique %in% trimws(geocode_db$original_location)
]
message("Non-ASCII strings with no geocode match: ", length(non_ascii))
print(non_ascii)

# 4. Write full report to CSV for review
# Uncomment the block below to include not-geocoded locations as their own
# rows in the report (each with issue = "not in geocode DB"). If it stays
# commented out, the code below detects that `report` was never created and
# substitutes a single placeholder row saying so, so the omission is visible
# in the CSV rather than silently dropped.
# report <- data.frame(
#   location    = not_geocoded,
#   issue       = "not in geocode DB",
#   type        = NA,
#   display_name = NA
# )
report_bad <- data.frame(
  location     = bad_matches$original_location,
  issue        = "bad osm type",
  type         = bad_matches$type,
  display_name = bad_matches$display_name
)

if (exists("report")) {
  # The block above is uncommented -- include not-geocoded locations as
  # their own rows.
  report_all <- bind_rows(report, report_bad)
} else if (length(not_geocoded) > 0) {
  # Block is commented out AND there are locations that would have gone into
  # it -- note that explicitly instead of silently dropping them.
  message(length(not_geocoded),
          " location(s) were not geocoded, but the `report` block above is ",
          "commented out, so they are NOT itemized in geocode_qc_report.csv ",
          "-- see the single placeholder row instead. Uncomment that block ",
          "to list them individually.")
  report_all <- bind_rows(
    data.frame(
      location     = "NOTE",
      issue        = paste0(length(not_geocoded),
                            " not-geocoded location(s) omitted -- ",
                            "`report` block above is commented out"),
      type         = NA,
      display_name = NA
    ),
    report_bad
  )
} else {
  # Nothing was left un-geocoded, so the commented-out block wouldn't have
  # added anything anyway -- no note needed.
  report_all <- report_bad
}

write.csv(report_all, "Intermediate outputs/Output from Step 5/geocode_qc_report.csv", row.names = FALSE)
message("Full report written to Intermediate outputs/Output from Step 5/geocode_qc_report.csv")
