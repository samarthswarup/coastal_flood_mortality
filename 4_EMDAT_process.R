##########################################
# Step 4 - EMDAT process                 #
#                                        #
# 2026-06-24                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# This code takes raw EMDAT data, subsets it based on disaster types of
# interest, and extracts subnational locations for geocoding. Locations are
# split on commas (respecting parentheses), stripped of generic descriptors
# (e.g. "province", "district") and parenthetical content, then combined with
# their country name for OSM matching. A manual override file (osm_replace.csv)
# substitutes EMDAT spellings that fail OSM lookup with confirmed alternatives.
#
# The key output is location_split_no_generic_no_parentheses_country.fixed,
# a jagged list (one element per EMDAT event) of cleaned location strings,
# passed to 5a_EMDAT_geocoding.R for geocoding.

rm(list = ls())

library(readxl)
library(stringr)
library(openxlsx)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

date_append_output <- "2026-06-24"

dir.create("Intermediate outputs/Output from Step 4", recursive = TRUE, showWarnings = FALSE)
dir.create("Output", recursive = TRUE, showWarnings = FALSE)


# I. Read inputs ---------------------------------------------------------------

emdat.data <- read_excel("Inputs/EM-DAT data/public_emdat_incl_hist_2025-01-05.xlsx")

country.data <-
  read.csv(paste0("Inputs/Identify mismatched and missing countries - step 3/",
                  "SLIIDERS country codes with names and territorial mapping.csv"))

osm.replace <- read.csv("Inputs/EM-DAT data/osm_replace.csv")


# II. Subset EMDAT to coastal disaster types from 1985 onward -----------------

disasters.consider <- c("Tropical cyclone",
                         "Coastal flood",
                         "Extra-tropical storm",
                         "Tsunami",
                         "Storm surge")

emdat.data.filtered <-
  emdat.data[emdat.data$`Disaster Subtype` %in% disasters.consider &
               emdat.data$`Start Year` >= 1985, ]


# III. Create deaths columns ---------------------------------------------------
#
# Keep the original reported deaths, and make a second version where NAs are
# treated as 0 (used in models that assume no report = no deaths).

emdat.data.filtered$total.deaths.reported <- emdat.data.filtered$`Total Deaths`
emdat.data.filtered$total.deaths.na.as.0  <-
  ifelse(is.na(emdat.data.filtered$`Total Deaths`), 0,
         emdat.data.filtered$`Total Deaths`)


# IV. Map EMDAT countries to SLIIDERS country codes ---------------------------
#
# EMDAT uses country names; SLIIDERS uses ISO-style codes. Some EMDAT entries
# cover areas that map to multiple SLIIDERS codes (e.g. "Netherlands Antilles"
# → Curacao, Aruba, etc.), so SLIIDERS.country.code is stored as a list column.
#
# Lookup tries the ISO 3166 name first, then the alternate name field.

emdat.data.filtered$SLIIDERS.country.code <- vector("list", nrow(emdat.data.filtered))

for (i in seq_len(nrow(emdat.data.filtered))) {
  emdat.country      <- emdat.data.filtered$Country[i]
  country.code.index <- which(country.data$Name..ISO.3166.if.available. == emdat.country)

  if (length(country.code.index) == 0)
    country.code.index <- which(country.data$Alternate.name == emdat.country)

  emdat.data.filtered$SLIIDERS.country.code[[i]] <-
    country.data$SLIIDERS.country.code[country.code.index]
}

## Print any events where SLIIDERS code doesn't match the EMDAT ISO code
## (used for QC — these are expected for territories like Canary Islands)
## Compared against SLIIDERS.country.code.flat below (the flattened string
## version), since SLIIDERS.country.code itself is a list column.
emdat.data.filtered$SLIIDERS.country.code.flat <-
  sapply(emdat.data.filtered$SLIIDERS.country.code,
         function(x) paste(unlist(x), collapse = ","))

countries.mismatch <-
  emdat.data.filtered$ISO[
    emdat.data.filtered$ISO != emdat.data.filtered$SLIIDERS.country.code.flat]
message(length(countries.mismatch), " events have EMDAT ISO ≠ SLIIDERS code (expected for territories)")

## Print events with no SLIIDERS match — should all be landlocked countries
no.match <- emdat.data.filtered$Country[
  sapply(emdat.data.filtered$SLIIDERS.country.code, length) == 0]
if (length(no.match) > 0) {
  message("Events with no SLIIDERS country match (should be landlocked): ",
          paste(unique(no.match), collapse = ", "))
}

## Remove events in landlocked countries (no SLIIDERS match)
emdat.data.filtered <-
  emdat.data.filtered[
    sapply(emdat.data.filtered$SLIIDERS.country.code, length) > 0, ]


# V. Set up location strings for geocoding ------------------------------------

## Generic descriptor words that interfere with OSM place matching.
## Comparison is done case-insensitively, so only lowercase versions are needed.
generic_words <- tolower(c(
  "islands", "island", "atolls", "atoll", "provinces", "province",
  "districts", "district", "region", "regions", "county", "counties",
  "isl.", "isl", "state", "states", "territory", "territories",
  "municipality", "city", "cities", "town", "towns", "village", "villages",
  "area", "zone", "division", "divisions", "prov", "dist"
))

## Split a location string on commas, but not commas inside parentheses
split_location_by_commas <- function(location) {
  if (is.na(location)) return(character(0))
  str_trim(unlist(strsplit(location, ",(?![^()]*\\))", perl = TRUE)))
}

## Remove generic descriptor words from a vector of place-name strings.
## Returns cleaned parts (generics stripped) and the removed generic words.
identify_and_remove_generics <- function(parts) {
  if (length(parts) == 0)
    return(list(location_split_no_generic = parts, generic = character(0)))

  cleaned_parts <- character(0)
  generic_parts <- rep(NA_character_, length(parts))

  for (i in seq_along(parts)) {
    words       <- unlist(strsplit(as.character(parts[[i]]), "\\s+"))
    lower_words <- tolower(words)
    is_generic  <- lower_words %in% generic_words   # generic_words is already
                                                     # lowercase, so compare
                                                     # lowercased words to it

    if (any(is_generic)) {
      generic_parts[i] <- paste(words[is_generic], collapse = " ")
      non_generic      <- words[!is_generic]
      if (length(non_generic) > 0)
        cleaned_parts <- c(cleaned_parts, paste(non_generic, collapse = " "))
    } else {
      cleaned_parts <- c(cleaned_parts, parts[[i]])
    }
  }

  list(location_split_no_generic = cleaned_parts, generic = generic_parts)
}

## Remove parenthetical content from place-name strings, storing what was removed
remove_and_store_parentheses <- function(parts) {
  cleaned_parts     <- str_trim(gsub("\\s*\\([^)]+\\)", "", parts))
  parentheses_parts <- ifelse(grepl("\\(", parts),
                              gsub(".*\\(([^)]+)\\).*", "\\1", parts),
                              NA_character_)
  list(cleaned_parts = cleaned_parts, parentheses_parts = parentheses_parts)
}

## Apply the pipeline: split → remove generics → remove parentheses
location_split <- lapply(emdat.data.filtered$Location, split_location_by_commas)

processed_split            <- lapply(location_split, identify_and_remove_generics)
location_split_no_generic  <- lapply(processed_split, `[[`, "location_split_no_generic")
# generic saved for reference but not used downstream
generic                    <- lapply(processed_split, `[[`, "generic")

processed_parentheses      <- lapply(location_split_no_generic, remove_and_store_parentheses)
location_split_no_generic_no_parentheses <-
  lapply(processed_parentheses, `[[`, "cleaned_parts")


## Simplify country names for OSM matching (remove formal qualifiers)
country_for_osm <- emdat.data.filtered$Country %>%
  gsub("\\(Bolivarian Republic of\\)", "", .) %>%
  gsub("\\(Islamic Republic of\\)",    "", .) %>%
  gsub("\\(Province of China\\)",      "", .) %>%
  gsub("\\(Kingdom of the Netherlands\\)", "", .) %>%
  gsub("\\(Kingdom of the\\)",         "", .) %>%
  gsub("\\(Plurinational State of\\)", "", .) %>%
  gsub("\\(Federated States of\\)",    "", .) %>%
  str_trim()

## Append country name to each location string for unambiguous OSM lookup
location_split_no_generic_no_parentheses_country <-
  lapply(seq_along(location_split_no_generic_no_parentheses), function(i)
    paste(location_split_no_generic_no_parentheses[[i]],
          country_for_osm[i], sep = ", "))


# VI. Apply OSM location overrides --------------------------------------------
#
# osm_replace.csv maps EMDAT location strings that fail OSM lookup to confirmed
# alternatives. Each row can supply up to 6 replacement strings (to allow one
# location to be expanded into multiple geocodeable sub-locations).
#
# Duplicate emdat_location entries are detected with duplicated(), keeping
# the first occurrence of each. Replacement slots 2-6 are treated as empty
# when they are NA or a zero-length string (read.csv may return either for
# a blank cell), via !is.na(x) && nzchar(x).
#
# Performance: normalize all location strings upfront into a named lookup
# vector rather than re-normalizing inside the double loop.

## Drop fully duplicated emdat_location entries, keeping first occurrence
osm.dup.idx <- which(duplicated(osm.replace$emdat_location))
if (length(osm.dup.idx) > 0) {
  warning(length(osm.dup.idx),
          " duplicate emdat_location entries in osm.replace removed (keeping first):\n",
          paste(osm.replace$emdat_location[osm.dup.idx], collapse = "\n"))
  osm.replace <- osm.replace[-osm.dup.idx, ]
}

normalize_string <- function(s) {
  s <- str_trim(s)
  s <- tolower(s)
  s <- gsub("[[:space:]]+", " ", s)
  s <- gsub(" ", " ", s)       # non-breaking space → regular space
  s <- gsub("[^[:print:]]", "", s)  # remove non-printable characters
  s
}

## Pre-normalize all location strings across all events for fast lookup
## Result: a named list where names are normalized strings and values are
## (event_index, position_within_event) pairs
all_locations_normalized <- lapply(
  seq_along(location_split_no_generic_no_parentheses_country),
  function(j)
    normalize_string(location_split_no_generic_no_parentheses_country[[j]])
)

location_split_no_generic_no_parentheses_country.fixed <-
  location_split_no_generic_no_parentheses_country

replacement_cols <- paste0("new_location_", 1:6)

for (i in seq_len(nrow(osm.replace))) {

  target_norm <- normalize_string(osm.replace$emdat_location[i])

  ## Get replacement strings, skipping empty/NA slots
  replacements <- sapply(replacement_cols, function(col) osm.replace[[col]][i])
  replacements <- replacements[!is.na(replacements) & nzchar(replacements)]
  if (length(replacements) == 0) next

  primary_replacement   <- replacements[1]
  additional_replacements <- replacements[-1]

  ## Find matching positions across all events
  for (j in seq_along(all_locations_normalized)) {
    hit_positions <- which(all_locations_normalized[[j]] == target_norm)

    if (length(hit_positions) > 0) {
      message("OSM override: event ", j, ", osm.replace row ", i)

      ## Replace matched position with primary replacement
      location_split_no_generic_no_parentheses_country.fixed[[j]][hit_positions] <-
        primary_replacement

      ## Append any additional replacements to end of this event's location list
      if (length(additional_replacements) > 0) {
        location_split_no_generic_no_parentheses_country.fixed[[j]] <-
          c(location_split_no_generic_no_parentheses_country.fixed[[j]],
            additional_replacements)
      }
    }
  }
}

## Deduplicate across all events to get the unique set for geocoding
location_split_no_generic_no_parentheses_country.fixed.unique <-
  unique(trimws(unlist(location_split_no_generic_no_parentheses_country.fixed)))

## Build index map: for each unique location, which event indices contain it?
## This tells the geocoding step which disasters are associated with each location.
location_index_map <-
  sapply(location_split_no_generic_no_parentheses_country.fixed.unique, function(loc)
    which(sapply(location_split_no_generic_no_parentheses_country.fixed,
                 function(event_locs) trimws(loc) %in% trimws(event_locs))),
    simplify = FALSE)

names(location_index_map) <- location_split_no_generic_no_parentheses_country.fixed.unique

## Sanity check: any unique locations not mapped to any event?
unmatched <- which(sapply(location_index_map, length) == 0)
if (length(unmatched) > 0)
  warning(length(unmatched), " unique locations not mapped to any event — check osm.replace")


direct_overrides <- list(
  "Móng Cái, Vietnam"                          = "Mong Cai, Quang Ninh, Vietnam",
  "País Vasco/Euskadi, Spain"                  = "Basque Country, Spain",
  "Thu Duc City, Ho Chi Minh City, Vietnam"    = "Thu Duc, Ho Chi Minh City, Vietnam",
  "Tarnopol Oblast, Ukraine"                   = "Ternopil Oblast, Ukraine",
  "Karnaphuli Upazila, Chattogram, Bangladesh" = "Karnaphuli, Chattogram District, Bangladesh",
  "Sitakunda, Chattogram, Bangladesh"          = "Sitakunda Upazila, Chattogram District, Bangladesh",
  "Nacala Velha, Nampula Province, Mozambique" = "Nacala-a-Velha, Nampula, Mozambique",
  "Nam Dinh City, Nam Dinh Province, Vietnam"  = "Nam Dinh, Nam Dinh Province, Vietnam",
  "Kowloon, Hong Kong"                         = "Kowloon City, Hong Kong",
  "Sulaman, Sabah, Malaysia"                   = "Sulaman, Tuaran, Sabah, Malaysia"
)

for (i in seq_along(location_split_no_generic_no_parentheses_country.fixed)) {
  for (old in names(direct_overrides)) {
    hit <- which(trimws(location_split_no_generic_no_parentheses_country.fixed[[i]]) == old)
    if (length(hit) > 0)
      location_split_no_generic_no_parentheses_country.fixed[[i]][hit] <- direct_overrides[[old]]
  }
}

# Rebuild unique list after overrides
location_split_no_generic_no_parentheses_country.fixed.unique <-
  unique(trimws(unlist(location_split_no_generic_no_parentheses_country.fixed)))


# VII. Output files ------------------------------------------------------------

## Filtered EMDAT events (excluding list column which can't be written to xlsx)
write.xlsx(
  emdat.data.filtered[, setdiff(colnames(emdat.data.filtered), "SLIIDERS.country.code")],
  paste0("Intermediate outputs/Output from Step 4/",
         "EMDAT filtered events with deaths ", date_append_output, ".xlsx")
)

## Log of EMDAT ISO codes that differ from matched SLIIDERS codes
## Note: multi-country events (e.g. Netherlands Antilles) will appear here;
## this is expected. Canary Islands (SPI→ESP) is also expected.
write.csv(
  data.frame(
    EMDAT.countries      = unique(countries.mismatch),
    SLIIDERS.countries.matched =
      emdat.data.filtered$SLIIDERS.country.code.flat[
        match(unique(countries.mismatch), emdat.data.filtered$ISO)]
  ),
  file = paste0("Intermediate outputs/Output from Step 4/SLIIDERS country codes subbed into EMDAT events ",
                date_append_output, ".csv")
)

# On successful completion, the following objects are available for Step 5:
#   location_split_no_generic_no_parentheses_country.fixed        (jagged list)
#   location_split_no_generic_no_parentheses_country.fixed.unique (unique strings)
#   location_index_map                                            (event index map)
#   emdat.data.filtered                                           (filtered events)
