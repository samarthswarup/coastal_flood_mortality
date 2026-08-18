###########################################
# Table S3 - Backfilled HDI/GNI data      #
#                                         #
# 2026-07-14                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding table script, run independently of the numbered pipeline.
#
# The live backfill logic lives in Step 7: for any event before 2022, if
# the event's Start.Year predates the earliest year for which that
# country has HDI/GNI data in hdr.data, Step 7 substitutes the earliest
# available year's value instead. This script detects that condition
# after the fact and tabulates it -- it does not change or duplicate what
# Step 7 already computes.
#
# Draws on:
#   - Step 7's final event-level output (Start.Year, hdr.country.code,
#     ISO, Country) -- already final, no re-derivation of the HDI/GNI
#     matching/merging logic itself.
#   - The same raw hdr.data input Step 7 reads (World Bank HDR data), used
#     only to compute "earliest year of data available per country per
#     indicator" -- the exact same formula Step 7 already uses inline
#     (min(hdr.data$year[country & indicatorCode])), just applied here to
#     detect which events triggered it, not to recompute HDI/GNI values
#     themselves.

rm(list = ls())

library(dplyr)
library(readxl)
library(tidyr)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 7 run to pull the final event-level
# output from
step7_date_append_output <- "2026-07-05"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# SECTION I: LOAD UPSTREAM OUTPUT + RAW HDR INPUT
# =============================================================================

emdat.data.filtered <- read_excel(
  paste0("Intermediate outputs/Output from Step 7/",
         "EMDAT events with exposed pops, HDI GNI and prev events ",
         step7_date_append_output, ".xlsx")
)

hdr.data <- read.csv("Inputs/World Bank pop data/HDR data 2025-01-16.csv")

# =============================================================================
# SECTION II: EARLIEST AVAILABLE YEAR PER COUNTRY PER INDICATOR
# (identical formula to Step 7's inline lookup)
# =============================================================================

earliest_year_table <- hdr.data %>%
  filter(indicatorCode %in% c("hdi", "gnipc")) %>%
  group_by(countryIsoCode, indicatorCode) %>%
  summarise(earliest_year = min(year, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicatorCode, values_from = earliest_year,
              names_prefix = "earliest_year_")

# =============================================================================
# SECTION III: FLAG BACKFILLED EVENTS
# (same eligibility rules as Step 7's loop: exclude PRK, pre-1990, and
# events >= 2022 which don't go through the earliest-year substitution)
# =============================================================================

eligible <- emdat.data.filtered %>%
  filter(hdr.country.code != "PRK",
         Start.Year >= 1990,
         Start.Year < 2022) %>%
  left_join(earliest_year_table, by = c("hdr.country.code" = "countryIsoCode")) %>%
  mutate(
    hdi_backfilled = !is.na(earliest_year_hdi)   & Start.Year < earliest_year_hdi,
    gni_backfilled = !is.na(earliest_year_gnipc) & Start.Year < earliest_year_gnipc
  )

# =============================================================================
# SECTION IV: TABULATE (Country/territory x Data type x Earliest year x
# Number of events affected)
# =============================================================================

# Country/territory display name for each hdr.country.code: look up from
# EMDAT events recorded directly under that ISO code (i.e. the data-owning
# country's own name, not a mapped territory's name).
country_name_lookup <- emdat.data.filtered %>%
  filter(ISO == hdr.country.code) %>%
  distinct(hdr.country.code, Country) %>%
  distinct(hdr.country.code, .keep_all = TRUE)

hdi_rows <- eligible %>%
  filter(hdi_backfilled) %>%
  group_by(hdr.country.code, earliest_year_hdi) %>%
  summarise(n_events = n(), .groups = "drop") %>%
  transmute(Country_Code = hdr.country.code,
            `Data type` = "HDI",
            `Earliest year for which data are available` = earliest_year_hdi,
            `Number of events affected` = n_events)

gni_rows <- eligible %>%
  filter(gni_backfilled) %>%
  group_by(hdr.country.code, earliest_year_gnipc) %>%
  summarise(n_events = n(), .groups = "drop") %>%
  transmute(Country_Code = hdr.country.code,
            `Data type` = "GNI",
            `Earliest year for which data are available` = earliest_year_gnipc,
            `Number of events affected` = n_events)

table_s3 <- bind_rows(hdi_rows, gni_rows) %>%
  left_join(country_name_lookup, by = c("Country_Code" = "hdr.country.code")) %>%
  mutate(
    `Country or territory with data available` = ifelse(
      is.na(Country), Country_Code, paste0(Country, " (", Country_Code, ")")
    )
  ) %>%
  arrange(`Country or territory with data available`, `Data type`) %>%
  select(`Country or territory with data available`, `Data type`,
         `Earliest year for which data are available`, `Number of events affected`)

# Flag any code with no matching EMDAT-native name (fell back to bare ISO
# code) so it can be checked/fixed manually rather than silently shipped
unresolved_names <- table_s3 %>%
  filter(!grepl("\\(", `Country or territory with data available`))
if (nrow(unresolved_names) > 0) {
  warning("Could not resolve a display name for: ",
          paste(unique(unresolved_names$`Country or territory with data available`), collapse = ", "),
          " -- check manually.")
}

# =============================================================================
# SECTION V: WRITE OUTPUT
# =============================================================================

write.csv(
  table_s3,
  file = paste0("Output/Tables/Table S3 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S3 written to Output/Tables/Table S3 ", date_append_output, ".csv")
