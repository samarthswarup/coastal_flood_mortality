###########################################
# Table S2 - Territories with missing     #
# demographic data                        #
#                                         #
# 2026-07-14                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding table script, run independently of the numbered pipeline.
# This is a 3-way merge of three already-final pipeline outputs, none of
# which individually is the full table:
#
#   (1) footnote "1" -- WB national population data 1990-1999 missing:
#       Step 3's "Intermediate outputs/Output from Step 3/Countries
#       aggregated for WB pops 1985-1999 <date>.csv" (built from
#       countries.to.replace.WB in 3_Calculate_tile_pop_Claude.R:
#       SLIIDERS tile-bearing territories absent from WBpop.transformed).
#
#   (2) footnote "2" -- SLIIDERS population scale 2000-2100 missing:
#       Step 3's "Intermediate outputs/Output from Step 3/Countries
#       aggregated for pop scale 2000-2100 <date>.csv" (built from
#       pop_scale.data.0.countries: territories where SLIIDERS
#       pop_scale == 0, requiring parent substitution).
#
#   (3) footnote "3" -- WB HDI and GNI data missing:
#       Step 7's "Intermediate outputs/Output from Step 7/Countries
#       aggregated for GNI, HDI, and national pop <date>.csv" (the
#       hdr.country.exceptions block in 7_GNI_HDI_nat_pop.R). This script
#       reads that file as a live input; it does not recompute the
#       EMDAT-to-HDR country matching itself. This footnote is correctly
#       scoped to EMDAT-observed events only (HDI/GNI is only looked up
#       per event), unlike (1)/(2) which cover all SLIIDERS tile-bearing
#       territories regardless of flood history -- this asymmetry is
#       intentional, not a bug (a territory with zero EMDAT events can
#       have footnote 1/2 but never 3).
#
# All three source files are already final -- this script only joins and
# reshapes them against the territorial mapping reference
# (SLIIDERS_country_codes_with_names_and_territorial_mapping.csv) to
# produce the grouped, footnoted display table.
#
# PARENT-COUNTRY OVERRIDE HANDLING: a territory's display parent is NOT
# always country.data's static "Group with (for World Bank)" column.
# Step 7 explicitly reassigns a few territories' HDI/GNI parent away from
# that default -- e.g. TWN and MAC are grouped with CHN in country.data
# (appropriate for WB population/pop-scale matching in Step 3), but Step 7
# overrides both to HKG specifically for HDI/GNI, since mainland China's
# development level doesn't represent Taiwan/Macao's actual socioeconomic
# reality (see 7_GNI_HDI_nat_pop.R ~lines 114-118). This script therefore
# prefers Step 7's ACTUAL assigned parent (hdi_gni_missing's
# wb.undp.countries column) whenever a territory has a footnote-3 entry,
# and only falls back to country.data's default parent for territories
# that only have footnote 1/2.
#
# ALTERNATE-CODE MATCHING: some territories share a POOLED alternate ISO
# code that EMDAT (and therefore hdi_gni_missing) records events under,
# rather than their own SLIIDERS code. Bonaire/Curacao/Aruba/Sint Maarten
# all share Alternate ISO code "ANT" (former Netherlands Antilles).
# Checking only each territory's own SLIIDERS.country.code would (a) miss
# footnote 3 for territories whose own code never appears directly in
# hdi_gni_missing, and (b) miss the Step 7 parent override for territories
# whose own "Group with" column is blank in country.data (Curacao and
# Aruba both are) -- their correct parent (NLD) only surfaces via the
# shared alternate code "ANT" -> "NLD" already present in hdi_gni_missing.
# Every footnote/parent check below therefore considers BOTH a
# territory's primary code and its Alternate ISO code.
#
# NOTE ON ARUBA: Aruba (ABW) does not appear in the raw World Bank HDR
# file by ISO code or by name, exactly like Bonaire/Curacao/Sint Maarten,
# so it correctly receives footnote 3 along with the other three -- this
# matches the SI table (Aruba is listed as "Aruba (ABW)3" there).
#
# NOT REPRODUCED HERE (add manually in the manuscript, not derivable from
# these three files): footnote (a) the Taiwan-specific explanatory note
# ("also missing WB national population data... but this data was
# obtained from the Government of Taiwan"), and footnote (b) the EM-DAT
# ATG/ANT code-mismatch explanatory note. Both are editorial explanations
# of a known discrepancy, not something derivable from data joins.

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(tidyr)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

# Date suffixes identifying which Step 3 / Step 7 runs to pull from
step3_date_append_output <- "2026-07-04"
step7_date_append_output <- "2026-07-05"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# USER CONFIGURATION: parent-country display names (hardcoded -- short,
# stable list; edit if a new parent country ever appears)
# =============================================================================

parent_country_names <- c(
  AUS = "Australia (AUS)",
  CHN = "China (CHN)",
  CYP = "Cyprus (CYP)",
  FIN = "Finland (FIN)",
  FRA = "France (FRA)",
  HKG = "Hong Kong Special Administrative Region of China (HKG)",
  MAR = "Morocco (MAR)",
  NLD = "Netherlands (NLD)",
  NZL = "New Zealand (NZL)",
  NOR = "Norway (NOR)",
  ESP = "Spain (ESP)",
  TUR = "Turkey (TUR)",
  GBR = "United Kingdom (GBR)",
  USA = "United States of America (USA)"
)

# =============================================================================
# SECTION I: LOAD TERRITORIAL MAPPING REFERENCE + THREE SOURCE FILES
# =============================================================================

country.data <- read.csv(
  paste0("Inputs/Identify mismatched and missing countries - step 3/",
         "SLIIDERS country codes with names and territorial mapping.csv")
)

wb_pop_missing <- read.csv(
  paste0("Intermediate outputs/Output from Step 3/Countries aggregated for WB pops 1985-1999 ",
         step3_date_append_output, ".csv")
)

pop_scale_missing <- read.csv(
  paste0("Intermediate outputs/Output from Step 3/Countries aggregated for pop scale 2000-2100 ",
         step3_date_append_output, ".csv")
)

hdi_gni_missing <- read.csv(
  paste0("Intermediate outputs/Output from Step 7/Countries aggregated for GNI, HDI, and national pop ",
         step7_date_append_output, ".csv")
)

# =============================================================================
# USER CONFIGURATION: manual exclusions and display-code overrides
# =============================================================================
# Both handle known quirks in the territorial mapping reference file that
# don't fit the general pattern -- see notes below and in the header.

# Rows to exclude entirely: same-entity coding artifacts, not genuine
# territory-parent groupings. XAD is SLIIDERS's own internal alternate code
# for Cyprus itself (Alternate ISO code = CYP) -- it's the same place, not
# a distinct territory lacking its own data.
excluded_codes <- c("XAD")

# Display-code overrides: for a handful of rows, country.data's PRIMARY
# SLIIDERS.country.code is not the code actually used to refer to that
# territory elsewhere in the pipeline. Canary Islands is a rare case where
# SLIIDERS itself aggregates the territory's geometry with its parent
# (Spain) -- its primary code in country.data is "ESP" (same as Spain),
# with "SPI" (the code used for it elsewhere in the pipeline, e.g.
# 6_Calculate_exposed_pops_Claude.R's "Canary Islands (SPI) special case")
# only appearing as the alternate code. Display SPI for this row instead.
display_code_overrides <- c(SPI = "ESP")  # names = display code, values = SLIIDERS.country.code to match on (with matching Territory.name)
display_code_override_names <- c(SPI = "Canary Islands")

# =============================================================================
# SECTION II: BUILD PER-TERRITORY FOOTNOTE FLAGS
# =============================================================================

# Some territories are recorded in EMDAT (and therefore in hdi_gni_missing)
# under a POOLED alternate code rather than their own SLIIDERS code -- e.g.
# Bonaire/Curacao/Aruba/Sint Maarten all share Alternate ISO code "ANT"
# (former Netherlands Antilles), and EMDAT records events under "ANT"
# directly. Checking only each territory's own SLIIDERS.country.code
# against the three source files misses this -- it would (a) miss
# footnote 3 for any of the four whose own code never appears in
# hdi_gni_missing, and (b) miss the Step 7 parent override entirely for
# territories whose OWN code has a blank "Group with" column in
# country.data (Curacao/Aruba both do), since the override lookup only
# succeeds via the shared alternate code "ANT" -> "NLD".
#
# So every check below considers BOTH a territory's primary
# SLIIDERS.country.code AND its Alternate.ISO.code (when present).

country_data_clean <- country.data %>%
  transmute(
    SLIIDERS.country.code = SLIIDERS.country.code,
    Territory.name         = Name..ISO.3166.if.available.,
    Parent.code.default     = Group.with..for.World.Bank.,
    Alternate.ISO.code      = Alternate.ISO.code
  ) %>%
  filter(!(SLIIDERS.country.code %in% excluded_codes)) %>%
  mutate(
    Display.code = SLIIDERS.country.code
  )

# Apply display-code overrides (currently just Canary Islands -> SPI)
for (ov in names(display_code_overrides)) {
  is_match <- country_data_clean$SLIIDERS.country.code == display_code_overrides[ov] &
              country_data_clean$Territory.name == display_code_override_names[ov]
  country_data_clean$Display.code[is_match] <- ov
}

check_any_code <- function(primary, alt, lookup_vec) {
  primary %in% lookup_vec | (!is.na(alt) & alt != "" & alt %in% lookup_vec)
}

step7_parent_lookup <- hdi_gni_missing %>%
  distinct(emdat.countries, wb.undp.countries)

lookup_parent_any_code <- function(primary, alt) {
  hit <- step7_parent_lookup$wb.undp.countries[step7_parent_lookup$emdat.countries == primary]
  if (length(hit) == 0 && !is.na(alt) && alt != "") {
    hit <- step7_parent_lookup$wb.undp.countries[step7_parent_lookup$emdat.countries == alt]
  }
  if (length(hit) == 0) NA_character_ else hit[1]
}

territory_flags <- country_data_clean %>%
  rowwise() %>%
  mutate(
    footnote_1 = check_any_code(SLIIDERS.country.code, Alternate.ISO.code, wb_pop_missing$SLIIDERS.country.code),
    footnote_2 = check_any_code(SLIIDERS.country.code, Alternate.ISO.code, pop_scale_missing$SLIIDERS.country.code),
    footnote_3 = check_any_code(SLIIDERS.country.code, Alternate.ISO.code, hdi_gni_missing$emdat.countries),
    wb.undp.countries = lookup_parent_any_code(SLIIDERS.country.code, Alternate.ISO.code),
    # Use Step 7's actual assigned parent (found via either code) when it
    # exists, otherwise fall back to country.data's default parent
    Parent.code = ifelse(!is.na(wb.undp.countries), wb.undp.countries, Parent.code.default)
  ) %>%
  ungroup() %>%
  filter(footnote_1 | footnote_2 | footnote_3) %>%
  filter(!is.na(Parent.code), Parent.code != "")


# =============================================================================
# SECTION III: FORMAT DISPLAY STRINGS
# =============================================================================

format_territory <- function(name, code, f1, f2, f3) {
  superscripts <- paste0(c("1", "2", "3")[c(f1, f2, f3)], collapse = ",")
  paste0(name, " (", code, ")", superscripts)
}

territory_flags <- territory_flags %>%
  rowwise() %>%
  mutate(display = format_territory(Territory.name, Display.code,
                                     footnote_1, footnote_2, footnote_3)) %>%
  ungroup()

missing_parent_names <- setdiff(unique(territory_flags$Parent.code), names(parent_country_names))
if (length(missing_parent_names) > 0) {
  warning("No hardcoded display name for parent country code(s): ",
          paste(missing_parent_names, collapse = ", "),
          " -- add to parent_country_names and re-run.")
}

table_s2 <- territory_flags %>%
  arrange(Parent.code, Territory.name) %>%
  group_by(Parent.code) %>%
  summarise(
    `Territories with missing demographic data` = paste(display, collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    `Country or territory with data available` = ifelse(
      Parent.code %in% names(parent_country_names),
      parent_country_names[Parent.code],
      Parent.code
    )
  ) %>%
  arrange(`Country or territory with data available`) %>%
  dplyr::select(`Country or territory with data available`,
         `Territories with missing demographic data`)

print(table_s2)

# =============================================================================
# SECTION IV: WRITE OUTPUT
# =============================================================================

write.csv(
  table_s2,
  file = paste0("Output/Tables/Table S2 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S2 written to Output/Tables/Table S2 ", date_append_output, ".csv")
message("Remember to add footnotes (a) and (b) manually -- see header comment.")
