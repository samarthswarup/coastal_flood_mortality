# Table S3
#
# Purpose: Combines territorial missing-data indicators for World Bank population, SLIIDERS population scaling, and HDI/GNI.
#
# Inputs:
#   - Step 3: two country/territory aggregation CSV files
#   - Step 7: missing HDI/GNI aggregation CSV
#   - Territorial country-code mapping CSV
#
# Outputs:
#   - Output/Tables/Table S3 <date>.csv
#
# R packages: dplyr, tidyr
# Run from the analysis directory containing Intermediate outputs/.
#

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(tidyr)

project_dir <- getwd()
if (!dir.exists(file.path(project_dir, "Intermediate outputs")) &&
    dir.exists(file.path(project_dir, "Claude", "Intermediate outputs"))) {
  project_dir <- file.path(project_dir, "Claude")
}
if (!dir.exists(file.path(project_dir, "Intermediate outputs"))) {
  stop("Run this script from the project analysis directory containing Intermediate outputs/.")
}
setwd(project_dir)

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

step3_date_append_output <- "2026-07-04"
step7_date_append_output <- "2026-07-05"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

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

country.data <- read.csv(
  paste0("../Input/Identify mismatched and missing countries - step 3/",
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

excluded_codes <- c("XAD")

display_code_overrides <- c(SPI = "ESP")
display_code_override_names <- c(SPI = "Canary Islands")

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

    Parent.code = ifelse(!is.na(wb.undp.countries), wb.undp.countries, Parent.code.default)
  ) %>%
  ungroup() %>%
  filter(footnote_1 | footnote_2 | footnote_3) %>%
  filter(!is.na(Parent.code), Parent.code != "")

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

table_s3 <- territory_flags %>%
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

print(table_s3)

write.csv(
  table_s3,
  file = paste0("Output/Tables/Table S3 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S3 written to Output/Tables/Table S3 ", date_append_output, ".csv")
message("Remember to add footnotes (a) and (b) manually -- see header comment.")
