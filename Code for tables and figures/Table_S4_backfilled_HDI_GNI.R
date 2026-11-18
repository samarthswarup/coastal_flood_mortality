# Table S4
#
# Purpose: Tabulates events for which the earliest available HDI or GNI observations were used.
#
# Inputs:
#   - Step 7: event-level coastal flood XLSX
#   - Raw World Bank HDR data CSV
#
# Outputs:
#   - Output/Tables/Table S4 <date>.csv
#
# R packages: dplyr, readxl, tidyr
# Run from the analysis directory containing Intermediate outputs/.
#

rm(list = ls())

library(dplyr)
library(readxl)
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

step7_date_append_output <- "2026-07-05"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

emdat.data.filtered <- read_excel(
  paste0("Intermediate outputs/Output from Step 7/",
         "EMDAT events with exposed pops, HDI GNI and prev events ",
         step7_date_append_output, ".xlsx")
)

hdr.data <- read.csv("../Input/World Bank pop data/HDR data 2025-01-16.csv")

earliest_year_table <- hdr.data %>%
  filter(indicatorCode %in% c("hdi", "gnipc")) %>%
  group_by(countryIsoCode, indicatorCode) %>%
  summarise(earliest_year = min(year, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicatorCode, values_from = earliest_year,
              names_prefix = "earliest_year_")

eligible <- emdat.data.filtered %>%
  filter(hdr.country.code != "PRK",
         Start.Year >= 1990,
         Start.Year < 2022) %>%
  left_join(earliest_year_table, by = c("hdr.country.code" = "countryIsoCode")) %>%
  mutate(
    hdi_backfilled = !is.na(earliest_year_hdi)   & Start.Year < earliest_year_hdi,
    gni_backfilled = !is.na(earliest_year_gnipc) & Start.Year < earliest_year_gnipc
  )

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

table_s4 <- bind_rows(hdi_rows, gni_rows) %>%
  left_join(country_name_lookup, by = c("Country_Code" = "hdr.country.code")) %>%
  mutate(
    `Country or territory with data available` = ifelse(
      is.na(Country), Country_Code, paste0(Country, " (", Country_Code, ")")
    )
  ) %>%
  arrange(`Country or territory with data available`, `Data type`) %>%
  dplyr::select(`Country or territory with data available`, `Data type`,
         `Earliest year for which data are available`, `Number of events affected`)

unresolved_names <- table_s4 %>%
  filter(!grepl("\\(", `Country or territory with data available`))
if (nrow(unresolved_names) > 0) {
  warning("Could not resolve a display name for: ",
          paste(unique(unresolved_names$`Country or territory with data available`), collapse = ", "),
          " (name unavailable).")
}

write.csv(
  table_s4,
  file = paste0("Output/Tables/Table S4 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S4 written to Output/Tables/Table S4 ", date_append_output, ".csv")
