# Table S10
#
# Purpose: Combines Model 3 country mortality estimates with historical event, death, and exposure summaries.
#
# Inputs:
#   - Step 8: CountryDeathCIs CSV
#   - Step 8: trendData RDS
#
# Outputs:
#   - Output/Tables/Table S10 <date>.csv
#
# R packages: dplyr
# Run from the analysis directory containing Intermediate outputs/.
#

rm(list = ls())
library(dplyr)

project_dir <- getwd()
if (!dir.exists(file.path(project_dir, "Intermediate outputs")) &&
    dir.exists(file.path(project_dir, "Claude", "Intermediate outputs"))) {
  project_dir <- file.path(project_dir, "Claude")
}
if (!dir.exists(file.path(project_dir, "Intermediate outputs"))) {
  stop("Run this script from the project analysis directory containing Intermediate outputs/.")
}
setwd(project_dir)
dir.create("Output/Tables", recursive = TRUE, showWarnings = FALSE)

step8_date_append_output <- "2026-11-13"
date_append_output <- format(Sys.Date(), "%Y-%m-%d")
N_YEARS <- 35

ci_path <- paste0(
  "Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ",
  step8_date_append_output, ".csv"
)
trend_path <- paste0(
  "Intermediate outputs/Output from Step 8/trendData ",
  step8_date_append_output, ".RDS"
)

for (p in c(ci_path, trend_path)) {
  if (!file.exists(p)) stop("Required Step 8 input not found: ", p,
                            "\nWorking directory: ", getwd())
}

ci <- read.csv(ci_path, stringsAsFactors = FALSE)
trendData <- readRDS(trend_path)

needed_ci <- c("Country_Code", "nEvents", "meanDeaths",
               "md_lcl", "md_ucl", "meanRate", "mr_lcl", "mr_ucl")
missing_ci <- setdiff(needed_ci, names(ci))
if (length(missing_ci)) {
  stop("Missing CountryDeathCIs columns: ", paste(missing_ci, collapse = ", "))
}

if (!"Country_Code" %in% names(trendData)) {
  if ("SLIIDERS.country.code.flat" %in% names(trendData)) {
    trendData$Country_Code <- trendData$SLIIDERS.country.code.flat
  } else {
    stop("trendData has no Country_Code or SLIIDERS.country.code.flat column.")
  }
}

exposure_col <- "pop.exposed.local.else.adm1.else.national.100yr"
needed_td <- c("Country_Code", "Start.Year", exposure_col, "total.deaths.na.as.0")
missing_td <- setdiff(needed_td, names(trendData))
if (length(missing_td)) {
  stop("Missing trendData columns: ", paste(missing_td, collapse = ", "))
}

trendData$Country_Code <- as.character(trendData$Country_Code)
ci$Country_Code <- as.character(ci$Country_Code)

td <- trendData %>%
  filter(Start.Year >= 1990, Start.Year <= 2024,
         !is.na(Country_Code), nzchar(Country_Code),
         is.finite(.data[[exposure_col]]),
         .data[[exposure_col]] > 0)

td$observed_event_rate <- td$total.deaths.na.as.0 / td[[exposure_col]]
td$observed_event_rate[!is.finite(td$observed_event_rate) |
                         td$observed_event_rate >= 1] <- NA_real_

if ("Country" %in% names(td)) {
  name_lookup <- td %>%
    filter(!is.na(Country), nzchar(as.character(Country))) %>%
    group_by(Country_Code) %>%
    summarise(country_name = as.character(first(Country)), .groups = "drop")
} else {
  name_lookup <- data.frame(Country_Code = character(),
                            country_name = character())
}

observed <- td %>%
  group_by(Country_Code) %>%
  summarise(
    events_in_trendData = n(),
    events_with_reported_deaths = if ("total.deaths.reported" %in% names(td))
      sum(!is.na(total.deaths.reported)) else NA_integer_,
    observed_total_deaths_NAs_as_zero = sum(total.deaths.na.as.0, na.rm = TRUE),
    observed_mean_deaths_per_event_NAs_as_zero =
      mean(total.deaths.na.as.0, na.rm = TRUE),
    mean_exposed_population_per_event = mean(.data[[exposure_col]], na.rm = TRUE),
    observed_mean_event_mortality_rate = mean(observed_event_rate, na.rm = TRUE),
    .groups = "drop"
  )

table_s10 <- ci %>%
  left_join(name_lookup, by = "Country_Code") %>%
  left_join(observed, by = "Country_Code") %>%
  mutate(
    `Country or territory` =
      ifelse(is.na(country_name), Country_Code,
             paste0(country_name, " (", Country_Code, ")")),
    `Estimated annual deaths (Model 3)` = meanDeaths * nEvents / N_YEARS
  ) %>%
  arrange(`Country or territory`) %>%
  transmute(
    `Country or territory`,
    Country_Code,
    `Number of modeled events` = nEvents,
    events_in_trendData,
    events_with_reported_deaths,
    observed_total_deaths_NAs_as_zero,
    observed_mean_deaths_per_event_NAs_as_zero,
    mean_exposed_population_per_event,
    observed_mean_event_mortality_rate,
    `Predicted mean deaths per event (Model 3)` = meanDeaths,
    `Predicted mean deaths lower 95% bound` = md_lcl,
    `Predicted mean deaths upper 95% bound` = md_ucl,
    `Predicted mean event mortality rate (Model 3)` = meanRate,
    `Predicted mortality rate lower 95% bound` = mr_lcl,
    `Predicted mortality rate upper 95% bound` = mr_ucl,
    `Estimated annual deaths (Model 3)`
  )

count_mismatch <- table_s10 %>%
  filter(!is.na(events_in_trendData),
         `Number of modeled events` != events_in_trendData)
if (nrow(count_mismatch) > 0) {
  warning(nrow(count_mismatch), " country rows have differing modeled-event ",
          "and trendData event counts; inspect before submitting Table S10.")
}

out_path <- paste0("Output/Tables/Table S10 ", date_append_output, ".csv")
write.csv(table_s10, out_path, row.names = FALSE)
message("Table S10 written: ", out_path, " (", nrow(table_s10), " countries).")
