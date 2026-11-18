# Table S13
#
# Purpose: Formats country-level mortality projections, confidence bounds, and changes relative to historical estimates.
#
# Inputs:
#   - Step 8: CountryDeathCIs CSV
#   - Step 9: country-level master projection CSV
#
# Outputs:
#   - Output/Tables/Table S13 <date>.xlsx
#
# R packages: dplyr, openxlsx
# Run from the analysis directory containing Intermediate outputs/.
#

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(openxlsx)

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

step8_date_append_output <- "2026-11-13"
step9_date_append_output <- "2026-11-14"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

N_YEARS <- 35

scenarios_to_plot <- c(
  "SSP1.19",
  "SSP1.26_med",
  "SSP2.45",
  "SSP3.70",
  "SSP5.85_med",
  "BOUND_worst",
  "BOUND_best"
)

stage_labels <- c(
  " SLR only" = "1. SLR only",
  " + Population (exposure only)" = "2. + Population exposure",
  " + Population (exposure + NB covariate)" = "3. + Population (exposure + national population covariate)",
  " + Population (both) & HDI" = "4. + HDI",
  " + Population (both) & HDI & coastal growth 1" = "5. + Coastal growth (Rentschler et al. 2023)",
  " + Population (both) & HDI & coastal growth 2" = "6. + Coastal growth (Tellman et al. 2021)"
)

ci_base <- read.csv(paste0(
  "Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ",
  step8_date_append_output, ".csv"))

country_master <- read.csv(
  paste0("Intermediate outputs/Output from Step 9/Country_level_master_database_", step9_date_append_output, ".csv"),
  stringsAsFactors = FALSE
)

trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

trendData$Country_Code <- trendData$SLIIDERS.country.code.flat

country_name_lookup <- trendData %>%
  filter(ISO == Country_Code) %>%
  distinct(Country_Code, Country) %>%
  distinct(Country_Code, .keep_all = TRUE)

manual_country_names <- c("BES,CUW,ABW,SXM" = "Fmr. Netherlands Antilles")

hist_country <- ci_base %>%
  transmute(Country_Code, hist_ann_deaths = meanDeaths * nEvents / N_YEARS)

proj_data <- country_master %>%
  filter(methodology %in% names(stage_labels), ssp %in% scenarios_to_plot) %>%
  left_join(hist_country, by = "Country_Code") %>%
  filter(!is.na(hist_ann_deaths), hist_ann_deaths > 0) %>%
  mutate(
    `Methodology stage` = stage_labels[methodology],
    pctInc     = 100 * (ann_deaths - hist_ann_deaths) / hist_ann_deaths,
    pctInc_lcl = 100 * (ann_deaths_lcl - hist_ann_deaths) / hist_ann_deaths,
    pctInc_ucl = 100 * (ann_deaths_ucl - hist_ann_deaths) / hist_ann_deaths
  )

table_s13 <- proj_data %>%
  left_join(country_name_lookup, by = "Country_Code") %>%
  mutate(
    `Country or territory` = case_when(
      Country_Code %in% names(manual_country_names) ~
        paste0(manual_country_names[Country_Code], " (", Country_Code, ")"),
      is.na(Country) ~ Country_Code,
      TRUE ~ paste0(Country, " (", Country_Code, ")")
    )
  ) %>%
  arrange(Country_Code %in% names(manual_country_names),
          `Country or territory`, year, ssp, `Methodology stage`) %>%
  transmute(
    `Country or territory`,
    Scenario = ssp,
    Year = year,
    `Methodology stage`,
    `Number of historical events` = n_events,
    `Historical baseline annual deaths` = round(hist_ann_deaths, 3),
    `Projected annual deaths` = round(ann_deaths, 3),
    `Projected annual deaths (95% CI lower)` = round(ann_deaths_lcl, 3),
    `Projected annual deaths (95% CI upper)` = round(ann_deaths_ucl, 3),
    `Projected % change vs. historical` = round(pctInc, 1),
    `Projected % change (95% CI lower)` = round(pctInc_lcl, 1),
    `Projected % change (95% CI upper)` = round(pctInc_ucl, 1)
  )

missing_names <- table_s13 %>%
  filter(!grepl("\\(", `Country or territory`))
if (nrow(missing_names) > 0) {
  warning("Could not resolve a display name for: ",
          paste(unique(missing_names$`Country or territory`), collapse = ", "),
          " (name unavailable).")
}

out_path <- paste0("Output/Tables/Table S13 ", date_append_output, ".xlsx")
openxlsx::write.xlsx(table_s13, out_path, asTable = TRUE, overwrite = TRUE)

message("Table S13 written to ", out_path, " (", nrow(table_s13), " rows)")
