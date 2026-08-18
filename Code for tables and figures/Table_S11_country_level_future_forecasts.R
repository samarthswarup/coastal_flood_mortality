###########################################
# Table S11 - Country-level future        #
# mortality forecasts                     #
# (data underlying Main Figure 4,         #
# geographic projections)                 #
#                                         #
# 2026-07-16                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding table script, run independently of the numbered pipeline.
# Draws on Step 9's saved country_master output, which
# Figure_4_geographic_projections.R also reads as an input for the
# maps/top-10 panels, formatted here as a full country x scenario x year
# table in its own right.
#
# EXPANDED beyond Figure 4's scope: Figure 4 only plots the final, fully
# decomposed stage. This table instead includes ALL SIX methodology
# decomposition stages (SLR only -> +population exposure -> +population
# NB covariate -> +HDI -> +coastal growth 1 -> +coastal growth 2), so a
# reader can see how each successive methodological addition changes
# country-level projections -- not just the headline estimate. Scenario
# set (7 scenarios) and the historical-baseline formula match Figure 4's
# config exactly.
#
# Reports, one row per Country_Code x Scenario x Year x Methodology stage:
#   - Country/territory name + code
#   - Scenario (SSP label)
#   - Year (2050 or 2100)
#   - Methodology stage (one of the six decomposition stages)
#   - Number of historical events (n_events, from country_master)
#   - Projected annual deaths (ann_deaths) with 95% CI
#   - Projected % change vs. historical baseline (pctInc) with 95% CI --
#     same formula as Figure 4: historical baseline computed per country
#     as meanDeaths * nEvents / N_YEARS (from Step 8's CountryDeathCIs),
#     matching Step 9's own global-baseline formula applied per country.
#
# Draws exclusively on already-final Step 8 (CountryDeathCIs.csv) and
# Step 9 (Country_level_master_database.csv) outputs -- no re-derivation
# of the model predictions themselves, only the same light percent-change
# calculation already used in Figure 4.
#
# OUTPUT FORMAT: .xlsx (via openxlsx), not .csv/.doc -- this table is
# large (countries x 7 scenarios x 2 years x 6 stages, ~10,900 rows), so
# a spreadsheet is more usable than a flat CSV or a Word table.

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(openxlsx)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

# Date suffixes identifying which Step 8 / Step 9 runs to pull from
step8_date_append_output <- "2026-07-20"
step9_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

N_YEARS <- 35   # matches Step 9's historical study period length

# =============================================================================
# USER CONFIGURATION: scenarios must stay in sync with
# Figure_4_geographic_projections.R (same 7 scenarios). This table includes
# ALL SIX methodology decomposition stages (not just the final one) -- one
# set of rows per stage, so a reader can see how each successive addition
# (population exposure, HDI, each coastal-growth multiplier) changes
# country-level projections. Labels match the terminology already used in
# Figure 3's stage_top/stage_bottom naming. Edit stage_labels to
# add/remove/relabel stages.
# =============================================================================

scenarios_to_plot <- c(
  "SSP1.19",
  "SSP1.26_med",
  "SSP2.45",
  "SSP3.70",
  "SSP5.85_med",
  "BOUND_worst",
  "BOUND_best"
)

# names = raw methodology label in country_master; values = display label
stage_labels <- c(
  " SLR only" = "1. SLR only",
  " + Population (exposure only)" = "2. + Population exposure",
  " + Population (exposure + NB covariate)" = "3. + Population (exposure + national population covariate)",
  " + Population (both) & HDI" = "4. + HDI",
  " + Population (both) & HDI & coastal growth 1" = "5. + Coastal growth (Rentschler et al. 2023)",
  " + Population (both) & HDI & coastal growth 2" = "6. + Coastal growth (Tellman et al. 2021)"
)

# =============================================================================
# SECTION I: LOAD UPSTREAM OUTPUTS
# =============================================================================

ci_base <- read.csv(paste0(
  "Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ",
  step8_date_append_output, ".csv"))

country_master <- read.csv(
  paste0("Intermediate outputs/Output from Step 9/Country_level_master_database_", step9_date_append_output, ".csv"),
  stringsAsFactors = FALSE
)

trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

# =============================================================================
# SECTION II: COUNTRY NAME LOOKUP (same approach as Table S2/S3/S8)
# =============================================================================

trendData$Country_Code <- trendData$SLIIDERS.country.code.flat

country_name_lookup <- trendData %>%
  filter(ISO == Country_Code) %>%
  distinct(Country_Code, Country) %>%
  distinct(Country_Code, .keep_all = TRUE)

# Manual display-name overrides for codes that never appear as their own
# direct trendData record and so can't be resolved via the standard
# lookup -- e.g. the pooled "BES,CUW,ABW,SXM" code EM-DAT uses for the
# former Netherlands Antilles (see the Table S2 discussion of this
# grouping).
manual_country_names <- c("BES,CUW,ABW,SXM" = "Fmr. Netherlands Antilles")

# =============================================================================
# SECTION III: HISTORICAL BASELINE + PERCENT CHANGE (identical to
# Figure_4_geographic_projections.R)
# =============================================================================

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

# =============================================================================
# SECTION IV: FORMAT + WRITE OUTPUT (long format: one row per
# Country_Code x Scenario x Year x Methodology stage)
# =============================================================================

table_s11 <- proj_data %>%
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

missing_names <- table_s11 %>%
  filter(!grepl("\\(", `Country or territory`))
if (nrow(missing_names) > 0) {
  warning("Could not resolve a display name for: ",
          paste(unique(missing_names$`Country or territory`), collapse = ", "),
          " -- check manually.")
}

out_path <- paste0("Output/Tables/Table S11 ", date_append_output, ".xlsx")
openxlsx::write.xlsx(table_s11, out_path, asTable = TRUE, overwrite = TRUE)

message("Table S11 written to ", out_path, " (", nrow(table_s11), " rows)")
