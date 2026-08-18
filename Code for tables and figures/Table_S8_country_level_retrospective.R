###########################################
# Table S8 - Country-level retrospective  #
# mortality estimates                     #
#                                         #
# 2026-07-16                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding table script, run independently of the numbered pipeline.
# Draws on two already-final Step 8 outputs that are otherwise only used
# as intermediate inputs to Figure_2_retrospective_mortality.R (the
# retrospective mortality maps), combined and formatted here as a
# standalone table.
#
# Source 1: CountryDeathCIs <date>.csv -- the MODELED per-country results
# (from predMod.3, the main model), read from "Intermediate outputs/Output
# from Step 8/Country tables/". Confirmed columns: Country_Code, nEvents,
# meanDeaths, meanLogDeaths, meanRate, mdsd, mldsd, mrsd, md_lcl, md_ucl,
# mld_lcl, mld_ucl, mr_lcl, mr_ucl. This file contains NO observed-data
# columns at all -- everything in it is a model prediction.
#
# Source 2: trendData (Step 8's saved RDS) -- used to compute the
# OBSERVED per-country aggregates the SI caption asks for (observed total
# deaths, observed average exposed population per event) and to look up
# each country's display name, since CountryDeathCIs.csv only has ISO
# codes.
#
# Reports, per country/territory (each of the three modeled quantities
# combined with its 95% CI into a single cell, mean on line 1, CI in
# parentheses on line 2 -- omitted entirely if the CI isn't available,
# rather than showing "NA-NA"):
#   - Number of Events (from CountryDeathCIs' own nEvents)
#   - Observed total deaths (sum of total.deaths.na.as.0 across that
#     country's events, from trendData)
#   - Observed average exposed population per event (millions) (mean of
#     pop.exposed.local.else.adm1.else.national.100yr across that
#     country's events, from trendData)
#   - Modeled deaths per event (meanDeaths) [95% CI: md_lcl-md_ucl]
#   - Modeled deaths per year (meanDeaths * nEvents / 35, with CI bounds
#     scaled by the same linear factor -- valid since deaths-per-year is
#     just a scalar multiple of the same per-event quantity)
#   - Modeled mortality rate (meanRate) [95% CI: mr_lcl-mr_ucl]
#
# Countries with no valid model prediction at all (e.g. DPRK/PRK, which
# is excluded from HDI/GNI-dependent modeling entirely -- see Table S5's
# footnote: "Nine events are removed from income category analyses due to
# data unavailability for North Korea") are OMITTED, not shown with NA
# rows -- filtered on !is.finite(meanDeaths), since mean() of an all-NA
# vector returns NaN, not NA, and both should be excluded.
#
# Draws exclusively on already-final Step 8 outputs -- no re-derivation
# of the modeled quantities (those come straight from CountryDeathCIs.csv)
# and only light aggregation (sum/mean) for the observed columns from
# trendData, which is itself already final.
#
# OUTPUT FORMAT: writes a Word-openable .doc file (HTML content saved
# with a .doc extension), matching the same convention already used
# elsewhere in this pipeline for sjPlot::tab_model() output -- not a CSV.

rm(list = ls())

library(dplyr)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 8 run to pull trendData and
# CountryDeathCIs.csv from
step8_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

N_YEARS <- 35   # matches Step 9's historical study period length

# =============================================================================
# SECTION I: LOAD UPSTREAM OUTPUTS
# =============================================================================

ci_base <- read.csv(paste0(
  "Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ",
  step8_date_append_output, ".csv"))

trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

# =============================================================================
# SECTION II: OBSERVED PER-COUNTRY AGGREGATES + COUNTRY NAME LOOKUP
# =============================================================================

trendData$Country_Code <- trendData$SLIIDERS.country.code.flat

observed_agg <- trendData %>%
  filter(!is.na(Country_Code)) %>%
  group_by(Country_Code) %>%
  summarise(
    observed_total_deaths = sum(total.deaths.na.as.0, na.rm = TRUE),
    observed_avg_exposed_pop_M = mean(pop.exposed.local.else.adm1.else.national.100yr, na.rm = TRUE) / 1e6,
    .groups = "drop"
  )

# Country/territory display name: look up from events recorded directly
# under that ISO code (same approach used in Table S2/S3 scripts)
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
# SECTION III: MERGE + DERIVE DEATHS-PER-YEAR (with scaled CI) + FILTER OUT
# UNMODELED COUNTRIES
# =============================================================================

table_s8_raw <- ci_base %>%
  filter(is.finite(meanDeaths)) %>%   # drop countries with no valid model prediction (e.g. PRK)
  left_join(observed_agg, by = "Country_Code") %>%
  left_join(country_name_lookup, by = "Country_Code") %>%
  mutate(
    `Country or territory` = case_when(
      Country_Code %in% names(manual_country_names) ~
        paste0(manual_country_names[Country_Code], " (", Country_Code, ")"),
      is.na(Country) ~ Country_Code,
      TRUE ~ paste0(Country, " (", Country_Code, ")")
    ),
    deaths_per_year = meanDeaths * nEvents / N_YEARS,
    deaths_per_year_lcl = md_lcl * nEvents / N_YEARS,
    deaths_per_year_ucl = md_ucl * nEvents / N_YEARS
  )

# =============================================================================
# SECTION IV: FORMAT COMBINED "mean (CI)" CELLS (HTML line break; a clean
# "NA" if the value itself is unavailable, and no dangling "NA-NA" -- the
# CI line is simply omitted if either bound is missing)
# =============================================================================

fmt_num <- function(x) {
  if (!is.finite(x)) return("NA")
  if (x == 0) return("0")

  sign_str <- if (x < 0) "-" else ""
  abs_x <- abs(x)

  # Values >= 1: literal 3 decimal places, no scientific notation needed
  if (abs_x >= 1) {
    return(paste0(sign_str, formatC(round(abs_x, 3), format = "f", digits = 3)))
  }

  # Values < 1: count leading zeros after the decimal point (before the
  # first significant digit) via string parsing, to avoid log10 floating-
  # point edge cases at exact powers of 10.
  s <- formatC(abs_x, format = "f", digits = 15)
  frac <- sub("^0\\.", "", s)
  leading_zeros <- nchar(regmatches(frac, regexpr("^0*", frac)))

  if (leading_zeros >= 4) {
    # 4 or more leading zeros (value < 0.0001) -> scientific notation
    return(paste0(sign_str, formatC(signif(abs_x, 3), format = "e", digits = 2)))
  }

  # Otherwise: fixed notation with enough decimal places to show 3
  # significant figures past the leading zeros
  paste0(sign_str, formatC(signif(abs_x, 3), format = "f", digits = leading_zeros + 3))
}

fmt_cell <- function(mean_val, lcl, ucl) {
  vapply(seq_along(mean_val), function(i) {
    if (!is.finite(mean_val[i])) return("NA")
    mean_str <- fmt_num(mean_val[i])
    if (!is.finite(lcl[i]) || !is.finite(ucl[i])) return(mean_str)
    paste0(mean_str, "<br>(", fmt_num(lcl[i]), " to ", fmt_num(ucl[i]), ")")
  }, character(1))
}

table_s8 <- table_s8_raw %>%
  arrange(Country_Code %in% names(manual_country_names), `Country or territory`) %>%
  transmute(
    `Country or territory`,
    `Number of Events` = nEvents,
    `Observed total deaths` = ifelse(is.finite(observed_total_deaths), observed_total_deaths, "NA"),
    `Observed avg. exposed population per event (M)` =
      ifelse(is.finite(observed_avg_exposed_pop_M), round(observed_avg_exposed_pop_M, 2), "NA"),
    `Modeled deaths per event (95% CI)` = fmt_cell(meanDeaths, md_lcl, md_ucl),
    `Modeled deaths per year (95% CI)` = fmt_cell(deaths_per_year, deaths_per_year_lcl, deaths_per_year_ucl),
    `Modeled mortality rate (95% CI)` = fmt_cell(meanRate, mr_lcl, mr_ucl)
  )

missing_names <- table_s8 %>%
  filter(!grepl("\\(", `Country or territory`))
if (nrow(missing_names) > 0) {
  warning("Could not resolve a display name for: ",
          paste(unique(missing_names$`Country or territory`), collapse = ", "),
          " -- check manually.")
}

# =============================================================================
# SECTION V: WRITE OUTPUT AS A WORD-OPENABLE .doc (HTML content)
# =============================================================================

html_escape_headers <- names(table_s8)

table_rows <- apply(table_s8, 1, function(row) {
  row <- trimws(row)
  paste0("<tr>", paste0("<td style='border:1px solid #999; padding:4px; vertical-align:top;'>",
                        row, "</td>", collapse = ""), "</tr>")
})

html_out <- paste0(
  "<html><head><meta charset='utf-8'></head><body>",
  "<h3>Table S8. Country-level retrospective mortality estimates</h3>",
  "<table style='border-collapse:collapse; font-family:Calibri, sans-serif; font-size:10pt;'>",
  "<tr>", paste0("<th style='border:1px solid #999; padding:4px; background:#f2f2f2;'>",
                html_escape_headers, "</th>", collapse = ""), "</tr>",
  paste(table_rows, collapse = ""),
  "</table></body></html>"
)

out_path <- paste0("Output/Tables/Table S8 ", date_append_output, ".doc")
writeLines(html_out, out_path, useBytes = TRUE)

message("Table S8 written to ", out_path)
