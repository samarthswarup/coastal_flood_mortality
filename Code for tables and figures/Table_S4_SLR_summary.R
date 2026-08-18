###########################################
# Table S4 - Scenario descriptions        #
# (SLR by scenario, incl. bounding runs)  #
#                                         #
# 2026-07-14                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding table script, run independently of the numbered pipeline.
# Reports global-mean sea level rise (GMSL) by scenario, with a hardcoded
# scenario description column, plus rows for the decoupled BOUND_worst /
# BOUND_best bounding scenarios used in Step 9 and Figures 3/4.
#
# Draws exclusively on Step 1's SLR output files (already final --
# gsl_msl05 is a direct AR6 scenario value, not something this script
# derives). BOUND_worst/BOUND_worst_med/BOUND_best rows reuse
# already-computed GMSL values rather than re-reading or re-deriving
# anything. BOUND_worst reuses AR6_low_ssp585 (low-confidence, includes
# potential high-end ice-sheet instability) as the headline worst-case,
# to capture the long-tail ice-sheet-driven risk; the medium-confidence
# version is retained as a secondary row, BOUND_worst_med (reusing
# AR6_med_ssp585). BOUND_best reuses AR6_med_ssp119. In each case what
# differs for the bounding scenario is the population/HDI trajectory
# (SSP3 for BOUND_worst*, SSP5 for BOUND_best), not the sea level
# forcing, so the GMSL numbers are correctly identical to the matching
# row -- this is not a duplication error.
#
# Source files: Output of Step 1 (SLIIDERS input reader), one CSV per
# scenario x year x quantile, under:
#   Intermediate outputs/Output from Step 1/SLR_[step1_date_append]/
# Filenames follow the pattern: {scenario}_{year}_{quantile*100}.csv
# (see 1_SLIIDERS_input_reader.py, Step III).

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

step1_date_append <- "2026-06-24"

slr_dir <- paste0("Intermediate outputs/Output from Step 1/SLR_",
                   step1_date_append, "/")

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# USER CONFIGURATION: scenario descriptions (hardcoded, editable here)
# =============================================================================
# One entry per scenario label used below. Edit freely -- these are display
# text only and do not affect the GMSL values pulled from Step 1.

scenario_descriptions <- c(
  AR6_med_ssp119 = "SSP1-1.9: very low emissions, net-zero ~2050; AR6 medium-confidence sea level",
  AR6_med_ssp126 = "SSP1-2.6: sustainable development, strong mitigation; AR6 medium-confidence",
  AR6_low_ssp126 = "SSP1-2.6: sustainable development, strong mitigation; AR6 low-confidence (includes potential high-end ice-sheet instability)",
  AR6_med_ssp245 = "SSP2-4.5: moderate socioeconomic and emissions trends, no strong mitigation; AR6 medium-confidence",
  AR6_med_ssp370 = "SSP3-7.0: regional rivalry, high emissions, limited mitigation; AR6 medium-confidence",
  AR6_med_ssp585 = "SSP5-8.5: fossil-fueled development, high emissions, minimal mitigation; AR6 medium-confidence",
  AR6_low_ssp585 = "SSP5-8.5: fossil-fueled development, high emissions, minimal mitigation; AR6 low-confidence (includes potential high-end ice-sheet instability)",
  BOUND_worst    = "Decoupled bounding scenario: SSP5-8.5 sea-level forcing (AR6 low-confidence, includes potential high-end ice-sheet instability, same as AR6_low_ssp585) paired with SSP3 population/HDI trajectory -- not a standard SSP-RCP pairing",
  BOUND_worst_med = "Decoupled bounding scenario, secondary comparison: SSP5-8.5 sea-level forcing (AR6 medium-confidence, same as AR6_med_ssp585) paired with SSP3 population/HDI trajectory -- not a standard SSP-RCP pairing",
  BOUND_best     = "Decoupled bounding scenario: SSP1-1.9 sea-level forcing (AR6 medium-confidence, same as AR6_med_ssp119) paired with SSP5 population/HDI trajectory -- not a standard SSP-RCP pairing"
)

# =============================================================================
# SECTION I: DEFINE SCENARIOS/YEARS/QUANTILES + BUILD FILENAMES
# =============================================================================

## These match the 7 SSP-consistent settings used in Step 8/9, i.e. the
## scenarios that have their OWN distinct SLR file. BOUND_worst/BOUND_best
## are handled separately below since they reuse an existing file rather
## than pointing to a new one.
scenarios_used <- c(
  "AR6_med_ssp119",
  "AR6_med_ssp126",
  "AR6_low_ssp126",
  "AR6_med_ssp245",
  "AR6_med_ssp370",
  "AR6_med_ssp585",
  "AR6_low_ssp585"
)

years_used <- c(2050, 2100)

## Quantiles reported: median (0.50) plus the 17th/83rd percentile likely
## range referenced in Results/Figure S7.
quantiles_used <- c(0.17, 0.50, 0.83)

quantile_to_suffix <- function(q) sprintf("%02d", round(q * 100))

results_list <- list()

for (scen in scenarios_used) {
  for (yr in years_used) {
    for (q in quantiles_used) {

      fname <- paste0(scen, "_", yr, "_", quantile_to_suffix(q), ".csv")
      fpath <- file.path(slr_dir, fname)

      if (!file.exists(fpath)) {
        warning(paste("File not found, skipping:", fpath))
        next
      }

      ## Only need first row -- gsl_msl05 is identical across all segments
      df_first_row <- read.csv(fpath, nrows = 1)

      if (!"gsl_msl05" %in% names(df_first_row)) {
        warning(paste("Column 'gsl_msl05' not found in:", fname))
        next
      }

      results_list[[length(results_list) + 1]] <- data.frame(
        scenario  = scen,
        year      = yr,
        quantile  = q,
        gsl_msl05 = df_first_row$gsl_msl05[1]
      )
    }
  }
}

gmsl_long <- do.call(rbind, results_list)

if (is.null(gmsl_long) || nrow(gmsl_long) == 0) {
  stop("No SLR files were successfully loaded -- results_list is empty. ",
       "Check that slr_dir points to an existing directory and that step1_date_append ",
       "matches an actual folder there.\nslr_dir was: ", slr_dir,
       "\nDirectory exists: ", dir.exists(slr_dir))
}

# =============================================================================
# SECTION II: ADD BOUND_worst / BOUND_worst_med / BOUND_best BY REUSING
# MATCHING ROWS (not a new file read -- these scenarios share their SLR
# forcing exactly with an already-loaded AR6 scenario; see header note)
# =============================================================================

# BOUND_worst uses low-confidence forcing (AR6_low_ssp585) as the
# headline worst-case, to capture the long-tail, ice-sheet-instability-
# driven risk. BOUND_worst_med (the medium-confidence version) is
# retained as a secondary comparison row.
bound_worst_rows <- gmsl_long %>%
  filter(scenario == "AR6_low_ssp585") %>%
  mutate(scenario = "BOUND_worst")

bound_worst_med_rows <- gmsl_long %>%
  filter(scenario == "AR6_med_ssp585") %>%
  mutate(scenario = "BOUND_worst_med")

bound_best_rows <- gmsl_long %>%
  filter(scenario == "AR6_med_ssp119") %>%
  mutate(scenario = "BOUND_best")

gmsl_long <- bind_rows(gmsl_long, bound_worst_rows, bound_worst_med_rows, bound_best_rows)

# =============================================================================
# SECTION III: RESHAPE INTO WIDE SI TABLE FORMAT
# =============================================================================

fmt <- function(x) sprintf("%.2f", round(x, 2))

gmsl_wide_raw <- gmsl_long %>%
  pivot_wider(names_from = quantile, values_from = gsl_msl05,
              names_prefix = "q_")

gmsl_table <- gmsl_wide_raw %>%
  mutate(
    gmsl_display = paste0(fmt(q_0.5), " (", fmt(q_0.17), "-", fmt(q_0.83), ")")
  ) %>%
  dplyr::select(scenario, year, gmsl_display) %>%
  pivot_wider(names_from = year, values_from = gmsl_display,
              names_prefix = "GMSL_")

# Attach descriptions + final display order (SSP-consistent scenarios
# first in the same order as scenario_descriptions, then the two bounding
# scenarios at the end)
scenario_order <- names(scenario_descriptions)

table_s4 <- gmsl_table %>%
  mutate(
    Description = scenario_descriptions[scenario],
    scenario = factor(scenario, levels = scenario_order)
  ) %>%
  arrange(scenario) %>%
  rename(`Scenario label` = scenario,
         `2050` = GMSL_2050,
         `2100` = GMSL_2100) %>%
  dplyr::select(`Scenario label`, Description, `2050`, `2100`)

missing_descriptions <- setdiff(unique(gmsl_long$scenario), names(scenario_descriptions))
if (length(missing_descriptions) > 0) {
  warning("No hardcoded description found for: ",
          paste(missing_descriptions, collapse = ", "))
}

print(table_s4)

# =============================================================================
# SECTION IV: WRITE OUTPUT
# =============================================================================

write.csv(
  table_s4,
  file = paste0("Output/Tables/Table S4 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S4 written to Output/Tables/Table S4 ", date_append_output, ".csv")
