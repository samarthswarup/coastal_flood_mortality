# Table S6
#
# Purpose: Extracts global mean sea-level-rise values for the modeled scenarios, years, and percentiles.
#
# Inputs:
#   - Step 1: scenario/year/percentile SLR CSV files containing gsl_msl05
#
# Outputs:
#   - Output/Tables/Table S6 <date>.csv
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

step1_date_append <- "2026-06-24"

slr_dir <- paste0("Intermediate outputs/Output from Step 1/SLR_",
                   step1_date_append, "/")

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

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

scenario_order <- names(scenario_descriptions)

table_s6 <- gmsl_table %>%
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

print(table_s6)

write.csv(
  table_s6,
  file = paste0("Output/Tables/Table S6 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S6 written to Output/Tables/Table S6 ", date_append_output, ".csv")
