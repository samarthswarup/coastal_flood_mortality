###########################################
# Table S12 - Total deaths by scenario    #
# and full methodological decomposition   #
# (3 sub-tables: S12.1-S12.3, by SLR      #
# percentile: 50th/17th/83rd)             #
#                                         #
# 2026-07-24                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# SI complement to Main Figure 3, extended to show how SLIIDERS SLR
# uncertainty interacts with the full methodological decomposition, not
# just a single central estimate. Three sub-tables, same 6 decomposition
# stages x 7 scenarios x 2 years in each: S12.1 at the 50th-percentile SLR
# (Step 9's own output), S12.2 and S12.3 at the 17th and 83rd percentiles
# (from Step 10's percentile-sensitivity companion script).
#
# S12.2/S12.3 have NO confidence interval columns (Ann_deaths_lcl/ucl are
# NA throughout) -- Step 10 deliberately skips the replicate bootstrap for
# these two extra percentiles to keep compute tractable, and this table
# never displays CIs in the first place (only Ann_deaths and
# Pct_vs_historical, per this script's own build_part_html()).
#
# Freestanding table script. Draws on:
#   - Step 9's own decomposition_table_population_methodology_<date>.csv
#     (50th percentile, S12.1)
#   - Step 10's decomposition_table_population_methodology_p17_<date>.csv
#     and _p83_<date>.csv (S12.2/S12.3)
# NOTHING is recomputed here -- this script only reshapes the three
# already-computed tables into a wide, color-coded presentation, at the
# same level of granularity as Table S11 and Figure 3.
#
# Rows = the SAME six decomposition stages used everywhere else in this
# project. Columns = the SAME 7-scenario set Figures 3/4 use.
#
# COLOR SCALE: computed ONCE, across the UNION of all three tables'
# Pct_vs_historical values (not per-table) -- so S12.1/S12.2/S12.3 share
# the same color intensity mapping and are visually comparable to each
# other at a glance, not just individually color-coded in isolation. White
# is positioned at 0's TRUE fractional location on this shared scale.
#
# OUTPUT FORMAT: Word-openable .doc (HTML content saved with a .doc
# extension), matching the convention already used elsewhere in this
# pipeline for HTML-content .doc output (e.g. Table S7, Table S8).

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(tidyr)
library(scales)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

# Date suffixes identifying which runs to pull each table from
step9_date_append_output     <- "2026-07-20"  # S12.1 (50th percentile, Step 9's own output)
step10_date_append_output    <- "2026-07-24"  # S12.2/S12.3 (17th/83rd, from Step 10)

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# USER CONFIGURATION
# =============================================================================

stage_rows <- c(
  " SLR only"                                      = "(1) SLR only",
  " + Population (exposure only)"                  = "(2) + Population exposure",
  " + Population (exposure + NB covariate)"        = "(3) + Population (exposure + national population covariate)",
  " + Population (both) & HDI"                     = "(4) + HDI",
  " + Population (both) & HDI & coastal growth 1"  = "(5) + Coastal growth (Rentschler et al. 2023)",
  " + Population (both) & HDI & coastal growth 2"  = "(6) + Coastal growth (Tellman et al. 2021)"
)

scenarios_matched <- c("SSP1.19", "SSP1.26_med", "SSP2.45", "SSP3.70",
                       "SSP5.85_med", "BOUND_worst", "BOUND_best")

# =============================================================================
# SECTION I: LOAD ALL THREE UPSTREAM TABLES
# =============================================================================

load_decomp <- function(path, label) {
  if (!file.exists(path)) {
    available <- list.files(dirname(path), pattern = "^decomposition_table_population_methodology")
    stop(
      "File not found for ", label, ": ", path, "\n",
      "Files actually present in that folder:\n  ", paste(available, collapse = "\n  "),
      "\n-> Check the date suffix above against the dates listed."
    )
  }
  read.csv(path, stringsAsFactors = FALSE)
}

decomp_table_p50 <- load_decomp(
  paste0("Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_", step9_date_append_output, ".csv"),
  "S12.1 (50th percentile)"
)
decomp_table_p17 <- load_decomp(
  paste0("Intermediate outputs/Output from Step 10/decomposition_table_population_methodology_p17_", step10_date_append_output, ".csv"),
  "S12.2 (17th percentile)"
)
decomp_table_p83 <- load_decomp(
  paste0("Intermediate outputs/Output from Step 10/decomposition_table_population_methodology_p83_", step10_date_append_output, ".csv"),
  "S12.3 (83rd percentile)"
)

# =============================================================================
# SECTION I.5: SCENARIO COLUMN ORDER
# =============================================================================
# Computed from the p50 table (S12.1), same approach Figures 3/4 use, then
# reused identically for S12.2/S12.3 so all three tables show scenarios in
# the SAME order -- essential for a reader to compare a given scenario's
# column across all three sub-tables directly.

scenario_cols <- decomp_table_p50 %>%
  filter(Year == "2050", Scenario == " + Population (both) & HDI & coastal growth 2",
         SSP %in% scenarios_matched) %>%
  arrange(Pct_vs_historical) %>%
  pull(SSP)

# =============================================================================
# SECTION II: SHARED COLOR MAPPING ACROSS ALL THREE TABLES
# =============================================================================
# Computed across the UNION of all three tables' Pct_vs_historical values,
# not per-table -- so S12.1/S12.2/S12.3 are colored on the SAME scale and
# visually comparable to each other, rather than each being individually
# normalized (which would make e.g. S12.3's worst-case cell look the same
# shade of red as S12.1's, even though the underlying magnitude differs
# substantially).

all_pct <- c(
  decomp_table_p50$Pct_vs_historical[decomp_table_p50$Scenario %in% names(stage_rows)],
  decomp_table_p17$Pct_vs_historical[decomp_table_p17$Scenario %in% names(stage_rows)],
  decomp_table_p83$Pct_vs_historical[decomp_table_p83$Scenario %in% names(stage_rows)]
)
color_lo <- min(all_pct, na.rm = TRUE)
color_hi <- max(all_pct, na.rm = TRUE)

color_ramp_fn <- function(pct) {
  if (is.na(pct)) return("#FFFFFF")
  frac <- scales::rescale(pct, from = c(color_lo, color_hi), to = c(0, 1))
  frac <- pmin(pmax(frac, 0), 1)
  mid_frac <- scales::rescale(0, from = c(color_lo, color_hi), to = c(0, 1))
  mid_frac <- pmin(pmax(mid_frac, 1e-6), 1 - 1e-6)
  ramp <- if (frac <= mid_frac) {
    colorRampPalette(c("#1A9850", "#FFFFFF"))(101)[round(100 * frac / mid_frac) + 1]
  } else {
    colorRampPalette(c("#FFFFFF", "#D73027"))(101)[round(100 * (frac - mid_frac) / (1 - mid_frac)) + 1]
  }
  ramp
}

text_color_for <- function(hex) {
  rgb_vals <- grDevices::col2rgb(hex)
  luminance <- 0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]
  if (luminance < 130) "white" else "black"
}

# =============================================================================
# SECTION III: BUILD ONE PART (2050 or 2100), FOR A GIVEN decomp_table
# =============================================================================
# Takes decomp_table as an explicit argument so it can be called against
# any of the three tables. Only S12.1 has confidence intervals to display
# via Ann_deaths_lcl/ucl -- S12.2/S12.3 always show NA for those columns
# since the bootstrap was skipped for them.

build_part_html <- function(decomp_table, yr, part_label) {
  hist_row <- decomp_table %>%
    filter(Year == yr, Scenario == "Historical baseline") %>%
    pull(Ann_deaths) %>%
    unique()

  rows_html <- character(0)

  hist_cells <- paste0(
    "<td style='border:1px solid #999; padding:5px; text-align:center; background:#f2f2f2;' colspan='",
    length(scenario_cols), "'>", format(hist_row, big.mark = ","), " deaths/yr</td>"
  )
  rows_html <- c(rows_html, paste0(
    "<tr><td style='border:1px solid #999; padding:5px; font-weight:bold;'>Historical baseline</td>",
    hist_cells, "</tr>"
  ))

  for (stage_raw in names(stage_rows)) {
    row_label <- stage_rows[[stage_raw]]
    row_data <- decomp_table %>%
      filter(Year == yr, Scenario == stage_raw, SSP %in% scenario_cols)

    cells <- vapply(scenario_cols, function(sc) {
      this_row <- row_data %>% filter(SSP == sc)
      if (nrow(this_row) == 0 || is.na(this_row$Ann_deaths[1])) {
        return("<td style='border:1px solid #999; padding:5px; text-align:center;'>NA</td>")
      }
      deaths <- format(this_row$Ann_deaths[1], big.mark = ",")
      pct    <- this_row$Pct_vs_historical[1]
      pct_str <- ifelse(pct >= 0, paste0("+", round(pct, 1), "%"), paste0(round(pct, 1), "%"))
      bg <- color_ramp_fn(pct)
      txt_col <- text_color_for(bg)
      paste0("<td style='border:1px solid #999; padding:5px; text-align:center; background:", bg,
             "; color:", txt_col, ";'>", deaths, "<br><span style='font-size:8pt;'>(", pct_str, ")</span></td>")
    }, character(1))

    rows_html <- c(rows_html, paste0(
      "<tr><td style='border:1px solid #999; padding:5px;'>", row_label, "</td>",
      paste(cells, collapse = ""), "</tr>"
    ))
  }

  scenario_display <- ifelse(scenario_cols == "BOUND_worst", "Worst case",
                       ifelse(scenario_cols == "BOUND_best", "Best case",
                              gsub("_", " ", scenario_cols)))
  header_cells <- paste0("<th style='border:1px solid #999; padding:5px; background:#e6e6e6;'>",
                         scenario_display, "</th>", collapse = "")

  paste0(
    "<h3>", part_label, "</h3>",
    "<table style='border-collapse:collapse; font-family:Calibri, sans-serif; font-size:10pt; width:100%;'>",
    "<tr><th style='border:1px solid #999; padding:5px; background:#e6e6e6;'>Decomposition stage</th>",
    header_cells, "</tr>",
    paste(rows_html, collapse = ""),
    "</table>"
  )
}

# =============================================================================
# SECTION IV: ASSEMBLE + WRITE OUTPUT (three sub-tables)
# =============================================================================

build_subtable <- function(decomp_table, subtable_label, subtitle) {
  paste0(
    "<h2>", subtable_label, ". ", subtitle, "</h2>",
    build_part_html(decomp_table, "2050", "Part A. 2050"),
    "<br><br>",
    build_part_html(decomp_table, "2100", "Part B. 2100"),
    "<br><br><hr><br>"
  )
}

html_out <- paste0(
  "<html><head><meta charset='utf-8'></head><body>",
  "<h1>Table S12. Total projected annual deaths by scenario, methodological decomposition stage, ",
  "and SLIIDERS SLR percentile</h1>",
  "<p>Cell values show total (global) projected annual deaths; parenthetical values show percent change vs. ",
  "the historical baseline. Red indicates more deaths than the historical baseline; green indicates fewer. ",
  "Color scale is shared across all three sub-tables (S12.1-S12.3) for direct visual comparison. ",
  "S12.2 and S12.3 show point estimates only (no confidence interval) -- see Methods/Figure S7 for the ",
  "SLIIDERS SLR percentile uncertainty characterization at the aggregate level.</p>",
  build_subtable(decomp_table_p50, "Table S12.1", "50th percentile SLR (central estimate)"),
  build_subtable(decomp_table_p17, "Table S12.2", "17th percentile SLR"),
  build_subtable(decomp_table_p83, "Table S12.3", "83rd percentile SLR"),
  "</body></html>"
)

out_path <- paste0("Output/Tables/Table S12 ", date_append_output, ".doc")
writeLines(html_out, out_path, useBytes = TRUE)

message("Table S12 (S12.1-S12.3) written to ", out_path)
