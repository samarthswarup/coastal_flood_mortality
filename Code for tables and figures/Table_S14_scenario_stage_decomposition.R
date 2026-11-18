# Tables S14.1–S14.3
#
# Purpose: Formats global mortality estimates by methodological stage for median, 17th-, and 83rd-percentile SLR scenarios.
#
# Inputs:
#   - Step 9: decomposition_table_population_methodology_<date>.csv (p50)
#   - Step 9a: decomposition_table_population_methodology_p17_<date>.csv and p83_<date>.csv
#
# Outputs:
#   - Output/Tables/Table S14 <date>.doc (Word-openable HTML)
#
# R packages: dplyr, tidyr, scales
# Run from the analysis directory containing Intermediate outputs/.
#

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(tidyr)
library(scales)

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

step9_date_append_output <- "2026-11-14"
step9_dir <- "Intermediate outputs/Output from Step 9"

find_percentile_dates <- function(perc) {
  pat <- paste0("^decomposition_table_population_methodology_p", perc,
                "_([0-9]{4}-[0-9]{2}-[0-9]{2})\\.csv$")
  files <- list.files(step9_dir, pattern = pat)
  sub(pat, "\\1", files)
}
paired_dates <- intersect(find_percentile_dates("17"), find_percentile_dates("83"))
if (!length(paired_dates)) {
  stop("No matching Step 9a p17/p83 decomposition tables found. ",
       "Run scripts/9a_SLR_percentiles_Table_S14.R first.")
}
step9a_date_append_output <- max(paired_dates)
message("Using Step 9a p17/p83 files dated: ", step9a_date_append_output)

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

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

load_decomp <- function(path, label) {
  if (!file.exists(path)) {
    available <- list.files(dirname(path), pattern = "^decomposition_table_population_methodology")
    stop(
      "File not found for ", label, ": ", path, "\n",
      "Files actually present in that folder:\n  ", paste(available, collapse = "\n  "),
      "\nCheck the Step 9 and Step 9a output dates."
    )
  }
  read.csv(path, stringsAsFactors = FALSE)
}

decomp_table_p50 <- load_decomp(
  file.path(step9_dir,
            paste0("decomposition_table_population_methodology_", step9_date_append_output, ".csv")),
  "S14.1 (50th percentile)"
)
decomp_table_p17 <- load_decomp(
  paste0("Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_p17_", step9a_date_append_output, ".csv"),
  "S14.2 (17th percentile)"
)
decomp_table_p83 <- load_decomp(
  paste0("Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_p83_", step9a_date_append_output, ".csv"),
  "S14.3 (83rd percentile)"
)

scenario_cols <- decomp_table_p50 %>%
  filter(Year == "2050", Scenario == " + Population (both) & HDI & coastal growth 2",
         SSP %in% scenarios_matched) %>%
  arrange(Pct_vs_historical) %>%
  pull(SSP)

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
  "<h1>Table S14. Total projected annual deaths by scenario, methodological decomposition stage, ",
  "and SLIIDERS SLR percentile</h1>",
  "<p>Cell values show total (global) projected annual deaths; parenthetical values show percent change vs. ",
  "the historical baseline. Red indicates more deaths than the historical baseline; green indicates fewer. ",
  "Color scale is shared across all three sub-tables (S14.1-S14.3) for direct visual comparison. ",
  "S14.2 and S14.3 show point estimates only (no confidence interval) -- see Methods/Figure S8 for the ",
  "SLIIDERS SLR percentile uncertainty characterization at the aggregate level.</p>",
  build_subtable(decomp_table_p50, "Table S14.1", "50th percentile SLR (central estimate)"),
  build_subtable(decomp_table_p17, "Table S14.2", "17th percentile SLR"),
  build_subtable(decomp_table_p83, "Table S14.3", "83rd percentile SLR"),
  "</body></html>"
)

out_path <- paste0("Output/Tables/Table S14 ", date_append_output, ".doc")
writeLines(html_out, out_path, useBytes = TRUE)

message("Table S14 (S14.1-S14.3) written to ", out_path)
