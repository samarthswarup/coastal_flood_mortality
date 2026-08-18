##########################################
# Figure 3 - Scenario projections         #
#                                         #
# 2026-07-14                              #
# Ryan Calder                             #
# Notes added by Claude                   #
##########################################

# Freestanding figure script, built directly on Step 9's final output --
# not a relocation of any plotting code from 8_ModelFitting_and_SLR_projections.R, since
# Step 9's six-row population/HDI/coastal-growth decomposition and
# decoupled BOUND_worst/BOUND_best bounding scenarios have no equivalent
# in that script's plotting logic. 9_HDI_pop_time_varying_projections.R itself contains no
# plotting code at all -- it only writes decomp_table/country_master as
# CSVs.
#
# Draws exclusively on decomp_table as saved by Step 9
# (Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_<date>.csv) --
# no re-derivation of projected deaths, CIs, or the historical baseline.
# The only new calculation here is converting the already-final
# Ann_deaths_lcl/Ann_deaths_ucl (absolute counts) into the same
# percent-vs-historical units as Pct_vs_historical, using the historical
# baseline value already present in decomp_table's own
# "Historical baseline" rows -- i.e., simple arithmetic on final columns,
# not a recalculation.

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(ggplot2)
library(scales)
library(RColorBrewer)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Plots", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 9 run to pull decomp_table from
step9_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# USER CONFIGURATION: choose which scenarios appear in Figure 3
# =============================================================================
# Valid scenario labels (must match the `SSP` column in decomp_table, which
# in turn comes from Step 9's `ssp_pairings` list):
#
#   Core SSP-consistent (AR6_med SLR):
#     "SSP1.19", "SSP1.26_med", "SSP2.45", "SSP3.70", "SSP5.85_med"
#
#   AR6_low SLR sensitivity variants (same SSP, lower-confidence SLR):
#     "SSP1.26_low", "SSP5.85_low", "BOUND_worst_low"
#
#   Decoupled bounding scenarios (forcing and population/HDI deliberately
#   mismatched -- NOT standard SSP-RCP pairings):
#     "BOUND_worst", "BOUND_best"
#
# Edit this vector to control what appears in the figure. Order given here
# is the order scenarios will appear on the x-axis.

scenarios_to_plot <- c(
  "SSP1.19",
  "SSP1.26_med",
  "SSP2.45",
  "SSP3.70",
  "SSP5.85_med",
  "BOUND_worst",
  "BOUND_best"
)

# Which two decomposition stages to compare, top row vs. bottom row. Must
# match values in decomp_table's `Scenario` column. Full list of valid
# stages, in pipeline order:
#   " SLR only"
#   " + Population (exposure only)"
#   " + Population (exposure + NB covariate)"
#   " + Population (both) & HDI"
#   " + Population (both) & HDI & coastal growth 1"   [Rentschler multiplier]
#   " + Population (both) & HDI & coastal growth 2"   [Tellman multiplier]
#
# Default: top row = SLR + population exposure only, with no
# population-level mortality response (i.e. no NB population covariate,
# HDI, or coastal growth yet); bottom row = fully decomposed estimate
# (+ NB covariate + HDI + coastal growth). Edit these two to compare any
# other pair of stages.

stage_top    <- " + Population (exposure only)"
stage_bottom <- " + Population (both) & HDI & coastal growth 2"

stage_labels <- c("SLR + population exposure only",
                  "+ HDI + coastal growth")
names(stage_labels) <- c(stage_top, stage_bottom)

# =============================================================================
# SECTION I: LOAD UPSTREAM OUTPUT
# =============================================================================

decomp_table <- read.csv(
  paste0("Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_",
         step9_date_append_output, ".csv"),
  stringsAsFactors = FALSE
)

# =============================================================================
# SECTION II: FILTER TO SELECTED SCENARIOS/STAGE + DERIVE PERCENT CIs
# =============================================================================

# Historical baseline (same value for both years by construction in Step 9,
# but keep it per-Year in case that ever changes)
baseline <- decomp_table %>%
  filter(Scenario == "Historical baseline") %>%
  dplyr::select(Year, hist_ann_deaths = Ann_deaths)

plot_data <- decomp_table %>%
  filter(Scenario %in% c(stage_top, stage_bottom), SSP %in% scenarios_to_plot) %>%
  left_join(baseline, by = "Year") %>%
  mutate(
    Pct_lcl = 100 * (Ann_deaths_lcl - hist_ann_deaths) / hist_ann_deaths,
    Pct_ucl = 100 * (Ann_deaths_ucl - hist_ann_deaths) / hist_ann_deaths,
    Stage_label = factor(stage_labels[Scenario], levels = stage_labels),
    Year = factor(Year, levels = c("2050", "2100"))
  )

missing_scenarios <- setdiff(scenarios_to_plot, unique(plot_data$SSP))
if (length(missing_scenarios) > 0) {
  warning("Requested scenario(s) not found in decomp_table: ",
          paste(missing_scenarios, collapse = ", "))
}

# Order scenarios by ascending 2050 value in the BOTTOM row (fully
# decomposed estimate) -- the headline stage -- and hold that order
# consistent across both rows/both years.
scenario_order <- plot_data %>%
  filter(Scenario == stage_bottom, Year == "2050") %>%
  arrange(Pct_vs_historical) %>%
  pull(SSP)

# Fall back to scenarios_to_plot order for any scenario missing a 2050
# bottom-row value (shouldn't normally happen)
scenario_order <- c(scenario_order, setdiff(scenarios_to_plot, scenario_order))

plot_data <- plot_data %>%
  mutate(SSP = factor(SSP, levels = scenario_order))

# =============================================================================
# SECTION III: PLOT
# =============================================================================

n_scen <- length(scenario_order)
# Skip YlGnBu's 2 lightest shades (near-white/pale yellow -- nearly
# invisible on a white background) before interpolating across the
# remaining, better-contrast portion of the ramp.
pal_full <- RColorBrewer::brewer.pal(9, "YlGnBu")
pal <- colorRampPalette(pal_full[3:9])(n_scen)
names(pal) <- scenario_order

# Panel letter labels (a)-(d) for the 2x2 Stage_label x Year grid, in
# standard reading order (top-left, top-right, bottom-left, bottom-right)
panel_letters <- expand.grid(
  Stage_label = levels(plot_data$Stage_label),
  Year = levels(plot_data$Year)
) %>%
  arrange(Stage_label, Year) %>%
  mutate(label = paste0("(", letters[1:4], ")"))

figure.3 <- ggplot(plot_data, aes(x = SSP, y = Pct_vs_historical, color = SSP)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = Pct_lcl, ymax = Pct_ucl), width = 0.15, linewidth = 0.7) +
  geom_point(size = 2.6) +
  geom_text(data = panel_letters, aes(x = -Inf, y = Inf, label = label),
            inherit.aes = FALSE, hjust = -0.3, vjust = 1.5,
            fontface = "bold", size = 4.2) +
  facet_grid(Stage_label ~ Year,
             labeller = labeller(Year = c(`2050` = "By 2050", `2100` = "By 2100"))) +
  scale_color_manual(values = pal) +
  labs(
    x = "Scenario",
    y = "Projected change in annual deaths\nvs. historical baseline (%)",
    title = "Scenario projections of coastal flood mortality",
    caption = paste0(
      "Top row: SLR + population exposure only (no population-level mortality response). Bottom row: fully\n",
      "decomposed estimate (adds population growth as both increased exposure AND a national-population covariate\n",
      "in the mortality regression, plus projected HDI and a coastal growth multiplier).\n",
      "Scenarios ordered by ascending bottom-row 2050 value. Error bars = 95% CI."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, size = 8, color = "grey40")
  )

# =============================================================================
# SECTION IV: WRITE OUTPUT
# =============================================================================

ggsave(
  filename = paste0("Output/Plots/Fig 3 ", date_append_output, ".pdf"),
  plot = figure.3,
  width = 8, height = 8, units = "in", dpi = 300
)

message("Figure 3 written to Output/Plots/Fig 3 ", date_append_output, ".pdf")
