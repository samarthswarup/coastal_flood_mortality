###########################################
# SI Figure S6.1 (2050) / S6.2 (2100)     #
# Bounding scenarios (worst/best case)    #
# across six methodological stages        #
#                                         #
# 2026-07-17                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding figure script, run independently of the numbered pipeline.
# Countries with >= 50 historical events, one point + 95% CI error bar per
# country, showing BOUND_worst and BOUND_best side by side (dodged),
# faceted by all six methodology decomposition stages -- so a reader can
# see how each successive methodological addition (population exposure,
# HDI, each coastal-growth multiplier) changes the best-case/worst-case
# spread, not just the headline estimate. Produces two separate figures
# (one for 2050, one for 2100), each a 2x3 grid of stage-facets.
#
# Draws on the same Step 8/Step 9 outputs as Figure 4 and Table S11 --
# CountryDeathCIs.csv (historical baseline + event-count threshold) and
# country_master (the projections themselves) -- using the identical
# historical-baseline formula used throughout this project. No
# re-derivation of any modeled quantities.

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(ggplot2)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Plots", showWarnings = FALSE, recursive = TRUE)

# Date suffixes identifying which Step 8 / Step 9 runs to pull from
step8_date_append_output <- "2026-07-20"
step9_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

N_YEARS <- 35   # matches Step 9's historical study period length

# =============================================================================
# USER CONFIGURATION
# =============================================================================

# Two bounding scenarios shown side by side
scenarios_use <- c("BOUND_worst", "BOUND_best")

# Panel titles: simple "(N)" labels only -- full stage definitions live
# in the SI figure caption, not in the plot itself.
stage_labels <- c(
  " SLR only" = "(1)",
  " + Population (exposure only)" = "(2)",
  " + Population (exposure + NB covariate)" = "(3)",
  " + Population (both) & HDI" = "(4)",
  " + Population (both) & HDI & coastal growth 1" = "(5)",
  " + Population (both) & HDI & coastal growth 2" = "(6)"
)

# Minimum historical event count for inclusion (matches SI Figure S6)
MIN_EVENTS <- 50

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

# =============================================================================
# SECTION II: HISTORICAL BASELINE + PERCENT CHANGE, ALL SIX STAGES
# (identical formula to Figure 4 / Table S11 / SI Figure S6)
# =============================================================================

hist_country <- ci_base %>%
  transmute(Country_Code, hist_ann_deaths = meanDeaths * nEvents / N_YEARS,
            nEvents_hist = nEvents)

proj_data <- country_master %>%
  filter(methodology %in% names(stage_labels), ssp %in% scenarios_use) %>%
  left_join(hist_country, by = "Country_Code") %>%
  filter(!is.na(hist_ann_deaths), hist_ann_deaths > 0) %>%
  filter(nEvents_hist >= MIN_EVENTS) %>%
  mutate(
    `Methodology stage` = factor(stage_labels[methodology], levels = stage_labels),
    ssp = factor(ssp, levels = scenarios_use),
    pctInc     = 100 * (ann_deaths - hist_ann_deaths) / hist_ann_deaths,
    pctInc_lcl = 100 * (ann_deaths_lcl - hist_ann_deaths) / hist_ann_deaths,
    pctInc_ucl = 100 * (ann_deaths_ucl - hist_ann_deaths) / hist_ann_deaths
  )

if (nrow(proj_data) == 0) {
  stop("No countries with >= ", MIN_EVENTS, " historical events found for scenarios '",
       paste(scenarios_use, collapse = "', '"), "' -- check ssp/methodology labels match ",
       "country_master's actual values.")
}

# Country order: sorted by descending 2050 pctInc under the FINAL stage +
# BOUND_worst (the headline worst-case), held consistent across every
# facet/panel/year for easy visual comparison.
country_order <- proj_data %>%
  filter(year == "2050", methodology == names(stage_labels)[6], ssp == "BOUND_worst") %>%
  arrange(desc(pctInc)) %>%
  pull(Country_Code)
country_order <- c(country_order, setdiff(unique(proj_data$Country_Code), country_order))

proj_data <- proj_data %>%
  mutate(Country_Code = factor(Country_Code, levels = country_order))

# =============================================================================
# SECTION III: PLOT -- ONE FIGURE PER YEAR, FACETED BY STAGE, SCENARIOS
# DODGED WITHIN EACH FACET
# =============================================================================

pal <- c(BOUND_worst = "#B2182B", BOUND_best = "#2166AC")
scenario_display_labels <- c(BOUND_worst = "Worst case", BOUND_best = "Best case")

make_year_figure <- function(yr) {
  cur <- proj_data %>% filter(year == yr)

  ggplot(cur, aes(x = Country_Code, y = pctInc, color = ssp)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = pctInc_lcl, ymax = pctInc_ucl),
                  position = position_dodge(width = 0.6), width = 0.2, linewidth = 0.5) +
    geom_point(position = position_dodge(width = 0.6), size = 1.6) +
    facet_wrap(~`Methodology stage`, ncol = 2, scales = "free_y") +
    scale_color_manual(values = pal, labels = scenario_display_labels, name = "Scenario") +
    labs(
      x = "Country",
      y = paste0("Percent change in average deaths (", yr, ")")
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      legend.position = "bottom"
    )
}

figure_2050 <- make_year_figure("2050")
figure_2100 <- make_year_figure("2100")

# =============================================================================
# SECTION IV: WRITE OUTPUT (two separate files, one per year)
# =============================================================================

ggsave(
  filename = paste0("Output/Plots/Fig S6.1 ", date_append_output, ".pdf"),
  plot = figure_2050,
  width = 12, height = 11, units = "in", dpi = 300
)

ggsave(
  filename = paste0("Output/Plots/Fig S6.2 ", date_append_output, ".pdf"),
  plot = figure_2100,
  width = 12, height = 11, units = "in", dpi = 300
)

message("SI Figure S6.1 (2050) and S6.2 (2100) written to Output/Plots/")
