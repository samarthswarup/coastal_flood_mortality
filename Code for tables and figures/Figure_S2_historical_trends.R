##########################################
# Figure S2 - Historical trends           #
# (1990-2024) by GNI income group         #
#                                         #
# 2026-07-14                              #
# Ryan Calder                             #
# Notes added by Claude                   #
##########################################

# Freestanding figure script, run independently of the numbered pipeline.
# Plots the actual yearly event-level series underlying the historical
# trend statistics reported in SI Table S4, rather than just the
# Mann-Kendall/Sen's slope summary statistics on their own.
#
# Draws exclusively on trendData as saved by Step 8 -- no re-derivation of
# exposed populations, death counts, or death rates. Points are plotted at
# each event's real recorded date (not aggregated to yearly means), so the
# figure reflects actual clustering/spacing of events over time. Trend
# lines are LOESS smoothers (with 95% CI band) shown for visual reference
# only -- they are not a substitute for, and will not numerically match,
# the Mann-Kendall/Sen's slope test in SI Table S4, which tests the
# chronological per-event sequence (rank-based: only event order matters,
# not elapsed time between events). Table S4 remains the source of formal
# trend statistics (tau, p, slope); this figure should cite that table.

rm(list = ls())

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(scales)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Plots", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 8 run to pull trendData from
step8_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

YEAR_MIN <- 1990

# =============================================================================
# SECTION I: LOAD UPSTREAM OUTPUT
# =============================================================================

trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

# =============================================================================
# SECTION II: HARMONIZE FILTERING + DERIVED FIELDS
# (identical rules to Table S4 / rest of pipeline)
# =============================================================================

trendData <- trendData %>%
  filter(Start.Year >= YEAR_MIN,
         !is.na(pop.exposed.local.else.adm1.else.national.100yr),
         pop.exposed.local.else.adm1.else.national.100yr > 0) %>%
  mutate(
    death.rate.na.as.0 = total.deaths.na.as.0 / pop.exposed.local.else.adm1.else.national.100yr,
    death.rate.na.as.0 = ifelse(is.finite(death.rate.na.as.0) & death.rate.na.as.0 < 1, death.rate.na.as.0, NA_real_)
  )

trendData$gni.group <- factor(trendData$gni.group, levels = c("LI", "LMI", "UMI", "HI"))

# =============================================================================
# SECTION III: BUILD PER-EVENT SERIES (Global + each GNI group)
#
# Plotted at each event's real recorded date -- NOT aggregated to yearly
# means -- so the figure shows actual clustering/spacing of events, unlike
# the rank-based Mann-Kendall test in Table S4 (which only uses event
# sequence position, not elapsed time between events).
#
# Event Frequency is the one exception: "frequency" is inherently a count
# over an interval, so it is aggregated to yearly counts, plotted at Jan 1
# of each year (kept as a Date so it shares an x-axis type with the other,
# per-event panels).
# =============================================================================

make_events <- function(label, dat) {
  freq <- dat %>%
    filter(!is.na(Start.Year)) %>%
    count(Start.Year, name = "value") %>%
    mutate(Date = as.Date(paste0(Start.Year, "-01-01")),
           Variable = "Event Frequency") %>%
    select(Date, value, Variable)

  other <- dat %>%
    filter(!is.na(Start.Date)) %>%
    transmute(
      Date = Start.Date,
      `Estimated Exposed Population` = pop.exposed.local.else.adm1.else.national.100yr,
      `Deaths (NAs as zero)`         = total.deaths.na.as.0,
      `Estimated Death Rate (NAs as zero)` = death.rate.na.as.0
    ) %>%
    pivot_longer(-Date, names_to = "Variable", values_to = "value")

  bind_rows(freq, other) %>%
    mutate(Group = label)
}

events_all <- bind_rows(
  make_events("Global", trendData),
  make_events("LI",  filter(trendData, gni.group == "LI")),
  make_events("LMI", filter(trendData, gni.group == "LMI")),
  make_events("UMI", filter(trendData, gni.group == "UMI")),
  make_events("HI",  filter(trendData, gni.group == "HI"))
)

var_order <- c(
  "Event Frequency",
  "Estimated Exposed Population",
  "Deaths (NAs as zero)",
  "Estimated Death Rate (NAs as zero)"
)

events_all$Variable <- factor(events_all$Variable, levels = var_order)
events_all$Group <- factor(events_all$Group, levels = c("Global", "LI", "LMI", "UMI", "HI"))

# =============================================================================
# SECTION IV: PLOT
# =============================================================================

pal <- c(Global = "black", LI = "#F8766D", LMI = "#C49A00", UMI = "#00BA38", HI = "#619CFF")

figure.s2 <- ggplot(events_all, aes(x = Date, y = value, color = Group, fill = Group)) +
  geom_point(alpha = 0.5, size = 1.1) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 0.9) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    x = "Date",
    y = NULL,
    color = "GNI group",
    fill = "GNI group",
    title = "Historical trends in coastal flood events, exposure, and mortality (1990-2024)",
    caption = paste(
      "Points = individual events at their recorded date (Event Frequency = yearly counts, plotted at Jan 1 of each year).",
      "Lines = LOESS-smoothed trend with 95% CI band, shown for visual reference only -- not a statistical trend test.",
      "Formal Mann-Kendall/Sen's slope trend tests (fit to the chronological per-event sequence) are reported in SI Table S4.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, size = 8, color = "grey40")
  )

# =============================================================================
# SECTION V: WRITE OUTPUT
# =============================================================================

ggsave(
  filename = paste0("Output/Plots/Fig S2 ", date_append_output, ".pdf"),
  plot = figure.s2,
  width = 10, height = 8, units = "in", dpi = 300
)

message("Figure S2 written to Output/Plots/Fig S2 ", date_append_output, ".pdf")
