###########################################
# SI Figure S4 - Exposed populations by   #
# income category and spatial definition  #
#                                         #
# 2026-07-14                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding figure script, run independently of the numbered pipeline.
# Two panels comparing estimated exposed population across World Bank GNI
# groups:
#   A: all coastal flooding events
#   B: only events with sub-country (local or admin-1) location information
#
# Both panels filter to Start.Year >= 1990, matching the same 1990-2024
# window used throughout Table S4/Fig S2/Fig S3, and both use
# var.equal = FALSE (Welch ANOVA), matching Fig S3 -- more robust to
# unequal variances across groups, appropriate here since exposed
# population is highly skewed and its spread differs a lot across income
# groups.

rm(list = ls())

library(dplyr)
library(readxl)
library(ggstatsplot)
library(scales)
library(patchwork)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Plots", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 7 run to pull the final event-level
# output from
step7_date_append_output <- "2026-07-05"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# SECTION I: LOAD UPSTREAM OUTPUT
# =============================================================================

emdat.data.filtered <- read_excel(
  paste0("Intermediate outputs/Output from Step 7/",
         "EMDAT events with exposed pops, HDI GNI and prev events ",
         step7_date_append_output, ".xlsx")
) %>%
  filter(Start.Year >= 1990)

# =============================================================================
# SECTION II: PANEL A - ALL EVENTS
# =============================================================================

pal <- c(LI = "#F8766D", LMI = "#C49A00", UMI = "#00BA38", HI = "#619CFF")

pA <- emdat.data.filtered %>%
  dplyr::filter(
    !is.na(gni.group),
    !is.na(pop.exposed.local.else.adm1.else.national.100yr),
    pop.exposed.local.else.adm1.else.national.100yr > 0
  ) %>%
  dplyr::mutate(
    gni.group = factor(gni.group, levels = c("LI", "LMI", "UMI", "HI"))
  ) %>%
  ggstatsplot::ggbetweenstats(
    x = gni.group,
    y = pop.exposed.local.else.adm1.else.national.100yr,
    type = "parametric",
    var.equal = FALSE,
    mean.plotting = TRUE,
    mean.ci = TRUE,
    pairwise.comparisons = FALSE,
    xlab = "GNI income group",
    ylab = "Estimated exposed population",
    title = "A. All coastal flooding events"
  ) +
  scale_y_continuous(labels = scientific) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal)

# =============================================================================
# SECTION III: PANEL B - EVENTS WITH SUB-COUNTRY LOCATION DATA
# =============================================================================

pB <- emdat.data.filtered %>%
  filter(
    !is.na(gni.group),
    (!is.na(pop.exposed.local.if.available.100yr) |
       !is.na(pop.exposed.adm1.if.available.100yr))
  ) %>%
  mutate(
    gni.group = factor(gni.group, levels = c("LI", "LMI", "UMI", "HI"))
  ) %>%
  ggbetweenstats(
    x = gni.group,
    y = pop.exposed.local.else.adm1.else.national.100yr,
    type = "parametric",
    var.equal = FALSE,
    mean.plotting = TRUE,
    mean.ci = TRUE,
    pairwise.comparisons = FALSE,
    xlab = "GNI income group",
    ylab = "Estimated exposed population",
    title = "B. Events with sub-country locations"
  ) +
  scale_y_continuous(labels = scientific) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal)

# =============================================================================
# SECTION IV: ASSEMBLE + WRITE OUTPUT
# =============================================================================

figure.s4 <- pA + pB +
  plot_annotation(
    title = "Comparison of estimated exposed populations grouped by country income category"
  )

ggsave(
  filename = paste0("Output/Plots/Fig S4 ", date_append_output, ".pdf"),
  plot = figure.s4,
  width = 12, height = 6, units = "in",
  dpi = 300
)

message("SI Figure S4 written to Output/Plots/Fig S4 ", date_append_output, ".pdf")
