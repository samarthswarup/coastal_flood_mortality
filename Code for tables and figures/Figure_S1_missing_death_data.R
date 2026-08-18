##########################################
# SI Figure S1 - Missing death data by    #
# income level                            #
#                                         #
# 2026-07-14                              #
# Ryan Calder                             #
# Notes added by Claude                   #
##########################################

# Freestanding figure script, run independently of the numbered pipeline.
# Draws exclusively on trendData as saved by Step 8
# (Intermediate outputs/Output from Step 8/trendData <date>.RDS) -- no
# re-derivation of exposed populations or death counts. The only
# calculation here is the same light derived-field logic already used in
# Table S4 (death.rate, gni.group.baseline as an ordered factor,
# zeroDeath indicator) -- not a new calculation, same formulas as source.
#
# 8_ModelFitting_and_SLR_projections.R's own version of this barplot is already commented
# out there. The data-prep lines just above it in that script (df,
# filtered.df, df.gni, etc.) stay active, though -- they also feed that
# script's regression models (baseSet <- df.gni and downstream), so they
# can't be removed without breaking Step 8 itself.

rm(list = ls())

library(dplyr)

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
# SECTION II: DERIVE DEATH RATE + GNI GROUP FACTOR
# (identical logic to the original df$death.rate / df.gni / df.gnidr chain,
# and to Table S4's own derived fields)
# =============================================================================

df <- trendData %>%
  filter(Start.Year > 1989) %>%
  mutate(death.rate = total.deaths.na.as.0 / pop.exposed.local.else.adm1.else.national.100yr)

df.gni  <- df      %>% filter(!is.na(gni.group))
df.gnid <- df.gni  %>% filter(!is.na(death.rate))
df.gnidr <- df.gnid %>% filter(death.rate < 1)

df.gnidr$gni.group.baseline <- factor(df.gnidr$gni.group.baseline,
                                       levels = c("LI", "LMI", "UMI", "HI"))
df.gnidr$zeroDeath <- ifelse(df.gnidr$death.rate == 0, 1, 0)

zeroTable <- table(df.gnidr$zeroDeath, df.gnidr$gni.group.baseline)
zeroTable <- zeroTable[2, ] / (zeroTable[1, ] + zeroTable[2, ])

# =============================================================================
# SECTION III: PLOT (base-R barplot, matching original appearance exactly)
# =============================================================================

pdf(paste0("Output/Plots/Fig S1 ", date_append_output, ".pdf"),
    width = 5, height = 5)

barplot(zeroTable, col = c("#F8766D", "#C49A00", "#00BA38", "#619CFF"),
        ylim = c(0, 0.35), xlab = "Income Category",
        ylab = "Proportion of events with missing death data",
        las = 2)
lines(c(0.1, 4.9), c(0.05, 0.05), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.10, 0.10), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.15, 0.15), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.20, 0.20), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.25, 0.25), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.30, 0.30), lty = 2, col = "black")

box()
dev.off()

message("Figure written to Output/Plots/Fig S1 ", date_append_output, ".pdf")
