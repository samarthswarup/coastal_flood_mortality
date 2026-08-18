###########################################
# Table S9 - Flood recurrence interval    #
# sensitivity (exposed pop. + death rate) #
#                                         #
# 2026-07-14                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding table script, run independently of the numbered pipeline.
#
# Purely descriptive (no model fitting): for each of 4 flood recurrence
# intervals (10/100/1000/10000yr) used to define "exposed population,"
# reports:
#   - per.event.pop.M: mean exposed population per event (millions),
#     pooled across all qualifying events equally
#   - per.event.death.rate: mean(deaths / exposure) per event, pooled
#     across all qualifying events equally
#   - per.country.avg.per.event.pop.M: mean exposed population per event,
#     averaged WITHIN each country first, then averaged ACROSS countries
#     (unweighted by event count -- gives small and large countries equal
#     weight, unlike the per-event version above)
#   - per.country.avg.per.event.death.rate: same country-then-average
#     logic applied to the death rate
#
# SOURCE-DATA NOTE: this script starts from trendData (Step 8's saved
# output, not pre-filtered by year or by country) and applies its own
# Start.Year >= 1990 and SLIIDERS.country.code.flat != "PRK" filters
# directly.
#
# NOTE ON NAMING: ".no.0" here means "exclude ZERO-death events"
# (total.deaths.na.as.0 > 0) -- NOT "exclude missing-death events," which
# is what ".no.0"/"na.as.0" mean elsewhere in this codebase (e.g. Table
# S5/S6's "Deaths (NAs as zero)" column). Easy to misread.
#
# Draws exclusively on trendData as saved by Step 8 -- no re-derivation of
# exposed populations or death counts.

rm(list = ls())

library(dplyr)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 8 run to pull trendData from
step8_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# SECTION I: LOAD UPSTREAM OUTPUT + BUILD FILTERED SUBSETS
# =============================================================================

trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

df.adj <- trendData
df.adj$Country_Code <- df.adj$SLIIDERS.country.code.flat

df.since.1990.no.prk <- df.adj[
  df.adj$Start.Year >= 1990 & df.adj$SLIIDERS.country.code.flat != "PRK", ]

df.since.1990.no.prk.no.0 <- df.adj[
  df.adj$Start.Year >= 1990 & df.adj$SLIIDERS.country.code.flat != "PRK" &
    df.adj$total.deaths.na.as.0 > 0, ]

# =============================================================================
# SECTION II: COMPUTE PER-RECURRENCE-INTERVAL STATISTICS
# =============================================================================

flood.recur <- c("10", "100", "1000", "10000")
exp.name.vec <- paste0("pop.exposed.local.else.adm1.else.national.", flood.recur, "yr")

per.event.pop <- unlist(lapply(exp.name.vec, function(x)
  mean(df.since.1990.no.prk.no.0[[x]], na.rm = TRUE)))
per.event.pop.M <- per.event.pop / 10^6

per.event.death.rate <- unlist(lapply(exp.name.vec, function(x)
  mean(df.since.1990.no.prk.no.0$total.deaths.na.as.0 /
         df.since.1990.no.prk.no.0[[x]], na.rm = TRUE)))

per.country.avg.per.event.pop <- unlist(lapply(exp.name.vec, function(x)
  mean(unlist(lapply(unique(df.since.1990.no.prk$Country_Code), function(y)
    mean((df.since.1990.no.prk[[x]])[which(df.since.1990.no.prk$Country_Code == y)],
         na.rm = TRUE))))))
per.country.avg.per.event.pop.M <- per.country.avg.per.event.pop / 10^6

per.country.avg.per.event.death.rate <- unlist(lapply(exp.name.vec, function(x)
  mean(unlist(lapply(unique(df.since.1990.no.prk$Country_Code), function(y)
    mean((df.since.1990.no.prk$total.deaths.na.as.0)[which(df.since.1990.no.prk$Country_Code == y)] /
           (df.since.1990.no.prk[[x]])[which(df.since.1990.no.prk$Country_Code == y)],
         na.rm = TRUE))))))

table.s9 <- cbind.data.frame(
  flood.recur,
  per.event.pop.M = round(per.event.pop.M, 2),
  per.event.death.rate = round(per.event.death.rate, 4),
  per.country.avg.per.event.pop.M = round(per.country.avg.per.event.pop.M, 2),
  per.country.avg.per.event.death.rate = round(per.country.avg.per.event.death.rate, 4)
)

# =============================================================================
# SECTION III: WRITE OUTPUT
# =============================================================================

write.csv(
  table.s9,
  file = paste0("Output/Tables/Table S9 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S9 written to Output/Tables/Table S9 ", date_append_output, ".csv")
