###########################################
# Table S10 - Model comparison summary    #
# (N events/countries, avg exposed pop,   #
# predicted death rate, all 10 models)    #
#                                         #
# 2026-07-16                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding table script, run independently of the numbered pipeline.
# Uses the already-fitted predMod.1-10 objects directly -- no refitting,
# guaranteeing consistency with every other number reported elsewhere in
# the paper (same choice made for Table S7's regression tables).
#
# For each model, pulls the ACTUAL fitting data via model.frame(m),
# back-transforms the model's own offset column to get each event's
# exposure, gets predicted death counts via predict(m, type="response"),
# then reports:
#   - Number of Events = nrow(model frame)
#   - Number of Countries = distinct Country_Code in model frame
#   - Estimated avg. exposed pop. per event (M) = mean(exposure)/1e6
#   - Predicted average per-event death rate = mean(pred_counts/exposure)
#
# Draws exclusively on the 10 saved predMod RDS files from Step 8 -- no
# re-derivation of model fits. Also reconstructs baseKeep/baseKeep.tc/
# baseKeep.jonkman/baseKeep.since.2000 (same logic as
# Figure_S5_model_diagnostics.R and Table_S7_model_comparison.R), since
# model.frame() needs to resolve each model's fitting-data environment by
# name.

rm(list = ls())

library(dplyr)
library(readxl)
library(splines2)
library(purrr)
library(tibble)
library(glmmTMB)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 8 run to pull predMod.1-10 from
step8_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# SECTION I: RECONSTRUCT baseKeep / baseKeep.since.2000 / baseKeep.tc /
# baseKeep.jonkman (identical to Figure_S5_model_diagnostics.R and
# Table_S7_model_comparison.R)
# =============================================================================

step7_date_append_output <- "2026-07-20"

emdat.data.filtered <- readxl::read_excel(
  paste0("Intermediate outputs/Output from Step 7/EMDAT events with exposed pops, HDI GNI and prev events ",
         step7_date_append_output, ".xlsx")
)

df <- emdat.data.filtered %>%
  dplyr::mutate(DisNo.nocoun = gsub("[A-Za-z]", "", DisNo.))

df$Year <- df$Start.Year - 1990
df$no.events.5.years <- NA_integer_
for (i in seq_len(nrow(df))) {
  if (df$Start.Year[i] >= 1990 && df$Start.Year[i] <= 2024) {
    df$no.events.5.years[i] <- sum(df$Start.Year %in% (df$Start.Year[i] - 5):df$Start.Year[i] &
                                     df$ISO == df$ISO[i])
  }
}

df.gni <- df %>% dplyr::filter(!is.na(gni.group))

baseSet <- df.gni
baseSet$expPop10             <- baseSet$pop.exposed.local.else.adm1.else.national.10yr
baseSet$expPop100            <- baseSet$pop.exposed.local.else.adm1.else.national.100yr
baseSet$expPop1000           <- baseSet$pop.exposed.local.else.adm1.else.national.1000yr
baseSet$expPop10000          <- baseSet$pop.exposed.local.else.adm1.else.national.10000yr
baseSet$expPop100.adm0       <- baseSet$pop.exposed.adm0.100yr
baseSet$expPop100.adm1.only  <- baseSet$pop.exposed.adm1.if.available.100yr
baseSet$spline_year          <- splines2::bSpline(baseSet$Year, df = 3)

baseKeepVars <- c("total.deaths.na.as.0", "total.deaths.reported",
                  "spline_year", "hdi", "Declaration", "no.events.5.years",
                  "pop.total.adm0", "Disaster.Subtype", "DisNo.nocoun",
                  "SLIIDERS.country.code.flat", "Disaster.Group", "Disaster.Subgroup",
                  "expPop10", "expPop100", "expPop1000", "expPop10000",
                  "expPop100.adm0", "expPop100.adm1.only", "Start.Year")

baseKeep <- data.frame(baseSet[, names(baseSet) %in% baseKeepVars])
names(baseKeep)[names(baseKeep) == "SLIIDERS.country.code.flat"] <- "Country_Code"

baseKeep$log_expPop100.adm0 <- log(baseKeep$expPop100.adm0)
baseKeep$log_expPop100      <- log(baseKeep$expPop100)
baseKeep$log_expPop100.adm1 <- log(baseKeep$expPop100.adm1.only)
baseKeep$log_expPop10       <- log(baseKeep$expPop10)
baseKeep$log_expPop1000     <- log(baseKeep$expPop1000)
baseKeep$log_expPop10000    <- log(baseKeep$expPop10000)

baseKeep <- subset(baseKeep, !is.na(hdi) & is.finite(pop.total.adm0))

baseKeep.since.2000 <- baseKeep[baseKeep$Start.Year >= 2000, ]

baseSet.tc <- baseSet[baseSet$Disaster.Subtype == "Tropical cyclone", ]
baseSet.tc$spline_year <- splines2::bSpline(baseSet.tc$Year, df = 3)
baseKeep.tc <- data.frame(baseSet.tc[, names(baseSet.tc) %in% baseKeepVars])
baseKeep.tc <- subset(baseKeep.tc, !is.na(hdi) & !is.na(expPop100) & expPop100 > 0)
names(baseKeep.tc)[names(baseKeep.tc) == "SLIIDERS.country.code.flat"] <- "Country_Code"
baseKeep.tc$log_expPop100 <- log(baseKeep.tc$expPop100)

baseSet.jonkman <- baseSet[
  baseSet$Disaster.Group == "Natural" &
    baseSet$Disaster.Subgroup %in% c("Hydrological", "Meteorological") &
    baseSet$Disaster.Subtype %in% c("Coastal flood", "Storm surge"),
]
baseSet.jonkman$spline_year <- splines2::bSpline(baseSet.jonkman$Year, df = 3)
baseKeep.jonkman <- data.frame(baseSet.jonkman[, names(baseSet.jonkman) %in% baseKeepVars])
baseKeep.jonkman <- subset(baseKeep.jonkman, !is.na(hdi) & !is.na(expPop100) & expPop100 > 0)
names(baseKeep.jonkman)[names(baseKeep.jonkman) == "SLIIDERS.country.code.flat"] <- "Country_Code"
baseKeep.jonkman$log_expPop100 <- log(baseKeep.jonkman$expPop100)

# =============================================================================
# SECTION II: LOAD THE 10 FITTED MODEL OBJECTS
# =============================================================================

for (i in 1:10) {
  eval(parse(text = paste0(
    "predMod.", i, " <- readRDS('Intermediate outputs/Output from Step 8/predMod.", i, " ",
    step8_date_append_output, ".RDS')"
  )))
}

# =============================================================================
# SECTION III: SUMMARIZE EACH MODEL
# =============================================================================

model_desc <- c(
  "Model 1"  = "ADM0 exposures",
  "Model 2"  = "ADM1-only exposures",
  "Model 3"  = "Local else national (deaths NAs->0) -- Main",
  "Model 4"  = "NA-death events removed",
  "Model 5"  = "10-yr recurrence",
  "Model 6"  = "1000-yr recurrence",
  "Model 7"  = "10000-yr recurrence",
  "Model 8"  = "Events since 2000",
  "Model 9"  = "Tropical cyclones only",
  "Model 10" = "Jonkman et al. (2024) coastal floods"
)

summ_one <- function(m, name) {
  mf <- model.frame(m)                       # data actually used
  off_col <- grep("^offset\\(", names(mf), value = TRUE)
  stopifnot(length(off_col) == 1)
  exposure <- exp(mf[[off_col]])             # back-transform from log(exposure)

  pred_counts <- predict(m, type = "response")   # expected deaths (counts)

  tibble(
    Model = name,
    `Model description` = model_desc[[name]],
    `Number of Events` = nrow(mf),
    `Number of Countries` = dplyr::n_distinct(mf$Country_Code),
    `Estimated average affected population per event (millions)` =
      mean(exposure, na.rm = TRUE) / 1e6,
    `Predicted average per event death rate` =
      mean(pred_counts / exposure, na.rm = TRUE)
  )
}

mods <- list(
  `Model 1` = predMod.1, `Model 2` = predMod.2, `Model 3` = predMod.3,
  `Model 4` = predMod.4, `Model 5` = predMod.5, `Model 6` = predMod.6,
  `Model 7` = predMod.7, `Model 8` = predMod.8, `Model 9` = predMod.9,
  `Model 10` = predMod.10
)

tbl_models <- imap_dfr(mods, summ_one) %>%
  mutate(
    Model = factor(Model, levels = paste("Model", 1:10)),
    .before = 1
  ) %>%
  arrange(Model) %>%
  mutate(
    `Estimated average affected population per event (millions)` =
      round(`Estimated average affected population per event (millions)`, 2),
    `Predicted average per event death rate` =
      signif(`Predicted average per event death rate`, 3)
  )

# =============================================================================
# SECTION IV: WRITE OUTPUT
# =============================================================================

write.csv(
  tbl_models,
  file = paste0("Output/Tables/Table S10 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S10 written to Output/Tables/Table S10 ", date_append_output, ".csv")
