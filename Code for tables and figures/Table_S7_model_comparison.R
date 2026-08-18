###########################################
# Table S7 - Regression model comparison  #
# (4 sub-tables: S7.1-S7.4, all 10        #
# models)                                 #
#                                         #
# 2026-07-16                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding table script, run independently of the numbered pipeline.
# Uses the already-fitted predMod.1-10 objects directly (no refitting),
# guaranteeing consistency with every other number reported elsewhere in
# the paper.
#
# 4 sub-tables: S7.1 = Models 1-3, S7.2 = Models 4-6, S7.3 = Models 7-8,
# S7.4 = Models 9-10.
#
# Draws exclusively on the 10 saved predMod RDS files from Step 8 -- no
# re-derivation of model fits. Also reconstructs baseKeep/baseKeep.tc/
# baseKeep.jonkman/baseKeep.since.2000 (same light column-assembly logic
# used in Figure_S5_model_diagnostics.R), since glmmTMB models only
# store a reference to their fitting data's variable NAME, not the data
# itself -- sjPlot::tab_model() may need to re-resolve that environment
# for some outputs (e.g. certain CI methods), even though AIC/logLik/
# deviance are stored directly on the fitted model object and don't
# strictly require it. Included as cheap insurance.

rm(list = ls())

library(dplyr)
library(readxl)
library(splines2)
library(sjPlot)
library(glmmTMB)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Tables/Table S7", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 8 run to pull predMod.1-10 from
step8_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# SECTION I: RECONSTRUCT baseKeep / baseKeep.since.2000 / baseKeep.tc /
# baseKeep.jonkman (identical to SI_Figure_S5_model_diagnostics.R)
# =============================================================================

step7_date_append_output <- "2026-07-05"

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
# SECTION III: GROUP INTO 4 SUB-TABLES + WRITE
# =============================================================================
# S7.1 = Models 1-3, S7.2 = Models 4-6, S7.3 = Models 7-8, S7.4 = Models 9-10.

mods.1 <- list(`Model 1` = predMod.1, `Model 2` = predMod.2, `Model 3` = predMod.3)
mods.2 <- list(`Model 4` = predMod.4, `Model 5` = predMod.5, `Model 6` = predMod.6)
mods.3 <- list(`Model 7` = predMod.7, `Model 8` = predMod.8)
mods.4 <- list(`Model 9` = predMod.9, `Model 10` = predMod.10)

# Compute a term display order matching the SI table's order (spline
# year terms, then Disaster Subtype levels, then hdi, then
# no.events.5.years, then Declaration, then pop.total.adm0) -- rather than
# sjPlot's default, which follows this script's formula term order
# (pop.total.adm0 first, since that's how predMod.X's formula is written).
# Computed programmatically from a representative model's fixed-effect
# names, since term sets vary across sub-tables (Model 8 has 6 spline
# terms instead of 3; Models 9/10 lack some Disaster Subtype levels).
compute_order_terms <- function(model) {
  term_names <- names(glmmTMB::fixef(model)$cond)[-1]  # exclude intercept
  classify <- function(x) {
    if (grepl("spline_year|bSpline", x)) return(1)
    if (grepl("Disaster\\.Subtype", x)) return(2)
    if (x == "hdi") return(3)
    if (x == "no.events.5.years") return(4)
    if (grepl("Declaration", x)) return(5)
    if (grepl("pop\\.total\\.adm0", x)) return(6)
    return(7)
  }
  groups <- vapply(term_names, classify, numeric(1))
  order(groups)
}

write_submodel_table <- function(mods, out_path) {
  term_order <- compute_order_terms(mods[[1]])
  tab_model(mods,
            show.intercept = FALSE,
            show.r2 = FALSE,
            show.ci = 0.95,
            show.aic = TRUE,
            show.est = TRUE,
            show.p = TRUE,
            show.loglik = TRUE,
            show.dev = TRUE,
            collapse.ci = TRUE,
            digits.rsq = 3,
            emph.p = TRUE,
            order.terms = term_order,
            dv.labels = names(mods),
            string.est = "Risk ratios",
            string.pred = "Covariate variables",
            title = "Regression model results",
            file = out_path)
}

write_submodel_table(mods.1, paste0("Output/Tables/Table S7/Table S7.1 ", date_append_output, ".doc"))
write_submodel_table(mods.2, paste0("Output/Tables/Table S7/Table S7.2 ", date_append_output, ".doc"))
write_submodel_table(mods.3, paste0("Output/Tables/Table S7/Table S7.3 ", date_append_output, ".doc"))
write_submodel_table(mods.4, paste0("Output/Tables/Table S7/Table S7.4 ", date_append_output, ".doc"))

message("Table S7 (4 sub-tables) written to Output/Tables/Table S7/")
