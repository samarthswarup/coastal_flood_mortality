# Table S12
#
# Purpose: Summarizes fitted event counts, countries, exposure, and predicted event mortality risk for Models 1–10.
#
# Inputs:
#   - Step 7: event-level coastal flood XLSX
#   - Step 8: predMod.1 through predMod.10 RDS files
#
# Outputs:
#   - Output/Tables/Table S12 <date>.csv
#
# R packages: dplyr, readxl, splines2, purrr, tibble, glmmTMB
# Run from the analysis directory containing Intermediate outputs/.
#

rm(list = ls())

library(dplyr)
library(readxl)
library(splines2)
library(purrr)
library(tibble)
library(glmmTMB)

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

step8_date_append_output <- "2026-11-13"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

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

for (i in 1:10) {
  eval(parse(text = paste0(
    "predMod.", i, " <- readRDS('Intermediate outputs/Output from Step 8/predMod.", i, " ",
    step8_date_append_output, ".RDS')"
  )))
}

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
  mf <- model.frame(m)
  off_col <- grep("^offset\\(", names(mf), value = TRUE)
  stopifnot(length(off_col) == 1)
  exposure <- exp(mf[[off_col]])

  pred_counts <- predict(m, type = "response")

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

write.csv(
  tbl_models,
  file = paste0("Output/Tables/Table S12 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S12 written to Output/Tables/Table S12 ", date_append_output, ".csv")
