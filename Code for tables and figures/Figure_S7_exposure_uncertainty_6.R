###########################################
# SI Figure S7 - Uncertainty in projected #
# fatalities across climate scenarios     #
# (2050 & 2100)                           #
#                                         #
# 2026-07-24                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding figure script, run independently of the numbered pipeline.
# Compares two sources of uncertainty in projected fatalities, for 7 core
# scenarios (5 SSP-consistent scenarios + BOUND_worst/BOUND_best), in 2050
# and 2100:
#   - SLIIDERS exposure-percentile spread (17th-83rd percentile of exposed
#     population for a given scenario/year, propagated through the fixed
#     mortality model)
#   - Sampling/model uncertainty (bootstrap resampling of events, holding
#     exposure fixed at the median)
# Both are expressed as percent deviation from each scenario's own central
# (median-exposure) predicted death total, and plotted as paired error
# bars per scenario (Sections V-VI). Section VII additionally converts
# both sources into absolute deaths/year and compares them side by side
# with a third source -- the model/bootstrap uncertainty already reported
# in Step 9's decomp_table (Ann_deaths_lcl/ucl) -- since that is the unit
# used for every other forecast range reported elsewhere in the paper.
#
# The 7-scenario set matches all_scenarios_fig3 (the same 5 core
# SSP-consistent scenarios plus both bounding scenarios used throughout
# Figure 3, Table S11, and the rest of the paper).
#
# Draws on Step 8's saved predMod.3/predData RDS outputs (no re-derivation
# of the mortality model or its fitting data) and Step 9's decomp_table
# (for Section VII only). Reconstructs baseKeep, since glmmTMB's
# model.frame()/model.matrix() need to resolve predMod.3's fitting-data
# environment by name. The BOUND_worst/BOUND_best column-naming quirk
# (different forcing label + ".popscale.BOUND_worst/best" suffix) is
# handled via the bound_lookup table below.

rm(list = ls())

library(dplyr)
library(splines2)
library(RColorBrewer)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Plots", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 8 run to pull predMod.3/predData from
step8_date_append_output <- "2026-07-20"

# Date suffix identifying which Step 9 run to pull decomp_table from
# (used only by Section VII's model-uncertainty comparison)
step9_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# SECTION I: RECONSTRUCT baseKeep (needed for model.frame()/model.matrix()
# to resolve predMod.3's fitting-data environment)
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
                  "SLIIDERS.country.code.flat",
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

# =============================================================================
# SECTION II: LOAD predMod.3, predData, AND decomp_table
# =============================================================================

predMod.3 <- readRDS(paste0("Intermediate outputs/Output from Step 8/predMod.3 ",
                             step8_date_append_output, ".RDS"))

predData <- readRDS(paste0("Intermediate outputs/Output from Step 8/predData ",
                            step8_date_append_output, ".RDS"))

predMod <- predMod.3

decomp_path <- paste0(
  "Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_",
  step9_date_append_output, ".csv"
)
if (!file.exists(decomp_path)) {
  available <- list.files("Intermediate outputs/Output from Step 9/",
                           pattern = "^decomposition_table_population_methodology_")
  stop(
    "File not found: ", decomp_path, "\n",
    "decomposition_table files actually present in that folder:\n  ",
    paste(available, collapse = "\n  "),
    "\n-> Check step9_date_append_output above against the dates listed."
  )
}
decomp_table <- read.csv(decomp_path, stringsAsFactors = FALSE)

# =============================================================================
# SECTION III: ENSURE spline_year EXISTS
# =============================================================================

if (!"spline_year" %in% names(predData)) {
  predData$Year <- as.numeric(substr(predData$DisNo., 1, 4)) - 1990
  predData$spline_year <- bSpline(predData$Year, df = 3)
}

base <- predData %>%
  filter(!is.na(hdi)) %>%
  mutate(
    declare       = factor(Declaration),
    Country_Code  = factor(SLIIDERS.country.code.flat),
    DisNo_nocoun  = factor(gsub("[A-Za-z]", "", DisNo.))
  )

mf <- model.frame(predMod)

if ("pop.total.adm0" %in% names(mf)) {
  if (!"pop.total.adm0" %in% names(base) && "pop_total_adm0" %in% names(base)) base[["pop.total.adm0"]] <- base[["pop_total_adm0"]]
}
n_events_base <- nrow(base)
N_YEARS <- 35

# =============================================================================
# SECTION IV: EXPOSURE COLUMN LOOKUP + PREDICTION HELPERS
# =============================================================================

# BOUND_worst/BOUND_best use a DIFFERENT column pattern than the 5 "med_"
# scenarios: an extra ".popscale.BOUND_worst"/".popscale.BOUND_best" suffix,
# and their underlying SLR label doesn't match the scenario name directly
# (BOUND_worst -> "low_SSP5.85", BOUND_best -> "med_SSP1.19" -- this is the
# low-confidence BOUND_worst swap already made elsewhere in the pipeline).
bound_lookup <- list(
  BOUND_worst = list(slr = "low_SSP5.85", suffix = ".popscale.BOUND_worst"),
  BOUND_best  = list(slr = "med_SSP1.19", suffix = ".popscale.BOUND_best")
)

exp_col <- function(df, setting, yr, perc) {
  if (setting %in% names(bound_lookup)) {
    slr    <- bound_lookup[[setting]]$slr
    suffix <- bound_lookup[[setting]]$suffix
    nm <- paste0("AR6_", slr, ".", yr, ".", perc,
                 ".pop.exposed.local.else.adm1.else.national.100yr", suffix)
  } else {
    nm <- paste0("AR6_", setting, ".", yr, ".", perc,
                 ".pop.exposed.local.else.adm1.else.national.100yr")
  }
  if (nm %in% names(df)) nm else character(0)
}

pred_mean_deaths <- function(exposure_vec) {
  nd <- base
  exposure_vec[!is.finite(exposure_vec) | exposure_vec <= 0] <- NA_real_
  nd$log_expPop100 <- log(exposure_vec)
  mu <- predict(predMod, newdata = nd, type = "response", allow.new.levels = TRUE)
  mean(mu, na.rm = TRUE)
}

# Same per-event mean, converted to a total annual death count (same
# conversion used throughout the pipeline: mean(predicted)*n_events/N_YEARS)
pred_total_deaths <- function(exposure_vec) {
  pred_mean_deaths(exposure_vec) * n_events_base / N_YEARS
}

boot_mean <- function(mu, B = 100) {
  n <- length(mu)
  if (n == 0) return(rep(NA_real_, B))
  idx <- matrix(sample.int(n, n * B, replace = TRUE), nrow = B)
  rowMeans(matrix(mu[idx], nrow = B), na.rm = TRUE)
}

# =============================================================================
# SECTION V: COMPUTE EXPOSURE-DRIVEN SPREAD + SAMPLING/MODEL UNCERTAINTY
# (percent-of-median-exposure framing, used for the plot)
# =============================================================================

# 7 scenarios: matches all_scenarios_fig3 (5 core SSP-consistent scenarios
# + both bounds) used everywhere else in the paper. low_SSP1.26/
# low_SSP5.85 are not included as standalone scenarios here since
# BOUND_worst already uses low_SSP5.85's own low-confidence SLR forcing.
settings_use <- c("med_SSP1.19",
                  "med_SSP1.26",
                  "med_SSP2.45",
                  "med_SSP3.70",
                  "med_SSP5.85",
                  "BOUND_worst",
                  "BOUND_best")
YEARS <- c(2050, 2100)

# (A) Exposure-driven spread (hold model fixed; vary exposure percentiles)
expo_res <- do.call(rbind, lapply(YEARS, function(yy) {
  do.call(rbind, lapply(settings_use, function(st) {
    c17 <- exp_col(base, st, yy, "17"); if (!length(c17)) return(NULL)
    c50 <- exp_col(base, st, yy, "50"); if (!length(c50)) return(NULL)
    c83 <- exp_col(base, st, yy, "83"); if (!length(c83)) return(NULL)
    v17 <- as.numeric(base[[c17]]); v50 <- as.numeric(base[[c50]]); v83 <- as.numeric(base[[c83]])
    m17 <- pred_mean_deaths(v17); m50 <- pred_mean_deaths(v50); m83 <- pred_mean_deaths(v83)
    data.frame(year = yy, setting = st, p17 = m17, p50 = m50, p83 = m83)
  }))
}))
if (is.null(expo_res) || !nrow(expo_res)) {
  build_names_for <- function(st) {
    yrs <- c(2050, 2100); percs <- c("17", "50", "83")
    combos <- expand.grid(yr = yrs, perc = percs)
    vapply(seq_len(nrow(combos)), function(i) {
      if (st %in% names(bound_lookup)) {
        paste0("AR6_", bound_lookup[[st]]$slr, ".", combos$yr[i], ".", combos$perc[i],
               ".pop.exposed.local.else.adm1.else.national.100yr", bound_lookup[[st]]$suffix)
      } else {
        paste0("AR6_", st, ".", combos$yr[i], ".", combos$perc[i],
               ".pop.exposed.local.else.adm1.else.national.100yr")
      }
    }, character(1))
  }
  need_names <- unlist(lapply(settings_use, build_names_for))
  missing_expo <- setdiff(need_names, names(base))
  stop("No matching AR6 exposure columns found in predData.\nMissing examples:\n",
       paste(missing_expo[1:min(8, length(missing_expo))], collapse = "\n"))
}

# (B) Sampling/model (conditional) -- hold exposure at .50; bootstrap events
# Matches B_GLOBAL (300) used for the main analysis's bootstrap CIs in
# 9_HDI_pop_time_varying_projections.R -- this is a separate, independent bootstrap (different
# code path, fresh random draws), not a reuse of that one, but there's no
# principled reason for this figure to use a less precise replicate count
# than every other reported uncertainty in the paper.
BOOT <- 300
boot_res <- do.call(rbind, lapply(YEARS, function(yy) {
  do.call(rbind, lapply(settings_use, function(st) {
    c50 <- exp_col(base, st, yy, "50"); if (!length(c50)) return(NULL)
    e50 <- as.numeric(base[[c50]])
    if (any(!is.finite(e50)) || any(e50 <= 0)) return(NULL)
    nd <- base; nd$log_expPop100 <- log(e50)
    mu0 <- predict(predMod, newdata = nd, type = "response", allow.new.levels = TRUE)
    mb <- boot_mean(mu0, B = BOOT)
    data.frame(year = yy, setting = st,
               samp_mean = mean(mb), samp_sd = sd(mb),
               samp_lcl = unname(quantile(mb, 0.05)), samp_ucl = unname(quantile(mb, 0.95)))
  }))
}))
if (is.null(boot_res) || !nrow(boot_res)) stop("Sampling/model bootstrap produced no results (check exposure .50 columns).")

# =============================================================================
# SECTION VI: MERGE + PLOT (percent-of-median-exposure framing)
# =============================================================================

unc <- dplyr::inner_join(expo_res, boot_res, by = c("year", "setting")) %>%
  mutate(
    expo_center   = p50,
    pct_expo_low  = 100 * (p17 - expo_center) / expo_center,
    pct_expo_high = 100 * (p83 - expo_center) / expo_center,
    pct_samp_lcl  = 100 * (samp_lcl - samp_mean) / expo_center,
    pct_samp_ucl  = 100 * (samp_ucl - samp_mean) / expo_center
  )

# Display labels only (internal `setting` values unchanged, still used for
# column lookups etc. throughout). Strips "med_" (confusable with
# "median" -- these are all medium-confidence SLR, not a median/central
# statistic) and relabels the bounding scenarios in plain language.
display_label <- c(
  "med_SSP1.19" = "SSP1.19", "med_SSP1.26" = "SSP1.26",
  "med_SSP2.45" = "SSP2.45", "med_SSP3.70" = "SSP3.70",
  "med_SSP5.85" = "SSP5.85",
  "BOUND_worst" = "Worst case", "BOUND_best" = "Best case"
)
unc$setting_label <- display_label[unc$setting]

pdf(paste0("Output/Plots/Fig S7 ", date_append_output, ".pdf"), width = 10, height = 6)

op <- par(mfrow = c(1, 2), mar = c(7, 5, 3, 1))
for (yy in YEARS) {
  cur <- unc[unc$year == yy, ]
  x <- seq_len(nrow(cur))
  ylim_rng <- range(c(cur$pct_expo_low, cur$pct_expo_high, cur$pct_samp_lcl, cur$pct_samp_ucl), na.rm = TRUE)
  # Note: what varies is predicted/expected fatalities (from the model),
  # not exposure itself -- exposure is a fixed, deterministic SLIIDERS
  # value for a given percentile, and for the red bars specifically it's
  # held fixed at the median throughout (only which historical events get
  # bootstrap-resampled varies). Label reflects that: uncertainty is in
  # the predicted death total, expressed relative to the central
  # (median-exposure) estimate -- not "around exposure."
  plot(x, rep(0, length(x)), xaxt = "n", xlab = "", ylab = "Uncertainty in predicted deaths (% around central estimate)",
       ylim = ylim_rng * 1.5, main = paste0(yy),
       las = 2)
  axis(1, at = x, labels = cur$setting_label, las = 2, cex.axis = 0.85)
  segments(x, cur$pct_expo_low, x, cur$pct_expo_high, lwd = 6,
           col = paste(brewer.pal(3, "Blues")[3], 90, sep = ""))  # exposure (outer)
  segments(x, cur$pct_samp_lcl, x, cur$pct_samp_ucl, lwd = 2,
           paste(brewer.pal(3, "Reds")[3], 90, sep = ""))          # sampling/model (inner)
  abline(h = 0, lty = 3)
  if (yy == 2100) {
    legend("topleft", inset = 0, bty = "n",
           legend = c("SLIIDERS SLR uncertainty, 17th-83rd percentile", "Mortality model uncertainty in expected fatalities (5-95% CI)"),
           lwd = c(6, 2), col = c(paste(brewer.pal(3, "Blues")[3], 90, sep = ""),
                                  paste(brewer.pal(3, "Reds")[3], 90, sep = "")),
           cex = 0.5, angle = 45, horiz = FALSE)
  }
}
par(op)

dev.off()

unc_summary <- unc %>% dplyr::select(year, setting, pct_expo_low, pct_expo_high, pct_samp_lcl, pct_samp_ucl)
print(unc_summary)

write.csv(
  unc_summary,
  file = paste0("Output/Plots/Figure S7 values ", date_append_output, ".csv"),
  row.names = FALSE
)

# =============================================================================
# SECTION VII: ABSOLUTE-DEATH-COUNT THREE-WAY UNCERTAINTY COMPARISON
# =============================================================================
# Same 7 scenarios/2 years, converted to deaths/year so this is directly
# comparable to every other forecast range reported in the paper, rather
# than a percent-of-rate relative to each scenario's own median.

s7_to_decomp_label <- c(
  "med_SSP1.19" = "SSP1.19", "med_SSP1.26" = "SSP1.26_med",
  "med_SSP2.45" = "SSP2.45", "med_SSP3.70" = "SSP3.70",
  "med_SSP5.85" = "SSP5.85_med",
  "BOUND_worst" = "BOUND_worst", "BOUND_best" = "BOUND_best"
)

# Stage matching Figure 3/Key Point 3's own definition: SLR + national pop
# trends only (no HDI, no pop-as-covariate, no coastal-growth multiplier)
stage_pop_trends_only <- " + Population (exposure only)"

sliiders_abs <- do.call(rbind, lapply(YEARS, function(yy) {
  do.call(rbind, lapply(settings_use, function(st) {
    c17 <- exp_col(base, st, yy, "17"); if (!length(c17)) return(NULL)
    c50 <- exp_col(base, st, yy, "50"); if (!length(c50)) return(NULL)
    c83 <- exp_col(base, st, yy, "83"); if (!length(c83)) return(NULL)
    data.frame(
      year = yy, setting = st,
      total_p17 = pred_total_deaths(as.numeric(base[[c17]])),
      total_p50 = pred_total_deaths(as.numeric(base[[c50]])),
      total_p83 = pred_total_deaths(as.numeric(base[[c83]]))
    )
  }))
}))

# (1) Inter-scenario spread: range of each scenario's own p50 total
interscenario <- sliiders_abs %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(scenario_min = min(total_p50), scenario_max = max(total_p50), .groups = "drop")

# (2) SLIIDERS exposure-percentile spread: p17-p83 WITHIN each scenario
sliiders_spread <- sliiders_abs %>%
  dplyr::mutate(sliiders_width = total_p83 - total_p17) %>%
  dplyr::select(year, setting, total_p17, total_p50, total_p83, sliiders_width)

# (3) Model/bootstrap uncertainty: Ann_deaths_lcl/ucl from Step 9's
# decomp_table, same stage and scenarios, for direct comparability
model_ci <- decomp_table %>%
  dplyr::filter(Scenario == stage_pop_trends_only,
                SSP %in% s7_to_decomp_label[settings_use],
                Year %in% c("2050", "2100")) %>%
  dplyr::mutate(model_ci_width = Ann_deaths_ucl - Ann_deaths_lcl,
                setting = names(s7_to_decomp_label)[match(SSP, s7_to_decomp_label)]) %>%
  dplyr::select(Year, SSP, setting, Ann_deaths, Ann_deaths_lcl, Ann_deaths_ucl, model_ci_width)

cat("\n=== (1) Inter-scenario spread (p50 across scenarios), deaths/year ===\n")
print(interscenario)
cat("\n=== (2) SLIIDERS exposure-percentile spread (within-scenario), deaths/year ===\n")
print(sliiders_spread)
cat("\n=== (3) Model bootstrap CI width (within-scenario), deaths/year ===\n")
print(model_ci)

three_way <- sliiders_spread %>%
  dplyr::mutate(Year = as.character(year)) %>%
  dplyr::left_join(model_ci %>% dplyr::mutate(Year = as.character(Year)) %>%
                      dplyr::select(Year, setting, model_ci_width),
                    by = c("Year", "setting")) %>%
  dplyr::select(year, setting, total_p50, sliiders_width, model_ci_width)

write.csv(
  three_way,
  file = paste0("Output/Plots/Figure S7 three-way uncertainty comparison ", date_append_output, ".csv"),
  row.names = FALSE
)

message("SI Figure S7 written to Output/Plots/Fig S7 ", date_append_output, ".pdf")
message("Three-way uncertainty comparison written to Output/Plots/Figure S7 three-way uncertainty comparison ", date_append_output, ".csv")
