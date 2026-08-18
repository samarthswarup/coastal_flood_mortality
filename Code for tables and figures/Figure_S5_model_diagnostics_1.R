###########################################
# SI Figure S5 - Model diagnostics        #
# (DHARMa residual checks, 11 models      #
# incl. Lloyd et al. 2016 replication)    #
#                                         #
# 2026-07-27                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding figure script, run independently of the numbered pipeline.
# Produces 11 diagnostic PDFs, one per candidate model, each a 1x2 layout:
# left panel is a QQ-uniformity plot with KS/dispersion/outlier test
# annotations, right panel is residuals-vs-predicted (rank) with a
# quantile-deviation test annotation. Models are:
#   1. adm0 exposure (100yr)
#   2. adm1-only exposure (100yr)
#   3. adm1+national exposure (100yr, main model)
#   4. Reported deaths (not NA->0)
#   5. Exposure (10yr)
#   6. Exposure (1000yr)
#   7. Exposure (10000yr)
#   8. Main model, events > 2000
#   9. Tropical cyclone subset only (Disaster.Subtype dropped from the
#      formula since it is constant within an all-tropical-cyclone subset)
#   10. Jonkman et al. (2024) coastal flood definition subset
#       (Disaster.Subtype retained, kept consistent with Models 1/2/3
#       since it still varies within this subset)
#   11. Lloyd et al. (2016) replication -- plain glm(), gaussian family,
#       log((death.rate + 1) * 100000) ~ log(pop) + prior events + HDI +
#       I(hdi^2). Written as I(hdi^2), not the bare hdi^2, since R formula
#       syntax collapses "y ~ x + x^2" back down to just "y ~ x" -- "^" on
#       a single variable expands interaction depth on sums like (a+b)^2,
#       it does not perform literal exponentiation. I(hdi^2) is required
#       to actually fit HDI-squared, matching Lloyd et al.'s own
#       Kuznets-curve specification.
#
# Models 1-10 are loaded as already-fitted model objects (predMod.1
# through predMod.10), each saved as its own RDS file during Step 8's run.
# Model 11 (the Lloyd et al. replication) is fit fresh here on baseKeep
# (Section II.5, below) rather than in Step 8, since it is a genuinely
# different model class (gaussian glm) from Models 1-10 (glmmTMB negative
# binomial) and exists only for this diagnostic comparison.
#
# glmmTMB model objects do not fully embed their fitting data -- they
# store a reference to the data frame NAME used at fit time ("baseKeep"),
# and DHARMa::simulateResiduals() needs to re-evaluate the model formula
# in an environment where that name resolves. So this script reconstructs
# baseKeep/baseKeep.since.2000 from Step 7's final output, following Step
# 8's own QUICK LOAD section -- light column assembly (renaming, log
# transforms, a spline basis), not model refitting or new calculation.
# death.rate is not a raw exported column; it is computed the same way
# Step 8 constructs it elsewhere (total.deaths.na.as.0 / expPop100, with
# an implausibility cap: values > 1 set to NA).
#
# Recent DHARMa versions auto-draw their own test-annotation text directly
# on both plotQQunif and plotResiduals, which would otherwise collide with
# this script's own custom annotate_block() text. testUniformity/
# testOutliers/testDispersion are passed as FALSE to plotQQunif to
# suppress its built-in left-panel annotations, and the right panel
# (residual vs. predicted) is built manually -- raw scatter plus
# quantreg::rq() quantile lines at tau = 0.25/0.5/0.75 -- rather than via
# plotResiduals(quantreg = TRUE), whose auto-printed title text is not
# reliably suppressible across DHARMa versions.
#
# DHARMa::simulateResiduals() uses random simulation (n = resim draws per
# model); no seed is set, so re-running will give slightly different
# simulated p-values each time, though conclusions should be stable across
# the significance annotations for well-specified models.

rm(list = ls())

library(DHARMa)
library(quantreg)
library(splines)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Plots/Figure S5", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 8 run to pull predMod.1-10 from
step8_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

# =============================================================================
# SECTION I: RECONSTRUCT baseKeep / baseKeep.since.2000
# =============================================================================
# glmmTMB model objects don't fully embed their fitting data -- they store
# a reference to the data frame NAME used at fit time ("baseKeep"), and
# DHARMa::simulateResiduals() needs to re-evaluate the model formula in an
# environment where that name resolves. So even though predMod.1-10 are
# loaded from saved RDS (not refit), baseKeep/baseKeep.since.2000 still
# need to exist -- this follows Step 8's own QUICK LOAD section. This is
# light column assembly from Step 7's already-final output, not model
# refitting or new calculation.

library(dplyr)
library(readxl)
library(splines2)

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

baseKeep.since.2000 <- baseKeep[baseKeep$Start.Year >= 2000, ]

# death.rate is NOT a raw exported column -- computed identically to how
# Step 8 constructs it in several places, including the same validity cap
# (>1 implausible, set to NA rather than kept).
baseKeep$death.rate <- baseKeep$total.deaths.na.as.0 / baseKeep$expPop100
baseKeep$death.rate <- ifelse(baseKeep$death.rate > 1, NA, baseKeep$death.rate)

# Model 9 subset: tropical cyclone-only
baseSet.tc <- baseSet[baseSet$Disaster.Subtype == "Tropical cyclone", ]
baseSet.tc$spline_year <- splines2::bSpline(baseSet.tc$Year, df = 3)
baseKeep.tc <- data.frame(baseSet.tc[, names(baseSet.tc) %in% baseKeepVars])
baseKeep.tc <- subset(baseKeep.tc, !is.na(hdi) & !is.na(expPop100) & expPop100 > 0)
names(baseKeep.tc)[names(baseKeep.tc) == "SLIIDERS.country.code.flat"] <- "Country_Code"
baseKeep.tc$log_expPop100 <- log(baseKeep.tc$expPop100)

# Model 10 subset: Jonkman et al. (2024) coastal flood definition
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
# SECTION II: LOAD THE 10 SAVED MODEL OBJECTS (MODELS 1-10)
# =============================================================================

for (i in 1:10) {
  eval(parse(text = paste0(
    "predMod.", i, " <- readRDS('Intermediate outputs/Output from Step 8/predMod.", i, " ",
    step8_date_append_output, ".RDS')"
  )))
}

# =============================================================================
# SECTION II.5: FIT LLOYD ET AL. 2016 REPLICATION (MODEL 11)
# =============================================================================
# Plain glm(), gaussian family -- a genuinely different model class from
# Models 1-10 (all glmmTMB negative binomial). Fit fresh here on baseKeep
# (same event sample as Models 1-10, for a fair diagnostic comparison).
#
# "I(hdi^2)" NOT "hdi^2" -- see header note.

predMod.lloyd <- glm(
  log((death.rate + 1) * 100000) ~ log(pop.total.adm0) + no.events.5.years + hdi + I(hdi^2),
  family = gaussian, data = baseKeep
)
summary(predMod.lloyd)

# =============================================================================
# SECTION III: DIAGNOSTIC HELPERS
# =============================================================================

fmt_p  <- function(p) ifelse(is.na(p), "NA", ifelse(p < 1e-16, "<1e-16", sprintf("%.3g", p)))
siglab <- function(p, a = 0.05) ifelse(is.na(p), "n/a", ifelse(p < a, "significant", "n.s."))

annotate_block <- function(lines, cex = 0.8, col = "firebrick3", xpad = 0.02, ypad = 0.04) {
  usr <- par("usr"); xr <- diff(usr[1:2]); yr <- diff(usr[3:4])
  x0 <- usr[1] + xpad * xr; y0 <- usr[4] - ypad * yr
  lh <- strheight("A", cex = cex) * 1.25
  rect(x0 - 0.01 * xr, y0 + 0.35 * lh,
       x0 + max(strwidth(lines, cex = cex)) + 0.01 * xr,
       y0 - (length(lines) - 0.6) * lh,
       col = rgb(1, 1, 1, 0.75), border = NA)
  for (i in seq_along(lines)) text(x0, y0 - (i - 1) * lh, lines[i], adj = c(0, 1), cex = cex, col = col)
}

draw_dharma_pair <- function(model, panel_label = "A.", resim = 1000) {
  stopifnot(inherits(model, "glmmTMB") || inherits(model, "merMod") || inherits(model, "lm"))
  res <- DHARMa::simulateResiduals(model, n = resim)

  t_uni  <- DHARMa::testUniformity(res, plot = FALSE)
  t_disp <- DHARMa::testDispersion(res, plot = FALSE)
  t_out  <- DHARMa::testOutliers(res, plot = FALSE)
  t_quan <- DHARMa::testQuantiles(res, plot = FALSE)

  opar <- par(mfrow = c(1, 2), mar = c(4.2, 4.6, 2.6, 1.0), mgp = c(2.2, 0.7, 0),
              oma = c(0, 0, 2.5, 0))
  on.exit(par(opar), add = TRUE)

  ## LEFT: QQ-uniformity. Recent DHARMa versions auto-draw their own
  ## KS/dispersion/outlier test text directly on the plot by default,
  ## which duplicates and overlaps with our own clean annotate_block below
  ## -- suppress DHARMa's built-in annotations explicitly.
  DHARMa::plotQQunif(res, main = "QQ plot residuals",
                     testUniformity = FALSE, testOutliers = FALSE,
                     testDispersion = FALSE)
  box()
  annotate_block(c(
    sprintf("KS uniformity: p = %s (%s)", fmt_p(t_uni$p.value),  siglab(t_uni$p.value)),
    sprintf("Dispersion:    p = %s (%s)", fmt_p(t_disp$p.value), siglab(t_disp$p.value)),
    sprintf("Outliers:      p = %s (%s)", fmt_p(t_out$p.value),  siglab(t_out$p.value))
  ))

  ## RIGHT: residuals vs predicted (rank). DHARMa::plotResiduals(quantreg=TRUE)
  ## auto-prints its own "Quantile deviations detected" title text that
  ## isn't reliably suppressible across DHARMa versions -- build this panel
  ## manually instead (raw scatter + our own quantile regression lines) for
  ## full control over labels.
  rank_pred  <- rank(res$fittedPredictedResponse, ties.method = "average")
  rank_pred  <- rank_pred / max(rank_pred)
  resid_vals <- res$scaledResiduals

  plot(rank_pred, resid_vals, pch = 2, cex = 0.55, col = "#1496E1",
       xlab = "Model predictions (rank transformed)", ylab = "DHARMa residual",
       main = "Residual vs. predicted", ylim = c(0, 1))
  abline(h = c(0.25, 0.5, 0.75), lty = 2, col = "grey60")

  ord <- order(rank_pred)
  for (q in c(0.25, 0.5, 0.75)) {
    fit_q <- quantreg::rq(resid_vals ~ splines::ns(rank_pred, df = 4), tau = q)
    lines(rank_pred[ord], fitted(fit_q)[ord], col = "red", lwd = 2)
  }
  box()
  annotate_block(c(
    sprintf("Quantile deviation: p = %s (%s)", fmt_p(t_quan$p.value), siglab(t_quan$p.value))
  ))

  mtext(panel_label, outer = TRUE, side = 3, adj = 0, line = 0.5,
        font = 2, cex = 1.2)
}

# =============================================================================
# SECTION IV: GENERATE 11 DIAGNOSTIC PDFs (ONE PER MODEL)
# =============================================================================

model_names <- c(paste0("predMod.", 1:10), "predMod.lloyd")
model_short_labels <- c(paste0("Model ", 1:10), "Lloyd et al. 2016 replication")
model_descriptions <- c(
  "1. adm0 exposure (100yr)",
  "2. adm1-only exposure (100yr)",
  "3. adm1+national exposure (100yr, main)",
  "4. Reported deaths (not NA->0)",
  "5. Exposure (10yr)",
  "6. Exposure (1000yr)",
  "7. Exposure (10000yr)",
  "8. Main model, events > 2000",
  "9. Tropical cyclone subset only",
  "10. Jonkman et al. (2024) coastal flood definition subset",
  "11. Lloyd et al. (2016) replication (gaussian GLM, log death rate ~ log pop + prior events + HDI + HDI^2)"
)

for (i in seq_along(model_names)) {
  fname_desc <- gsub("^[0-9]+\\.\\s*", "", model_descriptions[i])
  fname_desc <- gsub("[^A-Za-z0-9]+", "_", fname_desc)
  fname_desc <- gsub("_+$", "", fname_desc)
  pdf(paste0("Output/Plots/Figure S5/FigS5_model ", i, "_", fname_desc, " ", date_append_output, ".pdf"),
      width = 8, height = 4)
  draw_dharma_pair(get(model_names[i]), panel_label = model_short_labels[i])
  dev.off()
  message("Model ", i, ": ", model_descriptions[i])
}

message("SI Figure S5: 11 model diagnostic PDFs (Models 1-10 + Lloyd et al. 2016 replication) written to ",
        "Output/Plots/Figure S5/FigS5_model {1-11} ", date_append_output, ".pdf")
