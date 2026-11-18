# Figures S6.1–S6.6: Residual diagnostics for candidate mortality models
#
# Script dependencies:
#   - Step 7: EMDAT events with exposed pops, HDI GNI and prev events <date>.xlsx
#   - Step 8: predMod.1 through predMod.10 saved RDS objects
# R packages: DHARMa, dplyr, quantreg, readxl, splines, splines2
# Outputs:
#   - Output/Plots/Figure S6/FigS6_model <number>_<description> <date>.pdf
# Computation:
#   - Reconstructs fitted-model data; fits a Gaussian Lloyd et al. comparison model with a quadratic HDI term; draws DHARMa residual diagnostics for 11 models.
#
# Execute from the project analysis directory containing Intermediate outputs/.

rm(list = ls())

library(DHARMa)
library(quantreg)
library(splines)

project_dir <- getwd()
if (!dir.exists(file.path(project_dir, "Intermediate outputs")) &&
    dir.exists(file.path(project_dir, "Claude", "Intermediate outputs"))) {
  project_dir <- file.path(project_dir, "Claude")
}
if (!dir.exists(file.path(project_dir, "Intermediate outputs"))) {
  stop("Run this script from the directory containing Intermediate outputs/.")
}
setwd(project_dir)

dir.create("Output/Plots/Figure S6", showWarnings = FALSE, recursive = TRUE)

step8_date_append_output <- "2026-11-13"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")


library(dplyr)
library(readxl)
library(splines2)

step7_date_append_output <- "2026-07-05"

# Read Step 7 event data.
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

# Construct model-specific historical datasets.
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

baseKeep$death.rate <- baseKeep$total.deaths.na.as.0 / baseKeep$expPop100
baseKeep$death.rate <- ifelse(baseKeep$death.rate > 1, NA, baseKeep$death.rate)

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


# Load fitted models 1–10.
for (i in 1:10) {
  eval(parse(text = paste0(
    "predMod.", i, " <- readRDS('Intermediate outputs/Output from Step 8/predMod.", i, " ",
    step8_date_append_output, ".RDS')"
  )))
}


# Fit the Gaussian comparison model.
predMod.lloyd <- glm(
  log((death.rate + 1) * 100000) ~ log(pop.total.adm0) + no.events.5.years + hdi + I(hdi^2),
  family = gaussian, data = baseKeep
)
summary(predMod.lloyd)


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

# Calculate DHARMa tests and plot residual panels.
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

  DHARMa::plotQQunif(res, main = "QQ plot residuals",
                     testUniformity = FALSE, testOutliers = FALSE,
                     testDispersion = FALSE)
  box()
  annotate_block(c(
    sprintf("KS uniformity: p = %s (%s)", fmt_p(t_uni$p.value),  siglab(t_uni$p.value)),
    sprintf("Dispersion:    p = %s (%s)", fmt_p(t_disp$p.value), siglab(t_disp$p.value)),
    sprintf("Outliers:      p = %s (%s)", fmt_p(t_out$p.value),  siglab(t_out$p.value))
  ))

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


# Generate and save model diagnostic files.
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
  pdf(paste0("Output/Plots/Figure S6/FigS6_model ", i, "_", fname_desc, " ", date_append_output, ".pdf"),
      width = 8, height = 4)
  draw_dharma_pair(get(model_names[i]), panel_label = model_short_labels[i])
  dev.off()
  message("Model ", i, ": ", model_descriptions[i])
}

message("SI Figure S6: 11 model diagnostic PDFs (Models 1-10 + Lloyd et al. 2016 replication) written to ",
        "Output/Plots/Figure S6/FigS6_model {1-11} ", date_append_output, ".pdf")
