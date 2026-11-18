# Table S12.2
#
# Purpose: Calculates country-level mortality predictions for Model 8 and Model 3 on matched 2000–2024 events, with year-block bootstrap intervals.
#
# Inputs:
#   - Step 7: event-level coastal flood XLSX
#   - Step 8: predMod.3 and predMod.8 RDS files
#
# Outputs:
#   - Output/Tables/Table S12.2 country-level Model3 Model8 comparison <date>.csv
#   - Output/Tables/Table S12.2 notes <date>.txt
#
# R packages: glmmTMB, readxl, splines2
# Run from the analysis directory containing Intermediate outputs/.
#

PROJECT_DIR <- getwd()
if (!dir.exists(file.path(PROJECT_DIR, "Intermediate outputs")) &&
    dir.exists(file.path(PROJECT_DIR, "Claude", "Intermediate outputs"))) {
  PROJECT_DIR <- file.path(PROJECT_DIR, "Claude")
}
if (!dir.exists(file.path(PROJECT_DIR, "Intermediate outputs"))) {
  stop("Run this script from the project analysis directory containing Intermediate outputs/.")
}
PROJECT_DIR <- normalizePath(PROJECT_DIR, mustWork = TRUE)
STEP8_DATE  <- "2026-11-13"
STEP7_DATE  <- "2026-07-05"
B <- 1000L
SEED <- 256L
OUT_DIR <- file.path(PROJECT_DIR, "Output", "Tables")
OUTPUT_DATE <- format(Sys.Date(), "%Y-%m-%d")
TABLE_PATH <- file.path(OUT_DIR, paste0("Table S12.2 country-level Model3 Model8 comparison ", OUTPUT_DATE, ".csv"))
NOTES_PATH <- file.path(OUT_DIR, paste0("Table S12.2 notes ", OUTPUT_DATE, ".txt"))

suppressPackageStartupMessages(library(glmmTMB))
stopifnot(requireNamespace("readxl", quietly = TRUE),
          requireNamespace("splines2", quietly = TRUE))

model_dir <- file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 8")
step7_path <- file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 7",
  paste0("EMDAT events with exposed pops, HDI GNI and prev events ", STEP7_DATE, ".xlsx"))
m3_path <- file.path(model_dir, paste0("predMod.3 ", STEP8_DATE, ".RDS"))
m8_path <- file.path(model_dir, paste0("predMod.8 ", STEP8_DATE, ".RDS"))
for (p in c(step7_path, m3_path, m8_path)) {
  if (!file.exists(p)) stop("Missing input: ", p, "\nCheck PROJECT_DIR, STEP7_DATE and STEP8_DATE.")
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

m3 <- readRDS(m3_path)
m8 <- readRDS(m8_path)
raw <- as.data.frame(readxl::read_excel(step7_path))
required <- c("Start.Year", "ISO", "gni.group", "SLIIDERS.country.code.flat",
              "pop.exposed.local.else.adm1.else.national.100yr",
              "total.deaths.na.as.0", "hdi", "Declaration",
              "pop.total.adm0", "Disaster.Subtype")
miss <- setdiff(required, names(raw))
if (length(miss)) stop("Step 7 data missing: ", paste(miss, collapse = ", "))

raw$no.events.5.years <- NA_integer_
for (i in seq_len(nrow(raw))) {
  yr <- raw$Start.Year[i]
  if (!is.na(yr) && yr >= 1990 && yr <= 2024) {
    raw$no.events.5.years[i] <- sum(
      raw$Start.Year %in% ((yr - 5):yr) & raw$ISO == raw$ISO[i])
  }
}
raw <- raw[!is.na(raw$Start.Year) & raw$Start.Year > 1989, , drop = FALSE]
raw <- raw[!is.na(raw$gni.group), , drop = FALSE]
raw$Year <- raw$Start.Year - 1990
raw$Country_Code <- raw$SLIIDERS.country.code.flat
raw$expPop100 <- raw$pop.exposed.local.else.adm1.else.national.100yr

raw$spline_year <- splines2::bSpline(raw$Year, df = 3)
columns <- c("Start.Year", "Country_Code", "total.deaths.na.as.0",
             "expPop100", "hdi", "Declaration", "no.events.5.years",
             "pop.total.adm0", "Disaster.Subtype", "spline_year")
base3 <- raw[, columns, drop = FALSE]
base3$log_expPop100 <- log(base3$expPop100)
base3$Country_Code <- factor(base3$Country_Code)
base3$Declaration <- factor(base3$Declaration)
base3$Disaster.Subtype <- factor(base3$Disaster.Subtype)
base3 <- base3[is.finite(base3$expPop100) & base3$expPop100 > 0 &
                 is.finite(base3$pop.total.adm0) & base3$pop.total.adm0 > 0,
               , drop = FALSE]
base3 <- base3[stats::complete.cases(base3), , drop = FALSE]
base3 <- droplevels(base3)

base8 <- base3[base3$Start.Year >= 2000, , drop = FALSE]
base8$spline_year <- splines::bs(base8$Start.Year - 2000, df = 3)
base8 <- droplevels(base8)

cat("Reconstructed Model 3 events:", nrow(base3), "(saved model:", stats::nobs(m3), ")\n")
cat("Reconstructed Model 8 events:", nrow(base8), "(saved model:", stats::nobs(m8), ")\n")
if (nrow(base3) != stats::nobs(m3) || nrow(base8) != stats::nobs(m8)) {
  stop("Reconstructed fitting-sample size does not match the saved models. ",
       "Do not use the resulting comparison. Verify Step 7/8 dates.")
}
if (!identical(length(unique(base8$Country_Code)), 126L)) {
  warning("Model 8 includes ", length(unique(base8$Country_Code)),
          " countries rather than 126. Verify country-code coverage for Table S12.2.")
}

base3post <- base3[base3$Start.Year >= 2000, , drop = FALSE]
stopifnot(nrow(base3post) == nrow(base8),
          identical(as.character(base3post$Country_Code), as.character(base8$Country_Code)),
          identical(base3post$Start.Year, base8$Start.Year))

p8 <- as.numeric(predict(m8, newdata = base8, type = "response",
                         allow.new.levels = FALSE))
p3 <- as.numeric(predict(m3, newdata = base3post, type = "response",
                         allow.new.levels = FALSE))
if (any(!is.finite(p8)) || any(!is.finite(p3))) stop("Non-finite predictions; inspect model/data.")
stopifnot(all(p8 >= 0), all(p3 >= 0))

rate8 <- p8 / base8$expPop100
rate3 <- p3 / base3post$expPop100
countries <- sort(unique(as.character(base8$Country_Code)))
set.seed(SEED)

country_rows <- vector("list", length(countries))
for (k in seq_along(countries)) {
  ccode <- countries[k]
  ix <- which(as.character(base8$Country_Code) == ccode)
  years <- base8$Start.Year[ix]
  yr_blocks <- split(ix, years)
  uniq_years <- names(yr_blocks)

  r8 <- mean(rate8[ix]); r3 <- mean(rate3[ix])
  d8 <- mean(p8[ix]); d3 <- mean(p3[ix])

  annual8 <- sum(p8[ix]) / 25
  annual3 <- sum(p3[ix]) / 25

  s8 <- s3 <- sd8 <- sd3 <- NA_real_
  if (B >= 2 && length(uniq_years) > 1) {
    boot <- matrix(NA_real_, nrow = B, ncol = 4L)
    for (b in seq_len(B)) {
      sampled_years <- sample(uniq_years, length(uniq_years), replace = TRUE)
      positions <- unlist(yr_blocks[sampled_years], use.names = FALSE)
      boot[b, ] <- c(mean(rate8[positions]), mean(rate3[positions]),
                     mean(p8[positions]), mean(p3[positions]))
    }
    s8  <- stats::sd(boot[, 1]); s3  <- stats::sd(boot[, 2])
    sd8 <- stats::sd(boot[, 3]); sd3 <- stats::sd(boot[, 4])
  }
  lower <- function(mu, se) if (is.na(se)) NA_real_ else max(0, mu - stats::qnorm(.975)*se)
  upper <- function(mu, se) if (is.na(se)) NA_real_ else mu + stats::qnorm(.975)*se

  country_rows[[k]] <- data.frame(
    Country_Code = ccode,
    n_events_2000_2024 = length(ix),
    n_distinct_event_years = length(uniq_years),
    observed_deaths_2000_2024 = sum(base8$total.deaths.na.as.0[ix]),
    mean_exposed_per_event = mean(base8$expPop100[ix]),
    Model8_mean_deaths_per_event = d8,
    Model8_mean_deaths_per_event_lcl = lower(d8, sd8),
    Model8_mean_deaths_per_event_ucl = upper(d8, sd8),
    Model8_mean_mortality_risk = r8,
    Model8_risk_lcl = lower(r8, s8),
    Model8_risk_ucl = upper(r8, s8),
    Model8_deaths_per_1000_exposed = 1000*r8,
    Model8_annual_deaths_2000_2024 = annual8,
    Model3_same_events_mean_deaths_per_event = d3,
    Model3_same_events_mean_mortality_risk = r3,
    Model3_same_events_risk_lcl = lower(r3, s3),
    Model3_same_events_risk_ucl = upper(r3, s3),
    Model3_same_events_deaths_per_1000_exposed = 1000*r3,
    Model3_same_events_annual_deaths_2000_2024 = annual3,
    Model8_minus_Model3_risk = r8 - r3,
    Model8_over_Model3_risk_ratio = if (r3 > 0) r8/r3 else NA_real_,
    Model8_vs_Model3_risk_pct_change = if (r3 > 0) 100*(r8/r3 - 1) else NA_real_,
    stringsAsFactors = FALSE
  )
}
results <- do.call(rbind, country_rows)
results <- results[order(-results$n_events_2000_2024), ]
row.names(results) <- NULL
write.csv(results, TABLE_PATH, row.names = FALSE)

sp <- suppressWarnings(stats::cor(results$Model8_mean_mortality_risk,
                                  results$Model3_same_events_mean_mortality_risk,
                                  method = "spearman", use = "complete.obs"))
sp5 <- with(results[results$n_events_2000_2024 >= 5, ],
             suppressWarnings(stats::cor(Model8_mean_mortality_risk,
                                          Model3_same_events_mean_mortality_risk,
                                          method = "spearman", use = "complete.obs")))
summary_lines <- c(
  "Table S12.2: Country-level mortality risk, Model 8 vs Model 3 on matched 2000-2024 events",
  paste("Step 8 model date:", STEP8_DATE),
  paste("Step 7 data date:", STEP7_DATE),
  "Model 8: 2000-2024 subset; Model 3: 1990-2024 fitted model",
  paste("Model 8 events:", nrow(base8), "; countries:", length(countries)),
  paste("Model 3 events:", nrow(base3), "; countries:", length(unique(base3$Country_Code))),
  paste("Pooled Model 8 mean event-level mortality risk:", mean(rate8)),
  paste("Pooled Model 3 mean risk ON SAME post-2000 events:", mean(rate3)),
  paste("Country-level Spearman correlation of risk:", round(sp, 4)),
  paste("Country-level Spearman correlation for countries with >=5 events:", round(sp5, 4)),
  paste("Countries with >=5 post-2000 events:", sum(results$n_events_2000_2024 >= 5)),
  paste("Countries with >=10 post-2000 events:", sum(results$n_events_2000_2024 >= 10)),
  paste("Bootstrap B:", B, "; resamples observed event YEARS within each country"),
  "One-event-year countries have NA bootstrap intervals (insufficient temporal replication).",
  "Annual deaths = sum of event predicted deaths / 25 years; no uncertainty band computed."
)
writeLines(summary_lines, NOTES_PATH)
cat(paste(summary_lines, collapse = "\n"), "\n")
cat("\nSaved Table S12.2: ", TABLE_PATH, "\n", sep = "")
cat("Saved numerical notes: ", NOTES_PATH, "\n", sep = "")
