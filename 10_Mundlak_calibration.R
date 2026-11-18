
# Step 10 — Matched historical-event calibration of Model 3 and Mundlak
#
# Reconstructs the 1990–2024 fitted event sample, predicts historical deaths
# using the Step 8 primary negative-binomial model and the Step 8a Mundlak
# model, and tabulates observed-versus-fitted mortality at event and country
# levels. Reports absolute errors, RMSE, WAPE, correlations, and fit statistics.
# No models are refitted and no future projections are calculated.
#
# Requires: Step 7 event data and saved Step 8/8a fitted model objects.
# Outputs: Output/Diagnostics_RF_Mundlak/Model_comparison_calibration/.
# -----------------------------------------------------------------------------

PROJECT_DIR <- getwd()
# Use the same project directory as Steps 8 and 9.
if (!dir.exists(file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 8")) &&
    dir.exists(file.path(PROJECT_DIR, "Claude", "Intermediate outputs", "Output from Step 8"))) {
  PROJECT_DIR <- file.path(PROJECT_DIR, "Claude")
}
PROJECT_DIR <- normalizePath(PROJECT_DIR, mustWork = TRUE)

STEP7_DATE <- "2026-07-05"
STEP8_DATE <- "2026-11-13"
N_YEARS <- 35

needed <- c("glmmTMB", "readxl", "splines2")
missing <- needed[
  !vapply(needed, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Install packages: ", paste(missing, collapse = ", "))
}

setwd(PROJECT_DIR)

out_dir <- paste0(
  "Output/Diagnostics_RF_Mundlak/",
  "Model_comparison_calibration"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

nb_path <- paste0(
  "Intermediate outputs/Output from Step 8/predMod.3 ",
  STEP8_DATE, ".RDS"
)
cre_path <- paste0(
  "Output/Diagnostics_RF_Mundlak/",
  "Mundlak_NB_augmented_model.rds"
)
map_path <- paste0(
  "Output/Diagnostics_RF_Mundlak/",
  "Mundlak_mean_term_mapping.csv"
)
step7_path <- paste0(
  "Intermediate outputs/Output from Step 7/",
  "EMDAT events with exposed pops, HDI GNI and prev events ",
  STEP7_DATE, ".xlsx"
)

for (p in c(nb_path, cre_path, map_path, step7_path)) {
  if (!file.exists(p)) stop("Missing input: ", p)
}

nb <- readRDS(nb_path)
cre <- readRDS(cre_path)
map <- read.csv(map_path, stringsAsFactors = FALSE)

if (nobs(nb) != 1985 || nobs(cre) != 1985) {
  stop("Unexpected fitted-model sample size.")
}

# Reconstruct Step 8 regression dataset
raw <- as.data.frame(readxl::read_excel(step7_path))

raw$no.events.5.years <- NA_integer_

for (i in seq_len(nrow(raw))) {
  yr <- raw$Start.Year[i]
  
  if (!is.na(yr) && yr >= 1990 && yr <= 2024) {
    raw$no.events.5.years[i] <- sum(
      (raw$Start.Year %in% ((yr - 5):yr)) &
        (raw$ISO == raw$ISO[i])
    )
  }
}

raw <- raw[
  !is.na(raw$Start.Year) & raw$Start.Year > 1989,
  , drop = FALSE
]
raw <- raw[!is.na(raw$gni.group), , drop = FALSE]

raw$Year <- raw$Start.Year - 1990
raw$Country_Code <- raw$SLIIDERS.country.code.flat
raw$expPop100 <-
  raw$pop.exposed.local.else.adm1.else.national.100yr
raw$spline_year <- splines2::bSpline(raw$Year, df = 3)

req <- c(
  "total.deaths.na.as.0", "hdi", "Declaration",
  "no.events.5.years", "pop.total.adm0",
  "Disaster.Subtype", "Country_Code",
  "expPop100", "spline_year"
)

if (length(setdiff(req, names(raw)))) {
  stop("Missing model columns: ",
       paste(setdiff(req, names(raw)), collapse = ", "))
}

baseKeep <- raw[, req, drop = FALSE]
baseKeep$log_expPop100 <- log(baseKeep$expPop100)
baseKeep$Country_Code <- factor(baseKeep$Country_Code)
baseKeep$Declaration <- factor(baseKeep$Declaration)
baseKeep$Disaster.Subtype <- factor(baseKeep$Disaster.Subtype)

baseKeep <- baseKeep[
  !is.na(baseKeep$expPop100) &
    baseKeep$expPop100 > 0 &
    !is.na(baseKeep$pop.total.adm0) &
    baseKeep$pop.total.adm0 > 0,
  , drop = FALSE
]
baseKeep <- baseKeep[
  complete.cases(baseKeep), , drop = FALSE
]
baseKeep <- droplevels(baseKeep)
rownames(baseKeep) <- NULL

if (nrow(baseKeep) != 1985 ||
    length(unique(baseKeep$Country_Code)) != 130) {
  stop("Reconstructed sample does not match saved models.")
}

# Reconstruct Mundlak country means
M <- model.matrix(
  ~ log(pop.total.adm0) + spline_year + hdi +
    Declaration + no.events.5.years + Disaster.Subtype,
  data = baseKeep
)

M <- M[, colnames(M) != "(Intercept)", drop = FALSE]

cc <- as.character(baseKeep$Country_Code)
cn <- table(cc)

by_cc <- rowsum(M, group = cc, reorder = TRUE)
by_cc <- sweep(
  by_cc, 1L, as.numeric(cn[rownames(by_cc)]), "/"
)

means <- by_cc[cc, , drop = FALSE]

varying <- apply(means, 2L, function(z) {
  is.finite(sd(z)) && sd(z) > 1e-10
})
means <- means[, varying, drop = FALSE]

if (nrow(map) != ncol(means) ||
    !identical(
      as.character(map$original_covariate),
      colnames(means)
    )) {
  stop("Mundlak country-mean mapping does not match.")
}

colnames(means) <- as.character(map$term)

for (k in seq_len(ncol(means))) {
  baseKeep[[colnames(means)[k]]] <- means[, k]
}

# Event-level predictions: same historical observations
y <- as.numeric(baseKeep$total.deaths.na.as.0)
E <- as.numeric(baseKeep$expPop100)

mu_nb <- as.numeric(
  predict(nb, newdata = baseKeep, type = "response")
)
mu_cre <- as.numeric(
  predict(cre, newdata = baseKeep, type = "response")
)

if (any(!is.finite(c(y, E, mu_nb, mu_cre))) ||
    any(E <= 0) ||
    any(mu_nb <= 0) ||
    any(mu_cre <= 0)) {
  stop("Non-finite inputs or invalid predictions.")
}

# Validate against previously saved model totals
annual_nb <- sum(mu_nb) / N_YEARS
annual_cre <- sum(mu_cre) / N_YEARS

expected_nb <- 36940.62627168094
expected_cre <- 40273.56795614816

if (abs(annual_nb - expected_nb) > 0.1 ||
    abs(annual_cre - expected_cre) > 0.1) {
  stop(sprintf(
    paste0(
      "Predictions do not reproduce saved totals. ",
      "Model 3 = %.3f, Mundlak = %.3f"
    ),
    annual_nb, annual_cre
  ))
}

# Event-level diagnostics
summarize_model <- function(mu, label, model) {
  err <- mu - y
  
  data.frame(
    model = label,
    n_events = length(y),
    n_countries = length(unique(cc)),
    observed_deaths_total = sum(y),
    predicted_deaths_total = sum(mu),
    observed_annual_deaths = sum(y) / N_YEARS,
    predicted_annual_deaths = sum(mu) / N_YEARS,
    predicted_to_observed_ratio = sum(mu) / sum(y),
    mean_observed_event_rate = mean(y / E),
    mean_predicted_event_rate = mean(mu / E),
    observed_aggregate_rate = sum(y) / sum(E),
    predicted_aggregate_rate = sum(mu) / sum(E),
    mean_absolute_error = mean(abs(err)),
    median_absolute_error = median(abs(err)),
    RMSE = sqrt(mean(err^2)),
    mean_signed_error = mean(err),
    spearman_observed_predicted =
      suppressWarnings(cor(y, mu, method = "spearman")),
    pearson_log1p =
      cor(log1p(y), log1p(mu)),
    conditional_log_score_per_event =
      mean(dnbinom(
        y,
        size = sigma(model),
        mu = mu,
        log = TRUE
      ))
  )
}

event_stats <- rbind(
  summarize_model(mu_nb, "Model 3", nb),
  summarize_model(mu_cre, "Mundlak", cre)
)

# Country-level matched calibration
event <- data.frame(
  Country_Code = cc,
  observed_deaths = y,
  exposure = E,
  Model3_predicted_deaths = mu_nb,
  Mundlak_predicted_deaths = mu_cre
)

sp <- split(seq_len(nrow(event)), event$Country_Code)

country <- do.call(rbind, lapply(names(sp), function(z) {
  ii <- sp[[z]]
  
  yy <- y[ii]
  a <- mu_nb[ii]
  b <- mu_cre[ii]
  
  data.frame(
    Country_Code = z,
    n_events = length(ii),
    observed_annual_deaths = sum(yy) / N_YEARS,
    Model3_annual_deaths = sum(a) / N_YEARS,
    Mundlak_annual_deaths = sum(b) / N_YEARS,
    Model3_absolute_error = abs(sum(a - yy)) / N_YEARS,
    Mundlak_absolute_error = abs(sum(b - yy)) / N_YEARS
  )
}))

rownames(country) <- NULL

country_stats <- data.frame(
  statistic = c(
    "Mean country absolute error",
    "Median country absolute error",
    "Country RMSE",
    "Country WAPE",
    "Country Spearman correlation",
    "Countries with smaller absolute error"
  ),
  Model3 = c(
    mean(country$Model3_absolute_error),
    median(country$Model3_absolute_error),
    sqrt(mean(
      (country$Model3_annual_deaths -
         country$observed_annual_deaths)^2
    )),
    sum(country$Model3_absolute_error) /
      sum(country$observed_annual_deaths),
    cor(
      country$observed_annual_deaths,
      country$Model3_annual_deaths,
      method = "spearman"
    ),
    sum(country$Model3_absolute_error <
          country$Mundlak_absolute_error)
  ),
  Mundlak = c(
    mean(country$Mundlak_absolute_error),
    median(country$Mundlak_absolute_error),
    sqrt(mean(
      (country$Mundlak_annual_deaths -
         country$observed_annual_deaths)^2
    )),
    sum(country$Mundlak_absolute_error) /
      sum(country$observed_annual_deaths),
    cor(
      country$observed_annual_deaths,
      country$Mundlak_annual_deaths,
      method = "spearman"
    ),
    sum(country$Mundlak_absolute_error <
          country$Model3_absolute_error)
  )
)

# Fitted-model comparison
fit_stats <- data.frame(
  model = c("Model 3", "Mundlak"),
  AIC = c(AIC(nb), AIC(cre)),
  BIC = c(BIC(nb), BIC(cre)),
  logLik = c(
    as.numeric(logLik(nb)),
    as.numeric(logLik(cre))
  ),
  NB_size_parameter = c(sigma(nb), sigma(cre))
)

# Save outputs
write.csv(
  event_stats,
  file.path(out_dir, "01_event_level_diagnostics.csv"),
  row.names = FALSE
)
write.csv(
  country_stats,
  file.path(out_dir, "02_country_calibration.csv"),
  row.names = FALSE
)
write.csv(
  country,
  file.path(out_dir, "03_country_observed_vs_predicted.csv"),
  row.names = FALSE
)
write.csv(
  event,
  file.path(out_dir, "04_event_observed_vs_predicted.csv"),
  row.names = FALSE
)
write.csv(
  fit_stats,
  file.path(out_dir, "05_model_fit.csv"),
  row.names = FALSE
)

# Print numerical statistics
cat("\n============================================\n")
cat("MODEL 3 vs MUNDLAK: CALIBRATION DIAGNOSTICS\n")
cat("============================================\n\n")

cat("EVENT-LEVEL RESULTS\n")
print(event_stats, row.names = FALSE, digits = 5)

cat("\nCOUNTRY-LEVEL RESULTS\n")
print(country_stats, row.names = FALSE, digits = 5)

cat("\nMODEL FIT\n")
print(fit_stats, row.names = FALSE, digits = 5)

cat("\nHistorical event counts by country:\n")
print(summary(country$n_events))

cat("\nOUTPUT DIRECTORY:\n")
cat(normalizePath(out_dir), "\n")
