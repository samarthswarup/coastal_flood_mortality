# Step 9b — Historical and future mortality sensitivity to model specification
#
# Compares the Step 8 primary negative-binomial model (Model 3) with the
# Mundlak correlated-random-effects negative-binomial model fitted in Step 8a.
# Reconstructs the fitted historical event sample, predicts matched historical
# and 2050/2100 scenario mortality for both specifications, checks original-
# model future predictions against Step 9, and tabulates model differences,
# HDI scenario contrasts, country rankings and projected-HDI ranges.
# Uses the same coastal-growth multipliers as the current Step 9 script.
# Model predictions in this script are point estimates; no bootstrap is run.
#
# Requires: saved Step 8 model, Step 8a Mundlak model and term mapping,
# Step 9 country master, and country/HDI/population input tables.
# Outputs: Output/Diagnostics_RF_Mundlak/Projection_sensitivity/.
# -----------------------------------------------------------------------------

# ------------------------- SETTINGS -------------------------------------------
PROJECT_DIR <- getwd()                 # project containing "Intermediate outputs"
STEP8_DATE  <- "2026-11-13"           # matches Step 8 model
STEP9_DATE  <- "2026-11-14"           # Step 9 master with country-keyed growth multipliers
STEP7_DATE  <- "2026-07-05"           # matches Step 7 input
N_YEARS <- 35

# Missing training-country means for a country with no fitted NB events are
# filled with the EVENT-WEIGHTED GLOBAL training mean. Results for the fitted
# countries alone are ALSO reported. This is an explicit extrapolation.
UNSEEN_COUNTRY_MEANS <- "global_event_weighted"  # do not change silently

# Per-country annual point estimates should agree to numerical precision.
VALIDATE_STEP9 <- TRUE
ABS_TOL_DEATHS_PER_YEAR <- 0.05
REL_TOL <- 1e-7

# Optional verification refit (disabled by default).
REFIT_REFERENCE_FOR_VALIDATION <- FALSE

# When started from project root rather than its /Claude analysis directory:
if (!dir.exists(file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 8")) &&
    dir.exists(file.path(PROJECT_DIR, "Claude", "Intermediate outputs", "Output from Step 8"))) {
  PROJECT_DIR <- file.path(PROJECT_DIR, "Claude")
}
if (!dir.exists(file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 8"))) {
  stop("Set PROJECT_DIR to the directory containing 'Intermediate outputs/Output from Step 8'.")
}
PROJECT_DIR <- normalizePath(PROJECT_DIR, mustWork = TRUE)
OUT_DIR <- file.path(PROJECT_DIR, "Output", "Diagnostics_RF_Mundlak", "Projection_sensitivity")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

for (p in c("glmmTMB", "splines2", "readxl", "dplyr", "tidyr")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Install required R package: ", p)
}
suppressPackageStartupMessages({library(dplyr); library(tidyr)})

choose_file <- function(directory, pattern, date = NA_character_, kind = "file") {
  if (!dir.exists(directory)) stop("Directory not found: ", directory)
  candidates <- list.files(directory, pattern = pattern, full.names = TRUE)
  if (!is.na(date)) candidates <- candidates[grepl(date, basename(candidates), fixed = TRUE)]
  if (!length(candidates)) stop("Missing ", kind, " in ", directory,
                                if (!is.na(date)) paste0(" (date ", date, ")") else "")
  candidates <- candidates[order(file.info(candidates)$mtime, decreasing = TRUE)]
  if (length(candidates) > 1L) message("Multiple ", kind, " files: using ", basename(candidates[1]),
                                       ". Set explicit date to override.")
  candidates[[1]]
}
input_file <- function(relative_path) {
  # Locate scenario inputs under Input/ or Inputs/ in project/parent.
  bases <- c(PROJECT_DIR, dirname(PROJECT_DIR), dirname(dirname(PROJECT_DIR)))
  candidates <- unlist(lapply(bases, function(b) file.path(b, c("Inputs", "Input"), relative_path)))
  found <- candidates[file.exists(candidates)]
  if (!length(found)) stop("Missing input ", relative_path,
                           "; checked Input(s) folders under project and parent directories.")
  normalizePath(found[[1]], mustWork = TRUE)
}

# ------------------------- LOAD SAVED MODELS / OUTPUTS ------------------------
s8_dir <- file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 8")
s9_dir <- file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 9")
diag_dir <- file.path(PROJECT_DIR, "Output", "Diagnostics_RF_Mundlak")
model_file <- choose_file(s8_dir, "^predMod\\.3 .*\\.RDS$", STEP8_DATE, "Step 8 model")
s8_date <- sub("^predMod\\.3 (.*)\\.RDS$", "\\1", basename(model_file))
trend_file <- file.path(s8_dir, paste0("trendData ", s8_date, ".RDS"))
if (!file.exists(trend_file)) stop("Corresponding Step 8 trendData not found: ", trend_file)
step9_master_file <- choose_file(s9_dir, "^Country_level_master_database_.*\\.csv$",
                                 STEP9_DATE, "Step 9 country master")
step9_country <- read.csv(step9_master_file, stringsAsFactors = FALSE, check.names = FALSE)
step9_country$year <- as.character(step9_country$year)
step9_country$Country_Code <- as.character(step9_country$Country_Code)
step9_country$ssp <- as.character(step9_country$ssp)
step9_country$methodology <- as.character(step9_country$methodology)

nb_fit <- readRDS(model_file)
trendData <- readRDS(trend_file)
trendData$Country_Code <- as.character(trendData$Country_Code)

cre_file <- file.path(diag_dir, "Mundlak_NB_augmented_model.rds")
map_file <- file.path(diag_dir, "Mundlak_mean_term_mapping.csv")
if (!file.exists(cre_file) || !file.exists(map_file)) {
  stop("Run 8a_RF_Mundlak_model_fit.R first; need ", cre_file,
       " and Mundlak_mean_term_mapping.csv")
}
cre_fit <- readRDS(cre_file)
term_map <- read.csv(map_file, stringsAsFactors = FALSE)
stopifnot(all(c("term", "original_covariate") %in% names(term_map)))
if (!identical(sort(term_map$term), sort(grep("^MUND_", names(glmmTMB::fixef(cre_fit)$cond),
                                             value = TRUE)))) {
  stop("Mundlak model terms and mapping CSV do not match. Use outputs of the same diagnostic run.")
}
cat("Step 8 model:", basename(model_file), "\n")
cat("Step 9 country master:", basename(step9_master_file), "\n")
cat("Mundlak model:", basename(cre_file), "\n")

# ------------------------- REBUILD ORIGINAL TRAINING MEANS -------------------
# Reconstruct the model fitting rows to recover
# precisely the country means used for MUND_01 ... MUND_11.
s7_dir <- file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 7")
step7_file <- choose_file(s7_dir,
  "^EMDAT events with exposed pops, HDI GNI and prev events .*\\.xlsx$",
  STEP7_DATE, "Step 7 fitting data")
raw <- as.data.frame(readxl::read_excel(step7_file))
raw$no.events.5.years <- NA_integer_
for (i in seq_len(nrow(raw))) {
  yr <- raw$Start.Year[i]
  if (!is.na(yr) && yr >= 1990 && yr <= 2024) {
    raw$no.events.5.years[i] <- sum((raw$Start.Year %in% ((yr - 5):yr)) &
                                    (raw$ISO == raw$ISO[i]))
  }
}
raw <- raw[!is.na(raw$Start.Year) & raw$Start.Year > 1989, , drop = FALSE]
raw <- raw[!is.na(raw$gni.group), , drop = FALSE]
raw$Year <- raw$Start.Year - 1990
raw$Country_Code <- raw$SLIIDERS.country.code.flat
raw$expPop100 <- raw$pop.exposed.local.else.adm1.else.national.100yr
raw$spline_year <- splines2::bSpline(raw$Year, df = 3)
req <- c("total.deaths.na.as.0", "hdi", "Declaration", "no.events.5.years",
         "pop.total.adm0", "Disaster.Subtype", "Country_Code", "expPop100", "spline_year")
baseKeep <- raw[, req, drop = FALSE]
baseKeep$log_expPop100 <- log(baseKeep$expPop100)
baseKeep$Country_Code <- factor(baseKeep$Country_Code)
baseKeep$Declaration <- factor(baseKeep$Declaration)
baseKeep$Disaster.Subtype <- factor(baseKeep$Disaster.Subtype)
baseKeep <- baseKeep[
  !is.na(baseKeep$expPop100) & baseKeep$expPop100 > 0 &
    !is.na(baseKeep$pop.total.adm0) & baseKeep$pop.total.adm0 > 0, , drop = FALSE]
baseKeep <- baseKeep[stats::complete.cases(baseKeep), , drop = FALSE]
baseKeep <- droplevels(baseKeep)
row.names(baseKeep) <- NULL
if (nrow(baseKeep) != stats::nobs(nb_fit) || nrow(baseKeep) != stats::nobs(cre_fit)) {
  stop("Step 7 reconstructed fitting rows do not match both saved model sample sizes.")
}
if (REFIT_REFERENCE_FOR_VALIDATION) {
  f0 <- formula(nb_fit)
  nb_check <- glmmTMB::glmmTMB(f0, data = baseKeep, family = glmmTMB::nbinom2())
  if (abs(as.numeric(logLik(nb_check)) - as.numeric(logLik(nb_fit))) > 0.1) {
    stop("Reconstructed original model differs in log-likelihood.")
  }
  rm(nb_check)
}

train_matrix <- stats::model.matrix(
  ~ log(pop.total.adm0) + spline_year + hdi + Declaration +
    no.events.5.years + Disaster.Subtype, data = baseKeep)
train_matrix <- train_matrix[, colnames(train_matrix) != "(Intercept)", drop = FALSE]
country <- as.character(baseKeep$Country_Code)
counts <- table(country)
means_by_country <- rowsum(train_matrix, group = country, reorder = TRUE)
means_by_country <- sweep(means_by_country, 1L,
                          as.numeric(counts[rownames(means_by_country)]), "/")
global_means <- colMeans(train_matrix)
if (!identical(term_map$original_covariate,
               colnames(train_matrix)[colnames(train_matrix) %in% term_map$original_covariate])) {
  # Compare sets and map names, not matrix column order, which might vary.
  if (!setequal(term_map$original_covariate, colnames(train_matrix))) {
    stop("Reconstructed Mundlak predictor names do not match mapping. Inspect contrasts/levels.")
  }
}

attach_mundlak <- function(data) {
  cc <- as.character(data$Country_Code)
  matched <- match(cc, rownames(means_by_country))
  missing <- is.na(matched)
  mm <- matrix(NA_real_, nrow = nrow(data), ncol = ncol(means_by_country),
               dimnames = list(NULL, colnames(means_by_country)))
  if (any(!missing)) mm[!missing, ] <- means_by_country[matched[!missing], , drop = FALSE]
  if (any(missing)) {
    if (UNSEEN_COUNTRY_MEANS != "global_event_weighted") stop("Unsupported unseen-country policy.")
    mm[missing, ] <- matrix(rep(global_means, each = sum(missing)), nrow = sum(missing),
                            byrow = FALSE)
  }
  for (j in seq_len(nrow(term_map))) {
    data[[term_map$term[j]]] <- mm[, term_map$original_covariate[j]]
  }
  data
}

# Critical self-check: do recovered country means reproduce the saved CRE fit?
train_cre <- attach_mundlak(baseKeep)
hist_cre_saved <- as.numeric(predict(cre_fit, type = "response"))
hist_cre_reconstructed <- as.numeric(predict(cre_fit, newdata = train_cre,
                                              type = "response", allow.new.levels = TRUE))
max_rel_train <- max(abs(hist_cre_saved - hist_cre_reconstructed) /
                      pmax(1, abs(hist_cre_saved)), na.rm = TRUE)
cat("CRE fitted-vs-reconstructed prediction maximum relative difference:",
    signif(max_rel_train, 5), "\n")
if (!is.finite(max_rel_train) || max_rel_train > 1e-5) {
  stop("Historical MUND_* means do not reproduce the saved CRE model predictions.")
}
seen_countries <- rownames(means_by_country)
unseen_countries <- setdiff(unique(trendData$Country_Code), seen_countries)
cat("Countries in projection data absent from NB fit:",
    if (length(unseen_countries)) paste(unseen_countries, collapse = ", ") else "none", "\n")

# ------------------------- MATCH STEP 9 INPUT DATA ----------------------------
map_path <- input_file("Identify mismatched and missing countries - step 3/SLIIDERS country codes with names and territorial mapping.csv")
tellman_path <- input_file("Flood zone pop projection/Tellman_2021_S5_multipliers.csv")
liu_path <- input_file("HDI projection/Liu_2024_HDI_pop_forecasts.xlsx")
country.data <- read.csv(map_path)
hdr_code_map <- data.frame(
  Country_Code = country.data[, 1],
  hdr.country.code = ifelse(!is.na(country.data[, 6]) & country.data[, 6] != "",
                            country.data[, 6], country.data[, 1]),
  stringsAsFactors = FALSE)
trendData_codes <- data.frame(Country_Code = unique(trendData$Country_Code)) %>%
  dplyr::left_join(hdr_code_map, by = "Country_Code") %>%
  dplyr::mutate(hdr.country.code = ifelse(is.na(hdr.country.code), Country_Code, hdr.country.code)) %>%
  dplyr::distinct(Country_Code, .keep_all = TRUE)
gni_lookup <- trendData %>% dplyr::select(Country_Code, gni.group) %>%
  dplyr::distinct(Country_Code, .keep_all = TRUE) %>% dplyr::filter(!is.na(gni.group))
rentschler_mult <- c(LI = 0.89, LMI = 1.53, UMI = 1.92, HI = 0.79)
tellman_raw <- read.csv(tellman_path, stringsAsFactors = FALSE)
tellman_lookup <- setNames(tellman_raw$tellman_mult_2000_2015, tellman_raw$Country_Code)
build_country_mult <- function(country_codes, country_lookup, gni_fallback_vec) {
  mult <- country_lookup[country_codes]
  missing <- is.na(mult)
  if (any(missing)) {
    gni <- gni_lookup$gni.group[match(country_codes[missing], gni_lookup$Country_Code)]
    mult[missing] <- gni_fallback_vec[as.character(gni)]
  }
  mult[is.na(mult)] <- 1.0
  names(mult) <- as.character(country_codes)
  mult
}

# The same keyed mapping is used in Step 9. Validate it independently.
check_codes <- unique(as.character(trendData$Country_Code))
check_mult <- build_country_mult(check_codes, tellman_lookup, rentschler_mult)
if (!identical(names(check_mult), check_codes) ||
    anyNA(check_mult) || any(!is.finite(check_mult)) || any(check_mult <= 0)) {
  stop("Invalid or incorrectly keyed coastal-growth multipliers.")
}

liu_raw <- readxl::read_excel(liu_path)
name_map <- country.data %>%
  dplyr::select(Country_Code = 1, name1 = 2, name2 = 3, name3 = 4) %>%
  tidyr::pivot_longer(cols = c(name1, name2, name3), names_to = "name_col", values_to = "name") %>%
  dplyr::filter(!is.na(name), name != "") %>%
  dplyr::select(Country_Code, name) %>%
  dplyr::mutate(name = tolower(trimws(name)))
manual_overrides <- data.frame(
  name = tolower(c(
    "Armenia", "Austria", "Azerbaijan", "Belarus", "Bhutan",
    "Bolivia (Plurinational State of)", "Botswana", "Burkina Faso",
    "Burundi", "Cape Verde", "Central African Republic", "Chad",
    "Cote d'Ivoire", "Czech Republic", "Hungary", "Kazakhstan",
    "Kyrgyzstan", "Lao People's Democratic Republic", "Lesotho",
    "Luxembourg", "Malawi", "Mali", "Mongolia", "Nepal", "Niger",
    "Paraguay", "Republic of Moldova", "Rwanda", "Serbia", "Slovakia",
    "Swaziland", "Switzerland", "Syrian Arab Republic", "Tajikistan",
    "The former Yugoslav Republic of Macedonia", "Turkmenistan",
    "Uganda", "Zambia", "Zimbabwe")),
  Country_Code = c(
    "ARM", "AUT", "AZE", "BLR", "BTN", "BOL", "BWA", "BFA", "BDI", "CPV",
    "CAF", "TCD", "CIV", "CZE", "HUN", "KAZ", "KGZ", "LAO", "LSO", "LUX",
    "MWI", "MLI", "MNG", "NPL", "NER", "PRY", "MDA", "RWA", "SRB", "SVK",
    "SWZ", "CHE", "SYR", "TJK", "MKD", "TKM", "UGA", "ZMB", "ZWE"),
  stringsAsFactors = FALSE)
name_lookup <- dplyr::bind_rows(name_map, manual_overrides) %>% dplyr::distinct(name, .keep_all = TRUE)
liu <- liu_raw %>% dplyr::mutate(name = tolower(trimws(Area))) %>%
  dplyr::left_join(name_lookup, by = "name") %>% dplyr::filter(!is.na(Country_Code)) %>% dplyr::select(-name)
hdi_last <- trendData %>% dplyr::group_by(Country_Code) %>%
  dplyr::summarise(hdi_last = mean(hdi, na.rm = TRUE), .groups = "drop")
hdi_trend <- trendData_codes %>%
  dplyr::left_join(liu %>% dplyr::select(Country_Code,
    hdi_SSP1_2050 = SSP1_2050, hdi_SSP1_2100 = SSP1_2100,
    hdi_SSP2_2050 = SSP2_2050, hdi_SSP2_2100 = SSP2_2100,
    hdi_SSP3_2050 = SSP3_2050, hdi_SSP3_2100 = SSP3_2100,
    hdi_SSP4_2050 = SSP4_2050, hdi_SSP4_2100 = SSP4_2100,
    hdi_SSP5_2050 = SSP5_2050, hdi_SSP5_2100 = SSP5_2100,
    pop_SSP1_2050, pop_SSP1_2100, pop_SSP2_2050, pop_SSP2_2100,
    pop_SSP3_2050, pop_SSP3_2100, pop_SSP4_2050, pop_SSP4_2100,
    pop_SSP5_2050, pop_SSP5_2100),
    by = c("hdr.country.code" = "Country_Code")) %>%
  dplyr::distinct(Country_Code, .keep_all = TRUE) %>%
  dplyr::left_join(hdi_last, by = "Country_Code") %>%
  dplyr::mutate(hdi_2050 = ifelse(is.na(hdi_SSP2_2050), hdi_last, hdi_SSP2_2050),
         hdi_2100 = ifelse(is.na(hdi_SSP2_2100), hdi_last, hdi_SSP2_2100))

build_trendData <- function(td, year_label) {
  predKeepNames <- names(td)[grepl("pop.exposed.local.else.adm1.else.national", names(td))]
  predKeepNames <- predKeepNames[grepl(".100yr", predKeepNames)]
  predKeepNames <- predKeepNames[!grepl("pooled", predKeepNames)]
  predKeepNames <- predKeepNames[grepl(year_label, predKeepNames)]
  predKeepNames <- predKeepNames[grepl("SSP1.26", predKeepNames) |
                                   grepl("SSP5.85", predKeepNames) |
                                   grepl("SSP1.19", predKeepNames) |
                                   grepl("SSP2.45", predKeepNames) |
                                   grepl("SSP3.70", predKeepNames)]
  fixed <- c("total.deaths.na.as.0", "pop.total.adm0", "spline_year", "hdi",
             "Declaration", "no.events.5.years", "Disaster.Subtype", "Country_Code", "Start.Year")
  td <- td[, names(td) %in% c(fixed, predKeepNames)]
  td$Country_Code <- as.character(td$Country_Code)
  ssp_cols <- setdiff(names(td), fixed)
  fixed_cols <- setdiff(names(td), ssp_cols)
  td <- td[, c(fixed_cols, ssp_cols)]
  list(data = td, ssp_cols = ssp_cols, fixed_cols = fixed_cols)
}
td_by_year <- list(`2050` = build_trendData(trendData, "2050"),
                   `2100` = build_trendData(trendData, "2100"))

# Exact Step 9 scenario pairings; only point estimates, not confidence intervals.
ssp_pairings <- list(
 list(label="SSP1.19", slr="med_SSP1.19", SSP="SSP1", suffix="SSP1"),
 list(label="SSP1.26_med", slr="med_SSP1.26", SSP="SSP1", suffix="SSP1"),
 list(label="SSP1.26_low", slr="low_SSP1.26", SSP="SSP1", suffix="SSP1"),
 list(label="SSP2.45", slr="med_SSP2.45", SSP="SSP2", suffix="SSP2"),
 list(label="SSP3.70", slr="med_SSP3.70", SSP="SSP3", suffix="SSP3"),
 list(label="SSP5.85_med", slr="med_SSP5.85", SSP="SSP5", suffix="SSP5"),
 list(label="SSP5.85_low", slr="low_SSP5.85", SSP="SSP5", suffix="SSP5"),
 list(label="BOUND_worst", slr="low_SSP5.85", SSP="SSP3", suffix="BOUND_worst"),
 list(label="BOUND_worst_med", slr="med_SSP5.85", SSP="SSP3", suffix="BOUND_worst"),
 list(label="BOUND_best", slr="med_SSP1.19", SSP="SSP5", suffix="BOUND_best")
)
row_specs <- list(
 list(name=" SLR only", vary_hdi=FALSE, vary_pop=FALSE, popscale=FALSE, coastal=NULL),
 list(name=" + Population (exposure only)", vary_hdi=FALSE, vary_pop=FALSE, popscale=TRUE, coastal=NULL),
 list(name=" + Population (exposure + NB covariate)", vary_hdi=FALSE, vary_pop=TRUE, popscale=TRUE, coastal=NULL),
 list(name=" + Population (both) & HDI", vary_hdi=TRUE, vary_pop=TRUE, popscale=TRUE, coastal=NULL),
 list(name=" + Population (both) & HDI & coastal growth 1", vary_hdi=TRUE, vary_pop=TRUE, popscale=TRUE, coastal="rentschler"),
 list(name=" + Population (both) & HDI & coastal growth 2", vary_hdi=TRUE, vary_pop=TRUE, popscale=TRUE, coastal="tellman")
)

# ------------------------- HISTORICAL PREDICTIONS -----------------------------
# Annualized historical predictions on Step 8's saved trendData: same set of
# countries/events as Step 8's CountryDeathCIs and Step 9's historical baseline,
# NOT merely the smaller complete-case training sample.
hist_req <- c("total.deaths.na.as.0", "hdi", "Declaration", "no.events.5.years",
              "pop.total.adm0", "Disaster.Subtype", "Country_Code", "spline_year",
              "expPop100", "Start.Year")
missing_hist <- setdiff(hist_req, names(trendData))
if (length(missing_hist)) stop("Saved trendData lacks historical columns: ",
                               paste(missing_hist, collapse = ", "))
hist_data <- trendData[, hist_req]
hist_data$log_expPop100 <- log(hist_data$expPop100)
hist_nb <- as.numeric(predict(nb_fit, newdata = hist_data, type = "response",
                              allow.new.levels = TRUE))
hist_cre <- as.numeric(predict(cre_fit, newdata = attach_mundlak(hist_data),
                               type = "response", allow.new.levels = TRUE))
if (!identical(which(!is.na(hist_nb)), which(!is.na(hist_cre)))) {
  stop("Historical NB and CRE prediction validity differs.")
}
hist_valid <- !is.na(hist_nb)
hist_country <- data.frame(Country_Code = as.character(hist_data$Country_Code)[hist_valid],
                           nb = hist_nb[hist_valid], cre = hist_cre[hist_valid]) %>%
  dplyr::group_by(Country_Code) %>%
  dplyr::summarise(n_events = n(), NB_annual = sum(nb) / N_YEARS,
            CRE_annual = sum(cre) / N_YEARS, .groups = "drop") %>%
  dplyr::mutate(seen_in_fitted_model = Country_Code %in% seen_countries,
         delta_annual = CRE_annual - NB_annual,
         pct_difference = ifelse(NB_annual > 0, 100 * delta_annual / NB_annual, NA_real_))
write.csv(hist_country, file.path(OUT_DIR, "01_historical_country_fit_comparison.csv"), row.names = FALSE)
historical_summary <- data.frame(NB_annual = sum(hist_country$NB_annual),
                                 CRE_annual = sum(hist_country$CRE_annual))
historical_summary$delta_annual <- historical_summary$CRE_annual - historical_summary$NB_annual
historical_summary$pct_difference <- 100 * historical_summary$delta_annual / historical_summary$NB_annual

# Check that the original NB historical baseline equals Step 8 country baseline.
s8_ci_file <- file.path(s8_dir, "Country tables", paste0("CountryDeathCIs ", s8_date, ".csv"))
if (file.exists(s8_ci_file)) {
  s8_ci <- read.csv(s8_ci_file)
  s8_hist_annual <- sum(s8_ci$meanDeaths * s8_ci$nEvents, na.rm = TRUE) / N_YEARS
  gap <- abs(s8_hist_annual - historical_summary$NB_annual)
  cat("Original historical baseline vs saved Step 8 CountryDeathCIs difference:", gap, "
")
  if (gap > ABS_TOL_DEATHS_PER_YEAR + REL_TOL * abs(s8_hist_annual)) {
    stop("Historical baseline does not reproduce Step 8 CountryDeathCIs. ",
         "Check which Step 8 model and trendData dates were used.")
  }
}

# ------------------------- PROJECTION PREDICTIONS -----------------------------
# One prediction call per MODEL per year / scenario / methodology; no bootstrap.
results <- list()
validation <- list()
k <- 0L
v <- 0L
for (yr in c(2050L, 2100L)) {
  td_obj <- td_by_year[[as.character(yr)]]
  for (sp in ssp_pairings) {
    pop_col <- paste0("pop_", sp$SSP, "_", yr)
    hdi_col <- paste0("hdi_", sp$SSP, "_", yr)
    hdi_vec <- hdi_trend[[hdi_col]][match(td_obj$data$Country_Code, hdi_trend$Country_Code)]
    pop_vec <- hdi_trend[[pop_col]][match(td_obj$data$Country_Code, hdi_trend$Country_Code)] * 1000

    for (rs in row_specs) {
      td <- td_obj$data
      if (rs$vary_hdi) td$hdi[!is.na(hdi_vec)] <- hdi_vec[!is.na(hdi_vec)]
      if (rs$vary_pop) td$pop.total.adm0[!is.na(pop_vec)] <- pop_vec[!is.na(pop_vec)]
      candidate_cols <- td_obj$ssp_cols[
        grepl(sp$slr, td_obj$ssp_cols) & grepl("\\.50\\.", td_obj$ssp_cols)]
      if (rs$popscale) {
        exposure_col <- candidate_cols[
          grepl(paste0("\\.popscale\\.", sp$suffix, "$"), candidate_cols)]
      } else {
        exposure_col <- candidate_cols[!grepl("popscale", candidate_cols)]
      }
      if (length(exposure_col) != 1L) {
        stop("Step 9 exposure selection mismatch: ", yr, ", ", sp$label, ", ",
             rs$name, ": ", paste(exposure_col, collapse = ", "))
      }

      current <- td[, c(td_obj$fixed_cols, exposure_col)]
      names(current)[length(td_obj$fixed_cols) + 1L] <- "expPop100"
      if (!is.null(rs$coastal)) {
        cc <- unique(td$Country_Code)
        if (rs$coastal == "rentschler") {
          gni <- gni_lookup$gni.group[match(cc, gni_lookup$Country_Code)]
          coastal_mult <- rentschler_mult[as.character(gni)]
          names(coastal_mult) <- cc
          coastal_mult[is.na(coastal_mult)] <- 1.0
        } else {
          coastal_mult <- build_country_mult(cc, tellman_lookup, rentschler_mult)
        }
        # Match Step 9's explicitly country-keyed multiplier lookup.
        event_mult <- unname(coastal_mult[as.character(current$Country_Code)])
        if (anyNA(event_mult) || any(!is.finite(event_mult)) || any(event_mult <= 0)) {
          stop("Invalid country-level coastal multiplier in ", yr, " / ", sp$label)
        }
        current$expPop100 <- current$expPop100 * event_mult
      }
      current$log_expPop100 <- log(current$expPop100)

      nb_pred <- as.numeric(predict(nb_fit, newdata = current, type = "response",
                                    allow.new.levels = TRUE))
      cur_cre <- attach_mundlak(current)
      cre_pred <- as.numeric(predict(cre_fit, newdata = cur_cre, type = "response",
                                     allow.new.levels = TRUE))
      if (length(nb_pred) != nrow(current) || length(cre_pred) != nrow(current)) {
        stop("Prediction length changed for ", yr, " / ", sp$label, " / ", rs$name)
      }
      if (any(is.infinite(nb_pred)) || any(is.infinite(cre_pred))) {
        stop("Infinite predictions: inspect very large future extrapolations.")
      }
      if (!identical(which(!is.na(nb_pred)), which(!is.na(cre_pred)))) {
        stop("NB and CRE have different valid-event sets. Fix before comparing totals.")
      }
      valid <- !is.na(nb_pred)
      d <- data.frame(Country_Code = as.character(current$Country_Code)[valid],
                      exposure = current$expPop100[valid], NB = nb_pred[valid],
                      CRE = cre_pred[valid],
                      hdi_used = current$hdi[valid],
                      pop_used = current$pop.total.adm0[valid])
      if (!nrow(d)) stop("No valid events for ", yr, sp$label, rs$name)
      out <- d %>% dplyr::group_by(Country_Code) %>%
        dplyr::summarise(n_events = n(), NB_annual = sum(NB)/N_YEARS,
                  CRE_annual = sum(CRE)/N_YEARS,
                  NB_mortality_fraction = mean(NB)/mean(exposure),
                  CRE_mortality_fraction = mean(CRE)/mean(exposure),
                  hdi_mean_used = mean(hdi_used),
                  pop_mean_used = mean(pop_used),
                  .groups = "drop") %>%
        dplyr::mutate(year = as.character(yr), ssp = sp$label,
               methodology = rs$name,
               seen_in_fitted_model = Country_Code %in% seen_countries,
               abs_delta_deaths = CRE_annual - NB_annual,
               pct_delta_deaths = ifelse(NB_annual > 0,
                 100 * (CRE_annual / NB_annual - 1), NA_real_))
      k <- k + 1L
      results[[k]] <- as.data.frame(out)

      # Check against the exact Step 9 saved country-level annual estimates.
      if (VALIDATE_STEP9) {
        ref <- step9_country[step9_country$year == as.character(yr) &
                  step9_country$ssp == sp$label &
                  step9_country$methodology == rs$name,
                c("Country_Code", "ann_deaths", "n_events"), drop = FALSE]
        if (!nrow(ref)) stop("Missing Step 9 reference combination: ", yr, sp$label, rs$name)
        merged <- merge(as.data.frame(out)[, c("Country_Code", "NB_annual", "n_events")],
                        ref, by = "Country_Code", all = TRUE,
                        suffixes = c("_rebuilt", "_step9"))
        if (anyNA(merged$NB_annual) || anyNA(merged$ann_deaths) ||
            anyNA(merged$n_events_rebuilt) || anyNA(merged$n_events_step9)) {
          stop("Country composition differs from saved Step 9, ", yr, sp$label, rs$name)
        }
        merged$abs_difference <- abs(merged$NB_annual - merged$ann_deaths)
        merged$allowed <- ABS_TOL_DEATHS_PER_YEAR + REL_TOL * abs(merged$ann_deaths)
        merged$year <- yr; merged$ssp <- sp$label; merged$methodology <- rs$name
        v <- v + 1L
        validation[[v]] <- merged[, c("year", "ssp", "methodology", "Country_Code",
                                    "NB_annual", "ann_deaths", "abs_difference",
                                    "n_events_rebuilt", "n_events_step9", "allowed")]
        if (any(merged$abs_difference > merged$allowed) ||
            any(merged$n_events_rebuilt != merged$n_events_step9)) {
          write.csv(do.call(rbind, validation),
                    file.path(OUT_DIR, "00_STEP9_VALIDATION_DIFFERENCES.csv"), row.names = FALSE)
          bad <- merged[merged$abs_difference > merged$allowed |
                          merged$n_events_rebuilt != merged$n_events_step9, ]
          stop("MODEL CHECK FAILED: reproduced NB predictions differ from saved Step 9. ",
               "First affected country: ", bad$Country_Code[1], "; see validation CSV. ",
               "Check Step 8/9 input dates and script versions before using CRE predictions.")
        }
      }
      cat("Done ", yr, " / ", sp$label, " / ", trimws(rs$name), "\n", sep = "")
    }
  }
}

country_sensitivity <- dplyr::bind_rows(results)
write.csv(country_sensitivity,
          file.path(OUT_DIR, "02_country_2050_2100_NB_vs_Mundlak.csv"), row.names = FALSE)
if (VALIDATE_STEP9) {
  validation_table <- dplyr::bind_rows(validation)
  write.csv(validation_table, file.path(OUT_DIR, "00_STEP9_VALIDATION_DIFFERENCES.csv"),
            row.names = FALSE)
  cat("Maximum absolute reproduction difference from Step 9 country outputs:",
      max(validation_table$abs_difference), "annual deaths\n")
}

# ------------------------- GLOBAL AND HDI EFFECT TABLES -----------------------
all_global <- country_sensitivity %>% dplyr::group_by(year, ssp, methodology) %>%
  dplyr::summarise(NB_annual = sum(NB_annual), CRE_annual = sum(CRE_annual),
            n_countries = n(), .groups = "drop") %>% dplyr::mutate(population = "ALL_PROJECTION_COUNTRIES")
fit_global <- country_sensitivity %>% dplyr::filter(seen_in_fitted_model) %>%
  dplyr::group_by(year, ssp, methodology) %>%
  dplyr::summarise(NB_annual = sum(NB_annual), CRE_annual = sum(CRE_annual),
            n_countries = n(), .groups = "drop") %>% dplyr::mutate(population = "FIT_COUNTRIES_ONLY")
hist_fit_nb <- sum(hist_country$NB_annual[hist_country$seen_in_fitted_model])
hist_fit_cre <- sum(hist_country$CRE_annual[hist_country$seen_in_fitted_model])
global <- dplyr::bind_rows(all_global, fit_global) %>%
  dplyr::mutate(abs_delta_deaths = CRE_annual - NB_annual,
         pct_delta_deaths = ifelse(NB_annual > 0, 100 * abs_delta_deaths / NB_annual, NA_real_),
         historical_NB_annual = ifelse(population == "FIT_COUNTRIES_ONLY",
                                       hist_fit_nb, historical_summary$NB_annual),
         historical_CRE_annual = ifelse(population == "FIT_COUNTRIES_ONLY",
                                        hist_fit_cre, historical_summary$CRE_annual),
         NB_pct_vs_own_historical = 100 * (NB_annual / historical_NB_annual - 1),
         CRE_pct_vs_own_historical = 100 * (CRE_annual / historical_CRE_annual - 1))
write.csv(global, file.path(OUT_DIR, "03_global_2050_2100_NB_vs_Mundlak.csv"), row.names = FALSE)

# Two comparable HDI effects within each model, holding everything else
# (SLR, exposure scaling, national population covariate) fixed:
# Row 3 without projected HDI vs Row 4 with projected HDI.
no_hdi_label <- " + Population (exposure + NB covariate)"
with_hdi_label <- " + Population (both) & HDI"
no_hdi <- global[global$methodology == no_hdi_label,
  c("population", "year", "ssp", "NB_annual", "CRE_annual")]
with_hdi <- global[global$methodology == with_hdi_label,
  c("population", "year", "ssp", "NB_annual", "CRE_annual")]
hdi_effect <- merge(no_hdi, with_hdi, by = c("population", "year", "ssp"),
                    suffixes = c("_no_proj_HDI", "_with_proj_HDI"))
hdi_effect$NB_HDI_associated_deaths_averted <-
  hdi_effect$NB_annual_no_proj_HDI - hdi_effect$NB_annual_with_proj_HDI
hdi_effect$CRE_HDI_associated_deaths_averted <-
  hdi_effect$CRE_annual_no_proj_HDI - hdi_effect$CRE_annual_with_proj_HDI
hdi_effect$NB_HDI_pct_reduction <- 100 * hdi_effect$NB_HDI_associated_deaths_averted /
  hdi_effect$NB_annual_no_proj_HDI
hdi_effect$CRE_HDI_pct_reduction <- 100 * hdi_effect$CRE_HDI_associated_deaths_averted /
  hdi_effect$CRE_annual_no_proj_HDI
hdi_effect$change_in_associated_averted_deaths <-
  hdi_effect$CRE_HDI_associated_deaths_averted -
  hdi_effect$NB_HDI_associated_deaths_averted
write.csv(hdi_effect,
  file.path(OUT_DIR, "04_HDI_projection_effect_NB_vs_Mundlak.csv"), row.names = FALSE)

# Country rank robustness under the same scenario, with each model's
# annual mortality predictions (not mortalities normalized by population).
rank_table <- country_sensitivity %>% dplyr::group_by(year, ssp, methodology) %>%
  dplyr::mutate(NB_rank = rank(-NB_annual, ties.method = "min"),
         CRE_rank = rank(-CRE_annual, ties.method = "min"),
         rank_shift = CRE_rank - NB_rank) %>% ungroup() %>%
  dplyr::select(year, ssp, methodology, Country_Code, seen_in_fitted_model,
         NB_rank, CRE_rank, rank_shift, NB_annual, CRE_annual)
write.csv(rank_table, file.path(OUT_DIR, "05_country_ranking_comparison.csv"), row.names = FALSE)
rank_summary <- country_sensitivity %>% dplyr::group_by(year, ssp, methodology) %>%
  dplyr::summarise(Spearman_rho = suppressWarnings(cor(NB_annual, CRE_annual, method = "spearman")),
            .groups = "drop")
write.csv(rank_summary,
  file.path(OUT_DIR, "06_country_ranking_Spearman.csv"), row.names = FALSE)

# Compute modeled rate ratios for a +0.10 HDI difference, holding other
# covariate values fixed.
b_nb <- unname(glmmTMB::fixef(nb_fit)$cond["hdi"])
b_cre <- unname(glmmTMB::fixef(cre_fit)$cond["hdi"])
RR_dHDI_0p10 <- data.frame(
  model = c("Original NB", "Mundlak CRE NB"),
  beta_HDI = c(b_nb, b_cre),
  relative_mortality_for_plus_0p10_HDI = exp(0.1 * c(b_nb, b_cre)),
  change_pct_for_plus_0p10_HDI = 100 * (exp(0.1 * c(b_nb, b_cre)) - 1))
write.csv(RR_dHDI_0p10,
  file.path(OUT_DIR, "07_HDI_coefficient_sensitivity.csv"), row.names = FALSE)

# Flag projected HDI values outside the within-country historical range.
hdi_hist <- baseKeep %>% dplyr::mutate(Country_Code = as.character(Country_Code)) %>%
  dplyr::group_by(Country_Code) %>%
  dplyr::summarise(hdi_min = min(hdi), hdi_max = max(hdi),
            hdi_mean = mean(hdi), .groups = "drop")
hdi_domain <- country_sensitivity %>%
  dplyr::filter(methodology == with_hdi_label) %>%
  dplyr::select(year, ssp, Country_Code, hdi_mean_used) %>%
  dplyr::left_join(hdi_hist, by = "Country_Code") %>%
  dplyr::mutate(outside_country_historical_HDI_range =
           !is.na(hdi_min) & (hdi_mean_used < hdi_min | hdi_mean_used > hdi_max),
         hdi_change_vs_historical_country_mean = hdi_mean_used - hdi_mean)
write.csv(hdi_domain, file.path(OUT_DIR, "08_HDI_extrapolation_diagnostics.csv"), row.names = FALSE)

# ------------------------- READABLE CONSOLE SUMMARY ---------------------------
cat("\n=== HISTORICAL FIT ===\n")
print(historical_summary, row.names = FALSE)
cat("\n=== HDI + POPULATION SCENARIO (Row 4), SSP2.45 ===\n")
print(as.data.frame(global %>% dplyr::filter(population == "ALL_PROJECTION_COUNTRIES",
                                      ssp == "SSP2.45", methodology == with_hdi_label)), row.names = FALSE)
cat("\n=== HDI-ASSOCIATED AVOIDED DEATHS, SSP2.45 ===\n")
print(hdi_effect[hdi_effect$population == "ALL_PROJECTION_COUNTRIES" &
                   hdi_effect$ssp == "SSP2.45", ], row.names = FALSE)
cat("\n=== HDI COEFFICIENT SENSITIVITY ===\n")
print(RR_dHDI_0p10, row.names = FALSE)
cat("\n=== HISTORICAL-TO-FUTURE HDI EXTRAPOLATION ===\n")
print(hdi_domain %>% dplyr::group_by(year) %>%
        dplyr::summarise(countries_outside_historical_range =
                    sum(outside_country_historical_HDI_range),
                  countries_with_history = sum(!is.na(hdi_min)), .groups = "drop"))

# Save run inputs and numerical validation metadata.
summary_text <- c(
  paste("Step 8 model:", model_file),
  paste("Step 9 input:", step9_master_file),
  paste("Mundlak fitted model:", cre_file),
  paste("Step 7 input:", step7_file),
  paste("Training countries:", length(seen_countries)),
  paste("Projection countries without fitted Mundlak means:", length(unseen_countries)),
  paste("Maximum relative fitted-prediction difference:", max_rel_train),
  paste("Step 9 validation passed:", VALIDATE_STEP9)
)
writeLines(summary_text, file.path(OUT_DIR, "run_metadata.txt"))
cat("\nOutputs saved in:\n", OUT_DIR, "\n", sep = "")
