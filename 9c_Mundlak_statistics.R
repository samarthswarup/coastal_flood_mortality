# Step 9c — Tabulation of original-versus-Mundlak model statistics
#
# Reads saved outputs from Steps 8, 8a, 9 and 9b. Verifies matched
# original-model country projections against Step 9 and tabulates historical
# fitted deaths, AIC/likelihood-ratio statistics, HDI coefficients and rate
# ratios, 2050/2100 annual-death projections, country rankings and differences.
# Writes machine-readable CSV tables and a text report of numerical statistics.
# No models are fitted and no projection bootstrap is performed here.
#
# Output: Output/Diagnostics_RF_Mundlak/Mundlak_statistics_only/.
# Usage: Rscript scripts/9c_Mundlak_statistics.R [project_directory] [step9_date] [step8_date]

args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1L) args[[1]] else getwd()
step9_date <- if (length(args) >= 2L) args[[2]] else "2026-11-14"
step8_date <- if (length(args) >= 3L) args[[3]] else "2026-11-13"

if (!dir.exists(file.path(project_dir, "Intermediate outputs", "Output from Step 9")) &&
    dir.exists(file.path(project_dir, "Claude", "Intermediate outputs", "Output from Step 9"))) {
  project_dir <- file.path(project_dir, "Claude")
}
project_dir <- normalizePath(project_dir, mustWork = TRUE)
s9_dir <- file.path(project_dir, "Intermediate outputs", "Output from Step 9")
diag_dir <- file.path(project_dir, "Output", "Diagnostics_RF_Mundlak")
projection_dir <- file.path(diag_dir, "Projection_sensitivity")
step8_dir <- file.path(project_dir, "Intermediate outputs", "Output from Step 8")
output_dir <- file.path(diag_dir, "Mundlak_statistics_only")

paths <- list(
  prior = file.path(projection_dir, "02_country_2050_2100_NB_vs_Mundlak.csv"),
  historical = file.path(projection_dir, "01_historical_country_fit_comparison.csv"),
  step9_country = file.path(s9_dir,
    paste0("Country_level_master_database_", step9_date, ".csv")),
  decomposition = file.path(s9_dir,
    paste0("decomposition_table_population_methodology_", step9_date, ".csv")),
  hdi_coefficients = file.path(projection_dir, "07_HDI_coefficient_sensitivity.csv"),
  hdi_extrapolation = file.path(projection_dir, "08_HDI_extrapolation_diagnostics.csv"),
  nb_model = file.path(step8_dir, paste0("predMod.3 ", step8_date, ".RDS")),
  cre_model = file.path(diag_dir, "Mundlak_NB_augmented_model.rds"),
  mean_mapping = file.path(diag_dir, "Mundlak_mean_term_mapping.csv"),
  model_summary = file.path(diag_dir, "Mundlak_NB_summary.txt")
)

required <- c("prior", "historical", "step9_country", "decomposition")
missing_files <- paths[required][!file.exists(unlist(paths[required]))]
if (length(missing_files)) {
  stop("Missing input files:\n", paste(unlist(missing_files), collapse = "\n"))
}

read_csv <- function(path) utils::read.csv(path, stringsAsFactors = FALSE,
                                            check.names = FALSE)
require_columns <- function(d, cols, name) {
  missing <- setdiff(cols, names(d))
  if (length(missing)) stop(name, " missing columns: ", paste(missing, collapse = ", "))
}
join_key <- function(d) paste(d$year, d$ssp, d$methodology,
                               d$Country_Code, sep = " | ")
fmt <- function(x, digits = 2L) {
  ifelse(is.finite(x), formatC(x, format = "f", digits = digits,
                              big.mark = ","), "NA")
}

prior <- read_csv(paths$prior)
historical <- read_csv(paths$historical)
step9 <- read_csv(paths$step9_country)
decomp <- read_csv(paths$decomposition)

require_columns(prior, c("Country_Code", "year", "ssp", "methodology",
                         "NB_annual", "CRE_annual", "seen_in_fitted_model",
                         "n_events"), "Matched Mundlak projections")
require_columns(historical, c("Country_Code", "NB_annual", "CRE_annual",
                              "seen_in_fitted_model"), "Historical comparison")
require_columns(step9, c("Country_Code", "year", "ssp", "methodology",
                        "ann_deaths", "n_events"), "Step 9 country predictions")
require_columns(decomp, c("Year", "Scenario", "SSP", "Ann_deaths"),
                "Step 9 decomposition")

for (nm in c("prior", "step9")) {
  d <- get(nm)
  for (v in c("Country_Code", "year", "ssp", "methodology"))
    d[[v]] <- as.character(d[[v]])
  assign(nm, d)
}

stage_full <- " + Population (both) & HDI & coastal growth 2"
stage_with_hdi <- " + Population (both) & HDI"
stage_no_hdi <- " + Population (exposure + NB covariate)"
stage_slr <- " SLR only"

country <- prior
k_prior <- join_key(country)
k_s9 <- join_key(step9)
if (anyDuplicated(k_prior) || anyDuplicated(k_s9) || !setequal(k_prior, k_s9)) {
  stop("Country/scenario keys do not match across Step 9 and Mundlak projections")
}
step9 <- step9[match(k_prior, k_s9), , drop = FALSE]
if (!identical(k_prior, join_key(step9))) stop("Country/scenario matching failed")
err <- abs(as.numeric(country$NB_annual) - as.numeric(step9$ann_deaths))
tol <- 0.05 + 1e-7 * abs(as.numeric(step9$ann_deaths))
if (anyNA(err) || any(err > tol)) {
  stop("Mundlak comparison did not reproduce Step 9 NB predictions; maximum discrepancy: ",
       max(err, na.rm = TRUE))
}
if (!all(as.numeric(country$n_events) == as.numeric(step9$n_events))) {
  stop("Event-count mismatch")
}

country$seen_in_fitted_model <- as.logical(country$seen_in_fitted_model)
historical$seen_in_fitted_model <- as.logical(historical$seen_in_fitted_model)
if (anyNA(country$seen_in_fitted_model) ||
    anyNA(historical$seen_in_fitted_model)) stop("Invalid fitted-country indicator")

aggregate_models <- function(d, label) {
  x <- stats::aggregate(cbind(NB_annual, CRE_annual) ~
                          year + ssp + methodology, data = d, FUN = sum)
  x$population <- label
  sizes <- stats::aggregate(Country_Code ~ year + ssp + methodology,
                            data = d, FUN = length)
  x$n_countries <- sizes$Country_Code[match(
    paste(x$year, x$ssp, x$methodology),
    paste(sizes$year, sizes$ssp, sizes$methodology))]
  x
}

world <- aggregate_models(country, "all")
fitted <- aggregate_models(country[country$seen_in_fitted_model, , drop = FALSE], "fitted")
global <- rbind(world, fitted)
for (pop in c("all", "fitted")) {
  h <- if (pop == "all") historical else
    historical[historical$seen_in_fitted_model, , drop = FALSE]
  rows <- global$population == pop
  global$historical_NB_annual[rows] <- sum(h$NB_annual)
  global$historical_CRE_annual[rows] <- sum(h$CRE_annual)
}
global$CRE_minus_NB <- global$CRE_annual - global$NB_annual
global$CRE_minus_NB_pct <- 100 * global$CRE_minus_NB / global$NB_annual
global$NB_pct_from_historical <- 100 * (
  global$NB_annual / global$historical_NB_annual - 1)
global$CRE_pct_from_historical <- 100 * (
  global$CRE_annual / global$historical_CRE_annual - 1)

# Cross-check Step 9 global totals.
d <- decomp[!is.na(decomp$SSP) & decomp$SSP != "", , drop = FALSE]
k_d <- paste(d$Year, d$SSP, d$Scenario)
k_g <- paste(world$year, world$ssp, world$methodology)
idx <- match(k_d, k_g)
if (anyNA(idx) || any(abs(round(world$NB_annual[idx]) -
                            as.numeric(d$Ann_deaths)) > 1)) {
  stop("Step 9 decomposition does not agree with country totals")
}

historical_totals <- data.frame(
  Model = c("Original NB", "Mundlak CRE"),
  Annual_fitted_deaths = c(sum(historical$NB_annual), sum(historical$CRE_annual)),
  N_countries = nrow(historical)
)
historical_difference_pct <- 100 * (
  historical_totals$Annual_fitted_deaths[2] /
    historical_totals$Annual_fitted_deaths[1] - 1)

scenario_stats <- global[, c(
  "year", "ssp", "methodology", "population", "n_countries",
  "NB_annual", "CRE_annual", "CRE_minus_NB", "CRE_minus_NB_pct",
  "historical_NB_annual", "historical_CRE_annual",
  "NB_pct_from_historical", "CRE_pct_from_historical"
)]
scenario_stats <- scenario_stats[order(scenario_stats$population,
                                       scenario_stats$year, scenario_stats$ssp,
                                       scenario_stats$methodology), , drop = FALSE]

full_all <- scenario_stats[
  scenario_stats$methodology == stage_full & scenario_stats$population == "all",
  , drop = FALSE]
slr_all <- scenario_stats[
  scenario_stats$methodology == stage_slr & scenario_stats$population == "all",
  , drop = FALSE]
if (!nrow(full_all) || !nrow(slr_all)) stop("Missing full or SLR-only projections")

# For the HDI comparison under full coastal-growth-2 exposure, recover each
# country/scenario multiplier from the ratio of Step 9 stage 6 to stage 4.
# These differ only by the coastal-growth adjustment to the exposure offset.
key_ysc <- function(d) paste(d$year, d$ssp, d$Country_Code, sep = " | ")
full_c <- country[country$methodology == stage_full, , drop = FALSE]
hdi_c  <- country[country$methodology == stage_with_hdi, , drop = FALSE]
no_hdi <- country[country$methodology == stage_no_hdi, , drop = FALSE]
k_full <- key_ysc(full_c)
if (anyDuplicated(k_full) || anyDuplicated(key_ysc(hdi_c)) ||
    anyDuplicated(key_ysc(no_hdi))) stop("Duplicate country/year/scenario rows")
match_hdi <- match(k_full, key_ysc(hdi_c))
match_nohdi <- match(k_full, key_ysc(no_hdi))
if (anyNA(match_hdi) || anyNA(match_nohdi)) {
  stop("Missing Stage 3 or Stage 4 country projections")
}
hdi_c <- hdi_c[match_hdi, , drop = FALSE]
no_hdi <- no_hdi[match_nohdi, , drop = FALSE]
if (any(!is.finite(hdi_c$NB_annual)) || any(hdi_c$NB_annual <= 0)) {
  stop("Cannot infer coastal-growth offset multiplier from nonpositive NB predictions")
}
coastal_ratio <- full_c$NB_annual / hdi_c$NB_annual
cre_ratio <- full_c$CRE_annual / hdi_c$CRE_annual
if (anyNA(coastal_ratio) || any(!is.finite(coastal_ratio)) || any(coastal_ratio <= 0) ||
    anyNA(cre_ratio) || any(!is.finite(cre_ratio)) ||
    any(abs(coastal_ratio - cre_ratio) > 1e-5 * pmax(1, abs(coastal_ratio)))) {
  stop("NB and Mundlak coastal-growth multiplier ratios are inconsistent")
}
no_hdi$NB_no_future_HDI <- no_hdi$NB_annual * coastal_ratio
no_hdi$CRE_no_future_HDI <- no_hdi$CRE_annual * coastal_ratio
no_hdi_total <- stats::aggregate(cbind(NB_no_future_HDI, CRE_no_future_HDI) ~
                                   year + ssp, data = no_hdi, FUN = sum)
ih <- match(paste(full_all$year, full_all$ssp),
            paste(no_hdi_total$year, no_hdi_total$ssp))
if (anyNA(ih)) stop("Missing no-future-HDI scenarios")
hdi_stats <- data.frame(
  Year = full_all$year, SSP = full_all$ssp,
  NB_no_future_HDI = no_hdi_total$NB_no_future_HDI[ih],
  NB_with_future_HDI = full_all$NB_annual,
  CRE_no_future_HDI = no_hdi_total$CRE_no_future_HDI[ih],
  CRE_with_future_HDI = full_all$CRE_annual
)
hdi_stats$NB_difference <- hdi_stats$NB_no_future_HDI - hdi_stats$NB_with_future_HDI
hdi_stats$CRE_difference <- hdi_stats$CRE_no_future_HDI - hdi_stats$CRE_with_future_HDI
hdi_stats$NB_difference_pct <- 100 * hdi_stats$NB_difference / hdi_stats$NB_no_future_HDI
hdi_stats$CRE_difference_pct <- 100 * hdi_stats$CRE_difference / hdi_stats$CRE_no_future_HDI

full_country <- country[country$methodology == stage_full, , drop = FALSE]
country_groups <- split(full_country, paste(full_country$year, full_country$ssp))
rank_stats <- do.call(rbind, lapply(country_groups, function(g) {
  top_nb <- head(g$Country_Code[order(-g$NB_annual)], 10L)
  top_cre <- head(g$Country_Code[order(-g$CRE_annual)], 10L)
  data.frame(Year = unique(g$year), SSP = unique(g$ssp),
             N_countries = nrow(g),
             Spearman_rho = stats::cor(g$NB_annual, g$CRE_annual,
                                       method = "spearman"),
             Shared_top_10 = length(intersect(top_nb, top_cre)))
}))

model_stats <- data.frame(Statistic = character(), Value = numeric(),
                          stringsAsFactors = FALSE)
add_stat <- function(name, value) {
  model_stats <<- rbind(model_stats,
                       data.frame(Statistic = name, Value = as.numeric(value)))
}
add_stat("Historical_countries", nrow(historical))
add_stat("Historical_NB_annual", historical_totals$Annual_fitted_deaths[1])
add_stat("Historical_CRE_annual", historical_totals$Annual_fitted_deaths[2])
add_stat("Historical_CRE_minus_NB_pct", historical_difference_pct)
add_stat("Validated_projection_rows", nrow(country))
add_stat("Max_NB_projection_validation_error", max(err))

original_beta <- NA_real_
cre_beta <- NA_real_
if (file.exists(paths$hdi_coefficients)) {
  ctab <- read_csv(paths$hdi_coefficients)
  require_columns(ctab, c("model", "beta_HDI"), "HDI coefficient results")
  original_beta <- ctab$beta_HDI[match("Original NB", ctab$model)]
  cre_beta <- ctab$beta_HDI[match("Mundlak CRE NB", ctab$model)]
}

has_fits <- file.exists(paths$nb_model) && file.exists(paths$cre_model) &&
  requireNamespace("glmmTMB", quietly = TRUE)
if (has_fits) {
  nb_fit <- readRDS(paths$nb_model)
  cre_fit <- readRDS(paths$cre_model)
  if (stats::nobs(nb_fit) != stats::nobs(cre_fit)) {
    stop("Model observation counts differ")
  }
  nb_coeffs <- glmmTMB::fixef(nb_fit)$cond
  cre_coeffs <- glmmTMB::fixef(cre_fit)$cond
  if (!all(c("hdi") %in% names(nb_coeffs)) ||
      !all(c("hdi") %in% names(cre_coeffs))) stop("Missing HDI model coefficient")
  if (is.finite(original_beta) &&
      abs(original_beta - as.numeric(nb_coeffs[["hdi"]])) > 1e-5) {
    stop("Original NB HDI coefficient does not match saved CSV")
  }
  if (is.finite(cre_beta) &&
      abs(cre_beta - as.numeric(cre_coeffs[["hdi"]])) > 1e-5) {
    stop("CRE HDI coefficient does not match saved CSV")
  }
  original_beta <- as.numeric(nb_coeffs[["hdi"]])
  cre_beta <- as.numeric(cre_coeffs[["hdi"]])
  aic_nb <- stats::AIC(nb_fit)
  aic_cre <- stats::AIC(cre_fit)
  ll_nb <- stats::logLik(nb_fit)
  ll_cre <- stats::logLik(cre_fit)
  lrt_df <- attr(ll_cre, "df") - attr(ll_nb, "df")
  lrt_stat <- 2 * (as.numeric(ll_cre) - as.numeric(ll_nb))
  lrt_p <- if (lrt_df > 0) stats::pchisq(lrt_stat, df = lrt_df,
                                        lower.tail = FALSE) else NA_real_
  add_stat("N_model_observations", stats::nobs(nb_fit))
  add_stat("AIC_NB", aic_nb)
  add_stat("AIC_CRE", aic_cre)
  add_stat("Delta_AIC_CRE_minus_NB", aic_cre - aic_nb)
  add_stat("LogLik_NB", as.numeric(ll_nb))
  add_stat("LogLik_CRE", as.numeric(ll_cre))
  add_stat("LR_chisquare", lrt_stat)
  add_stat("LR_df", lrt_df)
  add_stat("LR_p", lrt_p)
  if (file.exists(paths$mean_mapping)) {
    mapping <- read_csv(paths$mean_mapping)
    require_columns(mapping, c("term", "original_covariate"), "Mundlak mapping")
    hdi_mean_term <- mapping$term[mapping$original_covariate == "hdi"]
    if (length(hdi_mean_term) != 1L ||
        !hdi_mean_term %in% names(cre_coeffs)) {
      stop("Cannot identify mean-HDI coefficient")
    }
    beta_between <- as.numeric(cre_coeffs[["hdi"]] + cre_coeffs[[hdi_mean_term]])
    v <- as.matrix(stats::vcov(cre_fit)$cond)
    se_between <- sqrt(v["hdi", "hdi"] +
                         v[hdi_mean_term, hdi_mean_term] +
                         2 * v["hdi", hdi_mean_term])
    add_stat("CRE_between_HDI_beta", beta_between)
    add_stat("CRE_between_HDI_SE", se_between)
    add_stat("CRE_between_HDI_95CI_lower", beta_between - 1.96 * se_between)
    add_stat("CRE_between_HDI_95CI_upper", beta_between + 1.96 * se_between)
  }
} else if (file.exists(paths$model_summary)) {
  z <- readLines(paths$model_summary, warn = FALSE)
  get_value <- function(prefix) {
    matched <- grep(paste0("^", prefix, ":"), z, value = TRUE)
    if (!length(matched)) return(NA_real_)
    suppressWarnings(as.numeric(trimws(sub("^[^:]+:", "", matched[[1]]))))
  }
  add_stat("LR_chisquare", get_value("Likelihood ratio statistic"))
  add_stat("LR_df", get_value("Likelihood ratio df"))
  add_stat("LR_p", get_value("Likelihood ratio p"))
}
if (length(original_beta) == 1L && is.finite(original_beta)) {
  add_stat("NB_HDI_beta", original_beta)
  add_stat("NB_IRR_per_0.1_HDI", exp(0.1 * original_beta))
}
if (length(cre_beta) == 1L && is.finite(cre_beta)) {
  add_stat("CRE_within_HDI_beta", cre_beta)
  add_stat("CRE_IRR_per_0.1_HDI", exp(0.1 * cre_beta))
}

hdi_extrapolation <- NULL
if (file.exists(paths$hdi_extrapolation)) {
  x <- read_csv(paths$hdi_extrapolation)
  require_columns(x, c("year", "ssp", "outside_country_historical_HDI_range"),
                  "HDI extrapolation")
  x$flag <- as.logical(x$outside_country_historical_HDI_range)
  hdi_extrapolation <- stats::aggregate(
    cbind(N_evaluated = as.integer(!is.na(x$flag)),
          N_outside = as.integer(!is.na(x$flag) & x$flag)) ~ year + ssp,
    data = x, FUN = sum)
  hdi_extrapolation$Percent_outside <- 100 *
    hdi_extrapolation$N_outside / hdi_extrapolation$N_evaluated
}

country_changes <- full_country
country_changes$CRE_minus_NB <- country_changes$CRE_annual -
  country_changes$NB_annual
country_changes$CRE_minus_NB_pct <- 100 * country_changes$CRE_minus_NB /
  country_changes$NB_annual
country_changes <- country_changes[order(country_changes$year, country_changes$ssp,
                                         -abs(country_changes$CRE_minus_NB)),
                                   c("year", "ssp", "Country_Code", "NB_annual",
                                     "CRE_annual", "CRE_minus_NB",
                                     "CRE_minus_NB_pct")]

# CSV outputs.
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv <- function(d, file) utils::write.csv(d, file.path(output_dir, file),
                                                 row.names = FALSE)
write_csv(model_stats, "01_model_statistics.csv")
write_csv(historical_totals, "02_historical_mortality.csv")
write_csv(scenario_stats, "03_scenario_comparisons_all_stages.csv")
write_csv(hdi_stats, "04_HDI_scenario_comparisons.csv")
write_csv(rank_stats, "05_country_rank_correlations.csv")
write_csv(country_changes, "06_country_projection_differences.csv")
if (!is.null(hdi_extrapolation)) {
  write_csv(hdi_extrapolation, "07_HDI_extrapolation_counts.csv")
}

# Text and console output.
lines <- character()
add <- function(...) lines <<- c(lines, ...)
add("MUNDLAK MODEL COMPARISON — STATISTICS", "")
add("MODEL STATISTICS")
for (i in seq_len(nrow(model_stats))) {
  add(sprintf("%-46s %s", model_stats$Statistic[i],
              format(model_stats$Value[i], scientific = FALSE, digits = 12,
                     trim = TRUE)))
}
add("", "HISTORICAL ANNUAL FITTED DEATHS")
add(paste(capture.output(print(historical_totals, row.names = FALSE)), collapse = "\n"))
add("", "SLR-ONLY AND FULLY LAYERED ANNUAL PROJECTIONS")
for (yr in c("2050", "2100")) {
  for (stg in c(stage_slr, stage_full)) {
    z <- scenario_stats[scenario_stats$population == "all" &
                          scenario_stats$year == yr &
                          scenario_stats$methodology == stg, , drop = FALSE]
    add(paste0("Year=", yr, " | Stage=", trimws(stg)))
    if (nrow(z)) {
      for (i in seq_len(nrow(z))) {
        add(sprintf("  %-13s NB=%12s  CRE=%12s  CRE-NB=%+8.2f%%",
                    z$ssp[i], fmt(z$NB_annual[i], 0), fmt(z$CRE_annual[i], 0),
                    z$CRE_minus_NB_pct[i]))
      }
    }
  }
}
add("", "HDI SCENARIO COMPARISONS")
for (i in seq_len(nrow(hdi_stats))) {
  z <- hdi_stats[i, ]
  add(sprintf("%s %-13s NB=%+8.2f%% (%s)  CRE=%+8.2f%% (%s)",
              z$Year, z$SSP, z$NB_difference_pct, fmt(z$NB_difference, 0),
              z$CRE_difference_pct, fmt(z$CRE_difference, 0)))
}
add("", "COUNTRY RANK CORRELATIONS")
for (i in seq_len(nrow(rank_stats))) {
  z <- rank_stats[i, ]
  add(sprintf("%s %-13s rho=%7.4f  common_top_10=%d  N=%d",
              z$Year, z$SSP, z$Spearman_rho,
              z$Shared_top_10, z$N_countries))
}
if (!is.null(hdi_extrapolation)) {
  add("", "HDI EXTRAPOLATION COUNTS")
  for (i in seq_len(nrow(hdi_extrapolation))) {
    z <- hdi_extrapolation[i, ]
    add(sprintf("%s %-13s %d/%d (%0.2f%%)",
                z$year, z$ssp, z$N_outside, z$N_evaluated,
                z$Percent_outside))
  }
}
add("", "LARGEST FIVE COUNTRY-LEVEL PROJECTION DIFFERENCES: SSP2.45")
for (yr in c("2050", "2100")) {
  z <- country_changes[country_changes$year == yr &
                         country_changes$ssp == "SSP2.45", , drop = FALSE]
  if (nrow(z)) {
    z <- head(z, 5L)
    for (i in seq_len(nrow(z))) {
      add(sprintf("%s %-5s NB=%12s  CRE=%12s  delta=%12s",
                  yr, z$Country_Code[i], fmt(z$NB_annual[i], 0),
                  fmt(z$CRE_annual[i], 0),
                  fmt(z$CRE_minus_NB[i], 0)))
    }
  }
}

writeLines(lines, file.path(output_dir, "Mundlak_statistics.txt"), useBytes = TRUE)
cat(paste(lines, collapse = "\n"), "\n", sep = "")
cat("\nOutput directory: ", output_dir, "\n", sep = "")
