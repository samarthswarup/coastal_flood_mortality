# Step 9a — 17th- and 83rd-percentile SLR mortality projections
#
# Recalculates Step 9's six-stage 2050/2100 global mortality decomposition
# using 17th- and 83rd-percentile SLR/exposure inputs rather than the median.
# Uses the same fitted Model 3, population/HDI SSP mapping, and coastal-growth
# adjustments as Step 9. Includes the seven scenarios used for SI Table S14.
# Writes decomposition-table point estimates and changes relative to the
# historical baseline. The percentile tables do not calculate confidence
# intervals; the CI columns are set to NA.
#
# Requires: Step 8 fitted model/data and Step 9 median decomposition table.
# Outputs: Step 9 directory, decomposition_table_population_methodology_p17_*
# and decomposition_table_population_methodology_p83_*.csv.
# Does not alter the median-SLR Step 9 results. The final formatted SI table
# layout is created separately (formatting script not included in this package).
# -----------------------------------------------------------------------------

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(splines2)
library(glmmTMB)

# Run from the project directory containing "Intermediate outputs".
PROJECT_DIR <- getwd()
if (!dir.exists(file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 7")) &&
    dir.exists(file.path(PROJECT_DIR, "Claude", "Intermediate outputs", "Output from Step 7"))) {
  PROJECT_DIR <- file.path(PROJECT_DIR, "Claude")
}
if (!dir.exists(file.path(PROJECT_DIR, "Intermediate outputs", "Output from Step 7"))) {
  stop("Run from project directory containing Intermediate outputs/Output from Step 7")
}
PROJECT_DIR <- normalizePath(PROJECT_DIR, mustWork = TRUE)
setwd(PROJECT_DIR)
# Resolve inputs in Input/ or Inputs/, in the project or its parent directory.
input_path <- function(relative_path) {
  roots <- c(PROJECT_DIR, dirname(PROJECT_DIR))
  candidates <- unlist(lapply(roots, function(root) {
    file.path(root, c("Input", "Inputs"), relative_path)
  }))
  found <- candidates[file.exists(candidates)]
  if (!length(found)) stop("Missing input: ", relative_path,
                           "; checked Input/ and Inputs/ under project and parent")
  found[[1L]]
}


dir.create("Intermediate outputs/Output from Step 9", showWarnings = FALSE, recursive = TRUE)

# Step 7 is not read by this script; the underlying processed events
# arrive in the Step 8 files below.
step8_date_append_output <- "2026-11-13"
step9_date               <- "2026-11-14"
date_append_output       <- "2026-11-14"  # paired with median Step 9 run

# Step 9 already applies the country-keyed coastal-growth multipliers.
# Read its median SLR decomposition directly.
step9_dir <- file.path("Intermediate outputs", "Output from Step 9")
p50_path <- file.path(step9_dir,
                       paste0("decomposition_table_population_methodology_", step9_date, ".csv"))
if (!file.exists(p50_path)) stop("Step 9 median decomposition not found: ", p50_path)
message("Median SLR benchmark: ", p50_path)

# =============================================================================
# SECTION I: LOAD STEP 8 DATA (same sources as Step 9)
# =============================================================================

predMod.3 <- readRDS(paste0("Intermediate outputs/Output from Step 8/predMod.3 ",
                             step8_date_append_output, ".RDS"))

trendData  <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

trendData$Country_Code <- as.character(trendData$Country_Code)

# Historical country baseline from the Step 8 country tables.
ci_base_path <- paste0("Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ",
                       step8_date_append_output, ".csv")
if (!file.exists(ci_base_path)) {
  available <- list.files("Intermediate outputs/Output from Step 8/Country tables/",
                          pattern = "^CountryDeathCIs")
  stop(
    "File not found: ", ci_base_path, "\n",
    "getwd() is currently: ", getwd(), "\n",
    "CountryDeathCIs files actually present in that folder:\n  ",
    paste(available, collapse = "\n  "),
    "\n-> Check step8_date_append_output above against the dates listed."
  )
}
ci_base <- read.csv(ci_base_path)

# Historical baseline should agree with the Step 9 p50 file.
p50_reference <- read.csv(p50_path, stringsAsFactors = FALSE)
reference_baseline <- unique(p50_reference$Ann_deaths[
  p50_reference$Scenario == "Historical baseline"])
if (length(reference_baseline) != 1L || !is.finite(reference_baseline)) {
  stop("Could not identify a unique historical baseline in Step 9 reference.")
}

N_YEARS <- 35

# =============================================================================
# SECTION II: COUNTRY CODE MAPPING (same methodology as Step 9)
# =============================================================================

country.data <- read.csv(input_path("Identify mismatched and missing countries - step 3/SLIIDERS country codes with names and territorial mapping.csv"))

hdr_code_map <- data.frame(
  Country_Code     = country.data[, 1],
  hdr.country.code = ifelse(
    !is.na(country.data[, 6]) & country.data[, 6] != "",
    country.data[, 6],
    country.data[, 1]
  ),
  stringsAsFactors = FALSE
)

trendData_codes <- data.frame(Country_Code = unique(trendData$Country_Code)) %>%
  left_join(hdr_code_map, by = "Country_Code") %>%
  dplyr::mutate(hdr.country.code = ifelse(is.na(hdr.country.code), Country_Code, hdr.country.code)) %>%
  dplyr::distinct(Country_Code, .keep_all = TRUE)

# =============================================================================
# SECTION IIb: COASTAL FLOODPLAIN GROWTH MULTIPLIERS (same as Step 9)
# =============================================================================

gni_lookup <- trendData %>%
  dplyr::select(Country_Code, gni.group) %>%
  dplyr::distinct(Country_Code, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(gni.group))

rentschler_mult <- c(LI=0.89, LMI=1.53, UMI=1.92, HI=0.79)

tellman_raw <- read.csv(input_path("Flood zone pop projection/Tellman_2021_S5_multipliers.csv"),
                        stringsAsFactors = FALSE)
tellman_lookup <- setNames(tellman_raw$tellman_mult_2000_2015, tellman_raw$Country_Code)

# Indexing by Country_Code must remain keyed by
# country name even after the GNI fallback is assigned. The prior code lost
# names on fallback assignments, so the downstream second name lookup could
# use NA and silently fall back to 1 instead of the intended multiplier.
# Country codes are preserved as explicit vector names.
build_country_mult <- function(country_codes, country_lookup, gni_fallback_vec) {
  country_codes <- as.character(country_codes)
  mult <- country_lookup[country_codes]
  missing <- is.na(mult)
  if (any(missing)) {
    gni <- gni_lookup$gni.group[match(country_codes[missing], gni_lookup$Country_Code)]
    mult[missing] <- unname(gni_fallback_vec[as.character(gni)])
  }
  mult[is.na(mult)] <- 1.0
  names(mult) <- country_codes  # CRITICAL: keep multiplier keyed by country
  mult
}

# Check that the country-specific multiplier vector is fully named and finite.
check_codes <- unique(as.character(trendData$Country_Code))
check_mult <- build_country_mult(check_codes, tellman_lookup, rentschler_mult)
if (!identical(names(check_mult), check_codes) ||
    anyNA(check_mult) || any(!is.finite(check_mult)) || any(check_mult <= 0)) {
  stop("Invalid or incorrectly keyed coastal-growth multipliers.")
}

# =============================================================================
# SECTION III: HDI AND POPULATION PROJECTIONS FROM LIU ET AL. (2024)
# =============================================================================

liu_raw <- read_excel(input_path("HDI projection/Liu_2024_HDI_pop_forecasts.xlsx"))

name_map <- country.data %>%
  dplyr::select(Country_Code = 1,
         name1        = 2,
         name2        = 3,
         name3        = 4) %>%
  tidyr::pivot_longer(cols = c(name1, name2, name3),
                      names_to  = "name_col",
                      values_to = "name") %>%
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
    "Uganda", "Zambia", "Zimbabwe"
  )),
  Country_Code = c(
    "ARM","AUT","AZE","BLR","BTN","BOL","BWA","BFA","BDI","CPV",
    "CAF","TCD","CIV","CZE","HUN","KAZ","KGZ","LAO","LSO","LUX",
    "MWI","MLI","MNG","NPL","NER","PRY","MDA","RWA","SRB","SVK",
    "SWZ","CHE","SYR","TJK","MKD","TKM","UGA","ZMB","ZWE"
  ),
  stringsAsFactors = FALSE
)

name_lookup <- bind_rows(name_map, manual_overrides) %>%
  dplyr::distinct(name, .keep_all = TRUE)

liu <- liu_raw %>%
  dplyr::mutate(name = tolower(trimws(Area))) %>%
  left_join(name_lookup, by = "name") %>%
  dplyr::filter(!is.na(Country_Code)) %>%
  dplyr::select(-name)

hdi_last <- trendData %>%
  dplyr::group_by(Country_Code) %>%
  dplyr::summarise(hdi_last = mean(hdi, na.rm = TRUE), .groups = "drop")

liu_by_sliiders <- trendData_codes %>%
  left_join(liu %>% dplyr::select(Country_Code,
                            hdi_SSP1_2050 = SSP1_2050, hdi_SSP1_2100 = SSP1_2100,
                            hdi_SSP2_2050 = SSP2_2050, hdi_SSP2_2100 = SSP2_2100,
                            hdi_SSP3_2050 = SSP3_2050, hdi_SSP3_2100 = SSP3_2100,
                            hdi_SSP4_2050 = SSP4_2050, hdi_SSP4_2100 = SSP4_2100,
                            hdi_SSP5_2050 = SSP5_2050, hdi_SSP5_2100 = SSP5_2100,
                            pop_SSP1_2050, pop_SSP1_2100,
                            pop_SSP2_2050, pop_SSP2_2100,
                            pop_SSP3_2050, pop_SSP3_2100,
                            pop_SSP4_2050, pop_SSP4_2100,
                            pop_SSP5_2050, pop_SSP5_2100),
            by = c("hdr.country.code" = "Country_Code")) %>%
  dplyr::distinct(Country_Code, .keep_all = TRUE) %>%
  left_join(hdi_last, by = "Country_Code")

liu_by_sliiders <- liu_by_sliiders %>%
  dplyr::mutate(
    hdi_2050 = ifelse(is.na(hdi_SSP2_2050), hdi_last, hdi_SSP2_2050),
    hdi_2100 = ifelse(is.na(hdi_SSP2_2100), hdi_last, hdi_SSP2_2100)
  )

hdi_trend <- liu_by_sliiders

build_trendData <- function(trendData, year_label) {
  predKeepNames <- names(trendData)[grepl("pop.exposed.local.else.adm1.else.national", names(trendData))]
  predKeepNames <- predKeepNames[grepl(".100yr", predKeepNames)]
  predKeepNames <- predKeepNames[!grepl("pooled", predKeepNames)]
  predKeepNames <- predKeepNames[grepl(year_label, predKeepNames)]
  predKeepNames <- predKeepNames[grepl("SSP1.26", predKeepNames) |
                                   grepl("SSP5.85", predKeepNames) |
                                   grepl("SSP1.19", predKeepNames) |
                                   grepl("SSP2.45", predKeepNames) |
                                   grepl("SSP3.70", predKeepNames)]

  keepVars <- c("total.deaths.na.as.0", "pop.total.adm0", "spline_year",
                "hdi", "Declaration", "no.events.5.years",
                "Disaster.Subtype", "Country_Code", predKeepNames)

  td <- trendData[, names(trendData) %in% keepVars]
  td$Country_Code <- as.character(td$Country_Code)
  ssp_cols   <- setdiff(names(td), c("total.deaths.na.as.0", "pop.total.adm0",
                                     "spline_year", "hdi", "Declaration",
                                     "no.events.5.years", "Disaster.Subtype", "Country_Code"))
  fixed_cols <- setdiff(names(td), ssp_cols)
  td <- td[, c(fixed_cols, ssp_cols)]

  list(data = td, ssp_cols = ssp_cols, fixed_cols = fixed_cols,
       ssp_start = length(fixed_cols) + 1)
}

td2050 <- build_trendData(trendData, "2050")
td2100 <- build_trendData(trendData, "2100")

# =============================================================================
# SECTION IV: PARAMETERIZED PROJECTION FUNCTION (POINT ESTIMATES)
# =============================================================================
# Adapted from Step 9's projection function. The `perc` argument selects
# the SLR/exposure percentile; `skip_bootstrap = TRUE` returns point estimates
# without bootstrap confidence intervals.

run_projections_perc <- function(td_obj, hdi_proj_col, vary_hdi, vary_pop,
                                 pop_proj_df, label, slr_pattern = "med_SSP2.45",
                                 use_exposure_popscale = FALSE,
                                 exposure_popscale_suffix = NULL,
                                 coastal_mult_vec = NULL,
                                 perc = "50",
                                 skip_bootstrap = TRUE) {

  td       <- td_obj$data
  ssp_cols <- td_obj$ssp_cols
  ssp_start <- td_obj$ssp_start

  if (use_exposure_popscale && is.null(exposure_popscale_suffix)) {
    stop("use_exposure_popscale = TRUE requires exposure_popscale_suffix to be set.")
  }

  if (vary_hdi) {
    hdi_lookup <- hdi_trend[, c("Country_Code", hdi_proj_col)]
    names(hdi_lookup)[2] <- "hdi_proj"
    td <- left_join(td, hdi_lookup, by = "Country_Code")
    td$hdi[!is.na(td$hdi_proj)] <- td$hdi_proj[!is.na(td$hdi_proj)]
    td$hdi_proj <- NULL
  }

  if (vary_pop) {
    pop_lookup <- pop_proj_df
    names(pop_lookup)[2] <- "pop_proj"
    td <- left_join(td, pop_lookup, by = "Country_Code")
    td$pop.total.adm0[!is.na(td$pop_proj)] <- td$pop_proj[!is.na(td$pop_proj)]
    td$pop_proj <- NULL
  }

  if (!is.null(coastal_mult_vec)) {
    td$coastal_mult <- coastal_mult_vec[td$Country_Code]
    td$coastal_mult[is.na(td$coastal_mult)] <- 1.0
  }

  total_deaths <- 0
  n_events     <- 0

  # Match the requested SLR percentile in the input column name.
  candidate_cols <- ssp_cols[grepl(slr_pattern, ssp_cols) & grepl(paste0("\\.", perc, "\\."), ssp_cols)]

  if (use_exposure_popscale) {
    ssp_col_name <- candidate_cols[grepl(paste0("\\.popscale\\.", exposure_popscale_suffix, "$"),
                                         candidate_cols)]
  } else {
    ssp_col_name <- candidate_cols[!grepl("popscale", candidate_cols)]
  }

  if (length(ssp_col_name) == 0) {
    stop("No exposure column matched for label '", label, "' (slr_pattern='",
         slr_pattern, "', perc='", perc, "', use_exposure_popscale=", use_exposure_popscale,
         ", suffix='", exposure_popscale_suffix, "').")
  }
  if (length(ssp_col_name) > 1) {
    stop("Ambiguous exposure column match for label '", label, "': ",
         paste(ssp_col_name, collapse = ", "))
  }

  cat("  [", label, ", p", perc, "] starting...\n"); flush(stdout())

  # ===========================================================================
  # PASS 1: per-country point predictions
  # ===========================================================================
  country_point_info <- list()

  for (i in 1:length(unique(td$Country_Code))) {

    this_country <- unique(td$Country_Code)[i]
    keep_cols <- c(td_obj$fixed_cols, ssp_col_name,
                   if (!is.null(coastal_mult_vec)) "coastal_mult")
    current <- td[td$Country_Code == this_country, keep_cols]
    names(current)[length(td_obj$fixed_cols) + 1] <- "expPop100"

    if (!is.null(coastal_mult_vec)) {
      current$expPop100 <- current$expPop100 * current$coastal_mult
      current$coastal_mult <- NULL
    }

    current$log_expPop100 <- log(current$expPop100)

    thisPred <- predict(predMod.3, newdata = current, type = "response",
                        allow.new.levels = TRUE)
    valid <- !is.na(thisPred)
    thisPred_valid <- thisPred[valid]

    total_deaths <- total_deaths + sum(thisPred_valid)
    n_events     <- n_events + length(thisPred_valid)

    if (length(thisPred_valid) > 0) {
      mean_deaths   <- mean(thisPred_valid)
      country_point_info[[this_country]] <- list(
        ann_deaths_this = mean_deaths / N_YEARS * length(thisPred_valid),
        n_valid         = length(thisPred_valid)
      )
    }
  }

  ann_deaths <- total_deaths / N_YEARS

  cat(label, "(p", perc, "): ann deaths =", round(ann_deaths),
      "(", n_events, "events )\n")
  flush(stdout())

  if (skip_bootstrap) {
    country_results <- do.call(rbind, lapply(names(country_point_info), function(cc) {
      data.frame(Country_Code = cc,
                 ann_deaths = country_point_info[[cc]]$ann_deaths_this,
                 ann_deaths_lcl = NA_real_, ann_deaths_ucl = NA_real_,
                 n_events = country_point_info[[cc]]$n_valid)
    }))
    return(list(ann_deaths = ann_deaths, country_results = country_results,
                global_lcl = NA_real_, global_ucl = NA_real_))
  }

  stop("skip_bootstrap = FALSE not implemented in this sensitivity script -- ",
       "Use Step 9 for bootstrap confidence intervals.")
}

# =============================================================================
# SECTION V: SSP PAIRINGS AND PROJECTION-STAGE SPECIFICATIONS
# =============================================================================

ssp_pairings <- list(
  list(label="SSP1.19", slr="med_SSP1.19", liu_hdi_2050="hdi_SSP1_2050", liu_hdi_2100="hdi_SSP1_2100",
       liu_pop_2050="pop_SSP1_2050", liu_pop_2100="pop_SSP1_2100", popscale_suffix="SSP1"),
  list(label="SSP1.26_med", slr="med_SSP1.26", liu_hdi_2050="hdi_SSP1_2050", liu_hdi_2100="hdi_SSP1_2100",
       liu_pop_2050="pop_SSP1_2050", liu_pop_2100="pop_SSP1_2100", popscale_suffix="SSP1"),
  list(label="SSP2.45", slr="med_SSP2.45", liu_hdi_2050="hdi_SSP2_2050", liu_hdi_2100="hdi_SSP2_2100",
       liu_pop_2050="pop_SSP2_2050", liu_pop_2100="pop_SSP2_2100", popscale_suffix="SSP2"),
  list(label="SSP3.70", slr="med_SSP3.70", liu_hdi_2050="hdi_SSP3_2050", liu_hdi_2100="hdi_SSP3_2100",
       liu_pop_2050="pop_SSP3_2050", liu_pop_2100="pop_SSP3_2100", popscale_suffix="SSP3"),
  list(label="SSP5.85_med", slr="med_SSP5.85", liu_hdi_2050="hdi_SSP5_2050", liu_hdi_2100="hdi_SSP5_2100",
       liu_pop_2050="pop_SSP5_2050", liu_pop_2100="pop_SSP5_2100", popscale_suffix="SSP5"),
  # Seven selected scenarios are retained here; the additional forcing
  # variants calculated by Step 9 are not included in these percentile tables.
  list(label="BOUND_worst", slr="low_SSP5.85", liu_hdi_2050="hdi_SSP3_2050", liu_hdi_2100="hdi_SSP3_2100",
       liu_pop_2050="pop_SSP3_2050", liu_pop_2100="pop_SSP3_2100", popscale_suffix="BOUND_worst"),
  list(label="BOUND_best",  slr="med_SSP1.19", liu_hdi_2050="hdi_SSP5_2050", liu_hdi_2100="hdi_SSP5_2100",
       liu_pop_2050="pop_SSP5_2050", liu_pop_2100="pop_SSP5_2100", popscale_suffix="BOUND_best")
)

row_specs <- list(
  list(name = " SLR only",
       vary_hdi = FALSE, vary_pop = FALSE, use_exposure_popscale = FALSE,
       coastal_mult_vec = NULL),
  list(name = " + Population (exposure only)",
       vary_hdi = FALSE, vary_pop = FALSE, use_exposure_popscale = TRUE,
       coastal_mult_vec = NULL),
  list(name = " + Population (exposure + NB covariate)",
       vary_hdi = FALSE, vary_pop = TRUE, use_exposure_popscale = TRUE,
       coastal_mult_vec = NULL),
  list(name = " + Population (both) & HDI",
       vary_hdi = TRUE, vary_pop = TRUE, use_exposure_popscale = TRUE,
       coastal_mult_vec = NULL),
  list(name = " + Population (both) & HDI & coastal growth 1",
       vary_hdi = TRUE, vary_pop = TRUE, use_exposure_popscale = TRUE,
       coastal_mult_vec = "rentschler"),
  list(name = " + Population (both) & HDI & coastal growth 2",
       vary_hdi = TRUE, vary_pop = TRUE, use_exposure_popscale = TRUE,
       coastal_mult_vec = "tellman")
)

hist_ann_deaths <- sum(ci_base$meanDeaths * ci_base$nEvents, na.rm = TRUE) / N_YEARS
if (abs(round(hist_ann_deaths) - reference_baseline) > 1) {
  stop("Historical baseline does not match Step 9 p50 output: ",
       round(hist_ann_deaths), " vs ", reference_baseline,
       ". Check Step 8 / Step 9 run compatibility.")
}

# =============================================================================
# SECTION VI: MAIN LOOP -- 17th and 83rd percentiles only (50th already
# already exists as Step 9 median decomp_table; not recomputed here)
# =============================================================================

for (perc_target in c("17", "83")) {

  cat("\n\n############################################\n")
  cat("### PERCENTILE:", perc_target, "###\n")
  cat("############################################\n\n")

  global_summary_rows <- list()

  for (yr in c("2050", "2100")) {

    td_obj <- if (yr == "2050") td2050 else td2100
    cat("\n=== SSP-CONSISTENT PROJECTIONS (p", perc_target, "):", yr, "===\n")

    for (sp in ssp_pairings) {

      hdi_col <- if (yr == "2050") sp$liu_hdi_2050 else sp$liu_hdi_2100
      pop_col <- if (yr == "2050") sp$liu_pop_2050  else sp$liu_pop_2100

      pop_df <- hdi_trend %>%
        dplyr::mutate(pop_proj = .data[[pop_col]] * 1000) %>%
        dplyr::select(Country_Code, pop_proj) %>%
        dplyr::filter(!is.na(pop_proj))

      for (rs in row_specs) {

        mult_vec <- NULL
        if (identical(rs$coastal_mult_vec, "rentschler")) {
          country_codes_here <- unique(td_obj$data$Country_Code)
          gni <- gni_lookup$gni.group[match(country_codes_here, gni_lookup$Country_Code)]
          mult_vec <- rentschler_mult[as.character(gni)]
          names(mult_vec) <- country_codes_here
          mult_vec[is.na(mult_vec)] <- 1.0
        } else if (identical(rs$coastal_mult_vec, "tellman")) {
          mult_vec <- build_country_mult(unique(td_obj$data$Country_Code),
                                         tellman_lookup, rentschler_mult)
        }

        result <- run_projections_perc(
          td_obj, hdi_col, vary_hdi = rs$vary_hdi, vary_pop = rs$vary_pop,
          pop_proj_df = if (rs$vary_pop) pop_df else NULL,
          label = paste(yr, sp$label, trimws(rs$name)),
          slr_pattern = sp$slr,
          use_exposure_popscale = rs$use_exposure_popscale,
          exposure_popscale_suffix = sp$popscale_suffix,
          coastal_mult_vec = mult_vec,
          perc = perc_target,
          skip_bootstrap = TRUE
        )

        global_summary_rows[[length(global_summary_rows) + 1]] <- data.frame(
          year = yr, ssp = sp$label, methodology = trimws(rs$name),
          Ann_deaths = result$ann_deaths
        )
      }
    }
  }

  global_summary <- do.call(rbind, global_summary_rows)

  # Build decomposition table with Step 9 column names (CI
  # columns, which are NA throughout since bootstrap was skipped)
  decomp_rows <- list(
    data.frame(Year="2050", Scenario="Historical baseline",
               SSP=NA_character_, Ann_deaths=round(hist_ann_deaths),
               Ann_deaths_lcl=NA_real_, Ann_deaths_ucl=NA_real_,
               Pct_vs_historical=0, Deaths_averted_vs_SLR=NA_real_)
  )

  for (yr in c("2050", "2100")) {
    sub <- global_summary[global_summary$year == yr, ]
    for (this_ssp in unique(sub$ssp)) {
      ssp_sub <- sub[sub$ssp == this_ssp, ]
      slr_only_val <- ssp_sub$Ann_deaths[ssp_sub$methodology == "SLR only"]
      for (rs in row_specs) {
        rs_name <- trimws(rs$name)
        this_row <- ssp_sub[ssp_sub$methodology == rs_name, ]
        decomp_rows[[length(decomp_rows)+1]] <- data.frame(
          Year                  = yr,
          Scenario              = paste0(" ", rs_name),  # leading space, matches Step 9's own convention
          SSP                   = this_ssp,
          Ann_deaths            = round(this_row$Ann_deaths),
          Ann_deaths_lcl        = NA_real_,
          Ann_deaths_ucl        = NA_real_,
          Pct_vs_historical     = round(100*(this_row$Ann_deaths - hist_ann_deaths)/hist_ann_deaths, 1),
          Deaths_averted_vs_SLR = round(slr_only_val - this_row$Ann_deaths)
        )
      }
    }
    if (yr == "2050") {
      decomp_rows[[length(decomp_rows)+1]] <- data.frame(
        Year="2100", Scenario="Historical baseline",
        SSP=NA_character_, Ann_deaths=round(hist_ann_deaths),
        Ann_deaths_lcl=NA_real_, Ann_deaths_ucl=NA_real_,
        Pct_vs_historical=0, Deaths_averted_vs_SLR=NA_real_)
    }
  }
  decomp_table_perc <- do.call(rbind, decomp_rows)

  out_path <- paste0("Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_p",
                     perc_target, "_", date_append_output, ".csv")
  write.csv(decomp_table_perc, out_path, row.names = FALSE)
  cat("\nWritten:", out_path, "\n")
}

cat("\nDone. Point-estimate-only decomposition tables written for p17 and p83.\n")
cat("Step 9 (p50) decomposition table is unchanged -- use its direct output for Table S14.1.\n")
cat("Output date tag: ", date_append_output, "\n", sep = "")
