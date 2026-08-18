##########################################
# Step 9 - Projections with time-varying  #
# HDI and national population             #
#                                         #
# 2026-07-20                              #
# Ryan Calder                             #
# Notes added by Claude                   #
##########################################

# This script addresses reviewer comment asking whether future changes in HDI
# and national population should be incorporated into mortality projections.
# It replicates Step 8's trendData2050/2100 approach exactly, then additionally
# swaps in SSP2-consistent HDI and population projections from Liu et al. (2024)
# before predicting. Territories inherit their parent country's projection via
# the SLIIDERS territorial mapping file used in Step 7.

rm(list=ls())

library(readxl)
library(dplyr)
library(tidyr)
library(splines2)
library(glmmTMB)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Intermediate outputs/Output from Step 9", showWarnings = FALSE, recursive = TRUE)

step7_date_append_output = "2026-07-05"
step8_date_append_output = "2026-07-20"
date_append_output       = "2026-07-20"

# =============================================================================
# SECTION I: QUICK LOAD
# =============================================================================

predMod.3 <- readRDS(paste0("Intermediate outputs/Output from Step 8/predMod.3 ",
                             step8_date_append_output, ".RDS"))

trendData  <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

# Country_Code loads as a factor. Factors used as list[[ ]] names or as
# indices into named vectors (both used extensively below: e.g.
# country_point_info[[this_country]], coastal_mult_vec[td$Country_Code],
# tellman_lookup[country_codes]) silently fall back to POSITIONAL indexing
# on the factor's integer level code instead of name-based matching -- no
# error, just wrong or empty results. Coercing once here, before anything
# derives from trendData, means every downstream object (trendData_codes,
# gni_lookup, hdi_trend, td2050/td2100 via build_trendData) inherits a
# clean character column, rather than depending on incidental coercion
# from a later left_join to mask the issue for some rows but not others.
trendData$Country_Code <- as.character(trendData$Country_Code)

ci_base    <- read.csv(paste0("Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ",
                               step8_date_append_output, ".csv"))

N_YEARS <- 35
B <- 5           # unused by this script; the active bootstrap replicate
                 # count is B_GLOBAL below.
B_GLOBAL <- 300  # bootstrap replicates for country- AND global-level CIs.
                 # Global sums average out a lot of country-level noise, so
                 # resolving a global CI needs more replicates than a single
                 # country's CI would. Because the global-level CI requires
                 # a *shared* set of replicate draws across all countries
                 # (see below), the same B_GLOBAL draws are reused for the
                 # country-level CI too, rather than running two separate
                 # bootstraps.

# =============================================================================
# SECTION II: COUNTRY CODE MAPPING (mirrors Step 7)
# =============================================================================

# Load the same territorial mapping used in Step 7 to map SLIIDERS country
# codes to HDR country codes (e.g. GLP -> FRA, PRI -> USA)
country.data <- read.csv(paste0(
  "Inputs/Identify mismatched and missing countries - step 3/",
  "SLIIDERS country codes with names and territorial mapping.csv"))

# Build SLIIDERS -> HDR mapping: territories map to their parent country
hdr_code_map <- data.frame(
  Country_Code     = country.data[, 1],
  hdr.country.code = ifelse(
    !is.na(country.data[, 6]) & country.data[, 6] != "",
    country.data[, 6],
    country.data[, 1]
  ),
  stringsAsFactors = FALSE
)

# Add mapping to trendData countries — deduplicate in case of multiple rows
# per SLIIDERS code (e.g. ESP appears for both Spain and Canary Islands)
trendData_codes <- data.frame(Country_Code = unique(trendData$Country_Code)) %>%
  left_join(hdr_code_map, by = "Country_Code") %>%
  dplyr::mutate(hdr.country.code = ifelse(is.na(hdr.country.code), Country_Code, hdr.country.code)) %>%
  dplyr::distinct(Country_Code, .keep_all = TRUE)

cat("Territories mapped to parent for HDI/pop lookup:\n")
mapped <- trendData_codes[trendData_codes$Country_Code != trendData_codes$hdr.country.code, ]
print(mapped)

# =============================================================================
# SECTION IIb: COASTAL FLOODPLAIN GROWTH MULTIPLIERS (ported from Step 10)
# =============================================================================
#
# Addresses the reviewer comment (Rentschler et al. 2023) that population in
# high-risk coastal flood zones may grow faster than national averages. Rows
# 5/6 below apply these multipliers to the popscale-adjusted exposure term
# in Row 4 ("+ Population (both) & HDI"), asking: what if coastal floodplain
# population grows faster/slower than the national rate SLIIDERS assumes?
#
# Growth 1 — Rentschler et al. (2023, Nature): income-group ratios of
#   high-risk coastal settlement growth relative to overall settlement
#   growth (1985-2015). Area-based, not population-based; income-group
#   rather than country-level.
# Growth 2 — Tellman et al. (2021, Nature): country-level observed ratio of
#   flood-exposed population growth to total population growth (2000-2015).
#   Falls back to Rentschler GNI-group value for countries flagged
#   "uncertain" or not covered.

gni_lookup <- trendData %>%
  dplyr::select(Country_Code, gni.group) %>%
  dplyr::distinct(Country_Code, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(gni.group))

rentschler_mult <- c(LI=0.89, LMI=1.53, UMI=1.92, HI=0.79)

tellman_raw <- read.csv("Inputs/Flood zone pop projection/Tellman_2021_S5_multipliers.csv",
                        stringsAsFactors = FALSE)
tellman_lookup <- setNames(tellman_raw$tellman_mult_2000_2015, tellman_raw$Country_Code)

cat("Tellman coverage:", sum(unique(trendData$Country_Code) %in% names(tellman_lookup)),
    "of", length(unique(trendData$Country_Code)), "trendData countries\n")

## Build a single per-country multiplier vector for each growth methodology,
## with fallback to Rentschler GNI-group value where a country-level
## multiplier is unavailable (mirrors Step 10's logic exactly).
build_country_mult <- function(country_codes, country_lookup, gni_fallback_vec) {
  mult <- country_lookup[country_codes]
  missing <- is.na(mult)
  if (any(missing)) {
    gni <- gni_lookup$gni.group[match(country_codes[missing], gni_lookup$Country_Code)]
    mult[missing] <- gni_fallback_vec[as.character(gni)]
  }
  mult[is.na(mult)] <- 1.0  # final fallback: no adjustment
  mult
}

# =============================================================================
# SECTION III: HDI AND POPULATION PROJECTIONS FROM LIU ET AL. (2024)
# =============================================================================

# Read Liu et al. (2024) SSP-consistent HDI and population projections
# Population is in thousands; HDI is on [0,1] scale
# Source: "Inputs/HDI projection/Liu_2024_HDI_pop_forecasts.xlsx"
liu_raw <- read_excel("Inputs/HDI projection/Liu_2024_HDI_pop_forecasts.xlsx")

# Match Liu country names to SLIIDERS ISO codes using the territorial mapping
# file plus manual overrides for variant country names in the Liu dataset
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

# Manual overrides for Liu name variants not in the mapping file
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
  # For territories, map to parent country via hdr_code_map
  # (territories share their parent's HDI/pop projection)
  dplyr::select(-name)

cat("Liu 2024 countries matched:", nrow(liu), "of", nrow(liu_raw), "\n")

# Build hdi_trend table: use SSP2 as primary, fall back to last observed HDI
hdi_last <- trendData %>%
  dplyr::group_by(Country_Code) %>%
  dplyr::summarise(hdi_last = mean(hdi, na.rm = TRUE), .groups = "drop")

# For territories (e.g. GLP->FRA), inherit parent country's Liu projection
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

# Fall back to last observed HDI for countries not in Liu dataset
liu_by_sliiders <- liu_by_sliiders %>%
  dplyr::mutate(
    hdi_2050 = ifelse(is.na(hdi_SSP2_2050), hdi_last, hdi_SSP2_2050),
    hdi_2100 = ifelse(is.na(hdi_SSP2_2100), hdi_last, hdi_SSP2_2100)
  )

hdi_trend <- liu_by_sliiders

cat("\nHDI projections (SSP2):\n")
cat("Mean current HDI: ", round(mean(hdi_trend$hdi_last,  na.rm=T), 3), "\n")
cat("Mean HDI 2050:    ", round(mean(hdi_trend$hdi_2050,  na.rm=T), 3), "\n")
cat("Mean HDI 2100:    ", round(mean(hdi_trend$hdi_2100,  na.rm=T), 3), "\n")
cat("Countries using Liu data:", sum(!is.na(hdi_trend$hdi_SSP2_2050)), "\n")
cat("Countries using fallback:", sum(is.na(hdi_trend$hdi_SSP2_2050)), "\n")

# Population projections from Liu (thousands -> raw persons)
pop_2050 <- hdi_trend %>%
  dplyr::mutate(pop_2050 = pop_SSP2_2050 * 1000) %>%
  dplyr::select(Country_Code, pop_2050) %>%
  dplyr::filter(!is.na(pop_2050))

pop_2100_est <- hdi_trend %>%
  dplyr::mutate(pop_2100 = pop_SSP2_2100 * 1000) %>%
  dplyr::select(Country_Code, pop_2100) %>%
  dplyr::filter(!is.na(pop_2100))

cat("\nPopulation coverage at 2050:", nrow(pop_2050), "of",
    length(unique(trendData$Country_Code)), "countries\n")
cat("Population coverage at 2100:", nrow(pop_2100_est), "of",
    length(unique(trendData$Country_Code)), "countries\n")



build_trendData <- function(trendData, year_label) {
  
  predKeepNames <- names(trendData)[grepl("pop.exposed.local.else.adm1.else.national", names(trendData))]
  predKeepNames <- predKeepNames[grepl(".100yr", predKeepNames)]
  predKeepNames <- predKeepNames[!grepl("pooled", predKeepNames)]
  predKeepNames <- predKeepNames[grepl(year_label, predKeepNames)]
  ## Both geometry-only and popscale-adjusted columns are retained here, so
  ## that run_projections() can select either one via its
  ## use_exposure_popscale argument. run_projections()'s inner selection
  ## logic uses an exact match (not a substring match) to avoid ambiguity
  ## between geometry-only and popscale-adjusted columns that share the
  ## same slr_pattern substring.
  predKeepNames <- predKeepNames[grepl("SSP1.26", predKeepNames) |
                                   grepl("SSP5.85", predKeepNames) |
                                   grepl("SSP1.19", predKeepNames) |
                                   grepl("SSP2.45", predKeepNames) |
                                   grepl("SSP3.70", predKeepNames)]
  
  keepVars <- c("total.deaths.na.as.0", "pop.total.adm0", "spline_year",
                "hdi", "Declaration", "no.events.5.years",
                "Disaster.Subtype", "Country_Code", "Start.Year", predKeepNames)

  td <- trendData[, names(trendData) %in% keepVars]
  # Country_Code arrives as a factor (see trendData). Factors are NOT
  # character vectors from R's point of view, so using one as a list[[ ]]
  # name or as an index into a named vector (both used heavily downstream:
  # country_point_info[[this_country]], coastal_mult_vec[td$Country_Code],
  # tellman_lookup[country_codes], etc.) silently falls back to POSITIONAL
  # indexing on the factor's integer level code, not name-based matching.
  # That doesn't error -- it just quietly returns wrong/empty results, which
  # is exactly what produced "bootstrapping 0 countries" for the two rows
  # that never happened to pass Country_Code through a left_join (joins
  # incidentally coerce it to character, masking the bug for other rows).
  # Coercing once here, at the single source of td2050/td2100, guarantees
  # every downstream list-key and named-vector lookup is character-based.
  td$Country_Code <- as.character(td$Country_Code)
  # Start.Year is carried through here because PASS 2 below block-bootstraps
  # by year rather than by individual event. It must be excluded from
  # ssp_cols here, same as the other fixed columns, or it would be
  # miscategorized as an "ssp_col" (anything not in this exclusion list
  # gets swept into ssp_cols, which is meant to hold only the SSP-specific
  # exposure columns).
  ssp_cols   <- setdiff(names(td), c("total.deaths.na.as.0", "pop.total.adm0",
                                     "spline_year", "hdi", "Declaration",
                                     "no.events.5.years", "Disaster.Subtype",
                                     "Country_Code", "Start.Year"))
  fixed_cols <- setdiff(names(td), ssp_cols)
  td <- td[, c(fixed_cols, ssp_cols)]
  
  list(data = td, ssp_cols = ssp_cols, fixed_cols = fixed_cols,
       ssp_start = length(fixed_cols) + 1)
}

td2050 <- build_trendData(trendData, "2050")
td2100 <- build_trendData(trendData, "2100")

# =============================================================================
# SECTION IV: RUN PROJECTIONS
# Replicates Step 8 loop exactly, with optional HDI substitution
# =============================================================================

run_projections <- function(td_obj, hdi_proj_col, vary_hdi, vary_pop,
                            pop_proj_df, label, slr_pattern = "med_SSP2.45",
                            use_exposure_popscale = FALSE,
                            exposure_popscale_suffix = NULL,
                            coastal_mult_vec = NULL) {
  
  td       <- td_obj$data
  ssp_cols <- td_obj$ssp_cols
  ssp_start <- td_obj$ssp_start
  
  if (use_exposure_popscale && is.null(exposure_popscale_suffix)) {
    stop("use_exposure_popscale = TRUE requires exposure_popscale_suffix to be set.")
  }
  
  # Optionally substitute projected HDI
  if (vary_hdi) {
    hdi_lookup <- hdi_trend[, c("Country_Code", hdi_proj_col)]
    names(hdi_lookup)[2] <- "hdi_proj"
    td <- left_join(td, hdi_lookup, by = "Country_Code")
    td$hdi[!is.na(td$hdi_proj)] <- td$hdi_proj[!is.na(td$hdi_proj)]
    td$hdi_proj <- NULL
  }
  
  # Optionally substitute projected national population (regression covariate)
  if (vary_pop) {
    pop_lookup <- pop_proj_df
    names(pop_lookup)[2] <- "pop_proj"
    td <- left_join(td, pop_lookup, by = "Country_Code")
    td$pop.total.adm0[!is.na(td$pop_proj)] <- td$pop_proj[!is.na(td$pop_proj)]
    td$pop_proj <- NULL
  }

  # Optionally apply a coastal-vs-national floodplain growth multiplier to
  # the exposure term (Rentschler/Tellman-style adjustment). Applied per
  # event row via Country_Code match; NOT applied to pop.total.adm0.
  if (!is.null(coastal_mult_vec)) {
    td$coastal_mult <- coastal_mult_vec[td$Country_Code]
    td$coastal_mult[is.na(td$coastal_mult)] <- 1.0
  }
  
  total_deaths <- 0
  n_events     <- 0
  
  ## Exact-match selection of the single exposure column to use, rather
  ## than looping over every column matching slr_pattern. This is required
  ## now that both geometry-only and popscale-adjusted columns coexist in
  ## `td` -- a fuzzy match would ambiguously grab both and blend results.
  candidate_cols <- ssp_cols[grepl(slr_pattern, ssp_cols) & grepl("\\.50\\.", ssp_cols)]
  
  if (use_exposure_popscale) {
    ssp_col_name <- candidate_cols[grepl(paste0("\\.popscale\\.", exposure_popscale_suffix, "$"),
                                         candidate_cols)]
  } else {
    ssp_col_name <- candidate_cols[!grepl("popscale", candidate_cols)]
  }
  
  if (length(ssp_col_name) == 0) {
    stop("No exposure column matched for label '", label, "' (slr_pattern='",
         slr_pattern, "', use_exposure_popscale=", use_exposure_popscale,
         ", suffix='", exposure_popscale_suffix, "').")
  }
  if (length(ssp_col_name) > 1) {
    stop("Ambiguous exposure column match for label '", label, "': ",
         paste(ssp_col_name, collapse = ", "))
  }

  cat("  [", label, "] starting...\n"); flush(stdout())

  # ===========================================================================
  # PASS 1: per-country point predictions (fast -- one predict() call per
  # country, ~150 calls total). Builds and stores each country's prepped
  # `current` data frame for reuse in Pass 2, so it isn't rebuilt again.
  # ===========================================================================
  country_current_list <- list()
  country_point_info   <- list()

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
      mean_exposure <- mean(current$expPop100[valid])
      country_current_list[[this_country]] <- current
      country_point_info[[this_country]] <- list(
        mean_deaths     = mean_deaths,
        mort_fraction   = mean_deaths / mean_exposure,
        ann_deaths_this = mean_deaths / N_YEARS * length(thisPred_valid),
        n_valid         = length(thisPred_valid),
        nrow_current    = nrow(current)
      )
    }
  }

  ann_deaths <- total_deaths / N_YEARS

  # ===========================================================================
  # PASS 2: batched bootstrap for country- and global-level CI.
  #
  # Countries with >1 event get resampled and predicted together in ONE
  # combined predict() call per replicate, rather than one predict() call
  # per country per replicate. This cuts predict() calls from roughly
  # (n_countries x B_GLOBAL) down to just B_GLOBAL -- e.g. ~45,000 calls
  # down to ~300 for a single row/scenario -- since glmmTMB's predict() has
  # meaningful fixed overhead per call that batching amortizes away.
  #
  # For the global CI to be valid, every country must use the SAME
  # replicate index `b` for "the b-th random resampling draw," so that
  # summing across countries within a replicate reflects a single coherent
  # draw rather than mixing independent draws -- batching preserves this
  # exactly, since each replicate's combined frame is built from one
  # resample per country, all under the same `b`.
  # ===========================================================================
  all_countries <- names(country_point_info)
  multi_event_countries  <- all_countries[sapply(country_point_info,
                                                 function(x) x$nrow_current > 1)]
  single_event_countries <- setdiff(all_countries, multi_event_countries)

  # Single-event countries have no variance to bootstrap (resampling with
  # replacement always returns the same lone event) -- add their
  # deterministic contribution once, rather than "resampling" them
  # B_GLOBAL times for no statistical benefit.
  # NOTE: sapply() over an empty vector returns list(), not numeric(0), so
  # sum() on it would error with "invalid 'type' (list)" when every country
  # in this row/scenario happens to have >1 event. vapply() with an explicit
  # FUN.VALUE avoids that -- it returns numeric(0) for empty input, and
  # sum(numeric(0)) is just 0, no special-casing needed.
  deterministic_total <- sum(vapply(single_event_countries,
                                    function(cc) country_point_info[[cc]]$ann_deaths_this,
                                    FUN.VALUE = numeric(1)))

  n_multi <- length(multi_event_countries)
  # Matrix of per-country bootstrap means, one row per country, one column
  # per replicate -- lets us compute each country's CI with sd() afterward,
  # exactly like the original per-country loop did, just filled in via the
  # batched predict() calls below instead of individual ones.
  country_boot_means_mat <- matrix(NA_real_, nrow = max(n_multi, 1), ncol = B_GLOBAL,
                                   dimnames = list(if (n_multi > 0) multi_event_countries else "none", NULL))
  global_boot_totals <- rep(deterministic_total, B_GLOBAL)

  cat("  [", label, "] bootstrapping", n_multi, "countries x", B_GLOBAL,
      "replicates (batched into", B_GLOBAL, "predict() calls)...\n")
  flush(stdout())

  progress_step <- max(1, round(B_GLOBAL / 10))

  if (n_multi > 0) {
    for (b in 1:B_GLOBAL) {

      # Block bootstrap: resample YEARS with replacement, not individual
      # events. All events within a drawn year move together as one
      # block, preserving any same-year temporal clustering instead of
      # treating every event as exchangeable regardless of when it
      # occurred. This relaxes the assumption that error terms are
      # independent across time. Requires Start.Year to survive into
      # `cur` -- see build_trendData()'s keepVars/ssp_cols above.
      resampled_list <- lapply(multi_event_countries, function(cc) {
        cur <- country_current_list[[cc]]
        unique_years <- unique(cur$Start.Year)
        sampled_years <- sample(unique_years, length(unique_years), replace = TRUE)
        do.call(rbind, lapply(sampled_years, function(yr) cur[cur$Start.Year == yr, ]))
      })
      combined <- do.call(rbind, resampled_list)

      combined_pred <- predict(predMod.3, newdata = combined, type = "response",
                               allow.new.levels = TRUE)

      # Split predictions back out by country using Country_Code, which was
      # already carried through in `current` (part of td_obj$fixed_cols) --
      # no extra grouping column needed.
      pred_by_country <- split(combined_pred, combined$Country_Code)

      for (cc in multi_event_countries) {
        this_mean <- mean(pred_by_country[[cc]], na.rm = TRUE)
        country_boot_means_mat[cc, b] <- this_mean
        global_boot_totals[b] <- global_boot_totals[b] +
          this_mean * country_point_info[[cc]]$n_valid / N_YEARS
      }

      if (b %% progress_step == 0 || b == B_GLOBAL) {
        cat("    ...", round(100 * b / B_GLOBAL), "% (", b, "/", B_GLOBAL, "replicates)\n")
        flush(stdout())
      }
    }
  }

  # Assemble country-level results (point estimate + CI) from Pass 1 point
  # predictions and Pass 2 bootstrap matrix.
  country_results <- data.frame()
  for (this_country in all_countries) {
    info <- country_point_info[[this_country]]
    if (this_country %in% multi_event_countries) {
      boot_sd <- sd(country_boot_means_mat[this_country, ], na.rm = TRUE)
      lcl <- info$mean_deaths - qnorm(0.975) * boot_sd
      ucl <- info$mean_deaths + qnorm(0.975) * boot_sd
    } else {
      lcl <- NA_real_
      ucl <- NA_real_
    }
    country_results <- rbind(country_results, data.frame(
      Country_Code        = this_country,
      ann_deaths          = info$ann_deaths_this,
      ann_deaths_lcl      = lcl / N_YEARS * info$n_valid,
      ann_deaths_ucl      = ucl / N_YEARS * info$n_valid,
      mortality_fraction  = info$mort_fraction,
      n_events            = info$n_valid
    ))
  }

  # Global-level CI, from the shared-replicate bootstrap accumulated above.
  # mean(global_boot_totals) should closely match ann_deaths -- both are
  # estimates of the same quantity, one from the point predictions directly,
  # the other from the mean of the bootstrap distribution. Large divergence
  # between them would indicate a bug in the accumulation logic above.
  global_lcl <- mean(global_boot_totals) - qnorm(0.975) * sd(global_boot_totals)
  global_ucl <- mean(global_boot_totals) + qnorm(0.975) * sd(global_boot_totals)
  cat(label, ": ann deaths =", round(ann_deaths), 
      "(", n_events, "events ) | global 95% CI: [", round(global_lcl), ",",
      round(global_ucl), "] (bootstrap mean check:", round(mean(global_boot_totals)), ")\n")
  flush(stdout())
  list(ann_deaths = ann_deaths, country_results = country_results,
       global_lcl = global_lcl, global_ucl = global_ucl)
}

# SSP-consistent pairings: SLR scenario -> Liu HDI/pop SSP
# Using AR6_med and 50th percentile SLR throughout
# SSP1.26 and SSP5.85 also have AR6_low variants — included for completeness
ssp_pairings <- list(
  list(label="SSP1.19", slr="med_SSP1.19", liu_hdi_2050="hdi_SSP1_2050", liu_hdi_2100="hdi_SSP1_2100",
       liu_pop_2050="pop_SSP1_2050", liu_pop_2100="pop_SSP1_2100", popscale_suffix="SSP1"),
  list(label="SSP1.26_med", slr="med_SSP1.26", liu_hdi_2050="hdi_SSP1_2050", liu_hdi_2100="hdi_SSP1_2100",
       liu_pop_2050="pop_SSP1_2050", liu_pop_2100="pop_SSP1_2100", popscale_suffix="SSP1"),
  list(label="SSP1.26_low", slr="low_SSP1.26", liu_hdi_2050="hdi_SSP1_2050", liu_hdi_2100="hdi_SSP1_2100",
       liu_pop_2050="pop_SSP1_2050", liu_pop_2100="pop_SSP1_2100", popscale_suffix="SSP1"),
  list(label="SSP2.45", slr="med_SSP2.45", liu_hdi_2050="hdi_SSP2_2050", liu_hdi_2100="hdi_SSP2_2100",
       liu_pop_2050="pop_SSP2_2050", liu_pop_2100="pop_SSP2_2100", popscale_suffix="SSP2"),
  list(label="SSP3.70", slr="med_SSP3.70", liu_hdi_2050="hdi_SSP3_2050", liu_hdi_2100="hdi_SSP3_2100",
       liu_pop_2050="pop_SSP3_2050", liu_pop_2100="pop_SSP3_2100", popscale_suffix="SSP3"),
  list(label="SSP5.85_med", slr="med_SSP5.85", liu_hdi_2050="hdi_SSP5_2050", liu_hdi_2100="hdi_SSP5_2100",
       liu_pop_2050="pop_SSP5_2050", liu_pop_2100="pop_SSP5_2100", popscale_suffix="SSP5"),
  list(label="SSP5.85_low", slr="low_SSP5.85", liu_hdi_2050="hdi_SSP5_2050", liu_hdi_2100="hdi_SSP5_2100",
       liu_pop_2050="pop_SSP5_2050", liu_pop_2100="pop_SSP5_2100", popscale_suffix="SSP5"),
  # Decoupled bounding scenarios: forcing and population/HDI deliberately
  # mismatched to bound the outcome space. NOT standard SSP-RCP pairings --
  # label accordingly wherever these appear in the manuscript/SI.
list(label="BOUND_worst", slr="low_SSP5.85", liu_hdi_2050="hdi_SSP3_2050", liu_hdi_2100="hdi_SSP3_2100",
       liu_pop_2050="pop_SSP3_2050", liu_pop_2100="pop_SSP3_2100", popscale_suffix="BOUND_worst"),
  list(label="BOUND_worst_med", slr="med_SSP5.85", liu_hdi_2050="hdi_SSP3_2050", liu_hdi_2100="hdi_SSP3_2100",
       liu_pop_2050="pop_SSP3_2050", liu_pop_2100="pop_SSP3_2100", popscale_suffix="BOUND_worst"),
  list(label="BOUND_best", slr="med_SSP1.19", liu_hdi_2050="hdi_SSP5_2050", liu_hdi_2100="hdi_SSP5_2100",
       liu_pop_2050="pop_SSP5_2050", liu_pop_2100="pop_SSP5_2100", popscale_suffix="BOUND_best")
)
# Build population lookup tables for all SSPs from Liu data
make_pop_df <- function(col_2050, col_2100, year) {
  hdi_trend %>%
    dplyr::mutate(pop = .data[[if(year==2050) col_2050 else col_2100]] * 1000) %>%
    dplyr::select(Country_Code, pop) %>%
    dplyr::filter(!is.na(pop)) %>%
    rename_with(~ "pop_proj", "pop")
}

hist_ann_deaths <- sum(ci_base$meanDeaths * ci_base$nEvents, na.rm=T) / N_YEARS
country_master <- NULL
global_ci_rows <- list()

for (yr in c("2050", "2100")) {
  
  td_obj <- if (yr == "2050") td2050 else td2100
  cat("\n=== SSP-CONSISTENT PROJECTIONS:", yr, "===\n")
  
  for (sp in ssp_pairings) {
    
    hdi_col <- if (yr == "2050") sp$liu_hdi_2050 else sp$liu_hdi_2100
    pop_col <- if (yr == "2050") sp$liu_pop_2050  else sp$liu_pop_2100
    
    # Build pop lookup for this SSP/year
    pop_df <- hdi_trend %>%
      dplyr::mutate(pop_proj = .data[[pop_col]] * 1000) %>%
      dplyr::select(Country_Code, pop_proj) %>%
      dplyr::filter(!is.na(pop_proj))
    
    # Six methodology rows, each building on the previous:
    #   1. SLR only                                   -- geometry-only exposure, nothing else varies
    #   2. + Population (exposure only)                -- popscale-adjusted exposure
    #   3. + Population (exposure + NB covariate)       -- adds Liu pop.total.adm0 substitution
    #   4. + Population (both) & HDI                    -- adds Liu HDI substitution
    #   5. + Population (both) & HDI & coastal growth 1 -- adds Rentschler multiplier on top of Row 4
    #   6. + Population (both) & HDI & coastal growth 2 -- adds Tellman multiplier on top of Row 4
    # NOTE: labels use a leading space instead of "+" -- Excel treats a
    # leading "+" as the start of a formula and throws an error.
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

    row_results <- list()

    for (rs in row_specs) {

      mult_vec <- NULL
      if (identical(rs$coastal_mult_vec, "rentschler")) {
        # Rentschler is purely income-group based (no country-level values),
        # so every country uses its GNI-group multiplier directly.
        country_codes_here <- unique(td_obj$data$Country_Code)
        gni <- gni_lookup$gni.group[match(country_codes_here, gni_lookup$Country_Code)]
        mult_vec <- rentschler_mult[as.character(gni)]
        names(mult_vec) <- country_codes_here
        mult_vec[is.na(mult_vec)] <- 1.0
      } else if (identical(rs$coastal_mult_vec, "tellman")) {
        mult_vec <- build_country_mult(unique(td_obj$data$Country_Code),
                                       tellman_lookup, rentschler_mult)
      }

      row_results[[rs$name]] <- run_projections(
        td_obj, hdi_col, vary_hdi = rs$vary_hdi, vary_pop = rs$vary_pop,
        pop_proj_df = if (rs$vary_pop) pop_df else NULL,
        label = paste(yr, sp$label, trimws(rs$name)),
        slr_pattern = sp$slr,
        use_exposure_popscale = rs$use_exposure_popscale,
        exposure_popscale_suffix = sp$popscale_suffix,
        coastal_mult_vec = mult_vec
      )
    }

    # Accumulate country-level master database, tagging each row with
    # year, scenario, and methodology so it can be filtered/replotted later
    for (rs in row_specs) {
      this_country_results <- row_results[[rs$name]]$country_results
      if (nrow(this_country_results) == 0) {
        # Defensive guard: cbind(scalar, 0-row-data.frame) errors with
        # "arguments imply differing number of rows: 1, 0" instead of
        # producing a sensible (empty) result. This should only trigger if
        # a row/scenario genuinely has zero valid predictions across every
        # country -- warn loudly rather than crashing silently past it.
        warning("Empty country_results for ", yr, " / ", sp$label, " / ",
                trimws(rs$name), " -- skipping this row in country_master.")
      } else {
        country_master <- rbind(
          country_master,
          cbind(year = yr, ssp = sp$label, methodology = rs$name,
                this_country_results)
        )
      }
      # Global-level CI for this row: NOT derivable by summing the
      # per-country CIs above (that would sum unrelated bootstrap draws --
      # see run_projections()). Captured directly from the shared-replicate
      # bootstrap computed inside run_projections() instead.
      global_ci_rows[[length(global_ci_rows) + 1]] <- data.frame(
        year        = yr,
        ssp         = sp$label,
        methodology = rs$name,
        Ann_deaths_lcl = row_results[[rs$name]]$global_lcl,
        Ann_deaths_ucl = row_results[[rs$name]]$global_ucl
      )
    }
  }
}

# =============================================================================
# SECTION V: SUMMARY
# =============================================================================

cat("\n=== SSP-CONSISTENT RESULTS SUMMARY ===\n")
cat("Historical baseline:", round(hist_ann_deaths), "deaths/yr\n\n")

# Global-level CI table, one row per year x ssp x methodology, from the
# shared-replicate bootstrap computed inside run_projections() (NOT a sum
# of per-country CIs -- see note above).
global_ci_table <- do.call(rbind, global_ci_rows)

# Global decomposition table, built by summing country_master up to the
# global level -- this guarantees the global table and the country-level
# table are always numerically consistent with each other (same underlying
# per-country predictions, just aggregated). Note this gives the POINT
# ESTIMATE only; the CI comes from global_ci_table above, since CIs can't
# be derived by summing/aggregating country-level CIs.

global_summary <- country_master %>%
  dplyr::group_by(year, ssp, methodology) %>%
  dplyr::summarise(Ann_deaths = sum(ann_deaths, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(global_ci_table, by = c("year", "ssp", "methodology"))

# Sanity check: the point estimate from summed country predictions should
# closely match the mean of the global bootstrap distribution (computed
# independently inside run_projections). Large divergence would flag a bug.
cat("\n=== POINT ESTIMATE vs. BOOTSTRAP MEAN SANITY CHECK ===\n")
print(global_summary %>%
        dplyr::mutate(bootstrap_midpoint = (Ann_deaths_lcl + Ann_deaths_ucl) / 2,
                      pct_diff = round(100 * (Ann_deaths - bootstrap_midpoint) / Ann_deaths, 2)) %>%
        dplyr::filter(abs(pct_diff) > 5) %>%  # only print rows where they diverge notably
        dplyr::select(year, ssp, methodology, Ann_deaths, bootstrap_midpoint, pct_diff),
      n = Inf)

print(global_summary %>%
        dplyr::mutate(Ann_deaths = round(Ann_deaths),
                      Ann_deaths_lcl = round(Ann_deaths_lcl),
                      Ann_deaths_ucl = round(Ann_deaths_ucl)) %>%
        dplyr::arrange(year, ssp, methodology), n = Inf)

cat("\n=== GLOBAL SUMMARY (Row 1 vs Row 4 range check) ===\n")
for (yr in c("2050", "2100")) {
  sub <- global_summary[global_summary$year == yr, ]
  cat("\n---", yr, "---\n")
  for (rs_name in c(" SLR only", " + Population (exposure only)",
                     " + Population (exposure + NB covariate)",
                     " + Population (both) & HDI",
                     " + Population (both) & HDI & coastal growth 1",
                     " + Population (both) & HDI & coastal growth 2")) {
    this_row <- sub[sub$methodology == rs_name, ]
    cat(trimws(rs_name), "range: ", round(min(this_row$Ann_deaths)), "-",
        round(max(this_row$Ann_deaths)), "deaths/yr\n")
  }
}

# Build decomposition table: historical baseline rows + 6 methodology rows
# per year x scenario, with % vs. historical, deaths averted vs. SLR-only,
# and now the global-level 95% CI (lcl/ucl) for each row.
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
    slr_only_val <- ssp_sub$Ann_deaths[ssp_sub$methodology == " SLR only"]
    for (rs_name in c(" SLR only", " + Population (exposure only)",
                       " + Population (exposure + NB covariate)",
                       " + Population (both) & HDI",
                       " + Population (both) & HDI & coastal growth 1",
                       " + Population (both) & HDI & coastal growth 2")) {
      this_row <- ssp_sub[ssp_sub$methodology == rs_name, ]
      decomp_rows[[length(decomp_rows)+1]] <- data.frame(
        Year                  = yr,
        Scenario              = rs_name,
        SSP                   = this_ssp,
        Ann_deaths            = round(this_row$Ann_deaths),
        Ann_deaths_lcl         = round(this_row$Ann_deaths_lcl),
        Ann_deaths_ucl         = round(this_row$Ann_deaths_ucl),
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
decomp_table <- do.call(rbind, decomp_rows)

cat("\n=== DECOMPOSITION TABLE ===\n")
print(decomp_table, row.names=FALSE)

# Save outputs. decomp_table and country_master together contain everything
# needed downstream: historical baseline, per-methodology point estimates
# and CIs at both country and global level, and deaths averted vs. SLR-only.
# The Liu et al. HDI/population input snapshot is not written out separately
# since it is just a copy of matched input data -- the country-code matching
# itself is verified via the console messages printed above (territories
# mapped, coverage counts).
write.csv(decomp_table,
          paste0("Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_", date_append_output, ".csv"),
          row.names = FALSE)
write.csv(country_master,
          paste0("Intermediate outputs/Output from Step 9/Country_level_master_database_", date_append_output, ".csv"),
          row.names = FALSE)

cat("\nOutputs saved to Intermediate outputs/Output from Step 9/\n")
