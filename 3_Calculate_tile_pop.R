##########################################
# Step 3 - Calculate yearly populations  #
# at different surge heights and for     #
# countries as a whole                   #
#                                        #
# 2026-06-24                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# Overview of what this script does:
#
# 1. Reads tile-level 2019 exposed populations (Step 2) and country-level
#    population data (World Bank 1985-1999, SLIIDERS pop_scale 2000-2100).
#
# 2. For 1985-1999: scales each tile's 2019 exposed-pop fraction by the WB
#    country total for each year to get tile-level exposed populations.
#
# 3. For 2000-2100: multiplies each tile's 2019 exposed population by the
#    SLIIDERS pop_scale factor (ratio of SSP-projected country pop to 2019
#    country pop) to get SSP-scenario tile populations.
#
# 4. For SLR scenario columns (from Step 2): applies the same pop_scale
#    multiplier to combine SLR-adjusted geometry with demographic change.

rm(list = ls())

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

step1_date_append_output = "2026-06-24"  # version of Step 1 files to read in
step2_date_append_output = "2026-06-25"  # version of Step 2 files to read in
date_append_output       = "2026-07-04"  # version to append to Step 3 files

surge.period.vec <- c("10yr", "100yr", "1000yr", "10000yr")


# I. Read inputs ---------------------------------------------------------------

WB_pop.data <- read_excel("Inputs/World Bank pop data/WB pop data 2024-01-16.xlsx")

seg_adm_pop_exposed.df <-
  read.csv(paste0("Intermediate outputs/Output from Step 2/",
                  "Population below surge return periods ",
                  "(present + future) by tile no pop growth ",
                  step2_date_append_output, ".csv"))

# country.data maps SLIIDERS country codes to World Bank codes and parent
# countries for overseas territories that lack their own WB or pop_scale data
country.data <-
  read.csv(paste0("Inputs/Identify mismatched and missing countries - step 3/",
                  "SLIIDERS country codes with names and territorial mapping.csv"))

pop_scale.data <-
  read.csv(paste0("Intermediate outputs/Output from Step 1/",
                  "pop_scale_data ", step1_date_append_output, ".csv"))

taiwan.pop.data <- read.csv("Inputs/Taiwan data/Total population_20240810005643.csv",
                             skip = 9)


# II. Build long-format World Bank population table ----------------------------
#
# The WB Excel file has one row per country and year columns named like
# "1960 [YR1960]". We extract the 4-digit year prefix, pivot to long format,
# and drop rows/columns we don't need.

# Trim column names to 4-digit year strings for columns 5:95
names(WB_pop.data)[5:95] <- substr(names(WB_pop.data)[5:95], 1, 4)
WB_pop.data[, 5:95]      <- lapply(WB_pop.data[, 5:95], as.numeric)
WB_pop.data               <- WB_pop.data[!is.na(WB_pop.data$`Country Name`), ]

year_columns <- names(WB_pop.data)[5:95]

WBpop.transformed <- do.call(rbind, lapply(year_columns, function(yr) {
  data.frame(
    Country      = WB_pop.data$`Country Name`,
    Country_Code = WB_pop.data$`Country Code`,
    Year         = as.integer(yr),
    Population   = as.numeric(WB_pop.data[[yr]])
  )})) %>%
  dplyr::filter(!is.na(Population))


# III. Append Taiwan population data -------------------------------------------
#
# Taiwan is not in the World Bank data; we use a separate source.
# Covers years 1970-2070 to span both historical and future periods.

taiwan.total.row <- match("Total", taiwan.pop.data$X.1)
taiwan.years     <- 1970:2070
taiwan.col.idx   <- match(paste0("X", taiwan.years), colnames(taiwan.pop.data))

taiwan.data.to.add <- data.frame(
  Country      = "Taiwan",
  Country_Code = "TWN",
  Year         = taiwan.years,
  Population   = as.numeric(taiwan.pop.data[taiwan.total.row, taiwan.col.idx])
)

WBpop.transformed <- bind_rows(WBpop.transformed, taiwan.data.to.add)


# IV. Extract SLIIDERS country codes from tile IDs ----------------------------
#
# seg_adm format encodes the country code between an underscore and a period,
# e.g. "seg001_USA.adm1". The regex extracts that portion.
# A fallback uses character positions 16-18 for tiles where the regex fails.

seg_adm_pop_exposed.df <- seg_adm_pop_exposed.df %>%
  mutate(country_code = str_extract(seg_adm, "(?<=_)[A-Z]+(?=\\.)"))

na_idx <- which(is.na(seg_adm_pop_exposed.df$country_code))
if (length(na_idx) > 0) {
  message(length(na_idx), " tiles had no regex country code match; using substr fallback.")
  seg_adm_pop_exposed.df$country_code[na_idx] <-
    substr(seg_adm_pop_exposed.df$seg_adm[na_idx], 16, 18)
}

# Drop index columns added by read.csv
seg_adm_pop_exposed.df <-
  seg_adm_pop_exposed.df[, !colnames(seg_adm_pop_exposed.df) %in% c("X", "X.1")]


# V. Map SLIIDERS codes to World Bank codes ------------------------------------
#
# Some SLIIDERS codes (e.g. overseas territories like GUM for Guam) don't exist
# in the WB data. For these, country.data defines a parent country to use
# instead. country_code_WB is used ONLY for joining WB population totals.
# pop_scale lookups always use the original SLIIDERS country_code.

countries.to.replace.WB <-
  setdiff(unique(seg_adm_pop_exposed.df$country_code),
          unique(WBpop.transformed$Country_Code[WBpop.transformed$Year == 2019]))

seg_adm_pop_exposed.df <- seg_adm_pop_exposed.df %>%
  mutate(country_code_WB = if_else(
    country_code %in% countries.to.replace.WB,
    country.data$Group.with..for.World.Bank.[
      match(country_code, country.data$SLIIDERS.country.code)],
    country_code
  ))

# Sanity check: any remaining mismatches?
still_missing <- setdiff(unique(seg_adm_pop_exposed.df$country_code_WB),
                          unique(WBpop.transformed$Country_Code))
if (length(still_missing) > 0)
  warning("These country_code_WB values have no WB population data: ",
          paste(still_missing, collapse = ", "))


# VI. Compute 2019 exposed-population fractions --------------------------------
#
# For each tile, calculate what fraction its exposed population represents of
# its country's total 2019 population. This fraction is assumed stable over
# time and used to scale annual country totals down to tile level.

WBpop.2019 <- WBpop.transformed %>%
  dplyr::filter(Year == 2019) %>%
  dplyr::select(Country_Code, pop2019.whole.country = Population)

seg_adm_pop_exposed.df <- seg_adm_pop_exposed.df %>%
  left_join(WBpop.2019, by = c("country_code_WB" = "Country_Code")) %>%
  mutate(pop2019.whole.country = as.numeric(pop2019.whole.country))

# Calculate one proportion column per surge return period
for (surge in surge.period.vec) {
  pop_col  <- paste0("base.2019.pop.", surge)
  prop_col <- paste0("prop.2019.", surge)
  seg_adm_pop_exposed.df[[prop_col]] <-
    seg_adm_pop_exposed.df[[pop_col]] / seg_adm_pop_exposed.df$pop2019.whole.country
}


# VII. 1985-1999: WB-based tile populations ------------------------------------
#
# For years before SLIIDERS pop_scale coverage, we use actual WB country
# populations scaled by the stable 2019 fraction.
# Formula: tile_pop(year, surge) = prop.2019.surge * WB_country_pop(year)

WBpop.wide.historical <- WBpop.transformed %>%
  dplyr::filter(Year %in% 1985:1999) %>%
  dplyr::select(Country_Code, Year, Population)

for (y in 1985:1999) {
  pop_year <- WBpop.wide.historical %>%
    dplyr::filter(Year == y) %>%
    dplyr::select(Country_Code, Year_Pop = Population)

  country_pop_vec <- pop_year$Year_Pop[
    match(seg_adm_pop_exposed.df$country_code_WB, pop_year$Country_Code)]

  for (surge in surge.period.vec) {
    col_name <- paste0("base.", y, ".pop.", surge)
    seg_adm_pop_exposed.df[[col_name]] <-
      seg_adm_pop_exposed.df[[paste0("prop.2019.", surge)]] * country_pop_vec
  }
}


# VIII. Fix zero pop_scale values in SLIIDERS data ----------------------------
#
# Some small territories have pop_scale == 0, meaning SLIIDERS doesn't model
# them directly. We substitute the parent country's pop_scale for the same
# SSP and year. Note: if the parent also has 0, the result will remain 0
# (a warning is issued below).

pop_scale.data.0.index    <- which(pop_scale.data$pop_scale == 0)
pop_scale.data.0.countries <- unique(pop_scale.data$country[pop_scale.data.0.index])

pop_scale.data$pop_scale[pop_scale.data.0.index] <-
  sapply(pop_scale.data.0.index, function(x) {
    parent <- country.data$Group.with..for.World.Bank.[
      match(pop_scale.data$country[x], country.data$SLIIDERS.country.code)]
    val <- pop_scale.data$pop_scale[
      pop_scale.data$ssp     == pop_scale.data$ssp[x] &
      pop_scale.data$year    == pop_scale.data$year[x] &
      pop_scale.data$country == parent]
    if (length(val) == 0 || all(val == 0)) {
      warning("pop_scale still 0 after substitution for country: ",
              pop_scale.data$country[x])
      return(0)
    }
    val[1]
  })


# IX. 2000-2100: SLIIDERS pop_scale tile populations --------------------------
#
# pop_scale is a country-level ratio: projected_pop(year, SSP) / pop_2019.
# Multiplying by the tile's 2019 exposed population gives projected tile pop.
# Formula: tile_pop(year, SSP, surge) = pop_scale(country, year, SSP)
#                                       * base.2019.pop.surge
#
# NOTE: pop_scale is indexed by SLIIDERS country_code, NOT country_code_WB.
# Using country_code_WB here would give wrong results for overseas territories
# (e.g., Guam would get USA's scale factor applied, which is the WB grouping,
# but pop_scale.data uses GUM directly).

ssp.vec              <- unique(pop_scale.data$ssp)
pop_scale.year.vec   <- c(min(pop_scale.data$year):2025, 2050, 2100)

# Pre-nest pop_scale for fast lookup: list keyed by "country|ssp|year"
pop_scale.lookup <- pop_scale.data %>%
  mutate(key = paste(country, ssp, year, sep = "|")) %>%
  dplyr::select(key, pop_scale)

# Build all SSP columns vectorially (one year+SSP+surge at a time)
for (yr in pop_scale.year.vec) {
  for (ssp in ssp.vec) {

    # Extract scale factor for every tile's SLIIDERS country code
    keys        <- paste(seg_adm_pop_exposed.df$country_code, ssp, yr, sep = "|")
    scale_vec   <- pop_scale.lookup$pop_scale[match(keys, pop_scale.lookup$key)]

    for (surge in surge.period.vec) {
      col_name <- paste0("base.", yr, ".", ssp, ".pop.", surge)
      base_col <- paste0("base.2019.pop.", surge)
      seg_adm_pop_exposed.df[[col_name]] <-
        scale_vec * seg_adm_pop_exposed.df[[base_col]]
    }
  }

  message("SSP population columns done for year: ", yr)
}


# X. SLR scenario columns: apply pop_scale to get composite estimates ----------
#
# Step 2 produced columns for populations exposed under SLR-adjusted surge
# heights (e.g. "SSP1.2050.17_100yr.pop.100yr"). These reflect SLR geometry
# changes but still use 2019 population counts.
#
# Here we multiply by the matching pop_scale to add demographic change,
# producing composite columns (SLR geometry + future population).
#
# SSP-to-SLR scenario mapping (based on scientific convention):
#   SSP1 / ssp119 / ssp126 / NCC_AR6 / k14_RCP26 → SSP1 pop_scale
#   SSP2 / ssp245 / k14_RCP45                     → SSP2 pop_scale
#   SSP3 / ssp370                                 → SSP3 pop_scale
#   SSP5 / ssp585 / k14_RCP85                     → SSP5 pop_scale
#
# NOTE: NCC_AR6 (no climate change) is paired with SSP1 population growth
# as a conservative baseline — sea level stays flat but population still grows.
#
# pop_scale lookups use country_code (the SLIIDERS code), not country_code_WB
# -- using the WB-grouped code here would misassign territories' pop_scale.
# SSP pattern matching is anchored to the column-name structure (e.g. "SSP1\\.")
# rather than a bare substring, so "SSP1" can't accidentally match "SSP10".

# Define which SSP population scenario to apply to each SLR scenario pattern
slr_ssp_map <- list(
  list(pattern = "SSP1\\.",   ssp = "SSP1"),
  list(pattern = "SSP2\\.",   ssp = "SSP2"),
  list(pattern = "SSP3\\.",   ssp = "SSP3"),
  list(pattern = "SSP5\\.",   ssp = "SSP5"),
  list(pattern = "NCC_AR6",   ssp = "SSP1"),
  list(pattern = "k14_RCP26", ssp = "SSP1"),
  list(pattern = "k14_RCP45", ssp = "SSP2"),
  list(pattern = "k14_RCP85", ssp = "SSP5")
)

# Identify SLR columns from Step 2 (contain return period, no "base", no "popscale")
slr_cols <- colnames(seg_adm_pop_exposed.df)[
  grepl("pop\\.(10|100|1000|10000)yr", colnames(seg_adm_pop_exposed.df)) &
  !grepl("base",     colnames(seg_adm_pop_exposed.df)) &
  !grepl("popscale", colnames(seg_adm_pop_exposed.df)) &
  !grepl("prop",     colnames(seg_adm_pop_exposed.df))
]

for (col in slr_cols) {

  # Determine which year (2050 or 2100) this column refers to
  yr <- if (grepl("\\.2050\\.", col)) 2050 else
        if (grepl("\\.2100\\.", col)) 2100 else next  # skip if no year found

  # Find matching SSP from the map
  matched_ssp <- NA
  for (entry in slr_ssp_map) {
    if (grepl(entry$pattern, col)) { matched_ssp <- entry$ssp; break }
  }
  if (is.na(matched_ssp)) next  # no matching SSP rule; skip column

  # Look up pop_scale for each tile using SLIIDERS country_code (not WB code)
  keys      <- paste(seg_adm_pop_exposed.df$country_code, matched_ssp, yr, sep = "|")
  scale_vec <- pop_scale.lookup$pop_scale[match(keys, pop_scale.lookup$key)]

  new_col <- paste0(col, ".popscale.", matched_ssp)
  seg_adm_pop_exposed.df[[new_col]] <-
    seg_adm_pop_exposed.df[[col]] * scale_vec

  message("Created: ", new_col)
}


# X-bis. Decoupled bounding scenarios (mismatched forcing/population) --------
#
# The canonical SSP-RCP pairings above couple physical forcing (sea level
# rise magnitude) with a specific socioeconomic population trajectory via
# the scientific convention embedded in slr_ssp_map. This is a deliberate,
# defensible choice, but it means forcing severity and population growth
# are never independently varied -- e.g. the highest-forcing scenario
# (SSP5.85) is always paired with SSP5's own (relatively high-growth)
# population trajectory, never with a more pessimistic one.
#
# To bound the outcome space and test sensitivity to this coupling, we add
# two explicit decoupled scenarios here:
#   1. "Worst-case bound": highest forcing (SSP5.85) x SSP3 population
#      growth (the population trajectory associated with the weakest
#      socioeconomic outlook among the scenarios considered)
#   2. "Best-case bound": lowest forcing (SSP1.19) x SSP5 population
#      growth (the trajectory associated with the strongest economic
#      growth among the scenarios considered)
#
# These are NOT standard SSP-RCP pairings and should be labeled as such
# (e.g. "BOUND_worst" / "BOUND_best") wherever they appear in downstream
# tables or figures, to avoid being mistaken for additional canonical
# scenarios.

decoupled_bounds <- list(
  list(label = "BOUND_worst", slr_pattern = "SSP5\\.85", pop_ssp = "SSP3"),
  list(label = "BOUND_best",  slr_pattern = "SSP1\\.19", pop_ssp = "SSP5")
)

## Re-identify the same SLR geometry columns used in Section X above
slr_cols_for_bounds <- colnames(seg_adm_pop_exposed.df)[
  grepl("pop\\.(10|100|1000|10000)yr", colnames(seg_adm_pop_exposed.df)) &
  !grepl("base",      colnames(seg_adm_pop_exposed.df)) &
  !grepl("popscale",  colnames(seg_adm_pop_exposed.df)) &
  !grepl("prop",      colnames(seg_adm_pop_exposed.df))
]

for (bound in decoupled_bounds) {

  matching_cols <- slr_cols_for_bounds[grepl(bound$slr_pattern, slr_cols_for_bounds)]

  if (length(matching_cols) == 0) {
    warning(paste0("No SLR columns matched pattern '", bound$slr_pattern,
                    "' for bounding scenario '", bound$label,
                    "' -- check pattern against actual column names."))
    next
  }

  for (col in matching_cols) {

    yr <- if (grepl("\\.2050\\.", col)) 2050 else
          if (grepl("\\.2100\\.", col)) 2100 else next

    keys      <- paste(seg_adm_pop_exposed.df$country_code, bound$pop_ssp, yr, sep = "|")
    scale_vec <- pop_scale.lookup$pop_scale[match(keys, pop_scale.lookup$key)]

    new_col <- paste0(col, ".popscale.", bound$label)
    seg_adm_pop_exposed.df[[new_col]] <-
      seg_adm_pop_exposed.df[[col]] * scale_vec

    message("Created decoupled bound: ", new_col,
            " (forcing pattern: ", bound$slr_pattern,
            ", population: ", bound$pop_ssp, ")")
  }
}


# XI. Output files -------------------------------------------------------------

dir.create("Intermediate outputs/Output from Step 3", recursive = TRUE, showWarnings = FALSE)
dir.create("Output", recursive = TRUE, showWarnings = FALSE)

write.csv(
  WBpop.transformed,
  paste0("Intermediate outputs/Output from Step 3/",
         "All pops by year (past and future) by country ",
         date_append_output, ".csv")
)

write.csv(
  seg_adm_pop_exposed.df,
  paste0("Intermediate outputs/Output from Step 3/",
         "All pops by year (past and future) by SSP by return period by tile",
         " with pop growth ", date_append_output, ".csv")
)

# Log which countries were grouped for WB data
write.csv(
  data.frame(
    SLIIDERS.country.code = countries.to.replace.WB,
    SLIIDERS.country.name = country.data$Name..ISO.3166.if.available.[
      match(countries.to.replace.WB, country.data$SLIIDERS.country.code)],
    WB.country.code = country.data$Group.with..for.World.Bank.[
      match(countries.to.replace.WB, country.data$SLIIDERS.country.code)]
  ),
  paste0("Intermediate outputs/Output from Step 3/Countries aggregated for WB pops 1985-1999 ",
         date_append_output, ".csv")
)

# Log which countries were grouped for pop_scale data
write.csv(
  data.frame(
    SLIIDERS.country.code = pop_scale.data.0.countries,
    SLIIDERS.country.name = country.data$Name..ISO.3166.if.available.[
      match(pop_scale.data.0.countries, country.data$SLIIDERS.country.code)],
    WB.country.code = country.data$Group.with..for.World.Bank.[
      match(pop_scale.data.0.countries, country.data$SLIIDERS.country.code)]
  ),
  paste0("Intermediate outputs/Output from Step 3/Countries aggregated for pop scale 2000-2100 ",
         date_append_output, ".csv")
)

