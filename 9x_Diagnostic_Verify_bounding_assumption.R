##########################################
# Step 9x - Diagnostic                   #
# Verify bounding assumption             #
#                                        #
# 2026-07-04                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# Diagnostic script, not part of the numbered pipeline's main data flow.
# Run this AFTER Step 9's Section III (Liu et al. 2024 data loading) so that
# `hdi_trend` is already in your environment. Confirms whether SSP3 is
# actually the worst (highest pop growth, lowest HDI) and SSP5 the best
# (lowest pop growth, highest HDI) among the four SSPs used in the main
# analysis (SSP1, SSP2, SSP3, SSP5) -- the assumption behind the
# BOUND_worst / BOUND_best decoupled scenarios.

ssps <- c(1, 2, 3, 5)

# I. Aggregate population comparison (sum across countries) ------------------
cat("=== Total population by SSP (sum across countries) ===\n")
for (yr in c(2050, 2100)) {
  cat("\nYear:", yr, "\n")
  totals <- sapply(ssps, function(s) {
    col <- paste0("pop_SSP", s, "_", yr)
    if (col %in% names(hdi_trend)) sum(hdi_trend[[col]], na.rm = TRUE) else NA
  })
  names(totals) <- paste0("SSP", ssps)
  print(round(totals))
  cat("Highest (expected: SSP3):", names(totals)[which.max(totals)], "\n")
  cat("Lowest  (expected: SSP5):", names(totals)[which.min(totals)], "\n")
}

# II. Mean HDI comparison (unweighted mean across countries) -----------------
cat("\n\n=== Mean HDI by SSP (unweighted mean across countries) ===\n")
for (yr in c(2050, 2100)) {
  cat("\nYear:", yr, "\n")
  means <- sapply(ssps, function(s) {
    col <- paste0("hdi_SSP", s, "_", yr)
    if (col %in% names(hdi_trend)) mean(hdi_trend[[col]], na.rm = TRUE) else NA
  })
  names(means) <- paste0("SSP", ssps)
  print(round(means, 3))
  cat("Highest (expected: SSP5):", names(means)[which.max(means)], "\n")
  cat("Lowest  (expected: SSP3):", names(means)[which.min(means)], "\n")
}

# III. Country-by-country check: does SSP3 dominate on BOTH axes -------------
# for every country, or just in aggregate? This matters because your
# mortality model operates at the country/event level, not on global
# aggregates -- a scenario could be "worst on average" while still being
# less bad than another scenario in specific high-mortality countries.

cat("\n\n=== Country-level dominance check (2100) ===\n")

pop_cols <- paste0("pop_SSP", ssps, "_2100")
hdi_cols <- paste0("hdi_SSP", ssps, "_2100")

if (all(pop_cols %in% names(hdi_trend)) & all(hdi_cols %in% names(hdi_trend))) {

  check_df <- hdi_trend[, c("Country_Code", pop_cols, hdi_cols)]

  check_df$pop_ssp3_is_max <- check_df[[paste0("pop_SSP3_2100")]] ==
    apply(check_df[, pop_cols], 1, max, na.rm = TRUE)

  check_df$hdi_ssp3_is_min <- check_df[[paste0("hdi_SSP3_2100")]] ==
    apply(check_df[, hdi_cols], 1, min, na.rm = TRUE)

  n_countries <- sum(!is.na(check_df$pop_ssp3_is_max))

  cat("Countries where SSP3 has the highest 2100 population (of SSP1/2/3/5):",
      sum(check_df$pop_ssp3_is_max, na.rm = TRUE), "of", n_countries, "\n")
  cat("Countries where SSP3 has the lowest 2100 HDI (of SSP1/2/3/5):",
      sum(check_df$hdi_ssp3_is_min, na.rm = TRUE), "of", n_countries, "\n")
  cat("Countries where SSP3 dominates on BOTH axes simultaneously:",
      sum(check_df$pop_ssp3_is_max & check_df$hdi_ssp3_is_min, na.rm = TRUE),
      "of", n_countries, "\n")

  cat("\nIf either count is well below 100% of countries, the 'BOUND_worst'\n")
  cat("label may not hold uniformly -- some countries could see a WORSE\n")
  cat("outcome under a different SSP's population/HDI trajectory than SSP3's.\n")
  cat("Consider reporting this caveat explicitly if so.\n")

} else {
  cat("One or more expected pop_SSP*/hdi_SSP* columns not found in hdi_trend --",
      "check column names before interpreting.\n")
}

