##########################################
# Step 9y - Diagnostic                   #
# Nondominant countries                  #
#                                        #
# 2026-07-04                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# Diagnostic script, not part of the numbered pipeline's main data flow.
# Run this in the same session as 9x_Diagnostic_Verify_bounding_assumption.R
# (needs hdi_trend already loaded). Identifies the specific countries where SSP3
# does NOT have the highest 2100 population among SSP1/2/3/5, and cross-
# references them against event counts in the historical dataset (ci_base
# or ci_50, whichever is available in your Step 8/9 environment) to check
# whether the "BOUND_worst" approximation is likely to matter in practice.

ssps <- c(1, 2, 3, 5)
pop_cols <- paste0("pop_SSP", ssps, "_2100")
hdi_cols <- paste0("hdi_SSP", ssps, "_2100")

check_df <- hdi_trend[, c("Country_Code", pop_cols, hdi_cols)]

check_df$max_pop_ssp <- apply(check_df[, pop_cols], 1, function(x) {
  if (all(is.na(x))) return(NA)
  ssps[which.max(x)]
})

nondominant <- check_df[!is.na(check_df$max_pop_ssp) & check_df$max_pop_ssp != 3, ]

cat("=== Countries where SSP3 is NOT the highest-population SSP at 2100 ===\n")
print(nondominant[, c("Country_Code", "max_pop_ssp", pop_cols)])

# Cross-reference against event counts, if available, to gauge how much
# this matters for the aggregate mortality total. Adjust object/column
# names below to match whatever is in your environment (e.g. ci_base,
# ci_50, or trendData_codes with an nEvents-style column).

if (exists("ci_base")) {
  event_counts <- aggregate(nEvents ~ Country_Code, data = ci_base, FUN = sum)
  nondominant_weighted <- merge(nondominant, event_counts, by = "Country_Code", all.x = TRUE)
  nondominant_weighted <- nondominant_weighted[order(-nondominant_weighted$nEvents), ]

  cat("\n=== Same countries, with historical event counts (highest weight first) ===\n")
  print(nondominant_weighted[, c("Country_Code", "max_pop_ssp", "nEvents")])

  total_events <- sum(event_counts$nEvents, na.rm = TRUE)
  nondominant_events <- sum(nondominant_weighted$nEvents, na.rm = TRUE)

  cat("\nShare of total historical events belonging to non-dominant countries:",
      round(100 * nondominant_events / total_events, 1), "%\n")
  cat("(If this is a small share, BOUND_worst is unlikely to meaningfully\n")
  cat("understate the true worst case in aggregate. If large, consider a\n")
  cat("country-specific 'worst SSP' pairing instead of a single global one.)\n")

} else {
  cat("\n'ci_base' not found in environment -- substitute the object that",
      "holds\nper-country historical event counts to complete the weighting check.\n")
}

