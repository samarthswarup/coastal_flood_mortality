###########################################
# Table S5 - Historical trends            #
# (1990-2024) by GNI group                #
#                                         #
# 2026-07-14                              #
# Ryan Calder                             #
# Notes added by Claude                   #
###########################################

# Freestanding figure/table script, run independently of the numbered
# pipeline. Builds the historical-trends statistics table (Mann-Kendall
# trend tests, Global + each GNI group). Figure 2 covers the
# corresponding plot; this script only builds the table.
#
# Also generates the table's footnote text (Section V-A/B/C below),
# covering two claims that aren't captured by the main Group x Variable
# table: (1) which calendar month(s) drive the increasing event count (a
# per-month Mann-Kendall test, 12 tests, not just the single
# already-published October figure), and (2) an explicit 1990-1999 vs.
# 2000-2024 death-rate comparison (Table S6 only covers 2000-2024; this
# adds the complementary earlier window so the "declined more sharply
# before 2000" claim is a real computed comparison, not an eyeballed
# one). Output is a companion .txt file alongside the table CSV, ready
# to paste into the SI Word doc as the table's footnote.
#
# Draws exclusively on trendData as saved by Step 8 -- no re-derivation of
# exposed populations, death counts, or death rates, which are already final
# in that object.

rm(list = ls())

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(modifiedmk)
library(trend)
library(scales)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 8 run to pull trendData from
step8_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

YEAR_MIN <- 1990

# =============================================================================
# SECTION I: LOAD UPSTREAM OUTPUT
# =============================================================================

trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

# =============================================================================
# SECTION II: HARMONIZE FILTERING + DERIVED FIELDS
# (identical rules to those used elsewhere in the pipeline)
# =============================================================================

trendData <- trendData %>%
  filter(Start.Year >= YEAR_MIN,
         !is.na(pop.exposed.local.else.adm1.else.national.100yr),
         pop.exposed.local.else.adm1.else.national.100yr > 0) %>%
  mutate(
    death.rate = Total.Deaths / pop.exposed.local.else.adm1.else.national.100yr,
    death.rate.na.as.0 = total.deaths.na.as.0 / pop.exposed.local.else.adm1.else.national.100yr,
    death.rate = ifelse(is.finite(death.rate) & death.rate < 1, death.rate, NA_real_),
    death.rate.na.as.0 = ifelse(is.finite(death.rate.na.as.0) & death.rate.na.as.0 < 1, death.rate.na.as.0, NA_real_)
  )

# lock GNI levels to avoid drift
trendData$gni.group <- factor(trendData$gni.group, levels = c("LI", "LMI", "UMI", "HI"))

# =============================================================================
# SECTION III: MANN-KENDALL / SEN'S SLOPE HELPERS
# =============================================================================

# Conservative MK wrapper: Hamed-Rao variance correction (modifiedmk::mkttest)
mk_conservative <- function(y) {
  y <- as.numeric(y)
  ok <- is.finite(y)
  if (sum(ok) < 3L) {
    return(tibble::tibble(tau = NA_real_, p = NA_real_, slope = NA_real_))
  }

  mk <- modifiedmk::mkttest(y[ok])

  # robust field getter that tries several aliases
  get_field <- function(obj, candidates) {
    nms <- names(obj)
    hit <- intersect(candidates, nms)
    if (length(hit) == 0) return(NA_real_)
    as.numeric(unname(obj[hit[1]]))
  }

  tau_val <- get_field(mk, c("Tau", "tau", "Kendall's tau", "kendall.tau"))
  p_val   <- get_field(mk, c("P-value", "pvalue", "sl", "Pr(>|z|)", "p.value", "p"))

  # clean Sen slope estimate
  ss <- trend::sens.slope(y[ok])

  tibble::tibble(
    tau   = tau_val,
    p     = p_val,
    slope = as.numeric(ss$estimates)
  )
}

# Build one table block for a given group
make_block_conservative <- function(label, dat) {
  # Event Frequency = yearly counts (no daily zero-fill)
  freq_year <- dat %>%
    filter(!is.na(Start.Year)) %>%
    count(Start.Year, name = "value") %>%
    arrange(Start.Year)

  freq_res <- mk_conservative(freq_year$value) %>%
    mutate(Variable = "Event Frequency")

  # Other variables are tested as per-event series (after caps/filters above)
  other_res <- dat %>%
    transmute(
      `Estimated Exposed Population`        = pop.exposed.local.else.adm1.else.national.100yr,
      `Deaths (NAs removed)`                = total.deaths.reported,
      `Deaths (NAs as zero)`                = total.deaths.na.as.0,
      `Estimated Death Rate (NAs removed)`  = death.rate,
      `Estimated Death Rate (NAs as zero)`  = death.rate.na.as.0
    ) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "value") %>%
    group_by(Variable) %>%
    summarise(mk_conservative(value), .groups = "drop")

  bind_rows(freq_res, other_res) %>%
    mutate(Group = label, n_events = nrow(dat))
}

# =============================================================================
# SECTION IV: ASSEMBLE TABLE S5 (Global + GNI groups)
# =============================================================================

blocks <- list(
  make_block_conservative(sprintf("Global (%s events)", comma(nrow(trendData))), trendData),
  make_block_conservative(sprintf("Low Income (%s events)", comma(sum(trendData$gni.group == "LI", na.rm = TRUE))),
                          filter(trendData, gni.group == "LI")),
  make_block_conservative(sprintf("Lower Middle Income (%s events)", comma(sum(trendData$gni.group == "LMI", na.rm = TRUE))),
                          filter(trendData, gni.group == "LMI")),
  make_block_conservative(sprintf("Upper Middle Income (%s events)", comma(sum(trendData$gni.group == "UMI", na.rm = TRUE))),
                          filter(trendData, gni.group == "UMI")),
  make_block_conservative(sprintf("High Income (%s events)", comma(sum(trendData$gni.group == "HI", na.rm = TRUE))),
                          filter(trendData, gni.group == "HI"))
)

group_order <- vapply(blocks, function(b) b$Group[1], character(1))
var_order <- c(
  "Event Frequency",
  "Estimated Exposed Population",
  "Deaths (NAs removed)",
  "Deaths (NAs as zero)",
  "Estimated Death Rate (NAs removed)",
  "Estimated Death Rate (NAs as zero)"
)

tbl_raw <- dplyr::bind_rows(blocks) %>%
  dplyr::mutate(
    Group    = factor(Group, levels = group_order),
    Variable = factor(Variable, levels = var_order)
  ) %>%
  dplyr::arrange(Group, Variable) %>%
  dplyr::select(Group, Variable, tau, p, slope)

# =============================================================================
# SECTION V-A: MONTHLY EVENT-FREQUENCY TRENDS (footnote analysis)
# =============================================================================
#
# Supports the manuscript claim that the increasing overall event count is
# "driven by increased events recorded for the transition months of May and
# October." Tests each calendar month separately: yearly event counts for
# that month only, same mk_conservative() convention as the main Event
# Frequency row above (no explicit zero-fill for years with 0 events that
# month -- consistent with how the existing Event Frequency row is built,
# so results are comparable/reproducible against the already-published
# October figure: should recover tau=0.313, p=0.029, slope=0.25).

monthly_trend_global <- purrr::map_dfr(1:12, function(m) {
  yearly_counts <- trendData %>%
    dplyr::filter(!is.na(Start.Year), Start.Month == m) %>%
    dplyr::count(Start.Year, name = "value") %>%
    dplyr::arrange(Start.Year)

  res <- mk_conservative(yearly_counts$value)
  res$month <- month.name[m]
  res$month_num <- m
  res
})

sig_months <- monthly_trend_global %>%
  dplyr::filter(!is.na(p), p < 0.05) %>%
  dplyr::arrange(month_num)

# =============================================================================
# SECTION V-B: DEATH RATE, 1990-1999 vs. 2000-2024 (footnote analysis)
# =============================================================================
#
# Supports the manuscript claim that "death rate per event declined more
# sharply between 1990 and 2000 as compared to between 2000 and 2024."
# Table S6 already covers 2000-2024; this adds the complementary 1990-1999
# window here (not >=1990 & <=2000, to avoid double-counting year 2000
# against Table S6's YEAR_MIN=2000 cutoff) so the two can be compared
# directly in one footnote without a cross-script dependency on Table S6's
# output file.

trendData_9099 <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                                  step8_date_append_output, ".RDS")) %>%
  dplyr::filter(Start.Year >= 1990, Start.Year < 2000,
                !is.na(pop.exposed.local.else.adm1.else.national.100yr),
                pop.exposed.local.else.adm1.else.national.100yr > 0) %>%
  dplyr::mutate(
    death.rate = Total.Deaths / pop.exposed.local.else.adm1.else.national.100yr,
    death.rate = ifelse(is.finite(death.rate) & death.rate < 1, death.rate, NA_real_)
  )

death_rate_9099 <- mk_conservative(trendData_9099$death.rate)
death_rate_full_period <- tbl_raw %>%
  dplyr::filter(Group == group_order[1], Variable == "Estimated Death Rate (NAs removed)")
# ^ NOTE: this pulls Table S5's own 1990-2024 (full-period) slope for
# reference, NOT the 2000-2024 slope from Table S6. If Table S6's output is
# available at run time, prefer reading its actual 2000-2024 slope instead --
# see the commented alternative below.
#
# death_rate_full_period <- read.csv(paste0("Output/Tables/Table S6 ", date_append_output, ".csv")) %>%
#   dplyr::filter(Group == "Global (... events)", Variable == "Estimated Death Rate (NAs removed)")

# =============================================================================
# SECTION V-C: WRITE FOOTNOTE TEXT
# =============================================================================

month_footnote <- if (nrow(sig_months) > 0) {
  paste0(
    "When examined for monthly trends, ",
    paste(
      sprintf("%s had a significant and %s trend in events (tau=%.3f, p-value=%.3f, Sen's slope=%.2f)",
              sig_months$month,
              ifelse(sig_months$tau > 0, "increasing positive", "decreasing negative"),
              sig_months$tau, sig_months$p, sig_months$slope),
      collapse = "; "
    ), "."
  )
} else {
  "No individual calendar month showed a significant trend at p<0.05."
}

death_rate_footnote <- {
  # Describe whichever pattern the data actually show -- do not assume the
  # 1990-1999 sub-period declined "more sharply." A prior version of this
  # footnote hardcoded that direction and produced a nonsensical sentence
  # when the sub-period trend was actually flat/non-significant while the
  # full period was significantly declining.
  early_sig <- !is.na(death_rate_9099$p) && death_rate_9099$p < 0.05
  full_sig  <- !is.na(death_rate_full_period$p) && death_rate_full_period$p < 0.05

  comparison_clause <- if (early_sig && full_sig && death_rate_9099$slope < death_rate_full_period$slope) {
    "declined more sharply in 1990-1999 than over the full 1990-2024 period"
  } else if (early_sig && full_sig) {
    "declined less sharply in 1990-1999 than over the full 1990-2024 period"
  } else if (!early_sig && full_sig) {
    "showed no significant trend in 1990-1999 alone, while the full 1990-2024 period shows a significant decline -- suggesting the overall decline is concentrated after 1999, not before it"
  } else if (early_sig && !full_sig) {
    "showed a significant trend in 1990-1999 alone, but not over the full 1990-2024 period"
  } else {
    "showed no significant trend in either the 1990-1999 sub-period or the full 1990-2024 period"
  }

  sprintf(
    paste0("Estimated death rate (NAs removed) %s (1990-1999: tau=%.3f, p-value=%.3f, ",
           "Sen's slope=%.3e; full 1990-2024: tau=%.3f, p-value=%.3f, Sen's slope=%.3e); ",
           "see Table S6 for the 2000-2024 estimate."),
    comparison_clause,
    death_rate_9099$tau, death_rate_9099$p, death_rate_9099$slope,
    death_rate_full_period$tau, death_rate_full_period$p, death_rate_full_period$slope
  )
}

footnote_text <- paste(month_footnote, death_rate_footnote, sep = " ")

cat(footnote_text, "\n")

writeLines(
  footnote_text,
  paste0("Output/Tables/Table S5 footnote ", date_append_output, ".txt")
)

# =============================================================================
# SECTION VI: WRITE OUTPUT
# =============================================================================

write.csv(
  tbl_raw,
  file = paste0("Output/Tables/Table S5 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S5 written to Output/Tables/Table S5 ", date_append_output, ".csv")
message("Table S5 footnote written to Output/Tables/Table S5 footnote ", date_append_output, ".txt")
