# Table S7
#
# Purpose: Calculates Mann–Kendall trend statistics and Sen slopes for 1990–2024 events and GNI groups.
#
# Inputs:
#   - Step 8: trendData RDS
#
# Outputs:
#   - Output/Tables/Table S7 <date>.csv
#   - Output/Tables/Table S7 footnote <date>.txt
#
# R packages: dplyr, tidyr, tibble, purrr, modifiedmk, trend, scales
# Run from the analysis directory containing Intermediate outputs/.
#

rm(list = ls())

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(modifiedmk)
library(trend)
library(scales)

project_dir <- getwd()
if (!dir.exists(file.path(project_dir, "Intermediate outputs")) &&
    dir.exists(file.path(project_dir, "Claude", "Intermediate outputs"))) {
  project_dir <- file.path(project_dir, "Claude")
}
if (!dir.exists(file.path(project_dir, "Intermediate outputs"))) {
  stop("Run this script from the project analysis directory containing Intermediate outputs/.")
}
setwd(project_dir)

dir.create("Output/Tables", showWarnings = FALSE, recursive = TRUE)

step8_date_append_output <- "2026-11-13"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

YEAR_MIN <- 1990

trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

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

trendData$gni.group <- factor(trendData$gni.group, levels = c("LI", "LMI", "UMI", "HI"))

mk_conservative <- function(y) {
  y <- as.numeric(y)
  ok <- is.finite(y)
  if (sum(ok) < 3L) {
    return(tibble::tibble(tau = NA_real_, p = NA_real_, slope = NA_real_))
  }

  mk <- modifiedmk::mkttest(y[ok])

  get_field <- function(obj, candidates) {
    nms <- names(obj)
    hit <- intersect(candidates, nms)
    if (length(hit) == 0) return(NA_real_)
    as.numeric(unname(obj[hit[1]]))
  }

  tau_val <- get_field(mk, c("Tau", "tau", "Kendall's tau", "kendall.tau"))
  p_val   <- get_field(mk, c("P-value", "pvalue", "sl", "Pr(>|z|)", "p.value", "p"))

  ss <- trend::sens.slope(y[ok])

  tibble::tibble(
    tau   = tau_val,
    p     = p_val,
    slope = as.numeric(ss$estimates)
  )
}

make_block_conservative <- function(label, dat) {

  freq_year <- dat %>%
    filter(!is.na(Start.Year)) %>%
    count(Start.Year, name = "value") %>%
    arrange(Start.Year)

  freq_res <- mk_conservative(freq_year$value) %>%
    mutate(Variable = "Event Frequency")

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
           "see Table S8 for the 2000-2024 estimate."),
    comparison_clause,
    death_rate_9099$tau, death_rate_9099$p, death_rate_9099$slope,
    death_rate_full_period$tau, death_rate_full_period$p, death_rate_full_period$slope
  )
}

footnote_text <- paste(month_footnote, death_rate_footnote, sep = " ")

cat(footnote_text, "\n")

writeLines(
  footnote_text,
  paste0("Output/Tables/Table S7 footnote ", date_append_output, ".txt")
)

write.csv(
  tbl_raw,
  file = paste0("Output/Tables/Table S7 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S7 written to Output/Tables/Table S7 ", date_append_output, ".csv")
message("Table S7 footnote written to Output/Tables/Table S7 footnote ", date_append_output, ".txt")
