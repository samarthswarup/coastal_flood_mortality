# Table S8
#
# Purpose: Calculates Mann–Kendall trend statistics and Sen slopes for 2000–2024 events and GNI groups.
#
# Inputs:
#   - Step 8: trendData RDS
#
# Outputs:
#   - Output/Tables/Table S8 <date>.csv
#   - Output/Tables/Table S8 footnote <date>.txt
#
# R packages: dplyr, tidyr, tibble, modifiedmk, trend, scales
# Run from the analysis directory containing Intermediate outputs/.
#

rm(list = ls())

library(dplyr)
library(tidyr)
library(tibble)
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

YEAR_MIN <- 2000

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

death_rate_00_24_global <- tbl_raw %>%
  dplyr::filter(Group == group_order[1], Variable == "Estimated Death Rate (NAs removed)")

footnote_text <- sprintf(
  paste0("Estimated death rate (NAs removed), 2000-2024: tau=%.3f, p-value=%.3f, ",
         "Sen's slope=%.3e. See Table S7 for the 1990-1999 vs. full 1990-2024 period ",
         "comparison; the 1990-1999 sub-period showed no significant trend, so the ",
         "overall 1990-2024 decline is concentrated in this 2000-2024 window."),
  death_rate_00_24_global$tau, death_rate_00_24_global$p, death_rate_00_24_global$slope
)

write.csv(
  tbl_raw,
  file = paste0("Output/Tables/Table S8 ", date_append_output, ".csv"),
  row.names = FALSE
)

writeLines(
  footnote_text,
  paste0("Output/Tables/Table S8 footnote ", date_append_output, ".txt")
)

message("Table S8 written to Output/Tables/Table S8 ", date_append_output, ".csv")
message("Table S8 footnote written to Output/Tables/Table S8 footnote ", date_append_output, ".txt")
