# Figure S3: Historical trends in coastal flood events and mortality
#
# Script dependencies:
#   - Step 8: trendData <date>.RDS
# R packages: dplyr, ggplot2, scales, tibble, tidyr
# Outputs:
#   - Output/Plots/Fig S3 <date>.pdf
# Computation:
#   - Plots annual event counts and event-level exposure, deaths and death rates for 1990–2024; includes LOESS smoothers and confidence bands.
#
# Execute from the project analysis directory containing Intermediate outputs/.

rm(list = ls())

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(scales)

project_dir <- getwd()
if (!dir.exists(file.path(project_dir, "Intermediate outputs")) &&
    dir.exists(file.path(project_dir, "Claude", "Intermediate outputs"))) {
  project_dir <- file.path(project_dir, "Claude")
}
if (!dir.exists(file.path(project_dir, "Intermediate outputs"))) {
  stop("Run this script from the directory containing Intermediate outputs/.")
}
setwd(project_dir)

dir.create("Output/Plots", showWarnings = FALSE, recursive = TRUE)

step8_date_append_output <- "2026-11-13"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

YEAR_MIN <- 1990


# Read historical event data.
trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))


trendData <- trendData %>%
  filter(Start.Year >= YEAR_MIN,
         !is.na(pop.exposed.local.else.adm1.else.national.100yr),
         pop.exposed.local.else.adm1.else.national.100yr > 0) %>%
  mutate(
    death.rate.na.as.0 = total.deaths.na.as.0 / pop.exposed.local.else.adm1.else.national.100yr,
    death.rate.na.as.0 = ifelse(is.finite(death.rate.na.as.0) & death.rate.na.as.0 < 1, death.rate.na.as.0, NA_real_)
  )

trendData$gni.group <- factor(trendData$gni.group, levels = c("LI", "LMI", "UMI", "HI"))


# Assemble annual-frequency and event-level series.
make_events <- function(label, dat) {
  freq <- dat %>%
    filter(!is.na(Start.Year)) %>%
    count(Start.Year, name = "value") %>%
    mutate(Date = as.Date(paste0(Start.Year, "-01-01")),
           Variable = "Event Frequency") %>%
    dplyr::select(Date, value, Variable)

  other <- dat %>%
    filter(!is.na(Start.Date)) %>%
    transmute(
      Date = Start.Date,
      `Estimated Exposed Population` = pop.exposed.local.else.adm1.else.national.100yr,
      `Deaths (NAs as zero)`         = total.deaths.na.as.0,
      `Estimated Death Rate (NAs as zero)` = death.rate.na.as.0
    ) %>%
    pivot_longer(-Date, names_to = "Variable", values_to = "value")

  bind_rows(freq, other) %>%
    mutate(Group = label)
}

events_all <- bind_rows(
  make_events("Global", trendData),
  make_events("LI",  filter(trendData, gni.group == "LI")),
  make_events("LMI", filter(trendData, gni.group == "LMI")),
  make_events("UMI", filter(trendData, gni.group == "UMI")),
  make_events("HI",  filter(trendData, gni.group == "HI"))
)

var_order <- c(
  "Event Frequency",
  "Estimated Exposed Population",
  "Deaths (NAs as zero)",
  "Estimated Death Rate (NAs as zero)"
)

events_all$Variable <- factor(events_all$Variable, levels = var_order)
events_all$Group <- factor(events_all$Group, levels = c("Global", "LI", "LMI", "UMI", "HI"))


pal <- c(Global = "black", LI = "#F8766D", LMI = "#C49A00", UMI = "#00BA38", HI = "#619CFF")

# Draw and save Figure S3.
figure.s3 <- ggplot(events_all, aes(x = Date, y = value, color = Group, fill = Group)) +
  geom_point(alpha = 0.5, size = 1.1) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 0.9) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    x = "Date",
    y = NULL,
    color = "GNI group",
    fill = "GNI group",
    title = "Historical trends in coastal flood events, exposure, and mortality (1990-2024)",
    caption = paste(
      "Points = individual events at their recorded date (Event Frequency = yearly counts, plotted at Jan 1 of each year).",
      "Lines = LOESS-smoothed trend with 95% CI band, shown for visual reference only -- not a statistical trend test.",
      "Formal Mann-Kendall/Sen's slope trend tests (fit to the chronological per-event sequence) are reported in SI Table S7.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, size = 8, color = "grey40")
  )


ggsave(
  filename = paste0("Output/Plots/Fig S3 ", date_append_output, ".pdf"),
  plot = figure.s3,
  width = 10, height = 8, units = "in", dpi = 300
)

message("Figure S3 written to Output/Plots/Fig S3 ", date_append_output, ".pdf")
