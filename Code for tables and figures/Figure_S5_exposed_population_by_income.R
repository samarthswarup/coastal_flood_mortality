# Figure S5: Estimated exposed population by GNI income category
#
# Script dependencies:
#   - Step 7: EMDAT events with exposed pops, HDI GNI and prev events <date>.xlsx
# R packages: dplyr, ggstatsplot, patchwork, readxl, scales
# Outputs:
#   - Output/Plots/Fig S5 <date>.pdf
# Computation:
#   - Compares estimated exposed populations for all events and the subset with subnational locations using Welch ANOVA.
#
# Execute from the project analysis directory containing Intermediate outputs/.

rm(list = ls())

library(dplyr)
library(readxl)
library(ggstatsplot)
library(scales)
library(patchwork)

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

step7_date_append_output <- "2026-07-05"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")


# Read Step 7 event data.
emdat.data.filtered <- read_excel(
  paste0("Intermediate outputs/Output from Step 7/",
         "EMDAT events with exposed pops, HDI GNI and prev events ",
         step7_date_append_output, ".xlsx")
) %>%
  filter(Start.Year >= 1990)


pal <- c(LI = "#F8766D", LMI = "#C49A00", UMI = "#00BA38", HI = "#619CFF")

# Prepare all-events comparison.
pA <- emdat.data.filtered %>%
  dplyr::filter(
    !is.na(gni.group),
    !is.na(pop.exposed.local.else.adm1.else.national.100yr),
    pop.exposed.local.else.adm1.else.national.100yr > 0
  ) %>%
  dplyr::mutate(
    gni.group = factor(gni.group, levels = c("LI", "LMI", "UMI", "HI"))
  ) %>%
  ggstatsplot::ggbetweenstats(
    x = gni.group,
    y = pop.exposed.local.else.adm1.else.national.100yr,
    type = "parametric",
    var.equal = FALSE,
    mean.plotting = TRUE,
    mean.ci = TRUE,
    pairwise.comparisons = FALSE,
    xlab = "GNI income group",
    ylab = "Estimated exposed population",
    title = "A. All coastal flooding events"
  ) +
  scale_y_continuous(labels = scientific) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal)


# Prepare subnational-location comparison.
pB <- emdat.data.filtered %>%
  filter(
    !is.na(gni.group),
    (!is.na(pop.exposed.local.if.available.100yr) |
       !is.na(pop.exposed.adm1.if.available.100yr))
  ) %>%
  mutate(
    gni.group = factor(gni.group, levels = c("LI", "LMI", "UMI", "HI"))
  ) %>%
  ggbetweenstats(
    x = gni.group,
    y = pop.exposed.local.else.adm1.else.national.100yr,
    type = "parametric",
    var.equal = FALSE,
    mean.plotting = TRUE,
    mean.ci = TRUE,
    pairwise.comparisons = FALSE,
    xlab = "GNI income group",
    ylab = "Estimated exposed population",
    title = "B. Events with sub-country locations"
  ) +
  scale_y_continuous(labels = scientific) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal)


# Combine and save Figure S5.
figure.s5 <- pA + pB +
  plot_annotation(
    title = "Comparison of estimated exposed populations grouped by country income category"
  )

ggsave(
  filename = paste0("Output/Plots/Fig S5 ", date_append_output, ".pdf"),
  plot = figure.s5,
  width = 12, height = 6, units = "in",
  dpi = 300
)

message("SI Figure S5 written to Output/Plots/Fig S5 ", date_append_output, ".pdf")

