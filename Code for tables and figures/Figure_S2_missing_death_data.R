# Figure S2: Missing death counts by country income category
#
# Script dependencies:
#   - Step 8: trendData <date>.RDS
# R packages: dplyr
# Outputs:
#   - Output/Plots/Fig S2 <date>.pdf
# Computation:
#   - Calculates the fraction of events with zero-imputed missing death counts for each baseline GNI income category.
#
# Execute from the project analysis directory containing Intermediate outputs/.

rm(list = ls())

library(dplyr)

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


# Calculate mortality rates and GNI-category summaries.
df <- trendData %>%
  filter(Start.Year > 1989) %>%
  mutate(death.rate = total.deaths.na.as.0 / pop.exposed.local.else.adm1.else.national.100yr)

df.gni  <- df      %>% filter(!is.na(gni.group))
df.gnid <- df.gni  %>% filter(!is.na(death.rate))
df.gnidr <- df.gnid %>% filter(death.rate < 1)

df.gnidr$gni.group.baseline <- factor(df.gnidr$gni.group.baseline,
                                       levels = c("LI", "LMI", "UMI", "HI"))
df.gnidr$zeroDeath <- ifelse(df.gnidr$death.rate == 0, 1, 0)

zeroTable <- table(df.gnidr$zeroDeath, df.gnidr$gni.group.baseline)
zeroTable <- zeroTable[2, ] / (zeroTable[1, ] + zeroTable[2, ])


# Draw and save Figure S2.
pdf(paste0("Output/Plots/Fig S2 ", date_append_output, ".pdf"),
    width = 5, height = 5)

barplot(zeroTable, col = c("#F8766D", "#C49A00", "#00BA38", "#619CFF"),
        ylim = c(0, 0.35), xlab = "Income Category",
        ylab = "Proportion of events with missing death data",
        las = 2)
lines(c(0.1, 4.9), c(0.05, 0.05), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.10, 0.10), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.15, 0.15), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.20, 0.20), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.25, 0.25), lty = 2, col = "black")
lines(c(0.1, 4.9), c(0.30, 0.30), lty = 2, col = "black")

box()
dev.off()

message("Figure written to Output/Plots/Fig S2 ", date_append_output, ".pdf",
        ".")

