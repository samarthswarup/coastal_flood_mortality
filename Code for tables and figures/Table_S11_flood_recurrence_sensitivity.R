# Table S11
#
# Purpose: Summarizes event and country-average exposed populations and death rates at four flood recurrence intervals.
#
# Inputs:
#   - Step 8: trendData RDS
#
# Outputs:
#   - Output/Tables/Table S11 <date>.csv
#
# R packages: dplyr
# Run from the analysis directory containing Intermediate outputs/.
#

rm(list = ls())

library(dplyr)

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

trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

df.adj <- trendData
df.adj$Country_Code <- df.adj$SLIIDERS.country.code.flat

df.since.1990.no.prk <- df.adj[
  df.adj$Start.Year >= 1990 & df.adj$SLIIDERS.country.code.flat != "PRK", ]

df.since.1990.no.prk.no.0 <- df.adj[
  df.adj$Start.Year >= 1990 & df.adj$SLIIDERS.country.code.flat != "PRK" &
    df.adj$total.deaths.na.as.0 > 0, ]

flood.recur <- c("10", "100", "1000", "10000")
exp.name.vec <- paste0("pop.exposed.local.else.adm1.else.national.", flood.recur, "yr")

per.event.pop <- unlist(lapply(exp.name.vec, function(x)
  mean(df.since.1990.no.prk.no.0[[x]], na.rm = TRUE)))
per.event.pop.M <- per.event.pop / 10^6

per.event.death.rate <- unlist(lapply(exp.name.vec, function(x)
  mean(df.since.1990.no.prk.no.0$total.deaths.na.as.0 /
         df.since.1990.no.prk.no.0[[x]], na.rm = TRUE)))

per.country.avg.per.event.pop <- unlist(lapply(exp.name.vec, function(x)
  mean(unlist(lapply(unique(df.since.1990.no.prk$Country_Code), function(y)
    mean((df.since.1990.no.prk[[x]])[which(df.since.1990.no.prk$Country_Code == y)],
         na.rm = TRUE))))))
per.country.avg.per.event.pop.M <- per.country.avg.per.event.pop / 10^6

per.country.avg.per.event.death.rate <- unlist(lapply(exp.name.vec, function(x)
  mean(unlist(lapply(unique(df.since.1990.no.prk$Country_Code), function(y)
    mean((df.since.1990.no.prk$total.deaths.na.as.0)[which(df.since.1990.no.prk$Country_Code == y)] /
           (df.since.1990.no.prk[[x]])[which(df.since.1990.no.prk$Country_Code == y)],
         na.rm = TRUE))))))

table.s11 <- cbind.data.frame(
  flood.recur,
  per.event.pop.M = round(per.event.pop.M, 2),
  per.event.death.rate = round(per.event.death.rate, 4),
  per.country.avg.per.event.pop.M = round(per.country.avg.per.event.pop.M, 2),
  per.country.avg.per.event.death.rate = round(per.country.avg.per.event.death.rate, 4)
)

write.csv(
  table.s11,
  file = paste0("Output/Tables/Table S11 ", date_append_output, ".csv"),
  row.names = FALSE
)

message("Table S11 written to Output/Tables/Table S11 ", date_append_output, ".csv")
