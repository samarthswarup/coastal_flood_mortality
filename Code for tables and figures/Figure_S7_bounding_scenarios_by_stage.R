# Figures S7.1 and S7.2: Country-level bounding scenarios across projection stages
#
# Script dependencies:
#   - Step 8: CountryDeathCIs <date>.csv
#   - Step 9: Country_level_master_database_<date>.csv
# R packages: dplyr, ggplot2
# Outputs:
#   - Output/Plots/Fig S7.1 <date>.pdf
#   - Output/Plots/Fig S7.2 <date>.pdf
# Computation:
#   - Computes country-level percentage changes and interval bounds for best- and worst-case scenarios, grouped by projection stage, for 2050 and 2100.
#
# Execute from the project analysis directory containing Intermediate outputs/.

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(ggplot2)

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
step9_date_append_output <- "2026-11-14"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

N_YEARS <- 35   # matches Step 9's historical study period length


scenarios_use <- c("BOUND_worst", "BOUND_best")

stage_labels <- c(
  " SLR only" = "(1)",
  " + Population (exposure only)" = "(2)",
  " + Population (exposure + NB covariate)" = "(3)",
  " + Population (both) & HDI" = "(4)",
  " + Population (both) & HDI & coastal growth 1" = "(5)",
  " + Population (both) & HDI & coastal growth 2" = "(6)"
)

MIN_EVENTS <- 50


# Read historical and projected country mortality.
ci_base <- read.csv(paste0(
  "Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ",
  step8_date_append_output, ".csv"))

country_master <- read.csv(
  paste0("Intermediate outputs/Output from Step 9/Country_level_master_database_", step9_date_append_output, ".csv"),
  stringsAsFactors = FALSE
)


# Calculate historical baselines and projected percentage changes.
hist_country <- ci_base %>%
  transmute(Country_Code, hist_ann_deaths = meanDeaths * nEvents / N_YEARS,
            nEvents_hist = nEvents)

proj_data <- country_master %>%
  filter(methodology %in% names(stage_labels), ssp %in% scenarios_use) %>%
  left_join(hist_country, by = "Country_Code") %>%
  filter(!is.na(hist_ann_deaths), hist_ann_deaths > 0) %>%
  filter(nEvents_hist >= MIN_EVENTS) %>%
  mutate(
    `Methodology stage` = factor(stage_labels[methodology], levels = stage_labels),
    ssp = factor(ssp, levels = scenarios_use),
    pctInc     = 100 * (ann_deaths - hist_ann_deaths) / hist_ann_deaths,
    pctInc_lcl = 100 * (ann_deaths_lcl - hist_ann_deaths) / hist_ann_deaths,
    pctInc_ucl = 100 * (ann_deaths_ucl - hist_ann_deaths) / hist_ann_deaths
  )

if (nrow(proj_data) == 0) {
  stop("No countries with >= ", MIN_EVENTS, " historical events found for scenarios '",
       paste(scenarios_use, collapse = "', '"), "' -- check ssp/methodology labels match ",
       "country_master's actual values.")
}

country_order <- proj_data %>%
  filter(year == "2050", methodology == names(stage_labels)[6], ssp == "BOUND_worst") %>%
  arrange(desc(pctInc)) %>%
  pull(Country_Code)
country_order <- c(country_order, setdiff(unique(proj_data$Country_Code), country_order))

proj_data <- proj_data %>%
  mutate(Country_Code = factor(Country_Code, levels = country_order))


pal <- c(BOUND_worst = "#B2182B", BOUND_best = "#2166AC")
scenario_display_labels <- c(BOUND_worst = "Worst case", BOUND_best = "Best case")

# Generate one figure for each projection year.
make_year_figure <- function(yr) {
  cur <- proj_data %>% filter(year == yr)

  ggplot(cur, aes(x = Country_Code, y = pctInc, color = ssp)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = pctInc_lcl, ymax = pctInc_ucl),
                  position = position_dodge(width = 0.6), width = 0.2, linewidth = 0.5) +
    geom_point(position = position_dodge(width = 0.6), size = 1.6) +
    facet_wrap(~`Methodology stage`, ncol = 2, scales = "free_y") +
    scale_color_manual(values = pal, labels = scenario_display_labels, name = "Scenario") +
    labs(
      x = "Country",
      y = paste0("Percent change in average deaths (", yr, ")")
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      legend.position = "bottom"
    )
}

figure_2050 <- make_year_figure("2050")
figure_2100 <- make_year_figure("2100")


ggsave(
  filename = paste0("Output/Plots/Fig S7.1 ", date_append_output, ".pdf"),
  plot = figure_2050,
  width = 12, height = 11, units = "in", dpi = 300
)

ggsave(
  filename = paste0("Output/Plots/Fig S7.2 ", date_append_output, ".pdf"),
  plot = figure_2100,
  width = 12, height = 11, units = "in", dpi = 300
)

message("SI Figure S7.1 (2050) and S7.2 (2100) written to Output/Plots/")
