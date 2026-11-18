# Figure 3: Global mortality projections by scenario and methodological stage
#
# Script dependencies:
#   - Step 9: decomposition_table_population_methodology_<date>.csv
# R packages: dplyr, ggplot2, RColorBrewer, scales
# Outputs:
#   - Output/Plots/Fig 3 <date>.pdf
# Computation:
#   - Plots percentage changes in projected annual deaths relative to the historical baseline; transforms saved annual-death confidence bounds into percentage units.
#
# Execute from the project analysis directory containing Intermediate outputs/.

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(ggplot2)
library(scales)
library(RColorBrewer)

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

step9_date_append_output <- "2026-11-14"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")


# Select scenarios and stage comparisons.
scenarios_to_plot <- c(
  "SSP1.19",
  "SSP1.26_med",
  "SSP2.45",
  "SSP3.70",
  "SSP5.85_med",
  "BOUND_worst",
  "BOUND_best"
)


stage_top    <- " + Population (exposure only)"
stage_bottom <- " + Population (both) & HDI & coastal growth 2"

stage_labels <- c("SLR + population exposure only",
                  "+ HDI + coastal growth")
names(stage_labels) <- c(stage_top, stage_bottom)


# Read Step 9 global projections.
decomp_table <- read.csv(
  paste0("Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_",
         step9_date_append_output, ".csv"),
  stringsAsFactors = FALSE
)


baseline <- decomp_table %>%
  filter(Scenario == "Historical baseline") %>%
  dplyr::select(Year, hist_ann_deaths = Ann_deaths)

# Prepare plotted mortality changes and confidence bounds.
plot_data <- decomp_table %>%
  filter(Scenario %in% c(stage_top, stage_bottom), SSP %in% scenarios_to_plot) %>%
  left_join(baseline, by = "Year") %>%
  mutate(
    Pct_lcl = 100 * (Ann_deaths_lcl - hist_ann_deaths) / hist_ann_deaths,
    Pct_ucl = 100 * (Ann_deaths_ucl - hist_ann_deaths) / hist_ann_deaths,
    Stage_label = factor(stage_labels[Scenario], levels = stage_labels),
    Year = factor(Year, levels = c("2050", "2100"))
  )

missing_scenarios <- setdiff(scenarios_to_plot, unique(plot_data$SSP))
if (length(missing_scenarios) > 0) {
  warning("Requested scenario(s) not found in decomp_table: ",
          paste(missing_scenarios, collapse = ", "))
}

scenario_order <- plot_data %>%
  filter(Scenario == stage_bottom, Year == "2050") %>%
  arrange(Pct_vs_historical) %>%
  pull(SSP)

scenario_order <- c(scenario_order, setdiff(scenarios_to_plot, scenario_order))

plot_data <- plot_data %>%
  mutate(SSP = factor(SSP, levels = scenario_order))


n_scen <- length(scenario_order)
pal_full <- RColorBrewer::brewer.pal(9, "YlGnBu")
pal <- colorRampPalette(pal_full[3:9])(n_scen)
names(pal) <- scenario_order

panel_letters <- expand.grid(
  Stage_label = levels(plot_data$Stage_label),
  Year = levels(plot_data$Year)
) %>%
  arrange(Stage_label, Year) %>%
  mutate(label = paste0("(", letters[1:4], ")"))

# Generate and save Figure 3.
figure.3 <- ggplot(plot_data, aes(x = SSP, y = Pct_vs_historical, color = SSP)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = Pct_lcl, ymax = Pct_ucl), width = 0.15, linewidth = 0.7) +
  geom_point(size = 2.6) +
  geom_text(data = panel_letters, aes(x = -Inf, y = Inf, label = label),
            inherit.aes = FALSE, hjust = -0.3, vjust = 1.5,
            fontface = "bold", size = 4.2) +
  facet_grid(Stage_label ~ Year,
             labeller = labeller(Year = c(`2050` = "By 2050", `2100` = "By 2100"))) +
  scale_color_manual(values = pal) +
  labs(
    x = "Scenario",
    y = "Projected change in annual deaths\nvs. historical baseline (%)",
    title = "Scenario projections of coastal flood mortality",
    caption = paste0(
      "Top row: SLR + population exposure only (no population-level mortality response). Bottom row: fully\n",
      "decomposed estimate (adds population growth as both increased exposure AND a national-population covariate\n",
      "in the mortality regression, plus projected HDI and a coastal growth multiplier).\n",
      "Scenarios ordered by ascending bottom-row 2050 value. Error bars = 95% CI."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, size = 8, color = "grey40")
  )


ggsave(
  filename = paste0("Output/Plots/Fig 3 ", date_append_output, ".pdf"),
  plot = figure.3,
  width = 8, height = 8, units = "in", dpi = 300
)

message("Figure 3 written to Output/Plots/Fig 3 ", date_append_output, ".pdf")
