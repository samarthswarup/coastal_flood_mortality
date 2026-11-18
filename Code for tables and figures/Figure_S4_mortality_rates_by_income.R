# Figure S4: Event mortality rates by GNI income category
#
# Script dependencies:
#   - Step 8: trendData <date>.RDS
# R packages: dplyr, ggstatsplot, patchwork, rlang
# Outputs:
#   - Output/Plots/Fig S4 <date>.pdf
# Computation:
#   - Produces four income-group comparisons with alternate missing-death handling and rate transformations; uses Welch ANOVA.
#
# Execute from the project analysis directory containing Intermediate outputs/.

rm(list = ls())

library(dplyr)
library(patchwork)
library(ggstatsplot)
library(rlang)

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

GROUPING_VAR <- "gni.group"


# Read historical event data.
trendData <- readRDS(paste0("Intermediate outputs/Output from Step 8/trendData ",
                             step8_date_append_output, ".RDS"))

df <- trendData %>%
  filter(Start.Year > 1989) %>%
  mutate(death.rate = total.deaths.na.as.0 / pop.exposed.local.else.adm1.else.national.100yr)

filtered.df <- df %>%
  filter(!is.na(total.deaths.reported)) %>%
  mutate(death.rate.local = total.deaths.reported / pop.exposed.local.else.adm1.else.national.100yr)


pal <- c(LI = "#F8766D", LMI = "#C49A00", UMI = "#00BA38", HI = "#619CFF")
levels_gni <- c("LI", "LMI", "UMI", "HI")

theme_panel <- theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.tag = element_text(face = "bold", size = 14))

# Define Welch-ANOVA plotting helper.
between_plot <- function(data, xvar, yvar, tag, title, ylab) {
  data <- data %>%
    mutate(!!xvar := factor(.data[[xvar]], levels = levels_gni))

  lev <- levels(droplevels(data[[as_string(xvar)]]))
  vals <- pal[lev]; names(vals) <- lev

  ggstatsplot::ggbetweenstats(
    data              = data,
    x                 = !!xvar,
    y                 = !!sym(yvar),
    type              = "parametric",
    var.equal         = FALSE,
    mean.plotting     = TRUE,
    mean.ci           = TRUE,
    pairwise.comparisons = FALSE,
    messages          = FALSE,
    results.subtitle  = TRUE,
    ggtheme           = theme_minimal(),
    xlab              = "World Bank GNI category",
    ylab              = ylab
  ) +
    scale_fill_manual(values = vals) +
    scale_color_manual(values = vals) +
    labs(title = title, tag = tag) +
    theme_panel
}


# Calculate rates and transformations for four panels.
A_data <- df %>%
  filter(!is.na(.data[[GROUPING_VAR]]),
         !is.na(pop.exposed.local.else.adm1.else.national.100yr),
         pop.exposed.local.else.adm1.else.national.100yr > 0) %>%
  mutate(
    death.rate = total.deaths.na.as.0 / pop.exposed.local.else.adm1.else.national.100yr,
    !!GROUPING_VAR := factor(.data[[GROUPING_VAR]], levels = levels_gni)
  ) %>%
  filter(is.finite(death.rate), death.rate < 1)

pA <- between_plot(
  data  = A_data,
  xvar  = sym(GROUPING_VAR),
  yvar  = "death.rate",
  tag   = "A",
  title = "NA deaths -> 0 (Models 1,2,3,5,6,7,8) -- Welch ANOVA",
  ylab  = "Deaths per exposed person"
)

B_data <- filtered.df %>%
  filter(!is.na(.data[[GROUPING_VAR]]),
         !is.na(pop.exposed.local.else.adm1.else.national.100yr),
         pop.exposed.local.else.adm1.else.national.100yr > 0) %>%
  mutate(
    death.rate.local = total.deaths.reported / pop.exposed.local.else.adm1.else.national.100yr,
    !!GROUPING_VAR := factor(.data[[GROUPING_VAR]], levels = levels_gni)
  ) %>%
  filter(is.finite(death.rate.local), death.rate.local < 1)

pB <- between_plot(
  data  = B_data,
  xvar  = sym(GROUPING_VAR),
  yvar  = "death.rate.local",
  tag   = "B",
  title = "NAs dropped (Model 4) -- Welch ANOVA",
  ylab  = "Deaths per exposed person"
)

C_data <- A_data %>%
  filter(death.rate > 0) %>%
  mutate(logDeathRate = log(death.rate))

pC <- between_plot(
  data  = C_data,
  xvar  = sym(GROUPING_VAR),
  yvar  = "logDeathRate",
  tag   = "C",
  title = "log death rate (nonzero only) -- Welch ANOVA",
  ylab  = "log(deaths per exposed)"
)

min_pos <- suppressWarnings(min(A_data$death.rate[A_data$death.rate > 0], na.rm = TRUE))
shift   <- if (is.finite(min_pos)) min_pos / 2 else 1e-12

D_data <- A_data %>%
  mutate(
    death.rate.shift = death.rate + shift,
    logDeathRate0s   = log(death.rate.shift)
  )

pD <- between_plot(
  data  = D_data,
  xvar  = sym(GROUPING_VAR),
  yvar  = "logDeathRate0s",
  tag   = "D",
  title = paste0("log death rate (zero-shift included; shift = ", signif(shift, 3), ") -- Welch ANOVA"),
  ylab  = "log(deaths per exposed)"
)


# Combine and save Figure S4.
figure.s4 <- (pA + pB) / (pC + pD) +
  plot_annotation(
    title   = "Average event death rates by World Bank GNI category",
    caption = "A uses NA->0; B drops NAs; C logs nonzero-only values; D logs after adding a small positive shift to include zeros. Welch ANOVA (var.equal=FALSE) in all panels."
  )

ggsave(
  filename = paste0("Output/Plots/Fig S4 ", date_append_output, ".pdf"),
  plot     = figure.s4,
  width    = 14, height = 10, units = "in", dpi = 300
)

message("SI Figure S4 written to Output/Plots/Fig S4 ", date_append_output, ".pdf")
