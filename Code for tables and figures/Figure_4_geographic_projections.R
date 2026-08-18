##########################################
# Figure 4 - Geographic projections        #
#                                          #
# 2026-07-14                               #
# Ryan Calder                              #
# Notes added by Claude                    #
##########################################

# Freestanding figure script, built directly on Step 9's current output --
# not a relocation of any plotting code from 8_ModelFitting_and_SLR_projections.R. Step 9's
# country_master (bootstrap-based CIs, six-stage population/HDI/coastal-
# growth decomposition) has no equivalent in that script's plotting logic,
# which depended on the pre-Step-9 single-treatment forecast pipeline.
#
# Draws exclusively on:
#   - country_master, as saved by Step 9 (final per-country point estimates
#     + bootstrap CIs; no re-derivation)
#   - ci_base, as saved by Step 8 (final per-country historical mean deaths
#     per event + event counts; no re-derivation)
# The only new calculation here is converting country-level ann_deaths /
# ann_deaths_lcl / ann_deaths_ucl into percent-change-vs-historical terms,
# using each country's own historical annual death rate. That rate itself
# uses the exact same formula Step 9 already uses for the GLOBAL historical
# baseline (meanDeaths * nEvents / N_YEARS) -- applied per country instead
# of summed across all countries, not a new derivation.
#
# Maps (4a/4b) and the top-10 detail panels (4c/4d) both use the same
# decomposition stage -- SLR + population exposure only (stage_use,
# matching Figure 3's stage_top) -- rather than the fully decomposed
# estimate. This isolates SLR + national population trends in the figure,
# with no HDI, no NB population covariate, and no coastal-growth
# multiplier anywhere in it.
#
# The maps' median-across-scenarios summary excludes BOUND_worst/
# BOUND_best (scenarios_for_median, 5 scenarios only), while the detail
# panels (4c/4d) show all 7 scenarios individually, including the two
# bounding ones. BOUND_worst/BOUND_best are deliberately decoupled
# extremes (mismatched climate/socioeconomic pairings constructed to
# bracket the outcome space), not internally-consistent "plausible"
# scenarios like the other 5 -- folding them into a median would conflate
# the range of uncertainty with the central estimate. They remain fully
# visible as explicit comparisons in 4c/4d, just not blended into the
# maps' summary statistic.

rm(list = ls())

library(dplyr)
filter <- dplyr::filter
select <- dplyr::select
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(grid)
library(scales)
library(ggplotify)
library(patchwork)
library(RColorBrewer)

# EDIT THIS to the absolute path of your local copy of the project root (the
# folder that contains this script plus "Inputs/", "Intermediate outputs/",
# and "Output/"). This is the ONLY machine-specific line in this script.
setwd("~/coastal_flood_mortality")

dir.create("Output/Plots", showWarnings = FALSE, recursive = TRUE)

# Date suffix identifying which Step 8 / Step 9 runs to pull from
step8_date_append_output <- "2026-07-20"
step9_date_append_output <- "2026-07-20"

# Date suffix for files written by this script
date_append_output <- format(Sys.Date(), "%Y-%m-%d")

N_YEARS <- 35   # matches Step 9's N_YEARS -- historical study period length

# =============================================================================
# USER CONFIGURATION: choose which scenarios/stage feed the maps + top-10
# panels. Same scenario labels and decomposition-stage labels as Figure 3 --
# see that script's header comment for the full valid list of each. Kept in
# sync with Figure 3's defaults so the two figures reflect the same
# methodology; change here if you want Figure 4 to use a different subset.
# =============================================================================

scenarios_to_plot <- c(
  "SSP1.19",
  "SSP1.26_med",
  "SSP2.45",
  "SSP3.70",
  "SSP5.85_med",
  "BOUND_worst",
  "BOUND_best"
)

# The 5 internally-consistent, SSP-matched scenarios only -- excludes
# BOUND_worst/BOUND_best. Used for the MAPS' median calculation:
# BOUND_worst/BOUND_best are deliberately decoupled extremes (mismatched
# climate/socioeconomic pairings constructed to bracket the outcome
# space), not "plausible middle-ground" scenarios -- folding them into a
# median conflates the range of uncertainty with the central estimate.
# They still appear individually in panels 4c/4d as explicit comparisons,
# just not blended into this summary statistic.
scenarios_for_median <- c("SSP1.19", "SSP1.26_med", "SSP2.45", "SSP3.70", "SSP5.85_med")

# Single decomposition stage used for BOTH the maps (4a/4b) and the
# top-10 detail panels (4c/4d): SLR + population exposure only, matching
# Figure 3's stage_top. Maps and detail panels intentionally show the
# same stage, so the two views of the figure stay aligned with each
# other rather than mixing different decomposition levels.
stage_use <- " + Population (exposure only)"

# Minimum historical event count for a country to be eligible for the
# top-10 "largest projected increase" panels (4c/4d).
MIN_EVENTS_FOR_TOP10 <- 10

# =============================================================================
# SECTION I: LOAD UPSTREAM OUTPUT
# =============================================================================

ci_base <- read.csv(paste0("Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ", step8_date_append_output, ".csv"))

country_master <- read.csv(
  paste0("Intermediate outputs/Output from Step 9/Country_level_master_database_", step9_date_append_output, ".csv"),
  stringsAsFactors = FALSE
)

# Loaded ONLY to compute scenario_order the same way Figure 3 does (see
# below) -- ranking by the GLOBAL aggregate value, not a country-averaged
# one, so the two figures show scenarios in the same left-to-right/color
# order for easy cross-referencing.
decomp_table <- read.csv(
  paste0("Intermediate outputs/Output from Step 9/decomposition_table_population_methodology_", step9_date_append_output, ".csv"),
  stringsAsFactors = FALSE
)

# =============================================================================
# SECTION II: BASEMAP + COUNTRY CODE HARMONIZATION
# (identical to Figure 2's basemap setup)
# =============================================================================

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::select(iso_a3, name_long, geometry)

world$iso_a3[which(world$name_long == "France")] <- "FRA"
world$iso_a3[which(world$name_long == "Norway")] <- "NOR"

world$iso_a3[which(world$iso_a3 == "CUW" |
                    world$iso_a3 == "ABW" |
                    world$iso_a3 == "SXM" |
                    world$iso_a3 == "BES")] <- "BES,CUW,ABW,SXM"

coords_lookup <- tibble::tribble(
  ~Country_Code, ~lon,      ~lat,
  "GLP",         -61.551,    16.265,
  "MTQ",         -61.024,    14.641,
  "TKL",         -171.850,   -9.166,
  "REU",          55.536,   -21.115,
  "SXM",         -63.067,    18.041,
  "MYT",          45.166,   -12.827,
  "ATG",         -61.80,    17.11,
  "BHS",         -76.00,    24.30,
  "GRD",         -61.68,    12.12,
  "KNA",         -62.78,    17.36,
  "MSR",         -62.19,    16.74,
  "TCA",         -71.80,    21.80,
  "TTO",         -61.19,    10.50,
  "VCT",         -61.20,    13.25,
  "LCA",         -60.98,    13.91,
  "BRB",         -59.55,    13.19,
  "DMA",         -61.36,    15.41,
  "PRI",         -66.59,    18.22,
  "VIR",         -64.80,    18.35,
  "DOM",         -70.50,    18.70,
  "JAM",         -77.30,    18.10,
  "VGB",         -64.62,    18.42,
  "AIA",         -63.07,    18.22,
  "CYM",         -81.37,    19.31,
  "BMU",         -64.78,    32.30,
  "BLM",         -62.85,    17.90,
  "MAF",         -63.05,    18.07,
  "COK",        -159.78,   -21.23,
  "FJI",         178.00,   -17.80,
  "NIU",        -169.87,   -19.05,
  "TON",        -175.20,   -21.10,
  "TUV",         179.20,    -8.52,
  "WSM",        -172.10,   -13.80,
  "MHL",         171.20,     7.10,
  "VUT",         167.90,   -16.30,
  "SLB",         160.00,    -9.60,
  "FSM",         158.16,     6.92,
  "KIR",         173.00,     1.40,
  "PLW",         134.56,     7.34,
  "WLF",        -177.15,   -13.30,
  "MNP",         145.76,    15.18,
  "ASM",        -170.70,   -14.27,
  "PYF",        -149.56,   -17.65,
  "NCL",         165.60,   -21.30,
  "GUM",         144.79,    13.44,
  "MDV",          73.51,     4.18,
  "SYC",          55.45,    -4.62,
  "MUS",          57.55,   -20.35,
  "COM",          43.33,   -11.64,
  "CPV",         -23.65,    15.11,
  "HKG",         114.17,    22.32,
  "MAC",         113.55,    22.20
) |>
  dplyr::distinct(Country_Code, .keep_all = TRUE)

separate_overlaps <- function(df, thresh_deg = 1, offset_deg = 0.5, wrap_dateline = TRUE) {
  stopifnot(all(c("lon", "lat") %in% names(df)))
  if (nrow(df) <= 1) return(dplyr::mutate(df, lon_plot = lon, lat_plot = lat))

  lon_use <- if (wrap_dateline) ifelse(df$lon < 0, df$lon + 360, df$lon) else df$lon
  lat_mean <- mean(df$lat, na.rm = TRUE)
  x <- lon_use * cos(lat_mean * pi / 180)
  y <- df$lat

  d <- stats::dist(cbind(x, y))
  hc <- stats::hclust(d, method = "single")
  cl <- stats::cutree(hc, h = thresh_deg)

  df %>%
    dplyr::mutate(`_cluster` = cl) %>%
    dplyr::group_by(`_cluster`) %>%
    dplyr::mutate(
      `_k` = dplyr::n(),
      `_idx` = dplyr::row_number(),
      `_angle` = ifelse(`_k` == 1, 0, 2 * pi * (`_idx` - 1) / `_k`),
      `_r` = ifelse(`_k` == 1, 0, offset_deg * pmin(1, 2 / `_k`)),
      lon_plot = lon + (`_r` * cos(`_angle`)) / pmax(0.3, cos(lat * pi / 180)),
      lat_plot = lat + `_r` * sin(`_angle`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-`_cluster`, -`_k`, -`_idx`, -`_angle`, -`_r`)
}

# =============================================================================
# SECTION III: COUNTRY-LEVEL HISTORICAL BASELINE + PERCENT CHANGE
# =============================================================================

# Same formula Step 9 uses for the global historical baseline
# (hist_ann_deaths <- sum(ci_base$meanDeaths * ci_base$nEvents) / N_YEARS),
# applied per country instead of summed.
hist_country <- ci_base %>%
  transmute(Country_Code, hist_ann_deaths = meanDeaths * nEvents / N_YEARS)

proj_data <- country_master %>%
  filter(methodology == stage_use, ssp %in% scenarios_for_median) %>%
  left_join(hist_country, by = "Country_Code") %>%
  filter(!is.na(hist_ann_deaths), hist_ann_deaths > 0) %>%
  mutate(
    pctInc     = 100 * (ann_deaths - hist_ann_deaths) / hist_ann_deaths,
    pctInc_lcl = 100 * (ann_deaths_lcl - hist_ann_deaths) / hist_ann_deaths,
    pctInc_ucl = 100 * (ann_deaths_ucl - hist_ann_deaths) / hist_ann_deaths
  )

# SEPARATE dataset for the top-10 detail panels (4c/4d) -- same stage as
# the maps now, but ALL 7 scenarios (including BOUND_worst/BOUND_best),
# since these panels show each scenario individually rather than a
# median, and the bounding scenarios are exactly what should appear here
# as explicit comparisons.
proj_data_detail <- country_master %>%
  filter(methodology == stage_use, ssp %in% scenarios_to_plot) %>%
  left_join(hist_country, by = "Country_Code") %>%
  filter(!is.na(hist_ann_deaths), hist_ann_deaths > 0) %>%
  mutate(
    pctInc     = 100 * (ann_deaths - hist_ann_deaths) / hist_ann_deaths,
    pctInc_lcl = 100 * (ann_deaths_lcl - hist_ann_deaths) / hist_ann_deaths,
    pctInc_ucl = 100 * (ann_deaths_ucl - hist_ann_deaths) / hist_ann_deaths
  )

# Median pctInc across the selected scenarios, one row per Country_Code x
# year -- this is the map-panel summary (4a/4b), matching the original
# figure's "median across settings" approach.
median_inc <- proj_data %>%
  group_by(year, Country_Code) %>%
  summarise(
    pctInc = median(pctInc, na.rm = TRUE),
    events = max(n_events, na.rm = TRUE),
    .groups = "drop"
  )

median_inc_50  <- median_inc %>% filter(year == "2050")
median_inc_100 <- median_inc %>% filter(year == "2100")

# Same median-across-scenarios summary, but for the DETAIL stage (used to
# select/rank the top-10 countries for panels 4c/4d, so both the country
# selection AND the displayed values reflect the same, more limited
# mechanism -- not top-10 countries selected under the full decomposition
# but displayed under a different stage).
median_inc_detail <- proj_data_detail %>%
  group_by(year, Country_Code) %>%
  summarise(
    pctInc = median(pctInc, na.rm = TRUE),
    events = max(n_events, na.rm = TRUE),
    .groups = "drop"
  )

median_inc_detail_50  <- median_inc_detail %>% filter(year == "2050")
median_inc_detail_100 <- median_inc_detail %>% filter(year == "2100")

# =============================================================================
# SECTION IV: MAP DATA (panels 4a/4b)
# =============================================================================

map_df_50  <- world |> dplyr::left_join(median_inc_50,  by = c("iso_a3" = "Country_Code"))
map_df_100 <- world |> dplyr::left_join(median_inc_100, by = c("iso_a3" = "Country_Code"))

make_bubbles_for <- function(inc_df) {
  candidate_codes <- intersect(coords_lookup$Country_Code, inc_df$Country_Code)
  inc_df |>
    dplyr::semi_join(tibble::tibble(Country_Code = candidate_codes), by = "Country_Code") |>
    dplyr::left_join(coords_lookup, by = "Country_Code") |>
    dplyr::filter(!is.na(lon), !is.na(lat)) |>
    separate_overlaps(thresh_deg = 1, offset_deg = 0.3)
}

bubbles_50  <- make_bubbles_for(median_inc_50)
bubbles_100 <- make_bubbles_for(median_inc_100)

fmt_pct_ticks <- function(x) paste0(scales::number(x, accuracy = 1), "%")

plot_panel_diverging <- function(map_df, bubbles_df, value_var,
                                 title, legend_title,
                                 limit_lo = NULL, limit_hi = NULL,
                                 legend_y = 0.035, barwidth_npc = 0.50) {
  vals <- map_df[[value_var]]

  if (is.null(limit_lo)) limit_lo <- as.numeric(quantile(vals, 0.01, na.rm = TRUE))
  if (is.null(limit_hi)) limit_hi <- as.numeric(quantile(vals, 0.99, na.rm = TRUE))

  outside_map <- map_df[!is.na(vals) & (vals < limit_lo | vals > limit_hi),
                        c("iso_a3", value_var)]
  bub_vals <- bubbles_df[[value_var]]
  outside_bub <- bubbles_df[!is.na(bub_vals) & (bub_vals < limit_lo | bub_vals > limit_hi),
                            c("Country_Code", value_var)]
  names(outside_bub)[1] <- "iso_a3"
  outside <- rbind(as.data.frame(outside_map)[, c("iso_a3", value_var)],
                   as.data.frame(outside_bub)[, c("iso_a3", value_var)])
  outside <- unique(outside)
  if (nrow(outside) > 0) {
    cat("\nCountries outside color ramp for", title, ":\n")
    print(outside[order(outside[[value_var]]), ])
  } else {
    cat("\nNo countries outside color ramp for", title, "\n")
  }

  map_plot <- map_df |>
    dplyr::mutate(fill_val = pmax(pmin(.data[[value_var]], limit_hi), limit_lo))
  bub_plot <- bubbles_df |>
    dplyr::mutate(fill_val = pmax(pmin(.data[[value_var]], limit_hi), limit_lo))

  ggplot(map_plot) +
    geom_sf(aes(fill = fill_val), color = "grey60", size = 0.1) +
    geom_point(
      data = bub_plot,
      aes(x = lon_plot, y = lat_plot, fill = fill_val),
      shape = 21, color = "grey30", stroke = 0.2, size = 2.7
    ) +
    geom_label_repel(
      data = bub_plot,
      aes(x = lon_plot, y = lat_plot, label = Country_Code, fill = fill_val),
      color = "black",
      alpha = 0.85,
      size = 2, fontface = "bold",
      label.size = 0.15, label.r = unit(0.08, "lines"),
      label.padding = unit(0.08, "lines"),
      box.padding = 0.25, point.padding = 0.06,
      max.overlaps = Inf, min.segment.length = 0,
      segment.size = 0.2, segment.alpha = 0.6,
      show.legend = FALSE,
      seed = 123
    ) +
    scale_fill_gradientn(
      colors = c("blue", "white", "red"),
      # Position white at 0's TRUE fractional location between limit_lo and
      # limit_hi -- NOT forced to the 50% midpoint the way
      # scale_fill_gradient2()'s default rescaler does. That default
      # forces blue/red to be symmetric using whichever side has the
      # larger range for BOTH sides -- since our positive range (up to
      # +652%) is ~11x larger than our negative range (down to -55.7%),
      # the old gradient2 compressed the ENTIRE negative range into an
      # imperceptibly narrow, uniformly pale sliver near white (even
      # PNG's -95% barely showed as blue). This fixes it: each side now
      # gets the full blue-to-white or white-to-red transition across its
      # own actual range.
      values = scales::rescale(c(limit_lo, 0, limit_hi), to = c(0, 1)),
      limits = c(limit_lo, limit_hi),
      oob = scales::squish,
      labels = fmt_pct_ticks,
      name   = legend_title,
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth  = unit(barwidth_npc, "npc"),
        barheight = unit(0.014, "npc"),
        ticks = TRUE
      )
    ) +
    coord_sf(expand = FALSE) +
    labs(title = title) +
    theme_void(base_size = 11) +
    theme(
      legend.position = c(0.5, legend_y),
      legend.justification = c(0.5, 0),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = scales::alpha("white", 0.6), color = NA),
      legend.title = element_text(size = 8),
      legend.text  = element_text(size = 7)
    )
}

# Shared color scale range across BOTH years (2050 and 2100), computed
# once from the combined data -- so a given % change reads as the same
# color in both maps, rather than each map auto-scaling to its own data
# range independently. Diverging scale (blue = decrease, white = 0,
# red = increase) is preserved throughout -- decreases are allowed and
# shown, not clipped.
shared_vals <- c(map_df_50$pctInc, map_df_100$pctInc, bubbles_50$pctInc, bubbles_100$pctInc)
# Fixed at -100% (the theoretical floor -- a 100% decrease means zero
# deaths, so nothing can go lower) rather than a data-driven percentile.
# -100% now renders as the full, most-saturated blue.
shared_limit_lo <- -100
shared_limit_hi <- as.numeric(quantile(shared_vals, 0.99, na.rm = TRUE))

figure.4a <- plot_panel_diverging(
  map_df = map_df_50, bubbles_df = bubbles_50,
  value_var    = "pctInc",
  title        = "(a) Median projected change in annual deaths by 2050\n(SLR + population exposure only; median of 5 SSP-matched scenarios)",
  legend_title = "% change vs. historical",
  limit_lo = shared_limit_lo, limit_hi = shared_limit_hi
)

figure.4b <- plot_panel_diverging(
  map_df = map_df_100, bubbles_df = bubbles_100,
  value_var    = "pctInc",
  title        = "(b) Median projected change in annual deaths by 2100\n(SLR + population exposure only; median of 5 SSP-matched scenarios)",
  legend_title = "% change vs. historical",
  limit_lo = shared_limit_lo, limit_hi = shared_limit_hi
)

# =============================================================================
# SECTION V: TOP-10 DETAIL PANELS (4c/4d)
# =============================================================================

# Scenario order: matches Figure 3's EXACT ranking method -- ascending by
# the GLOBAL aggregate value (decomp_table's Pct_vs_historical) at the
# fully-decomposed stage, 2050 -- NOT a country-averaged mean. This
# deliberately differs from stage_use (which is the SLR+population-
# exposure-only stage actually displayed in this figure's maps/panels):
# the ranking always uses the fully-decomposed stage, matching Figure 3,
# purely so both figures order/color scenarios identically for easy
# cross-referencing. A country-averaged ranking (the previous approach)
# can diverge from the global one -- e.g. Best case/Worst case can
# reverse for individual countries (Madagascar), which pulled the mean-
# across-countries ranking away from what Figure 3 shows.
scenario_order <- decomp_table %>%
  filter(Scenario == " + Population (both) & HDI & coastal growth 2", Year == "2050") %>%
  arrange(Pct_vs_historical) %>%
  pull(SSP)
scenario_order <- c(scenario_order, setdiff(scenarios_to_plot, scenario_order))

# Same palette fix as Figure 3: skip YlGnBu's 2 lightest shades (near-
# white/pale yellow -- nearly invisible on a white background) before
# interpolating across the remaining, better-contrast portion of the ramp.
pal_full <- RColorBrewer::brewer.pal(9, "YlGnBu")
pal <- colorRampPalette(pal_full[3:9])(length(scenario_order))
names(pal) <- scenario_order

make_top10_panel <- function(median_inc_df, proj_data, yr, title) {
  eligible <- median_inc_df %>% filter(events > MIN_EVENTS_FOR_TOP10)
  top10_order <- eligible %>%
    arrange(desc(pctInc)) %>%
    slice_head(n = 10) %>%
    pull(Country_Code)

  detail <- proj_data %>%
    filter(year == yr, Country_Code %in% top10_order) %>%
    mutate(
      Country_Code = factor(Country_Code, levels = top10_order),
      ssp = factor(ssp, levels = scenario_order)
    )

  ggplot(detail, aes(x = Country_Code, y = pctInc, color = ssp)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = pctInc_lcl, ymax = pctInc_ucl),
                  position = position_dodge(width = 0.5), width = 0.12, linewidth = 0.35) +
    geom_point(position = position_dodge(width = 0.5), size = 1.2) +
    scale_color_manual(values = pal, name = "Scenario") +
    labs(x = "Country", y = "Projected change in annual deaths\nvs. historical baseline (%)", title = title) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      plot.title = element_text(size = 9)
    )
}

figure.4c <- make_top10_panel(median_inc_detail_50,  proj_data_detail, "2050",
                               "(c) Countries with largest projected increase, by 2050\n(>10 historical events, SLR + population exposure only)")
figure.4d <- make_top10_panel(median_inc_detail_100, proj_data_detail, "2100",
                               "(d) Countries with largest projected increase, by 2100\n(>10 historical events, SLR + population exposure only)")

# =============================================================================
# SECTION VI: ASSEMBLE + WRITE OUTPUT
# =============================================================================

figure.4 <- figure.4a / figure.4b / (figure.4c + figure.4d) +
  plot_layout(heights = c(2, 2, 1))

ggsave(
  filename = paste0("Output/Plots/Fig 4 ", date_append_output, ".pdf"),
  plot = figure.4,
  width = 9, height = 15, units = "in"
)

message("Figure 4 written to Output/Plots/Fig 4 ", date_append_output, ".pdf")
