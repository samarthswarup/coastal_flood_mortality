# Figure 2: Retrospective coastal flood mortality by country
#
# Script dependencies:
#   - Step 8: CountryDeathCIs, CountryDeathCIs50 and CountryDeathCIs100 CSV files.
# R packages: dplyr, ggplot2, ggplotify, ggrepel, grid, patchwork, rnaturalearth, scales, sf, stringr
# Outputs:
#   - Output/Plots/Fig 2 <date>.pdf
# Computation:
#   - Maps of country-level fitted mortality and annual deaths; country-level rate and death-count intervals for countries meeting EVENT_THRESHOLD.
#
# Execute from the project analysis directory containing Intermediate outputs/.

rm(list = ls())

library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(grid)
library(scales)
library(ggplotify)
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

step8_date_append_output <- "2026-11-13"

date_append_output <- format(Sys.Date(), "%Y-%m-%d")

EVENT_THRESHOLD <- 50


# Load Step 8 country-level estimates.
ci_base <- read.csv(paste0("Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs ", step8_date_append_output, ".csv"))
ci_50   <- read.csv(paste0("Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs50 ", step8_date_append_output, ".csv"))
ci_100  <- read.csv(paste0("Intermediate outputs/Output from Step 8/Country tables/CountryDeathCIs100 ", step8_date_append_output, ".csv"))


# Prepare country boundaries and geographic joins.
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::select(iso_a3, name_long, geometry)

world$iso_a3[which(world$name_long == "France")] <- "FRA"
world$iso_a3[which(world$name_long == "Norway")] <- "NOR"

world$iso_a3[which(world$iso_a3 == "CUW" |
                    world$iso_a3 == "ABW" |
                    world$iso_a3 == "SXM" |
                    world$iso_a3 == "BES")] <- "BES,CUW,ABW,SXM"

map_df <- world |>
  left_join(ci_base, by = c("iso_a3" = "Country_Code"))

non.plotting.countries <-
  ci_base$Country_Code[which(is.na(match(ci_base$Country_Code, map_df$iso_a3)))]

cap <- quantile(map_df$meanDeaths, 1, na.rm = TRUE)
map_df <- map_df |>
  mutate(meanDeaths_capped = pmin(meanDeaths, cap))


# Set geographic point coordinates for small countries and territories.
centroid_for_code <- function(code, world_sf) {
  parts <- str_split(code, ",", simplify = TRUE)
  parts <- parts[parts != ""]
  piece_sf <- suppressWarnings(dplyr::filter(world_sf, iso_a3 %in% parts))
  if (nrow(piece_sf) == 0) return(NULL)
  geom_union <- sf::st_union(piece_sf$geometry)
  pt <- sf::st_point_on_surface(geom_union)
  xy <- sf::st_coordinates(pt)
  tibble::tibble(iso3_combo = code, lon = xy[1], lat = xy[2])
}

coords_lookup <- tibble::tribble(
  ~Country_Code, ~lon,      ~lat,
  "GLP",         -61.551,    16.265,   # Guadeloupe
  "MTQ",         -61.024,    14.641,   # Martinique
  "TKL",         -171.850,   -9.166,   # Tokelau (approx. archipelago centroid)
  "REU",          55.536,   -21.115,   # Reunion
  "SXM",         -63.067,    18.041,   # Sint Maarten (Dutch side)
  "MYT",          45.166,   -12.827    # Mayotte
)

coords_lookup_add <- tibble::tribble(
  ~Country_Code, ~lon,      ~lat,
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
)

coords_lookup <- dplyr::bind_rows(
  coords_lookup,
  coords_lookup_add
) |>
  dplyr::distinct(Country_Code, .keep_all = TRUE)

candidate_codes <- intersect(coords_lookup$Country_Code, ci_base$Country_Code)

bubbles_df <- ci_base |>
  dplyr::semi_join(tibble::tibble(Country_Code = candidate_codes), by = "Country_Code") |>
  dplyr::left_join(coords_lookup, by = "Country_Code") |>
  dplyr::filter(!is.na(lon), !is.na(lat)) |>
  dplyr::mutate(meanDeaths_capped = pmin(meanDeaths, cap))

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

bubbles_df_sep <- bubbles_df %>%
  separate_overlaps(thresh_deg = 1, offset_deg = 0.3)


fmt_log_ticks <- function(x) {
  sapply(x, function(v) {
    lab <- if (v >= 1) {
      scales::comma(v, accuracy = 1)
    } else if (v >= 0.1) {
      sprintf("%.1f", v)
    } else if (v >= 0.01) {
      sprintf("%.2f", v)
    } else {
      sprintf("%.3f", v)
    }
    lab <- sub("(\\.\\d*?)0+$", "\\1", lab, perl = TRUE)
    lab <- sub("\\.$", "", lab)
    lab
  })
}

map_df <- map_df |>
  dplyr::mutate(deaths_per_year = meanDeaths * nEvents / 35)

bubbles_df_sep <- bubbles_df_sep |>
  dplyr::mutate(deaths_per_year = meanDeaths * nEvents / 35)

# Construct mapped mortality panels.
plot_panel <- function(map_df, bubbles_df_sep, value_var,
                        title, legend_title,
                        cap_pctl = 1, legend_y = 0.035, barwidth_npc = 0.50) {

  vals_map <- map_df[[value_var]]
  cap_val  <- as.numeric(stats::quantile(vals_map, cap_pctl, na.rm = TRUE))

  map_plot <- map_df |>
    dplyr::mutate(fill_val = pmin(.data[[value_var]], cap_val))

  bub_plot <- bubbles_df_sep |>
    dplyr::mutate(fill_val = pmin(.data[[value_var]], cap_val))

  dmin <- min(c(
    map_plot$fill_val[map_plot$fill_val > 0],
    bub_plot$fill_val[bub_plot$fill_val > 0]
  ), na.rm = TRUE)

  brks <- 10^seq(floor(log10(dmin)), ceiling(log10(cap_val)), by = 1)

  ggplot(map_plot) +
    geom_sf(aes(fill = fill_val), color = "grey60", size = 0.1) +
    geom_point(
      data = bub_plot,
      aes(x = lon_plot, y = lat_plot, fill = fill_val),
      shape = 21, color = "grey30", stroke = 0.2, size = 2.7
    ) +
    geom_label_repel(
      data = bub_plot,
      aes(x = lon_plot, y = lat_plot,
          label = Country_Code,
          fill  = fill_val),
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
    scale_fill_viridis_c(
      trans = "log10",
      breaks = brks,
      option = "turbo",
      labels = fmt_log_ticks,
      limits = c(dmin, cap_val),
      na.value = "grey90",
      name = legend_title,
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


# Construct four figure panels.
figure.2a <- plot_panel(
  map_df, bubbles_df_sep,
  value_var    = "meanDeaths",
  title        = "(a) Mean predicted deaths per event (1990-2024)",
  legend_title = "Deaths per event (predicted; log scale)",
  cap_pctl = 0.99
)

figure.2b <- plot_panel(
  map_df, bubbles_df_sep,
  value_var    = "deaths_per_year",
  title        = "(b) Predicted deaths per year (1990-2024 mean)",
  legend_title = "Deaths per year (predicted; log scale)"
)

keep_base <- ci_base[ci_base$nEvents >= EVENT_THRESHOLD, ]
keep_50   <- ci_50[ci_50$nEvents >= EVENT_THRESHOLD, ]
keep_100  <- ci_100[ci_100$nEvents >= EVENT_THRESHOLD, ]

keep_base <- keep_base[order(keep_base$meanRate), ]

figure.2c <- ggplotify::as.ggplot(function() {
  par(mar = c(6, 4.5, 3, 1), xpd = NA)
  plot(-5, -5,
       xlim = c(1, nrow(keep_base)), xlab = "", xaxt = "n",
       las = 2,
       ylim = c(0, 0.005), ylab = "Deaths per 1000 exposed",
       main = "(c) Predicted mean (+/- 95% CI) death rate per event",
       yaxt = "n")
  axis(2, at = 1:5 / 1000, labels = 1:5, las = 2)
  axis(1, at = 1:nrow(keep_base), labels = keep_base$Country_Code, las = 3, cex.axis = 1)

  for (i in 1:nrow(keep_base)) {
    lines(c(i, i), c(max(0, keep_base$mr_lcl[i]), keep_base$mr_ucl[i]))
    lines(c(i - 0.1, i + 0.1), c(max(0, keep_base$mr_lcl[i]), max(0, keep_base$mr_lcl[i])))
    lines(c(i - 0.1, i + 0.1), c(keep_base$mr_ucl[i], keep_base$mr_ucl[i]))
    points(i, keep_base$meanRate[i], pch = 21, col = "black", bg = "#1496E1")
  }
  box()
})

keep_base <- keep_base[order(keep_base$meanDeaths), ]

figure.2d <- ggplotify::as.ggplot(function() {
  par(mar = c(6, 4.5, 3, 1), xpd = NA)
  plot(-5, -5,
       xlim = c(1, nrow(keep_base)), xlab = "Country", xaxt = "n",
       las = 2,
       main = "(d) Predicted mean (+/- 95% CI) deaths per event",
       yaxt = "n",
       ylim = c(0, 10000), ylab = "Deaths per event (x1000)")
  axis(1, at = 1:nrow(keep_base), labels = keep_base$Country_Code, las = 3, cex.axis = 1)
  axis(2, at = ((1:16) / 4) * 10^4, labels = (1:16) * 10 / 4, las = 1)

  for (i in 1:nrow(keep_base)) {
    lines(c(i, i), c(max(0, keep_base$md_lcl[i]), keep_base$md_ucl[i]))
    lines(c(i - 0.1, i + 0.1), c(max(0, keep_base$md_lcl[i]), max(0, keep_base$md_lcl[i])))
    lines(c(i - 0.1, i + 0.1), c(keep_base$md_ucl[i], keep_base$md_ucl[i]))
    points(i, keep_base$meanDeaths[i], pch = 21, col = "black", bg = "#1496E1")
  }
  box()
})


# Combine and save Figure 2.
figure.2 <- (figure.2a + figure.2b) / (figure.2c + figure.2d)

ggsave(
  filename = paste0("Output/Plots/Fig 2 ", date_append_output, ".pdf"),
  plot = figure.2,
  width = 12, height = 12, units = "in"
)

message("Figure 2 written to Output/Plots/Fig 2 ", date_append_output, ".pdf")
