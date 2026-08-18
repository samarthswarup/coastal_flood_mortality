##########################################
# Code needed for both 5a and 5b         #
#                                        #
# 2025-08-10                             #
# Ryan Calder                            #
# Notes added by Claude                  #
##########################################

# This code defines a function called geocode_range() which accepts index positions
# (1,2,3, etc.) of the object
# location_split_no_generic_no_parentheses_country.fixed, created in
# "4_EMDAT_process.R". It uses the Open Street Map interface
# via function geocode() to return OSM data including lat lon for every
# location supplied. The function geocode_range() also appends information on
# the EMDAT disasters relevant for each location geocoded.

# 0. Read libraries and define functions

library(tidygeocoder)
library(dplyr)
library(openxlsx)
library(tidyr)
library(purrr)
library(readxl)
library(stringi)
library(maps)
library(sf)
library(httr2)
library(geojsonsf)
library(ggplot2)



normalize_spaces <- function(x) {
  stri_trim_both(stri_replace_all_regex(x, "\\p{Zs}+", " "))
}

log_message <- function(message) {
  cat(message, "\n")
}

# convert_boundingbox <- function(df) {
#   if ("boundingbox" %in% names(df)) {
#     df <- df %>%
#       mutate(boundingbox =
#                ifelse(is.list(boundingbox),
#                       sapply(boundingbox, function(x) paste(x, collapse = ", ")),
#                       boundingbox))
#   }
#   return(df)
# }

convert_boundingbox <- function(df) {
  if ("boundingbox" %in% names(df)) {
    df <- df %>%
      mutate(boundingbox =
               ifelse(is.list(boundingbox),
                      sapply(boundingbox, function(x) {
                        # Extract the bounding box coordinates
                        bbox_coords <- as.numeric(unlist(strsplit(x, ", ")))
                        bbox_north <- bbox_coords[1]
                        bbox_south <- bbox_coords[2]
                        bbox_east <- bbox_coords[3]
                        bbox_west <- bbox_coords[4]

                        # Check if the bounding box spans the 180° meridian
                        if (bbox_west > bbox_east) {
                          # Split the bounding box into two parts:
                          # 1. From west to 180°
                          # 2. From -180° to east
                          part_1 <- c(bbox_north, bbox_south, 180, bbox_west)
                          part_2 <- c(bbox_north, bbox_south, bbox_east, -180)

                          # Return both parts as separate bounding boxes
                          return(list(paste(part_1, collapse = ", "), paste(part_2, collapse = ", ")))
                        } else {
                          # Return the original bounding box if it does not span the 180° meridian
                          return(x)
                        }
                      }),
                      boundingbox))
  }
  return(df)
}



escape_parentheses <- function(x) {
  gsub("([()])", "\\\\\\1", x)
}

is_word <- function(s) grepl("^\\w+$", s)

escape_regex <- function(s) {
  gsub("([.|()\\^{}+$*?]|\\[|\\]|\\\\)", "\\\\\\1", s)
}

geocode_range <- function(range) {
  all_columns <- c()
  result_list <- list()

  for (i in range) {
    location <- unlist(location_split_no_generic_no_parentheses_country.fixed.unique[i])
    location_escaped <- gsub("[\\h\\v\\s]+$", "", escape_parentheses(location), perl = TRUE)
    location_escaped <- gsub('["\']+$', "", location_escaped)  # remove trailing quotes (double or single)

    temp_df <- data.frame(address = location, stringsAsFactors = FALSE)

    # Attempt geocoding
    result <- tryCatch(
      {
        tidygeocoder::geocode(temp_df, address = address, method = "osm", full_results = TRUE)
      },
      error = function(e) {
        log_message(paste("Error geocoding address:", location))
        return(NULL)
      }
    )

    if (!is.null(result) && nrow(result) > 0) {
      if ("osm_type" %in% colnames(result)) {
        preferred_result <- result %>% filter(osm_type %in% c("relation", "boundary"))
        if (nrow(preferred_result) == 0) {
          preferred_result <- result
        }
      } else {
        preferred_result <- result
      }

      escaped_patterns <- sapply(location_escaped, function(loc) {
        loc_escaped <- escape_regex(loc)
        if (is_word(loc)) {
          paste0("\\b", loc_escaped, "\\b")
        } else {
          loc_escaped
        }
      })


      matches =
        which(trimws(location_split_no_generic_no_parentheses_country.fixed.unique)==trimws(location))



      if(length(matches)==0){

        print(paste("Error at i = ",i,sep=""))

        break




      }

      preferred_result <- preferred_result %>%
        mutate(original_location = location,
               item_number = paste(matches, collapse = ","),
               disaster_number =
                 paste(emdat.data.filtered$DisNo.[unlist(location_index_map[matches])], collapse = ","))

      result <- preferred_result
      log_message(paste("Successfully geocoded address:", location))
    } else {
      log_message(paste("Failed to geocode address:", location))
    }

    log_message(paste("Completed geocoding for item number:", i))

    if (!is.null(result)) {
      all_columns <- union(all_columns, colnames(result))
      result_list[[length(result_list) + 1]] <- result
    }
  }

  combined_results <- do.call(rbind, lapply(result_list, function(df) {
    missing_cols <- setdiff(all_columns, colnames(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    return(df[, all_columns])
  }))

  return(combined_results)
}

# ---------------- small utils ----------------
slugify <- function(x, max_nchar = 32) {
  s <- gsub("[^A-Za-z0-9]+", "_", tolower(x))
  s <- gsub("^_+|_+$", "", s)
  if (nchar(s) > max_nchar) substr(s, 1, max_nchar) else s
}

is_bad_sf <- function(x) is.null(x) || !inherits(x, "sf") || inherits(x, "try-error") || nrow(x) == 0

# ================= OSM fetch + selection =================
fetch_osm_polygon <- function(osm_type, osm_id, ua = "rsdc@vt.edu (research; R)") {
    # osm_type should be one of: "relation", "way", "node"
    prefix <- switch(tolower(as.character(osm_type)),
                     relation = "R",
                     way      = "W",
                     node     = "N",
                     stop("Unknown osm_type: ", osm_type))

    # build an ID string safely (no scientific notation, no decimals)
    id_str <- if (is.numeric(osm_id)) sprintf("%.0f", osm_id) else as.character(osm_id)

    key  <- paste0(prefix, id_str)

    resp <- httr2::request("https://nominatim.openstreetmap.org/lookup") |>
      httr2::req_user_agent(ua) |>
      httr2::req_url_query(osm_ids = key, format = "geojson", polygon_geojson = 1) |>
      httr2::req_perform()

    httr2::resp_check_status(resp)
    geojsonsf::geojson_sf(httr2::resp_body_string(resp))
  }

pick_primary_feature <- function(g_sf) {
  stopifnot(inherits(g_sf, "sf"), nrow(g_sf) > 0)
  gtypes <- as.character(sf::st_geometry_type(sf::st_geometry(g_sf), by_geometry = TRUE))
  rank_geom <- match(gtypes,
                     c("MULTIPOLYGON","POLYGON","MULTILINESTRING","LINESTRING","MULTIPOINT","POINT"))
  rank_geom[is.na(rank_geom)] <- 999
  cls <- tolower(if ("class" %in% names(g_sf)) g_sf$class else "")
  typ <- tolower(if ("type"  %in% names(g_sf)) g_sf$type  else "")
  bonus <- ifelse(grepl("boundary|place|administrative", paste(cls, typ)), -0.1, 0)
  g_sf[order(rank_geom + bonus)[1], , drop = FALSE]
}

# ================= ensure area for points/lines =================
ensure_areal <- function(sf_one_row, radius_km_point = 1, buffer_m_line = 1000) {
  stopifnot(inherits(sf_one_row, "sf"), nrow(sf_one_row) == 1L)
  g  <- sf::st_geometry(sf_one_row)
  if (any(sf::st_is_empty(g))) stop("empty geometry")
  gt <- as.character(sf::st_geometry_type(g, by_geometry = TRUE))
  if (gt %in% c("POLYGON","MULTIPOLYGON")) return(sf_one_row)

  cen  <- sf::st_coordinates(sf::st_centroid(sf::st_transform(g, 4326)))[1, ]
  aeqd <- sf::st_crs(paste0("+proj=aeqd +lat_0=", cen[2], " +lon_0=", cen[1],
                            " +datum=WGS84 +units=m +no_defs"))
  if (gt %in% c("POINT","MULTIPOINT")) {
    r_m <- radius_km_point * 1000
    g2  <- sf::st_transform(g, aeqd) |> sf::st_buffer(r_m) |> sf::st_make_valid() |> sf::st_transform(4326)
    sf::st_geometry(sf_one_row) <- sf::st_cast(g2, "MULTIPOLYGON", warn = FALSE)
    return(sf_one_row)
  }
  if (gt %in% c("LINESTRING","MULTILINESTRING")) {
    g2 <- sf::st_transform(g, aeqd) |> sf::st_buffer(buffer_m_line) |> sf::st_make_valid() |> sf::st_transform(4326)
    sf::st_geometry(sf_one_row) <- sf::st_cast(g2, "MULTIPOLYGON", warn = FALSE)
    return(sf_one_row)
  }
  stop("Unsupported geometry type: ", gt)
}

# ================= normalize + dissolve to ONE MULTIPOLYGON =================
normalize_and_dissolve <- function(sf_one_row, sliver_m2 = 1000) {
  stopifnot(inherits(sf_one_row, "sf"), nrow(sf_one_row) == 1L)

  sf_areal <- ensure_areal(sf_one_row)

  old_s2 <- sf::sf_use_s2(); on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))
  ea <- 6933L

  x  <- sf::st_transform(sf_areal, ea) |>
    sf::st_set_precision(1) |>
    sf::st_make_valid() |>
    sf::st_buffer(0)

  parts <- sf::st_cast(sf::st_geometry(x), "POLYGON", warn = FALSE)
  if (length(parts) == 0) {
    uu <- sf::st_unary_union(sf::st_geometry(sf::st_transform(sf_areal, ea)))
    parts <- sf::st_cast(uu, "POLYGON", warn = FALSE)
  }

  a     <- as.numeric(sf::st_area(sf::st_sfc(parts, crs = sf::st_crs(x))))
  keep  <- is.finite(a) & a >= sliver_m2
  if (!any(keep)) keep[which.max(a)] <- TRUE

  geom_one <- sf::st_cast(
    sf::st_union(sf::st_sfc(parts[keep], crs = sf::st_crs(x))),
    "MULTIPOLYGON", warn = FALSE
  )
  sf::st_transform(geom_one, 4326)  # sfc length 1
}

# ================= file index selection (for numbering) =================
.pick_file_index <- function(row, fallback) {
  if ("src_idx" %in% names(row) && is.finite(suppressWarnings(as.integer(row$src_idx))))
    return(as.integer(row$src_idx))
  if ("global_idx" %in% names(row) && is.finite(suppressWarnings(as.integer(row$global_idx))))
    return(as.integer(row$global_idx))
  if ("item_number" %in% names(row)) {
    m <- regmatches(as.character(row$item_number), regexpr("\\d+", as.character(row$item_number)))
    if (length(m) == 1L && nzchar(m)) {
      v <- suppressWarnings(as.integer(m))
      if (is.finite(v)) return(v)
    }
  }
  as.integer(fallback)
}

# ================= safe map saving (won’t crash the loop) =================
.safe_save_map <- function(p, png_path, world_plot, g_plot, xlim, ylim, title_txt, subt_txt) {
  ok <- FALSE
  # try ragg
  if (requireNamespace("ragg", quietly = TRUE)) {
    ok <- try({
      ragg::agg_png(png_path, width = 1600, height = 1200, res = 150); print(p); grDevices::dev.off()
      TRUE
    }, silent = TRUE)
    ok <- isTRUE(ok)
  }
  # try standard png
  if (!ok) {
    ok <- try({
      grDevices::png(png_path, width = 1600, height = 1200, res = 150)
      print(p); grDevices::dev.off(); TRUE
    }, silent = TRUE)
    ok <- isTRUE(ok)
  }
  # base plot fallback
  if (!ok) {
    try({
      grDevices::png(png_path, width = 1600, height = 1200, res = 150)
      if (!is.null(world_plot)) plot(sf::st_geometry(world_plot), col = NA, border = "grey70")
      plot(sf::st_geometry(g_plot), col = grDevices::adjustcolor("red", 0.35),
           border = "red", add = !is.null(world_plot), axes = TRUE,
           xlim = xlim, ylim = ylim)
      title(main = title_txt, sub = subt_txt)
      grDevices::dev.off()
    }, silent = TRUE)
  }
}

# ================= main: local.polygon() =================
# Use either:
#   local.polygon(df)                          # processes all rows of df
#   local.polygon(idxs, df = df_subset)        # processes df_subset[idxs, ]
# Returns character vector of .gpkg paths (NA if a row failed)
local.polygon <- function(x,
                          df = NULL,
                          out_dir = "Intermediate outputs/Output from Step 5/Local polygons",
                          ua = "rsdc@vt.edu (research; R)",
                          radius_km_point = 1,
                          buffer_m_line = 1000,
                          sliver_m2 = 1000,
                          make_map = TRUE,
                          zoom_km = 600,
                          verbose = TRUE) {

  # Overloaded interface
  if (is.data.frame(x)) {
    df_in <- x
    idxs_for_lookup <- seq_len(nrow(df_in))
    file_indices <- vapply(seq_len(nrow(df_in)),
                           function(k) .pick_file_index(df_in[k, , drop = FALSE], fallback = k),
                           integer(1))
  } else {
    if (is.null(df)) stop("When x is numeric (row indices), you must supply df=...")
    if (!is.numeric(x)) stop("x must be a data frame/tibble or a numeric vector of row indices.")
    if (any(x < 1 | x > nrow(df))) stop("Row index out of bounds for supplied df.")
    df_in <- df[x, , drop = FALSE]
    idxs_for_lookup <- seq_len(nrow(df_in))
    file_indices <- vapply(seq_len(nrow(df_in)),
                           function(k) .pick_file_index(df_in[k, , drop = FALSE], fallback = x[k]),
                           integer(1))
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_paths <- rep(NA_character_, nrow(df_in))

  # basemap once (optional)
  world_src <- NULL
  if (make_map && requireNamespace("rnaturalearth", quietly = TRUE)) {
    world_src <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
  }

  for (j in idxs_for_lookup) {
    row <- df_in[j, , drop = TRUE]
    file_idx <- file_indices[j]
    if (verbose) message("[", file_idx, "] ", row$address)
    Sys.sleep(1)   # <-- add this line


    # sanity on OSM ids
    if (!("osm_type" %in% names(row)) || !("osm_id" %in% names(row)) ||
        is.na(row$osm_type) || is.na(row$osm_id)) {
      message(sprintf("⚠ Address '%s' (row %d) missing OSM identifiers — re-geocode in Step 5b.",
                      row$address, file_idx))
      out_paths[j] <- NA_character_
      next
    }


    # fetch and pick
    g_all <- try(fetch_osm_polygon(row$osm_type, row$osm_id, ua = ua), silent = TRUE)
    if (is_bad_sf(g_all)) { if (verbose) message("  fetch failed"); next }
    g_raw <- try(pick_primary_feature(g_all), silent = TRUE)
    if (is_bad_sf(g_raw)) { if (verbose) message("  pick_primary_feature failed"); next }

    # buffer if needed, then normalize + dissolve to ONE MULTIPOLYGON
    g_raw <- try(ensure_areal(g_raw, radius_km_point, buffer_m_line), silent = TRUE)
    if (inherits(g_raw, "try-error")) { if (verbose) message("  ensure_areal failed"); next }

    geom_one <- try(normalize_and_dissolve(g_raw, sliver_m2 = sliver_m2), silent = TRUE)
    if (inherits(geom_one, "try-error") || !inherits(geom_one, "sfc") || length(geom_one) != 1) {
      if (verbose) message("  normalize/dissolve failed; skipping")
      next
    }

    # build clean 1-row sf and write
    g_out <- sf::st_sf(
      data.frame(
        address         = as.character(row$address),
        item_number     = as.character(row$item_number),
        disaster_number = as.character(row$disaster_number),
        src_idx         = as.integer(file_idx),
        stringsAsFactors = FALSE
      ),
      geometry = geom_one
    )

    fpath <- file.path(out_dir, paste0(sprintf("%04d", file_idx), "_", slugify(row$address), ".gpkg"))
    sf::st_write(g_out, fpath, layer = "polygon", driver = "GPKG", delete_layer = TRUE, quiet = TRUE)
    out_paths[j] <- fpath
    if (verbose) message("  wrote: ", fpath)

    # map (never abort on device errors)
    if (make_map) {
      plot_crs <- 3857
      g_plot   <- sf::st_transform(g_out, plot_crs)
      bbm  <- sf::st_bbox(g_plot)
      span <- max(as.numeric(bbm["xmax"] - bbm["xmin"]),
                  as.numeric(bbm["ymax"] - bbm["ymin"]))
      half_m <- max(zoom_km * 1000, 0.6 * span)

      ctr <- try(sf::st_coordinates(sf::st_centroid(sf::st_union(sf::st_geometry(g_plot))))[1, ], silent = TRUE)
      if (inherits(ctr, "try-error") || any(!is.finite(ctr))) {
        ctr <- c(mean(c(bbm["xmin"], bbm["xmax"])), mean(c(bbm["ymin"], bbm["ymax"])))
      }
      xlim <- ctr[1] + c(-half_m, half_m)
      ylim <- ctr[2] + c(-half_m, half_m)

      world_plot <- if (!is.null(world_src)) sf::st_transform(world_src, plot_crs) else NULL

      p <- ggplot2::ggplot() +
        { if (!is.null(world_plot))
          ggplot2::geom_sf(data = world_plot, fill = "grey90", color = "grey70") } +
        ggplot2::geom_sf(data = g_plot, fill = "red", alpha = 0.35, color = "red") +
        ggplot2::coord_sf(crs = sf::st_crs(plot_crs), default_crs = sf::st_crs(plot_crs),
                          xlim = xlim, ylim = ylim, expand = FALSE, datum = NA) +
        ggplot2::labs(title = row$address, subtitle = row$display_name) +
        ggplot2::theme_minimal()

      png_path <- sub("\\.gpkg$", ".png", fpath)
      .safe_save_map(p, png_path, world_plot, g_plot, xlim, ylim, row$address, row$display_name)
      if (verbose) message("  map attempted: ", png_path)
    }
  }

  if (any(is.na(out_paths))) message("Some rows failed; returned NA for those paths.")
  out_paths
}
