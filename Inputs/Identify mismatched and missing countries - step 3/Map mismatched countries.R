##########################################
# Map mismatched SLIIDERS tiles          #
# to verify inputs for Step 3            #
#                                        #
#                                        #
# 2025-06-24                             #
# Ryan Calder                            #
##########################################

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(utils)
library(ggplot2)

# Unzip and read the shapefile
unzip(zipfile = "Input/SLIIDERS data/SLIIDERS_SHP.zip",
      exdir="Input/SLIIDERS data/")
shapefile_path = 
  "Input/SLIIDERS data/SLIIDERS_SHP/sliiders_v1_1_seg_voronoi.shp"
shapefile_data <- st_read(shapefile_path)

# Display the structure of the shapefile data
str(shapefile_data)

# Plot the shapefile
plot(shapefile_data)

# Filter features where seg_countr is unknown
filtered_data_ANT <- shapefile_data[shapefile_data$seg_countr == "BES", ]
filtered_data_XAD <- shapefile_data[shapefile_data$seg_countr == "XAD", ]
filtered_data_XCL <- shapefile_data[shapefile_data$seg_countr == "XCL", ]
filtered_data_XPI <- shapefile_data[shapefile_data$seg_countr == "XPI", ]
filtered_data_XPL <- shapefile_data[shapefile_data$seg_countr == "XPL", ]
filtered_data_XSP <- shapefile_data[shapefile_data$seg_countr == "XSP", ]
filtered_data_ZNC <- shapefile_data[shapefile_data$seg_countr == "ZNC", ]

## XAD is Cyprus
## XCL is Clipperton Island, France
## XPI is uninhabited island claimed by China
## XPL has no shape data
## XSP is uninhabited island claimed by China
## ZNC is Northern Cyprus


# Filter features to look at overseas parts of Netherlands (none -- they have
# their own country codes) and Spain (Canary Islands part of ESP)

filtered_data_NLD <- shapefile_data[shapefile_data$seg_countr == "NLD", ]
filtered_data_ESP <- shapefile_data[shapefile_data$seg_countr == "ESP", ]


# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Set variable below to declare which filtered_data_XXX country you want to
# make a map for

map_country = "ESP"

bbox <- st_bbox(get(paste0("filtered_data_", map_country)))
bbox_expanded <- bbox
bbox_expanded["xmin"] <- bbox["xmin"] - 10
bbox_expanded["xmax"] <- bbox["xmax"] + 10
bbox_expanded["ymin"] <- bbox["ymin"] - 10
bbox_expanded["ymax"] <- bbox["ymax"] + 10


# Create a  map with ggplot2
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +
  geom_sf(data = get(paste0("filtered_data_", map_country)), aes(fill = seg_countr), color = "black") +
  scale_fill_manual(values = setNames("blue", map_country)) +
  coord_sf(
    xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
    ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = paste0("Map of Features with seg_countr == '", map_country, "'"),
    fill = "Segment Country"
  )

