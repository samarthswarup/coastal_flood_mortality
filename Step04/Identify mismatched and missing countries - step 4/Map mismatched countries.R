#### Identify SLIIDERS tiles with mismatched country codes ####


library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Read the shapefile
shapefile_path <- "/Users/rsdc/Library/CloudStorage/OneDrive-VirginiaTech/NASA HAQ June 2021/Flood mortality risk – please clean up/SLIIDERS/SLIIDERS_SHP/sliiders_v1_1_seg_voronoi.shp"
shapefile_data <- st_read(shapefile_path)

# Display the structure of the shapefile data
str(shapefile_data)

# Plot the shapefile
plot(shapefile_data)

# Filter features where seg_countr is unclear
filtered_data_znc <- shapefile_data[shapefile_data$seg_countr == "ZNC", ]
filtered_data_xad <- shapefile_data[shapefile_data$seg_countr == "XAD", ]
filtered_data_xpi <- shapefile_data[shapefile_data$seg_countr == "XPI", ]
filtered_data_xcl <- shapefile_data[shapefile_data$seg_countr == "XCL", ]
filtered_data_nld <- shapefile_data[shapefile_data$seg_countr == "NLD", ]
filtered_data_esp <- shapefile_data[shapefile_data$seg_countr == "ESP", ]

## XCL is Clipperton Island, France
## XSP is uninhabited island claimed by China


# Expand the bounding box by 2 degrees in all directions


# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

bbox <- st_bbox(filtered_data_esp)
bbox_expanded <- bbox
bbox_expanded["xmin"] <- bbox["xmin"] - 10
bbox_expanded["xmax"] <- bbox["xmax"] + 10
bbox_expanded["ymin"] <- bbox["ymin"] - 10
bbox_expanded["ymax"] <- bbox["ymax"] + 10


# Create a base map with ggplot2
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +  # Base map of the world
  geom_sf(data = filtered_data_esp, aes(fill = seg_countr), color = "black") +  # Highlight filtered features
  scale_fill_manual(values = c("ESP" = "blue")) +  # Set color for NLD features
  coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]), ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), expand = FALSE) +  # Zoom to expanded bounding box
  theme_minimal() +
  labs(title = "Map of Features with seg_countr == 'ESP'",
       fill = "Segment Country")

ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +  # Base map of the world
  geom_sf(data = filtered_data_xad, aes(fill = seg_countr), color = "black") +  # Highlight filtered features
  scale_fill_manual(values = c("XAD" = "blue")) +  # Set color for XAD features
  coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]), ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), expand = FALSE) +  # Zoom to expanded bounding box
  theme_minimal() +
  labs(title = "Map of Features with seg_countr == 'XAD'",
       fill = "Segment Country")


ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +  # Base map of the world
  geom_sf(data = filtered_data_xpi, aes(fill = seg_countr), color = "black") +  # Highlight filtered features
  scale_fill_manual(values = c("XPI" = "blue")) +  # Set color for XPI features
  coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]), ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), expand = FALSE) +  # Zoom to expanded bounding box
  theme_minimal() +
  labs(title = "Map of Features with seg_countr == 'XPI'",
       fill = "Segment Country")


bbox <- st_bbox(filtered_data_xcl)
bbox_expanded <- bbox
bbox_expanded["xmin"] <- bbox["xmin"] - 10
bbox_expanded["xmax"] <- bbox["xmax"] + 10
bbox_expanded["ymin"] <- bbox["ymin"] - 10
bbox_expanded["ymax"] <- bbox["ymax"] + 20


ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +  # Base map of the world
  geom_sf(data = filtered_data_xcl, aes(fill = seg_countr), color = "black") +  # Highlight filtered features
  scale_fill_manual(values = c("XCL" = "blue")) +  # Set color for XCL features
  coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]), ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), expand = FALSE) +  # Zoom to expanded bounding box
  theme_minimal() +
  labs(title = "Map of Features with seg_countr == 'XCL'",
       fill = "Segment Country")
