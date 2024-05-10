library(leaflet)
library(tidyverse)
library(sf)
library(leaflet.extras)
library(terra)
library(wesanderson)

mokus_sf <- st_read("data/spatial/moku/Moku_NEW.shp", quiet = TRUE) %>% 
  janitor::clean_names() %>% 
  mutate(name = str_remove(name2, "\\s\\w+"),
         name2 = if_else(name2 == "KALAWA", "HALAWA", name2),
         name = if_else(name == "KALAWA", "HALAWA", name),
         island = str_to_upper(island))

# Find a point on the surface of each geometry
points_on_surface <- st_point_on_surface(mokus_sf)

# Extract coordinates
centroid_coords <- st_coordinates(points_on_surface)

# Add lat and lon columns to the original data
mokus_sf$lat <- centroid_coords[, "Y"]
mokus_sf$lon <- centroid_coords[, "X"]

marine_extent_rast <- rast("data/spatial/extent-layers/marine/Benthic_Habitat_5Classes_2022_10.tif")

# Get the unique values (classes) in the raster
unique_classes <- levels(marine_extent_rast)[[1]]$Habitat

marine_extent_rast_no_na <- terra::classify(marine_extent_rast, c(1:length(unique_classes), 0))

# Create the color palette using wes_palette
colors <- as.character(wes_palette("Darjeeling1"))

# Shuffle the colors
shuffled_colors <- sample(colors)

island_labels <- data.frame(
  island = c("HAWAII", 
             "OAHU", 
             "KAUAI", 
             "NIIHAU", 
             "KAHOOLAWE", 
             "LANAI",
             "MOLOKAI", 
             "MAUI"),
  lat = c(19.351647176347488, 
          21.59485141420381, 
          22.401295315702967, 
          21.98668405502797, 
          20.387120892420214, 
          20.778627806569286, 
          21.33686153447943, 
          21.07848084802507),
  lon = c(-156.18654482636637, 
          -157.57739720498617, 
          -159.41289281713267, 
          -160.39025812149933, 
          -156.57098710165647, 
          -157.15534196351234, 
          -157.00290895556614, 
          -156.17145618495033)
)

# MHI centroid
# lat <- 20.482906296022126
# long <- -157.98571042643727

# Oahu centroid
lat <- 21.42
long <- -157.9445

# Create the main map
m <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  setView(long, lat, zoom = 9) %>%
  addPolygons(data = mokus_sf, 
              fillColor = "transparent",
              color = "#5BBCD6", 
              weight = 2) %>%
  addLabelOnlyMarkers(data = mokus_sf, 
                      lng = ~lon, 
                      lat = ~lat, 
                      label = ~name,
                      labelOptions = labelOptions(
                        noHide = TRUE, 
                        textOnly = TRUE, 
                        direction = "center",
                        style = list(
                          color = "white", 
                          fontWeight = "bold", 
                          fontSize = "13px")
                      )) %>%
  addLabelOnlyMarkers(data = island_labels, 
                      lng = ~lon, 
                      lat = ~lat, 
                      label = ~island,
                      labelOptions = labelOptions(
                        noHide = TRUE, 
                        textOnly = TRUE, 
                        direction = "left",
                        style = list(
                          color = "yellow", 
                          fontWeight = "bold", 
                          fontSize = "22px")
                      ))  
  # addRasterImage(marine_extent_rast_no_na, 
  #                colors = shuffled_colors, 
  #                opacity = 0.85,
  #                layerId = "Marine Extents")  %>%
  # addLayersControl(
  #   baseGroups = c("Basemap"),
  #   overlayGroups = c("Marine Extents"),
  #   options = layersControlOptions(collapsed = FALSE)
  # )
  

# # Add legend
# m <- addLegend(
#   map = m,
#   position = "bottomleft",
#   colors = shuffled_colors,
#   labels = unique_classes,
#   title = "Habitat Classes"
# )

# Add an inset map
m <- addMiniMap(
  map = m,
  tiles = providers$Esri.WorldImagery,
  width = 100,
  height = 100,
  position = "bottomleft"
)
