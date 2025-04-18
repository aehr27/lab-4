#task 2, my own map!

library(sf)
library(terra)
library(tmap)
library(tidyverse)
library(dplyr)
library(osmdata)

#data

gallatin <- st_read("./montana/MontanaCounties2021/MontanaCounties.gdb") %>%
  st_make_valid() %>%
  filter(NAME == "GALLATIN") %>%
  st_buffer(0)

bozeman <- st_read("./montana/City_Limits/City_Limits.shp")

yellowstone <- st_read("./montana/Yellowstone/YellowstonePark1995.shp") %>%
  st_make_valid() %>%
  st_transform(st_crs(gallatin)) %>%
  st_intersection(gallatin)

g.forest <- st_read("./montana/NationalForests/NationalForest2002.shp") %>%
  st_make_valid() %>%
  st_transform(st_crs(gallatin)) %>%
  st_intersection(gallatin)

#raster stuff

elev_files <- list.files(path = "./montana/elevation_data",
                         pattern = "\\.tif$",
                         full.names = TRUE)

elev_rasters <- lapply(elev_files, rast)

elev_combined <- sprc(elev_rasters) %>% merge()

target_crs <- crs(gallatin)
elev_projected <- project(elev_combined, target_crs)

gallatin_vect <- vect(gallatin)

elev_cropped <- crop(elev_projected, gallatin_vect)
elev_final <- mask(elev_cropped, gallatin_vect)

#montana state university

msu <- opq(bbox = "Bozeman, Montana") %>%
  add_osm_feature(key = "amenity", value = "university") %>%
  add_osm_feature(key = "name", value = "Montana State University") %>%
  osmdata_sf()

msu.point <- st_centroid(msu$osm_polygons)

msu.point <- st_transform(msu.point, st_crs(gallatin))

#final map!

final_map <- tm_shape(elev_final) +
  #elevation
  tm_raster(
    col.scale = tm_scale(values = "brewer.yl_or_br"),
    col.legend = tm_legend(title = "Elevation (m)"),
    col_alpha = 0.8
  ) +
  
  #polygons
  tm_shape(yellowstone) + tm_polygons(fill = "yellow", fill_alpha = 0.5, col = "darkgoldenrod", lwd = 1) +
  tm_shape(g.forest) + tm_polygons(fill = "green", fill_alpha = 0.5, col = "darkgreen", lwd = 1) +
  tm_shape(bozeman) + tm_polygons(fill = "blue", fill_alpha = 0.7, col = "darkblue", lwd = 1) +
  
  # MSU 
  tm_shape(msu.point) + tm_symbols(shape = 20, size = 0.8, fill = "maroon") +
  
  # County border
  tm_shape(gallatin) + tm_borders(col = "black", lwd = 1) +  
  
  #legend
  tm_add_legend(
    type = "symbols",
    labels = c("Yellowstone NP", "Gallatin NF", "Bozeman City", "MSU Campus"),
    col = c("darkgoldenrod", "darkgreen", "darkblue", "maroon"),
    fill = c("yellow", "green", "blue", "maroon"),
    shape = c(22, 22, 22, 20),  
    title = "Key Locations",
    size = 0.7
  ) +
  
  # Title and subtitle
  tm_title("ECOLOGICAL RESEARCH SITES\nGALLATIN COUNTY, MONTANA", 
           position = tm_pos_out ("center", "top"),
           size = 0.9,
           fontface = "bold",
           just ="center") +
  
  tm_credits("Montana State University SES REU | Summer 2025",
             position = tm_pos_out("center", "top"),
             size = 0.9,
             just = "center",
             y = 0.95) +  
  
  # Layout
  tm_layout(
    legend.position = tm_pos_out("right", "center"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.7,
    frame = FALSE,
    inner.margins = c(0.05, 0.1, 0.05, 0.05) 
  ) +
  
  # Map elements
  tm_compass(position = tm_pos_in("left", "top"), size = 1.5) +
  tm_scalebar(position = tm_pos_in("left", "bottom"), breaks = c(0, 10, 20), text.size = 0.7) +
  
  tmap_options(component.autoscale = FALSE)

final_map
