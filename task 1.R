
library(sf)
library(terra)
library(tmap)
library(spData) 
library(tidyverse)
library(spDataLarge)
library(grid)

#Task 1 - Ohio Map
  #provided help

sa <- read_sf("./oh_data/oh_counties.gpkg") %>%
  dplyr::filter(NAME %in% c("Portage", "Summit"))

allohio  <- read_sf("./oh_data/oh_counties.gpkg")

oh_region = st_bbox(sa) %>%
  st_as_sfc()

myras <- rast("./oh_data/neoh_dem.tif") %>% 
  terra::project(., "EPSG:4269") %>%
  terra::crop(sa)

parks <- read_sf("./oh_data/oh_parks.gpkg")

parks.sa <- parks %>% st_transform(st_crs(sa)) %>%
  st_intersection(., sa)

oh_height_map = tm_shape(myras, bbox = oh_region) +
  tm_raster(col.scale = tm_scale_continuous(values = "YlGn"),
            col.legend = tm_legend(position = c("left", "top"))) +
  tm_shape(sa) + tm_polygons(fill_alpha = .2) +
  tm_shape(parks.sa) + tm_polygons(fill = "FEATTYPE", fill_alpha = .5) +
  tm_scalebar(position = c("left", "bottom"))

oh_height_map

oh_map = tm_shape(allohio) + tm_polygons() + 
  tm_shape(oh_region) + tm_borders(lwd = 3) +
  tm_layout(bg.color = "lightblue") +
  tm_scalebar(position = c("left", "bottom"))

oh_map

norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}

main_dim = norm_dim(oh_region)

ins_dim = norm_dim(sa)

main_vp = viewport(width = main_dim[1], height = main_dim[2])

ins_vp = viewport(width = ins_dim[1] * 0.6, height = ins_dim[2] * 0.6,
                  x = unit(1, "npc") - unit(0.6, "cm"), y = unit(0.1, "npc"),
                  just = c("right", "bottom"))

grid.newpage()
print(oh_height_map, vp = main_vp)
pushViewport(main_vp)
print(oh_map, vp = ins_vp)


#My map?

study_area <- st_read("./oh_data/oh_counties.gpkg") %>% 
  filter(NAME %in% c("Portage", "Summit")) %>%
  st_make_valid() %>%
  st_transform(4326)

allohio <- read_sf("./oh_data/oh_counties.gpkg")

ohio_data <- data.frame(
  NAME = allohio$NAME,
  population = runif(nrow(allohio), 10000, 500000) 
)


allohio <- left_join(allohio, ohio_data, by = "NAME")

oh_region = st_bbox(study_area) %>% st_as_sfc()

elevation <- rast("./oh_data/neoh_dem.tif") %>% 
  project("EPSG:4269") %>% 
  crop(study_area) %>% 
  mask(vect(study_area))

parks <- st_read("./oh_data/oh_parks.gpkg") %>% 
  st_transform(4326) %>% 
  st_make_valid() %>%
  st_intersection(study_area)

places <- st_read("./oh_data/oh_places.gpkg") %>% 
  st_transform(4326) %>%
  st_make_valid() %>%
  st_intersection(study_area)

rivers <- st_read("./oh_data/oh_rivers.gpkg") %>% 
  st_transform(4326) %>%
  st_make_valid() %>%
  st_intersection(study_area)

study_area_proj <- study_area %>% 
  st_transform(st_crs(rast("./oh_data/neoh_dem.tif")))

#ohio map

oh_map <- tm_shape(allohio) + 
  tm_polygons(col = "population", title = "Population", 
              palette = "Blues", border.col = "gray30") +
  tm_shape(oh_region) + 
  tm_borders(lwd = 3, col = "red", lty = "dashed") + # Non-default border symbols
  tm_layout(bg.color = "lightblue", frame = TRUE) 
  
oh_map

#local map

view(places)

local_map <- tm_shape(elevation) +
  tm_raster(palette = "scico.gray_c") +
  tm_shape(places) +
  tm_borders(col = "gray") +
  tm_text("NAME", col = "black", remove.overlap = FALSE, auto.placement = TRUE, size = 0.4) + #I cannot make all the names show up for the life of me 
  tm_shape(parks) +
  tm_polygons(col = "FEATTYPE", palette = "Greens", alpha = 0.7) +
  tm_shape(study_area) +
  tm_borders(lwd = 2) +
  tm_shape(rivers) + #I feel like there should be more?? 
  tm_lines(col = "blue") +
  tm_layout(legend.outside = TRUE)


local_map

#combo map

norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}

main_dim = norm_dim(study_area)
ins_dim = norm_dim(oh_region)

main_vp = viewport(width = main_dim[1] * 1.5, height = main_dim[2] * 1.5,
                   x = unit(0.5, "npc"), y = unit(0.45, "npc"))

ins_vp = viewport(width = ins_dim[1] * 0.45, height = ins_dim[2] * 0.45,
                  x = unit(0.94, "npc"), y = unit(0.08, "npc"),
                  just = c("right", "bottom"))

oh_map_inset <- oh_map + tm_layout(legend.show = FALSE)

local_map_title <- local_map +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(position = tm_pos_out("right", "top")) +
  tm_layout(
    legend.outside = TRUE,
    legend.text.size = 0.5,
    legend.title.size = 0.6,
    frame = FALSE
  )

grid.newpage()

grid.text("Summit & Portage Counties Topography with Parks, Rivers, and Places",
          x = unit(0.5, "npc"), y = unit(0.97, "npc"),
          gp = gpar(fontsize = 16, fontface = "bold"))

print(local_map_title, vp = main_vp)
pushViewport(main_vp)
print(oh_map_inset, vp = ins_vp)