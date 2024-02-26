################################################################################
# MAKE STUDY SITES MAP
#
################################################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(fuzzyjoin)
library(ggspatial)
library(patchwork)

# map data
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

# load data
location_resights <- readRDS("./processed-data/location-resights.rds")

location_rounded <- location_resights %>% 
  mutate(Latitude = round(Latitude),
         Longitude = round(Longitude)) %>% 
  group_by(Latitude, Longitude) %>% 
  mutate(n = n()) %>% 
  select(Latitude, Longitude, n) %>% 
  distinct() %>% 
  ungroup()

# map jb resights
#windowsFonts(Times=windowsFont("TT Times New Roman"))

location_jb_resights <- location_resights %>% 
  filter(ProjectID == 22)

location_jb_rounded <- location_jb_resights %>% 
  mutate(Latitude = round(Latitude, 2),
         Longitude = round(Longitude, 2)) %>% 
  group_by(Latitude, Longitude) %>% 
  mutate(n = n()) %>% 
  select(Latitude, Longitude, n) %>% 
  distinct() %>% 
  ungroup()


jb_map <- ne_states(country = "Canada", returnclass = "sf") %>% 
  filter(name %in% c("Ontario", "Québec", "Nunavut"))

xmin_jb <- min(location_jb_resights$Longitude) - 2
xmax_jb <- max(location_jb_resights$Longitude) + 2
ymin_jb <- min(location_jb_resights$Latitude) - 1.5
ymax_jb <- max(location_jb_resights$Latitude) + 2

annotation <- data.frame(
  x = c(-80, -81.5),
  y = c(52.5, 53.05),
  label = c("James Bay", "Akimiski\n  Island"))

# create study area buffer
# jb_buf <- location_jb_resights %>% 
#   st_as_sf(coords = c("Longitude", "Latitude")) %>% 
#   st_set_crs(4326) %>% 
#   st_transform(2958) %>% 
#   st_buffer(dist = 10000) %>% 
#   st_union() %>% 
#   #st_buffer(dist = -10000) %>% 
#   st_transform(crs = 4326)

bbox_jb <- st_bbox(location_jb_resights_sf) %>% 
  st_as_sfc() %>% 
  st_transform(crs = 4326)

jb_coast <- ne_coastline(scale = 10, returnclass = "sf") %>% 
  st_intersection(., bbox_jb) %>% 
  st_buffer(dist = 2000)



# create map
jb_resights_map <- ggplot(data = jb_map) +
  geom_sf(colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_sf(data = jb_coast, aes(colour = "black"), fill = "grey10", alpha = 0.8) +
  # geom_point(data = location_jb_rounded,
  #            aes(x = Longitude, y = Latitude, size = n),
  #            shape = 16, colour = "black", alpha = 0.5) +
  geom_text(data=annotation, aes(x=x, y=y, label=label), size = 3) +
  scale_colour_identity(guide = "legend", labels = c("Study area")) +
  annotation_scale(location = "br", line_width = 0.2,
                   text_cex = 0.5, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.6, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_orienteering(line_width = 0.5,
                                                          text_size = 3),
                         height = unit(0.3, "cm"), width = unit(0.3, "cm")) +
  #scale_size_continuous(name = "number of\nresights", range = c(0.5, 3)) +
  coord_sf(xlim = c(xmin_jb, xmax_jb), ylim = c(ymin_jb, ymax_jb), expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 8))

# # map all included resights
# xmin <- min(location_resights$Longitude) - 15
# xmax <- max(location_resights$Longitude) + 5
# ymin <- min(location_resights$Latitude) - 5
# ymax <- max(location_resights$Latitude) + 20
# 
# resights_map <- ggplot(data = world) +
#   geom_sf(colour = NA) +
#   geom_sf(data = lakes, fill = "white", colour = NA) +
#   geom_point(data = location_rounded,
#              aes(x = Longitude, y = Latitude, size = n),
#              shape = 16, colour = "black", alpha = 0.5) +
#   geom_rect(aes(xmin = xmin_jb, xmax = xmax_jb, ymin = ymin_jb, ymax = ymax_jb),
#             fill = NA, colour = "black", size = 0.5) +
#   annotation_scale(location = "bl", line_width = 0.25,
#                    text_cex = 0.5, height = unit(0.15, "cm")) +
#   annotation_north_arrow(location = "bl", which_north = "true",
#                          pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
#                          style = north_arrow_orienteering,
#                          height = unit(0.75, "cm"), width = unit(0.75, "cm")) +
#   #scale_size_continuous(name = "number of\nresights", range = c(0.1, 2)) +
#   coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_line(colour = "transparent"),
#         legend.position = "bottom", legend.box = "horizontal",
#         legend.title = element_text(size = 6),
#         legend.text = element_text(size = 6))
# 
# png(filename = "./figures/study-sites-size-map.png", height = 4, width = 6,
#     units = "in", res = 600)
# 
# print(resights_map + jb_resights_map +
#         plot_annotation(tag_levels = 'a'))
# 
# dev.off()

################################################################################

# re-project data to equal area projection
world_sf <- world %>% 
  st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

lakes_sf <- lakes %>% 
  st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

poly_coord_df <- data.frame(lon = c(xmax_jb, xmin_jb),
                            lat = c(ymax_jb, ymin_jb))

pol = st_polygon(
  list(
    cbind(
      poly_coord_df$lon[c(1,2,2,1,1)], 
      poly_coord_df$lat[c(1,1,2,2,1)])))

poly = st_sfc(pol, crs = 4326) %>% 
  st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# poly <- poly_coord_df %>% 
#   st_as_sf(coords = c("lon", "lat"), 
#            crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
#   st_bbox() %>% 
#   st_as_sfc()

location_jb_resights_sf <- st_as_sf(location_jb_resights, coords = c("Longitude", "Latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(2958)
# 
# poly_jb_coord_df <- data.frame(lon = c(xmax_jb, xmin_jb),
#                                lat = c(ymax_jb, ymin_jb))
# 
# pol_jb = st_polygon(
#   list(
#     cbind(
#       poly_coord_df$lon[c(1,2,2,1,1)], 
#       poly_coord_df$lat[c(1,1,2,2,1)])))
# 
# poly_jb = st_sfc(pol, crs = 4326) %>% 
#   st_transform(crs = 2958)
# 
bbox_jb <- location_jb_resights_sf %>%
  st_bbox()
# 
# xmin <- bbox[1] - 50000
# ymin <- bbox[2] - 50000
# xmax <- bbox[3] + 50000
# ymax <- bbox[4] + 50000
# 
# # create study area buffer
# jb_buf <- location_jb_resights_sf %>% 
#   st_buffer(dist = 15000) %>% 
#   st_union()
# 
# jb_map_sf <- jb_map %>% 
#   st_transform(crs = 2958)
# 
# anno_sf <- st_as_sf(annotation, coords = c("x", "y")) %>%
#   st_set_crs(4326) %>% 
#   st_transform(2958)
# 
# # create map
# jb_resights_map_sf <- ggplot() +
#   geom_sf(data = jb_map_sf, colour = NA) +
#   geom_sf(data = lakes_sf, fill = "white", colour = NA) +
#   geom_sf(data = jb_buf,
#           fill = "black", alpha = 0.5) +
#   #geom_sf(data = anno_sf, aes(label=label), size = 2) +
#   annotation_scale(location = "br", line_width = 0.2,
#                    text_cex = 0.5, height = unit(0.1, "cm")) +
#   annotation_north_arrow(location = "br", which_north = "true",
#                          pad_x = unit(0.6, "cm"), pad_y = unit(0.5, "cm"),
#                          style = north_arrow_orienteering(line_width = 0.5,
#                                                           text_size = 3),
#                          height = unit(0.3, "cm"), width = unit(0.3, "cm")) +
#   coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_line(colour = "transparent"),
#         legend.position = "bottom", legend.box = "horizontal",
#         legend.title = element_text(size = 6),
#         legend.text = element_text(size = 6))
# 
# jb_resights_map_sf

# all resights
location_rounded_sf <- st_as_sf(location_rounded, coords = c("Longitude", "Latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# get bounding box for data and set limits for map
bbox <- location_rounded_sf %>% 
  st_bbox()

xmin <- bbox[1] - 2100000
ymin <- bbox[2] - 1200000
xmax <- bbox[3] + 1400000
ymax <- bbox[4] + 1000000

# plot map
resights_map_sf <- ggplot() +
  geom_sf(data = world_sf, colour = NA) +
  geom_sf(data = lakes_sf, fill = "white", colour = NA) +
  geom_sf(data = location_rounded_sf,
             aes(size = n),
             shape = 16, colour = "black", alpha = 0.5) +
  geom_sf(data = poly,
            fill = NA, colour = "black", size = 0.25) +
  annotation_scale(location = "br", line_width = 0.2,
                   text_cex = 0.5, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.6, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_orienteering(line_width = 0.5,
                                                          text_size = 3),
                         height = unit(0.3, "cm"), width = unit(0.3, "cm")) +
  scale_size_binned(name = "Number of\nresights", breaks = c(1, 100, 500, 1000, 10000, 19200),
                    range = c(0.25, 3)) + # range = c(0.25, 2)
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6))

resights_map_sf

# export plot
png(filename = "./figures/study-sites-size-map-sf-binned.png", height = 6, width = 7,
    units = "in", res = 1200)

print(jb_resights_map + resights_map_sf +
        plot_annotation(title = 'Figure 1',
                        tag_levels = 'A'))

dev.off()
