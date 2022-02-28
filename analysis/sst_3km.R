library(tidyverse) 
library(ncdf4) 
library(raster) 
library(sf) 
library(rnaturalearth) 

# load 30km buffer shapefile
coast_30km <- st_read("./map-data/atl_coast_30km.shp")

# # shrink to 3km buffer
# coast_3km <- st_buffer(coast_30km, dist = -27000)
# 
# ggplot() +
#   geom_sf(data = coast_30km, alpha = 0.5) +
#   geom_sf(data = coast_3km)

coast <- ne_coastline(scale = "large", returnclass = "sf")

states <- ne_states(country = "United States of America", returnclass = "sf") %>% 
  filter(postal %in% c("ME", "NH", "MA", "RI", "CT", "NY", "NJ", "DE", "MD",
                       "VA", "NC", "SC", "GA", "FL", "AL", "MS"))

coastline <- st_transform(coast, crs = 5070)

states <- st_transform(states, crs = 5070)

atl_coast <- st_crop(coastline, st_bbox(states))

atl_coast <- st_intersection(atl_coast, coast_30km)

atl_coast_buf <- st_buffer(atl_coast, dist = 3000)

atl_coast_buf <- st_union(atl_coast_buf)

ggplot() +
  geom_sf(data = atl_coast_buf) #+
  #geom_sf(data = coast_3km)

test <- nc_open("./data/sst/20100301090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc")

{
  sink("./data/sst/20100301090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc_metadata.txt")
  print(test)
  sink()
}

test <- raster(test)

# plot
test_df <- as.data.frame(test, xy = TRUE) %>% 
  filter(!is.na(layer)) #%>% 
  #mutate(year = 2010)

ggplot() +
  #geom_sf(data = atl_coast_buf, fill = NA, colour = "black") +
  geom_tile(data = test_df, aes(x = x, y = y, fill = layer))

################################################################################

coast_3km <- atl_coast_buf %>% 
  st_transform(crs = 4326)

coast_3km <- coast_3km %>%
  #st_transform(st_crs(murSST$data)) %>% 
  st_as_sf()

library(rerddap)
library(rerddapXtracto)
library(mapdata)

sstInfo <- info('jplMURSST41')
# get latest daily sst
murSST <- griddap(sstInfo, latitude = c(24.5151, 45.26106), longitude = c(-89.86494, -66.76048),
                  time = c('last','last'), fields = 'analysed_sst')

mycolor <- colors$temperature
w <- map_data("worldHires", ylim = c(24.5151, 45.26106), xlim = c(-89.86494, -66.76048))
ggplot(data = murSST$data, aes(x = lon, y = lat, fill = analysed_sst)) + 
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  geom_sf(data = coast_3km, colour = "black") +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(-89.86494, -66.76048),  ylim = c(24.5151, 45.26106)) + ggtitle("Latest MUR SST")

################################################################################

coast_3km_coord <- st_coordinates(coast_3km) %>% 
  as.data.frame()

write.csv(coast_3km_coord, "./map-data/coast_3km_coord.csv")

poly <- read.csv("./map-data/coast_3km_coord.csv", header = TRUE)[,2:3]

parameter <- sstInfo$variables$variable_name[1]

xcoord <- poly$X
ycoord <- poly$Y

tcoord <- c("2010-05-01", "2010-05-01")

murSST_2010 <- rxtractogon(sstInfo, parameter = parameter, xcoord = xcoord, ycoord = ycoord,
                           tcoord = tcoord)

ggplot(data = murSST_2010$data, aes(x = lon, y = lat, fill = analysed_sst)) + 
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  geom_sf(data = coast_3km, colour = "black") +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(-89.86494, -66.76048),  ylim = c(24.5151, 45.26106)) + ggtitle("Latest MUR SST")

################################################################################

# me_va <- states %>% 
#   filter(postal %in% c("ME", "NH", "MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA")) %>% 
#   st_transform(crs = 4326)
# 
# me_va_bbox <- st_bbox(me_va) %>% 
#   st_as_sfc() #%>% 
#   as.data.frame(., xy = TRUE)
# 
# nc_ga <- states %>% 
#   filter(postal %in% c("NC", "SC", "GA")) %>% 
#   st_transform(crs = 4326)
# 
# nc_ga_bbox <- st_bbox(nc_ga) %>% 
#   st_as_sfc()
# 
# fl_ms <- states %>% 
#   filter(postal %in% c("FL", "AL", "MS")) %>% 
#   st_transform(crs = 4326)
# 
# fl_ms_bbox <- st_bbox(fl_ms) %>% 
#   st_as_sfc()

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

world <- world %>% 
  st_transform(st_crs(me_de_bbox))

lakes <- lakes %>% 
  st_transform(st_crs(me_de_bbox))


# ggplot() +
#   geom_sf(data = world, colour = NA) +
#   geom_sf(data = lakes, fill = "white", colour = NA) +
#   geom_sf(data = me_va_bbox, fill = NA) +
#   geom_sf(data = nc_ga_bbox, fill = NA) +
#   geom_sf(data = fl_ms_bbox, fill = NA)
  
lat <- c(30.8, 24.3)
lon <- c(-79.7, -89.2)
fl_ms_df <- data.frame(lon, lat)
fl_ms_poly <- fl_ms_df %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

lat <- c(36.6, 30.8)
lon <- c(-75.5, -81.7)
nc_ga_df <- data.frame(lon, lat)
nc_ga_poly <- nc_ga_df %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

lat <- c(44.9, 36.6)
lon <- c(-66.9, -77.4)
me_va_df <- data.frame(lon, lat)
me_va_poly <- me_va_df %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

xmin <- min(fl_ms_df$lon) - 3
xmax <- max(me_va_df$lon) + 3
ymin <- min(fl_ms_df$lat) - 3
ymax <- max(me_va_df$lat) + 3

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_sf(data = fl_ms_poly, fill = NA) +
  geom_sf(data = nc_ga_poly, fill = NA) +
  geom_sf(data = me_va_poly, fill = NA) +
  geom_sf(data = coast_30km_simp, fill = NA) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE)
  
 coast_30km_simp <- st_simplify(coast_30km) %>% 
   st_transform(crs = 4326)
 
#################################################################################

sstInfo <- info('jplMURSST41')

# get latest daily sst
(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2010-05-01", "2010-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

mean_sst <- murSST$data %>%
  #filter(!is.na(analysed_sst)) %>% 
  group_by(lon, lat) %>% 
  summarize(sst = mean(analysed_sst, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(x = lon, y = lat) %>% 
  as.data.frame()

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_raster(data = mean_sst, aes(x = lon, y = lat, fill = sst), interpolate = FALSE) +
  # geom_sf(data = fl_ms_poly, fill = NA) +
  # geom_sf(data = nc_ga_poly, fill = NA) +
  # geom_sf(data = me_va_poly, fill = NA) +
  geom_sf(data = coast_30km_simp, fill = NA) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE)

# remap latitiudes and longitudes to even grid
# myLats <- unique(mean_sst$y)
# myLons <- unique(mean_sst$x)
# myLats <- seq(range(myLats)[1], range(myLats)[2], length.out = length(myLats))
# myLons <- seq(range(myLons)[1], range(myLons)[2], length.out = length(myLons))
# # melt these out to full grid
# mapFrame <- expand.grid(x = myLons, y = myLats)
# mapFrame$y <- rev(mapFrame$y)
# # form a frame with the new values and the data
# tempFrame <- data.frame(sst = mean_sst$sst, y = mapFrame$y, x = mapFrame$x)
# 
# mean_sst_rast <- rasterFromXYZ(tempFrame, crs = 4326)

ext <- extent(mean_sst[,1:2])

rast <- raster(ext)

mean_sst_raster <- raster::rasterize(mean_sst[,1:2], mean_sst[,3])

mean_sst_coast <- raster::mask(mean_sst, coast_3km)

################################################################################

nc_2010 <- nc_open("./data/sst/griddap/ncga_2010.nc")

sst_2010_brick <- brick("./data/sst/griddap/ncga_2010.nc", varname = "analysed_sst")

atl_coast_hsc <- atl_coast_buf %>% 
  st_transform(st_crs(sst_2010_brick)) %>% 
  st_as_sf()

sst_2010_brick <- raster::mask(sst_2010_brick, atl_coast_hsc)

sst_2010_mean <- calc(sst_2010_brick, fun = mean, na.rm = TRUE)

# plot
sst_2010_mean_df <- as.data.frame(sst_2010_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2010)

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2010_mean_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE)
  
