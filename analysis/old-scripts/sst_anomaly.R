###################### SEA SURFACE TEMPERATURE ANOMALY #########################

library(tidyverse) 
library(ncdf4) 
library(raster) 
library(sf) 
library(rnaturalearth)
library(rerddap)
library(viridis)

# download GHRSST MUR v4.1 data (1-km resolution) as netCDF files
# only need to do this once

sstInfo <- info('jplMURSST41anommday')

# NC to GA
murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                  time = c("2010-05-01", "2010-05-31"), fields = 'sstAnom',
                  store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2011-05-01", "2011-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2012-05-01", "2012-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2013-05-01", "2013-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2014-05-01", "2014-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2015-05-01", "2015-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2016-05-01", "2016-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2017-05-01", "2017-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2018-05-01", "2018-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2019-05-01", "2019-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

# ME to VA
murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2010-05-01", "2010-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2011-05-01", "2011-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2012-05-01", "2012-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2013-05-01", "2013-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2014-05-01", "2014-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2015-05-01", "2015-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2016-05-01", "2016-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2017-05-01", "2017-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2018-05-01", "2018-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2019-05-01", "2019-05-31"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

# FL to MS
murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2010-04-01", "2010-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2011-04-01", "2011-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2012-04-01", "2012-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2013-04-01", "2013-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2014-04-01", "2014-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2015-04-01", "2015-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2016-04-01", "2016-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2017-04-01", "2017-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2018-04-01", "2018-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2019-04-01", "2019-04-30"), fields = 'sstAnom',
                   store = disk(path = "./data/sst-anomaly/", overwrite = TRUE))

rm(sstInfo, murSST)

atl_coast_hsc <- st_read("./map-data/atl_coast_3km.shp")

####---- 2010 ----####

# NC to GA
ncga_2010_nc <- nc_open("./data/sst-anomaly/ncga_2010.nc")

sst_ncga_2010 <- raster("./data/sst-anomaly/ncga_2010.nc", varname = "sstAnom")

atl_coast_hsc <- st_transform(atl_coast_hsc, crs = st_crs(sst_ncga_2010))

sst_ncga_2010 <- raster::mask(sst_ncga_2010, atl_coast_hsc)

# ME to GA
meva_2010_nc <- nc_open("./data/sst-anomaly/meva_2010.nc")

sst_meva_2010 <- brick("./data/sst-anomaly/meva_2010.nc", varname = "sstAnom")

sst_meva_2010 <- raster::mask(sst_meva_2010, atl_coast_hsc)

# FL to MS
flms_2010_nc <- nc_open("./data/sst-anomaly/flms_2010.nc")

sst_flms_2010 <- brick("./data/sst-anomaly/flms_2010.nc", varname = "sstAnom")

sst_flms_2010 <- raster::mask(sst_flms_2010, atl_coast_hsc)

# combine rasters
sst_2010 <- raster::merge(sst_ncga_2010, sst_meva_2010)
sst_2010 <- raster::merge(sst_2010, sst_flms_2010)

# calculate overall mean
sst_vals_2010 <- getValues(sst_2010)
sst_anomaly_2010 <- mean(sst_vals_2010, na.rm=TRUE)

# plot
sst_2010_anom_df <- as.data.frame(sst_2010, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2010)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2010_anom_df$x) - 1
xmax <- max(sst_2010_anom_df$x) + 1
ymin <- min(sst_2010_anom_df$y) - 1
ymax <- max(sst_2010_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2010_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2011 ----####

# NC to GA
ncga_2011_nc <- nc_open("./data/sst-anomaly/ncga_2011.nc")

sst_ncga_2011 <- raster("./data/sst-anomaly/ncga_2011.nc", varname = "sstAnom")

sst_ncga_2011 <- raster::mask(sst_ncga_2011, atl_coast_hsc)

# ME to GA
meva_2011_nc <- nc_open("./data/sst-anomaly/meva_2011.nc")

sst_meva_2011 <- brick("./data/sst-anomaly/meva_2011.nc", varname = "sstAnom")

sst_meva_2011 <- raster::mask(sst_meva_2011, atl_coast_hsc)

# FL to MS
flms_2011_nc <- nc_open("./data/sst-anomaly/flms_2011.nc")

sst_flms_2011 <- brick("./data/sst-anomaly/flms_2011.nc", varname = "sstAnom")

sst_flms_2011 <- raster::mask(sst_flms_2011, atl_coast_hsc)

# combine rasters
sst_2011 <- raster::merge(sst_ncga_2011, sst_meva_2011)
sst_2011 <- raster::merge(sst_2011, sst_flms_2011)

# calculate overall mean
sst_vals_2011 <- getValues(sst_2011)
sst_anomaly_2011 <- mean(sst_vals_2011, na.rm=TRUE)

# plot
sst_2011_anom_df <- as.data.frame(sst_2011, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2011)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2011_anom_df$x) - 1
xmax <- max(sst_2011_anom_df$x) + 1
ymin <- min(sst_2011_anom_df$y) - 1
ymax <- max(sst_2011_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2011_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2012 ----####

# NC to GA
ncga_2012_nc <- nc_open("./data/sst-anomaly/ncga_2012.nc")

sst_ncga_2012 <- raster("./data/sst-anomaly/ncga_2012.nc", varname = "sstAnom")

sst_ncga_2012 <- raster::mask(sst_ncga_2012, atl_coast_hsc)

# ME to GA
meva_2012_nc <- nc_open("./data/sst-anomaly/meva_2012.nc")

sst_meva_2012 <- brick("./data/sst-anomaly/meva_2012.nc", varname = "sstAnom")

sst_meva_2012 <- raster::mask(sst_meva_2012, atl_coast_hsc)

# FL to MS
flms_2012_nc <- nc_open("./data/sst-anomaly/flms_2012.nc")

sst_flms_2012 <- brick("./data/sst-anomaly/flms_2012.nc", varname = "sstAnom")

sst_flms_2012 <- raster::mask(sst_flms_2012, atl_coast_hsc)

# combine rasters
sst_2012 <- raster::merge(sst_ncga_2012, sst_meva_2012)
sst_2012 <- raster::merge(sst_2012, sst_flms_2012)

# calculate overall mean
sst_vals_2012 <- getValues(sst_2012)
sst_anomaly_2012 <- mean(sst_vals_2012, na.rm=TRUE)

# plot
sst_2012_anom_df <- as.data.frame(sst_2012, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2012)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2012_anom_df$x) - 1
xmax <- max(sst_2012_anom_df$x) + 1
ymin <- min(sst_2012_anom_df$y) - 1
ymax <- max(sst_2012_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2012_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2013 ----####

# NC to GA
ncga_2013_nc <- nc_open("./data/sst-anomaly/ncga_2013.nc")

sst_ncga_2013 <- raster("./data/sst-anomaly/ncga_2013.nc", varname = "sstAnom")

sst_ncga_2013 <- raster::mask(sst_ncga_2013, atl_coast_hsc)

# ME to GA
meva_2013_nc <- nc_open("./data/sst-anomaly/meva_2013.nc")

sst_meva_2013 <- brick("./data/sst-anomaly/meva_2013.nc", varname = "sstAnom")

sst_meva_2013 <- raster::mask(sst_meva_2013, atl_coast_hsc)

# FL to MS
flms_2013_nc <- nc_open("./data/sst-anomaly/flms_2013.nc")

sst_flms_2013 <- brick("./data/sst-anomaly/flms_2013.nc", varname = "sstAnom")

sst_flms_2013 <- raster::mask(sst_flms_2013, atl_coast_hsc)

# combine rasters
sst_2013 <- raster::merge(sst_ncga_2013, sst_meva_2013)
sst_2013 <- raster::merge(sst_2013, sst_flms_2013)

# calculate overall mean
sst_vals_2013 <- getValues(sst_2013)
sst_anomaly_2013 <- mean(sst_vals_2013, na.rm=TRUE)

# plot
sst_2013_anom_df <- as.data.frame(sst_2013, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2013)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2013_anom_df$x) - 1
xmax <- max(sst_2013_anom_df$x) + 1
ymin <- min(sst_2013_anom_df$y) - 1
ymax <- max(sst_2013_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2013_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2014 ----####

# NC to GA
ncga_2014_nc <- nc_open("./data/sst-anomaly/ncga_2014.nc")

sst_ncga_2014 <- raster("./data/sst-anomaly/ncga_2014.nc", varname = "sstAnom")

sst_ncga_2014 <- raster::mask(sst_ncga_2014, atl_coast_hsc)

# ME to GA
meva_2014_nc <- nc_open("./data/sst-anomaly/meva_2014.nc")

sst_meva_2014 <- brick("./data/sst-anomaly/meva_2014.nc", varname = "sstAnom")

sst_meva_2014 <- raster::mask(sst_meva_2014, atl_coast_hsc)

# FL to MS
flms_2014_nc <- nc_open("./data/sst-anomaly/flms_2014.nc")

sst_flms_2014 <- brick("./data/sst-anomaly/flms_2014.nc", varname = "sstAnom")

sst_flms_2014 <- raster::mask(sst_flms_2014, atl_coast_hsc)

# combine rasters
sst_2014 <- raster::merge(sst_ncga_2014, sst_meva_2014)
sst_2014 <- raster::merge(sst_2014, sst_flms_2014)

# calculate overall mean
sst_vals_2014 <- getValues(sst_2014)
sst_anomaly_2014 <- mean(sst_vals_2014, na.rm=TRUE)

# plot
sst_2014_anom_df <- as.data.frame(sst_2014, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2014)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2014_anom_df$x) - 1
xmax <- max(sst_2014_anom_df$x) + 1
ymin <- min(sst_2014_anom_df$y) - 1
ymax <- max(sst_2014_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2014_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2015 ----####

# NC to GA
ncga_2015_nc <- nc_open("./data/sst-anomaly/ncga_2015.nc")

sst_ncga_2015 <- raster("./data/sst-anomaly/ncga_2015.nc", varname = "sstAnom")

sst_ncga_2015 <- raster::mask(sst_ncga_2015, atl_coast_hsc)

# ME to GA
meva_2015_nc <- nc_open("./data/sst-anomaly/meva_2015.nc")

sst_meva_2015 <- brick("./data/sst-anomaly/meva_2015.nc", varname = "sstAnom")

sst_meva_2015 <- raster::mask(sst_meva_2015, atl_coast_hsc)

# FL to MS
flms_2015_nc <- nc_open("./data/sst-anomaly/flms_2015.nc")

sst_flms_2015 <- brick("./data/sst-anomaly/flms_2015.nc", varname = "sstAnom")

sst_flms_2015 <- raster::mask(sst_flms_2015, atl_coast_hsc)

# combine rasters
sst_2015 <- raster::merge(sst_ncga_2015, sst_meva_2015)
sst_2015 <- raster::merge(sst_2015, sst_flms_2015)

# calculate overall mean
sst_vals_2015 <- getValues(sst_2015)
sst_anomaly_2015 <- mean(sst_vals_2015, na.rm=TRUE)

# plot
sst_2015_anom_df <- as.data.frame(sst_2015, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2015)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2015_anom_df$x) - 1
xmax <- max(sst_2015_anom_df$x) + 1
ymin <- min(sst_2015_anom_df$y) - 1
ymax <- max(sst_2015_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2015_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2016 ----####

# NC to GA
ncga_2016_nc <- nc_open("./data/sst-anomaly/ncga_2016.nc")

sst_ncga_2016 <- raster("./data/sst-anomaly/ncga_2016.nc", varname = "sstAnom")

sst_ncga_2016 <- raster::mask(sst_ncga_2016, atl_coast_hsc)

# ME to GA
meva_2016_nc <- nc_open("./data/sst-anomaly/meva_2016.nc")

sst_meva_2016 <- brick("./data/sst-anomaly/meva_2016.nc", varname = "sstAnom")

sst_meva_2016 <- raster::mask(sst_meva_2016, atl_coast_hsc)

# FL to MS
flms_2016_nc <- nc_open("./data/sst-anomaly/flms_2016.nc")

sst_flms_2016 <- brick("./data/sst-anomaly/flms_2016.nc", varname = "sstAnom")

sst_flms_2016 <- raster::mask(sst_flms_2016, atl_coast_hsc)

# combine rasters
sst_2016 <- raster::merge(sst_ncga_2016, sst_meva_2016)
sst_2016 <- raster::merge(sst_2016, sst_flms_2016)

# calculate overall mean
sst_vals_2016 <- getValues(sst_2016)
sst_anomaly_2016 <- mean(sst_vals_2016, na.rm=TRUE)

# plot
sst_2016_anom_df <- as.data.frame(sst_2016, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2016)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2016_anom_df$x) - 1
xmax <- max(sst_2016_anom_df$x) + 1
ymin <- min(sst_2016_anom_df$y) - 1
ymax <- max(sst_2016_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2016_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2017 ----####

# NC to GA
ncga_2017_nc <- nc_open("./data/sst-anomaly/ncga_2017.nc")

sst_ncga_2017 <- raster("./data/sst-anomaly/ncga_2017.nc", varname = "sstAnom")

sst_ncga_2017 <- raster::mask(sst_ncga_2017, atl_coast_hsc)

# ME to GA
meva_2017_nc <- nc_open("./data/sst-anomaly/meva_2017.nc")

sst_meva_2017 <- brick("./data/sst-anomaly/meva_2017.nc", varname = "sstAnom")

sst_meva_2017 <- raster::mask(sst_meva_2017, atl_coast_hsc)

# FL to MS
flms_2017_nc <- nc_open("./data/sst-anomaly/flms_2017.nc")

sst_flms_2017 <- brick("./data/sst-anomaly/flms_2017.nc", varname = "sstAnom")

sst_flms_2017 <- raster::mask(sst_flms_2017, atl_coast_hsc)

# combine rasters
sst_2017 <- raster::merge(sst_ncga_2017, sst_meva_2017)
sst_2017 <- raster::merge(sst_2017, sst_flms_2017)

# calculate overall mean
sst_vals_2017 <- getValues(sst_2017)
sst_anomaly_2017 <- mean(sst_vals_2017, na.rm=TRUE)

# plot
sst_2017_anom_df <- as.data.frame(sst_2017, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2017)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2017_anom_df$x) - 1
xmax <- max(sst_2017_anom_df$x) + 1
ymin <- min(sst_2017_anom_df$y) - 1
ymax <- max(sst_2017_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2017_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2018 ----####

# NC to GA
ncga_2018_nc <- nc_open("./data/sst-anomaly/ncga_2018.nc")

sst_ncga_2018 <- raster("./data/sst-anomaly/ncga_2018.nc", varname = "sstAnom")

sst_ncga_2018 <- raster::mask(sst_ncga_2018, atl_coast_hsc)

# ME to GA
meva_2018_nc <- nc_open("./data/sst-anomaly/meva_2018.nc")

sst_meva_2018 <- brick("./data/sst-anomaly/meva_2018.nc", varname = "sstAnom")

sst_meva_2018 <- raster::mask(sst_meva_2018, atl_coast_hsc)

# FL to MS
flms_2018_nc <- nc_open("./data/sst-anomaly/flms_2018.nc")

sst_flms_2018 <- brick("./data/sst-anomaly/flms_2018.nc", varname = "sstAnom")

sst_flms_2018 <- raster::mask(sst_flms_2018, atl_coast_hsc)

# combine rasters
sst_2018 <- raster::merge(sst_ncga_2018, sst_meva_2018)
sst_2018 <- raster::merge(sst_2018, sst_flms_2018)

# calculate overall mean
sst_vals_2018 <- getValues(sst_2018)
sst_anomaly_2018 <- mean(sst_vals_2018, na.rm=TRUE)

# plot
sst_2018_anom_df <- as.data.frame(sst_2018, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2018)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2018_anom_df$x) - 1
xmax <- max(sst_2018_anom_df$x) + 1
ymin <- min(sst_2018_anom_df$y) - 1
ymax <- max(sst_2018_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2018_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2019 ----####

# NC to GA
ncga_2019_nc <- nc_open("./data/sst-anomaly/ncga_2019.nc")

sst_ncga_2019 <- raster("./data/sst-anomaly/ncga_2019.nc", varname = "sstAnom")

sst_ncga_2019 <- raster::mask(sst_ncga_2019, atl_coast_hsc)

# ME to GA
meva_2019_nc <- nc_open("./data/sst-anomaly/meva_2019.nc")

sst_meva_2019 <- brick("./data/sst-anomaly/meva_2019.nc", varname = "sstAnom")

sst_meva_2019 <- raster::mask(sst_meva_2019, atl_coast_hsc)

# FL to MS
flms_2019_nc <- nc_open("./data/sst-anomaly/flms_2019.nc")

sst_flms_2019 <- brick("./data/sst-anomaly/flms_2019.nc", varname = "sstAnom")

sst_flms_2019 <- raster::mask(sst_flms_2019, atl_coast_hsc)

# combine rasters
sst_2019 <- raster::merge(sst_ncga_2019, sst_meva_2019)
sst_2019 <- raster::merge(sst_2019, sst_flms_2019)

# calculate overall mean
sst_vals_2019 <- getValues(sst_2019)
sst_anomaly_2019 <- mean(sst_vals_2019, na.rm=TRUE)

# plot
sst_2019_anom_df <- as.data.frame(sst_2019, xy = TRUE) %>% 
  rename(sst_anom = layer) %>% 
  filter(!is.na(sst_anom)) %>% 
  mutate(year = 2019)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2019_anom_df$x) - 1
xmax <- max(sst_2019_anom_df$x) + 1
ymin <- min(sst_2019_anom_df$y) - 1
ymax <- max(sst_2019_anom_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  geom_tile(data = sst_2019_anom_df, aes(x = x, y = y, fill = sst_anom)) +
  scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- all years ----####

# create dataframe of annual means

Year <- c(2010:2019)
mean_sst_anomaly <- c(sst_anomaly_2010, sst_anomaly_2011, sst_anomaly_2012,
              sst_anomaly_2013, sst_anomaly_2014, sst_anomaly_2015, sst_anomaly_2016,
              sst_anomaly_2017, sst_anomaly_2018, sst_anomaly_2019)

sst_anom_cov <- as.data.frame(cbind(Year, mean_sst_anomaly))

sst_anom_cov <- sst_anom_cov %>% 
  mutate(Period = 1:10)

# combine raster dataframes
sst_anom_df <- bind_rows(sst_2010_anom_df, sst_2011_anom_df, sst_2012_anom_df,
                         sst_2013_anom_df, sst_2014_anom_df, sst_2015_anom_df,
                         sst_2016_anom_df, sst_2017_anom_df, sst_2018_anom_df,
                         sst_2019_anom_df)

# plot sst
theme_set(theme_minimal())

xmin <- min(sst_anom_df$x) - 3
xmax <- max(sst_anom_df$x) + 3
ymin <- min(sst_anom_df$y) - 3
ymax <- max(sst_anom_df$y) + 3

png(filename = "./figures/sst-anom-map.png", height = 4, width = 8,
    units = "in", res = 600)

print(ggplot(data = sst_anom_df) +
        geom_sf(data = world, colour = NA) +
        geom_sf(data = lakes, fill = "white", colour = NA) +
        geom_tile(data = sst_anom_df, aes(x = x, y = y, fill = sst_anom)) +
        coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
                 expand = FALSE) +
        facet_wrap(. ~ year, ncol = 5) +
        scale_fill_viridis(option = "turbo", name = "sea surface\ntemperature\nanomaly") +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
              strip.background = element_rect(fill = "transparent")))

dev.off()
