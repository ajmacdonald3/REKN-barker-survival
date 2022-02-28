####################### PREPARE SURVIVAL COVARIATES ############################

library(tidyverse) 
library(ncdf4) 
library(raster) 
library(sf) 
library(rnaturalearth)
library(rerddap)
library(viridis)

########################### SNOW COVER COVARIATE ###############################

# load shapefile of preferred rufa REKN breeding habitat
shp <- st_read("./data/Preferred_Habitat_and_Range_Intersect.shp")


####---- 2009 ----####

# load data in netCDF format

nc_2009 <- nc_open("./data/snowc.2009.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2009 <- brick("./data/snowc.2009.nc", varname="snowc")

# subset to key nest initiation period
brick_2009 <- subset(brick_2009, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(proj4string(brick_2009))

# keep only cells inside rufa range
#brick_2009_rufa <- crop(brick_2009, extent(rufa_range))
brick_2009_rufa <- raster::mask(brick_2009, rufa_range)

# calculate mean value per cell for june 15-30
mean_2009_rufa <- calc(brick_2009_rufa, fun = mean, na.rm = TRUE)

#mean_2009_rufa <- crop(x = mean_2009_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2009 <- getValues(mean_2009_rufa)
mean_snowc_2009 <- mean(vals_2009, na.rm=TRUE)

# plot
mean_2009_rufa_df <- as.data.frame(mean_2009_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2009)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2009_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2010 ----####

# load data in netCDF format

nc_2010 <- nc_open("./data/snowc.2010.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2010 <- brick("./data/snowc.2010.nc", varname="snowc")

# subset to key nest initiation period
brick_2010 <- subset(brick_2010, 166:181) # june 15-30

# keep only cells inside rufa range
brick_2010_rufa <- raster::mask(brick_2010, rufa_range)

# calculate mean value per cell for june 15-30
mean_2010_rufa <- calc(brick_2010_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2010 <- getValues(mean_2010_rufa)
mean_snowc_2010 <- mean(vals_2010, na.rm=TRUE)

# plot
mean_2010_rufa_df <- as.data.frame(mean_2010_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2010)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2010_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2011 ----####

# load data in netCDF format

nc_2011 <- nc_open("./data/snowc.2011.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2011 <- brick("./data/snowc.2011.nc", varname="snowc")

# subset to key nest initiation period
brick_2011 <- subset(brick_2011, 166:181) # june 15-30

# keep only cells inside rufa range
brick_2011_rufa <- raster::mask(brick_2011, rufa_range)

# calculate mean value per cell for june 15-30
mean_2011_rufa <- calc(brick_2011_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2011 <- getValues(mean_2011_rufa)
mean_snowc_2011 <- mean(vals_2011, na.rm=TRUE)

# plot
mean_2011_rufa_df <- as.data.frame(mean_2011_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2011)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2011_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2012 ----####

# load data in netCDF format

nc_2012 <- nc_open("./data/snowc.2012.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2012 <- brick("./data/snowc.2012.nc", varname="snowc")

# subset to key nest initiation period
brick_2012 <- subset(brick_2012, 167:182) # june 15-30

# keep only cells inside rufa range
brick_2012_rufa <- raster::mask(brick_2012, rufa_range)

# calculate mean value per cell for june 15-30
mean_2012_rufa <- calc(brick_2012_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2012 <- getValues(mean_2012_rufa)
mean_snowc_2012 <- mean(vals_2012, na.rm=TRUE)

# plot
mean_2012_rufa_df <- as.data.frame(mean_2012_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2012)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2012_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2013 ----####

# load data in netCDF format

nc_2013 <- nc_open("./data/snowc.2013.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2013 <- brick("./data/snowc.2013.nc", varname="snowc")

# subset to key nest initiation period
brick_2013 <- subset(brick_2013, 166:181) # june 15-30

# keep only cells inside rufa range
brick_2013_rufa <- raster::mask(brick_2013, rufa_range)

# calculate mean value per cell for june 15-30
mean_2013_rufa <- calc(brick_2013_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2013 <- getValues(mean_2013_rufa)
mean_snowc_2013 <- mean(vals_2013, na.rm=TRUE)

# plot
mean_2013_rufa_df <- as.data.frame(mean_2013_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2013)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2013_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2014 ----####

# load data in netCDF format

nc_2014 <- nc_open("./data/snowc.2014.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2014 <- brick("./data/snowc.2014.nc", varname="snowc")

# subset to key nest initiation period
brick_2014 <- subset(brick_2014, 166:181) # june 15-30

# keep only cells inside rufa range
brick_2014_rufa <- raster::mask(brick_2014, rufa_range)

# calculate mean value per cell for june 15-30
mean_2014_rufa <- calc(brick_2014_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2014 <- getValues(mean_2014_rufa)
mean_snowc_2014 <- mean(vals_2014, na.rm=TRUE)

# plot
mean_2014_rufa_df <- as.data.frame(mean_2014_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2014)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2014_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2015 ----####

# load data in netCDF format

nc_2015 <- nc_open("./data/snowc.2015.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2015 <- brick("./data/snowc.2015.nc", varname="snowc")

# subset to key nest initiation period
brick_2015 <- subset(brick_2015, 166:181) # june 15-30

# keep only cells inside rufa range
brick_2015_rufa <- raster::mask(brick_2015, rufa_range)

# calculate mean value per cell for june 15-30
mean_2015_rufa <- calc(brick_2015_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2015 <- getValues(mean_2015_rufa)
mean_snowc_2015 <- mean(vals_2015, na.rm=TRUE)

# plot
mean_2015_rufa_df <- as.data.frame(mean_2015_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2015)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2015_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2016 ----####

# load data in netCDF format

nc_2016 <- nc_open("./data/snowc.2016.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2016 <- brick("./data/snowc.2016.nc", varname="snowc")

# subset to key nest initiation period
brick_2016 <- subset(brick_2016, 167:182) # june 15-30

# keep only cells inside rufa range
brick_2016_rufa <- raster::mask(brick_2016, rufa_range)

# calculate mean value per cell for june 15-30
mean_2016_rufa <- calc(brick_2016_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2016 <- getValues(mean_2016_rufa)
mean_snowc_2016 <- mean(vals_2016, na.rm=TRUE)

# plot
mean_2016_rufa_df <- as.data.frame(mean_2016_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2016)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2016_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2017 ----####

# load data in netCDF format

nc_2017 <- nc_open("./data/snowc.2017.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2017 <- brick("./data/snowc.2017.nc", varname="snowc")

# subset to key nest initiation period
brick_2017 <- subset(brick_2017, 166:181) # june 15-30

# keep only cells inside rufa range
brick_2017_rufa <- raster::mask(brick_2017, rufa_range)

# calculate mean value per cell for june 15-30
mean_2017_rufa <- calc(brick_2017_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2017 <- getValues(mean_2017_rufa)
mean_snowc_2017 <- mean(vals_2017, na.rm=TRUE)

# plot
mean_2017_rufa_df <- as.data.frame(mean_2017_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2017)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2017_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2018 ----####

# load data in netCDF format

nc_2018 <- nc_open("./data/snowc.2018.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2018 <- brick("./data/snowc.2018.nc", varname="snowc")

# subset to key nest initiation period
brick_2018 <- subset(brick_2018, 166:181) # june 15-30

# keep only cells inside rufa range
brick_2018_rufa <- raster::mask(brick_2018, rufa_range)

# calculate mean value per cell for june 15-30
mean_2018_rufa <- calc(brick_2018_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2018 <- getValues(mean_2018_rufa)
mean_snowc_2018 <- mean(vals_2018, na.rm=TRUE)

# plot
mean_2018_rufa_df <- as.data.frame(mean_2018_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2018)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2018_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2019 ----####

# load data in netCDF format

nc_2019 <- nc_open("./data/snowc.2019.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2019 <- brick("./data/snowc.2019.nc", varname="snowc")

# subset to key nest initiation period
brick_2019 <- subset(brick_2019, 166:181) # june 15-30

# keep only cells inside rufa range
brick_2019_rufa <- raster::mask(brick_2019, rufa_range)

# calculate mean value per cell for june 15-30
mean_2019_rufa <- calc(brick_2019_rufa, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2019 <- getValues(mean_2019_rufa)
mean_snowc_2019 <- mean(vals_2019, na.rm=TRUE)

# plot
mean_2019_rufa_df <- as.data.frame(mean_2019_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2019)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2019_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- all years ----####

# create dataframe of annual means

Year <- c(2009:2019)
mean_snowc <- c(mean_snowc_2009, mean_snowc_2010, mean_snowc_2011, mean_snowc_2012,
                mean_snowc_2013, mean_snowc_2014, mean_snowc_2015, mean_snowc_2016,
                mean_snowc_2017, mean_snowc_2018, mean_snowc_2019)

arctic_snow_cov <- as.data.frame(cbind(Year, mean_snowc))

arctic_snow_cov <- arctic_snow_cov %>% 
  filter(!Year == 2009) %>% 
  mutate(Period = 1:10)

# combine raster dataframes
mean_rufa_df <- bind_rows(mean_2010_rufa_df, mean_2011_rufa_df, mean_2012_rufa_df,
                          mean_2013_rufa_df, mean_2014_rufa_df, mean_2015_rufa_df,
                          mean_2016_rufa_df, mean_2017_rufa_df, mean_2018_rufa_df,
                          mean_2019_rufa_df)

# plot rasters
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

world <- world %>% 
  st_transform(st_crs(rufa_range))

lakes <- lakes %>% 
  st_transform(st_crs(rufa_range))

rufa_range2 <- st_simplify(rufa_range, dTolerance = 1000, preserveTopology = TRUE)

# plot snow cover
theme_set(theme_minimal())

xmin <- min(mean_rufa_df$x) - 100000
xmax <- max(mean_rufa_df$x) + 100000
ymin <- min(mean_rufa_df$y) - 100000
ymax <- max(mean_rufa_df$y) + 100000

png(filename = "./figures/snow-cover-map.png", height = 8, width = 8,
    units = "in", res = 600)

print(ggplot(data = mean_rufa_df) +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  #geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_rufa_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = rufa_range2, fill = NA, colour = "black", size = 0.2) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  facet_wrap(. ~ year, ncol = 3) +
  scale_fill_gradientn(colors = hcl.colors(9, "Greens"), name = "snow cover") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent")))

dev.off()

########################## SEA SURFACE TEMPERATURE #############################

# download GHRSST MUR v4.1 data (1-km resolution) as netCDF files
# only need to do this once

sstInfo <- info('jplMURSST41')

# NC to GA
(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2010-05-01", "2010-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2011-05-01", "2011-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2012-05-01", "2012-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2013-05-01", "2013-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2014-05-01", "2014-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2015-05-01", "2015-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2016-05-01", "2016-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2017-05-01", "2017-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2018-05-01", "2018-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(30.8, 36.6), longitude = c(-81.7, -75.5),
                   time = c("2019-05-01", "2019-05-31"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

# ME to VA
(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2010-05-15", "2010-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2011-05-15", "2011-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2012-05-15", "2012-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2013-05-15", "2013-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2014-05-15", "2014-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2015-05-15", "2015-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2016-05-15", "2016-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2017-05-15", "2017-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2018-05-15", "2018-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(36.6, 44.9), longitude = c(-77.4, -66.9),
                   time = c("2019-05-15", "2019-06-15"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

# FL to MS
(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2010-04-01", "2010-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2011-04-01", "2011-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2012-04-01", "2012-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2013-04-01", "2013-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2014-04-01", "2014-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2015-04-01", "2015-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2016-04-01", "2016-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2017-04-01", "2017-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2018-04-01", "2018-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

(murSST <- griddap(sstInfo, latitude = c(24.3, 30.8), longitude = c(-89.2, -79.7),
                   time = c("2019-04-01", "2019-04-30"), fields = 'analysed_sst',
                   store = disk(path = "./data/sst/griddap/", overwrite = TRUE)))

rm(sstInfo, murSST)

# generate 3 buffer around US Atlantic coast

# load 30km buffer shapefile
coast_30km <- st_read("./map-data/atl_coast_30km.shp")

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

####---- 2010 ----####

# NC to GA
ncga_2010_nc <- nc_open("./data/sst/griddap/ncga_2010.nc")

sst_ncga_2010_brick <- brick("./data/sst/griddap/ncga_2010.nc", varname = "analysed_sst")

atl_coast_hsc <- atl_coast_buf %>% 
  st_transform(st_crs(sst_ncga_2010_brick)) %>% 
  st_as_sf()

sst_ncga_2010_brick <- raster::mask(sst_ncga_2010_brick, atl_coast_hsc)

sst_ncga_2010_mean <- calc(sst_ncga_2010_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2010_nc <- nc_open("./data/sst/griddap/meva_2010.nc")

sst_meva_2010_brick <- brick("./data/sst/griddap/meva_2010.nc", varname = "analysed_sst")

sst_meva_2010_brick <- raster::mask(sst_meva_2010_brick, atl_coast_hsc)

sst_meva_2010_mean <- calc(sst_meva_2010_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2010_nc <- nc_open("./data/sst/griddap/flms_2010.nc")

sst_flms_2010_brick <- brick("./data/sst/griddap/flms_2010.nc", varname = "analysed_sst")

sst_flms_2010_brick <- raster::mask(sst_flms_2010_brick, atl_coast_hsc)

sst_flms_2010_mean <- calc(sst_flms_2010_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2010_mean <- raster::merge(sst_ncga_2010_mean, sst_meva_2010_mean)
sst_2010_mean <- raster::merge(sst_2010_mean, sst_flms_2010_mean)

# calculate overall mean
sst_vals_2010 <- getValues(sst_2010_mean)
sst_overall_2010 <- mean(sst_vals_2010, na.rm=TRUE)

# plot
sst_2010_mean_df <- as.data.frame(sst_2010_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2010)

world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

theme_set(theme_minimal())

xmin <- min(sst_2010_mean_df$x) - 1
xmax <- max(sst_2010_mean_df$x) + 1
ymin <- min(sst_2010_mean_df$y) - 1
ymax <- max(sst_2010_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2010_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2011 ----####

# NC to GA
ncga_2011_nc <- nc_open("./data/sst/griddap/ncga_2011.nc")

sst_ncga_2011_brick <- brick("./data/sst/griddap/ncga_2011.nc", varname = "analysed_sst")

sst_ncga_2011_brick <- raster::mask(sst_ncga_2011_brick, atl_coast_hsc)

sst_ncga_2011_mean <- calc(sst_ncga_2011_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2011_nc <- nc_open("./data/sst/griddap/meva_2011.nc")

sst_meva_2011_brick <- brick("./data/sst/griddap/meva_2011.nc", varname = "analysed_sst")

sst_meva_2011_brick <- raster::mask(sst_meva_2011_brick, atl_coast_hsc)

sst_meva_2011_mean <- calc(sst_meva_2011_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2011_nc <- nc_open("./data/sst/griddap/flms_2011.nc")

sst_flms_2011_brick <- brick("./data/sst/griddap/flms_2011.nc", varname = "analysed_sst")

sst_flms_2011_brick <- raster::mask(sst_flms_2011_brick, atl_coast_hsc)

sst_flms_2011_mean <- calc(sst_flms_2011_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2011_mean <- raster::merge(sst_ncga_2011_mean, sst_meva_2011_mean)
sst_2011_mean <- raster::merge(sst_2011_mean, sst_flms_2011_mean)

# calculate overall mean
sst_vals_2011 <- getValues(sst_2011_mean)
sst_overall_2011 <- mean(sst_vals_2011, na.rm=TRUE)

# plot
sst_2011_mean_df <- as.data.frame(sst_2011_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2011)

xmin <- min(sst_2011_mean_df$x) - 1
xmax <- max(sst_2011_mean_df$x) + 1
ymin <- min(sst_2011_mean_df$y) - 1
ymax <- max(sst_2011_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2011_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2012 ----####

# NC to GA
ncga_2012_nc <- nc_open("./data/sst/griddap/ncga_2012.nc")

sst_ncga_2012_brick <- brick("./data/sst/griddap/ncga_2012.nc", varname = "analysed_sst")

sst_ncga_2012_brick <- raster::mask(sst_ncga_2012_brick, atl_coast_hsc)

sst_ncga_2012_mean <- calc(sst_ncga_2012_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2012_nc <- nc_open("./data/sst/griddap/meva_2012.nc")

sst_meva_2012_brick <- brick("./data/sst/griddap/meva_2012.nc", varname = "analysed_sst")

sst_meva_2012_brick <- raster::mask(sst_meva_2012_brick, atl_coast_hsc)

sst_meva_2012_mean <- calc(sst_meva_2012_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2012_nc <- nc_open("./data/sst/griddap/flms_2012.nc")

sst_flms_2012_brick <- brick("./data/sst/griddap/flms_2012.nc", varname = "analysed_sst")

sst_flms_2012_brick <- raster::mask(sst_flms_2012_brick, atl_coast_hsc)

sst_flms_2012_mean <- calc(sst_flms_2012_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2012_mean <- raster::merge(sst_ncga_2012_mean, sst_meva_2012_mean)
sst_2012_mean <- raster::merge(sst_2012_mean, sst_flms_2012_mean)

# calculate overall mean
sst_vals_2012 <- getValues(sst_2012_mean)
sst_overall_2012 <- mean(sst_vals_2012, na.rm=TRUE)

# plot
sst_2012_mean_df <- as.data.frame(sst_2012_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2012)

xmin <- min(sst_2012_mean_df$x) - 1
xmax <- max(sst_2012_mean_df$x) + 1
ymin <- min(sst_2012_mean_df$y) - 1
ymax <- max(sst_2012_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2012_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2013 ----####

# NC to GA
ncga_2013_nc <- nc_open("./data/sst/griddap/ncga_2013.nc")

sst_ncga_2013_brick <- brick("./data/sst/griddap/ncga_2013.nc", varname = "analysed_sst")

sst_ncga_2013_brick <- raster::mask(sst_ncga_2013_brick, atl_coast_hsc)

sst_ncga_2013_mean <- calc(sst_ncga_2013_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2013_nc <- nc_open("./data/sst/griddap/meva_2013.nc")

sst_meva_2013_brick <- brick("./data/sst/griddap/meva_2013.nc", varname = "analysed_sst")

sst_meva_2013_brick <- raster::mask(sst_meva_2013_brick, atl_coast_hsc)

sst_meva_2013_mean <- calc(sst_meva_2013_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2013_nc <- nc_open("./data/sst/griddap/flms_2013.nc")

sst_flms_2013_brick <- brick("./data/sst/griddap/flms_2013.nc", varname = "analysed_sst")

sst_flms_2013_brick <- raster::mask(sst_flms_2013_brick, atl_coast_hsc)

sst_flms_2013_mean <- calc(sst_flms_2013_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2013_mean <- raster::merge(sst_ncga_2013_mean, sst_meva_2013_mean)
sst_2013_mean <- raster::merge(sst_2013_mean, sst_flms_2013_mean)

# calculate overall mean
sst_vals_2013 <- getValues(sst_2013_mean)
sst_overall_2013 <- mean(sst_vals_2013, na.rm=TRUE)

# plot
sst_2013_mean_df <- as.data.frame(sst_2013_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2013)

xmin <- min(sst_2013_mean_df$x) - 1
xmax <- max(sst_2013_mean_df$x) + 1
ymin <- min(sst_2013_mean_df$y) - 1
ymax <- max(sst_2013_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2013_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2014 ----####

# NC to GA
ncga_2014_nc <- nc_open("./data/sst/griddap/ncga_2014.nc")

sst_ncga_2014_brick <- brick("./data/sst/griddap/ncga_2014.nc", varname = "analysed_sst")

sst_ncga_2014_brick <- raster::mask(sst_ncga_2014_brick, atl_coast_hsc)

sst_ncga_2014_mean <- calc(sst_ncga_2014_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2014_nc <- nc_open("./data/sst/griddap/meva_2014.nc")

sst_meva_2014_brick <- brick("./data/sst/griddap/meva_2014.nc", varname = "analysed_sst")

sst_meva_2014_brick <- raster::mask(sst_meva_2014_brick, atl_coast_hsc)

sst_meva_2014_mean <- calc(sst_meva_2014_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2014_nc <- nc_open("./data/sst/griddap/flms_2014.nc")

sst_flms_2014_brick <- brick("./data/sst/griddap/flms_2014.nc", varname = "analysed_sst")

sst_flms_2014_brick <- raster::mask(sst_flms_2014_brick, atl_coast_hsc)

sst_flms_2014_mean <- calc(sst_flms_2014_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2014_mean <- raster::merge(sst_ncga_2014_mean, sst_meva_2014_mean)
sst_2014_mean <- raster::merge(sst_2014_mean, sst_flms_2014_mean)

# calculate overall mean
sst_vals_2014 <- getValues(sst_2014_mean)
sst_overall_2014 <- mean(sst_vals_2014, na.rm=TRUE)

# plot
sst_2014_mean_df <- as.data.frame(sst_2014_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2014)

xmin <- min(sst_2014_mean_df$x) - 1
xmax <- max(sst_2014_mean_df$x) + 1
ymin <- min(sst_2014_mean_df$y) - 1
ymax <- max(sst_2014_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2014_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2015 ----####

# NC to GA
ncga_2015_nc <- nc_open("./data/sst/griddap/ncga_2015.nc")

sst_ncga_2015_brick <- brick("./data/sst/griddap/ncga_2015.nc", varname = "analysed_sst")

sst_ncga_2015_brick <- raster::mask(sst_ncga_2015_brick, atl_coast_hsc)

sst_ncga_2015_mean <- calc(sst_ncga_2015_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2015_nc <- nc_open("./data/sst/griddap/meva_2015.nc")

sst_meva_2015_brick <- brick("./data/sst/griddap/meva_2015.nc", varname = "analysed_sst")

sst_meva_2015_brick <- raster::mask(sst_meva_2015_brick, atl_coast_hsc)

sst_meva_2015_mean <- calc(sst_meva_2015_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2015_nc <- nc_open("./data/sst/griddap/flms_2015.nc")

sst_flms_2015_brick <- brick("./data/sst/griddap/flms_2015.nc", varname = "analysed_sst")

sst_flms_2015_brick <- raster::mask(sst_flms_2015_brick, atl_coast_hsc)

sst_flms_2015_mean <- calc(sst_flms_2015_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2015_mean <- raster::merge(sst_ncga_2015_mean, sst_meva_2015_mean)
sst_2015_mean <- raster::merge(sst_2015_mean, sst_flms_2015_mean)

# calculate overall mean
sst_vals_2015 <- getValues(sst_2015_mean)
sst_overall_2015 <- mean(sst_vals_2015, na.rm=TRUE)

# plot
sst_2015_mean_df <- as.data.frame(sst_2015_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2015)

xmin <- min(sst_2015_mean_df$x) - 1
xmax <- max(sst_2015_mean_df$x) + 1
ymin <- min(sst_2015_mean_df$y) - 1
ymax <- max(sst_2015_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2015_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2016 ----####

# NC to GA
ncga_2016_nc <- nc_open("./data/sst/griddap/ncga_2016.nc")

sst_ncga_2016_brick <- brick("./data/sst/griddap/ncga_2016.nc", varname = "analysed_sst")

sst_ncga_2016_brick <- raster::mask(sst_ncga_2016_brick, atl_coast_hsc)

sst_ncga_2016_mean <- calc(sst_ncga_2016_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2016_nc <- nc_open("./data/sst/griddap/meva_2016.nc")

sst_meva_2016_brick <- brick("./data/sst/griddap/meva_2016.nc", varname = "analysed_sst")

sst_meva_2016_brick <- raster::mask(sst_meva_2016_brick, atl_coast_hsc)

sst_meva_2016_mean <- calc(sst_meva_2016_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2016_nc <- nc_open("./data/sst/griddap/flms_2016.nc")

sst_flms_2016_brick <- brick("./data/sst/griddap/flms_2016.nc", varname = "analysed_sst")

sst_flms_2016_brick <- raster::mask(sst_flms_2016_brick, atl_coast_hsc)

sst_flms_2016_mean <- calc(sst_flms_2016_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2016_mean <- raster::merge(sst_ncga_2016_mean, sst_meva_2016_mean)
sst_2016_mean <- raster::merge(sst_2016_mean, sst_flms_2016_mean)

# calculate overall mean
sst_vals_2016 <- getValues(sst_2016_mean)
sst_overall_2016 <- mean(sst_vals_2016, na.rm=TRUE)

# plot
sst_2016_mean_df <- as.data.frame(sst_2016_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2016)

xmin <- min(sst_2016_mean_df$x) - 1
xmax <- max(sst_2016_mean_df$x) + 1
ymin <- min(sst_2016_mean_df$y) - 1
ymax <- max(sst_2016_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2016_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2017 ----####

# NC to GA
ncga_2017_nc <- nc_open("./data/sst/griddap/ncga_2017.nc")

sst_ncga_2017_brick <- brick("./data/sst/griddap/ncga_2017.nc", varname = "analysed_sst")

sst_ncga_2017_brick <- raster::mask(sst_ncga_2017_brick, atl_coast_hsc)

sst_ncga_2017_mean <- calc(sst_ncga_2017_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2017_nc <- nc_open("./data/sst/griddap/meva_2017.nc")

sst_meva_2017_brick <- brick("./data/sst/griddap/meva_2017.nc", varname = "analysed_sst")

sst_meva_2017_brick <- raster::mask(sst_meva_2017_brick, atl_coast_hsc)

sst_meva_2017_mean <- calc(sst_meva_2017_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2017_nc <- nc_open("./data/sst/griddap/flms_2017.nc")

sst_flms_2017_brick <- brick("./data/sst/griddap/flms_2017.nc", varname = "analysed_sst")

sst_flms_2017_brick <- raster::mask(sst_flms_2017_brick, atl_coast_hsc)

sst_flms_2017_mean <- calc(sst_flms_2017_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2017_mean <- raster::merge(sst_ncga_2017_mean, sst_meva_2017_mean)
sst_2017_mean <- raster::merge(sst_2017_mean, sst_flms_2017_mean)

# calculate overall mean
sst_vals_2017 <- getValues(sst_2017_mean)
sst_overall_2017 <- mean(sst_vals_2017, na.rm=TRUE)

# plot
sst_2017_mean_df <- as.data.frame(sst_2017_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2017)

xmin <- min(sst_2017_mean_df$x) - 1
xmax <- max(sst_2017_mean_df$x) + 1
ymin <- min(sst_2017_mean_df$y) - 1
ymax <- max(sst_2017_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2017_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2018 ----####

# NC to GA
ncga_2018_nc <- nc_open("./data/sst/griddap/ncga_2018.nc")

sst_ncga_2018_brick <- brick("./data/sst/griddap/ncga_2018.nc", varname = "analysed_sst")

sst_ncga_2018_brick <- raster::mask(sst_ncga_2018_brick, atl_coast_hsc)

sst_ncga_2018_mean <- calc(sst_ncga_2018_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2018_nc <- nc_open("./data/sst/griddap/meva_2018.nc")

sst_meva_2018_brick <- brick("./data/sst/griddap/meva_2018.nc", varname = "analysed_sst")

sst_meva_2018_brick <- raster::mask(sst_meva_2018_brick, atl_coast_hsc)

sst_meva_2018_mean <- calc(sst_meva_2018_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2018_nc <- nc_open("./data/sst/griddap/flms_2018.nc")

sst_flms_2018_brick <- brick("./data/sst/griddap/flms_2018.nc", varname = "analysed_sst")

sst_flms_2018_brick <- raster::mask(sst_flms_2018_brick, atl_coast_hsc)

sst_flms_2018_mean <- calc(sst_flms_2018_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2018_mean <- raster::merge(sst_ncga_2018_mean, sst_meva_2018_mean)
sst_2018_mean <- raster::merge(sst_2018_mean, sst_flms_2018_mean)

# calculate overall mean
sst_vals_2018 <- getValues(sst_2018_mean)
sst_overall_2018 <- mean(sst_vals_2018, na.rm=TRUE)

# plot
sst_2018_mean_df <- as.data.frame(sst_2018_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2018)

xmin <- min(sst_2018_mean_df$x) - 1
xmax <- max(sst_2018_mean_df$x) + 1
ymin <- min(sst_2018_mean_df$y) - 1
ymax <- max(sst_2018_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2018_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
           expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        strip.background = element_rect(fill = "transparent"))

####---- 2019 ----####

# NC to GA
ncga_2019_nc <- nc_open("./data/sst/griddap/ncga_2019.nc")

sst_ncga_2019_brick <- brick("./data/sst/griddap/ncga_2019.nc", varname = "analysed_sst")

sst_ncga_2019_brick <- raster::mask(sst_ncga_2019_brick, atl_coast_hsc)

sst_ncga_2019_mean <- calc(sst_ncga_2019_brick, fun = mean, na.rm = TRUE)

# ME to GA
meva_2019_nc <- nc_open("./data/sst/griddap/meva_2019.nc")

sst_meva_2019_brick <- brick("./data/sst/griddap/meva_2019.nc", varname = "analysed_sst")

sst_meva_2019_brick <- raster::mask(sst_meva_2019_brick, atl_coast_hsc)

sst_meva_2019_mean <- calc(sst_meva_2019_brick, fun = mean, na.rm = TRUE)

# FL to MS
flms_2019_nc <- nc_open("./data/sst/griddap/flms_2019.nc")

sst_flms_2019_brick <- brick("./data/sst/griddap/flms_2019.nc", varname = "analysed_sst")

sst_flms_2019_brick <- raster::mask(sst_flms_2019_brick, atl_coast_hsc)

sst_flms_2019_mean <- calc(sst_flms_2019_brick, fun = mean, na.rm = TRUE)

# combine rasters
sst_2019_mean <- raster::merge(sst_ncga_2019_mean, sst_meva_2019_mean)
sst_2019_mean <- raster::merge(sst_2019_mean, sst_flms_2019_mean)

# calculate overall mean
sst_vals_2019 <- getValues(sst_2019_mean)
sst_overall_2019 <- mean(sst_vals_2019, na.rm=TRUE)

# plot
sst_2019_mean_df <- as.data.frame(sst_2019_mean, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2019)

xmin <- min(sst_2019_mean_df$x) - 1
xmax <- max(sst_2019_mean_df$x) + 1
ymin <- min(sst_2019_mean_df$y) - 1
ymax <- max(sst_2019_mean_df$y) + 1

ggplot() +
  geom_sf(data = world, colour = NA) +
  geom_sf(data = lakes, colour = NA, fill = "white") +
  #geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = sst_2019_mean_df, aes(x = x, y = y, fill = layer)) +
  #scale_fill_gradientn(colours = mycolor, na.value = NA) +
  scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
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
mean_sst <- c(sst_overall_2010, sst_overall_2011, sst_overall_2012,
              sst_overall_2013, sst_overall_2014, sst_overall_2015, sst_overall_2016,
              sst_overall_2017, sst_overall_2018, sst_overall_2019)

sst_cov <- as.data.frame(cbind(Year, mean_sst))

sst_cov <- sst_cov %>% 
  mutate(Period = 1:10)

# combine raster dataframes
sst_mean_df <- bind_rows(sst_2010_mean_df, sst_2011_mean_df, sst_2012_mean_df,
                         sst_2013_mean_df, sst_2014_mean_df, sst_2015_mean_df,
                         sst_2016_mean_df, sst_2017_mean_df, sst_2018_mean_df,
                         sst_2019_mean_df)

# plot sst
theme_set(theme_minimal())

xmin <- min(sst_mean_df$x) - 3
xmax <- max(sst_mean_df$x) + 3
ymin <- min(sst_mean_df$y) - 3
ymax <- max(sst_mean_df$y) + 3

png(filename = "./figures/sst-map.png", height = 4, width = 8,
    units = "in", res = 600)

print(ggplot(data = sst_mean_df) +
        geom_sf(data = world, colour = NA) +
        geom_sf(data = lakes, fill = "white", colour = NA) +
        geom_tile(data = sst_mean_df, aes(x = x, y = y, fill = layer)) +
        coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
                 expand = FALSE) +
        facet_wrap(. ~ year, ncol = 5) +
        scale_fill_viridis(option = "magma", name = "sea surface\ntemperature") +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
              strip.background = element_rect(fill = "transparent")))

dev.off()
