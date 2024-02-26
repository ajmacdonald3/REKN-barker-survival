########################## SEA SURFACE TEMPERATURE #############################

coast <- ne_coastline(scale = "large", returnclass = "sf")

states <- ne_states(country = "United States of America", returnclass = "sf") %>% 
  filter(postal %in% c("ME", "NH", "MA", "RI", "CT", "NY", "NJ", "DE", "MD",
                       "VA", "NC", "SC", "GA", "FL", "AL", "MS"))

coastline <- st_transform(coast, crs = 5070)

states <- st_transform(states, crs = 5070)

atl_coast <- st_intersection(coastline, states)

atl_coast_buf <- st_buffer(atl_coast, dist = 30000)

atl_coast_buf <- st_union(atl_coast_buf)

st_write(atl_coast_buf, "./map-data/atl_coast_30km.shp")

ggplot() +
  geom_sf(data = atl_coast_buf, alpha = 0.5) +
  geom_sf(data = atl_coast)

####---- 2010 ----####

# load data in netCDF format

nc_2010 <- nc_open("./data/sst/sst.day.mean.2010.nc")

{
  sink("./data/sst/sst.day.mean.2010.nc_metadata.txt")
  print(nc_2010)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2010 <- brick("./data/sst/sst.day.mean.2010.nc", varname="sst")

# subset to key nest initiation period
brick_2010 <- subset(brick_2010, 121:181) # june 15-30

# rotate so longitudes match
brick_2010 <- raster::rotate(brick_2010)

# match crs of shapefile with raster
atl_coast_hsc <- atl_coast_buf %>%
  st_transform(st_crs(brick_2010)) %>% 
  st_as_sf()

st_write(atl_coast_hsc, "./map-data/atl_coast_hsc.shp")

# keep only cells inside rufa range
brick_2010_sst <- raster::mask(brick_2010, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2010_sst <- calc(brick_2010_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2010 <- getValues(mean_2010_sst)
mean_sst_2010 <- mean(vals_2010, na.rm=TRUE)

# plot
mean_2010_sst_df <- as.data.frame(mean_2010_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2010)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2010_sst_df, aes(x = x, y = y, fill = layer))

####---- 2011 ----####

# load data in netCDF format

nc_2011 <- nc_open("./data/sst/sst.day.mean.2011.nc")

{
  sink("./data/sst/sst.day.mean.2011.nc_metadata.txt")
  print(nc_2011)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2011 <- brick("./data/sst/sst.day.mean.2011.nc", varname="sst")

# subset to key nest initiation period
brick_2011 <- subset(brick_2011, 121:181) # june 15-30

# rotate so longitudes match
brick_2011 <- raster::rotate(brick_2011)

# keep only cells inside rufa range
brick_2011_sst <- raster::mask(brick_2011, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2011_sst <- calc(brick_2011_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2011 <- getValues(mean_2011_sst)
mean_sst_2011 <- mean(vals_2011, na.rm=TRUE)

# plot
mean_2011_sst_df <- as.data.frame(mean_2011_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2011)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2011_sst_df, aes(x = x, y = y, fill = layer))

####---- 2012 ----####

# load data in netCDF format

nc_2012 <- nc_open("./data/sst/sst.day.mean.2012.nc")

{
  sink("./data/sst/sst.day.mean.2012.nc_metadata.txt")
  print(nc_2012)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2012 <- brick("./data/sst/sst.day.mean.2012.nc", varname="sst")

# subset to key nest initiation period
brick_2012 <- subset(brick_2012, 122:182) # june 15-30

# rotate so longitudes match
brick_2012 <- raster::rotate(brick_2012)

# keep only cells inside rufa range
brick_2012_sst <- raster::mask(brick_2012, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2012_sst <- calc(brick_2012_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2012 <- getValues(mean_2012_sst)
mean_sst_2012 <- mean(vals_2012, na.rm=TRUE)

# plot
mean_2012_sst_df <- as.data.frame(mean_2012_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2012)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2012_sst_df, aes(x = x, y = y, fill = layer))

####---- 2013 ----####

# load data in netCDF format

nc_2013 <- nc_open("./data/sst/sst.day.mean.2013.nc")

{
  sink("./data/sst/sst.day.mean.2013.nc_metadata.txt")
  print(nc_2013)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2013 <- brick("./data/sst/sst.day.mean.2013.nc", varname="sst")

# subset to key nest initiation period
brick_2013 <- subset(brick_2013, 121:181) # june 15-30

# rotate so longitudes match
brick_2013 <- raster::rotate(brick_2013)

# keep only cells inside rufa range
brick_2013_sst <- raster::mask(brick_2013, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2013_sst <- calc(brick_2013_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2013 <- getValues(mean_2013_sst)
mean_sst_2013 <- mean(vals_2013, na.rm=TRUE)

# plot
mean_2013_sst_df <- as.data.frame(mean_2013_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2013)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2013_sst_df, aes(x = x, y = y, fill = layer))

####---- 2014 ----####

# load data in netCDF format

nc_2014 <- nc_open("./data/sst/sst.day.mean.2014.nc")

{
  sink("./data/sst/sst.day.mean.2014.nc_metadata.txt")
  print(nc_2014)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2014 <- brick("./data/sst/sst.day.mean.2014.nc", varname="sst")

# subset to key nest initiation period
brick_2014 <- subset(brick_2014, 121:181) # june 15-30

# rotate so longitudes match
brick_2014 <- raster::rotate(brick_2014)

# keep only cells inside rufa range
brick_2014_sst <- raster::mask(brick_2014, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2014_sst <- calc(brick_2014_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2014 <- getValues(mean_2014_sst)
mean_sst_2014 <- mean(vals_2014, na.rm=TRUE)

# plot
mean_2014_sst_df <- as.data.frame(mean_2014_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2014)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2014_sst_df, aes(x = x, y = y, fill = layer))

####---- 2015 ----####

# load data in netCDF format

nc_2015 <- nc_open("./data/sst/sst.day.mean.2015.nc")

{
  sink("./data/sst/sst.day.mean.2015.nc_metadata.txt")
  print(nc_2015)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2015 <- brick("./data/sst/sst.day.mean.2015.nc", varname="sst")

# subset to key nest initiation period
brick_2015 <- subset(brick_2015, 121:181) # june 15-30

# rotate so longitudes match
brick_2015 <- raster::rotate(brick_2015)

# keep only cells inside rufa range
brick_2015_sst <- raster::mask(brick_2015, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2015_sst <- calc(brick_2015_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2015 <- getValues(mean_2015_sst)
mean_sst_2015 <- mean(vals_2015, na.rm=TRUE)

# plot
mean_2015_sst_df <- as.data.frame(mean_2015_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2015)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2015_sst_df, aes(x = x, y = y, fill = layer))

####---- 2016 ----####

# load data in netCDF format

nc_2016 <- nc_open("./data/sst/sst.day.mean.2016.nc")

{
  sink("./data/sst/sst.day.mean.2016.nc_metadata.txt")
  print(nc_2016)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2016 <- brick("./data/sst/sst.day.mean.2016.nc", varname="sst")

# subset to key nest initiation period
brick_2016 <- subset(brick_2016, 122:182) # june 15-30

# rotate so longitudes match
brick_2016 <- raster::rotate(brick_2016)

# keep only cells inside rufa range
brick_2016_sst <- raster::mask(brick_2016, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2016_sst <- calc(brick_2016_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2016 <- getValues(mean_2016_sst)
mean_sst_2016 <- mean(vals_2016, na.rm=TRUE)

# plot
mean_2016_sst_df <- as.data.frame(mean_2016_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2016)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2016_sst_df, aes(x = x, y = y, fill = layer))

####---- 2017 ----####

# load data in netCDF format

nc_2017 <- nc_open("./data/sst/sst.day.mean.2017.nc")

{
  sink("./data/sst/sst.day.mean.2017.nc_metadata.txt")
  print(nc_2017)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2017 <- brick("./data/sst/sst.day.mean.2017.nc", varname="sst")

# subset to key nest initiation period
brick_2017 <- subset(brick_2017, 121:181) # june 15-30

# rotate so longitudes match
brick_2017 <- raster::rotate(brick_2017)

# keep only cells inside rufa range
brick_2017_sst <- raster::mask(brick_2017, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2017_sst <- calc(brick_2017_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2017 <- getValues(mean_2017_sst)
mean_sst_2017 <- mean(vals_2017, na.rm=TRUE)

# plot
mean_2017_sst_df <- as.data.frame(mean_2017_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2017)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2017_sst_df, aes(x = x, y = y, fill = layer))

####---- 2018 ----####

# load data in netCDF format

nc_2018 <- nc_open("./data/sst/sst.day.mean.2018.nc")

{
  sink("./data/sst/sst.day.mean.2018.nc_metadata.txt")
  print(nc_2018)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2018 <- brick("./data/sst/sst.day.mean.2018.nc", varname="sst")

# subset to key nest initiation period
brick_2018 <- subset(brick_2018, 121:181) # june 15-30

# rotate so longitudes match
brick_2018 <- raster::rotate(brick_2018)

# keep only cells inside rufa range
brick_2018_sst <- raster::mask(brick_2018, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2018_sst <- calc(brick_2018_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2018 <- getValues(mean_2018_sst)
mean_sst_2018 <- mean(vals_2018, na.rm=TRUE)

# plot
mean_2018_sst_df <- as.data.frame(mean_2018_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2018)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2018_sst_df, aes(x = x, y = y, fill = layer))

####---- 2019 ----####

# load data in netCDF format

nc_2019 <- nc_open("./data/sst/sst.day.mean.2019.nc")

{
  sink("./data/sst/sst.day.mean.2019.nc_metadata.txt")
  print(nc_2019)
  sink()
}

# convert to rasterbrick format (layered rasters - one per day)
brick_2019 <- brick("./data/sst/sst.day.mean.2019.nc", varname="sst")

# subset to key nest initiation period
brick_2019 <- subset(brick_2019, 121:181) # june 15-30

# rotate so longitudes match
brick_2019 <- raster::rotate(brick_2019)

# keep only cells inside rufa range
brick_2019_sst <- raster::mask(brick_2019, atl_coast_hsc)

# calculate mean value per cell for june 15-30
mean_2019_sst <- calc(brick_2019_sst, fun = mean, na.rm = TRUE)

# calculate overall mean
vals_2019 <- getValues(mean_2019_sst)
mean_sst_2019 <- mean(vals_2019, na.rm=TRUE)

# plot
mean_2019_sst_df <- as.data.frame(mean_2019_sst, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2019)

ggplot() +
  geom_sf(data = atl_coast_hsc, fill = NA, colour = "black") +
  geom_tile(data = mean_2019_sst_df, aes(x = x, y = y, fill = layer))

####---- all years ----####

# create dataframe of annual means

Year <- c(2010:2019)
mean_sst <- c(mean_sst_2010, mean_sst_2011, mean_sst_2012,
              mean_sst_2013, mean_sst_2014, mean_sst_2015, mean_sst_2016,
              mean_sst_2017, mean_sst_2018, mean_sst_2019)

sst_cov <- as.data.frame(cbind(Year, mean_sst))

sst_cov <- sst_cov %>% 
  mutate(Period = 1:10)

# combine raster dataframes
mean_sst_df <- bind_rows(mean_2010_sst_df, mean_2011_sst_df, mean_2012_sst_df,
                         mean_2013_sst_df, mean_2014_sst_df, mean_2015_sst_df,
                         mean_2016_sst_df, mean_2017_sst_df, mean_2018_sst_df,
                         mean_2019_sst_df)

# plot rasters
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

world <- world %>% 
  st_transform(st_crs(atl_coast_hsc))

lakes <- lakes %>% 
  st_transform(st_crs(atl_coast_hsc))

# plot sst
theme_set(theme_minimal())

xmin <- min(mean_sst_df$x) - 3
xmax <- max(mean_sst_df$x) + 3
ymin <- min(mean_sst_df$y) - 3
ymax <- max(mean_sst_df$y) + 3

png(filename = "./figures/sst-map.png", height = 4, width = 8,
    units = "in", res = 600)

print(ggplot(data = mean_sst_df) +
        geom_sf(data = world, colour = NA) +
        geom_sf(data = lakes, fill = "white", colour = NA) +
        #geom_sf(data = rufa_range, fill = NA, colour = "black") +
        geom_tile(data = mean_sst_df, aes(x = x, y = y, fill = layer)) +
        #geom_sf(data = rufa_range2, fill = NA, colour = "black", size = 0.2) +
        coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax),
                 expand = FALSE) +
        facet_wrap(. ~ year, ncol = 5) +
        scale_fill_gradientn(colors = rev(hcl.colors(9, "RdYlBu")), name = "sea surface\ntemperature") +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_line(colour = "transparent"),
              strip.background = element_rect(fill = "transparent")))

dev.off()
