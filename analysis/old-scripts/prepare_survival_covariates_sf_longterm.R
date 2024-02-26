####################### PREPARE SURVIVAL COVARIATES ############################

library(tidyverse) 
library(ncdf4) 
library(raster) 
library(sf) 
library(rnaturalearth)
library(rerddap)
library(viridis)
library(cowplot)

########################### SNOW COVER COVARIATE ###############################

# load shapefile of preferred rufa REKN breeding habitat
shp <- st_read("./data/Preferred_Habitat_and_Range_Intersect.shp")

####---- 1995 ----####

# load data in netCDF format

nc_1995 <- nc_open("./data/snowc.1995.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_1995 <- brick("./data/snowc.1995.nc", varname="snowc")

# subset to key nest initiation period
brick_1995 <- subset(brick_1995, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_1995))

# keep only cells inside rufa range
#brick_1995_rufa <- crop(brick_1995, extent(rufa_range))
brick_1995_rufa <- raster::mask(brick_1995, rufa_range)

# calculate mean value per cell for june 15-30
mean_1995_rufa <- calc(brick_1995_rufa, fun = mean, na.rm = TRUE)

#mean_1995_rufa <- crop(x = mean_1995_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_1995 <- getValues(mean_1995_rufa)
mean_snowc_1995 <- mean(vals_1995, na.rm=TRUE)
var_snowc_1995 <- var(vals_1995, na.rm = TRUE)

# plot
mean_1995_rufa_df <- as.data.frame(mean_1995_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 1995)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_1995_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 1996 ----####

# load data in netCDF format

nc_1996 <- nc_open("./data/snowc.1996.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_1996 <- brick("./data/snowc.1996.nc", varname="snowc")

# subset to key nest initiation period
brick_1996 <- subset(brick_1996, 167:182) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_1996))

# keep only cells inside rufa range
#brick_1996_rufa <- crop(brick_1996, extent(rufa_range))
brick_1996_rufa <- raster::mask(brick_1996, rufa_range)

# calculate mean value per cell for june 15-30
mean_1996_rufa <- calc(brick_1996_rufa, fun = mean, na.rm = TRUE)

#mean_1996_rufa <- crop(x = mean_1996_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_1996 <- getValues(mean_1996_rufa)
mean_snowc_1996 <- mean(vals_1996, na.rm=TRUE)
var_snowc_1996 <- var(vals_1996, na.rm = TRUE)

# plot
mean_1996_rufa_df <- as.data.frame(mean_1996_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 1996)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_1996_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 1997 ----####

# load data in netCDF format

nc_1997 <- nc_open("./data/snowc.1997.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_1997 <- brick("./data/snowc.1997.nc", varname="snowc")

# subset to key nest initiation period
brick_1997 <- subset(brick_1997, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_1997))

# keep only cells inside rufa range
#brick_1997_rufa <- crop(brick_1997, extent(rufa_range))
brick_1997_rufa <- raster::mask(brick_1997, rufa_range)

# calculate mean value per cell for june 15-30
mean_1997_rufa <- calc(brick_1997_rufa, fun = mean, na.rm = TRUE)

#mean_1997_rufa <- crop(x = mean_1997_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_1997 <- getValues(mean_1997_rufa)
mean_snowc_1997 <- mean(vals_1997, na.rm=TRUE)
var_snowc_1997 <- var(vals_1997, na.rm = TRUE)

# plot
mean_1997_rufa_df <- as.data.frame(mean_1997_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 1997)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_1997_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 1998 ----####

# load data in netCDF format

nc_1998 <- nc_open("./data/snowc.1998.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_1998 <- brick("./data/snowc.1998.nc", varname="snowc")

# subset to key nest initiation period
brick_1998 <- subset(brick_1998, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_1998))

# keep only cells inside rufa range
#brick_1998_rufa <- crop(brick_1998, extent(rufa_range))
brick_1998_rufa <- raster::mask(brick_1998, rufa_range)

# calculate mean value per cell for june 15-30
mean_1998_rufa <- calc(brick_1998_rufa, fun = mean, na.rm = TRUE)

#mean_1998_rufa <- crop(x = mean_1998_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_1998 <- getValues(mean_1998_rufa)
mean_snowc_1998 <- mean(vals_1998, na.rm=TRUE)
var_snowc_1998 <- var(vals_1998, na.rm = TRUE)

# plot
mean_1998_rufa_df <- as.data.frame(mean_1998_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 1998)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_1998_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 1999 ----####

# load data in netCDF format

nc_1999 <- nc_open("./data/snowc.1999.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_1999 <- brick("./data/snowc.1999.nc", varname="snowc")

# subset to key nest initiation period
brick_1999 <- subset(brick_1999, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_1999))

# keep only cells inside rufa range
#brick_1999_rufa <- crop(brick_1999, extent(rufa_range))
brick_1999_rufa <- raster::mask(brick_1999, rufa_range)

# calculate mean value per cell for june 15-30
mean_1999_rufa <- calc(brick_1999_rufa, fun = mean, na.rm = TRUE)

#mean_1999_rufa <- crop(x = mean_1999_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_1999 <- getValues(mean_1999_rufa)
mean_snowc_1999 <- mean(vals_1999, na.rm=TRUE)
var_snowc_1999 <- var(vals_1999, na.rm = TRUE)

# plot
mean_1999_rufa_df <- as.data.frame(mean_1999_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 1999)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_1999_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2000 ----####

# load data in netCDF format

nc_2000 <- nc_open("./data/snowc.2000.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2000 <- brick("./data/snowc.2000.nc", varname="snowc")

# subset to key nest initiation period
brick_2000 <- subset(brick_2000, 167:182) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2000))

# keep only cells inside rufa range
#brick_2000_rufa <- crop(brick_2000, extent(rufa_range))
brick_2000_rufa <- raster::mask(brick_2000, rufa_range)

# calculate mean value per cell for june 15-30
mean_2000_rufa <- calc(brick_2000_rufa, fun = mean, na.rm = TRUE)

#mean_2000_rufa <- crop(x = mean_2000_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2000 <- getValues(mean_2000_rufa)
mean_snowc_2000 <- mean(vals_2000, na.rm=TRUE)
var_snowc_2000 <- var(vals_2000, na.rm = TRUE)

# plot
mean_2000_rufa_df <- as.data.frame(mean_2000_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2000)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2000_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2001 ----####

# load data in netCDF format

nc_2001 <- nc_open("./data/snowc.2001.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2001 <- brick("./data/snowc.2001.nc", varname="snowc")

# subset to key nest initiation period
brick_2001 <- subset(brick_2001, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2001))

# keep only cells inside rufa range
#brick_2001_rufa <- crop(brick_2001, extent(rufa_range))
brick_2001_rufa <- raster::mask(brick_2001, rufa_range)

# calculate mean value per cell for june 15-30
mean_2001_rufa <- calc(brick_2001_rufa, fun = mean, na.rm = TRUE)

#mean_2001_rufa <- crop(x = mean_2001_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2001 <- getValues(mean_2001_rufa)
mean_snowc_2001 <- mean(vals_2001, na.rm=TRUE)
var_snowc_2001 <- var(vals_2001, na.rm = TRUE)

# plot
mean_2001_rufa_df <- as.data.frame(mean_2001_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2001)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2001_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2002 ----####

# load data in netCDF format

nc_2002 <- nc_open("./data/snowc.2002.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2002 <- brick("./data/snowc.2002.nc", varname="snowc")

# subset to key nest initiation period
brick_2002 <- subset(brick_2002, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2002))

# keep only cells inside rufa range
#brick_2002_rufa <- crop(brick_2002, extent(rufa_range))
brick_2002_rufa <- raster::mask(brick_2002, rufa_range)

# calculate mean value per cell for june 15-30
mean_2002_rufa <- calc(brick_2002_rufa, fun = mean, na.rm = TRUE)

#mean_2002_rufa <- crop(x = mean_2002_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2002 <- getValues(mean_2002_rufa)
mean_snowc_2002 <- mean(vals_2002, na.rm=TRUE)
var_snowc_2002 <- var(vals_2002, na.rm = TRUE)

# plot
mean_2002_rufa_df <- as.data.frame(mean_2002_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2002)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2002_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2003 ----####

# load data in netCDF format

nc_2003 <- nc_open("./data/snowc.2003.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2003 <- brick("./data/snowc.2003.nc", varname="snowc")

# subset to key nest initiation period
brick_2003 <- subset(brick_2003, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2003))

# keep only cells inside rufa range
#brick_2003_rufa <- crop(brick_2003, extent(rufa_range))
brick_2003_rufa <- raster::mask(brick_2003, rufa_range)

# calculate mean value per cell for june 15-30
mean_2003_rufa <- calc(brick_2003_rufa, fun = mean, na.rm = TRUE)

#mean_2003_rufa <- crop(x = mean_2003_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2003 <- getValues(mean_2003_rufa)
mean_snowc_2003 <- mean(vals_2003, na.rm=TRUE)
var_snowc_2003 <- var(vals_2003, na.rm = TRUE)

# plot
mean_2003_rufa_df <- as.data.frame(mean_2003_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2003)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2003_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2004 ----####

# load data in netCDF format

nc_2004 <- nc_open("./data/snowc.2004.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2004 <- brick("./data/snowc.2004.nc", varname="snowc")

# subset to key nest initiation period
brick_2004 <- subset(brick_2004, 167:182) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2004))

# keep only cells inside rufa range
#brick_2004_rufa <- crop(brick_2004, extent(rufa_range))
brick_2004_rufa <- raster::mask(brick_2004, rufa_range)

# calculate mean value per cell for june 15-30
mean_2004_rufa <- calc(brick_2004_rufa, fun = mean, na.rm = TRUE)

#mean_2004_rufa <- crop(x = mean_2004_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2004 <- getValues(mean_2004_rufa)
mean_snowc_2004 <- mean(vals_2004, na.rm=TRUE)
var_snowc_2004 <- var(vals_2004, na.rm = TRUE)

# plot
mean_2004_rufa_df <- as.data.frame(mean_2004_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2004)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2004_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2005 ----####

# load data in netCDF format

nc_2005 <- nc_open("./data/snowc.2005.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2005 <- brick("./data/snowc.2005.nc", varname="snowc")

# subset to key nest initiation period
brick_2005 <- subset(brick_2005, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2005))

# keep only cells inside rufa range
#brick_2005_rufa <- crop(brick_2005, extent(rufa_range))
brick_2005_rufa <- raster::mask(brick_2005, rufa_range)

# calculate mean value per cell for june 15-30
mean_2005_rufa <- calc(brick_2005_rufa, fun = mean, na.rm = TRUE)

#mean_2005_rufa <- crop(x = mean_2005_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2005 <- getValues(mean_2005_rufa)
mean_snowc_2005 <- mean(vals_2005, na.rm=TRUE)
var_snowc_2005 <- var(vals_2005, na.rm = TRUE)

# plot
mean_2005_rufa_df <- as.data.frame(mean_2005_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2005)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2005_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2006 ----####

# load data in netCDF format

nc_2006 <- nc_open("./data/snowc.2006.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2006 <- brick("./data/snowc.2006.nc", varname="snowc")

# subset to key nest initiation period
brick_2006 <- subset(brick_2006, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2006))

# keep only cells inside rufa range
#brick_2006_rufa <- crop(brick_2006, extent(rufa_range))
brick_2006_rufa <- raster::mask(brick_2006, rufa_range)

# calculate mean value per cell for june 15-30
mean_2006_rufa <- calc(brick_2006_rufa, fun = mean, na.rm = TRUE)

#mean_2006_rufa <- crop(x = mean_2006_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2006 <- getValues(mean_2006_rufa)
mean_snowc_2006 <- mean(vals_2006, na.rm=TRUE)
var_snowc_2006 <- var(vals_2006, na.rm = TRUE)

# plot
mean_2006_rufa_df <- as.data.frame(mean_2006_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2006)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2006_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2007 ----####

# load data in netCDF format

nc_2007 <- nc_open("./data/snowc.2007.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2007 <- brick("./data/snowc.2007.nc", varname="snowc")

# subset to key nest initiation period
brick_2007 <- subset(brick_2007, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2007))

# keep only cells inside rufa range
#brick_2007_rufa <- crop(brick_2007, extent(rufa_range))
brick_2007_rufa <- raster::mask(brick_2007, rufa_range)

# calculate mean value per cell for june 15-30
mean_2007_rufa <- calc(brick_2007_rufa, fun = mean, na.rm = TRUE)

#mean_2007_rufa <- crop(x = mean_2007_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2007 <- getValues(mean_2007_rufa)
mean_snowc_2007 <- mean(vals_2007, na.rm=TRUE)
var_snowc_2007 <- var(vals_2007, na.rm = TRUE)

# plot
mean_2007_rufa_df <- as.data.frame(mean_2007_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2007)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2007_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2008 ----####

# load data in netCDF format

nc_2008 <- nc_open("./data/snowc.2008.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2008 <- brick("./data/snowc.2008.nc", varname="snowc")

# subset to key nest initiation period
brick_2008 <- subset(brick_2008, 167:182) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2008))

# keep only cells inside rufa range
#brick_2008_rufa <- crop(brick_2008, extent(rufa_range))
brick_2008_rufa <- raster::mask(brick_2008, rufa_range)

# calculate mean value per cell for june 15-30
mean_2008_rufa <- calc(brick_2008_rufa, fun = mean, na.rm = TRUE)

#mean_2008_rufa <- crop(x = mean_2008_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2008 <- getValues(mean_2008_rufa)
mean_snowc_2008 <- mean(vals_2008, na.rm=TRUE)
var_snowc_2008 <- var(vals_2008, na.rm = TRUE)

# plot
mean_2008_rufa_df <- as.data.frame(mean_2008_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2008)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2008_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- 2009 ----####

# load data in netCDF format

nc_2009 <- nc_open("./data/snowc.2009.nc")

# convert to rasterbrick format (layered rasters - one per day)
brick_2009 <- brick("./data/snowc.2009.nc", varname="snowc")

# subset to key nest initiation period
brick_2009 <- subset(brick_2009, 166:181) # june 15-30

rufa_range <- shp %>% 
  st_transform(st_crs(brick_2009))

# keep only cells inside rufa range
#brick_2009_rufa <- crop(brick_2009, extent(rufa_range))
brick_2009_rufa <- raster::mask(brick_2009, rufa_range)

# calculate mean value per cell for june 15-30
mean_2009_rufa <- calc(brick_2009_rufa, fun = mean, na.rm = TRUE)

#mean_2009_rufa <- crop(x = mean_2009_rufa, y = extent(rufa_range)) # crop to zoom in on breeding range

# calculate overall mean
vals_2009 <- getValues(mean_2009_rufa)
mean_snowc_2009 <- mean(vals_2009, na.rm=TRUE)
var_snowc_2009 <- var(vals_2009, na.rm = TRUE)

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
var_snowc_2010 <- var(vals_2010, na.rm = TRUE)

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
var_snowc_2011 <- var(vals_2011, na.rm = TRUE)

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
var_snowc_2012 <- var(vals_2012, na.rm = TRUE)

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
var_snowc_2013 <- var(vals_2013, na.rm = TRUE)

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
var_snowc_2014 <- var(vals_2014, na.rm = TRUE)

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
var_snowc_2015 <- var(vals_2015, na.rm = TRUE)

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
var_snowc_2016 <- var(vals_2016, na.rm = TRUE)

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
var_snowc_2017 <- var(vals_2017, na.rm = TRUE)

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
var_snowc_2018 <- var(vals_2018, na.rm = TRUE)

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
var_snowc_2019 <- var(vals_2019, na.rm = TRUE)

# plot
mean_2019_rufa_df <- as.data.frame(mean_2019_rufa, xy = TRUE) %>% 
  filter(!is.na(layer)) %>% 
  mutate(year = 2019)

ggplot() +
  geom_sf(data = rufa_range, fill = NA, colour = "black") +
  geom_tile(data = mean_2019_rufa_df, aes(x = x, y = y, fill = layer)) 

####---- all years ----####

# create dataframe of annual means

Year <- c(1995:2019)

mean_snowc <- c(mean_snowc_1995, mean_snowc_1996, mean_snowc_1997, mean_snowc_1998,
                mean_snowc_1999, mean_snowc_2000, mean_snowc_2001, mean_snowc_2002,
                mean_snowc_2003, mean_snowc_2004, mean_snowc_2005, mean_snowc_2006,
                mean_snowc_2007, mean_snowc_2008,
                mean_snowc_2009, mean_snowc_2010, mean_snowc_2011, mean_snowc_2012,
                mean_snowc_2013, mean_snowc_2014, mean_snowc_2015, mean_snowc_2016,
                mean_snowc_2017, mean_snowc_2018, mean_snowc_2019)

var_snowc <- c(var_snowc_1995, var_snowc_1996, var_snowc_1997, var_snowc_1998,
               var_snowc_1999, var_snowc_2000, var_snowc_2001, var_snowc_2002,
               var_snowc_2003, var_snowc_2004, var_snowc_2005, var_snowc_2006,
               var_snowc_2007, var_snowc_2008,
               var_snowc_2009, var_snowc_2010, var_snowc_2011, var_snowc_2012,
               var_snowc_2013, var_snowc_2014, var_snowc_2015, var_snowc_2016,
               var_snowc_2017, var_snowc_2018, var_snowc_2019)

arctic_snow_cov <- as.data.frame(cbind(Year, mean_snowc, var_snowc))

arctic_snow_cov <- arctic_snow_cov %>% 
  #filter(!Year == 2009) %>% 
  mutate(Period = 1:25)

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
sst_var_2010 <- var(sst_vals_2010, na.rm=TRUE)

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
sst_var_2011 <- var(sst_vals_2011, na.rm=TRUE)

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
sst_var_2012 <- var(sst_vals_2012, na.rm=TRUE)

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
sst_var_2013 <- var(sst_vals_2013, na.rm=TRUE)

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
sst_var_2014 <- var(sst_vals_2014, na.rm=TRUE)

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
sst_var_2015 <- var(sst_vals_2015, na.rm=TRUE)

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
sst_var_2016 <- var(sst_vals_2016, na.rm=TRUE)

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
sst_var_2017 <- var(sst_vals_2017, na.rm=TRUE)

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
sst_var_2018 <- var(sst_vals_2018, na.rm=TRUE)

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
sst_var_2019 <- var(sst_vals_2019, na.rm=TRUE)

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

var_sst <- c(sst_var_2010, sst_var_2011, sst_var_2012,
             sst_var_2013, sst_var_2014, sst_var_2015, sst_var_2016,
             sst_var_2017, sst_var_2018, sst_var_2019)

sst_cov <- as.data.frame(cbind(Year, mean_sst, var_sst))

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

###################### SEA SURFACE TEMPERATURE ANOMALY #########################

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
sst_anom_var_2010 <- var(sst_vals_2010, na.rm=TRUE)

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
sst_anom_var_2011 <- var(sst_vals_2011, na.rm=TRUE)

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
sst_anom_var_2012 <- var(sst_vals_2012, na.rm=TRUE)

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
sst_anom_var_2013 <- var(sst_vals_2013, na.rm=TRUE)

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
sst_anom_var_2014 <- var(sst_vals_2014, na.rm=TRUE)

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
sst_anom_var_2015 <- var(sst_vals_2015, na.rm=TRUE)

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
sst_anom_var_2016 <- var(sst_vals_2016, na.rm=TRUE)

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
sst_anom_var_2017 <- var(sst_vals_2017, na.rm=TRUE)

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
sst_anom_var_2018 <- var(sst_vals_2018, na.rm=TRUE)

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
sst_anom_var_2019 <- var(sst_vals_2019, na.rm=TRUE)

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

var_sst_anomaly <- c(sst_anom_var_2010, sst_anom_var_2011, sst_anom_var_2012,
                      sst_anom_var_2013, sst_anom_var_2014, sst_anom_var_2015, sst_anom_var_2016,
                      sst_anom_var_2017, sst_anom_var_2018, sst_anom_var_2019)

sst_anom_cov <- as.data.frame(cbind(Year, mean_sst_anomaly, var_sst_anomaly))

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

######################### NORTH ATLANTIC OSCILLATION ###########################

nao <- read_csv("./data/nao/north_atlantic_oscillation_long.csv")

nao_cov <- nao %>% 
  #filter(!Year == 2009) %>% 
  filter(Month %in% c(5, 6, 7)) %>% 
  group_by(Year) %>% 
  summarize(mean_nao = mean(NAO),
            var_nao = var(NAO)) %>% 
  mutate(Period = 1:25)

############################# ARCTIC OSCILLATION ###############################

ao <- read_csv("./data/ao/arctic_oscillation_long.csv")

ao_cov <- ao %>% 
  #filter(!Year == 2009) %>% 
  filter(Month %in% c(6, 7)) %>% 
  group_by(Year) %>% 
  summarize(mean_ao = mean(AO),
            var_ao = var(AO)) %>% 
  mutate(Period = 1:25)

############################ HORSESHOE CRAB EGGS ###############################

hsc_egg <- read_csv("./data/hsc/dat_fit.csv")

hsc_cov <- hsc_egg %>% 
  filter(group %in% c(2000:2019)) %>% 
  filter(x %in% c(130:151)) %>% 
  group_by(group) %>% 
  summarize(mean_hsc = mean(predicted),
            var_hsc = var(predicted)) %>% 
  mutate(Period = 6:25) %>%
  rename(Year = group) %>% 
  ungroup()

# lag effect
hsc_lag_cov <- hsc_egg %>% 
  filter(group %in% c(2009:2018)) %>% 
  filter(x %in% c(130:151)) %>% 
  group_by(group) %>% 
  summarize(mean_hsc_lag = mean(predicted),
            var_hsc_lag = var(predicted)) %>% 
  mutate(Period = 1:10) %>%
  rename(Year = group) %>% 
  ungroup()

################################################################################

# merge covariates

#surv_covs <- left_join(arctic_snow_cov, sst_cov, by = c("Year", "Period"))
#surv_covs <- left_join(surv_covs, sst_anom_cov, by = c("Year", "Period"))
surv_covs <- left_join(arctic_snow_cov, nao_cov, by = c("Year", "Period"))
surv_covs <- left_join(surv_covs, ao_cov, by = c("Year", "Period"))
surv_covs <- left_join(surv_covs, hsc_cov, by = c("Year", "Period"))
#surv_covs <- surv_covs %>% 
#  dplyr::select(-Year) %>% 
#  left_join(., hsc_lag_cov %>% dplyr::select(-Year), by = "Period")

saveRDS(surv_covs, file = "./processed-data/surv_covariates_long.rds")

# standardize covariates

surv_covs_std <- surv_covs %>% 
  mutate(mean_sst_std = (mean_sst - mean(mean_sst))/sd(mean_sst),
         sst_anom_std = (mean_sst_anomaly - mean(mean_sst_anomaly))/sd(mean_sst_anomaly),
         mean_snowc_std = (mean_snowc - mean(mean_snowc))/sd(mean_snowc),
         mean_nao_std = (mean_nao - mean(mean_nao))/sd(mean_nao),
         mean_ao_std = (mean_ao - mean(mean_ao))/sd(mean_ao),
         mean_hsc_std = (mean_hsc - mean(mean_hsc))/sd(mean_hsc),
         mean_hsc_lag_std = (mean_hsc_lag - mean(mean_hsc_lag))/sd(mean_hsc_lag)) %>% 
  dplyr::select(-mean_sst, -mean_sst_anomaly, -mean_snowc, -mean_nao, -mean_ao, -mean_hsc, -mean_hsc_lag)

saveRDS(surv_covs_std, file = "./processed-data/surv_covariates_std.rds")

################################################################################

# plot covariates

windowsFonts(Times=windowsFont("TT Times New Roman"))

# set custom theme for all plots
theme_cust <- function() {
  theme_classic(base_family = "Times") %+replace%
    theme(axis.title.x = element_text(size=12),
          axis.text.x  = element_text(size=10, colour = "black"),
          axis.title.y = element_text(size=12, angle = 90, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.y = element_text(size=10, colour = "black"),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          strip.text.x = element_text(size=12, face = "bold"),
          legend.text = element_text(size=10),
          legend.key.height = unit(1, "mm"),
          plot.title = element_text(size = 12, hjust = 0, vjust = 1.5),
          #panel.border = element_rect(size =0.5, fill = "transparent"),
          plot.margin = margin(10, 10, 10, 15))
}

# snow cover
snow_plot <- ggplot(surv_covs, aes(x=as.factor(Year), y=mean_snowc*100, group=1)) +
  geom_rect(data=NULL,aes(xmin=-Inf,xmax="2008",ymin=-Inf,ymax=Inf),
            fill="grey90", alpha=0.5) +
  stat_smooth(colour = "grey50") +
  geom_errorbar(data = surv_covs, aes(x=as.factor(Year), ymin=(mean_snowc*100)-(var_snowc*100),
                                      ymax=(mean_snowc*100)+(var_snowc*100)),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(shape = 21, size=2, fill = "black") +
  ylab("Mean snow cover (%)") +
  theme_cust() +
  theme(axis.text.x  = element_text(angle = -45, vjust = 0.5),
        axis.title.x = element_blank())

# sea surface temp
sst_plot <- ggplot(surv_covs, aes(x=as.factor(Year), y=mean_sst, group=1)) +
  geom_errorbar(data = surv_covs, aes(x=as.factor(Year), ymin=mean_sst-var_sst,
                                      ymax=mean_sst+var_sst),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(shape = 21, size=2, fill = "black") +
  ylab("Mean sea surface\ntemperature (°C)") +
  theme_cust() +
  theme(axis.text.x  = element_text(angle = -45, vjust = 0.5),
        axis.title.x = element_blank())

# sea surface temp anomaly
sst_anom_plot <- ggplot(surv_covs, aes(x=as.factor(Year), y=mean_sst_anomaly, group=1)) +
  geom_errorbar(data = surv_covs, aes(x=as.factor(Year), ymin=mean_sst_anomaly-var_sst_anomaly,
                                      ymax=mean_sst_anomaly+var_sst_anomaly),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(shape = 21, size=2, fill = "black") +
  ylab("Mean sea surface\ntemperature anomaly (°C)") +
  theme_cust() +
  theme(axis.text.x  = element_text(angle = -45, vjust = 0.5),
        axis.title.x = element_blank())

# nao
nao_plot <- ggplot(surv_covs, aes(x=as.factor(Year), y=mean_nao, group=1)) +
  geom_rect(data=NULL,aes(xmin=-Inf,xmax="2008",ymin=-Inf,ymax=Inf),
            fill="grey90", alpha=0.5) +
  stat_smooth(colour = "grey50") +
  geom_errorbar(data = surv_covs, aes(x=as.factor(Year), ymin=mean_nao-var_nao,
                                      ymax=mean_nao+var_nao),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(shape = 21, size=2, fill = "black") +
  ylab("North Atlantic Oscillation") +
  theme_cust() +
  theme(axis.text.x  = element_text(angle = -45, vjust = 0.5),
        axis.title.x = element_blank())

# ao
ao_plot <- ggplot(surv_covs, aes(x=as.factor(Year), y=mean_ao, group=1)) +
  geom_rect(data=NULL,aes(xmin=-Inf,xmax="2008",ymin=-Inf,ymax=Inf),
            fill="grey90", alpha=0.5) +
  stat_smooth(colour = "grey50") +
  geom_errorbar(data = surv_covs, aes(x=as.factor(Year), ymin=mean_ao-var_ao,
                                      ymax=mean_ao+var_ao),
                width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(shape = 21, size=2, fill = "black") +
  ylab("Arctic Oscillation") +
  theme_cust() +
  theme(axis.text.x  = element_text(angle = -45, vjust = 0.5),
        axis.title.x = element_blank())

# hsc egg availability
hsc_plot <- ggplot(surv_covs, aes(x=as.factor(Year), y=mean_hsc, group=1)) +
  geom_rect(data=NULL,aes(xmin=-Inf,xmax="2008",ymin=-Inf,ymax=Inf),
            fill="grey90", alpha=0.5) +
  stat_smooth(colour = "grey50") +
  # geom_errorbar(data = surv_covs, aes(x=as.factor(Year), ymin=mean_hsc-var_hsc,
  #                                     ymax=mean_hsc+var_hsc),
  #               width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(shape = 21, size=2, fill = "black") +
  ylab("Horseshoe crab egg density") +
  theme_cust() +
  theme(axis.text.x  = element_text(angle = -45, vjust = 0.5),
        axis.title.x = element_blank())

# plot all
png(filename = paste0("figures/cov-plots-long.png"),
    width=10, height=7, units="in", res=600)

plot_grid(snow_plot, nao_plot, ao_plot, hsc_plot, labels = "auto", ncol = 2)

dev.off()

png(filename = paste0("figures/cov-plots.png"),
    width=6, height=6, units="in", res=600)

plot_grid(snow_plot, sst_anom_plot, nao_plot, ao_plot, labels = "auto", ncol = 2)

dev.off()

################################################################################
# Principal component analysis
#
################################################################################

library(corrplot)
library(PerformanceAnalytics)

# load covariates
surv_covs <- readRDS("./processed-data/surv_covariates.rds")

surv_covs_pca <- surv_covs %>% 
  dplyr::select(mean_snowc, mean_sst, mean_nao, mean_ao, mean_hsc)

# check correlations
cor.mat <- round(cor(surv_covs_pca), 2)
cor.mat
corrplot(cor.mat, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
chart.Correlation(surv_covs_pca, histogram = TRUE, pch = 19)

surv_pca <- prcomp(surv_covs_pca, center = TRUE, scale. = TRUE)

summary(surv_pca)

# eigenvalues
surv_pca$sdev^2

# scores
surv_pca$x

# loadings
surv_pca$rotation

biplot(surv_pca)

################################################################################
library(FactoMineR)
library(factoextra)
library(corrplot)
library(PerformanceAnalytics)

# load covariates
surv_covs <- readRDS("./processed-data/surv_covariates.rds")

surv_covs_pca <- surv_covs %>% 
  dplyr::select(mean_snowc, mean_sst, mean_nao, mean_ao)

# check correlations
cor.mat <- round(cor(surv_covs_pca), 2)
cor.mat
corrplot(cor.mat, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
chart.Correlation(surv_covs_pca, histogram = TRUE, pch = 19)

# run the pca
surv_pca <- PCA(surv_covs_pca, scale.unit = TRUE, graph = TRUE)

eigenvalues <- surv_pca$eig
fviz_screeplot(surv_pca)

# loadings
loadings_pca <- surv_pca$svd$V
loadings_pca2 <- surv_pca$var$coord
saveRDS(loadings_pca, "./processed-data/surv_pc_loadings.rds")
sweep(surv_pca$var$coord,2,sqrt(surv_pca$eig[1:ncol(surv_pca$var$coord),1]),FUN="/")

# scores
scores_pca <- surv_pca$ind$coord
saveRDS(scores_pca, "./processed-data/surv_pc_scores.rds")

################################################################################

# plot just nao and ao

surv_covs <- readRDS("./processed-data/surv_covariates.rds")

osc_covs <- surv_covs %>% 
  select(Year, mean_nao, mean_ao) %>% 
  pivot_longer(!Year, names_to = "teleconnection", values_to = "Index")

osc_plot <- ggplot(osc_covs, aes(x = as.factor(Year), y = Index, group = teleconnection,
                                 fill = teleconnection)) +
  # geom_errorbar(data = surv_covs, aes(x=as.factor(Year), ymin=mean_ao-var_ao,
  #                                     ymax=mean_ao+var_ao),
  #               width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(shape = 21, size = 3) +
  scale_fill_viridis(discrete = TRUE, breaks = c("mean_ao", "mean_nao"),
                     labels = c("Arctic Oscillation", "North Atlantic Oscillation")) +
  scale_y_continuous(breaks = seq(-2, 2, 0.5)) +
  theme_cust() +
  theme(axis.text.x  = element_text(angle = -45, vjust = 0.5),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.85))

png(filename = paste0("figures/oscillation-plots.png"),
    width=6, height=4, units="in", res=600)

print(osc_plot)

dev.off()

# plot hsc egg density

hsc_plot <- ggplot(surv_covs, aes(x = as.factor(Year), y = mean_hsc, group = 1)) +
  # geom_errorbar(data = surv_covs, aes(x=as.factor(Year), ymin=mean_ao-var_ao,
  #                                     ymax=mean_ao+var_ao),
  #               width=0, size=0.5, colour="black", linetype=1) +
  geom_line(size=0.5) +
  geom_point(shape = 21, size = 3, fill = "blue") +
  #scale_y_continuous(breaks = seq(-2, 2, 0.5)) +
  ylab(expression(paste("Predicted horseshoe crab eggs per ", m^2))) +
  theme_cust() +
  theme(axis.text.x  = element_text(angle = -45, vjust = 0.5),
        axis.title.x = element_blank())

png(filename = paste0("figures/hsc-plot.png"),
    width=6, height=4, units="in", res=600)

print(hsc_plot)

dev.off()

