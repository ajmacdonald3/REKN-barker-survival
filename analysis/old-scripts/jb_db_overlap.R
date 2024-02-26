################################################################################
# JAMES BAY AND DELAWARE BAY BIRD OVERLAP
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

################################################################################

# combine and clean all resighting data

# load data
all_data <- read_excel("./data/all_projects_resights.xlsx", sheet = "REKNresights")
jb_data <- read_excel("./data/jb_project_resights.xlsx", sheet = "Sheet1")
cc_data <- read_excel("./data/cc_project_resights.xlsx", sheet = "CapeCod_REKNresights_JEL")
br_data <- read_excel("./data/br_project_resights.xlsx", sheet = "proj87REKNresights")
pb_data <- read_excel("./data/bandedbirds_publicresights.xlsx", sheet = "qry_Res_proj1REKNresights")
fl_data1 <- read_excel("./data/fl_banding_resights.xlsx", sheet = "proj17ResightData")
fl_data2 <- read_excel("./data/fl_banding_resights.xlsx", sheet = "proj33ResightData") %>% 
  select(-`...18`)

fl_data <- bind_rows(fl_data1, fl_data2) %>% 
  mutate(Name = LocationID) %>% 
  rename(dbo_ResightingMasters.Comments = ResightingMasters.Comments) %>% 
  rename(dbo_Resightings.Comments = Resightings.Comments) %>% 
  select(ProjectID, ResightDate, LocationID, Name, Latitude, Longitude, dbo_ResightingMasters.Comments,
         SpeciesID, BirdID, MetalID, FlagID, FlagCode, dbo_Resightings.Comments, ResightCertainty)
rm(fl_data1, fl_data2)

pb_data <- pb_data %>% 
  mutate(Name = NA) %>% 
  mutate(ResightCertainty = NA) %>% 
  select(ProjectID, ResightDate, LocationID, Name, Latitude, Longitude, dbo_ResightingMasters.Comments,
         SpeciesID, BirdID, MetalID, FlagID, FlagCode, dbo_Resightings.Comments, ResightCertainty)

all_resights <- bind_rows(all_data, jb_data)
all_resights <- bind_rows(all_resights, br_data)
all_resights <- bind_rows(all_resights, pb_data)
all_resights <- bind_rows(all_resights, fl_data)

# remove uncertain resights
all_resights_clean <- all_resights %>% 
  mutate(ResightCertainty = str_replace(ResightCertainty, "^75% \\(COULD BE TV9\\)\\?$", "75")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^100%$", "100")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^0.95$", "95")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^0.75$", "75")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^NOT 100% SURE$", "50")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^POSSIBLY H$", "50")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^90%$", "90")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^1$", "100")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^0.8$", "80")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^75%$", "75")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^NOT 100% - COULD HAVE BEEN AHU\\?$", "50")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^N VERY FADED$", "50")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^0.5$", "50")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^80%$", "80")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^0.9$", "90")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^COULD BE N9V$", "50")) %>% 
  mutate(ResightCertainty = str_replace(ResightCertainty, "^Y$", "100")) %>% 
  mutate(ResightCertainty = str_replace(ResightCertainty, "^C$", "100")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^\\?$", "50")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^N$", "50")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^L$", "50")) %>% 
  mutate(ResightCertainty = str_replace(ResightCertainty, "^U$", "50")) %>%
  mutate(ResightCertainty = str_replace(ResightCertainty, "^NOT SURE$", "50"))

all_resights_clean$ResightCertainty <- as.numeric(all_resights_clean$ResightCertainty)  

all_resights_clean <- all_resights_clean %>% 
  filter(!is.na(Longitude)) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(!Longitude < (-1000)) %>% 
  mutate(Longitude = ifelse(Longitude > 0 & Latitude > 0, (Longitude*(-1)), Longitude)) %>% 
  filter(!str_detect(FlagCode, "Q")) %>% 
  filter(is.na(ResightCertainty) | ResightCertainty > 94 | ResightCertainty == 100 ) %>% 
  filter(!ProjectID == 31)

all_resights_clean <- all_resights_clean %>% 
  filter(!FlagID %in% c("FEY", "FEBK", "EY", "FEDP", "NA", "CB", "FEGY", "FEW/FY",
                        "FEPU", "ELB", "FY/FELG", "EO", "FLB", "FY/FLG"))

# remove resights with no banding records (except Argentina and Brazil)

# load banding records
banding_records <- read_excel("./data/banding_records.xlsx", sheet = "REKNcaptures")
fl_banding_records <- read_excel("./data/fl_banding_resights.xlsx", sheet = "proj16BandingData")

band_numbers <- banding_records %>% 
  select(MetalID) %>% 
  bind_rows(., fl_banding_records %>% select(MetalID)) %>% 
  distinct() %>% pull()

# separate out brazil and argentina - don't have the banding records for them so keep all
brar_resights <- all_resights_clean %>% 
  filter(FlagID %in% c("FDB", "FEDB", "FO", "FEO"))

other_resights <- all_resights_clean %>% 
  filter(!FlagID %in% c("FDB", "FEDB", "FO", "FEO"))

# remove resights that don't have a banding record
other_resights <- other_resights %>% 
  filter(MetalID %in% band_numbers)

# add brazil and argentina resights back in
all_resights_clean <- bind_rows(other_resights, brar_resights)

# clean up
ls()

rm(list=setdiff(ls(), "all_resights_clean"))

################################################################################

# determine number of birds ever seen in delaware bay
# map data
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
lakes <- ne_load(type = "lakes", scale = "medium", category = "physical",
                 returnclass = "sf",
                 destdir = "./map-data/lakes")

# create sf dataframe of other sites as point locations
db <- st_point(c(-75.145, 39.0583))

db_loc <- st_sfc(db, crs = 4326)

# transform to CRS that uses metres so can add buffers around polygons
db_buf <- st_transform(db_loc, crs = 3395)

# add a 50 km buffer to each polygon
db_buf <- st_buffer(db_buf, dist = 50000)

# transform back to WGS 84 CRS
db_buf <- st_transform(db_buf, crs = 4326)

# add site labels
db_df <- data.frame(site = "DB")

db_buf <- st_sf(db_df, geometry = db_buf)

# filter out public resights
pub_resights <- all_resights_clean %>% 
  filter(ProjectID == 1)

# filter out delaware bay projects resights
dbproj_resights <- all_resights_clean %>% 
  filter(ProjectID %in% c(3, 8))

# filter out public resights within delaware bay
pub_resights_sf <- st_as_sf(pub_resights, coords = c("Longitude", "Latitude"),
                            crs = 4326)

db_pub_subset <- st_intersection(pub_resights_sf, db_buf)

# plot public resights
xmin <- min(pub_resights$Longitude) - 1
xmax <- max(pub_resights$Longitude) + 1
ymin <- min(pub_resights$Latitude) - 1
ymax <- max(pub_resights$Latitude) + 1

xmin <- st_bbox(db_buf)[1] - 0.5
xmax <- st_bbox(db_buf)[3] + 1.5
ymin <- st_bbox(db_buf)[2] - 0.5
ymax <- st_bbox(db_buf)[4] + 0.5

pub_resights_map <- ggplot(data = world) +
  geom_sf(colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_point(data = pub_resights,
             aes(x = Longitude, y = Latitude),
             colour = "#440154FF", alpha = 0.5) +
  geom_sf(data = db_pub_subset,
          aes(colour = "#35B779FF", alpha = 0.5)) +
  geom_sf(data = db_buf, aes(colour = site, fill = site),
          alpha = 0.3) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) #+
#scale_color_viridis(discrete = TRUE, labels = sites) +
#scale_fill_viridis(discrete = TRUE, labels =  sites)

db_pub_subset <- db_pub_subset %>% 
  as.data.frame() %>% 
  select(ProjectID, ResightDate, BirdID)

dbproj_resights <- dbproj_resights %>% 
  select(ProjectID, ResightDate, BirdID)

db_resights <- bind_rows(db_pub_subset, dbproj_resights)

# get number of unique birds
db_birds <- db_resights %>% 
  select(BirdID) %>% 
  distinct() %>% pull()

# load encounter history data
barker_enchist <- readRDS("processed-data/barker-enchist.rds")

db_birds_total <- barker_enchist %>% 
  select(BirdID) %>% 
  filter(BirdID %in% db_birds) %>% 
  distinct()

# percent of birds in study seen in delaware bay at some point
(1898/2276)*100
