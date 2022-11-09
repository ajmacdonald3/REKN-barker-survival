################################################################################
# PREPARE BARKER ENCOUNTER HISTORIES
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

# remove/correct manually identified suspect detections
unique(all_resights_clean$FlagID)

suspect_resights <- all_resights_clean %>%
  filter(FlagID %in% c("FEY", "FEBK", "EY", "FEDP", "NA", "CB", "FEGY", "FEW/FY",
                       "FEPU", "ELB", "FY/FELG", "EO", "FLB", "FY/FLG"))

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

# separate out cleaned james bay resights
jb_resights <- all_resights_clean %>% 
  filter(ProjectID == 22)

# separate out brazil and argentina - don't have the banding records for them so keep all
brar_resights <- jb_resights %>% 
  filter(FlagID %in% c("FDB", "FEDB", "FO", "FEO"))

other_resights <- jb_resights %>% 
  filter(!FlagID %in% c("FDB", "FEDB", "FO", "FEO"))

# remove resights that don't have a banding record
other_resights <- other_resights %>% 
  filter(MetalID %in% band_numbers)

# add brazil and argentina resights back in
jb_resights_clean <- bind_rows(other_resights, brar_resights)

# start filtering process
# identify first james bay resight of all birds ever seen in james bay
jb_first <- jb_resights_clean %>% 
  group_by(BirdID) %>% 
  arrange(ResightDate) %>% 
  slice(1L) %>% 
  select(BirdID, ResightDate) %>% 
  rename(FirstResight = ResightDate) %>% 
  ungroup()

jb_ids <- jb_first %>% 
  select(BirdID) %>% pull()

# filter out all birds never resighted in james bay and any resights before first sighting in james bay
barker_resights <- all_resights_clean %>% 
  filter(BirdID %in% jb_ids)

barker_resights <- left_join(barker_resights, jb_first, by = "BirdID")

barker_resights <- barker_resights %>% 
  filter(!ResightDate < FirstResight) %>% 
  filter(!ResightDate > "2019-09-03") %>% 
  mutate(ProjectID = case_when(ProjectID == 1 & LocationID == "LONGRIDGEPT" ~ 22,
                               ProjectID == 1 & LocationID == "LONGRIDGERVR" ~ 22,
                               TRUE ~ ProjectID))

location_resights <- barker_resights %>% 
  select(ProjectID, ResightDate, BirdID, Latitude, Longitude)

# export dataframe so can make study site map
saveRDS(location_resights, "./processed-data/location-resights.rds")

# leaflet interactive map
pal_fun <- colorFactor(viridis(17), unique(barker_resights$ProjectID))

int_resight_map <- leaflet(barker_resights) %>% 
  addCircleMarkers(data = barker_resights,
                   lng = ~Longitude, lat = ~Latitude,
                   radius = 3,
                   color = ~pal_fun(ProjectID),
                   stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}') %>% 
  addLegend("bottomright",
            colors = viridis(17),
            labels = unique(barker_resights$ProjectID))

htmlwidgets::saveWidget(int_resight_map, file = "./figures/test_resight_map.html")

# format CC data
cc_data$ResightDate <- mdy(cc_data$ResightDate)

cc_resights <- cc_data %>% 
  select(ProjectID, ResightDate, BirdID) %>% 
  filter(BirdID %in% jb_ids) %>% 
  left_join(., jb_first, by = "BirdID") %>% 
  filter(!ResightDate < FirstResight) %>% 
  select(-FirstResight)

barker_resights <- barker_resights %>% 
  select(ProjectID, ResightDate, BirdID) %>% 
  bind_rows(., cc_resights)

# summarize resights by project
project_resights <- barker_resights %>% 
  group_by(ProjectID) %>% 
  summarize(n = n())

writexl::write_xlsx(project_resights, path = "./processed-data/resights-by-project-updated.xlsx")

barker_resights_state <- barker_resights %>% 
  mutate(ProjectID = ifelse(ProjectID == 22, 2, 3)) %>% 
  rename(ObsState = ProjectID)

# set and assign periods
set_periods <- tibble(Period = c(1:11),
                      Start = c("2009-07-01",
                                "2009-09-04",
                                "2010-09-04",
                                "2011-09-04",
                                "2012-09-04",
                                "2013-09-04",
                                "2014-09-04",
                                "2015-09-04",
                                "2016-09-04",
                                "2017-09-04",
                                "2018-09-04"),
                      End = c("2009-09-03",
                              "2010-09-03",
                              "2011-09-03",
                              "2012-09-03",
                              "2013-09-03",
                              "2014-09-03",
                              "2015-09-03",
                              "2016-09-03",
                              "2017-09-03",
                              "2018-09-03",
                              "2019-09-03"))

set_periods$Start <- as_date(set_periods$Start)
set_periods$End <- as_date(set_periods$End)

barker_resights_state <- fuzzy_left_join(barker_resights_state, set_periods,
                                         by = c("ResightDate" = "Start",
                                                "ResightDate" = "End"),
                                         match_fun = list(`>=`, `<=`))

barker_resights_eh <- barker_resights_state %>% 
  select(Period, BirdID, ObsState) %>% 
  distinct()

# calculate number of obs in jb
length(unique(barker_resights_eh$BirdID))

jb_obs <- barker_resights_eh %>% 
  filter(ObsState == 2) %>% 
  distinct()

# number of obs outside jb
out_obs <- barker_resights_eh %>% 
  filter(ObsState == 3) %>% 
  distinct()

# pull out birds detected in >1 state in a period
check <- barker_resights_eh %>%
  group_by(Period, BirdID) %>%
  count() %>% filter(n > 1) %>% 
  ungroup()

# for birds seen in and out of james bay, reassign obs state from 2 and 3 to 1
barker_resights_eh <- barker_resights_eh %>% 
  group_by(Period, BirdID) %>% 
  mutate(n = length(unique(ObsState))) %>% 
  mutate(ObsState = ifelse(n == 2, 1, ObsState)) %>% 
  select(Period, BirdID, ObsState) %>% 
  distinct() %>% 
  ungroup()

# create encounter history
barker_enchist <- barker_resights_eh %>% 
  distinct() %>% 
  arrange(Period) %>% 
  tidyr::pivot_wider(id_cols = BirdID, names_from = Period, values_from = ObsState,
                     values_fill = 0)

enchist_ids <- barker_enchist %>% 
  select(BirdID)

# compute date of first capture
get.first <- function(x) min(which(x!=0))

barker_enchist <- barker_enchist %>% 
  select(-BirdID) %>% 
  mutate(first = apply(., 1, get.first)) %>% 
  bind_cols(enchist_ids, .) %>% 
  filter(!first == 11) %>% 
  select(-first)

# update obs in and out of jb
enchist_ids <- barker_enchist %>% 
  select(BirdID) %>% 
  pull()

jb_obs <- jb_obs %>% 
  filter(BirdID %in% enchist_ids)

out_obs <- out_obs %>% 
  filter((BirdID %in% enchist_ids))

# export encounter histories
write.csv(barker_enchist, file = "./processed-data/barker-enchist.csv", row.names = FALSE)
saveRDS(barker_enchist, file = "./processed-data/barker-enchist.rds")

# summarize resights by project
resights_project <- barker_resights %>% 
  group_by(ProjectID) %>% 
  summarize(n = n())

write.csv(resights_project, file = "./processed-data/barker-projects.csv", row.names = FALSE)

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
pub_resights <- location_resights %>% 
  filter(ProjectID == 1)

# filter out delaware bay projects resights
dbproj_resights <- location_resights %>% 
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

db_birds_total <- barker_enchist %>% 
  select(BirdID) %>% 
  filter(BirdID %in% db_birds)

# percent of birds in study seen in delaware bay at some point during study
(1361/2276)*100

# map jb resights
windowsFonts(Times=windowsFont("TT Times New Roman"))

location_jb_resights <- location_resights %>% 
  filter(ProjectID == 22)

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

jb_resights_map <- ggplot(data = jb_map) +
  geom_sf(colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_point(data = location_jb_resights,
             aes(x = Longitude, y = Latitude),
             colour = "black", alpha = 0.3, size = 1, shape = 17) +
  geom_text(data=annotation, aes(x=x, y=y, label=label), family = "Times", size = 2) +
  annotation_scale(location = "bl", line_width = 0.25,
                   text_cex = 0.5, height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(0.75, "cm"), width = unit(0.75, "cm")) +
  coord_sf(xlim = c(xmin_jb, xmax_jb), ylim = c(ymin_jb, ymax_jb), expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"))

# map all included resights
xmin <- min(location_resights$Longitude) - 15
xmax <- max(location_resights$Longitude) + 5
ymin <- min(location_resights$Latitude) - 5
ymax <- max(location_resights$Latitude) + 20

resights_map <- ggplot(data = world) +
  geom_sf(colour = NA) +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_point(data = location_resights,
             aes(x = Longitude, y = Latitude, shape = as.logical(ProjectID == 22)),
             colour = "black", alpha = 0.3, size = 1) +
  geom_rect(aes(xmin = xmin_jb, xmax = xmax_jb, ymin = ymin_jb, ymax = ymax_jb),
            fill = NA, colour = "black", size = 0.5) +
  annotation_scale(location = "bl", line_width = 0.25,
                   text_cex = 0.5, height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(0.75, "cm"), width = unit(0.75, "cm")) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        legend.position = "none")

png(filename = "./figures/study-sites-map.png", height = 4, width = 6,
    units = "in", res = 600)

print(resights_map + jb_resights_map +
        plot_annotation(tag_levels = 'a'))

dev.off()

# pull out list of BirdIDs
barker_enchist <- readRDS("./processed-data/barker-enchist.rds")

bird_ids <- barker_enchist %>% 
  select(BirdID) %>% 
  distinct() %>% arrange(BirdID)

write.csv(bird_ids, file = "./processed-data/macdonald-birdid-list.csv", row.names = FALSE)

################################################################################

# summarize number of observations per year by flag colour

# filter out all birds never resighted in james bay and any resights before first sighting in james bay
barker_resights <- all_resights_clean %>% 
  filter(BirdID %in% jb_ids)

barker_resights <- left_join(barker_resights, jb_first, by = "BirdID")

barker_resights <- barker_resights %>% 
  filter(!ResightDate < FirstResight) %>% 
  filter(!ResightDate > "2019-09-03") %>% 
  mutate(ProjectID = case_when(ProjectID == 1 & LocationID == "LONGRIDGEPT" ~ 22,
                               ProjectID == 1 & LocationID == "LONGRIDGERVR" ~ 22,
                               TRUE ~ ProjectID))

cc_resights <- cc_data %>% 
  select(ProjectID, ResightDate, FlagID, BirdID) %>% 
  filter(BirdID %in% jb_ids) %>% 
  left_join(., jb_first, by = "BirdID") %>% 
  filter(!ResightDate < FirstResight) %>% 
  select(-FirstResight)

barker_resights <- barker_resights %>% 
  select(ProjectID, ResightDate, FlagID, BirdID) %>% 
  bind_rows(., cc_resights)

barker_resights_state <- barker_resights %>% 
  mutate(ProjectID = ifelse(ProjectID == 22, "JB", "Out")) %>% 
  rename(ObsState = ProjectID)

# set and assign periods
set_periods <- tibble(Period = c(1:11),
                      Start = c("2009-07-01",
                                "2009-09-04",
                                "2010-09-04",
                                "2011-09-04",
                                "2012-09-04",
                                "2013-09-04",
                                "2014-09-04",
                                "2015-09-04",
                                "2016-09-04",
                                "2017-09-04",
                                "2018-09-04"),
                      End = c("2009-09-03",
                              "2010-09-03",
                              "2011-09-03",
                              "2012-09-03",
                              "2013-09-03",
                              "2014-09-03",
                              "2015-09-03",
                              "2016-09-03",
                              "2017-09-03",
                              "2018-09-03",
                              "2019-09-03"))

set_periods$Start <- as_date(set_periods$Start)
set_periods$End <- as_date(set_periods$End)

barker_resights_state <- fuzzy_left_join(barker_resights_state, set_periods,
                                         by = c("ResightDate" = "Start",
                                                "ResightDate" = "End"),
                                         match_fun = list(`>=`, `<=`))

bb_resights <- barker_resights_state %>% 
  select(Period, FlagID, BirdID, ObsState) %>% 
  filter(BirdID %in% enchist_ids) %>% 
  distinct()


bb_resights <- bb_resights %>% 
  mutate(FlagID = replace(FlagID, str_detect(FlagID, "FELG"), "FLG")) %>% 
  mutate(FlagID = replace(FlagID, str_detect(FlagID, "FEDG"), "FDG")) %>% 
  mutate(FlagID = replace(FlagID, str_detect(FlagID, "FEDB"), "FDB")) %>% 
  mutate(FlagID = replace(FlagID, str_detect(FlagID, "FEO"), "FO")) %>% 
  mutate(FlagID = replace(FlagID, str_detect(FlagID, "FEW"), "FW")) %>% 
  mutate(FlagID = replace(FlagID, str_detect(FlagID, "FER"), "FR")) %>% 
  distinct()

# james bay
jb_resights_summary <- bb_resights %>% 
  filter(ObsState == "JB") %>% 
  group_by(Period, FlagID) %>% 
  tally()

jb_year_summary <- jb_resights_summary %>% 
  group_by(Period) %>% 
  summarize(n = sum(n))

jb_flag_summary <- jb_resights_summary %>% 
  group_by(FlagID) %>% 
  summarize(n = sum(n))

jb_resights_summary_wide <- jb_resights_summary %>% 
  pivot_wider(names_from = Period, values_from = n, values_fill = 0)

# outside
out_resights_summary <- bb_resights %>% 
  filter(ObsState == "Out") %>% 
  group_by(Period, FlagID) %>% 
  tally()

out_year_summary <- out_resights_summary %>% 
  group_by(Period) %>% 
  summarize(n = sum(n))

out_flag_summary <- out_resights_summary %>% 
  group_by(FlagID) %>% 
  summarize(n = sum(n))

out_resights_summary_wide <- out_resights_summary %>% 
  pivot_wider(names_from = Period, values_from = n, values_fill = 0)
