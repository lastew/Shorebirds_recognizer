#### Pull habitat variables by station ####

# Here I want to extract locations to run recognizers at
# based on a bounding box
# and habitat variables (LCC)
# and distance to stream (NRCAN topo data)


#### Set up and load data ####

library(tidyr)
library(dplyr)
library(sf)
library(raster)
library(tmap)
library(rasterVis)
library(stars)
library(terra)
library(stringr)
library(tidyverse)
library(sfheaders)

# bbox - bounding box
# sta - ARU locations in spatial data format
# lcc - land cover canada habitat raster


lcc <- raster("../../can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/CAN_NALCMS_landcover_2020_30m.tif")
str(lcc)
lcc.codes <- read.csv("../../can_land_cover_2020_30m_tif/lcc_codes.csv", header = T)
st_crs(lcc) # WGS 84
st_bbox(lcc)

# bounding box: -120 to -110 longitude, 58 to 61.5 latitude

latitude = c(58,58,61.5,61.5,58)
longitude = c(-120,-110,-110,-120,-120)
my.df = data.frame(latitude,longitude)

bbox <- sfheaders::sf_polygon(
  obj = my.df
  , x = "longitude"
  , y = "latitude")

st_crs(bbox) <-  CRS("epsg:4326") # WGS84
bbox = st_transform(bbox,st_crs(lcc))
str(bbox)
st_crs(bbox)

sta=st_read("../../BOSS_ARU_Locations.csv", options=c("X_POSSIBLE_NAMES=longitude","Y_POSSIBLE_NAMES=latitude"))
str(sta)
sta<-sta[!is.na(sta$Latitude) & !is.na(sta$Longitude),]
sta = sta %>% filter(Latitude>58 & Latitude<61.5 & Longitude > -120 & Longitude < -110)
st_crs(sta) <- CRS("epsg:4326") # WGS84
sta = st_transform(sta,st_crs(lcc))

# crop by same bounding box as above
lcc <- raster::crop(lcc,st_bbox(bbox))

# zones <- st_read("../6. Spatial data/Ecozones/Ecoregions")
# st_crs(zones)  # NAD83
# head(zones)
# zones = zones  %>% dplyr::select(REGION_NAM,geometry)

#### Map it to make sure it looks ok ####

fr <- sta%>% filter(ProjectID=="FR")
lcc_tmp <- raster::crop(lcc,st_bbox(fr))

tm_shape(fr) + # check that it looks they way I want it to
  tm_dots() +
  tm_shape(lcc_tmp) + tm_raster()



#### Extract station-level land cover information ####

sta_lcc <- raster::extract(lcc, sta) 
summary(sta_lcc)
sta$value <- sta_lcc
