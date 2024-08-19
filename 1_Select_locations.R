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

# bbox - bounding box
# sta - ARU locations in spatial data format
# lcc - land cover canada habitat raster


# bounding box: -120 to -110 longitude, 58 to 61.5 latitude
bbox <- st_polygon(list(cbind(c(-120,-110,-110,-120,-120), c(58,58,61.5,61.5,58))))
crs(bbox)
bbox = st_sfc(bbox,crs=4326) # WGS84


sta=st_read("../../BOSS_ARU_Locations.csv", options=c("X_POSSIBLE_NAMES=longitude","Y_POSSIBLE_NAMES=latitude"))
str(sta)
sta<-sta[!is.na(sta$Latitude) & !is.na(sta$Longitude),]
st_crs(sta) <- CRS("+init=epsg:4326") # WGS84
sta = sta %>% filter(Latitude>58 & Latitude<61.5 & Longitude > -120 & Longitude < -110)


lcc <- raster("../../can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/CAN_NALCMS_landcover_2020_30m.tif")
str(lcc)
lcc.codes <- read.csv("../../can_land_cover_2020_30m_tif/lcc_codes.csv", header = T)
st_crs(lcc) # WGS 84 
# crop by same bounding box as above
lcc <- raster::crop(lcc,st_bbox(bbox))




# zones <- st_read("../6. Spatial data/Ecozones/Ecoregions")
# st_crs(zones)  # NAD83
# head(zones)
# zones = zones  %>% dplyr::select(REGION_NAM,geometry)

#### Map it to make sure it looks ok ####




#### Extract station-level ecoregion information ####

sta <- st_transform(sta, st_crs(zones))
stazones <- st_intersection(sta,zones)
head(stazones)
unique(stazones$REGION_NAM)
abbr <- tibble(REGION_NAM = c("Peel River Plateau", "Mackenzie Mountains","Fort MacPherson Plain","Mackenzie River Plain"),
               zabbr = c("PRP","MMO","FMP","MRP"))
stazones = merge(stazones,abbr,by="REGION_NAM")
stazones = stazones %>% dplyr::select(location,REGION_NAM,zabbr)

