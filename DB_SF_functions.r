library(ggplot2)
library(dplyr)
library(dbplyr)   #database functions
library(RSQLite)  #database backend(?)
library(sf)  # "simple features" GIS
library(mapview)
library(purrr)   #map() 
library(dataRetrieval)
#library(zoo)  #yearmo data class
#install.packages("mapview")
#install.packages("rgdal")
#library(rgdal)
#library(raster)   #has crs()

#open database connection
RMS_HUC8_db <- src_sqlite("data/RMS_HUC8.sqlite", create = FALSE)
#read in RMS data from SQLite db connection for 8-digit HUC
read_HUC8 <- function(x) tbl(RMS_HUC8_db$con,x) %>% collect() 

# first example for just 2 HUCs ---------------------------

# run read function for multiple 8-digit values and combine
RMS_output <- map(c("18010106","18010105"),read_HUC8)  #creates list
RMS_output <- do.call(rbind, RMS_output)  #rbinds list to one data frame

#query list of PODs in DB to match water rights in RMS query
PODs <- tbl(RMS_HUC8_db$con,"POD_sel_cols") %>% filter(APPL_ID %in% RMS_output$APPL_ID) %>% collect()

#whittle down POD attributes
PODs_coord <- PODs[c("POD_ID","POD_NUMBER","APPL_ID","LATITUDE","LONGITUDE","FACE_VALUE_AMOUNT","PRIMARY_OWNER_NAME")]

# Convert POD coordinates to Simple Features (sf package) shape df --------
PODs.point <- st_as_sf(x = PODs_coord, 
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = "+proj=longlat +datum=WGS84")
st_crs(PODs.point)  #confirm coordinate reference system (CRS)

mapview(PODs.point, popup = popupTable(PODs.point, zcol = 1:6),map.types="OpenStreetMap", zcol = "FACE_VALUE_AMOUNT")
#these are illustrated by FACE_VALUE_AMOUNT but Statements have zero face value. . . 

#note that subsetting works on simple feature exactly like data frame
PODs.point.nonzero <- PODs.point[PODs.point$FACE_VALUE_AMOUNT>0,]

mapview(PODs.point.nonzero, popup = popupTable(PODs.point.nonzero, zcol = 1:6), 
        cex="FACE_VALUE_AMOUNT", zcol = "FACE_VALUE_AMOUNT")

# Next section queries for Cannabis Regions 4&7 ---------------------------

CR <- st_read("GIS/Final_Cannabis_Policy_Regions.shp", 
              query = "SELECT * FROM Final_Cannabis_Policy_Regions
              WHERE Order2 = 4 OR Order2 = 7")
mapview(CR)

# Load HUC8's, calc centroids, intersect CRs ------------------------------

HUC8 <- st_read("GIS/HUC8_all.shp")
#HUC8_CR <- st_intersection(HUC8,CR) # creates extraneous edge issues
#mapview(CR) + mapview(HUC8_CR)  #fails due to the above

HUC8_centroids <- st_centroid(HUC8)
# ggplot()+ geom_sf(data=HUC8,size=1,color="black") + 
#   geom_sf(data=HUC8_centroids,aes(color="black"), size=1) + coord_sf()
HUC8_centroids_CR <- st_intersection(HUC8_centroids,CR)
#result is list of HUC8 centroids that are within Cannabis Regions (#4 and #7)
HUC8_in_CR <- as.character(HUC8_centroids_CR$HUC_8)

# Run DB query with HUC8 list ---------------------------------------------
RMS_output <- map(HUC8_in_CR,read_HUC8)  #creates list
RMS_output <- do.call(rbind, RMS_output)  #rbinds list to one data frame

#query list of PODs in DB to match water rights in RMS query
# Note this could take a couple minutes for RMS >1M!
#PODs <- tbl(RMS_HUC8_db$con,"POD_sel_cols") %>% filter(APPL_ID %in% RMS_output$APPL_ID) %>% collect()

HUC8_in_CR <- as.integer(HUC8_in_CR) # for filter below
#this goes a lot faster!
PODs <- tbl(RMS_HUC8_db$con,"POD_sel_cols") %>% filter(HUC_8_NUMBER %in% HUC8_in_CR) %>% collect()

#whittle down POD attributes
PODs_coord <- PODs[c("POD_ID","POD_NUMBER","APPL_ID","LATITUDE","LONGITUDE","FACE_VALUE_AMOUNT","PRIMARY_OWNER_NAME","USE_CODE")]

# Convert POD coordinates to Simple Features (sf package) shape df --------
PODs.point <- st_as_sf(x = PODs_coord, 
                       coords = c("LONGITUDE", "LATITUDE"),
                       crs = "+proj=longlat +datum=WGS84")

mapview(PODs.point, popup = popupTable(PODs.point, zcol = 1:6),map.types="OpenStreetMap", zcol = "FACE_VALUE_AMOUNT", 
        cex="FACE_VALUE_AMOUNT") + mapview(CR, alpha.regions=0.05) 

# some manipulations to get sizes to view better
breaks = c(0,1,10,100,1000,10000,100000,1000000)
PODs.point$size <-  breaks[cut(PODs.point$FACE_VALUE_AMOUNT, breaks = breaks,right=TRUE)]
                        #labels = c("<1","1-10","10-100","100-1,000","1,000-10,000","10,000-100,000","100,000+"))
#unique(PODs.point$size)  # use to test size classes and breaks 
# i.e. Levels: (0,1] (1,10] (10,100] (100,1e+03] (1e+03,1e+04] (1e+04,1e+05]

mapview(PODs.point, popup = popupTable(PODs.point, zcol = 1:6),map.types="OpenStreetMap", zcol = "size", 
        cex="size", alpha=0.3) 

# Pull in gage list and USGS data for CR 4&7 ------------------------------

GageList <- read.csv("../CRIPU_gage_scraping/data/GageList.csv") 
gages <- as.character(GageList$GAGENO)
gages[1] <- paste0("0",gages[1])

#download USGS metadata for sites (lat/long etc.)
gages_info <- readNWISsite(siteNumbers=gages)

#make point shapefile for gages
gages.point <- st_as_sf(x = gages_info, 
                        coords = c("dec_long_va", "dec_lat_va"),
                        crs = "+proj=longlat +datum=NAD83")     #note datum for USGS!! not WGS84
#gages.point.CR <- st_intersection(gages.point,CR) #fails! need to transform CRS to match
st_crs(gages.point)
st_crs(CR)
CR_proj <- st_transform(CR,4269, check=TRUE)   #reproject
st_crs(CR_proj)
gages.point.CR <- st_intersection(gages.point,CR_proj)

ggplot()+ geom_sf(data=CR_proj,size=1,color="black") + 
  geom_sf(data=gages.point.CR,size=2,fill="blue",shape=22) + coord_sf()

which(names(gages.point.CR)=="drain_area_va")
mapview(gages.point.CR,
        popup = popupTable(gages.point.CR, zcol = c(2:3,28)),map.types="OpenStreetMap") +
  mapview(CR_proj, alpha.regions=0.05)

gages_info[gages_info$site_no==11475800,][1:8]

# Read in NLDI drainage basin for USGS gage -------------------------------

nldi_sf <- read_sf("https://cida.usgs.gov/nldi/nwissite/USGS-11475800/basin")
gages.11475800 <- gages.point.CR[gages.point.CR$site_no==11475800,]

streams <- st_read("../GIS/rf1_CR4_7.shp")

ggplot() + geom_sf(data=nldi_sf,size=1,color="black") +
  geom_sf(data=gages.point.CR,size=3,fill="blue",shape=22) + 
  geom_sf(data=streams, size=1,color="blue") +
  coord_sf(xlim=c(-123.2,-123.8),ylim=c(39.5,40.0)) +
  geom_sf_label(mapping = aes(label=gages.11475800$site_no), data = gages.11475800, 
                nudge_x = 0.065, nudge_y = 0.02)

# # locate points within gage watershed -----------------------------------

PODs.11475800 <- st_intersection(PODs.point,nldi_sf)
sum(PODs.11475800$FACE_VALUE_AMOUNT)

mapview(list(streams,gages.11475800,PODs.11475800,nldi_sf),map.types="OpenStreetMap",
        alpha.regions=0.05, cex="size")
