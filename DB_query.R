library(ggplot2)
library(dplyr)
library(dbplyr)   #database functions
library(RSQLite)  #database backend(?)
library(zoo)  #yearmo data class
install.packages("mapview")
library(sf)  # "simple features" GIS
install.packages("rgdal")
library(rgdal)
library(raster)   #has crs()
library(mapview)

#open database file
RMS_HUC8 <- src_sqlite("data/RMS_HUC8.sqlite", create = FALSE)
#read in RMS data from SQLite db connection for 8-digit HUC
SF_Eel_RMS <- tbl(RMS_HUC8$con,"18010106") %>% collect()
length(unique(SF_Eel_RMS$APPL_ID))   # [1] 593
one_example <-SF_Eel_RMS[SF_Eel_RMS$APPL_ID==unique(SF_Eel_RMS$APPL_ID)[1],]

# NA_tbl <- tbl(RMS_HUC8$con,"NA") %>% collect()
# NA_tbl_uniqueHUC12 <- unique(NA_tbl$HUC_12_NUMBER)

src_tbls(RMS_HUC8) %>% tail()

#brief digression to look at NA records from DB initialization
DB_NA <- tbl(RMS_HUC8$con,"NA") %>% collect()
NA_APPL_ID <- (unique(DB_NA$APPL_ID))  #511 unique APPL_ID's with NA as HUC12
DB_NA_POD <- tbl(RMS_HUC8$con,"POD_sel_cols") %>% filter(APPL_ID %in% NA_APPL_ID) %>% collect()
# only 102 from this filter
DB_NA_POD2 <- tbl(RMS_HUC8$con,"POD_sel_cols") %>% filter(APPLICATION_NUMBER %in% NA_APPL_ID) %>% collect()
# 611 from this filter!! More than 1 POD per is.na(APPL_ID) (511).  Most missing lat/long also. . .

SF_Eel_POD <- tbl(RMS_HUC8$con,"POD_sel_cols") %>% filter(APPL_ID %in% SF_Eel_RMS$APPL_ID) %>% collect()
# 599 PODs for 593 unique APPL_IDs
SF_Eel_coord <- SF_Eel_POD[c("POD_ID","POD_NUMBER","APPL_ID","LATITUDE","LONGITUDE","FACE_VALUE_AMOUNT")]

# Convert POD coordinates to Simple Features (sf package) shape df --------

SF_Eel.point <- st_as_sf(x = SF_Eel_coord, 
                        coords = c("LONGITUDE", "LATITUDE"),
                        crs = "+proj=longlat +datum=WGS84")
mapview(SF_Eel.point, popup = popupTable(SF_Eel.point, zcol = 1:4),map.types="OpenStreetMap", zcol = "FACE_VALUE_AMOUNT")

SF_Eel.point.nonzero <- SF_Eel.point[SF_Eel.point$FACE_VALUE_AMOUNT>0,]

mapview(SF_Eel.point.nonzero, popup = popupTable(SF_Eel.point.nonzero, zcol = 1:4),map.types="OpenStreetMap", zcol = "FACE_VALUE_AMOUNT")

# read in ArcGIS .shp for HUC8s -------------------------------------------

#reading watershed files can be big, subset with query SELECT [columns] FROM [filename] WHERE [column] = [VALUE]
HUC8 <- st_read("GIS/HUC8_all.shp", query = "SELECT * FROM HUC8_all WHERE HUC_8 = '18010106'") 

HUC8 <- st_read("GIS/HUC8_all.shp", query = "SELECT * FROM HUC8_all WHERE HUC_8 LIKE '1801010%'")

crs(HUC8)   #only works for Raster* objects
proj4string(HUC8)    #only works for Raster* objects
st_crs(HUC8)

ggplot()+ geom_sf(data=HUC8,size=1,color="black") + 
  geom_sf(data=SF_Eel.point.nonzero,aes(color="FACE_VALUE_AMOUNT"), size=1) + coord_sf()

mapview(SF_Eel.point.nonzero, popup = popupTable(SF_Eel.point.nonzero, zcol = 1:4), 
        cex="FACE_VALUE_AMOUNT",
        map.types="OpenStreetMap", zcol = "FACE_VALUE_AMOUNT")+ mapview(HUC8, alpha.regions=0.2)

CR <- st_read("GIS/Final_Cannabis_Policy_Regions.shp")
CR <- st_read("GIS/Final_Cannabis_Policy_Regions.shp", 
              query = "SELECT * FROM Final_Cannabis_Policy_Regions
              WHERE Order2 = 4 OR Order2 = 7")
mapview(CR)

#<- read.zoo(text = Lines, FUN = as.yearmon)