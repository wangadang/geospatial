library(dplyr)
library(readr) # for read_csv
library(data.table) # for fread
library(dbplyr)   #database functions
library(RSQLite)  #database backend(?)
library(progress) # to show progress of long queries

setwd("C:/Users/wanderson/Documents/R/eWRIMS_read/")

csv <- "data/ewrims_flat_file_pod.csv"
csv2 <- "data/water_use_report_extended.csv"
csv3 <- "data/ewrims_flat_file_annual_report.csv"
HUC8_CR_4_and_7 <- read.csv("data/HUC8_CR_4_and_7.txt", stringsAsFactors = FALSE)
HUC8_CR_4_and_7 <- unlist(HUC8_CR_4_and_7$HUC_8)

## Read in csv: POD flat file ----------------------------------------------
POD_read <- read_csv(csv,guess_max = 20000) #need guess_max for proper numeric column type spec
selected_columns <- c(1,2,3,6,10,11,12,16,17,23,29,30,42,44,45,46,47,48,51,52,53,54,55,56,57,64,
                      76,77,82,85,86,112,113,116,119,129,130,135,136,138,139)  #n=38 selected of 171
POD_selected_cols <- POD_read[,selected_columns]

# HUC8_unique <- unique(POD_selected_cols$HUC_8_NUMBER)
# HUC8_unique[is.na(HUC8_unique)] <- "NA"

#initialize database with water_use_report_extended format
WURE_format <- fread(csv2,nrows=0, stringsAsFactors = FALSE, select = c(1:6,75,146) , data.table = FALSE)

RMS_HUC8 <- src_sqlite("data/RMS_HUC8_CR47.sqlite", create = FALSE)  #set TRUE if first time run!
for(i in 1:length(unique(HUC8_CR_4_and_7))){
  copy_to(RMS_HUC8,       #loads into database connection "RMS_HUC8"
          WURE_format,
          name=as.character(HUC8_CR_4_and_7[i]),
          temporary=FALSE, overwrite=TRUE)
}
#check DB init
src_tbls(RMS_HUC8)
tbl(RMS_HUC8$con,HUC8_unique[1]) %>% collect() 

# Read in water_use_report_extended by chunks -------------

# need to initialize these prior to loop below
chunks <- 10000   #assume that files are >10000 rows
last.row <- 0
csv_nrows <- 6735288  #hard coded, but could pre-run e.g. with nrow(fread(.,select=[1L]))
count_lines <-0
num_CR_per_chunk <- integer() #diagnostic number of matches for each chunk in nloops

#number of loops
nloops <- ceiling(csv_nrows/chunks)

pb <- progress_bar$new(
  format = "   Processing... [:bar] :percent eta: :eta",
  total = nloops, clear = FALSE, width= 60)

start_time <- Sys.time()
for (i in 1:nloops){ 
  temp <- fread(csv2, skip=(i-1)*chunks+1, nrow=chunks, header=FALSE,
                stringsAsFactors=FALSE, select = c(1:6,75,146),
                data.table=FALSE, blank.lines.skip=TRUE)
  names(temp)<-names(WURE_format)
  temp$HUC8 <- as.character(trunc(temp$HUC_12_NUMBER/10000))
  #numHUC8 <- length(unique(temp$HUC8))
  
  numHUC8_CR <- sum(unique(temp$HUC8) %in% HUC8_CR_4_and_7)
  HUC8_CR_in_temp <- unique(temp$HUC8[temp$HUC8 %in% HUC8_CR_4_and_7])
  #diagnostic
  num_CR_per_chunk[i] <- numHUC8_CR
  
  ## next loop operates on unique HUC8 codes within each chunk
  ## until I can get them all to solve, just doing Cannabis Regions 4 and 7
  if(numHUC8_CR >0) {
    for(j in 1:numHUC8_CR){
      temp_sel <- temp[temp$HUC8 %in% HUC8_CR_4_and_7,]
      #temp_sel <- temp[temp$HUC8==unique(temp$HUC8)[j],]
      count_lines <- count_lines + nrow(temp_sel)
      #ifelse(unique(temp$HUC8)[j] %in% HUC8_unique,  #remember HUC8_unique was initialized from POD file, may not match
        db_insert_into(con = RMS_HUC8$con,       #appends into database connection "RMS_HUC8"
                     table = HUC8_CR_in_temp[j],
                     #table = unique(temp$HUC8)[j],
                     #table = "test",
                     values = temp_sel[,1:8])   #, comma if using ifelse above
        # db_insert_into(con = RMS_HUC8$con,       #for unidentifiable HUC8 values
        #              table = "NA",
        #              #table = "test",
        #              values = temp_sel[,1:8]))
    }}
last.row <- last.row + nrow(temp)    ## increment the current count
pb$tick()
}
print(Sys.time()-start_time)

# for establishing "test" DB table and removing
tbl(RMS_HUC8$con,"test") %>% collect()
dbRemoveTable(RMS_HUC8$con,"test")
copy_to(RMS_HUC8,       #loads into database connection "RMS_HUC8"
        WURE_format,
        name="test",
        temporary=FALSE, overwrite=TRUE)

SF_Eel_RMS <- tbl(RMS_HUC8$con,"18010106") %>% collect()
length(unique(SF_Eel_RMS$APPL_ID))
one_example <-SF_Eel_RMS[SF_Eel_RMS$APPL_ID==unique(SF_Eel_RMS$APPL_ID)[1],]

test <- fread(csv2, nrow=100, header=TRUE,
              stringsAsFactors=FALSE,
              data.table=FALSE, blank.lines.skip=TRUE)
