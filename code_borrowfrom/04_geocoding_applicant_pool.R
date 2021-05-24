
#######################################
# Imports and functions
#######################################

rm(list = ls())

library(dplyr)
library(readr)
library(janitor)
library(readxl)
library(tidygeocoder)
library(sf)
library(sp)
library(geosphere)
library(ggplot2)
library(ggmap)
library(rgdal)
library(broom)
library(tigris)

source("02_Code/01_cross_city_code/01_helpful_functions.R")

df <- readRDS(get_path_to_most_recent(file_path = get_path("/Shared drives/OES data 2003/Clean_Data/"),
                                            extension = ".RDS", 
                                            name = "Final_Award_Data_San_Diego_cleaned")) 

sprintf("There are %s rows and %s unique submittal numbers",
        nrow(df),
        length(unique(df$edd_submission_number)))

#######################################
# Geocode business addresses
#######################################

# Set to true if you want to redo all the geocoding. Warning: takes about 5 hours.
RUN_BIZLOCATION_GEOCODE <- FALSE

if(RUN_BIZLOCATION_GEOCODE){

geocoded <- 
  with(df[1,],
       geo_osm(street = street_clean,
               state = state_clean, 
               postalcode = zip_clean))

geocoded <- cbind(submittal_number = df[1,]$submittal_number,geocoded)

# for(i in uncoded_businesses_indices){ <- had to go back and code some that didn't make it in during first loop
for(i in (nrow(geocoded) +1 ):nrow(df)){
  print(paste0("Geocoding row ",i, " of ",nrow(df)))
  # First try Census
  # Then try OSM 
  # And append if both don't error
  
  new_row <- try(expr = {
    with(df[i,],
         geo_census(street = street_clean,
                 state = state_clean, 
                 postalcode = zip_clean))},
    silent = TRUE)
  
  did_error <- identical(class(new_row), "try-error")
  
  no_match <- TRUE
  
  if(!did_error) no_match <- new_row %>% pull(lat) %>% is.na()
  
  if(did_error | no_match){
    new_row <- try(expr = {
      with(df[i,],
           geo_osm(street = street_clean,
                   state = state_clean, 
                   postalcode = zip_clean))
    },
    silent = TRUE)
  }
  
  did_error <- identical(class(new_row), "try-error")
  
  if(did_error) {
    new_row <-  with(df[i,],
                     data.frame(
                       street = street_clean,
                             state = state_clean, 
                             postalcode = zip_clean, 
                             lat = NA, long = NA))
  }
  
  
  new_row <- cbind(submittal_number = df[i,]$submittal_number,new_row)
  geocoded <- rbind(geocoded,new_row)
}

# add 51 and check for other ones missing

# uncoded_businesses <- setdiff(df$submittal_number, geocoded$submittal_number)
# uncoded_businesses_indices <- which(df$submittal_number %in% uncoded_businesses)
# setdiff(geocoded$submittal_number, df$submittal_number)


# Check and remove duplicates ---------------------------------------------
# Sometimes the code returns multiple coordinates, eg.:
# geo_census(street = "po box 329 san luis rey",state = "CA", postalcode = "92054")

geocoded <- geocoded %>% arrange(submittal_number)
dup_numbers <- geocoded$submittal_number[duplicated(geocoded$submittal_number)]
geocoded %>% filter(submittal_number %in% dup_numbers)

# For now, keeping first record only
geo_no_dups <- geocoded %>% filter(!duplicated(submittal_number))
nrow(geo_no_dups)


# Code remaining NAs ------------------------------------------------------

# Stole this from python scripts :/
api_key <- "AIzaSyDR1BD17-hlTMMyp7U2Uft-ENRrfGMu4jg"

register_google(key = api_key)

missing_latlons <- geo_no_dups %>% filter(is.na(lat)|is.na(long))

google_geocodes <- with(missing_latlons, 
                        ggmap::geocode(location = paste0(street, ", ", state, ", ", postalcode)))

missing_latlons$long <- google_geocodes$lon
missing_latlons$lat <- google_geocodes$lat

geo_full <- 
  rbind(
    geo_no_dups %>% filter(!is.na(lat) & !is.na(long)),
    missing_latlons
  )

write.csv(geo_full, get_path("/Shared drives/OES data 2003/Int_Objects/Geocoded_Applicant_Data_San_Diego.csv"),row.names = FALSE, na = "")

} else {
  
  
  # Load geocoded application data
  geo_full <- read.csv(get_path("/Shared drives/OES data 2003/Int_Objects/Geocoded_Applicant_Data_San_Diego.csv"))
  
}

#######################################
# Geocode or load majority biz owner
# address
#######################################

## rj note - for time reasons, only did with non-duplicate
## and non owner addresses, so will be missing for ones outside
df_ownergeocode <- df %>%
  filter((keep == 1 | is.na(keep)) & !is.na(derived_majowner_street_clean) &
           !derived_majowner_pobox) %>%
  select(edd_submission_number, 
         contains("derived_majowner")) 

## shuffle order
df_ownergeocode <- df_ownergeocode %>%
  sample_n(nrow(df_ownergeocode))

## set parameter for if geocoding owner address
RUN_OWNERLOCATION_GEOCODE = FALSE 

## split df into chunks and call/write in chunks of 100
## to minimize loss from failure
if(RUN_OWNERLOCATION_GEOCODE){
  split_tobatch = split(df_ownergeocode, (as.numeric(rownames(df_ownergeocode))-1) %/% 100)
  
  ## loop through batch, geocode and write
  for(i in 1:length(split_tobatch)){
    
    ## one batch
    one_batch = split_tobatch[[i]]
    
    ## geocode that batch
    one_batch_ret <- cxy_geocode(one_batch, street = "derived_majowner_street_clean", 
                                 city = "derived_majowner_city", 
                                 state = "derived_majowner_state", output = "full", class = "sf")
    
    ## save
    saveRDS(one_batch_ret,
            sprintf("%s/owner_geocode_batch_%s.rds",
                    get_path("/Shared drives/OES data 2003/Int_Objects/geocode_address_int"),
                    i),
            compress = FALSE)
    print(sprintf("saved geocode for batch: %s", i))
    
  }
  
} else{
  
  ## read in files 
  all_batch_list = lapply(list.files(get_path("/Shared drives/OES data 2003/Int_Objects/geocode_address_int"),
                                     full.names = TRUE), function(x) readRDS(x))
  all_batch_df = do.call(rbind.data.frame, all_batch_list)
  
}


#######################################
# Download or load two sets of 
# shapefiles
#######################################

#######################################
# Set 1: CA blocks
#######################################

## flag whether to download blocks from tigris
## or to load pre-downloaded blocks
DOWNLOAD_BLOCKS <- FALSE
if(DOWNLOAD_BLOCKS){
  ca_blocks = blocks(state = "CA")
  st_write(ca_blocks, sprintf("%s%s",
                    get_path("/Shared drives/OES data 2003/Raw_Data/Other_geo_files/"),
                    "ca_blocks.shp"))
} else{
  ca_blocks = st_read(sprintf("%s%s", get_path("/Shared drives/OES data 2003/Raw_Data/Other_geo_files/"),
                      "ca_blocks.shp"))
}


## align crs and get intersection of owner address with blocks
owner_int_blocks <- st_intersection(st_transform(all_batch_df,
                  crs = st_crs(ca_blocks)), ca_blocks)

## get submission matching to multiple blocks
multmatch_owner <- owner_int_blocks %>%
  group_by(edd_submission_number) %>%
  filter(n() > 1) %>%
  distinct() %>%
  pull(edd_submission_number)

## align crs and get intersection of biz location address with blocks
bizlocation_int_blocks <- st_intersection(st_transform(geos_sf,
                  crs = st_crs(ca_blocks)), ca_blocks) %>%
  rename(derived_bizlocation_blockid = GEOID10,
         derived_bizlocation_statefp = STATEFP,
         derived_bizlocation_countyfp = COUNTYFP,
         derived_bizlocation_tractfp = TRACTCE10) %>%
  select(-MTFCC10, -NAME10, -contains("UA"),
         -contains("10"))

multmatch_location = bizlocation_int_blocks %>%
      group_by(submittal_number) %>%
      filter(n() > 1) %>%
      distinct() %>%
      pull(submittal_number)

## clean and join to main data-- esp for owner location
## some fields will be missing in left join if we excluded
## from geocoding
## step 1: join owner location blocks
df_wownergeo = merge(df %>% select(edd_submission_number, keep,
                                              owner1_name,
                                              owner2,
                                              derived_which_owner_majority),
                        owner_int_blocks %>% select(-contains("derived_majowner")),
                        by = "edd_submission_number",
                        all.x = TRUE) %>%
  mutate(derived_majowner_geocode_category = 
           case_when(edd_submission_number %in% df_ownergeocode$edd_submission_number &
                       is.na(GEOID10) ~ "Tried geocode; fail",
                     edd_submission_number %in% df_ownergeocode$edd_submission_number ~ "Tried geocode; succeed",
                     TRUE ~ "Filtered out pre-geocode (po box; missing add)")) %>%
  rename(derived_majowner_blockid = GEOID10,
         derived_majowner_statefp = STATEFP,
         derived_majowner_countyfp = COUNTYFP,
         derived_majowner_tractfp = TRACTCE10) %>%
  select(-MTFCC10, -NAME10, -contains("UA"),
         -contains("10")) %>%
  mutate(is_duplicated_majownerblock = ifelse(edd_submission_number %in% 
                                          multmatch_owner, 
                                          TRUE, FALSE))

## step 2: join biz location blocks
## nrow increases since a few locations match to
## multiple blocks
df_bothblocks = merge(df_wownergeo,
                      bizlocation_int_blocks,
                      by.x = "edd_submission_number",
                      by.y = "submittal_number",
                      suffixes = c("_forbizowner", "_forbizlocation"),
                      all.x = TRUE) %>%
            mutate(is_duplicated_bizlocationblock = ifelse(edd_submission_number %in% 
                    multmatch_location, TRUE, FALSE)) 


#######################################
# Set 2: sd council boundaries
#######################################


# Load boundaries of San Diego Council
cncl <- st_read(get_path("/Shared drives/OES data 2003/Raw_Data/Council_Districts_Map/council_districts_datasd.shp"))

# Set coordinate projection
cncl_st <- st_transform(cncl, "+proj=longlat +datum=WGS84")

# Turn geocoded dataset into a spatial object
geos_sf <- st_as_sf(
  x = geo_full,  
  crs = "+proj=longlat +datum=WGS84",
  coords = c("long", "lat"))

# Intersect the council and geocoded data
cncl_geo <- st_intersection(cncl_st, geos_sf) %>% 
  data.frame()

# Define businesses that intersect with the council boundaries 
# as in San Diego 
geo_full <- 
  geo_full %>% 
  mutate(in_SD = (submittal_number %in% cncl_geo$submittal_number))

# Join up main df with council boundaries df
df_wsd <- left_join(df, geo_full %>% select(lat, long, in_SD, submittal_number), 
            by = c("edd_submission_number" = "submittal_number"))

# Code a new variable 
# rj note - code had "amount" which i think
# was from older df
# two options: (1) award_amount or (2) derived_amount
# used derived_amount here
df_wsd <- df_wsd %>% 
  mutate(
    status = case_when(
      derived_amount > 0 & in_SD ~ "In SD, Funded",
      derived_amount == 0 & in_SD ~ "In SD, Unfunded",
      derived_amount > 0 & !in_SD ~ "Outside SD, Funded",
      derived_amount == 0 & !in_SD ~ "Outside SD, Unfunded"
    )
  )


ggplot() +
  geom_sf(data = cncl_st) +
  geom_point(data = df_wsd %>% filter(!is.na(long)),
             aes(x =long, y =lat, color = status)) +
  coord_sf(xlim = c(-117.5,-116.8), ylim = c(32.5,33.2)) +
  theme_bw()


ggplot() +
  geom_sf(data = cncl_st) +
  geom_point(data = df_wsd %>% filter(!is.na(long)) %>% 
               filter(submitted_time < median(submitted_time)),
             aes(x =long, y =lat, color = status)) +
  coord_sf(xlim = c(-117.5,-116.8), ylim = c(32.5,33.2)) +
  theme_bw()

exceptions <- df_wsd %>% filter(status == "Outside SD, Funded")

ggplot() +
  geom_sf(data = cncl_st) +
  geom_jitter(data = exceptions,width = .02, height = .02,
              aes(x =long, y =lat, color = status)) +
  coord_sf(xlim = c(-117.5,-116.8), ylim = c(32.5,33.2)) +
  theme_bw()


#######################################
# Find distance of points to border
#######################################

# Read shapefile in as SpatialPolygon
cncl_ogr <- readOGR(dsn = get_path("/Shared drives/OES data 2003/Raw_Data/Council_Districts_Map/council_districts_datasd.shp"))
# Transform coordinates
cncl_ogr <- spTransform(cncl_ogr, CRS("+proj=longlat +datum=WGS84"))
# Grab points of boundaries as tibble
cncl_pts <- tidy(cncl_ogr)

# Function for Euclidean distance
euclid_dist <- function(lat_1, lon_1, lat_2, lon_2) sqrt((lat_2 - lat_1)^2 + (lon_2 - lon_1)^2)
# Get euclid dist of one point to many points
euclid_dist_list <- 
  function(lat_1, lon_1, lat_list, lon_list) mapply(FUN = euclid_dist, 
                                                    lat_2 = lat_list, 
                                                    lon_2 = lon_list, 
                                                    MoreArgs = list(lat_1 = lat_1, lon_1 = lon_1), 
                                                    SIMPLIFY = FALSE) %>% unlist()
# Get closest point to one point from list of points
get_closest_coords <- function(lat_1, lon_1, lat_list, lon_list){
  all_dists <- euclid_dist_list(lat_1 = lat_1, lon_1 = lon_1, lat_list = lat_list, lon_list = lon_list)
  which_min_dist <- which.min(all_dists)
  return(c(lat = lat_list[which_min_dist], long = lon_list[which_min_dist], distance = all_dists[which_min_dist]))
}
# Get closest boundary point for every business
closest_points <- 
  with(df_wsd, mapply(
    FUN = get_closest_coords,
    lat_1 = lat, lon_1 = long, 
    MoreArgs = list(lat_list = cncl_pts$lat, lon_list = cncl_pts$long), 
    SIMPLIFY = FALSE))

closest_points <- do.call(what = rbind.data.frame, args = closest_points)
names(closest_points) <- c("border_lat","border_long", "distance_to_border")

## rj note- commented out since
## original code had cbind but 
## this causes error with updated award data
## which has 9821 rows compared to 9816 in the 
## closest_points -- commented out for now 
# df_wclos <- cbind(df_wsd, closest_points)
# 
# ggplot() +
#   geom_point(data = cncl_pts,mapping = aes(x = long, y = lat), color = "red") +
#   geom_point(data = df_wclos,mapping = aes(x = long, y = lat), color = "blue") +
#   coord_cartesian(xlim = c(-117.5,-116.8), ylim = c(32.5,33.2)) +
#   geom_segment(data = df_wclos, aes(x = long, xend = border_long, y = lat, yend = border_lat))

#######################################
# Do final joins and export data
#######################################

df_wsd_wblocks = merge(df_wsd %>% select(edd_submission_number, lat, long, status, in_SD),
                       df_bothblocks,
                       by = "edd_submission_number",
                       all.x = TRUE)

# rj-- wrote as tmp for now while above is not fixed
write.csv(df_wsd_wblocks, get_path("/Shared drives/OES data 2003/Int_Objects/temp_Geocoded_Applicant_Data_San_Diego_wblocks.csv"),row.names = FALSE, na = "")



### older write code- needs border lat and border long and distance_to_border merged above
write.csv(df %>% select(submittal_number, lat, long, border_lat, border_long, distance_to_border, status, in_SD), get_path("/Shared drives/OES data 2003/Int_Objects/Geocoded_Applicant_Data_San_Diego.csv"),row.names = FALSE, na = "")

