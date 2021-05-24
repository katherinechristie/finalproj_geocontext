


#######################################
# Script to pull ACS data at tract level
# for 2019
#########################################

## packages
library(tigris)
library(ggplot2)
library(rgdal)
library(sf)
library(tigris)
library(tidyverse)
library(gridExtra)
library(stringr)
library(tictoc)
library(tidycensus)
library(reshape2)
library(readxl)
library(data.table)


options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

#######################################
# Define ACS vars to pull
#########################################

acs_vars = load_variables(2019,
                          "acs5", cache = TRUE)

# ## example how to search for vars
View(print.data.frame(acs_vars %>% filter(grepl("Renter-occupied", label, ignore.case = TRUE)) %>%
                  dplyr::select(name, label)))
#
View(print.data.frame(acs_vars %>% filter(grepl("B07013", name, ignore.case = TRUE)) %>%
                  dplyr::select(name, label)))

total_pop = c("B01001_001")

race = c("B02001_001",
         "B02001_002",
         "B02001_003", 
         "B02001_004", 
         "B02001_005",
         "B02001_006",
         "B02001_007",
         "B02001_008",
         "B03001_002",
         "B03001_003",
         "B03001_001")

## get names of variables that are assocated with
educ_vars = c("B16010_001", "B16010_002", "B16010_015",
              "B16010_028", "B16010_041", "B19013_001")
housing_status = c("B25064_001",
                   "B25123_002",
                   "B25123_008",
                   "B25123_001",
                   "B07013_001",
                   "B07013_002", #
                   "B07013_003", # renter occupied
                   "B25002_001", # vacancy 
                   "B25002_003") 

poverty = c("B06012_001",
            "B06012_002",
            "B06012_003",
            "B06012_004",
            "B09010_001", # snap
            "B09010_002",
            "C18120_001",
            "C18120_006") # unemployment  

income = c(sprintf("B06010_00%s", seq(1:9)), 
           sprintf("B06010_0%s", seq(10, 12))) 
occupancy = c(sprintf("B25014_00%s", seq(1:9)),
              sprintf("B25014_0%s", seq(10, 13)))

famstruc  = c(sprintf("B11012_00%s", seq(1:9)),
              sprintf("B11012_0%s", seq(10, 17)))

rentburden = c(sprintf("B25070_00%s", seq(1:9)),
               sprintf("B25070_0%s", seq(10, 11)))

veteran = c("B21001_002",
            "B21001_001")
tribes = c("B02014_001", "B02014_002")

### consolidate
vars_topull = c(total_pop, 
                race,
                educ_vars, housing_status,
                poverty, income, occupancy, 
                famstruc, rentburden, veteran,
                tribes)

## start key
census_api_key("8105419cada33ca0aaa48b111b8c44b9484e286a")


### pull
### note that we're not pulling shapefiles
us <- unique(fips_codes$state)[1:51]
pull_all_tracts = lapply(us,  function(x) get_acs(geography = "tract",
                                                  variables = vars_topull,
                                                  state = x,
                                                  year = 2019))

print("Finished pulling tracts; save")
path_todata = "/Volumes/GoogleDrive/Shared drives/OES data 2113 ERA/Raw/ACS_Tract/"

saveRDS(pull_all_tracts,
        sprintf("%s%s", path_todata, "acs_2019raw.RDS")) 

pull_all_tracts = readRDS(sprintf("%s%s", path_todata, "acs_2019raw.RDS"))

pull_all_tracts_df = do.call(rbind.data.frame,
                             pull_all_tracts)

## rename variables
vars_pulled = acs_vars %>%
  filter(name %in% vars_topull) %>%
  mutate(cleaned_name = gsub(" ", "_", 
                             gsub("Estimate!!|Total|\\:\\!\\!", 
                                  "",
                                  label)),
         cleaned_name_2 = ifelse(cleaned_name == ":",
                                 tolower(gsub(" ", "_", concept)),
                                 cleaned_name),
         cleaned_name_3 = sprintf("%s_%s", 
                                  cleaned_name_2,
                                  gsub(" ", "_", concept))) %>%
  dplyr::select(name, cleaned_name_3)

## key vars- B07013_003: renter occupied
## denom - B007013-001

pull_tracts_wnames = merge(pull_all_tracts_df,
                           vars_pulled,
                           by.x = "variable",
                           by.y = "name",
                           all.x = TRUE)  %>%
  dplyr::select(GEOID, estimate, cleaned_name_3, variable) 

pull_tracts_wnames_wide_informative = reshape(pull_tracts_wnames %>% select(-variable), 
                          idvar = "GEOID", timevar = "cleaned_name_3", direction = "wide")

fwrite(pull_tracts_wnames_wide_informative,
        sprintf("%s%s", path_todata, "acs_2019clean_varnames.csv")) 

pull_tracts_wnames_wide = reshape(pull_tracts_wnames %>% select(-cleaned_name_3), 
                                              idvar = "GEOID", timevar = "variable", direction = "wide")

fwrite(pull_tracts_wnames_wide,
       sprintf("%s%s", path_todata, "acs_2019clean_varnumbers.csv")) 


