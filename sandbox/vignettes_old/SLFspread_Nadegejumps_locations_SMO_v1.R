# Title: SLFspread_Nadegejumps_locations_SMO_v1
# Created: 01-25-2022
# Purpose: To manipulate SLF spread data points by associating the latitude/longitudes with county and municipality names
# A unique key ID will also be added per unique data point
# Author: Samuel M. Owens
# Contact: sam.owens@temple.edu

# Necessary Packages----

library(tidyverse)
library(magrittr)
library(sf)
library(tigris)
library(maps)
options(tigris_use_cache = TRUE)

# Load Necessary Datasets----

# Here is where the raw jump records data were read in
SLF_jumps <- read_csv(
  "/Volumes/GoogleDrive/Shared drives/slfModeling/projects/slfSpread/Data/Raw/(Nadege) jumps_raw.csv"
  )

# Here is an object containing strings of state names that will be used with the counties object
# in the tigirs package
which_states <- c("Alabama",
                  "Alaska",
                  "Arizona",
                  "Arkansas",
                  "California",
                  "Colorado",
                  "Connecticut",
                  "Delaware",
                  "Florida",
                  "Georgia",
                  "Hawaii",
                  "Idaho",
                  "Illinois",
                  "Indiana",
                  "Iowa",
                  "Kansas",
                  "Kentucky",
                  "Louisiana",
                  "Maine",
                  "Maryland",
                  "Massachusetts",
                  "Michigan",
                  "Minnesota",
                  "Mississippi",
                  "Missouri",
                  "Montana",
                  "Nebraska",
                  "Nevada",
                  "New Hampshire",
                  "New Jersey",
                  "New Mexico",
                  "New York",
                  "North Carolina",
                  "North Dakota",
                  "Ohio",
                  "Oklahoma",
                  "Oregon",
                  "Pennsylvania",
                  "Rhode Island",
                  "South Carolina",
                  "South Dakota",
                  "Tennessee",
                  "Texas",
                  "Utah",
                  "Vermont",
                  "Virginia",
                  "Washington",
                  "West Virginia",
                  "Wisconsin",
                  "Wyoming"
)

# Here are the state names that will be added to the counties tigris dataset. They were initially
# retrieved from USGS.gov on 2021-04-21.
state_names <- read_csv(
  "/Volumes/GoogleDrive/My Drive/Temple Classes/Analytics in R 5323/gcsaR_datasets/US_FIPS_Codes.csv",
  skip = 1
)

counties <- counties(state = which_states, 
                     cb = T, 
                     resolution = "500k"
                     ) %>% 
  mutate(FIPS = as.numeric(paste0(STATEFP, 
                                  COUNTYFP))
  )

 urban_areas <- urban_areas()

# Joining counties dataset with state names----

state_names %<>%
  select(State, `FIPS State`) %>%
  # eliminating duplicate entries- county FIPS names caused duplicates
  unique() %>%
  rename(FIPS_State = `FIPS State`) 

counties %<>%
  left_join(state_names, 
            by = c("STATEFP" = "FIPS_State")
)

# Unique Key ID and ref # columns added to Jump dataset----

SLF_jumps_transform <- SLF_jumps %>%
  select(bio_year, 
         latitude_rounded, 
         longitude_rounded
         ) %>%
  arrange(bio_year,
          latitude_rounded,
          longitude_rounded
          ) %>%
  mutate(ref_ID = paste("JMP", 
                        seq(along.with = bio_year), 
                        sep = "_"),
         row_ID = seq(along.with = ref_ID)
         # This ID column is simply a series of numbers that are used to match the FIPS codes.
         # They are deleted once the FIPS codes are added
         ) 

# FIPS codes and county names added----

# code courtesy of Seba :D
SLF_jumps_transform %<>%
  # reducing data to coordinates only, and ID for future merging
  select(latitude_rounded, longitude_rounded, row_ID) %>%
  # transforming into sf object
  sf::st_as_sf(coords = c("longitude_rounded", "latitude_rounded"),
               crs = sf::st_crs(counties)) %>% 
  # intersecting state polygons with coordinate points in data, 
  # to find in which US state the coordinates are
  st_join(., counties, join = sf::st_intersects) %>%
  as_tibble() %>% 
  # simplifying to row ID, county name and county identity
  select(row_ID, county = NAME, FIPS, State) %>% 
  # then joining this into main dataset
  left_join(SLF_jumps_transform, ., by = "row_ID")

# Municipality data added----

SLF_jumps_transform  %<>%
  select(latitude_rounded, longitude_rounded, row_ID) %>%
  sf::st_as_sf(coords = c("longitude_rounded", "latitude_rounded"),
               crs = sf::st_crs(urban_areas)) %>% 
  st_join(., urban_areas, join = sf::st_intersects) %>%
  as_tibble() %>% 
  select(row_ID, urban_area = NAMELSAD10, urban_GEOID = GEOID10) %>% 
  left_join(SLF_jumps_transform, ., by = "row_ID") %>%
  # removing unnecessary ID column
  select(-c(row_ID)) 

# Final data cleaning----

SLF_jumps_tidied <- SLF_jumps_transform %>%
  rename(county_FIPS = FIPS) %>%
  # moving the ref_ID column to front and moving state names to left of county names
  select(ref_ID, 
         bio_year,
         latitude_rounded,
         longitude_rounded,
         State,
         everything()
  )

# Data export----

write_csv(SLF_jumps_tidied,
          "/Volumes/GoogleDrive/Shared drives/slfModeling/projects/slfSpread/Data/Root/SLFspread_Jumps_SMO_v1.csv"
)
