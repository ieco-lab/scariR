# Title: slfSpread_jumps_locations_SMO_v2
# Created: 02-14-2022
# Purpose: This Rscript is based on a modified version of the Nadege Jumps dataset
  # where the data have been clustered ("rarefied") to include only one point per cluster of coordinates.
  # In the first version, a key ID and county, municipal and state names were added
  # to coordinates. In this version, the Key ID will be changed to a combo of the
  # latitude and longitude. Data points will be clustered further if necessary.
  # I have also made all of the filepaths relative
# Author: Samuel M. Owens
# Contact: sam.owens@temple.edu

# Necessary Packages----

library(here)
library(tidyverse)
library(magrittr)
library(sf)
library(tigris)
library(maps)
options(tigris_use_cache = TRUE)

# Load Necessary Datasets----

# Here is where the raw jump records data were read in
SLF_jumps_clustered <- read_csv(
  file.path(here(), "data_raw/jumps_full_rarefied.csv")
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
  file.path(here(), "data_raw/US_FIPS_Codes.csv"),
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

SLF_jumps_transform <- SLF_jumps_clustered %>%
  select(bio_year,
         latitude_rounded,
         longitude_rounded,
         # the rarefied column keeps only the centroid point in a cluster of points
         # so, = TRUE represents a unique spread event
         Rarefied
  ) %>%
  filter(Rarefied == "TRUE" ) %>%
  arrange(bio_year,
          latitude_rounded,
          longitude_rounded
  ) %>%
  mutate(ref_ID = paste(latitude_rounded,
                        longitude_rounded,
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
  # transforming into spatial feature (sf) object
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
  left_join(SLF_jumps_transform, ., by = "row_ID")

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
  ) %>%
  # removing unnecessary rowID and Rarefied columns
  select(-c(row_ID, Rarefied))

# Data export----

write_csv(SLF_jumps_tidied,
          file.path(here(), "data_root/slfSpread_jumps_locations_SMO_v2.csv")
)

