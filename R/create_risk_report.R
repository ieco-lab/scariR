#'Creates
#'
#'@description
#'
#'@param locality The country, region or subregion
#'
#'
#'@details
#'
#'Requires the following packages: 'tidyverse', 'cli', rnaturalearthhires.
#'
#'Note that this function performs downloads from [naturalearthdata.com](https://www.naturalearthdata.com/).
#'The function will automatically create subfolders in "root/data-raw" containing the shapefiles.
#'
#'@return
#'
#'
#'@examples
#'
#'example
#'
#'@export
create_risk_report <- function(locality) {

  # Error checks----------------------------------------------------------------




  # import settings for files---------------------------------------------------



  # import shapefiles (locality)------------------------------------------------

  # update packages
  rnaturalearth::check_rnaturalearthdata()
  rnaturalearth::check_rnaturalearthhires()

  # create directories for shapefiles
  dir.create(here::here(), "data-raw", "ne_countries")
  dir.create(here::here(), "data-raw", "ne_states_provinces")


  # import countries
  countries_sf <- rnaturalearth::ne_download(
    scale = 10, # highest resolution
    type = "admin_0_countries", # states and provinces
    category = "cultural",
    destdir = file.path(here::here(), "data-raw", "ne_countries"),
    load = TRUE, # load into environment
    returnclass = "sf" # shapefile
  )

  # import states
  states_provinces_sf <- rnaturalearth::ne_download(
    scale = 10, # highest resolution
    type = "admin_1_states_provinces", # states and provinces
    category = "cultural",
    destdir = file.path(here::here(), "data-raw", "ne_states_provinces"),
    load = TRUE, # load into environment
    returnclass = "sf" # shapefile
  )







}
