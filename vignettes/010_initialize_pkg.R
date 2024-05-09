# INTITALIZE PACKAGE------------------------------------------------------------
library(here)

# list package dependencies
print(list.files(file.path(here(), "renv/library/R-4.2/x86_64-w64-mingw32")))

# renv initialization

# activate renv
renv::activate()

# initialization of project
renv::init()

# discover what packages still need to be installed
renv::snapshot()

renv::restore(c("ENMTools", "SLFspread", "USAboundaries", "devtools", "doParallel",
"ggfortify", "ggrepel", "humboldt", "leaflet", "maps", "maptools", "mapview",
"measurements", "parsedate", "patchwork", "raster", "rgdal", "rgeos",
"rnaturalearth", "rnaturalearthdata", "scrubr", "sf", "slfrsk", "sp", "spocc",
"stargazer", "taxize", "terra", "tigris", "viridis"))



# create internal dataset for create_risk_report.R------------------------------

library(terra)
library(tidyverse)
library(here)

# .csv files
threshold_exponential_values <- read.csv(file = file.path(here::here(), "data-raw", "threshold_exponential_values.csv"))
# .rds files
IVR_locations <- readr::read_rds(file = file.path(here::here(), "data", "wineries_tidied.rds"))
summary_global <- readr::read_rds(file = file.path(here::here(), "data", "global_summary_all_iterations_v2.rds"))
summary_regional_ensemble <- readr::read_rds(file = file.path(here::here(), "data", "ensemble_threshold_values.rds"))
# predicted xy suitability
xy_global_1995 <- readr::read_rds(file = file.path(here::here(), "data", "global_wineries_1981-2010_xy_pred_suit.rds"))
xy_global_2055 <- readr::read_rds(file = file.path(here::here(), "data", "global_wineries_2041-2070_GFDL_ssp370_xy_pred_suit.rds"))
xy_regional_ensemble_1995 <- readr::read_rds(file = file.path(here::here(), "data", "regional_ensemble_wineries_1981-2010_xy_pred_suit.rds"))
xy_regional_ensemble_2055 <- readr::read_rds(file = file.path(here::here(), "data", "regional_ensemble_wineries_2041-2070_GFDL_ssp370_xy_pred_suit.rds"))


# transform data frames to internal dataset
usethis::use_data(
  threshold_exponential_values, IVR_locations, summary_global, summary_regional_ensemble, xy_global_1995, xy_global_2055, xy_regional_ensemble_1995, xy_regional_ensemble_2055,
  internal = TRUE,
  overwrite = FALSE
  )
