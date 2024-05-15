
# library package
library(tidyverse)
library(cli)
library(here)
library(rnaturalearth)
# remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
library(kableExtra)
library(formattable)
library(webshot2)
library(terra)
library(ggnewscale)

# create internal dataset for create_risk_report.R------------------------------

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



# transform all data frames to internal dataset
usethis::use_data(
  threshold_exponential_values, IVR_locations, summary_global, summary_regional_ensemble, xy_global_1995, xy_global_2055, xy_regional_ensemble_1995, xy_regional_ensemble_2055,
  internal = TRUE,
  overwrite = FALSE
)



# apply function----------------------------------------------------------------

# use function
slfSpread::create_risk_report(
  locality = "united states",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "USA"),
  create.dir = FALSE,
  save.report = TRUE
)

# try state level
slfSpread::create_risk_report(
  locality = "california",
  locality.type = "states_provinces",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "USA"),
  create.dir = FALSE,
  save.report = TRUE
)

slfSpread::create_risk_report(
  locality = "pennsylvania",
  locality.type = "states_provinces",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "USA"),
  create.dir = FALSE,
  save.report = TRUE
)


slfSpread::create_risk_report(
  locality = "china",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "china"),
  create.dir = FALSE,
  save.report = TRUE
)

# this report should have 2 IVR points
slfSpread::create_risk_report(
  locality = "shandong",
  locality.type = "states_provinces",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "china"),
  create.dir = FALSE,
  save.report = TRUE
)

# this report should have no IVR records
slfSpread::create_risk_report(
  locality = "yunnan",
  locality.type = "states_provinces",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "china"),
  create.dir = FALSE,
  save.report = TRUE
)
# it worked still so thats good

# countries of interest
# france
slfSpread::create_risk_report(
  locality = "france",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "france"),
  create.dir = FALSE,
  save.report = TRUE
)

# chile
slfSpread::create_risk_report(
  locality = "chile",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "chile"),
  create.dir = FALSE,
  save.report = TRUE
)

# australia
slfSpread::create_risk_report(
  locality = "australia",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "australia"),
  create.dir = FALSE,
  save.report = TRUE
)

# romania
slfSpread::create_risk_report(
  locality = "romania",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "romania"),
  create.dir = FALSE,
  save.report = TRUE
)

# spain
slfSpread::create_risk_report(
  locality = "spain",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "spain"),
  create.dir = FALSE,
  save.report = TRUE
)

# italy
slfSpread::create_risk_report(
  locality = "italy",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "italy"),
  create.dir = FALSE,
  save.report = TRUE
)


## CC shift areas of interest

# united kingdom
slfSpread::create_risk_report(
  locality = "united kingdom",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "UK"),
  create.dir = TRUE,
  save.report = TRUE
)

# michigan
slfSpread::create_risk_report(
  locality = "michigan",
  locality.type = "states_provinces",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "USA"),
  create.dir = FALSE,
  save.report = TRUE
)

# finland
slfSpread::create_risk_report(
  locality = "finland",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "finland"),
  create.dir = TRUE,
  save.report = TRUE
)


## do some edits to reports

# the spain report came out with the the maps needing some work.
# The map is zoomed very far out because of some territories belonging to spain.
# I will try to limiting the x and y coordinates of the plot to zoom in.

spain_current <- slf_risk_report[["risk_maps"]][["current_risk_map"]] +
  xlim(-10, 5) +
  ylim(35, 44)

spain_future <- slf_risk_report[["risk_maps"]][["2055_risk_map"]] +
  xlim(-10, 5) +
  ylim(35, 44)

spain_shift <- slf_risk_report[["range_shift_map"]] +
  xlim(-10, 5) +
  ylim(35, 44)


#save
ggsave(
  spain_current,
  filename = file.path(here::here(), "vignette-outputs", "reports", "spain", "Spain_risk_map_present_edited.jpg"),
  height = 8,
  width = 10,
  device = "jpeg",
  dpi = "retina"
)

ggsave(
  spain_future,
  filename = file.path(here::here(), "vignette-outputs", "reports", "spain", "Spain_risk_map_2055_edited.jpg"),
  height = 8,
  width = 10,
  device = "jpeg",
  dpi = "retina"
)

ggsave(
  spain_shift,
  filename = file.path(here::here(), "vignette-outputs", "reports", "spain", "Spain_range_shift_map_edited.jpg"),
  height = 8,
  width = 10,
  device = "jpeg",
  dpi = "retina"
)

### USA
# the USA map usually comes including Alaska and hawaii, which we do not want

USA_current <- united_states_slf_risk_report[["risk_maps"]][["current_risk_map"]] +
  xlim(-125, -65) +
  ylim(25, 50)

USA_future <- united_states_slf_risk_report[["risk_maps"]][["2055_risk_map"]] +
  xlim(-125, -65) +
  ylim(25, 50)

USA_shift <- united_states_slf_risk_report[["range_shift_map"]] +
  xlim(-125, -65) +
  ylim(25, 50)


#save
ggsave(
  USA_current,
  filename = file.path(here::here(), "vignette-outputs", "reports", "USA", "united_states__risk_map_present_edited.jpg"),
  height = 8,
  width = 10,
  device = "jpeg",
  dpi = "retina"
)

ggsave(
  USA_future,
  filename = file.path(here::here(), "vignette-outputs", "reports", "USA", "united_states_risk_map_2055_edited.jpg"),
  height = 8,
  width = 10,
  device = "jpeg",
  dpi = "retina"
)

ggsave(
  USA_shift,
  filename = file.path(here::here(), "vignette-outputs", "reports", "USA", "united_states__shift_map_edited.jpg"),
  height = 8,
  width = 10,
  device = "jpeg",
  dpi = "retina"
)


## create facets of maps--------------------------------------------------------








## DEPRECATED--
# inclusion of bbox cropping in function

#library(sf)
#library(cartographer)

# I originally included package cargtographer in the dependencies, but decided
# against it because it depends on some superceded packages. I only need the data object called
# countries_bbox from the package, so I downloaded it from the
# package and will include it in the import for my function.
# info retrieved from: https://gist.github.com/graydon/11198540

# bbox_list <- countries_bbox

# append france because that is absent for some reason
# it was causing issues because of its territories
# info from http://bboxfinder.com/#42.240307,-5.075684,51.171134,8.336426
# xmin and ymin edited slightly for visualization
# bbox_list <- tibble::add_row(bbox_list, iso = "FR", name = "France", x_min = -5.000, x_max = 8.336426, y_min = 42.00, y_max = 51.171134)

##--


