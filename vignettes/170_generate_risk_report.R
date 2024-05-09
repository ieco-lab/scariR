
# Generate risk report output---------------------------------------------------

# library package
library(tidyverse)
library(cli)
library(here)
library(rnaturalearth)
# remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
#remotes::install_github("ropensci/cartographer")
library(cartographer)
library(kableExtra)
library(formattable)
library(webshot2)
library(terra)
library(sf)

# use function
slfSpread::create_risk_report(
  locality = "united states",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "USA"),
  create.dir = TRUE,
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
  locality = "china",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "china"),
  create.dir = TRUE,
  save.report = TRUE
)

# this report should have 1 IVR point
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
  create.dir = TRUE,
  save.report = TRUE
)

# chile
slfSpread::create_risk_report(
  locality = "chile",
  locality.type = "country",
  mypath = file.path(here::here(), "vignette-outputs", "reports", "chile"),
  create.dir = TRUE,
  save.report = TRUE
)

