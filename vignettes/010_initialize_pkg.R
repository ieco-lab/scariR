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



