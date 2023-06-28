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
