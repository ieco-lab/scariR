Our package `slfSpread` outlines a workflow for conducting species distribution modeling (SDM) for the risk of establishment of the invasive *Lycorma delicatula* (spotted lanternfly or SLF) under climate change. We primarily provide tools and resources for viticulturists to assess and respond to the risk SLF for viticulture at their locality. Viticulturists should apply our function [create_risk_report](/R/create_risk_report.R) to create risk maps, range shift maps, risk plots and other outputs at the scale of countries or states/provinces. We also apply a novel multi-scale approach to model the global risk for establishment of *Lycorma delicatula*, which modelers should adapt and apply to other potential invaders under climate change. We find that our multi-scale approach of ensembling MaxEnt models at the regional-scale provides more refined predictions than our global-scale model and highlights important biological limitations on the spread of SLF.

## Installation

This package should be first be downloaded and installed from GitHub by running the following code:

```
require(devtools)
# install.packages("devtools") # if devtools is not installed yet
devtools::install_github("ieco-lab/slfSpread")
library(slfSpread)
```

The **dependency** packages should then be installed for the package to run properly:

Here are the main packages that `slfSpread` depends on:

```
install.packages(c('cli', 'common', 'CoordinateCleaner', 'devtools', 'dismo', 'dplyr', 'dsmextra', 'ENMTools', 'formattable', 'gginnards', 'ggpattern', 'ggrepel', 'grid', 'here', 'humboldt', 'kableExtra', 'kgc', 'knitr', 'lydemapr', 'patchwork', 'pkgdown', 'plotROC', 'pROC', 'raster', 'rasterVis', 'readr', 'renv', 'rgbif', 'rJava', 'rmarkdown', 'rnaturalearth', 'rnaturalearthhires', 'scales', 'SDMtune', 'sf', 'sp', 'spThin', 'stats', 'stringr', 'taxize', 'terra', 'tibble', 'tidyr', 'tidygeocoder', 'tidyverse', 'usethis', 'utils', 'viridis', 'webshot', 'webshot2'))

# install specific version of ggnewscale
install.packages("ggnewscale", version = '0.4.10')
```

## Citation

The package `slfSpread` is a research compendium for our manuscript, Owens and Helmus, 2024:

**insert citation**

# How to Use this Project

## 1. Produce localized reports on SLF risk to viticulture

Our primary datasets, including risk maps and a viticultural risk analysis, can be accessed by executing the function `create_risk_report()`. This function creates a localized (at the country or state/provincial level) report of the risk for *Lycorma delicatula* to local viticulture. It can be applied to globally important winegrowing regions because it uses our dataset `data/wineries_tidied.rds`, which contains a sample (1,074) of the world's most important winegrowing regions. Users can and should apply this function for their locality as we are projecting that the risk of *Lycorma delicatula* establishment will exapnd under climate change. *Lycorma delicatula* has been classified as being capable of (global) pan-invasion and viticulure has already been devastated in some regions, so there is high potential for viticultural damage under climate change (Huron et al, 2022).

Our function rcreates major datasets that we provide in our main analysis, including:

1. list of known important wine regions within the locality with predicted suitability values and levels
2. current and future risk maps for SLF establishment
3. range shift map of potential range expansion for L delicatula under climate change
4. viticultural quadrant plot depicting of the risk for SLF establishment for known wine regions within the locality. This plot depicts the intersection of our two modeled scales.
5. risk table quantifying the level of risk to vineyards according to the quadrant plot


Here is an example of a workflow for creating a localized risk map and how this might be applied:

I might begin by creating a report at the national level. To create this, you will need the alpha-3 iso country code.

```
slfSpread::create_risk_report(
  locality.iso = "usa",
  locality.type = "country", # to specifiy the type of report
  mypath = file.path(here::here(), "vignette-outputs", "reports", "United States"),
  create.dir = TRUE, # should a directory be created for the report?
  save.report = TRUE # should the report be saved?
)
```

You should first note the three most important outputs of this function: 
1. the risk maps 
2. the accompanying viticultural risk quadrant plot
3. tabled list of viticultural regions in this locality

First, we observe the risk map, which depicts the risk of SLF establishment using the agreement between our regional-scale model ensemble and our global-scale model. More agreement on suitability mean higher risk.

<img src="https://github.com/user-attachments/assets/c02e9028-a5df-4993-8c0b-a49a73be679d" width="60%"/>

**Fig. 1:** Projected current and future risk of *Lycorma delicatula* establishment under climate change | USA

The points on the map represent key viticultural regions. We have extracted the suitability of each viticultural region and depicted its quantitative shift in risk on our second output, the viticultural risk quadrant plot. This plot quantifies the level of risk along both modeled scales, both $\color{violet}{\textsf{presently}}$ and in the $\color{purple}{\textsf{future}}$ under a predicted climate change shift (arrows):

<img src="https://github.com/user-attachments/assets/49df16d5-d9b2-4c5b-960d-b9dca245f0f9" width="50%"/>

**Fig. 2:** Projected shift in the risk for *Lycorma delicatula* establishment at key viticultural regions due to climate change | USA

The accompanying table provides a list of key viticultural regions and their geographical region (state/province), with predicted risk levels: 

**Table 1:** List of viticultural regions and their projected risk

<img src="https://github.com/user-attachments/assets/d75eb908-4255-4286-bf9d-f4bcae380937"/>

You may begin to notice that a particular region has many records, like we can see is the case for Washington State. You could then produce a report only for that region, to get a better idea of the overall trend of risk shift due to climate change. We will produce a report for Washington State alone, to better visualize this trend:

```
slfSpread::create_risk_report(
  locality.iso = "usa", # the country iso is still required
  locality.name = "washington", # the name must be specified
  locality.type = "state_province", # we have changed the report type
  mypath = file.path(here::here(), "vignette-outputs", "reports", "United States"),
  create.dir = FALSE, # dir already exists
  save.report = TRUE
)
```

We can see that most of Washington is at some level of SLF risk presently. Under climate change, risk is projected to decrease some, but one or both modeled scales still predict that SLF can establish in most of the state.

<img src="https://github.com/user-attachments/assets/d1019a9c-7649-4bf6-a91b-8338cf54f9d0" width="60%"/>

**Fig. 3:** Projected current and future risk of *Lycorma delicatula* establishment under climate change | Washington, USA

Based on the viticultural risk quadrant plot, we can now see that Washington state exhibits a totally different trend from the rest of the country. While viticultural regions across the united states exhibit a range of risk levels, regions in Washington are either at high or extreme risk for SLF establishment, and this pattern does not change under predicted climate change levels. 

<img src="https://github.com/user-attachments/assets/2a94ce74-9df7-45df-989b-fccf79566a93" width="50%"/>

**Fig. 4:** Projected shift in the risk for *Lycorma delicatula* establishment at key viticultural regions due to climate change | Washington, USA


In our more localized analysis, we might also be interested in quantifying the total area at risk for SLF or the total number of viticultural regions at risk for SLF establishment. For this, we will look at two additional outputs from `create_risk_report()`: 

1. risk map area table- this quantifies the areas and proportions of the total occupied by each risk category on the risk map
2. viticultural risk table- this quantifies the number of winegrowing regions that are projected to fall into each risk category, both now and in the future under climate change

First, let's look at the risk map area table:

**Table 2:** Table quantifying the projected current and future suitable area for *Lycorma delicatula* establishment | Washington, USA

<img src="https://github.com/user-attachments/assets/e795c878-546d-44d3-9dc2-195850d15029"/>

We can see from this table that the total unsuitable area decreased under climate change, from 8.6% to 7.4%, but this change was mostly found in the regional-scale model ensemble, which predicted ~9% more suitable arae under climate change. Our modeled scales also diverged in the agreed suitable area (suitable_agreement decreased by ~9%). 

Next, lets look at the viticultural risk table:

**Table 3:** Table quantifying the projected shift in the risk for *Lycorma delicatula* establishment at key viticultural regions due to climate change | Washington, USA

<img src="https://github.com/user-attachments/assets/76681887-5b69-4256-87dd-2861c4755ba7" width="90%"/>

From this table, we can see that all 7 viticultural regions maintain their present risk level under climate change. Six regions fall into the high risk category (regional ensemble suitability only) and one falls into the extreme risk category (suitable area agreement).

Users should explore the other localities and data types available with this function.


## 2. Recreate the analysis for another invasive species of interest

For modelers who wish to apply this pipeline for other invasive species, this pipeline can easily be adapted to model the risk of establishment by simply changing the input datasets and modeled scales. 

First, a modeler would need to change the input datasets, outlined in vignettes 020-040. In vignette 020, I retrieved input data from GBIF, which hosts datasets for thousands of other species. I recommend including extra data from other databases and the literature as I did in my analysis, but GBIF is a great starting point for data retrieval. Here is an example using the packages `rgbif` and `taxize`:

```{r get taxa ID and GBIF presence data}
# get species ID from gbif database
ids <- taxize::get_ids(sci_com = "Emerald Ash Borer", db = "gbif")

# initiate download
records_gbif <- rgbif::occ_download(
  # general formatting
  type = "and",
  format = "SIMPLE_CSV",
  # inclusion rules
  pred("taxonKey", ids[[1]]), # search by ID, not species name
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred("occurrenceStatus", "PRESENT")
)
```

The input covariates would also need to change based on what is biologically relevant for the particular species. I outline the process of choosing input covariates in vignette 030. 

Once the input data have been adapted to the user's needs, the regional-scale models will need to be applied per region of interest. Each regional-scale model depends on a spcecific background area selection for MaxEnt that will need to change. I outline the process for choosing this area in vignette 060, in which I subset the presence data by region and intersect these subsets with the Köppen-Geiger climate zones to select the appropriate background area. This resulting polygon would need to be cropped to the region of interest. I use the `kgc`, `terra`, and `sf` packages for this analysis:

```{r get K-G zones and intersect with presence data}
# get K-G zones
kmz_data <- kgc::kmz

# generate coordinates
kmz_lat <- kgc::genCoords(latlon = "lat", full = TRUE)
kmz_lon <- kgc::genCoords(latlon = "lon", full = TRUE)

# join data and coordinates
kmz_data <- cbind(kmz_lat, kmz_lon) %>%
  cbind(., kmz_data) %>%
  as.data.frame() %>%
  # relocate column
  dplyr::select(kmz_lon, everything()) 

# convert to raster
KG_zones_rast <- terra::rast(
  x = kmz_data,
  type = "xyz",
  crs = "EPSG:4326"
  )

# next, convert raster to polygon
KG_zones_poly <- terra::as.polygons(
  x = KG_zones_rast,
  aggregate = TRUE, # combine cells with the same value into one area
  values = TRUE, # include cell values as attributes
  crs = "EPSG:4326"
)
KG_zones_poly <- sf::st_as_sf(KG_zones_poly)

# intersect polygon and presences
regional_poly <- sf::st_filter(x = KG_zones_poly, y = presence_data)
```

Once the presence data, coviariate data and background points have been chosen, the user might apply this framework for any invasive species and ensemble models for any region of interest.

# References

Bryant, C., Wheeler, N. R., Rubel, F., & French, R. H. (2017). kgc: Koeppen-Geiger Climatic Zones. https://CRAN.R-project.org/package=kgc

Chamberlain S, Barve V, Mcglinn D, Oldoni D, Desmet P, Geffert L, Ram K (2024). rgbif: Interface to the Global Biodiversity Information Facility API. R package version 3.8.0, https://CRAN.R-project.org/package=rgbif.

Scott Chamberlain and Eduard Szocs (2013). taxize - taxonomic search and retrieval in R. F1000Research, 2:191. URL: https://f1000research.com/articles/2-191/v2

Gallien, L., Douzet, R., Pratte, S., Zimmermann, N. E., & Thuiller, W. (2012). Invasive species distribution models – how violating the equilibrium assumption can create new insights. Global Ecology and Biogeography, 21(11), 1126–1136. https://doi.org/10.1111/j.1466-8238.2012.00768.x

Huron, N. A., Behm, J. E., & Helmus, M. R. (2022). Paninvasion severity assessment of a U.S. grape pest to disrupt the global wine market. Communications Biology, 5(1), 655. https://doi.org/10.1038/s42003-022-03580-w

Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal 10 (1), 439-446, https://doi.org/10.32614/RJ-2018-009

Phillips, S. J., Anderson, R. P., & Schapire, R. E. (2006). Maximum entropy modeling of species geographic distributions. Ecological Modelling, 190(3), 231–259. https://doi.org/10.1016/j.ecolmodel.2005.03.026

Hijmans R (2024). terra: Spatial Data Analysis. R package version 1.7-81, https://rspatial.github.io/terra/, https://rspatial.org/.

Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” Journal of Open Source Software, 4(43), 1686. doi:10.21105/joss.01686.
