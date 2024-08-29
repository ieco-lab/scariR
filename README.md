Our package `slfSpread` outlines a workflow for conducting species distribution modeling (SDM) for the risk of establishment of the invasive *Lycorma delicatula* (spotted lanternfly or SLF) under climate change. We primarily provide tools and resources for viticulturists to assess and respond to the risk SLF for viticulture at their locality. Viticulturists should apply our function [create_risk_report](/ieco-lab/slfSpread/blob/master/R/create_risk_report.R) to create risk maps, range shift maps, risk plots and other outputs at the scale of countries or states/provinces. We also apply a novel multi-scale approach to model this invasive pest that modelers should adapt and apply to model the risk for establishment of other potential global invaders under climate change. We find that our multi-scale approach of ensembling MaxEnt models at the regional-scale provides more refined predictions than our global-scale model and highlights important biological limitations on the spread of SLF.

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

## How to Use this Project

### 1. Produce localized reports on SLF risk to viticulture

Our primary datasets, including risk maps and a viticultural risk analysis, can be accessed by executing the function `create_risk_report`. This function creates a localized (at the country or state/provincial level) report of the risk for *Lycorma delicatula* to local viticulture. It also recreates the other major datasets that we provide in our analysis, including:

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
2. the accompanying risk quadrant plot
3. tabled list of viticultural regions in this locality

First, we observe the risk map, which depicts the risk of SLF establishment using the agreement between our regional-scale model ensemble and our global-scale model. More agreement on suitability mean higher risk.

<img src="https://github.com/user-attachments/assets/c02e9028-a5df-4993-8c0b-a49a73be679d" width="60%"/>

**Fig. 1:** Projected current and future risk of *Lycorma delicatula* establishment under climate change | USA

The points on the map represent key viticultural regions. We have extracted the suitability of each viticultural region and depicted its quantitative shift in risk on our second output, the risk quadrant plot. This plot quantifies the level of risk along both modeled scales, both $\color{violet}{\textsf{presently}}$ and in the $\color{purple}{\textsf{future}}$ under a predicted climate change shift (arrows):

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

Based on the risk quadrant plot, we can now see that Washington state exhibits a totally different trend from the rest of the country. While viticultural regions across the united states exhibit a range of risk levels, regions in Washington are either at high or extreme risk for SLF establishment, and this pattern does not change under predicted climate change levels. 

<img src="https://github.com/user-attachments/assets/2a94ce74-9df7-45df-989b-fccf79566a93" width="60%"/>

**Fig. 4:** Projected shift in the risk for Lycorma delicatula establishment at key viticultural regions due to climate change | Washington, USA

This function can be applied to important winegrowing regions across the globe because it uses our dataset `data/wineries_tidied.rds`, which contains a sample (1,074) of the world's most important winegrowing regions.


### 2. Recreate the analysis for another pest of interest


# References

Phillips, S. J., Anderson, R. P., & Schapire, R. E. (2006). Maximum entropy modeling of species geographic distributions. Ecological Modelling, 190(3), 231–259. https://doi.org/10.1016/j.ecolmodel.2005.03.026

Gallien, L., Douzet, R., Pratte, S., Zimmermann, N. E., & Thuiller, W. (2012). Invasive species distribution models – how violating the equilibrium assumption can create new insights. Global Ecology and Biogeography, 21(11), 1126–1136. https://doi.org/10.1111/j.1466-8238.2012.00768.x

