# Description

This package is a research compendium for the manuscript: 

"Multi-scale modeling of the spotted lanternfly Lycorma delicatula (Hemiptera: Fulgoridae) reveals displaced risk to viticulture and regional range expansion due to climate change"

We outline a workflow for conducting species distribution modeling (SDM) for the invasive Lycorma delicatula. We primarily provide tools for viticulturalists to assess the risk of establishment of L. delicatula at their locality currently and under predicted future climate conditions. Viticulturalists should apply our function 'create_risk_report' to retrieve present and climate change risk maps, range shift maps, risk plots and other outputs at the scale of countries or states/provinces. These resources are intended to help viticulturalists assess respond to predicted risk for Lycorma delicatula establishment under climate change. We also apply a novel multi-scale approach to model this invasive pest and find that our ensemble of regional-scale models provides more refined predictions than our global-scale model and highlights important biological limitations on the spread of SLF more effectively than our global-scale model. Modelers should adapt and apply this framework to model the global suitability for establishment of other incipient invaders under climate change.

![Alt text](/vignette-outputs/figur)

Here are the main packages that this package depends on:

```
install.packages(c('cli', 'common', 'CoordinateCleaner', 'devtools', 'dismo', 'dplyr', 'dsmextra', 'ENMTools', 'formattable', 'gginnards', 'ggpattern', 'ggrepel', 'grid', 'here', 'humboldt', 'kableExtra', 'kgc', 'knitr', 'lydemapr', 'patchwork', 'pkgdown', 'plotROC', 'pROC', 'raster', 'rasterVis', 'readr', 'renv', 'rgbif', 'rJava', 'rmarkdown', 'rnaturalearth', 'rnaturalearthhires', 'scales', 'SDMtune', 'sf', 'sp', 'spThin', 'stats', 'stringr', 'taxize', 'terra', 'tibble', 'tidyr', 'tidygeocoder', 'tidyverse', 'usethis', 'utils', 'viridis', 'webshot', 'webshot2'))

# install specific version of ggnewscale
install.packages("ggnewscale", version = '0.4.10')
```

# How to reproduce this analysis



# Data



# References

