# Shifting Climate Assessments for Risk of Invasions
## `scari` R Package Overview
<a href="https://ieco-lab.github.io/scarifSDM/"><img src="man/figures/scari_logo_v1_0.png" align="right" height="180" alt="scarifSDM website" /></a>

`scari` is an R package and research compendium that documents a multiscale species distribution modeling (SDM) workflow to forecast establishment and impact risk of a species invasion as it shifts with climate change.

We developed this workflow to quantify the shifting risk of future establishment of the invasive species *Lycorma delicatula* (spotted lanternfly or SLF) in important viticulture regions worldwide. The `R` function [create_risk_report](https://github.com/ieco-lab/scarifSDM/blob/master/R/create_risk_report.R) produces risk maps, range shift estimates, risk plots and other outputs at the scale of countries or smaller geopolitical units.

### Citation

The package `scari` is a research compendium for:

Owens, S. M. (2024). Multi-scale Modeling of the Spotted Lanternfly Lycorma delicatula (Hemiptera: Fulgoridae) Reveals Displaced Risk to Viticulture and Regional Range Expansion Due to Climate Change [M.S., Temple University]. In ProQuest Dissertations and Theses (3099643448). https://www.proquest.com/dissertations-theses/multi-scale-modeling-spotted-lanternfly-em/docview/3099643448/se-2?accountid=130527
![image](https://github.com/user-attachments/assets/c840b94d-0c42-40bb-9134-cae1c31bb7e3)


### Installation

This package should be first be downloaded and installed from GitHub by running the following code:

```
require(devtools)
# install.packages("devtools") # if devtools is not installed yet
devtools::install_github("ieco-lab/scari")
library(scari)
```

The **dependency** packages should then be installed for the package to run properly:

Here are the main packages that `scari` depends on:

```
install.packages(c('cli', 'common', 'CoordinateCleaner', 'devtools', 'dismo', 'dplyr', 'dsmextra', 'ENMTools', 'formattable', 'GeoThinneR', 'gginnards', 'ggplot2', 'gitcreds' 'grid', 'here', 'humboldt', 'kableExtra', 'kgc', 'knitr', 'lydemapr', 'patchwork', 'pkgdown', 'plotROC', 'pROC', 'raster', 'rasterVis', 'readr', 'renv', 'rgbif', 'rJava', 'rmarkdown', 'rnaturalearth', 'rnaturalearthhires', 'scales', 'SDMtune', 'sf', 'sp', 'spThin', 'stats', 'stringr', 'terra', 'tibble', 'tidygeocoder', 'tidyr', 'tidyverse', 'usethis', 'utils', 'viridis', 'webshot', 'webshot2'))

# we also suggest installing the following packages:
install.packages(c("blockCV", "knitr", "rmarkdown"))

# install specific version of ggnewscale
library(remotes)
remotes::install_version("ggnewscale", version = "0.4.10")
```

### Sitemap

This project is organized into general sections of our modeling pipeline: Our [vignettes](https://github.com/ieco-lab/scarifSDM/tree/master/vignettes) follow this general order: 

* 010: Initialize `scarifSDM`: initialization of `renv` package for dependencies
* 020-030: Retrieve and tidy input data for MaxEnt
* 040-090: SDM modeling pipeline: train global and 3 regional-scale models
* 100-110: Ensemble Regional-scale SDMs
* 120-142: Quantify SLF risk and model fit
* 150: Quantify risk to specific viticultural regions using our function [create_risk_report()](https://github.com/ieco-lab/scarifSDM/blob/master/vignettes/150_create_risk_report.Rmd)

## How to Use this Project

Before diving into this project and our modeling workflow, an end user should:
1. read the [companion paper](), which outlines the conceptual underpinnings for this project
2. install the package `renv`, which ensures that R package versions are consistent for running this package (this craetes a projct-specific R package library, so it should not affect your main library)
3. run the first vignette, [010_initialize_renv](https://github.com/ieco-lab/scarifSDM/blob/master/vignettes/010_initialize_pkg.Rmd), which initializes `renv` and lists our package's dependencies.
4. See "Get Started" for help in using our package to:
4.1 Produce localized reports on SLF risk to viticulture, and
4.2 Recreate our analysis for another invasive species of interest

### Computing Information

This package was developed and its vignettes were rendered on a Dell Precision desktop PC with the following characteristics:
* Core: intel Xeon CPU, 3.60 GHz
* RAM memory: 64 GB
* Operating System: Windows 10 Enterprise, version 22H2
* R version: 4.4.2

## References

Gallien, L., Douzet, R., Pratte, S., Zimmermann, N. E., & Thuiller, W. (2012). Invasive species distribution models – how violating the equilibrium assumption can create new insights. Global Ecology and Biogeography, 21(11), 1126–1136. https://doi.org/10.1111/j.1466-8238.2012.00768.x

Huron, N. A., Behm, J. E., & Helmus, M. R. (2022). Paninvasion severity assessment of a U.S. grape pest to disrupt the global wine market. Communications Biology, 5(1), 655. https://doi.org/10.1038/s42003-022-03580-w

Phillips, S. J., Anderson, R. P., & Schapire, R. E. (2006). Maximum entropy modeling of species geographic distributions. Ecological Modelling, 190(3), 231–259. https://doi.org/10.1016/j.ecolmodel.2005.03.026

