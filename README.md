# Shifting Climate Assessments for Risk of Invasions
## `scari` R Package Overview <a href="https://ieco-lab.github.io/scari/"><img src="man/figures/logo.png" align="right" height="120" alt="scari website" /></a>

`scari` is an R package and research compendium that documents a multiscale species distribution modeling (SDM) workflow to forecast establishment and impact risk of a species invasion as it shifts with climate change.

We developed this workflow to quantify the shifting risk of future establishment of the invasive species *Lycorma delicatula* (spotted lanternfly or SLF) in important viticulture regions worldwide. The `R` function [create_risk_report](https://github.com/ieco-lab/scari/blob/master/R/create_risk_report.R) produces risk maps, range shift estimates, risk plots and other outputs at the scale of countries or smaller geopolitical units.

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
install.packages(c('cli', 'common', 'CoordinateCleaner', 'devtools', 'dismo', 'ENMTools', 'formattable', 'gginnards', 'gitcreds', 'grid', 'here', 'httr', 'kableExtra', 'kgc', 'patchwork', 'pkgdown', 'plotROC', 'pROC', 'raster', 'rasterVis', 'remotes', 'renv', 'rgbif', 'rJava', 'rmarkdown', 'rnaturalearth', 'rnaturalearthhires', 'scales', 'SDMtune', 'sf', 'sp', 'stats', 'stringr', 'terra', 'tidygeocoder', 'tidyverse', 'usethis', 'utils', 'viridis', 'webshot', 'webshot2'))

# we also suggest installing the following packages:
install.packages("blockCV")

# Install package which cannot be obtained from the CRAN
library(devtools)
remotes::install_github("ieco-lab/lydemapr", build_vignettes = FALSE)
remotes::install_github("ropensci/rnaturalearthdata")
remotes::install_github("ropensci/rnaturalearthhires")
remotes::install_github("densitymodelling/dsmextra")

# install specific versions of packages
remotes::install_version("ggnewscale", version = "0.4.10") # due to issue with color scales
remotes::install_version("GeoThinneR", version = "1.1.0") # due to issue with thin_points
```

### Sitemap

This project is organized into general sections of our modeling pipeline: Our [vignettes](https://github.com/ieco-lab/scari/tree/master/vignettes) follow this general order: 

* 010: Initialize `scari`: initialization of `renv` package for dependencies
* 020-030: Retrieve and tidy input data for MaxEnt
* 040-090: SDM modeling pipeline: train global and 3 regional-scale models
* 100-110: Ensemble Regional-scale SDMs
* 120-142: Quantify SLF risk and model fit
* 150: Quantify risk to specific viticultural regions using our function [create_risk_report()](https://github.com/ieco-lab/scari/blob/master/vignettes/150_create_risk_report.Rmd)

## How to Use this Project

Before diving into this project and our modeling workflow, an end user should:
1. read the [companion paper](), which outlines the conceptual underpinnings for this project
2. download, build and install this package locally
3. install the package `renv`, which ensures that R package versions are consistent for running this package (this creates a project-specific R package library, so it should not affect your main library)
4. run the first vignette, [010_initialize_renv](https://github.com/ieco-lab/scari/blob/master/vignettes/010_initialize_pkg.Rmd), which initializes `renv` and lists our package's dependencies.
5. See "Get Started" for help in using our package to:
  - Produce localized reports on SLF risk to viticulture, and
  - Recreate our analysis for another invasive species of interest

### Computing Information

This package was developed and its vignettes were rendered on a Dell Precision desktop PC with the following characteristics:
* Core: intel Xeon CPU, 3.60 GHz
* RAM memory: 64 GB
* Operating System: Windows 11 Enterprise, version 24H2
* R version: 4.5.1

### Other software:

Be sure to download and install Java to use this package: https://www.oracle.com/java/technologies/downloads/#java24. Be sure to install the correct bit version for your OS.


## References

Gallien, L., Douzet, R., Pratte, S., Zimmermann, N. E., & Thuiller, W. (2012). Invasive species distribution models – how violating the equilibrium assumption can create new insights. Global Ecology and Biogeography, 21(11), 1126–1136. https://doi.org/10.1111/j.1466-8238.2012.00768.x

Huron, N. A., Behm, J. E., & Helmus, M. R. (2022). Paninvasion severity assessment of a U.S. grape pest to disrupt the global wine market. Communications Biology, 5(1), 655. https://doi.org/10.1038/s42003-022-03580-w

Phillips, S. J., Anderson, R. P., & Schapire, R. E. (2006). Maximum entropy modeling of species geographic distributions. Ecological Modelling, 190(3), 231–259. https://doi.org/10.1016/j.ecolmodel.2005.03.026

