########## ABOUT ########## 

# slfSpread 
# folder: vignettes
# descriptions and changelog

This README is for the vignettes subfolder, which contains workflows for my package slfSpread. Outputs from these vignettes are stored in "data" or "vignette-outputs".

########## FILES ########## 

----------_ch1_run_global_MaxEnt_model.Rmd----------

## About

This vignette creates all data objects needed to run the global model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- in sandbox/vignettes_old. creates v1 of the global model
v2- creates v2 of the global model- changed procedure for global model cross-validation from k-fold random selection to k-fold blocked selection (multiple papers said this was more rigorous)- added blockCV package usage- LATER: reverted to k-fold random CV because blocked CV did not work- creates 



----------_ch1_setup_regional_MaxEnt_models.Rmd----------

## About

This vignette creates rasters and background point datasets needed for both regional-scale models.

## Changelog

v0- initial version- in sandbox/vignettes_old- trained the invaded model on the entire easternUSA
v1- updated invaded model weighting to use new global model output, updated invaded model background selection to 355km buffer around presences- generated invaded background points v2 and regional_invaded_buffer layers
v2- reverted to eastern USA test area instead of 355km buffer



----------_ch1_run_invaded_regional_MaxEnt_model.Rmd----------

## About

This vignette creates all data objects needed to run the regional_invaded model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- in sandbox/vignettes_old
v2- creates the v2 of the regional_invaded_model in maxent/models. Removed gridSearch and used model settings from global instead.
v3- creates the v3 of the regional_invaded model in maxent/models. In sandbox/vignettes_old
v4- creates the v4 of the regional_invaded model in maxent/models. reverted to regional background method that uses entire eastern USA background area



----------_ch1_run_native_regional_MaxEnt_model.Rmd----------

## About

This vignette creates all data objects needed to run the regional_native model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- in sandbox/vignettes_old
v2- creates the v1 of the regional_native_model in maxent/models. Removed gridSearch and used model settings from global instead.





----------_ch1_create_suitability_xy_plots_SLF.Rmd----------

## About

This vignette begins data analysis for our MaxEnt by creating suitability scatter plots for SLF point datasets.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- using function to transform scatter plot axes




----------_ch1_create_suitability_xy_plots_viticulture.Rmd----------

## About

This vignette begins data analysis for our MaxEnt by creating suitability scatter plots for locations of viticultural regions.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- using function to transform scatter plot axes