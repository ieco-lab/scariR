###########################
########## ABOUT ##########
###########################

# slfSpread
# folder: vignettes
# descriptions and changelog

This README is for the vignettes subfolder, which contains workflows for my package slfSpread. Outputs from these vignettes are stored in "data" or "vignette-outputs".

###########################
########## FILES ##########
###########################

----------010_initialize_pkg.R----------

## About

Initializes renv, saves data objects for internal use

## Changelog

v0- initial version
v1- 2024-08-26


----------020_retrieve_SLF_records.Rmd----------

## About

retrieves SLF records from various databases and literature sources, harmonizes and compiles them

## Changelog

v0- initial version
v1
v2- 2024-01-05- retired use of spocc package and replaced with rgbif for GBIF workflow
v3- 2024-07-29- rework methods and data update- using new GBIF and lydeMapR data (up until 2023 and including TN and chicago)
v4- 2024-08-05- swapped workflow from scrubr (deprecated) to coordinateCleaner R package, removed Taiwan records based on new paper pre-print


----------030_retrieve_bioclim_variables.Rmd----------

## About

This vignette downloads and tidies the bioclimatic covariates for our models. It also outlines the process for choosing the final set for our models via reducing co-linearity.

## Changelog

v0- initial version
v1
v2
v3- 2024-07-29- rework- updated SLF presence data and inclusion of other SSP scenarios- adds SSP126 and SSP585 to analyses
v4- 2024-10-01- cleaned and simplified workflow.



----------040_setup_global_MaxEnt_model.Rmd----------

## About

Preparations for running the global-scale model, including cropping rasters, choosing random background points, and plotting all data elements.

## Changelog

v0- initial version
v1- 2024-07-31- rework- updated SLF presence data and inclusion of other SSP scenarios- added cropping of other ssp scenario rasters
v2- 2024-10-01- changed projection of maps to Behrmann conical equal-area

----------050_run_global_MaxEnt_model.Rmd----------

## About

This vignette creates all data objects needed to run the global model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- in sandbox/vignettes_old. creates v1 of the global model
v2- creates v2 of the global model- changed procedure for global model cross-validation from k-fold random selection to k-fold blocked selection (multiple papers said this was more rigorous)- added blockCV package usage- LATER: reverted to k-fold random CV because blocked CV did not work- creates
v3- 2024-07-31- rework- updated SLF presence data and inclusion of other SSP scenarios- creates v3 of the global model, fixed some methods for cross validation of model, added other SSP scenario predictions for model



----------051_compute_MaxEnt_summary_statistics_workflow.Rmd----------

## About

This vignette contains an example workflow of the R function "compute_MaxEnt_summary_statistics.R", which uses the model objects from SDMtune to calculate summary statistics.

## Changelog

v0-
v1- 2024-08-27


----------060_setup_regional_MaxEnt_models.Rmd----------

## About

This vignette creates rasters, presence and background point datasets needed for the regional-scale models.

## Changelog

v0- initial version- in sandbox/vignettes_old- trained the invaded model on the entire easternUSA
v1- updated invaded model weighting to use new global model output, updated invaded model background selection to 355km buffer around presences- generated invaded background points v2 and regional_invaded_buffer layers
v2- reverted to eastern USA test area instead of 355km buffer
v3- did not weight easternUSA model. Moved section on weighting to supp materials
v4- added koeppen climate download and use for selecting background points
v5- added table indicating number of training points per model, added plot faceting
v6- 2024-08-02- rework- updated SLF presence data and inclusion of other SSP scenarios
v7- 2024-10-07- changed projection of maps to Behrmann conical equal-area



----------070_run_regional_invaded_MaxEnt_model.Rmd----------

## About

This vignette creates all data objects needed to run the regional_invaded model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- in sandbox/vignettes_old
v2- creates the v2 of the regional_invaded_model in maxent/models. Removed gridSearch and used model settings from global instead.
v3- creates the v3 of the regional_invaded model in maxent/models. In sandbox/vignettes_old
v4- creates the v4 of the regional_invaded model in maxent/models. reverted to regional background method that uses entire eastern USA background area
v5
v6- 2024-08-02- rework- updated SLF presence data and inclusion of other SSP scenarios- changed validation (testing) region to Ri.Asia training region (see citation)- creates v7 of the regional_invaded model



----------080_run_regional_invaded_asian_MaxEnt_model.Rmd----------

## About

This vignette creates all data objects needed to run the regional_invaded_asian model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version
v1-
v2- 2024-08-08- rework- updated SLF presence data and inclusion of other SSP scenarios, creates v2 of the regional_invaded_asian model




----------090_run_regional_native_MaxEnt_model.Rmd----------

## About

This vignette creates all data objects needed to run the regional_native model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- in sandbox/vignettes_old
v2- creates the v1 of the regional_native_model in maxent/models. Removed gridSearch and used model settings from global instead.
v3
v4- 2024-08-08- rework- updated SLF presence data and inclusion of other SSP scenarios, creates v3 of the regional_native model



----------100_run_ExDet_MIC.Rmd----------

## About

This vignette runs the exdet and MIC analyses and saves their outputs. It also creates plots of the outputs.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- added ExDet and MIC for global model
v2- added grey area to each figure to represent bg area
v3- 2024-08-09- rework- updated SLF presence data and inclusion of other SSP scenarios




----------110_ensemble_regional_models.Rmd----------

## About

This vignette takes steps to ensemble the predictions of each regional model into an ensemble. Ensembling is done per time period and ssp scenario. The three ssp scenarios are also ensembled into a final mean / modal prediction. First, rasters are weighted using the AUC and exdet per model and a weighted mean is taken. Next, rasters and MTSS / MTP thresholds are ensembled. MICs are also ensembled. Finally, we analyze the percent contribution of each model to the ensemble.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1-
v2- added section to ensemble ExDet and MIC rasters, bar chart showing % cont to regional ensemble
v3- 2024-08-09- rework- updated SLF presence data and inclusion of other SSP scenarios



----------120_create_suitability_plots.Rmd----------

## About

This vignette intersects the global and regional_ensemble model predictions into categorized suitability maps based on the MTSS threshold.

## Changelog

v0-
v1-
v2- 2024-08-09- rework- updated SLF presence data and inclusion of other SSP scenarios





----------130_create_suitability_xy_plots_viticulture.Rmd----------

## About

This vignette begins data analysis for our MaxEnt by creating suitability scatter plots for locations of viticultural regions.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- using function to transform scatter plot axes
v2-
v3- 2024-08-14- rework- updated SLF presence data and inclusion of other SSP scenarios




----------131_create_suitability_xy_plots_SLF.Rmd----------

## About

This vignette begins data analysis for our MaxEnt by creating suitability scatter plots for SLF point datasets.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- using function to transform scatter plot axes
v2
v3- re-created workflow according to vig 130_v2
v4- 2024-08-14- rework- updated SLF presence data and inclusion of other SSP scenarios



----------140_plot_response_curves.Rmd----------

## About

This vignette plots the response curve outputs from SDMtune as ggplots and adds all ensemble curves to the same plot.

## changelog

v0- initial version
v1- 2024-08-16- rework- updated SLF presence data and inclusion of other SSP scenarios


----------150_generate_risk_report.Rmd----------

## About

This vignette contains example usage for the function "generate_risk_report.R", which creates a risk report for the SLF based on model outputs.

## changelog

v0- initial version
v1- 2024-08-16- rework- updated SLF presence data and inclusion of other SSP scenarios



----------160_generate_format_figures.Rmd----------

## About

This vignette formats risk tables from vignettes 130 and 131 by adding colors and headers.

## changelog

v0- initial version
v1- 2024-08-14- rework- updated SLF presence data and inclusion of other SSP scenarios
v2- changeed name from format_risk_tables to generate_format_figures- combined with generate_extra_plots (161)


