########## ABOUT ########## 

# slfSpread 
# folder: vignettes
# descriptions and changelog

This README is for the vignettes subfolder, which contains workflows for my package slfSpread. Outputs from these vignettes are stored in "data" or "vignette-outputs".

########## FILES ########## 

----------_ch1_run_native_regional_MaxEnt_model_ .Rmd----------

## About

This vignette creates all data objects needed to run the regional_native model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- in sandbox/vignettes_old
v2- creates the v1 of the regional_native_model in maxent/models. Removed gridSearch and used model settings from global instead.

----------_ch1_run_invaded_regional_MaxEnt_model_ .Rmd----------

## About

This vignette creates all data objects needed to run the regional_invaded model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- in sandbox/vignettes_old
v2- creates the v2 of the regional_invaded_model in maxent/models. Removed gridSearch and used model settings from global instead.

----------_ch1_run_global_MaxEnt_model_ .Rmd----------

## About

This vignette creates all data objects needed to run the global model. It trains the model and uses various functions to create suitability maps, summary statistics, etc.

## Changelog

v0- initial version- in sandbox/vignettes_old
v1- in sandbox/vignettes_old. creates v1 of the global model
v2- creates v2 of the global model- changed procedure for global model cross-validation from k-fold random selection to k-fold blocked selection (multiple papers said this was more rigorous)- added blockCV package usage