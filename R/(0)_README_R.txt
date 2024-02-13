########## ABOUT ########## 

# slfSpread 
# folder: R
# descriptions and changelog

This ReadME is for the R subfolder, which contains custom R functions for the package. 

----------compute_DD_suitability.R----------

## changelog

v1- 2023-11-25- added arguments to convert final output to .tif file



----------compute_MaxEnt_summary_statistics_CV.R----------

## About

This is the original version of this function, but the name was changed (_CV) to indicate the type of object it should be used on (S4 type SDModelCV)
This function will create a directory for and save a MaxEnt model that was run using the `SDMtune` R package. 
It will use the model to calculate a list of summary statistics based on the test data and covariates given.
ONLY works on SDModelCV type models from SDMtune package.

## Changelog

- 2024-02-13- name changed and version without _CV created



----------compute_MaxEnt_summary_statistics.R----------

## About

This is the new adaptation of this function that was designed to be used on SDMtune MaxEnt models without cross-validation (S4 type Maxent)
This function will create a directory for and save a MaxEnt model that was run using the `SDMtune` R package. 
It will use the model to calculate a list of summary statistics based on the test data and covariates given.
ONLY works on S4 Maxent type models from SDMtune package.

## Changelog

- 2024-02-13- this version created to deal with maxent models without cross validation (no _CV)



----------create_MaxEnt_suitability_maps_CV.R----------

## About

This is the original version of this function, but the name was changed (_CV) to indicate the type of object it should be used on (S4 type SDModelCV)
This function will create a map of establishment suitability based on a MaxEnt model trained using the `SDMtune` R package. 
It will optionally create thresholded versions of these maps using thresholds given by the MaxEnt algorithm.

## Changelog

- 2024-02-13- name changed and version without _CV created


----------create_MaxEnt_suitability_maps.R----------

## About

This is the new adaptation of this function that was designed to be used on SDMtune MaxEnt models without cross-validation (S4 type Maxent)
This function will create a map of establishment suitability based on a MaxEnt model trained using the `SDMtune` R package. 
It will optionally create thresholded versions of these maps using thresholds given by the MaxEnt algorithm.

## Changelog

- 2024-02-13- this version created to deal with maxent models without cross validation (no _CV)




----------predict_xy_suitability_CV.R----------


## About

This is the original version of this function, but the name was changed (_CV) to indicate the type of object it should be used on (S4 type SDModelCV)
This function predicts establishment suitability based on a trained MaxEnt model for a set of xy coordinates. 
These coordinates do not need to be within the training area for the model.

## Changelog

- 2024-02-13- name changed and version without _CV created




----------predict_xy_suitability.R----------


## About

This is the new adaptation of this function that was designed to be used on SDMtune MaxEnt models without cross-validation (S4 type Maxent)
This function predicts establishment suitability based on a trained MaxEnt model for a set of xy coordinates. 
These coordinates do not need to be within the training area for the model.

## Changelog

- 2024-02-13- this version created to deal with maxent models without cross validation (no _CV)




