###########################
########## ABOUT ##########
###########################

# scari
# folder: R
# descriptions and changelog

This README is for the R subfolder, which contains custom R functions for the package.

####################################
########## FUNCTIONS LIST ##########
####################################

----------calculate_risk_quadrant.R----------

## About

Calculates placement of point-wise suitability values on risk quadrant plot.
First used in vignette 130

## Changelog

v0- initial version



----------compute_MaxEnt_summary_statistics_CV.R----------

## About

This is the original version of this function, but the name was changed (_CV) to indicate the type of object it should be used on (S4 type 'SDModelCV')

This function will create a directory for and save a MaxEnt model that was run using the `SDMtune` R package.
It will use the model to calculate a list of summary statistics based on the test data and covariates given.
ONLY works on SDModelCV type models from SDMtune package.

First used in vignette 050

## Changelog

v0-
v1- 2024-02-13- name changed and version without _CV created



----------compute_MaxEnt_summary_statistics.R----------

## About

This is the new adaptation of this function that was designed to be used on SDMtune MaxEnt models without cross-validation (S4 type 'Maxent')
This function will create a directory for and save a MaxEnt model that was run using the `SDMtune` R package.
It will use the model to calculate a list of summary statistics based on the test data and covariates given.
ONLY works on S4 Maxent type models from SDMtune package.

First used in vignette 070

## Changelog

v0-
v1- 2024-02-13- this version created to deal with maxent models without cross validation (no _CV)




----------create_MaxEnt_suitability_maps_CV.R----------

## About

This is the original version of this function, but the name was changed (_CV) to indicate the type of object it should be used on (S4 type SDModelCV)
This function will create a map of establishment suitability based on a MaxEnt model trained using the `SDMtune` R package.
It will optionally create thresholded versions of these maps using thresholds given by the MaxEnt algorithm.

First used in vignette 050

## Changelog

v0-
v1- 2024-02-13- name changed and version without _CV created




----------create_MaxEnt_suitability_maps.R----------

## About

This is the new adaptation of this function that was designed to be used on SDMtune MaxEnt models without cross-validation (S4 type Maxent)
This function will create a map of establishment suitability based on a MaxEnt model trained using the `SDMtune` R package.
It will optionally create thresholded versions of these maps using thresholds given by the MaxEnt algorithm.

First used in vignette 070

## Changelog

v0-
v1- 2024-02-13- this version created to deal with maxent models without cross validation (no _CV)



----------create_risk_report.R----------

## About

This function creates a report at the country or state/province level for the risk of establishment of Lycorma delicatula.
The report covers the major data outputs from this R package analysis, including current and future risk maps, range shift maps, risk plots and risk tables.

First used in vignette 160

## Changelog

v0-
v1-
v2-
v3-
v4- 2024-08-29- added functionality to calculate totals of predicted SLF risk categories from risk maps



----------rescale_cloglog_suitability.R----------

## About

This function will take the cloglog suitability output from MaxEnt, which is on a 0-1 scale, and re-scale it to have a set median value.
The range of the values will still be 0-1, but the median will now be the value of the `thresh` parameter.
The scaling function is applied as an exponential, for the purposes of visualizing suitability change around a critical threshold
(at the scale of 0-1, it can often be hard to see changes across the suitability threshold if its value is very small).
Note that this function will not work if the value of thresh is 0.

First used in vignette 130

## Changelog

v0- initial version






----------predict_xy_suitability_CV.R----------


## About

This is the original version of this function, but the name was changed (_CV) to indicate the type of object it should be used on (S4 type SDModelCV)
This function predicts establishment suitability based on a trained MaxEnt model for a set of xy coordinates.
These coordinates do not need to be within the training area for the model.

First used in vignette 050

## Changelog

v0- 2024-02-13- name changed and version without _CV created
v1- 2024-08-07- added method to make predictions based on buffer around points- take the max value within the buffer rather than the exact point value




----------predict_xy_suitability.R----------


## About

This is the new adaptation of this function that was designed to be used on SDMtune MaxEnt models without cross-validation (S4 type Maxent)
This function predicts establishment suitability based on a trained MaxEnt model for a set of xy coordinates.
These coordinates do not need to be within the training area for the model.

First used in vignette 070

## Changelog

v0- 2024-02-13- this version created to deal with maxent models without cross validation (no _CV)
v1- 2024-08-07- added method to make predictions based on buffer around points- take the max value within the buffer rather than the exact point value





############################################################
########## Supp Materials and unfinished analyses ##########
############################################################

----------compute_DD_suitability.R----------

## changelog

v1- now in sandbox/supp_materials- 2023-11-25- added arguments to convert final output to .tif file



----------weight_model_output.R----------

## About

Calculates an inverse weight function to be applied to rasters using terra::app().

## changelog

v0- now in sandbox/supp_materials


----------convert_to_celsius.R----------

## About

Written specifically to convert monthly temperature data retrieved from CHELSA from Kelvin to Celsius.

## changelog

v0- now in sandbox/supp_materials



################################
########## Deprecated ##########
################################

----------map_slf.R----------

## About

Creates a leaflet map of SLF presence points

## changelog

v0- now in sandbox/slfLocate


----------pesthub_compile.R----------

## About

Compiles different pesthub data frames into a single data frame, writted for CDFA reporting.

## changelog

v0- now in sandbox/slfLocate

