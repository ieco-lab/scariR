#'Rescales cloglog suitability output from MaxEnt model as an exponential.
#'
#'@description
#'This function will take the cloglog suitability output from MaxEnt, which is
#'on a 0-1 scale, and re-scale it around a set median value. The range of the
#'values will still be 0-1, but the median will now be the value of the `thresh`
#'parameter. The scaling function is applied as an exponential, for the purposes
#'of visualizing suitability change around a critical threshold (at the scale of
#'0-1, it can often be hard to see changes across the suitability threshold if
#'its value is very small). THis function will not work correctly if the desired
#'threshold value is 0.
#'
#'This function and some of its inputs were co-authored by Jacob Woods.
#'
#'@param xy.predicted Data import. A predicted cloglog suitability with
#'longitude and latitude data. This output should be taken from one of the
#'internal package functions: [scarifSDM::predict_xy_suitability()] or
#'[scarifSDM::predict_xy_suitability_CV()]. See details for specifics of formatting.
#'
#'@param thresh Numeric or Character. This may be imported manually
#'(numeric), or may be selected from one of the thresholds for the model
#'(character). See details for a list of preset options and other usages.
#'
#'@param exponential.file Data import. Should be a .csv file or data frame. Contains
#'a comprehensive list of possible threshold values and the corresponding c1, c2
#'ans c3 values. Where x is equal to the original suitability value and y is
#'equal to the transformed version of that suitability value, these values are
#'used in the exponential function "y = c1 * c2^x + c3".
#'
#'@param summary.file Data import. Should be a .csv file or data frame.
#'Contains preset values for thresholds. If an import, file path should be in
#'the format produced by the [file.path()] function (i.e. with '/' instead of '\\').
#'
#'For a global or regional model, this will be a file containing summary
#'statistics that is created by one of the following internal package functions:
#'[scarifSDM::compute_MaxEnt_summary_statistics()], with a filename ending in
#'"_summary.csv", or
#'[scarifSDM::compute_MaxEnt_summary_statistics_CV()], with a filename ending in
#'"_summary_all_iterations.csv".
#'
#'For the regional_ensemble model, this file should end in "_threshold_values.csv"
#'
#'@param rescale.name Character. Descriptive name to be given to column of
#'rescaled values.
#'
#'@param rescale.thresholds Logical. If true, the function will also rescale
#'the list of thesholds given in the thresh presets list. All other thresholds
#'are rescaled with reference to the threshold specified by `thresh`. This is
#'advised if visualizing with multiple thresholds.
#'
#'@details
#'
#'Requires the following packages: 'tidyverse', 'cli'.
#'
#'This function is based on an exponential equation of the form:
#'
#'f(x) = c_1 * c_2^x + c_3
#'
#'where x is the input suitability value and f(x) is the transformed value of x
#'with respect to `thresh` as the median of the range. The function calls a
#'reference table to transform the thresh (z) into its respective value f(z) = 0.5
#'on the exponential scale. The values for the exponential equation were found
#'using the equation:
#'
#'log((0.5 - c_3) / c1) / log(c_2)
#'
#'## xy.predicted
#'
#'Fromatting: Should be a data.frame or .csv file. The columns should be in the
#'order: descriptive columns, x (longitude), y (latitude), cloglog suitability
#'(last column). If an import, file path should be in the format produced by the
#'[file.path()] function (i.e. with '/' instead of '\\').
#'
#'## thresh:
#'
#'Thresh presets list:
#'* `MTP` = Minimum Training Presence
#'* `MTP.CC` = (regional_ensemble model only) Minimum Training Presence, transformed for climate predictions.
#'* `MTSS` = Maximum training sensitivity plus specificity
#'* `MTSS.CC` = (regional_ensemble model only) Maximum training sensitivity plus specificity, transformed for climate predictions.
#'
#'@return
#'
#'Returns the input data frame with an additional column containing the re-scaled
#'exponential values for the cloglog suitability.
#'
#'If rescale.thresholds = TRUE, will return a list of data frames. The first
#'object in the list will be the rescaled suitability values, and the second
#'object in the list will be the rescaled thresholds.
#'
#'@examples
#'
#'# EXAMPLE USAGE:
#'
#'```R
#'
#'xy_regional_ensemble_1995_rescaled <- scarifSDM::rescale_cloglog_suitability(
#' xy.predicted = xy_regional_ensemble_1995,
#' thresh = "MTSS", # max training and sensitivity plus specificity
#' exponential.file = file.path(here::here(), "data-raw", "threshold_exponential_values.csv"),
#' summary.file = summary_regional_ensemble,
#' rescale.name = "xy_regional_ensemble_1995",
#' rescale.thresholds = TRUE
#')
#'
#'# if rescale.thresholds = TRUE, the output will be a list of two data frames that will need to be separated
#'
#'xy_regional_ensemble_1995_rescaled_thresholds <- xy_regional_ensemble_1995_rescaled[[2]]
#'xy_regional_ensemble_1995_rescaled <- xy_regional_ensemble_1995_rescaled[[1]]
#'
#'```
#'
#'@export
rescale_cloglog_suitability <- function(xy.predicted, thresh, exponential.file, summary.file, rescale.name = NA, rescale.thresholds = FALSE) {

  # Error checks----------------------------------------------------------------

  # ensure objects are character type
  if (!is.na(rescale.name) & is.character(rescale.name) == FALSE) {
    cli::cli_abort("Parameter 'rescale.name' must be of type 'character'")
    stop()
  }

  # ensure objects are logical type
  if (is.logical(rescale.thresholds) == FALSE) {
    cli::cli_abort("Parameter 'rescale.thresholds' must be of type 'logical'")
    stop()
  }


  # import settings for files---------------------------------------------------

  # xy.predicted
  if (is.character(xy.predicted)) {
    xy_import <- read.csv(xy.predicted) # read as csv

  } else {
    xy_import <- as.data.frame(xy.predicted) # make data frame
  }


  # summary.file
  if (is.character(summary.file)) {
    thresh_preset_import <- read.csv(summary.file) # read as csv

  } else {
    thresh_preset_import <- as.data.frame(summary.file) # make data frame
  }


  # exponential.file
  if (is.character(exponential.file)) {
    exponential_import <- read.csv(exponential.file) # read as csv

 # } else if(exists("exponential.file")) { # if it is already in environment, import as is
  #  exponential_import <- exponential.file

  } else {
    exponential_import <- as.data.frame(exponential.file) # make data frame
  }

  # thresh presets--------------------------------------------------------------
  # conditional import to ensure proper thresh preset is used
  if (thresh == "MTP.CC" && str_detect(summary.file, "_summary.csv") == TRUE) {
    cli::cli_abort("MTP.CC can only be used with a thresh imported for the 'regional_ensemble' model")
    stop()

  } else if (length(thresh) == 1 && thresh == "MTSS.CC" && any(str_detect(summary.file, "_iteration"))) {
    cli::cli_abort("MTSS.CC can only be used with a thresh imported for the 'regional_ensemble' model")
    stop()
  }

  # conditional for presets import.
  # if the thresh import fits the specification of the global model output, import these values
  if(nrow(thresh_preset_import) == 52 && colnames(thresh_preset_import)[1] == "statistic") {

    thresh_presets <- c(
      "MTP" = as.numeric(thresh_preset_import[30, ncol(thresh_preset_import)]), # Minimum.training.presence.Cloglog.threshold
      "MTSS" = as.numeric(thresh_preset_import[42, ncol(thresh_preset_import)]) # Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
    )

    # else if the thresh import fits the specifications of the regional_ensemble output, import these values
  } else if (nrow(thresh_preset_import) > 6 & nrow(thresh_preset_import) < 52 && ncol(thresh_preset_import) < 7) {

    thresh_presets <- c(
      "MTP" = (dplyr::filter(thresh_preset_import, thresh == "MTP") %>%  # Minimum.training.presence.Cloglog.threshold
                 # above: filter out MTP rows
                 # below: select first row (hist value)
                dplyr::slice_head() %>%
                 # get the value col
                dplyr::select(4) %>%
                as.numeric()
               ),
      "MTSS" = (dplyr::filter(thresh_preset_import, thresh == "MTSS") %>%  # Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
                  dplyr::slice_head() %>%
                  dplyr::select(4) %>%
                  as.numeric()
                ),
      # only used for format of thresh file from regional_ensemble model- this the MTSS for the historical data
      "MTP.CC" = (dplyr::filter(thresh_preset_import, thresh == "MTP") %>%  # MTP transformed for climate change- this is the MTP for the mean ssp scenario
                    dplyr::slice_tail() %>%
                    # select the last row (the CC mean value)
                    dplyr::select(4) %>%
                    as.numeric()
                  ),
      "MTSS.CC" = (dplyr::filter(thresh_preset_import, thresh == "MTSS") %>%  # MTSS transformed for climate change- this is the MTSS for the mean ssp scenario
                     dplyr::slice_tail() %>%
                     dplyr::select(4) %>%
                     as.numeric())
    )

    # DEPRECATED
    # else if the thresh import fits the specifications of the regional_ensemble output, import these values
 # } else if (nrow(thresh_preset_import) <= 2 && ncol(thresh_preset_import) < 7) {
   # thresh_presets <- c(
  #    "MTP" = as.numeric(thresh_preset_import[1, 2]), # Minimum.training.presence.Cloglog.threshold
   #   "MTSS" = as.numeric(thresh_preset_import[2, 2]) # Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
   # )
    # else, print a warning message
  } else {
    cli::cli_abort("'summary.file' import does not fit the expected parameters. The summary file for a global-scale model should be nrow == 52 and a summary file for a regional-scale model should be nrow = 10")
    stop()

  }

  # thresh import---------------------------------------------------------------

  if(is.numeric(thresh)) {
    thresh_value <- thresh

    # if a preset, import value of preset
  } else if(is.element(thresh, names(thresh_presets))){
    thresh_value <- thresh_presets[thresh]

    # otherwise, stop and give warning
  } else {
    cli::cli_alert_info(paste0("'thresh' must be numeric or one of: \n    ", paste(names(thresh_presets), collapse = ' | ')))
    stop()

  }

  # include all decimal places
  thresh_value <- as.double(format(thresh_value, digits = 10))

  # ensure thresh isnt 0
  if (thresh_value <= 0) {
    cli::cli_abort("The value of parameter 'thresh' must be greater than 0")
    stop()

  } else if (is.na(thresh_value)) {
    cli::cli_abort("The imported summary file is likely formatted incorrectly (the number of rows should equal 10)")
    stop()

  }


  # internal function for transformation----------------------------------------

  # function to rescale vector to exponent
  # the range is always 0-1
  rescale_vector <- function(suit_column_internal, thresh_val_internal) {

    # first, find the thresh value in exponential_import that is closest to thresh_val_internal
    # throwaway function to calculate the closest value, which returns the row number of the value that is closest to the s_val specified value
    find_closest <- function(x_val, s_val) {

     which.min(abs(x_val - s_val))

      }

    # apply function to the 1st column of the exponential_import table
    thresh_val_closest <- find_closest(
      x_val = exponential_import[, 1],
      s_val = thresh_val_internal
    )

    # isolate row containing proper c values
    c_values <- dplyr::slice(exponential_import, thresh_val_closest)

    # apply exponential to vector
    scaled_vector <- as.numeric(c_values$c1.value) * (as.numeric(c_values$c2.value)^suit_column_internal) + as.numeric(c_values$c3.value)

    # return the scaled vector of values
    return(scaled_vector)

  }

  # select data column and apply function---------------------------------------

  # import suit column
  suit_column <- dplyr::select(xy_import, last_col())

  # apply internal function
  rescale_vector_output <- rescale_vector(
    suit_column_internal = suit_column,
    thresh_val_internal = thresh_value
    )

  # create output
  xy_output <- xy_import
  # append new column
  xy_output <- cbind(xy_output, rescale_vector_output)
  # rename final column
  names(xy_output)[length(names(xy_output))] <- ifelse(!is.na(rescale.name), paste0(rescale.name, "_rescaled"), "cloglog_suitability_rescaled")


  # conditional output----------------------------------------------------------

  # if this is true, also re-scale the thresholds from the summary.file and return with the suitability output
  if(rescale.thresholds == TRUE) {

    # convert to df
    thresh_presets_tmp <- as.data.frame(thresh_presets)

    # rescale
    thresh_output <- rescale_vector(
      suit_column_internal = thresh_presets_tmp,
      thresh_val_internal = thresh_value
      )

    # convert rownames to column
    thresh_output <- rownames_to_column(thresh_output, var = "threshold") |>
      dplyr::rename("value" = "thresh_presets")

    # output
    xy_thresh_output <- list("cloglog_suitability" = xy_output, "thresholds" = thresh_output)

    return(xy_thresh_output)

  # otherwise, just return the re-scaled suitability output output
    } else {

       return(xy_output)

    }

}
