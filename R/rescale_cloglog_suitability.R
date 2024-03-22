#'Rescales cloglog suitability output from MaxEnt model as an exponential.
#'
#'@description
#'This function will take the cloglog suitability output from MaxEnt, which is
#'on a 0-1 scale, and re-scale it to have a set median value. The range of the
#'values will still be 0-1, but the median will now be the value of the `thresh`
#'parameter. The scaling function is applied as an exponential, for the purposes
#'of visualizing suitability change around a critical threshold (at the scale of
#'0-1, it can often be hard to see changes across the suitability threshold if
#'its value is very small). Note that this function will not work if the value
#'of thresh is 0.
#'
#'This function was co-authored by Jacob Woods.
#'
#'@param xy.predicted Data import. The predicted cloglog suitability output
#'taken from one of the internal package functions:
#'[slfSpread::predict_xy_suitability()] or
#'[slfSpread::predict_xy_suitability_CV()]. Should be a data.frame or .csv
#'file. If an import, file path should be in the format produced by the
#'[file.path()] function (i.e. with '/' instead of '\\').
#'
#'@param thresh Numeric or Character. This may be imported manually
#'(numeric), or may be selected from one of the thresholds for the model
#'(character). See details for a list of preset options and other usages.
#'
#'@param summary.file Data import. Contains preset values for thresh and summary
#'statistics and is created by the internal package funcions:
#'[slfSpread::compute_MaxEnt_summary_statistics()] or
#'[slfSpread::compute_MaxEnt_summary_statistics_CV()] The filename should end in
#'"summary_all_iterations.csv"). Should be a .csv file or data frame. If an
#'import, file path should be in the format produced by the [file.path()]
#'function (i.e. with '/' instead of '\\').
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
#'## thresh:
#'
#'Thresh presets list:
#'* `BTO` = Balance training omission predicted area and threshold value
#'* `EE` = Equate entropy of thresholded and original distributions
#'* `ETSS` = Equal training sensitivity and specificity
#'* `MTP` = Minimum Training Presence
#'* `MTSS` = Maximum training sensitivity plus specificity
#'* `ten_percentile` or `10_percentile` = Ten percentile training presence
#'
#'
#'@return
#'
#'Returns the input data frame, with the column of cloglog suitability values
#'replaced by the re-scaled exponential values.
#'
#'If rescale.thresholds = TRUE, will return a list of data frames. The first
#'object in the list will be the rescaled suitability values, and the second
#'object in the list will be the rescaled thresholds.
#'
#'@examples
#'
#'example
#'
#'@export
rescale_cloglog_suitability <- function(xy.predicted, thresh, summary.file, rescale.name = NA, rescale.thresholds = FALSE) {

  # Error checks----------------------------------------------------------------

  # ensure objects are character type
  if (!is.na(rescale.name) & is.character(rescale.name) == FALSE) {
    cli::cli_alert_danger("Parameter 'rescale.name' must be of type 'character'")
    stop()
  }

  # ensure objects are logical type
  if (is.logical(rescale.thresholds) == FALSE) {
    cli::cli_alert_danger("Parameter 'rescale.thresholds' must be of type 'logical'")
    stop()
  }


  # import for xy.predicted-----------------------------------------------------

  # import settings
  if (is.character(xy.predicted)) {
    xy_import <- read.csv(xy.predicted) # read as csv

  } else {
    xy_import <- as.data.frame(xy.predicted) # make data frame

  }



  # import for summary.file-----------------------------------------------------

  if (is.character(summary.file)) {
    thresh_preset_import <- read.csv(summary.file) # read as csv

  } else {
    thresh_preset_import <- as.data.frame(summary.file) # make data frame

  }



  # thresh presets and import---------------------------------------------------

  thresh_presets <- c(
    "MTP" = as.numeric(thresh_preset_import[30, ncol(thresh_preset_import)]), # Minimum.training.presence.Cloglog.threshold
    "ten_percentile" = as.numeric(thresh_preset_import[34, ncol(thresh_preset_import)]), # 10.percentile.training.presence.Cloglog.threshold
    "10_percentile" = as.numeric(thresh_preset_import[34, ncol(thresh_preset_import)]), # 10.percentile.training.presence.Cloglog.threshold
    "ETSS" = as.numeric(thresh_preset_import[38, ncol(thresh_preset_import)]), # Equal.training.sensitivity.and.specificity.Cloglog.threshold
    "MTSS" = as.numeric(thresh_preset_import[42, ncol(thresh_preset_import)]), # Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
    "BTO" = as.numeric(thresh_preset_import[46, ncol(thresh_preset_import)]), # Balance.training.omission..predicted.area.and.threshold.value.Cloglog.threshold
    "EE" = as.numeric(thresh_preset_import[50, ncol(thresh_preset_import)]) # Equate.entropy.of.thresholded.and.original.distributions.Cloglog.threshold
  )


  # thresh import
  if(is.numeric(thresh)) {
    thresh_value <- thresh

    # if a preset, import value of preset
  } else if(is.element(thresh, names(thresh_presets))){
    thresh_value <- thresh_presets[thresh]

    # otherwise, stop and give warning
  } else {
    stop(paste0("'thresh' must be numeric or one of: \n    ", paste(names(thresh_presets), collapse = ' | ')))

  }

  # include all decimal places
  thresh_value <- as.double(format(thresh_value, digits = 10))


  # ensure thresh isnt 0
  if (thresh_value == 0) {
    cli::cli_alert_danger("The value of parameter 'thresh' must be greater than 0")
    stop()
  }


  # internal function for transformation----------------------------------------

  # function to rescale vector to exponent
  rescale_vector <- function(suit_column_internal, thresh_val_internal, min_val = NA, max_val = NA) {

    # conditional min and max settings
    # if specified, import
    if(!is.na(min_val) & !is.na(max_val)) {

      min_val_internal <- min_val
      max_val_internal <- max_val

      # otherwise, use the range of the suitability dataset
    } else {

      min_val_internal <- min(suit_column_internal) # make 0
      max_val_internal <- max(suit_column_internal) # make 1

    }

    # function

    # apply log transformation to thresh_val_internal that makes it scale to 0.5
    base_val <- exp(log(0.5) / (0.00000000000001 + thresh_val_internal - min_val_internal))
    # Apply exponential scaling
    scaled_vector <- (base_val ^ (suit_column_internal - min_val_internal) - 1) / (base_val ^ (max_val_internal - min_val_internal) - 1)

    # return the scaled vector of values
    return(scaled_vector)

  }

  # select data column and apply function---------------------------------------

  # import suit column
  suit_column <- dplyr::select(xy_import, 4)

  # apply internal function
  rescale_vector_output <- rescale_vector(
    suit_column_internal = suit_column,
    thresh_val_internal = thresh_value)

  # create output
  xy_output <- dplyr::select(xy_import, 1:3)
  # append new column
  xy_output <- cbind(xy_output, rescale_vector_output)
  # rename column
  colnames(xy_output)[4] <- ifelse(!is.na(rescale.name), paste0(rescale.name, "_rescaled"), "cloglog_suitability_rescaled")


  # conditional output----------------------------------------------------------

  # if this is true, also re-scale the thresholds from the summary.file and return with the suitability output
  if(rescale.thresholds == TRUE) {

    # convert to df
    thresh_presets_tmp <- as.data.frame(thresh_presets)

    # rescale
    thresh_output <- rescale_vector(
      suit_column_internal = thresh_presets_tmp,
      thresh_val_internal = thresh_value,
      # use the range from the suitability dataset instead of the thresholds dataset
      min_val = min(suit_column),
      max_val = max(suit_column)
      )

    # convert rownames to column
    thresh_output <- rownames_to_column(thresh_output, var = "threshold") |>
      rename("value" = "thresh_presets")

    # output
    xy_thresh_output <- list("cloglog_suitability" = xy_output, "thresholds" = thresh_output)

    return(xy_thresh_output)

  # otherwise, just return the re-scaled suitability output output
    } else {

       return(xy_output)

    }

}
