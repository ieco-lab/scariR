#'Rescales cloglog suitability output from MaxEnt model as an exponential.
#'
#'
#'@param xy.predicted Data import. The predicted cloglog suitability output
#'taken from one of the internal functions:
#'[slfSpread::predict_xy_suitability()] or
#'[slfSpread::predict_xy_suitability_CV()]. Should be a data.frame or .csv
#'file. If an import, file path should be in the format produced by the
#'[file.path()] function (i.e. with '/' instead of '\\').
#'
#'@param rescale.name Character. Descriptive name to be given to column of
#'rescaled values and the output object name. Exclude unnecessary phrases.
#'
#'@param thresh thresh Numeric or Character. This may be imported manually
#'(numeric), or may be selected from one of the thresholds for the model
#'(character). See details for a list of preset options and other usages.
#'
#'@param summary.file Data import. Contains preset values for thresh. Should be
#'a .csv file or data frame that contains the summary statistics output created by
#'[slfSpread::compute_MaxEnt_summary_statistics()] or
#'[slfSpread::compute_MaxEnt_summary_statistics_CV()] The filename should end in
#'"summary_all_iterations.csv"). If an import, file path should be in the format
#'produced by the [file.path()] function (i.e. with '/' instead of '\\').
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
#'@examples
#'
#'example
#'
#'@export
rescale_cloglog_suit <- function(xy.predicted, rescale.name, thresh, summary.file) {

  # Error checks----------------------------------------------------------------

  # ensure objects are character type
  if (is.character(rescale.name) == FALSE) {
    cli::cli_alert_danger("Parameter 'rescale.name' must be of type 'character'")
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



  # internal function for transformation----------------------------------------

  # function to rescale vector to exponent
  rescale_vector <- function(suit_column_internal, thresh_value_internal) {

    # define min and max
    min_val <- min(suit_column_internal)
    max_val <- max(suit_column_internal)

    # apply log transformation to thresh_value_internal that makes it scale to 0.5
    base_val <- exp(log(0.5) / (thresh_value_internal - min_val))
    # Apply exponential scaling
    scaled_vector <- (base_val ^ (vector - min_val) - 1) / (base_val ^ (max_val - min_val) - 1)

    # return the scaled vector of values
    return(scaled_vector)

  }



  # select data column and apply function---------------------------------------

  # import suit column
  suit_column <- dplyr::select(xy_import[, 4])

  # apply internal function
  rescale_vector_output <- rescale_vector(suit_column, thresh_value)


  # create output
  xy_output <- dplyr::select(xy_import[, 1:3])
  # append new column
  xy_output <- cbind(xy_output, rescale_vector_output) |>
    # rename column
    rename(rescale.name = colnames(xy_output[, 4]))



  # return object and assign name-----------------------------------------------
  return(xy_output)

  assign(xy_output, value = paste0(rescale.name, "_rescaled"))


}
