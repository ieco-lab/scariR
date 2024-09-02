#'Calculate placement of point-wise suitability values on risk quadrant plot
#'
#'@description This function categorizes the risk of establishment by Lycorma
#'delicatula according to our risk quadrant framework, which is the intersection
#'between predictions made by the global and regional-scale models. It will return
#' which quadrant a suitability value will fall into on an xy-scatter.
#'
#'@param suit.x Suitability values comprising the x-axis. Global-scale model is
#'usually placed along the x-axis.
#'
#'@param suit.y Suitability values comprising the y-axis. Regional-scale ensemble
#'model is usually placed along the x-axis.
#'
#'@param thresh.x The threshold for suitability accompanying the x-axis model
#'values.
#'
#'@param thresh.y The threshold for suitability accompanying the y-axis model
#'values.
#'
#'@details
#'
#'#'The function requires the packages 'dplyr' and 'cli'.
#'
#'Input data formats:
#'* param "suit" = matrix of suitability values
#'* param "thresh" = numeric
#'
#'Input suitability data can be created using the function `predict_xy_suitability`,
#'or extracted from a raster using `terra::extract()`, as needed.
#'
#'@return
#'
#'Returns a data frame. Values will be one of: "extreme", "high", "moderate",
#'or "low". These correspond with our interpretation of the risk quadrant plots we have created:
#'
#'* extreme risk = quadrant 4 (upper right)
#'* high risk = quadrant 3 (upper left)
#'* moderate risk = quadrant 2 (bottom right)
#'* low risk = quadrant 1 (bottom left)
#'
#'@examples
#'
#'# I typically use it with dplyr::mutate() to create a new column in a data frame.
#'
#'IVR_locations_risk <- dplyr::mutate(IVR_locations_risk, risk_1995 = slfSpread::calculate_risk_quadrant(
#'  suit.x = IVR_locations_joined$xy_global_1995_rescaled,
#'    suit.y = IVR_locations_joined$xy_regional_ensemble_1995_rescaled,
#'    thresh.x = global_MTSS, # this threshold remains the same
#'    thresh.y = regional_ensemble_MTSS_1995
#'    ))
#'
#'@export
calculate_risk_quadrant <- function(suit.x, suit.y, thresh.x, thresh.y) {

  ## error checks---------------------------------------------------------------

  # ensure objects are numeric type
  if (is.numeric(thresh.x) == FALSE) {
    cli::cli_abort("Parameter 'thresh.x' must be of type 'numeric'")
    stop()
  }

  if (is.numeric(thresh.y) == FALSE) {
    cli::cli_abort("Parameter 'thresh.y' must be of type 'numeric'")
    stop()
  }

  ## import settings------------------------------------------------------------

  # suit.x
  if (is.character(suit.x)) {
    suit_x <- read.csv(suit.x) # read as csv

  } else {
    suit_x <- as.data.frame(suit.x) # make data frame
  }

  # suit.y
  if (is.character(suit.y)) {
    suit_y <- read.csv(suit.y) # read as csv

  } else {
    suit_y <- as.data.frame(suit.y) # make data frame
  }


  ## function-------------------------------------------------------------------

  # apply case_when
  risk_output <- dplyr::case_when(
    suit_x >= thresh.x & suit_y >= thresh.y ~ "extreme",
    suit_x < thresh.x & suit_y >= thresh.y  ~ "high",
    suit_x >= thresh.x & suit_y < thresh.y  ~ "moderate",
    suit_x < thresh.x & suit_y < thresh.y   ~ "low"
  )

  # convert to df
  as.data.frame(risk_output)

  # return
  return(risk_output)
}
