#'Calculate placement on risk quadrant plot
#'
#'@description This function calculates the risk of establishment by Lycorma
#'delicatula according to our risk quadrant framework (which quadrant a suitability
#'value will fall into on an xy-scatter). It requires the input of a matrix of
#'suitability values and a corresponding suitability threshold for each of the x
#'and y axes for this plot.
#'
#'@param suit.x Suitability values comprising the x-axis.
#'
#'@param suit.y Suitability values comprising the y-axis.
#'
#'@param thresh.x The threshold for suitability along the x-axis.
#'
#'@param thresh.y The threshold for suitability along the y-axis.
#'
#'@details details
#'
#'
#'@return
#'
#'One of: "extreme", "high", "moderate", or "low". These correspond with our
#'interpretation of the risk quadrant plots we have created:
#'
#'* extreme risk = quadrant 4 (upper right)
#'* high risk = quadrant 3 (upper left)
#'* moderate risk = quadrant 2 (bottom right)
#'* low risk = quadrant 1 (bottom left)
#'
#'@examples example
#'
#'@export
calculate_risk_quadrant <- function(suit.x, suit.y, thresh.x, thresh.y) {

  ## error checks---------------------------------------------------------------

  # ensure objects are numeric type
  if (is.numeric(thresh.x) == FALSE) {
    cli::cli_alert_danger("Parameter 'thresh.x' must be of type 'numeric'")
    stop()
  }

  if (is.numeric(thresh.y) == FALSE) {
    cli::cli_alert_danger("Parameter 'thresh.y' must be of type 'numeric'")
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
  risk_output <- case_when(
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
