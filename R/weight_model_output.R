#'Weight model output
#'
#'
#'
#'@param x Data import. Should be applied on across the cells of a raster, so x
#'can be thought of as the imported value of a single cell.
#'
#'@details
#'details
#'
#'
#'@return
#'
#'returns weight
#'
#'@examples
#'
#'example
#'
#'@export
weight_model_output <- function(x) {

  # equation
  weight <- (1 / (1 + (x / (x - 1))^2))

  return(weight)

}
