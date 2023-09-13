#'Create a [leaflet] map of SLF occurrences
#'
#'This function is designed to map a set of coordinates of interest using the
#'leaflet package.
#'
#'using the leaflet package
#'and in this package, it is used to map new SLF jump locations. It is designed
#'to show the point with a specified latitude and longitude margin on all sides
#'of the point (zoom level). It is suggested that this function be used to
#'append mapping data to a pre-existing data frame or tibble.
#'
#'#'The function requires the packages 'tidyverse' and 'leaflet'.
#'
#'@param x Data import. Should be a .csv file or data frame that contains
#'input temperature data. If a .csv file, file path should be in the format
#'produced by the [file.path()] function (i.e. with '/' instead of '\\').
#'See details for additional import formatting.
#'
#'@param point_lon,point_lat
#'
#'@param point_label
#'
#'@return A logical value (\code{TRUE} or \code{FALSE}) that answers the question
#'"is any element of the vector not an NA.
#'
#'@details
#'This function was previously called map_jumps_v2. Major changes from v1:
#'Specified all variables as arguments- allows this function to be used in
#'context of a single point, rather than just a data frame or tibble.
#'
#'@examples
#'
#'library(leaflet)
#'library(tidyverse)
#'
#'map_slf <-
#'
#'

# Arguments:

# Point_lon and point_lat: represent the longitude and latitude of the point you want to map
# Point_label:  whatever you assign to be the label of that point
# Extent: the width and height on either side of each point that you want mapped, in degrees latitude or longitude
# Zoom: allows you to set the zoom level, if you want tot have a different one-
  # note: even if you choose to use fitBounds() to zoom in on the point, a zoom level must still be established within setView().
  # fitBounds() would then override the zoom level

# Major changes from v1:

# Specified all variables as arguments- allows this function to be used in context of a single point, rather than just a data frame or tibble

map_slf <- function(point_lon, point_lat, point_label, extent = 0.01, zoom = 10) {

  # create icon for SLF
  slf_marker <- makeAwesomeIcon(
    icon = "bug",
    iconColor = "black",
    markerColor = "orange",
    library = "fa"
  )

  # read in data object or file
  # function defaults to read_csv if the parameter "x" is a character vector
  # (assumes this represents a file path). Otherwise, it is loaded in from the
  # environment
  if(is.character(x)) {
    data <- read_csv(x)
  } else {
    data <- x
  }

  # create leaflet map
  leaflet() %>%
    setView(lng = point_lon,
            lat = point_lat,
            zoom = 10
    ) %>%
    fitBounds(lng1 = point_lon + extent,
              lng2 = point_lon - extent,
              lat1 = point_lat + extent,
              lat2 = point_lat - extent
    ) %>%
    addTiles() %>%
    addAwesomeMarkers(lng = point_lon,
                      lat = point_lat,
                      icon = slf_marker,
                      popup = point_label
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    return()

}
