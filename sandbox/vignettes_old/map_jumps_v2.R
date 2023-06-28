# map_jumps_v2

# This function is designed to map a coordinate point using the leaflet package and in this package, it is used to map new SLF jump locations.
# It is designed to show the point with a specified latitude and longitude margin on all sides of the point (zoom level)
# It is suggested that this function be used to append mapping data to a pre-existing data frame or tibble

# Arguments:

# Point_long and point_lat: represent the longitude and latitude of the point you want to map
# Point_label:  whatever you assign to be the label of that point
# Extent: the width and height on either side of each point that you want mapped, in degrees latitude or longitude
# Zoom: allows you to set the zoom level, if you want tot have a different one- 
  # note: even if you choose to use fitBounds() to zoom in on the point, a zoom level must still be established within setView(). 
  # fitBounds() would then override the zoom level

# Major changes from v1: 

# Specified all variables as arguments- allows this function to be used in context of a single point, rather than just a data frame or tibble

map_jumps_v2 <- function(point_long, point_lat, point_label, extent = 0.01, zoom = 10) {
  
  leaflet() %>%
    setView(lng = point_long,
            lat = point_lat,
            zoom = 10
    ) %>%
    fitBounds(lng1 = point_long + extent,
              lng2 = point_long - extent,
              lat1 = point_lat + extent,
              lat2 = point_lat - extent
    ) %>%
    addTiles() %>%
    addAwesomeMarkers(lng = point_long,
                      lat = point_lat,
                      icon = SLF_marker,
                      popup = point_label
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    
    return()
}