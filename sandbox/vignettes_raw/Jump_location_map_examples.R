# loading necessary packages
library(tidyverse)
library(leaflet)

# creating some random point coordinates, and some random labels
long <- runif(5, -77, -75)
lat <- runif(5, 38, 42)
slf_jump_labels <- paste0("point", 1:5)

# turning them into a tibble
jump_data <- tibble(long = long,
                    lat = lat,
                    slf_jump_labels)

# now I'm making some small edits to your function, but it's pretty much the same
# the only change is that now the function maintains a 1 to 1 flow, taking one point
# in input and spitting out one map in output

map_jumps_v2 <- function(point_long, point_lat, point_label, extent = 0.01, zoom = 10) {

  # in the function, point_long and point_lat represent the longitude and latitude
  # of the point you want to zoom in
  # point_label is the label you gave to that point
  # extent represents the width and height of the area shown around the point
  # zoom allows you to set the zoom level, if you want tot have a different one

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
                      #icon = SLF_marker,
                      label = point_label
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    return()
}



# now the advantage of this is that you can use both within and outside of your list
# of jumps. For example, you can highlight a random point, like here:
map_jumps_v2(point_long = -71,
             point_lat = 42,
             point_label = "MyNewPoint",
             extent = 0.01, zoom = 10)
# this is now not showing the Icon, as I don't have your awesome SLF_marker

# you can change extent by hand
map_jumps_v2(point_long = -71,
             point_lat = 42,
             point_label = "MyNewPoint",
             extent = 0.1, zoom = 10)

# also, you can omit zoom and extent, and they will default to the values you have
# in the definition of the function
map_jumps_v2(point_long = -71,
             point_lat = 42,
             point_label = "MyNewPoint")




# now you can use the function for your jumps, in a few ways:
# 1) you can of course pipe this function inside a loop, and export the maps one by one
for(i in 1:nrow(jump_data)){

  map <- map_jumps_v2(point_long = jump_data$long[[i]],
                      point_lat = jump_data$lat[[i]],
                      point_label = jump_data$slf_jump_labels[[i]])

  assign(paste0("map", i), map)

}
map1
map2


# 2) or, and this is even better, you can store the maps using the (confusingly named!)
# purrr::map function.
# the map function takes a list (or lists) in input, and applied a function you provide
# to the items of that list
l <- list(9, 25, 64)
l_squared <- purrr::map(l, sqrt)


# in a tibble, 'purrr::map' combined with 'mutate' it's great to take one or more columns
# (which are lists) and return another column using a function you provide
# for example, it can turn one column into another one
jump_data %>%
  mutate(squared_latitude = purrr::map_dbl( # here the _dbl only tells map that we will be making a single number
    lat,       # the column we want to apply the function to
    sqrt       # the function we want to apply
  ))


# or we can use two columns to make another one
# here we can (pointlessly) sum up lat and long
jump_data %>%
  mutate(summed_lat_long = purrr::map2_dbl( # "map2" means map takes two column arguments
    lat,     # the FIRST column we want to apply the function to
    long,    # the SECOND column we want to apply the function to
    sum      # the function we want to apply to values in both columns
  ))


# or, more conveniently, we can take any number of columns to make another one
jump_data %>%
  mutate(pasted_values = purrr::pmap_chr( # "pmap" now takes a list of columns, as many as we want. and spits out a character
    list(long, lat, slf_jump_labels),
    paste   # the function we want to apply to values in both columns
  ))


# finally, we can replace our custom-made function, to get the maps and store them in the tibble
jump_data_with_maps <- jump_data %>%
  mutate(mapped_jumps = purrr::pmap(
    list(long, lat, slf_jump_labels), #IMPORTANT, this need to be in the same order as the arguments of our custom function
    map_jumps_v2   # here's our custom function
  ))

jump_data_with_maps
# as you can see the new columns we made now contains leaflet maps, stored alongside the coordinates and name of the jump locations
jump_data_with_maps$mapped_jumps[[1]]
