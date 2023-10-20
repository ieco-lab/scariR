#'
#'
#'The function requires the packages 'tidyverse', 'here', 'devtools', 'SDMtune', 'terra' and 'viridis'.
#'
#'@param
#'
#'
#'
#'@param
#'
#'
#'
#'@details
#'
#'
#'
#'@return
#'
#'
#'
#'@examples
#'# x---------------------------------------------------------------------------
#'
#'
#'# troubleshooting-------------------------------------------------------------
#'
#'
#'
#'@export
create_suitability_maps <- function() {

  model.obj
  map_style
  create.dir
  mypath
  thresh

  # output directory
  mypath <- file.path(here() %>%
                        dirname(),
                      "maxent/models/slf_easternUSA_buffered_step1")

  # Thresh preset values--------------------------------------------------------

  # preset values for thresh parameter
  thresh_presets <- c(
    egg_laying_50 = 1616.4,
    egg_laying_10 = 1564.1,
    adult_emergence_50 = 1115.5,
    adult_emergence_1 = 991
  )

  # Error checks----------------------------------------------------------------




  # Create object with thresh value---------------------------------------------

  thresh_value <- read.csv(file = file.path(mypath, "easternUSA_buffered_summary_all_iterations.csv")) %>%
    .[42, 6]

  # object for map ggplot style-------------------------------------------------

  map_style <- list(
    xlab("longitude"),
    ylab("latitude"),
    theme_classic(),
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue")
    ),
    coord_equal()
  )

  # Create mean distribution map------------------------------------------------

  # use predict function
  SDMtune::predict(
    object = easternUSA_buffered_model,
    data = x_env_covariates, # the covariate layers used to train the model
    fun = "mean",
    type = "cloglog",
    clamp = FALSE,
    progress = TRUE,
    filename = file.path(mypath, "easternUSA_buffered_predicted_suitability.asc"),
    # the function automatically adds the function name on the end
    filetype = "AAIGrid"
  )

  # load in mean predictions
  easternUSA_buffered_suit <- terra::rast(x = file.path(mypath, "easternUSA_buffered_predicted_suitability_mean.asc")) %>%
    terra::as.data.frame(., xy = TRUE)

  # plot
  easternUSA_buffered_suit_plot <- ggplot() +
    geom_raster(data = easternUSA_buffered_suit,
                aes(x = x, y = y, fill = mean)) +
    labs(title = "Mean suitability for SLF",
         subtitle = "Model: easternUSA_buffered") +
    viridis::scale_fill_viridis(option = "D") +
    map_style

  # save plot output
  ggsave(easternUSA_buffered_suit_plot,
         filename = file.path(mypath, "plots", "easternUSA_buffered_predicted_suitability_mean.jpg"),
         height = 8,
         width = 10,
         device = "jpeg",
         dpi = "retina")

  # Create binary raster with threshold-----------------------------------------

  # terra required classification matrices
  rescale_class <- data.frame(
    from = c(0, thresh_value),
    to = c(thresh_value, 1),
    becomes = c(0, 1)
  )

  # load in raster
  buffered_mean_raster <- terra::rast(x = file.path(mypath, "easternUSA_buffered_predicted_suitability_mean.asc"))

  # re-classify raster according to threshold
  terra::classify(x = buffered_mean_raster,
                  rcl = buffered_rescale_class,
                  right = FALSE, # close left side of value
                  filename = file.path(mypath, "easternUSA_buffered_predicted_suitability_mean_thresholded.asc"), # also write to file
                  overwrite = FALSE
  )

  # Create mask raster for thresholded raster figure----------------------------


  # create regional suitability value matrix for terra
  buffered_rescale_class <- data.frame(
    from = 0,
    to = thresh_value,
    becomes = 1
  )

    # reclassify and write regional raster
    mask_layer <- terra::classify(x = buffered_mean_raster,
                                  rcl = buffered_rescale_class,
                                  right = FALSE, # close left side of value
                                  others = NA,
                                  filename = file.path(mypath, "easternUSA_buffered_MTSS_mask_layer.asc"), # also write to file
                                  overwrite = FALSE)


  # Create thresholded suitability map------------------------------------------

  # load in mean predictions
  buffered_mask_layer_df <- terra::rast(x = file.path(mypath, "easternUSA_buffered_MTSS_mask_layer.asc")) %>%
    terra::as.data.frame(., xy = TRUE)

  # plot mean raster first
  easternUSA_buffered_threshold_plot <- ggplot() +
    # plot regular raster of values first
    geom_raster(data = easternUSA_buffered_suit,
                aes(x = x, y = y, fill = mean)) +
    # plot binary threshold on top
    geom_raster(data = buffered_mask_layer_df,
                aes(x = x, y = y), fill = "azure4")
  labs(title = "Suitability for SLF, MTSS training threshold",
       subtitle = "Model: 355km buffer") +
    viridis::scale_fill_viridis(option = "D") +
    map_style

  # save plot output
  ggsave(easternUSA_buffered_threshold_plot,
         filename = file.path(mypath, "plots", "easternUSA_buffered_predicted_suitability_mean_thresholded.jpg"),
         height = 8,
         width = 10,
         device = "jpeg",
         dpi = "retina")


}




