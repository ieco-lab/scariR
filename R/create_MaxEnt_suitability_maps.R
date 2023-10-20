#'
#'
#'The function requires the packages 'tidyverse', 'here', 'devtools', 'SDMtune', 'terra' and 'viridis'.
#'
#'@param
#'
#'
#'
#'@param predict.fun Character. The function to be applied to combine the
#'iterations of the model when predicting a raster output. Can be one of:
#'"mean", "sd" (standard deviation) or "max". If multiple are desired, must be
#'in the form: `c("mean", "sd", "max")`
#'
#'@param thresh Numeric or Character. This may be imported manually (numeric),
#'or may be selected from one of the thresholds for the model (character). If a
#'preset, the specified mean threshold value for all iterations of the model is
#'chosen. See details for preset options.
#'
#'
#'@details
#'
#'
#'
#'@return
#'
#' 3 outputs
#' * mean
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
create_suitability_maps <- function(model.obj, model.name, mypath, create.dir = FALSE, env.covar.obj, predict.fun = "mean", thresh, map_style) {

  # Error checks----------------------------------------------------------------





  # Thresh preset values--------------------------------------------------------

  thresh_preset_import <- read.csv(file = file.path(mypath, "easternUSA_buffered_summary_all_iterations.csv"))

  # preset values for thresh parameter
  thresh_presets <- c(
    "MTP" = thresh_preset_import[30, 6], # Minimum.training.presence.Cloglog.threshold
    "ten_percentile" = thresh_preset_import[34, 6], # 10.percentile.training.presence.Cloglog.threshold
    "ETSS" = thresh_preset_import[38, 6], # Equal.training.sensitivity.and.specificity.Cloglog.threshold
    "MTSS" = thresh_preset_import[42, 6], # Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
    "BTO" = thresh_preset_import[46, 6], # Balance.training.omission..predicted.area.and.threshold.value.Cloglog.threshold
    "EE" = thresh_preset_import[50, 6] # Equate.entropy.of.thresholded.and.original.distributions.Cloglog.threshold
  )

  # Criteria for selecting thresh value-----------------------------------------

  # if numeric, import
  if(is.numeric(thresh)) {
    thresh_value <- thresh

    # if a preset, import value of preset
  } else if(is.element(thresh, names(thresh_presets))){
    threshold <- thresh_presets[thresh]

    # otherwise, stop and give warning
  } else {
    stop(paste0("thresh must be numeric or one of: \n    ", paste(names(thresh_presets), collapse = ' | ')))

  }

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
    fun = predict.fun,
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




