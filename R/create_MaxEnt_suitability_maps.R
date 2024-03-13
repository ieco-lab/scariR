#'Predicts maps of establishment suitability based on a MaxEnt model without cross-validation ('Maxent' object)
#'
#'This function will create a map of establishment suitability based on a MaxEnt
#'model trained using the `SDMtune` R package. It will optionally create
#'thresholded versions of these maps using thresholds given by the MaxEnt
#'algorithm.
#'
#'@param model.obj A model object created by the package 'SDMtune', should be of
#'class 'SDMmodelCV'.
#'
#'@param model.name Character. A string matching the name of the object set for
#'`model.obj`. Exclude unnecessary phrases, such as the "_model" ending.
#'
#'@param mypath Character.A file path to the sub directory where the model
#'output will be stored. Should be used with the [file.path()] function
#'(i.e. with '/' instead of '\\'). If this sub directory does not already exist
#'and should be created by the function, set `create.dir` = TRUE. This will
#'create a folder from the last part of the filepath in `mypath`.
#'
#'@param create.dir Logical. Should the last element of `mypath` create a sub
#'directory for the model output? If TRUE, the main folder will be created for
#'the model output. If FALSE (ie, the sub directory already exists), only the
#'"plots" folder within the model output sub directory will be created.
#'
#'@param env.covar.obj A stack of rasters of environmental covariates. These
#'covariates may be the same covariates used to train the model, or they may be
#'temporally or spatially projected (ex, for climate change). Suitability
#'predictions will be made based on these rasters. See details for additional
#'formatting information.
#'
#'@param describe.proj Character. Is the environmental covariate raster stack in
#'`env.covar.obj` projected to a different geographical space, time period, etc
#'than the model was trained on? If yes, enter the name of the projected period
#'or region. This ensures that plot titles and file names reflect the projected
#'region or time period. If using multiple words, separate with an underscore.
#'
#'@param predict.type Character. Default is "cloglog". The type of raster output
#'to be created from the trained model. Can be either `cloglog` or
#'`c("cloglog", "logistic")`. Cannot only be `logistic`.
#'
#'@param clamp.pred Logical. Default is TRUE. Should clamping be performed?
#'
#'@param map.thresh Logical, TRUE by default. This function determines if a
#'thresholded suitability map will be created. If not, output will only consist
#'of suitability maps of the type specified in `predict.fun`. **Note** threshold
#'maps can only be created for the cloglog output from the model. If multiple
#'values for `predict.type` are specified, the function will still only produce
#'a threshold map based on the cloglog output
#'
#'@param thresh Numeric or Character. Does not need to be defined if
#'`map.thresh = FALSE` This may be imported manually (numeric), or may be
#'selected from one of the thresholds for the model (character). If a preset,
#'the specified mean threshold value for all iterations of the model is chosen.
#'See details for a list of preset options and other usages.
#'
#'@param summary.file Data import. Does not need to be defined if
#'`map.thresh = FALSE`. Should be a .csv file or data frame that contains the
#'summary statistics output created by
#'[slfSpread::compute_MaxEnt_summary_statistics()] (filename ending in
#'"_summary.csv"). If an import, file path should be in the format produced
#'by the [file.path()] function (i.e. with '/' instead of '\\').
#'
#'@param map.style List, default is NA. This is used to apply
#'ggplot aesthetics to the plot outputs. If specified, the given value should be
#'a list of ggplot aesthetic options. If not, the built-in default list will be
#'used (see details). See examples for usage.
#'
#'@details
#'
#'The function requires the packages 'cli', 'tidyverse', 'here', 'devtools', 'SDMtune', 'terra' and 'viridis'.
#'
#'The thresholding feature of this function will limit what is considered
#'suitable using one of a list of commonly applied thresholds. These threshold
#'values are determined by the MaxEnt algorithm. (ie, a non-thresholded map on
#'the cloglog scale has a lower color scale limit of 0, while a thresholded map
#'has a lower limit determined by the algorithm.)
#'
#'**NOTE** This function will create a thresholded suitability map for a raster
#'output using the SD function, but this map would not be meaningful because
#'it does not illustrate cloglog suitability (while thresholds are created
#'using the cloglog suitability metrics).
#'
#'## thresh:
#'
#'This can be a single numeric or preset character value. It may also be a
#'concatenated set of numerics or presets, as in  `c(0.2, 0.3)` or
#'`c("MTSS", "BTO")`. Note that this function only uses the cloglog version of
#'these thresholds.
#'
#'Thresh presets list:
#'* `BTO` = Balance training omission predicted area and threshold value
#'* `EE` = Equate entropy of thresholded and original distributions
#'* `ETSS` = Equal training sensitivity and specificity
#'* `MTP` = Minimum Training Presence
#'* `MTSS` = Maximum training sensitivity plus specificity
#'* `ten_percentile` or `10_percentile` = Ten percentile training presence
#'
#'## env.covar.obj:
#'This must a `SpatRaster` raster stack created using [terra::rast()]. The stack
#'should include the same covariates (as raster layers) that you used to train
#'the model and the names of the variables in these layers must be the same.
#'You can check for naming consistency using [names()].
#'
#'## map.style:
#'
#'Map style default options:
#'
#'* `xlab("longitude")`
#'* `ylab("latitude")`
#'* `labs(fill = "Suitability for SLF")`
#'* `theme_classic()`
#'* `theme(legend_position = "bottom")`
#'* `theme(panel.background = element_rect(fill = "lightblue2", color = "lightblue2"))`
#'* `scale_x_continuous(expand = c(0, 0))`
#'* `scale_y_continuous(expand = c(0, 0))`
#'* `coord_equal()`
#'* `viridis::scale_fill_viridis(option = "D")`
#'
#'@return
#'A raster of suitability values projected to the same spatial extent as the
#'input `env.covar.obj` and a corresponding .jpg figure are created. If multiple
#'values are given for `predict.fun`, then one raster and jpg image
#'will be created for each value. If `map.thresh = TRUE`, then the output will
#'also include a binary raster of suitability and a .jpg image of unsuitable
#'areas layered on top of suitability raster. This threshold of suitability is
#'determined by the value of `thresh`.
#'
#'Use caution: will overwrite previous files by default.
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
create_MaxEnt_suitability_maps <- function(model.obj, model.name, mypath, create.dir = FALSE, env.covar.obj, describe.proj = NA, predict.type = "cloglog", clamp.pred = TRUE, map.thresh = FALSE, thresh = NA, summary.file = NA, map.style = NA) {

  # Error checks----------------------------------------------------------------

  # ensure objects are character type
  if (is.character(model.name) == FALSE) {
    cli::cli_alert_danger("Parameter 'model.name' must be of type 'character'")
    stop()
  }
  if (is.character(predict.type) == FALSE) {
    cli::cli_alert_danger("Parameter 'predict.type' must be of type 'character'")
    stop()
  }
  if (is.character(mypath) == FALSE) {
    cli::cli_alert_danger("Parameter 'mypath' must be of type 'character'")
    stop()
  }
  if (is.character(describe.proj) == FALSE) {
    cli::cli_alert_danger("Parameter 'describe.proj' must be of type 'character'")
    stop()
  }

  # ensure objects are logical type
  if (is.logical(clamp.pred) == FALSE) {
    cli::cli_alert_danger("Parameter 'clamp.pred' must be of type 'logical'")
    stop()
  }
  if (is.logical(map.thresh) == FALSE) {
    cli::cli_alert_danger("Parameter 'map.thresh' must be of type 'logical'")
    stop()
  }

  # ensure predict.type contains at least "cloglog"
  if ("cloglog" %in% predict.type == TRUE) {
    predict_type <- predict.type

  } else {
    cli::cli_alert_danger("Parameter 'predict.type' must at least contain 'cloglog'")
    stop()
  }

  # Create sub directory for files----------------------------------------------

  if (create.dir == FALSE) {
    # print message
    cli::cli_alert_info("proceeding without creating model subdirectory folder")
    # create plots subfolder
    dir.create(mypath, "plots")

  } else if (create.dir == TRUE) {
    # create sub directory from ending of mypath object
    dir.create(mypath)
    # print message
    cli::cli_alert_info(paste0("sub directory for files created at: ", mypath))
    # create plots folder within
    dir.create(mypath, "plots")

  } else {
    cli::cli_alert_danger("'create.dir' must be of type 'logical'")
    stop()

  }

  # object for map ggplot style and criteria------------------------------------

  # if it is not changed from NA, import default style
  if (is.na(map.style)) {

    map_style <- list(
      xlab("longitude"),
      ylab("latitude"),
      theme_classic(),
      theme(legend.position = "bottom",
            panel.background = element_rect(fill = "lightblue2",
                                            colour = "lightblue2")
      ),
      scale_x_continuous(expand = c(0, 0)),
      scale_y_continuous(expand = c(0, 0)),
      labs(fill = "Suitability for SLF"),
      viridis::scale_fill_viridis(option = "D"),
      coord_equal()
    )

    # if it is changed to a list, import the given list
  } else if (is.list(map_style) == TRUE) {
    map_style <- map.style

    # otherwise, warn that given values must be a list
  } else {
    cli::cli_alert_danger("parameter 'map.style' must be of type 'list'")
    stop()

  }

  # Create main distribution map------------------------------------------------


  # create suitability map for every type of output listed
  for (b in predict_type) {

    # initialization message
    print(paste0("predicting raster: ", b))

    # use predict function
    SDMtune::predict(
      object = model.obj,
      data = env.covar.obj, # the covariate layers used to train the model
      type = b,
      clamp = clamp.pred,
      filename = file.path(mypath, paste0(model.name, "_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), b, ifelse(is.na(describe.proj), "", paste0("_", describe.proj)), ".asc")),
      # the function automatically adds the function name on the end
      overwrite = TRUE,
      wopt = list(NAflag = NA)

    )

    # message of completion
    print(paste0(b, " raster created and saved at: ", mypath))


    # load in predictions
    model_suit <- terra::rast(x = file.path(mypath, paste0(model.name, "_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), b, ifelse(is.na(describe.proj), "", paste0("_", describe.proj)), ".asc"))) %>%
      terra::as.data.frame(., xy = TRUE)

    # plot
    model_suit_plot <- ggplot() +
      geom_raster(data = model_suit,
                  aes(x = x, y = y, fill = model_suit[, 3])) +
      labs(title = paste0("Suitability for SLF: ", b),
           subtitle = paste0("Model: '", model.name, "'", ifelse(clamp.pred == TRUE, ", clamped", ""), ifelse(is.na(describe.proj), "", paste0(", projected to ", describe.proj)))) +
      map_style

    # save plot output
    ggsave(model_suit_plot,
           filename = file.path(mypath, "plots", paste0(model.name, "_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), b, ifelse(is.na(describe.proj), "", paste0("_", describe.proj)), ".jpg")),
           height = 8,
           width = 10,
           device = "jpeg",
           dpi = "retina")

    # end of loop-------------------------------------------------------------

    # message of completion
    print(paste0("figure created for raster: ", b))

    # remove raster objects
    rm(model_suit)
    rm(model_suit_plot)

  } # end of for(b in predict_type) statement




  # thresholded mapping---------------------------------------------------------

  # conditional statement if thresholded maps will be created
  if (map.thresh == TRUE) {

    # Thresh import rules-----------------------------------------------------

    # import settings for summary.file
    if (is.character(summary.file)) {
      thresh_preset_import <- read.csv(summary.file) # read as csv

    } else {
      thresh_preset_import <- as.data.frame(summary.file) # make data frame

    }


    # Modeled thresh values---------------------------------------------------

    # import thresh presets
    thresh_presets <- c(
      "MTP" = as.numeric(thresh_preset_import[30, ncol(thresh_preset_import)]), # Minimum.training.presence.Cloglog.threshold
      "ten_percentile" = as.numeric(thresh_preset_import[34, ncol(thresh_preset_import)]), # 10.percentile.training.presence.Cloglog.threshold
      "10_percentile" = as.numeric(thresh_preset_import[34, ncol(thresh_preset_import)]), # 10.percentile.training.presence.Cloglog.threshold
      "ETSS" = as.numeric(thresh_preset_import[38, ncol(thresh_preset_import)]), # Equal.training.sensitivity.and.specificity.Cloglog.threshold
      "MTSS" = as.numeric(thresh_preset_import[42, ncol(thresh_preset_import)]), # Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
      "BTO" = as.numeric(thresh_preset_import[46, ncol(thresh_preset_import)]), # Balance.training.omission..predicted.area.and.threshold.value.Cloglog.threshold
      "EE" = as.numeric(thresh_preset_import[50, ncol(thresh_preset_import)]) # Equate.entropy.of.thresholded.and.original.distributions.Cloglog.threshold
      )

    # import previous raster layer--------------------------------------------

    # load in raster created in last loop
    model_suit_raster <- terra::rast(x = file.path(mypath, paste0(model.name, "_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), "cloglog", ifelse(is.na(describe.proj), "", paste0("_", describe.proj)), ".asc")))

    # thresh import-----------------------------------------------------------

    # allow multiple entries for thresh
    for (k in thresh) {

      # conditional statement to create tailored name for outputs
      # if a is a numeric, import this value to naming object used to name outputs
      if(is.numeric(k)) {
        thresh_name <- k

        # otherwise (if it is a preset value), change value to as.character to name outputs
      } else {
        thresh_name <- as.character(k)
      }


      # Criteria for selecting thresh value-----------------------------------

      # if numeric, import
      if(is.numeric(k)) {
        thresh_value <- k

        # if a preset, import value of preset
      } else if(is.element(k, names(thresh_presets))){
        thresh_value <- thresh_presets[k]

        # otherwise, stop and give warning
      } else {
        stop(paste0("'thresh' must be numeric or one of: \n    ", paste(names(thresh_presets), collapse = ' | ')))

      }

      # print messages--------------------------------------------------------

      # loop start
      print(paste0("begin threshold raster and map: ", thresh_name))
      # thresh value
      print(paste0("threshold value for ", thresh_name, ": ", thresh_value))

      # Create binary raster with threshold-----------------------------------

      # terra required classification matrices
      binary_rescale_class <- data.frame(
        from = c(0, thresh_value + 0.00000000000000000000000000000000000000000000000000000000000001), # there was an issue with creating the threshold map if the threshold was 0- this will avoid the issue by making the classification boundaries different
        to = c(thresh_value, 1),
        becomes = c(0, 1)
      )

      # re-classify raster according to threshold
      terra::classify(
        x = model_suit_raster,
        rcl = binary_rescale_class,
        right = TRUE,
        include.lowest = TRUE,
        others = NA,
        filename = file.path(mypath, paste0(model.name, "_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), "cloglog", "_thresholded_", thresh_name, ifelse(is.na(describe.proj), "", paste0("_", describe.proj)), ".asc")), # also write to file
        NAflag = NA,
        overwrite = TRUE
      )

      # message of completion
      print(paste0(thresh_name, " binary raster created and saved at: ", mypath))

      # Create mask raster for thresholded raster figure----------------------

      # create regional suitability value matrix for terra
      mask_rescale_class <- data.frame(
        from = 0,
        to = thresh_value + 0.00000000000000000000000000000000000000000000000000000000000001, # there was an issue with creating the threshold map if the threshold was 0- this will avoid the issue by making the classification boundaries different
        becomes = 1
      )

      # reclassify and write regional raster
      mask_layer <- terra::classify(
        x = model_suit_raster,
        rcl = mask_rescale_class,
        right = TRUE, # includes both sides
        include.lowest = TRUE,
        others = NA,
        filename = file.path(mypath, paste0(model.name, "_mask_layer", ifelse(clamp.pred == TRUE, "_clamped_", "_"), "cloglog_", thresh_name, ifelse(is.na(describe.proj), "", paste0("_", describe.proj)), ".asc")), # also write to file
        NAflag = NA,
        overwrite = TRUE
        )

      # message of completion
      print(paste0(thresh_name, " mask layer raster created and saved at: ", mypath))


      # Create thresholded suitability map------------------------------------

      # convert imported raster to df
      model_suit_raster_df <- terra::as.data.frame(model_suit_raster, xy = TRUE)

      # load in mask layer and convert to df for plotting
      model_mask_layer_df <- terra::rast(x = file.path(mypath, paste0(model.name, "_mask_layer", ifelse(clamp.pred == TRUE, "_clamped_", "_"), "cloglog_", thresh_name, ifelse(is.na(describe.proj), "", paste0("_", describe.proj)), ".asc"))) %>%
        terra::as.data.frame(., xy = TRUE)

      # plot suitability raster first
      model_threshold_plot <- ggplot() +
        # plot regular raster of values first
        geom_raster(data = model_suit_raster_df,
                    aes(x = x, y = y, fill = model_suit_raster_df[, 3])) +
        # plot binary threshold on top
        geom_raster(data = model_mask_layer_df,
                    aes(x = x, y = y), fill = "azure4") +
        labs(title = paste0("suitability for SLF: cloglog | ", thresh_name, " threshold"),
             subtitle = paste0("Model: '", model.name, "'", ifelse(clamp.pred == TRUE, ", clamped", ""), ifelse(is.na(describe.proj), "", paste0(", projected to ", describe.proj)))) +
        map_style

      # save plot output
      ggsave(model_threshold_plot,
             filename = file.path(mypath, "plots", paste0(model.name, "_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), "cloglog", "_thresholded_", thresh_name, ifelse(is.na(describe.proj), "", paste0("_", describe.proj)), ".jpg")),
             height = 8,
             width = 10,
             device = "jpeg",
             dpi = "retina")

      # end of loop operations------------------------------------------------

      # message of completion
      print(paste0("figure created for raster: ", thresh_name))

      # remove temp objects
      rm(thresh_name)
      rm(thresh_value)

    } # end of for(k in thresh) statement

    # remove temp raster
    rm(model_suit_raster)

    } # end of if (map.thresh == TRUE) statement

  # status update
  cli::cli_alert_success("finished plotting suitability maps")

  } # end of function


