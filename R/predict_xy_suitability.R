#'Predicts suitability for xy coordinates according to a trained MaxEnt model WITHOUT cross-validation ('Maxent' object)
#'
#'@description
#'This function predicts establishment suitability for SLF based on a trained
#'MaxEnt model for a set of xy coordinates. The default method is simple point-wise
#'predictions, but predictions can also be made using a buffer zone around each
#'xy coordinate by setting buffer.pred = TRUE.
#'
#'@param xy.obj Should be a .csv file or data frame that contains xy coordinate
#'data only. Coordinates should be in decimal degree format. Longitude should be
#'listed first, followed by latitude. If a .csv file, file path should be in the
#'format produced by the [file.path()] function (i.e. with '/' instead of '\\').
#'
#'@param xy.type Character. Description of the xy data. If species occurrence
#'data, list the name of the species. If another type of coordinate data, list that
#'name. If providing more than one data type or species, separate into different
#'data frames before adding to this function
#'
#'@param env.covar.obj A stack of rasters of environmental covariates. These
#'covariates may be the same covariates used to train the model, or they may be
#'temporally or spatially projected (ex, for climate change). Suitability
#'predictions will be made based on these rasters. See details for additional
#'formatting information.
#'
#'@param model.obj A model object created by the package `SDMtune`.
#'
#'@param mypath Character.A file path to the sub directory where the model
#'output will be stored. Should be used with the [file.path()] function
#'(i.e. with '/' instead of '\\'). If this sub directory does not already exist
#'and should be created by the function, set `create.dir = TRUE`. This will
#'create a folder from the last part of the filepath in `mypath`.
#'
#'@param predict.type Character. The type of raster output
#'to be created from the trained model. Can be one of `raw`, `logistic` or
#'`cloglog`. If multiple are desired, must be in the concatenated
#'form: `c("cloglog", "logistic")`
#'
#'@param clamp.pred Logical. Should clamping be performed?
#'
#'@param buffer.pred Logical. Should suitability predictions be made based
#'on a calculation from a buffer around the xy coordinate data? (For Example, take
#'the mean, max, etc value of a 20m buffer around the xy coordinates).
#'
#'@param buffer.fun Character. The possible functions that can be used to calculate
#'the suitability value for the buffer around a point. Possible values include:
#'"min", "mean", "max", "sum", "isNA", and "notNA".
#'
#'@param buffer.dist Numeric. The distance from each xy coordinate at which to
#'calculate the suitability value.
#'
#'@param crs Character. The crs of the projection used in building the input species data.
#'Default is "ESRI:54017", the Behrmann Equal Area projection.
#'
#'@param output.name The name of the file output. Separate words with _. Relevant
#'information might include the name of the model used to predict, the spatial
#'scale, the temporal scale, and the type of data points that are being used for
#'predictions.
#'
#'@details
#'
#'The function requires the packages 'cli', 'terra', 'tidyverse', and 'SDMtune'.
#'
#'The xy coordinates do not need to be within the training area for the model, but
#'clamping should be applied if they are not within the training are (clamp.pred = TRUE).
#'
#'@return
#'This function returns 3+ .csv files.
#'
#'* `xy_with_data`: contains the input xy coordinates with the value of each
#'given environmental covariate at that location.
#'* `xy_no_data`: contains any coordinates that could not be predicted because
#'of an NA value for one or more covariates at that location.
#'* `xy_pred_suit`: This is the main result file, which contains each coordinate,
#'the type of record and the predicted suitability value. One result file will
#'be returned per predict.type, predict.fun and buffer.fun combination.
#'
#'@examples
#'
#'```R
#'
#'# simple predict method
#'scari::predict_xy_suitability(
#' xy.obj = IVR_regions,
#' xy.type = "IVR regions",
#' env.covar.obj = x_regional_native_hist_env_covariates,
#' model.obj = regional_native,
#' mypath = mypath,
#' clamp.pred = TRUE,
#' predict.type = c("cloglog", "logistic"),
#' output.name = "regional_native_wineries_1981-2010"
#' )
#'
#' # buffered predict method
#'scari::predict_xy_suitability(
#' xy.obj = IVR_regions,
#' xy.type = "IVR regions",
#' env.covar.obj = x_regional_native_hist_env_covariates,
#' model.obj = regional_native,
#' mypath = mypath,
#' clamp.pred = TRUE,
#' predict.type = c("cloglog", "logistic"),
#' output.name = "regional_native_wineries_1981-2010",
#' buffer.pred = TRUE,
#' buffer.dist = 20000, # in meters
#' buffer.fun = c("min", "max")
#' )
#'
#'```
#'
#'@export
predict_xy_suitability <- function(xy.obj, xy.type, env.covar.obj, model.obj, mypath, predict.type = "cloglog", clamp.pred = TRUE, buffer.pred = FALSE, buffer.fun = c("min", "mean", "max"), buffer.dist = 20000, crs = "ESRI:54017", output.name) {

  # Error checks----------------------------------------------------------------

  # ensure objects are character type
  if (is.character(predict.type) == FALSE) {
    cli::cli_abort("Parameter 'predict.type' must be of type 'character'")
    stop()
  }
  if (is.character(xy.type) == FALSE) {
    cli::cli_abort("Parameter 'xy.type' must be of type 'character'")
    stop()
  }
  if (is.character(mypath) == FALSE) {
    cli::cli_abort("Parameter 'mypath' must be of type 'character'")
    stop()
  }
  if (is.character(output.name) == FALSE) {
    cli::cli_abort("Parameter 'output.name' must be of type 'character'")
    stop()
  }
  if (is.character(buffer.fun) == FALSE) {
    cli::cli_abort("Parameter 'buffer.fun' must be of type 'character'")
    stop()
  }

  # ensure objects are logical type
  if (is.logical(clamp.pred) == FALSE) {
    cli::cli_abort("Parameter 'clamp.pred' must be of type 'logical'")
    stop()
  }
  if (is.logical(buffer.pred) == FALSE) {
    cli::cli_abort("Parameter 'buffer.pred' must be of type 'logical'")
    stop()
  }

  # ensure objects are numeric type
  if (is.numeric(buffer.dist) == FALSE) {
    cli::cli_abort("Parameter 'buffer.dist' must be of type 'numeric'")
    stop()
  }

  # import for xy.obj-----------------------------------------------------------

  # import settings
  if (is.character(xy.obj)) {
    xy_import <- read.csv(xy.obj) # read as csv

  } else if(is.data.frame(xy.obj)){
    xy_import <- as.data.frame(xy.obj) # just read in if its a df

  } else {
    xy_import <- as.data.frame(xy.obj) # make data frame

  }


  # ensure xy.obj data import is properly formatted
  if (ncol(xy_import) == 2 & is.numeric(xy_import[, 1]) & is.numeric(xy_import[, 2])) {
    cli::cli_alert_success("importing xy coordinate data")
  } else {
    cli::cli_abort("Data import must contain only coordinates in decimal degree. Columns must be longitude (column label 'x') and latitude (column label 'y'), in that order")
    stop()
  }



  # prepare swd of environmental covariates and xy------------------------------

  # status
  cli::cli_alert_info("extracting covariate data for xy coordinates")

  # get SWD object containing point location data from rasters
  xy_import_withdata_SWD <- SDMtune::prepareSWD(
    species = xy.type,
    env = env.covar.obj,
    p = xy_import,
    verbose = TRUE # print helpful messages
  )

  # save to file
  SDMtune::swd2csv(xy_import_withdata_SWD, file_name = file.path(mypath, paste0(output.name, "_xy_with_data.csv")))

  # modify xy_import before proceeding
  # create a joining column for the xy data- this is because UTMs are too sensitive to join properly
  xy_import <- xy_import %>%
    dplyr::mutate(
      join_col_x = round(x, 5),
      join_col_y = round(y, 4) # rounding to the 1000s (1km) place to prevent overly sensitive exclusions for UTM data
    )

  # Create table of points with no data-----------------------------------------

  # status
  cli::cli_alert_info("isolating xy coordinates with no covariate data")

  # re-import data
  xy_import_withdata <- read_csv(file = file.path(mypath, paste0(output.name, "_xy_with_data.csv")))

  # make X and Y columns lower case
  colnames(xy_import_withdata)[3:4] <- c("x", "y")

  # create a joining column for the xy data- this is because UTMs are too sensitive to join properly
  xy_import_withdata <- xy_import_withdata %>%
    dplyr::mutate(
      join_col_x = round(x, 5),
      join_col_y = round(y, 4) # rounding to prevent overly sensitive exclusions for UTM data
    )

  # join with original dataset to find presences with no data
  # rounding done to prevent overly sensitive exclusions for UTM data
  xy_nodata <- anti_join(x = xy_import, y = xy_import_withdata, by = c("join_col_x", "join_col_y"))
  # write to csv, excluding join columns
  write_csv(x = xy_nodata[, 1:2], file = file.path(mypath, paste0(output.name, "_xy_no_data.csv")))



  # predict function------------------------------------------------------------

  # status
  cli::cli_alert_info("predicting suitability for xy coordinate data")

  ## load in datasets-----------------------------------------------------------

  # for simple point prediction method
  # tidy dataset for sdmtune::predict
  xy_covariates <- semi_join(x = xy_import_withdata, y = xy_import, by = c("join_col_x", "join_col_y")) %>%
    # select only necessary columns
    dplyr::select(5:length(.)) %>%
    dplyr::select(-c(join_col_x, join_col_y)) # remove join columns


  # for buffer point prediction method
  xy_sv <- semi_join(x = xy_import_withdata, y = xy_import, by = c("join_col_x", "join_col_y")) %>%
    # select only lat lon columns
    dplyr::select(3:4) %>%
    as.data.frame()
  # convert to spatvector
  xy_sv <- terra::vect(
    x = xy_sv,
    crs = crs,
    geom = c("x", "y")
  )

  # final edit to xy_import_withdata to remove points with no data
  # UNSURE why this works but it does
  xy_import_withdata <- semi_join(x = xy_import_withdata, y = xy_import, by = c("join_col_x", "join_col_y"))

  # loop for every function listed
    for (b in predict.type) {

      # initialization message
      cli::cli_alert_info(paste0("predicting xy suitability for function: ", b))



       # determines the type of prediction
      # if a a simple point-wise prediction is desired, perform this operation
      if (buffer.pred == FALSE) {

        ### simple prediction method--------------------------------------------

        # predict type alert
        cli::cli_alert_info("prediction type: simple")

      # make predictions
      xy_predict <- SDMtune::predict(
        object = model.obj, # model
        data = xy_covariates, # data for prediction
        type = b, # type of prediction made
        clamp = clamp.pred,
        progress = TRUE # progress bar
      ) %>%
        as.data.frame() %>%
        rename_with(
          .,
          ~ paste0(b, "_suitability", recycle0 = TRUE),
          .cols = ".")



      # bind predictions with tidy dataset
      xy_predict <- cbind(xy_predict, xy_import_withdata) %>%
        # select only necessary data
        dplyr::select(Species, x, y, contains("suitability"))

      # save output
      write_csv(
        x = xy_predict,
        file = file.path(mypath, paste0(output.name, "_xy_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), b, ".csv"))
        )


      # remove prediction
      rm(xy_predict)



      # else if a buffer type of prediction is preferred, use this method
      } else if (buffer.pred == TRUE) {

        ### buffer prediction method---------------------------------------------


        # predict type alert
        cli::cli_alert_info(paste0("prediction type: buffer of ", buffer.dist, "m around points"))

        # predict a raster
        xy_predict_raster <- SDMtune::predict(
          object = model.obj,
          data = env.covar.obj, # the covariate layers used to train the model
          type = b,
          clamp = clamp.pred
        )

        # take buffer around points
        xy_buffer <- terra::buffer(
          x = xy_sv,
          width = buffer.dist
        )


        # create table for loop
        xy_predict_output <- semi_join(x = xy_import_withdata, y = xy_import, by = c("join_col_x", "join_col_y")) %>%
          # select only lat lon columns
          dplyr::select(3:4)


        #### predict create a buffer per function--------------------------------
        for (c in buffer.fun) {

          # predict type alert
          cli::cli_alert_info(paste0("taking ", c, " value of ", buffer.dist, "m around points"))

          # use buffer to get zonal statistics from raster
          xy_predict_hold <- terra::zonal(
            x = xy_predict_raster, # raster of predictions we created
            z = xy_buffer, # buffer zone
            fun = c,
            na.rm = TRUE # remove NAs from buffer
          )

          # rename column
          colnames(xy_predict_hold) <- paste0(c, "_", b, "_suitability_", buffer.dist, "m_buffer")

          # join conf.matr.hold temporary object to empty table
          xy_predict_output <- cbind(xy_predict_output, xy_predict_hold)


        } # end of for(c in buffer.fun) statement

        # add Species column
        xy_predict_output <- cbind(xy_predict_output, xy_import_withdata[1]) %>%
          dplyr::relocate(Species)

        # write to file when finished
        write_csv(
          x = xy_predict_output,
          file = file.path(mypath, paste0(output.name, "_xy_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), b, "_", buffer.dist, "m_buffer.csv"))
        )

        # remove stuff
        rm(xy_predict_raster) # raster
        rm(xy_predict_output)

      } # end of if (buffer.pred = TRUE) statement


      # success message
      cli::cli_alert_success(paste0("finished predicting xy suitability for function: ", b))

    } # end of for(b in predict.type) statement

  # status update
  cli::cli_alert_success("finished predicting xy suitability")

} # end of function

