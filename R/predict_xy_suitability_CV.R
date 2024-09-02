#'Predicts suitability for xy coordinates according to a trained MaxEnt model WITH cross-validation ('SDMmodelCV' object)
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
#'@param predict.fun Character. This is the function to
#'be applied to combine the iterations of the model when predicting a suitability
#'output. Can be one of: `min`, `mean`, `median`, `max`, or `sd`
#'(standard deviation). If multiple are desired, must be in the concatenated
#'form: `c("mean", "max")`. Should be all lowercase.
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
#'@param output.name The name of the file output. Separate words with _. Relevant
#'information might include the name of the model used to predict, the spatial
#'scale, the temporal scale, and the type of data points that are being used for
#'predictions.
#'
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
#'* `xy_no_data`: contains the contains any coordinates that could not be
#'predicted because of an NA value for one or more covariates at that location.
#'* `xy_pred_suit`: This is the main result file, which contains each coordinate,
#'the type of record and the predicted suitability value. One result file will
#'be returned per predict.type, predict.fun and buffer.fun combination.
#'
#'@examples
#'
#'# EXAMPLE USAGE:
#'
#'# simple predict method
#'slfSpread::predict_xy_suitability_CV(
#' xy.obj = IVR_regions,
#' xy.type = "IVR regions",
#' env.covar.obj = x_global_hist_env_covariates,
#' model.obj = global_model,
#' mypath = mypath,
#' clamp.pred = TRUE,
#' predict.type = c("cloglog", "logistic"),
#' output.name = "global_wineries_1981-2010"
#' )
#'
#' # buffered predict method
#'slfSpread::predict_xy_suitability_CV(
#' xy.obj = IVR_regions,
#' xy.type = "IVR regions",
#' env.covar.obj = x_global_hist_env_covariates,
#' model.obj = global_model,
#' mypath = mypath,
#' clamp.pred = TRUE,
#' predict.type = c("cloglog", "logistic"),
#' output.name = "global_wineries_1981-2010",
#' buffer.pred = TRUE,
#' buffer.dist = 20000, # in meters
#' buffer.fun = c("min", "max")
#' )
#'
#'@export
predict_xy_suitability_CV <- function(xy.obj, xy.type, env.covar.obj, model.obj, mypath, predict.fun = "mean", predict.type = "cloglog", clamp.pred = TRUE, buffer.pred = FALSE, buffer.fun = c("min", "mean", "max"), buffer.dist = 20000, output.name) {

  # Error checks----------------------------------------------------------------

  # ensure objects are character type
  if (is.character(predict.fun) == FALSE) {
    cli::cli_abort("Parameter 'predict.fun' must be of type 'character'")
    stop()
  }
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

  # imoport settings
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



  # Create table of points with no data-----------------------------------------

  # status
  cli::cli_alert_info("isolating xy coordinates with no covariate data")

  # re-import data
  xy_import_withdata <- read_csv(file = file.path(mypath, paste0(output.name, "_xy_with_data.csv")))

  # make X and Y columns lower case
  colnames(xy_import_withdata)[3:4] <- c("x", "y")

  # join with original dataset to find presences with no data
  xy_nodata <- anti_join(x = xy_import, y = xy_import_withdata, by = c("x", "y"))

  # write to csv
  write_csv(x = xy_nodata, file = file.path(mypath, paste0(output.name, "_xy_no_data.csv")))



  # predict function------------------------------------------------------------

  # status
  cli::cli_alert_info("predicting suitability for xy coordinate data")

  # for simple point prediction method
  # tidy dataset for sdmtune::predict
  xy_covariates <- semi_join(x = xy_import_withdata, y = xy_import, by = c("x", "y")) %>%
    # select only data columns
    dplyr::select(5:length(.))

  # for buffer point prediction method
  xy_sv <- semi_join(x = xy_import_withdata, y = xy_import, by = c("x", "y")) %>%
    # select only lat lon columns
    dplyr::select(3:4) %>%
    as.data.frame()

  # convert to spatvector
  xy_sv <- terra::vect(
    x = xy_sv,
    crs = "EPSG:4326",
    geom = c("x", "y")
    )



  # loop for every function listed
  for (a in predict.fun) {

    for (b in predict.type) {

      # initialization message
      cli::cli_alert_info(paste0("predicting xy suitability for function: ", a, " | ", b))



      ## predict raster---------------------------------------------------------

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
          fun = a, # function to be applied
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
          file = file.path(mypath, paste0(output.name, "_xy_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), b, "_", a, ".csv"))
          )


        # remove stuff
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
         fun = a,
         type = b,
         clamp = clamp.pred,
         progress = TRUE
        )

       # take buffer around points
       xy_buffer <- terra::buffer(
         x = xy_sv,
         width = buffer.dist
       )


       # create table for loop
       xy_predict_output <- semi_join(x = xy_import_withdata, y = xy_import, by = c("x", "y")) %>%
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
         file = file.path(mypath, paste0(output.name, "_xy_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), b, "_", a, "_", buffer.dist, "m_buffer.csv"))
       )

       # remove stuff
       rm(xy_predict_raster) # raster
       rm(xy_predict_output)

      } # end of if (buffer.pred = TRUE) statement


      # success message
      cli::cli_alert_success(paste0("finished predicting xy suitability for function: ", a, " | ", b))



    } # end of for(b in predict.type) statement

  } # end of for(a in predict.fun) statement

  # status update
  cli::cli_alert_success("finished predicting xy suitability")

} # end of function

