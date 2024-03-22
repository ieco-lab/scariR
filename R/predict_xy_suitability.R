#'Predicts suitability for xy coordinates according to a trained MaxEnt model without cross-validation ('Maxent' object)
#'
#'@description
#'This function predicts establishment suitability based on a trained
#'MaxEnt model for a set of xy coordinates. These coordinates do not need to be
#'within the training area for the model.
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
#'@param predict.type Character. Default is "cloglog". The type of raster output
#'to be created from the trained model. Can be one of `raw`, `logistic` or
#'`cloglog`. If multiple are desired, must be in the concatenated
#'form: `c("cloglog", "logistic")`
#'
#'@param clamp.pred Logical. Default is TRUE. Should clamping be performed?
#'
#'@param output.name The name of the file output. Separate words with _. Relevant
#'information might include the name of the model used to predict, the spatial
#'scale, the temporal scale, and the type of data points that are being used for
#'predictions.
#'
#'@details
#'
#'The function requires the packages 'cli', 'tidyverse', and 'SDMtune'.
#'
#'@return
#'Returns 3 .csv files. The first file `xy_with_data` contains the input xy
#'coordinates with the value of each given environmental covariate at that
#'location. The second file `xy_no_data` contains the contains any
#'coordinates that could not be predicted because of an NA value for one or more
#'covariates at that location. The result file `xy_pred_suit`
#'contains each coordinate, the type of record and the predicted cloglog
#'suitability value.
#'
#'@examples
#'examples
#'
#'
#'@export
predict_xy_suitability <- function(xy.obj, xy.type, env.covar.obj, model.obj, mypath, predict.type = "cloglog", clamp.pred = TRUE, output.name) {

  # Error checks----------------------------------------------------------------

  # ensure objects are character type
  if (is.character(predict.type) == FALSE) {
    cli::cli_alert_danger("Parameter 'predict.type' must be of type 'character'")
    stop()
  }
  if (is.character(xy.type) == FALSE) {
    cli::cli_alert_danger("Parameter 'xy.type' must be of type 'character'")
    stop()
  }
  if (is.character(mypath) == FALSE) {
    cli::cli_alert_danger("Parameter 'mypath' must be of type 'character'")
    stop()
  }
  if (is.character(output.name) == FALSE) {
    cli::cli_alert_danger("Parameter 'output.name' must be of type 'character'")
    stop()
  }

  # ensure objects are logical type
  if (is.logical(clamp.pred) == FALSE) {
    cli::cli_alert_danger("Parameter 'clamp.pred' must be of type 'logical'")
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
    cli::cli_alert_danger("Data import must contain only coordinates in decimal degree. Columns must be longitude (column label 'x') and latitude (column label 'y'), in that order")
    stop()
  }



  # prepare swd of environmental covariates and xy------------------------------

  # status
  cli::cli_alert_success("preparing xy coordinate data for prediction")

  # get SWD object containing point location data from rasters
  xy_import_withdata_SWD <- SDMtune::prepareSWD(species = xy.type,
                                            env = env.covar.obj,
                                            p = xy_import,
                                            verbose = TRUE # print helpful messages
  )

  # save to file
  SDMtune::swd2csv(xy_import_withdata_SWD, file_name = file.path(mypath, paste0(output.name, "_xy_with_data.csv")))



  # Create table of points with no data-----------------------------------------

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
  cli::cli_alert_success("predicting suitability for xy coordinate data")

  # tidy dataset for sdmtune::predict
  xy_covariates <- semi_join(x = xy_import_withdata, y = xy_import, by = c("x", "y")) %>%
    # select only necessary columns
    dplyr::select(5:length(.))

  # loop for every function listed
    for (b in predict.type) {

      # initialization message
      print(paste0("predicting xy suitability for function: ", b))

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
      write_csv(x = xy_predict, file = file.path(mypath, paste0(output.name, "_xy_pred_suit", ifelse(clamp.pred == TRUE, "_clamped_", "_"), b, ".csv")))


      # remove prediction
      rm(xy_predict)
      # success message
      print(paste0("finished predicting xy suitability for function: ", b))

    } # end of for(b in predict.type) statement

  # status update
  cli::cli_alert_success("finished predicting xy suitability")

} # end of function

