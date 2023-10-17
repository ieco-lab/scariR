#'Create a model output
#'
#'
#'The function requires the packages 'tidyverse', 'here', 'devtools', 'viridis', 'pROC', 'SDMtune', and 'patchwork'
#'
#'@param model.obj
#'
#'@param model.name Character. A string matching the name of the object set for
#'`model.obj`. Exclude unnecessary phrases, such as the "_model" ending.
#'
#'@param mypath Character.A file path to the sub directory where the model
#'output will be stored. Should be used with the [file.path()] function
#'(i.e. with '/' instead of '\\').
#'
#'@param fixed.area.points Data import. Should be a .csv file or data frame
#'that contains the fixed area points. If a .csv file, file path should be in the format
#'produced by the [file.path()] function (i.e. with '/' instead of '\\').
#'See details for additional import formatting.
#'
#'@param env.covar.obj
#'
#'@param test.obj
#'
#'
#'@details
#'
#'## fixed.area.points:
#'The fixed area points dataset should be be in the following format: 3 columns,
#'with x as longitude, y as latitude and cell as the cell number from the
#'raster, in that order. Original dataset was generated using
#'`dismo::randomPoints()`
#'
#'@return blarg
#'
#'
#'
#'
#'@examples
#'
#'TBD
#'
#'@export
compute_MaxEnt_fixed_ROC <- function(model.obj, model.name, mypath, fixed.area.points, env.covar.obj, test.obj) {

  # Error Checks----------------------------------------------------------------

  # ensure objects are character type
  if (is.character(model.name) == FALSE) {
    stop("Parameter 'model.name' must be of type character")
  }
  if (is.character(mypath) == FALSE) {
    stop("Parameter 'mypath' must be of type character")
  }

  # ensure directory mypath exists
  if (dir.exists(mypath) == FALSE) {
    stop("Sub directory specified in object 'mypath' does not exist")
  }

  ## Read in fixed area points data---------------------------------------------

  # read as csv
  if (is.character(fixed.area.points)) {
    fixed.area.points_df <- read.csv(fixed.area.points)

    # just read in if its a df
  } else if (is.data.frame(fixed.area.points)) {
    fixed.area.points_df <- fixed.area.points

    # make data frame if its already an object but isnt a df
  } else {
    fixed.area.points_df <- as.data.frame(fixed.area.points)

  }

  # Make predictions for test dataset-------------------------------------------

  # read in file written with compute_MaxEnt_summary_statistics
  fixed_presences <- read.csv(file = file.path(mypath, paste0(model.name, "_test.csv"))) %>%
    dplyr::filter(pa == 1)
  # select only data columns
  fixed_presences <- dplyr::select(fixed_presences, 5:ncol(fixed_presences))

  # predict function
  fixed_presences_predict <- SDMtune::predict(
    object = model.obj, # model
    data = fixed_presences, # data for prediction
    fun = "mean", # function to be applied
    type = "cloglog", # default for MaxEnt
    clamp = FALSE, # dont do clamping to restrict predictions
    progress = TRUE # progress bar
  ) %>%
    as.data.frame() %>%
    rename("cloglog_suitability" = ".")

  # r bind rows and write to csv
  # read in dataset again
  fixed_presences <- read.csv(file = file.path(mypath, paste0(model.name, "_test.csv"))) %>%
    dplyr::filter(pa == 1) %>%
    dplyr::select(Species, X, Y)

  # bind rows
  fixed_presences <- cbind(fixed_presences, fixed_presences_predict)

  fixed_presences <-   dplyr::rename(fixed_presences, "x" = "X", "y" = "Y")

  # save output
  write_csv(x = fixed_presences, file = file.path(mypath, paste0(model.name, "_predicted_suitability_test_presences.csv")))

  # Make predictions for fixed background points--------------------------------

  # load fixed background points and extract raster values
  a_fixed_background <- fixed.area.points_df %>%
    dplyr::select(-cell)

    # get SWD object containing point location data from rasters
  a_fixed_background <- SDMtune::prepareSWD(species = "Lycorma delicatula",
                                            env = x_env_covariates,
                                            a = a_fixed_background,
                                            verbose = TRUE
    )

  # make predictions for background dataset
  a_fixed_background_predict <- SDMtune::predict(
    object = model.obj, # model
    data = a_fixed_background, # data for prediction
    fun = "mean", # function to be applied
    type = "cloglog", # default for MaxEnt
    clamp = FALSE, # dont do clamping to restrict predictions
    progress = TRUE # progress bar
  ) %>%
    as.data.frame() %>%
    rename("cloglog_suitability" = ".")

  # bind rows and write to csv

  # read in dataset again
  a_fixed_background2 <- fixed.area.points_df %>%
    dplyr::select(-cell)

  # bind rows
  a_fixed_background2 <- cbind(a_fixed_background2, a_fixed_background_predict)

  # save output
  write_csv(x = a_fixed_background2, file = file.path(mypath, paste0(model.name, "_predicted_suitability_fixed_background.csv")))

  # Format data for pROC--------------------------------------------------------

  # format data for ROC curve

  # add response column (pb)
  fixed_presences <- dplyr::mutate(fixed_presences, pb = 1) %>%
    # also drop species name
    dplyr::select(-Species)
  # add response column
  a_fixed_background2 <- dplyr::mutate(a_fixed_background2, pb = 0)

  # join datasets
  fixed_ROC_df <- full_join(fixed_presences, a_fixed_background2, by = c("x", "y", "pb", "cloglog_suitability")) %>%
    dplyr::select(-c(x, y))

  # Create ROC object and plot--------------------------------------------------

  # create ROC curve

  # calculate ROC object
  fixed_ROC <- pROC::roc(
    response = fixed_ROC_df$pb, # the actual presence or background data
    predictor = fixed_ROC_df$cloglog_suitability, # the predictions made by the model
  )

  # calculate AUC as object
  fixed_ROC_AUC <- pROC::auc(roc = fixed_ROC)
  # isolate AUC numeric value
  fixed_ROC_AUC_value <- as.numeric(print(fixed_ROC_AUC)) %>%
    round(., digits = 3)

  # plot as ggplot object
  fixed_ROC_ggplot <- pROC::ggroc(
    data = fixed_ROC,
    legacy.axes = TRUE, # makes x axis 1-specificity
    colour = "red2",
    linewidth = 1
  ) +
    geom_abline(slope = 1,
                linetype = 2) + # add 0.5 null model line
    theme_bw() +
    theme(aspect.ratio = 1,
          legend.position = "right") +
    labs(title = paste0(model.name, " fixed area ROC curve and AUC")) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    annotate(geom = "text",
             x = 0.75,
             y = 0.25,
             label = paste0("Fixed Area AUC:\n", fixed_ROC_AUC_value)
    )

  # save ROC curve
  ggsave(
    filename = file.path(mypath, "plots", paste0(model.name, "_fixed_background_ROC.jpg")),
    plot = fixed_ROC_ggplot,
    device = "jpeg",
    height = 8,
    width = 10,
    dpi = "retina"
  )

  # Create csv with fixed and flexible AUC values-------------------------------


  # Fixed and flexible AUC

  # redefine AUC object
  fixed_ROC_AUC_value <- as.numeric(print(fixed_ROC_AUC)) %>%
    round(., digits = 7)

  AUC_fixed_flexible_df <- data.frame(
    "Flexible_Area_AUC" = SDMtune::auc(model = model.obj, test = test.obj),
    "Fixed_Area_AUC" = fixed_ROC_AUC_value
  )

  # write to csv
  write.csv(AUC_fixed_flexible_df, file = file.path(mypath, paste0(model.name, "_fixed_flexible_AUCs.csv")), row.names = FALSE)


  # Plot combined ROC for fixed and flexible AUC values-------------------------

  # extract test AUCs per model iteration}

  # create empty date frame for joining
  empty.table <- data.frame(
    SDMtune::auc(model.obj@models[[1]], test = test.obj)
  )

  colnames(empty.table) <- "temp"

  # loop to write summary statistics for each iteration of the model
  for (a in seq(length(model.obj@models))) {

    # load in temp object
    data.obj <- data.frame(
      SDMtune::auc(model.obj@models[[a]], test = test.obj)
    )

    # change colname
    colnames(data.obj) <- paste0("iter_", a, "_AUC")

    # while a has not reached the length of the model replicates, append the AUCs to the empty table
    while (a <= length(model.obj@models)) {
      # join data.obj temporary object to empty table
      empty.table <- cbind(empty.table, data.obj)

      break

    }

    # write new object if length is met
    if (a == length(model.obj@models)) {

      test_flexible_AUCs <- dplyr::select(empty.table, -temp)

    }

    # remove temp object
    rm(data.obj)

  }

  # take max AUC iteration
  max_AUC <- which.max(test_flexible_AUCs) %>%
    as.numeric()


  # patchwork fixed and flexible ROC curves

  # create ROC plot
  flexible_ROC_ggplot <- SDMtune::plotROC(model.obj@models[[max_AUC]], test = test.obj) +
    ggtitle("flexible area ROC")

  fixed_ROC_ggplot <- fixed_ROC_ggplot +
    labs(title = element_blank()) +
    labs(title = "fixed area ROC")

  fixed_flexible_ROC_ggplot <- fixed_ROC_ggplot + flexible_ROC_ggplot +
    patchwork::plot_annotation(title = paste0(model.name, " ROC plots"))

  #save output
  ggsave(
    filename = file.path(mypath, "plots", paste0(model.name, "_fixed_flexible_ROC_plots.jpg")),
    plot = fixed_flexible_ROC_ggplot,
    device = "jpeg",
    height = 8,
    width = 10,
    dpi = "retina"
  )

}
