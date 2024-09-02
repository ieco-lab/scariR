#'Output summary statistics for a MaxEnt model without cross-validation ('Maxent' object)
#'
#'@description
#'This function will create a directory for and save a MaxEnt model that was run
#'using the `SDMtune` R package. It will use the model to calculate a list of
#'summary statistics based on the test data and covariates given. See 'return'
#'for a list of summary statistics outputs.
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
#'covariates are used to train and test the MaxEnt model, as well as to make
#'predictions. These should be the same covariates that you used to train the
#'model. This must a `SpatRaster` object created using [terra::rast()].
#'
#'@param train.obj The main group of presence and background points used to
#'train the model. Should be a SWD object, created using the
#'[SDMtune::prepareSWD()] function.
#'
#'
#'@param test.obj A withheld group of presence and background points used to
#'test the model after training. Should be a SWD object, created using the
#'[SDMtune::prepareSWD()] function.
#'
#'@param plot.fun Character. Default is "mean". The function used to determine
#'the level of the other variables when creating the marginal curve. Can be one
#'of: `min`, `mean`, `median`, `max`, or `sd` (standard deviation).
#'
#'@param plot.type Character. Default is "cloglog".The type of output
#'desired for the marginal and univariate response curves. Choices are "logistic"
#'and "cloglog".If both are used, must be concatenated in the form:
#'`c("logistic", "cloglog")`.
#'
#'@param jk.test.type Character. Default is "test". When a jackknife test is
#'conducted, this specifies whether to conduct the test on the training or the
#'test dataset. Choices are one of both of "train" and "test". If both are used,
#'must be concatenated in the form: `c("train", "test")`.
#'
#'@details
#'
#'The function requires the packages 'tidyverse', 'here', 'devtools', 'rJava', 'dismo', 'SDMtune', 'viridis', 'plotROC', 'cli', and 'terra'.
#'
#'@return
#'The output of this function includes the following:
#'
#'* training and test datasets used for the model
#'* listed model parameters and suitability thresholds
#'* variable contributions, permutation importance and SD
#'* confusion matrix
#'* jackknife tests for both training and testing data
#'* jackknife plots
#'* AUC / TSS
#'* ROC plots
#'* marginal and univariate response curves
#'
#'@examples
#'
#'# ARGUMENT USAGE:
#'# model.obj = regional_native_model
#'# model.name = "regional_native"
#'
#'mypath <- file.path(here::here() %>%
#'                     dirname(),
#'                      "maxent/models/slf_regional_native_v3")
#'
#'jk.test.type <- c("train", "test") # used to produce jackknife plots
#'plot.type <- c("cloglog", "logistic") # used to produce marginal and univariate response curves
#'
#'# EXAMPLE USAGE:
#'slfSpread::compute_MaxEnt_summary_statistics(
#' model.obj = regional_native_model,
#' model.name = "regional_native",
#' mypath = mypath,
#' create.dir = TRUE, # create subdirectory
#' env.covar.obj = x_native_env_covariates, # env covariates raster stacked
#' train.obj = regional_native_train, # training data used to create model
#' test.obj = regional_native_test, # data you wish to use to test the model
#' plot.type = c("cloglog", "logistic"), # types of univariate and marginal response curves to be created
#' jk.test.type = c("train", "test") # types of jackknife curves to be created
#')
#'
#'@export
compute_MaxEnt_summary_statistics <- function(model.obj, model.name = "MODEL", mypath, create.dir = FALSE, env.covar.obj, train.obj, test.obj, plot.fun = "mean", plot.type = "cloglog", jk.test.type = "test") {

    ## Error checks-------------------------------------------------------------

    # check if maxent is installed
    checkMaxentInstallation(verbose = TRUE)
    # stop if it isnt installed
    if (checkMaxentInstallation() == FALSE) {
      cli::cli_abort("The latest version of Java and MaxEnt for Java must be installed")
      stop()
    }

    # ensure objects are character type
    if (is.character(model.name) == FALSE) {
      cli::cli_abort("Parameter 'model.name' must be of type character")
      stop()
    }
    if (is.character(jk.test.type) == FALSE) {
      # error message
      cli::cli_abort("Parameter 'jk.test.type' must be of type character")
      stop()
    }
    if (is.character(plot.fun) == FALSE) {
      cli::cli_abort("Parameter 'plot.fun' must be of type character")
      stop()
    }
    if (is.character(plot.type) == FALSE) {
      cli::cli_abort("Parameter 'plot.type' must be of type character")
      stop()
    }
    if (is.character(mypath) == FALSE) {
      cli::cli_abort("Parameter 'mypath' must be of type character")
      stop()
    }



    ## Create sub directory for files-------------------------------------------

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
      cli::cli_abort("'create.dir' must be of type 'logical'")
      stop()

    }



    ## MaxEnt settings----------------------------------------------------------

    # set memory usage for Java
    options(java.parameters = "-Xmx2048m")



    ## Save MaxEnt model object-------------------------------------------------

    # save model
    readr::write_rds(model.obj, file = file.path(mypath, paste0(model.name, "_model.rds")))



    ## Get AUC, TSS, and variable contributions---------------------------------

    # vector of AUC values
    AUC <- c(
      # get training AUC value
      SDMtune::auc(model.obj),
      # get test AUC value
      SDMtune::auc(model.obj, test = test.obj)
    )

    # vector of TSS values
    TSS <- c(
      # get training TSS value
      SDMtune::tss(model.obj),
      # get test TSS value
      SDMtune::tss(model.obj, test = test.obj)
    )

    # compile to data frame
    auc_tss <- data.frame(AUC, TSS, row.names = c("Training", "Test"))

    # write to .csv
    # path to directory

    # write csv
    write.csv(auc_tss, file = file.path(mypath, paste0(model.name, "_auc_tss.csv")), row.names = TRUE)



    # variable importance
    var_imp <- SDMtune::maxentVarImp(model.obj)
    # write to csv
    write.csv(var_imp, file = file.path(mypath, paste0(model.name, "_var_contrib.csv")), row.names = FALSE)



    ## Plot ROC, univariate and marginal response curves------------------------


    ### Plot ROC Curves---------------------------------------------------------

    # status update
    cli::cli_alert_success("plotting ROC curves")



    # load in temp plot object
    plot.ROC <- SDMtune::plotROC(model.obj, test = test.obj)

    # add title
    plot.ROC <- plot.ROC +
      ggtitle(paste0("ROC curve (sensitivity vs 1 - specificity)- '", model.name, " model'"))

    # save output
    ggsave(plot.ROC,
           filename = file.path(mypath, "plots", paste0(model.name, "_ROC.jpg")),
           height = 8,
           width = 10,
           device = "jpeg",
           dpi = "retina")



    ### Plot Univariate Response Curves-----------------------------------------

    # status update
    cli::cli_alert_success("plotting univariate and marginal response curves")



    # for each variable used in the model, create a response curve
    for (a in names(env.covar.obj)) {

      # for each threshold type, plot a response curve
      for (b in plot.type) {

        # load in temp plot object
        plot.univar <- SDMtune::plotResponse(
          model = model.obj,
          var = a,
          type = b,
          only_presence = TRUE,
          rug = TRUE
        )

        # add title
        plot.univar <- plot.univar +
          ggtitle(paste0("'", model.name, " model'- ", a, " univariate response curve: ", b)) +
          labs(caption = "rug plots: top = presences, bottom = background/pseudoabsences")

        # save output
        ggsave(plot.univar,
               filename = file.path(mypath, "plots", paste0(model.name, "_", a, "_univar_resp_curve_", b, ".jpg")),
               height = 8,
               width = 10,
               device = "jpeg",
               dpi = "retina")

        # remove temp object
        rm(plot.univar)

      }

    }



    ### Plot Marginal Response Curves-------------------------------------------

    # for each variable used in the model, create a response curve
    for (a in names(env.covar.obj)) {

      # for each threshold type, plot a response curve
      for (b in plot.type) {

        for (c in plot.fun) {

          # load in temp plot object
          plot.response <- SDMtune::plotResponse(
            model = model.obj,
            var = a,
            type = b,
            fun = c,
            only_presence = TRUE,
            marginal = TRUE,
            rug = TRUE
          )

          # add title
          plot.response <- plot.response +
            ggtitle(paste0("'", model.name, " model'- ", a, " marginal response curve: ", b, " | ", c)) +
            labs(caption = "rug plots: top = presences, bottom = background/pseudoabsences")

          # save output
          ggsave(plot.response,
                 filename = file.path(mypath, "plots", paste0(model.name, "_", a, "_marg_resp_curve_", b, "_", c, ".jpg")),
                 height = 8,
                 width = 10,
                 device = "jpeg",
                 dpi = "retina")

          # remove temp object
          rm(plot.response)

        }

      }

    }



    ## Extract summary statistics-----------------------------------------------

    cli::cli_alert_success("writing summary statistics")

      # load in temp object
      data.obj <- as.data.frame(model.obj@model@results)

      # change colname
      colnames(data.obj) <- paste0("statistic_value")
      # write to csv
      write.csv(
        x = data.obj,
        file = file.path(mypath, paste0(model.name, "_summary.csv")),
        row.names = TRUE
      )



    ## Write training and testing data to file----------------------------------

    # write training data to csv
    SDMtune::swd2csv(swd = train.obj, file_name = file.path(mypath, paste0(model.name, "_train.csv")))

    # write testing data to csv
    SDMtune::swd2csv(swd = test.obj, file_name = file.path(mypath, paste0(model.name, "_test.csv")))



    ## Jackknife test for variable importance-----------------------------------

    # status update
    cli::cli_alert_success("performing jackknife tests")


    # jackknife
    jk.obj <- SDMtune::doJk(
      model = model.obj,
      test = test.obj,
      metric = "auc",
      with_only = TRUE, # also run test for each variable alone
      progress = TRUE
      )

    # write raw data to csv
    write_csv(x = jk.obj, file = file.path(mypath, paste0(model.name, "_jackknife_training_testing.csv")))

    # sub-loop to output jackknife plots
      for(a in jk.test.type) {

        # plot jackknife
        jk.obj_plot <- plotJk(jk = jk.obj, type = a)

        # modify objects to add titles and subtitles
        jk.obj_plot <- jk.obj_plot +
          ggtitle(paste0("'", model.name, " model'- jackknife test")) +
          labs(subtitle = paste0(a, "ing data"))

        ggsave(jk.obj_plot,
               filename = file.path(mypath, "plots", paste0(model.name, "_jackknife_", a, "ing.jpg")),
               height = 8,
               width = 10,
               device = "jpeg",
               dpi = "retina")

        # remove temp object
        rm(jk.obj_plot)

    }

    # remove temp object
    rm(jk.obj)



    ## Variable importance------------------------------------------------------

    # status update
    cli::cli_alert_success("computing variable importance")

    # calculate variable importance
    var_imp <- SDMtune::varImp(model = model.obj, progress = TRUE)
    # write to csv
    write_csv(x = var_imp, file = file.path(mypath, paste0(model.name, "_variable_importance.csv")))

    # plot
    var_imp_plot <- SDMtune::plotVarImp(df = var_imp)

    var_imp_plot <- var_imp_plot +
      ggtitle(paste0("Variable importance for '", model.name, " model'"))

    ggsave(var_imp_plot,
           filename = file.path(mypath, "plots", paste0(model.name, "_variable_importance.jpg")),
           height = 8,
           width = 10,
           device = "jpeg",
           dpi = "retina")



    ## Confusion Matrix for common threshold values-----------------------------

    # status update
    cli::cli_alert_success("generating confusion matrices")

    # load in summary file and slice the threshold values
    conf.matr.data <- read.csv(file.path(mypath, paste0(model.name, "_summary.csv"))) %>%
      dplyr::slice(18, 22, 26, 30, 34, 38, 42, 46, 50) # the rows containing the threshold data



    # create empty table with threshold labels
    conf.matr.output <- as.data.frame(conf.matr.data[, 1]) %>%
      dplyr::rename("hold_type" = "conf.matr.data[, 1]")


    # calculate confusion matrix using SDMtune
    conf.matr.hold <- SDMtune::confMatrix(
      model = model.obj,
      test = test.obj,
      type = "cloglog",
      th = conf.matr.data[, ncol(conf.matr.data)] # the mean value for all cloglog thresholds
    )

    # bind threshold names
    conf.matr.hold <- cbind(conf.matr.hold, conf.matr.data[, 1]) %>%
      dplyr::rename("hold_type" = "conf.matr.data[, 1]") %>%
      dplyr::select(hold_type, everything())

    # write individual run results to file
    write.table(
      x = conf.matr.hold,
      sep = ",",
      file = file.path(mypath, paste0(model.name, "_thresh_confusion_matrix.csv")),
      row.names = FALSE,
      col.names = c("threshold_type", "threshold_value", "tp", "fp", "fn", "tn")
    )


    # status update
    cli::cli_alert_success("finished creating summary statistics")

}

