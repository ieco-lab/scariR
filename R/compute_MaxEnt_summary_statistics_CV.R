#'Output summary statistics for a MaxEnt model ('SDMmodelCV' object)
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
#'@param trainFolds.obj A list of two matrices that specify the fold of the
#'training and testing points. This object is used to create k-fold partitions
#'from presence and background datasets to train a `SWDmodelCV` object. This is
#'created using the [SDMtune::randomFolds()] function.
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
#'* K-folds and which samples were included per fold
#'* variable contributions, permutation importance and SD
#'* confusion matrices per iteration
#'* jackknife tests for both training and testing data, per iteration
#'* jackknife plots
#'* AUC / TSS
#'* ROC plots
#'* marginal and univariate response curves
#'
#'@examples
#'
#'# ARGUMENT USAGE:
#'# model.obj = global_model
#'# model.name = "global"
#'
#'```R
#'
#'mypath <- file.path(here::here() %>%
#'                     dirname(),
#'                      "maxent/models/slf_global_v3")
#'
#'jk.test.type <- c("train", "test") # used to produce jackknife plots
#'plot.type <- c("cloglog", "logistic") # used to produce marginal and univariate response curves
#'
#'# EXAMPLE USAGE:
#'scari::compute_MaxEnt_summary_statistics_CV(
#' model.obj = global_model,
#' model.name = "global",
#' mypath = mypath,
#' create.dir = FALSE, # create subdirectory
#' env.covar.obj = x_global_hist_env_covariates, # env covariates raster stacked
#' train.obj = global_train, # training data used to create model
#' trainFolds.obj = global_trainFolds,  # k-folds of training data
#' test.obj = global_test, # data you wish to use to test the model
#' jk.test.type = c("train", "test"), # types of jackknife curves to be created
#' plot.type = c("cloglog", "logistic") # types of univariate and marginal response curves to be created
#')
#'
#'```
#'
#'@export
compute_MaxEnt_summary_statistics_CV <- function(model.obj, model.name = "MODEL", mypath, create.dir = FALSE, env.covar.obj, train.obj, trainFolds.obj, test.obj, plot.fun = "mean", plot.type = "cloglog", jk.test.type = "test") {

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

    ## Create sub directory for files-----------------------------------------------

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



    # loop that plots an ROC curve for each model iteration
    for (a in seq(length(model.obj@models))) {

      # load in temp plot object
      plot.ROC <- SDMtune::plotROC(model.obj@models[[a]], test = test.obj)

      # add title
      plot.ROC <- plot.ROC +
        ggtitle(paste0("ROC curve (sensitivity vs 1 - specificity)- '", model.name, " model', iteration ", a))

      # save output
      ggsave(plot.ROC,
             filename = file.path(mypath, "plots", paste0(model.name, "_", "iteration", a, "_ROC.jpg")),
             height = 8,
             width = 10,
             device = jpeg,
             dpi = "retina")

      # remove temp object
      rm(plot.ROC)

    }

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
               device = jpeg,
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
                 device = jpeg,
                 dpi = "retina")

          # remove temp object
          rm(plot.response)

        }

      }

    }

    ## Extract summary statistics-----------------------------------------------

    cli::cli_alert_success("writing summary statistics")

    # create empty date frame for joining
    empty.table <- as.data.frame(model.obj@models[[1]]@model@results) %>%
      mutate(statistic = row.names(model.obj@models[[1]]@model@results)) %>% # add column of statistic names
      dplyr::select(statistic)

    # loop to write summary statistics for each iteration of the model
    for (i in seq(length(model.obj@models))) {

      # load in temp object
      data.obj <- as.data.frame(model.obj@models[[i]]@model@results)

      # change colname
      colnames(data.obj) <- paste0("iteration_", i, "_summary")
      # write to csv
      write.csv(
        x = data.obj,
        file = file.path(mypath, paste0(model.name, "_summary_iteration", i, ".csv")),
        row.names = TRUE
      )

      # add column name to data object
      data.obj <- mutate(data.obj, statistic = row.names(model.obj@models[[1]]@model@results))

      # join data.obj temporary object to empty table
      empty.table <- right_join(empty.table, data.obj, by = "statistic")

      # if i has reached the length of the model replicates, compute a mean and write the final table to .csv
      if (i == length(model.obj@models)) {
        # compute the mean of the columns
        empty.table <- mutate(empty.table, mean = rowMeans(empty.table[, -1]))
        # write to csv
        write.csv(
          x = empty.table,
          file = file.path(mypath, paste0(model.name, "_summary_all_iterations.csv")),
          row.names = FALSE
        )

      }

      # remove temp object
      rm(data.obj)

    }

    ## Write training and testing data to file----------------------------------

    # write training data to csv
    SDMtune::swd2csv(swd = train.obj, file_name = file.path(mypath, paste0(model.name, "_train_Kfold.csv")))
    # load csv in again
    trainFolds.obj_output <- read.csv(file = file.path(mypath, paste0(model.name, "_train_Kfold.csv")))
    # bind training data with folds
    trainFolds.obj_output <- cbind(trainFolds.obj_output, trainFolds.obj)
    # overwrite csv for training
    write_csv(x = trainFolds.obj_output, file = file.path(mypath, paste0(model.name, "_train_Kfold.csv")))

    # write testing data to csv
    SDMtune::swd2csv(swd = test.obj, file_name = file.path(mypath, paste0(model.name, "_test.csv")))

    ## Jackknife test for variable importance-----------------------------------

    # status update
    cli::cli_alert_success("performing jackknife tests")

    ### Jackknife test for entire model training--------------------------------

    # jackknife
    JK.obj_df <- SDMtune::doJk(
      model = model.obj,
      # test = test.obj, # not used for models with replicates
      metric = "auc",
      with_only = TRUE, # also run test for each variable alone
      progress = TRUE
      )

    # write raw data to csv
    write_csv(x = JK.obj_df, file = file.path(mypath, paste0(model.name, "_jackknife_all_iterations_training.csv")))

    # plot jackknife
    JK.obj_df_plot <- plotJk(jk = JK.obj_df, type = "train")

    JK.obj_df_plot <- JK.obj_df_plot +
      ggtitle(paste0("'", model.name, " model'- jackknife test")) +
      labs(subtitle = "all iterations, training data")

    ggsave(JK.obj_df_plot,
               filename = file.path(mypath, "plots", paste0(model.name, "_jackknife_all_iterations_training.jpg")),
               height = 8,
               width = 10,
               device = jpeg,
               dpi = "retina")


    ### Jackknife test for each model iteration--------------------------------

    # for loop to output csv file of jackknife test, one per model iteration
    for (a in seq(length(model.obj@models))) {

      # jackknife
      jk.obj <- SDMtune::doJk(
        model = model.obj@models[[a]],
        test = test.obj,
        metric = "auc",
        with_only = TRUE, # also run test for each variable alone
        progress = TRUE
        )

      # write raw data to csv
      write_csv(x = jk.obj, file = file.path(mypath, paste0(model.name, "_jackknife_iteration", a, "_training_testing.csv")))

      # sub-loop to output 2 types of plots per test, training and testing
        for(b in jk.test.type) {

          # plot training jackknife
          jk.obj_plot <- plotJk(jk = jk.obj, type = b)

          # modify objects to add titles and subtitles
          jk.obj_plot <- jk.obj_plot +
            ggtitle(paste0("'", model.name, " model'- jackknife test")) +
            labs(subtitle = paste0("iteration ", a, ", ", b, "ing data"))

          ggsave(jk.obj_plot,
                 filename = file.path(mypath, "plots", paste0(model.name, "_jackknife_iteration", a, "_", b, "ing.jpg")),
                 height = 8,
                 width = 10,
                 device = jpeg,
                 dpi = "retina")

          # remove temp object
          rm(jk.obj_plot)

      }

      # remove temp object
      rm(jk.obj)

    }

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
           device = jpeg,
           dpi = "retina")

    ## Confusion Matrix for common threshold values-----------------------------

    # status update
    cli::cli_alert_success("generating confusion matrices")

    # load in summary file and slice the threshold values
    conf.matr.data <- read.csv(file.path(mypath, paste0(model.name, "_summary_all_iterations.csv"))) %>%
      dplyr::slice(18, 22, 26, 30, 34, 38, 42, 46, 50) # the rows containing the threshold data



    # create empty table with threshold labels
    conf.matr.output <- as.data.frame(conf.matr.data[, 1]) %>%
      dplyr::rename("hold_type" = "conf.matr.data[, 1]")

    for (a in seq(length(model.obj@models))) {

      conf.matr.hold <- SDMtune::confMatrix(
        model = model.obj@models[[a]],
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
        file = file.path(mypath, paste0(model.name, "_thresh_confusion_matrix_iteration", a, ".csv")),
        row.names = FALSE,
        col.names = c("threshold_type", "threshold_value", "tp", "fp", "fn", "tn")
      )

      # before all model iterations have been summarized, append to empty table
      while (a < length(model.obj@models)) {
        # join conf.matr.hold temporary object to empty table
        conf.matr.output <- right_join(conf.matr.output, conf.matr.hold, by = "hold_type")

        break

      }

      # when all model iterations have been appended, take the mean threshold value and write to csv
      if (a == length(model.obj@models)) {
        # compute the mean of the 5 columns
        conf.matr.output <- mutate(conf.matr.output,
                                   tp_mean = rowMeans(dplyr::across(contains("tp"))), # compute the average of all "tp" columns
                                   fp_mean = rowMeans(dplyr::across(contains("fp"))),
                                   fn_mean = rowMeans(dplyr::across(contains("fn"))),
                                   tn_mean = rowMeans(dplyr::across(contains("tn"))),
                                   th = rowMeans(dplyr::across(contains("th")))
        ) %>%
          dplyr::select(hold_type, th, tp_mean, fp_mean, fn_mean, tn_mean) %>%
          rename("threshold_value" = "th",
                 "threshold_type" = "hold_type") %>%
          dplyr::select(threshold_type, threshold_value, everything())

        # write to csv
        write.csv(
          x = conf.matr.output,
          file = file.path(mypath, paste0(model.name, "_thresh_confusion_matrix_all_iterations.csv")),
          row.names = FALSE
        )

      }

      #remove temp object
      rm(conf.matr.hold)

    }

    # status update
    cli::cli_alert_success("finished creating summary statistics")

}

