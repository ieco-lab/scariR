#'Create a model output
#'
#'
#'The function requires the packages 'tidyverse', 'here', 'devtools', 'rJava', 'dismo', 'SDMtune', 'viridis', 'plotROC' and 'terra'.
#'
#'@param model.obj
#'
#'@param model.name
#'
#'@param mypath
#'
#'@param env.covar.obj
#'
#'@param train.obj
#'
#'@param trainFolds.obj
#'
#'@param test.obj
#'
#'@param plot.types
#'
#'@param threshold.types
#'
#'
#'
#'
#'@details
#'
#'model.obj <- entire_easternUSA_model
#'model.name <- "entire_easternUSA_model"
#'mypath <- file.path(here() %>%
#'                     dirname(),
#'                                   "maxent/models/slf_easternUSA_entire_step1")
#'env.covar.obj <- x_env_covariates
#'train.obj <- entire_easternUSA_train
#'trainFolds.obj <- entire_easternUSA_trainFolds
#'test.obj <- entire_easternUSA_test
#'plot.types <- c("train", "test") # used to produce jackknife plots
#'threshold.types <- c("cloglog", "logistic") # used to produce marginal and univariate response curves
#'
#'@return
#'
#'All MaxEnt Summary Statistics
#'
#'
#'@examples
#'
#'TBD
#'
#'@export
compute_MaxEnt_summary_statistics <- function(model.obj, model.name, mypath, env.covar.obj, train.obj, trainFolds.obj, test.obj, plot.types, threshold.types) {

    ## Error checks-------------------------------------------------------------

    # check if maxent is installed
    checkMaxentInstallation(verbose = TRUE)
    # stop if it isnt installed
    if (checkMaxentInstallation() == FALSE) {
      stop("The latest version of Java and MaxEnt for Java must be installed")
    }

    # ensure objects are character type
    if (is.character(model.name) == FALSE) {
      stop("Parameter 'model.obj' must be of type character")
    }
    if (is.character(plot.types) == FALSE) {
      stop("Parameter 'plot.types' must be of type character")
    }
    if (is.character(threshold.types) == FALSE) {
      stop("Parameter 'threshold.types' must be of type character")
    }
    if (is.character(mypath) == FALSE) {
      stop("Parameter 'mypath' must be of type character")
    }

    ## MaxEnt settings----------------------------------------------------------

    # set memory usage for Java
    options(java.parameters = "-Xmx2048m")

    ## Save MaxEnt model object-------------------------------------------------

    # save model
    saveRDS(object = model.obj, file = file.path(mypath, paste0(model.name, ".rds")))

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

    # loop that plots an ROC curve for each model iteration
    for (a in seq(length(model.obj@models))) {

      # load in temp plot object
      plot.ROC <- SDMtune::plotROC(model.obj@models[[a]], test = test.obj)

      # add title
      plot.ROC <- plot.ROC +
        ggtitle(paste("ROC curve (sensitivity  vs 1 - Specificity) for", model.name, "iteration", a))

      # save output
      ggsave(plot.ROC,
             filename = file.path(mypath, "plots", paste0(model.name, "_", "iter", a, "_ROC.jpg")),
             height = 8,
             width = 10,
             device = "jpeg",
             dpi = "retina")

      # remove temp object
      rm(plot.ROC)

    }

    ### Plot Univariate Response Curves-----------------------------------------

    # for each variable used in the model, create a response curve
    for (a in names(env.covar.obj)) {

      # for each threshold type, plot a response curve
      for (b in threshold.types) {

        # load in temp plot object
        plot.univar <- SDMtune::plotResponse(
          model = model.obj,
          var = a,
          type = b,
          fun = "mean",
          rug = TRUE
        )

        # add title
        plot.univar <- plot.univar +
          ggtitle(paste(model.name, a, "univariate response curve-", b, "output"))

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
      for (b in threshold.types) {

        # load in temp plot object
        plot.response <- SDMtune::plotResponse(
          model = model.obj,
          var = a,
          type = b,
          marginal = TRUE,
          fun = "mean",
          rug = TRUE
        )

        # add title
        plot.response <- plot.response +
          ggtitle(paste(model.name, a, "marginal response curve-", b, "output"))

        # save output
        ggsave(plot.response,
               filename = file.path(mypath, "plots", paste0(model.name, "_", a, "_marg_resp_curve_", b, ".jpg")),
               height = 8,
               width = 10,
               device = "jpeg",
               dpi = "retina")

        # remove temp object
        rm(plot.response)

      }

    }

    ## Extract summary statistics-----------------------------------------------

    # create empty date frame for joining
    empty.table <- as.data.frame(model.obj@models[[1]]@model@results) %>%
      mutate(statistic = row.names(model.obj@models[[1]]@model@results)) %>% # add column of statistic names
      dplyr::select(statistic)

    # loop to write summary statistics for each iteration of the model
    for (i in seq(length(model.obj@models))) {

      # load in temp object
      data.obj <- as.data.frame(model.obj@models[[i]]@model@results)

      # change colname
      colnames(data.obj) <- paste0("iter_", i, "_summary")
      # write to csv
      write.csv(
        x = data.obj,
        file = file.path(mypath, paste0(model.name, "_summary_iter", i, ".csv")),
        row.names = TRUE
      )

      # add column name to data object
      data.obj <- mutate(data.obj, statistic = row.names(model.obj@models[[1]]@model@results))

      # while i has not reached the length of the model replicates, append the summary statistics to the empty table
      while (i < length(model.obj@models)) {
        # join data.obj temporary object to empty table
        empty.table <- right_join(empty.table, data.obj, by = "statistic")

        break

      }

      # if i has reached the length of the model replicates, compute a mean and write the final table to .csv
      if (i == length(model.obj@models)) {
        # compute the mean of the 5 columns
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
    write_csv(x = JK.obj_df, file = file.path(mypath, model.name, "_jackknife_all_iterations_training.csv"))

    # plot jackknife
    JK.obj_df_plot <- plotJk(jk = JK.obj_df, type = "train")

    JK.obj_df_plot <- JK.obj_df_plot +
      ggtitle(paste0(model.name, "- jackknife test")) +
      labs(subtitle = "all iterations, training data")

    ggsave(JK.obj_df_plot,
               filename = file.path(mypath, "plots", model.name, "_jackknife_all_iterations_training.jpg"),
               height = 8,
               width = 10,
               device = "jpeg",
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
      write_csv(x = jk.obj, file = file.path(mypath, paste0(model.name, "_jackknife_iter", a, "_training_testing.csv")))

      # sub-loop to output 2 types of plots per test, training and testing
        for(b in plot.types) {

          # plot training jackknife
          jk.obj_plot <- plotJk(jk = jk.obj, type = b)

          # modify objects to add titles and subtitles
          jk.obj_plot <- jk.obj_plot +
            ggtitle(paste0(model.name, "- jackknife test")) +
            labs(subtitle = paste0("iter ", a, ", ", b, "ing data"))

          ggsave(jk.obj_plot,
                 filename = file.path(mypath, "plots", paste0(model.name, "_jackknife_iter", a, "_", b, "ing.jpg")),
                 height = 8,
                 width = 10,
                 device = "jpeg",
                 dpi = "retina")

          # remove temp object
          rm(jk.obj_plot)

      }

      # remove temp object
      rm(jk.obj)

    }

    ## Variable importance------------------------------------------------------

    # calculate variable importance
    var_imp <- SDMtune::varImp(model = model.obj, progress = TRUE)
    # write to csv
    write_csv(x = var_imp, file = file.path(mypath, model.name, "_variable_importance.csv"))

    # plot
    var_imp_plot <- SDMtune::plotVarImp(df = var_imp)

    var_imp_plot <- var_imp_plot +
      ggtitle(paste("Variable importance for", model.name))

    ggsave(var_imp_plot,
           filename = file.path(mypath, "plots", paste0(model.name, "_variable_importance.jpg")),
           height = 8,
           width = 10,
           device = "jpeg",
           dpi = "retina")

}

