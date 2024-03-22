#'Compute annual degree day accumulation at listed x,y coordinate locations
#'
#'@description
#'A function that computes the total annual degree day accumulation for
#'*Lycorma delicatula* development, based on monthly mean temperature data.
#'This function was developed from the work of Dennis Calvin on the development
#'of populations of *Lycorma delicatula* living on *Ailanthus altissima* at
#'different DD accumulations.
#'
#'This function was co-authored by Stephanie Lewkiewicz.
#'
#'@param x Data import. Should be a .csv file or data frame that contains
#'input temperature data. If a .csv file, file path should be in the format
#'produced by the [file.path()] function (i.e. with '/' instead of '\\').
#'See details for additional import formatting.
#'
#'@param thresh A numeric value or pre-build character string, which represents
#'a commonly used threshold (see details for list of pre-build values).
#'This sets the threshold for the number of Degree Days necessary for a
#'population of *Lycorma delicatula* to reach the desired life stage.
#'
#'@param create.output Logical. If TRUE, will create additional outputs from the
#'returned dataframe (see "return"). Additional outputs include:
#'* a .csv file of the returned dataframe
#'* a global-scale .tif raster of the output at the 5 arcminute (~10km) resolution
#'
#'@param mypath Character. Only used if create.output = TRUE. A file path to the
#'sub directory where the additional outputs will be stored. Should be
#'used with the [file.path()] function (i.e. with '/' instead of '\\').
#'If this sub directory does not already exist and should be created by the
#'function, set `create.dir` = TRUE. This will create a folder from the last
#'part of the file path in `mypath`.
#'
#'@param create.dir Logical. Should the last element of `mypath` create a sub
#'directory for the output? If TRUE, the folder name specified will be created
#'for the output. If FALSE (ie, the sub directory already exists), no directory
#'will be created.
#'
#'@details
#'
#'The function requires the packages 'tidyverse', 'here', and 'terra'
#'
#'## x:
#'Data frames / .csv files should contain 15 columns. The initial 3
#'columns should contain descriptors, with the 1st column as the raster cell
#'number (see examples) and the 2nd and 3rd columns as longitude and latitude,
#'respectively. If 1st column is unavailable, see "troubleshooting" section of
#'`examples` for code to produce this. Columns 4-15 should contain mean
#'temperature data for each month of the year (one month per column), with each
#'row being a grid cell of the raster that was used to create it. The raster
#'that was used in the original workflow was at the 5 arcminute scale.
#'
#'## thresh:
#'We have pre-built a number of important DD thresholds, as outlined by Dennis
#'Calvin's work. These pre-build values can be called by entering their name
#'as a character string. Pre-built values for `thresh` include the following:
#'
#'* "adult_emergence_1" = 991 DD (default value), when 1% of a population
#'reaches the adult life stage. **This was outlined as a critical threshold for
#'establishment in North America.**
#'* "adult_emergence_50" = 1115.5 DD, when 50% of a population reaches the
#'adult stage.
#'* "egg_laying_10" = 1564.1 DD, when 10% of a population reaches egg-laying.
#'* "egg_laying_50" = 1616.4 DD, when 50% of a population
#'reaches egg-laying
#'
#'If only using for a data frame output, create as an object, such as:
#'`object <- compute_DD_suitability()`
#'
#'If multiple thresh outputs are desired, must be in the concatenated form:
#'`c("adult_emergence_1", "egg_laying_10")`. Should be all lowercase.
#'
#'@return A data frame containing the total annual degree days and a binary
#'measure of that location's suitability for establishment by
#'*Lycorma delicatula*, per coordinate pair. It will have the same length as
#'the input and will contain the following columns:
#'
#'* line no. = line number
#'* cell = grid cell unique identifier
#'* x = longitude
#'* y = latitude
#'* total_annual_dds = total degree days in one calendar year for that
#'coordinate pair
#'* suitability = binary, 1 if total_annual_dds >= thresh,
#'0 if total_annual_dds < thresh
#'
#'IF `create.output = TRUE`
#'
#'The following additional outputs will be created:
#'#'* a .csv file of the output dataframe
#'* a global-scale .tif raster of the output at the 5 arcminute (~10km) resolution
#'
#'## Error Messages
#'An error message will be given if the input data `x` are in the incorrect
#'format (see details) or if an incompatible value is given for parameter
#'`thresh` (if value is not a preset character string or a numeric object).
#'
#'@examples
#'# x---------------------------------------------------------------------------
#'
#'#' compute_DD_suitability(x = data.object)
#'
#'
#'compute_DD_suitability(x = file.path("path", "file.csv"))
#'
#'# thresh----------------------------------------------------------------------
#'
#'# DD can be set using a preset value. "egg_laying_10" is a preset value
#'# equal to 1564.1 DDs
#'
#'compute_DD_suitability(x = data.object, thresh = "egg_laying_10")
#'
#'
#'# DD can be set manually to a numeric value
#'
#'compute_DD_suitability(x = data.object, thresh = 1000)
#'
#'# troubleshooting-------------------------------------------------------------
#'
#'# If your input data does not contain a column of cell values (this
#'# requirement is outlined in the details section), one can easily be created
#'using the seq() function:
#'
#'library(tidyverse)
#'
#'data <- data %>%
#'   mutate(cell = seq(nrow(data.object))) %>%
#'   select(cell, tidyselect::everything())
#'
#'@export
compute_DD_suitability <- function(x, thresh = "adult_emergence_1", create.output = FALSE, mypath = NA, create.dir = FALSE) {

  ## Thresh preset values-------------------------------------------------------

  # preset values for thresh parameter
  thresh_presets <- c(
    egg_laying_50 = 1616.4,
    egg_laying_10 = 1564.1,
    adult_emergence_50 = 1115.5,
    adult_emergence_1 = 991
  )

  # loop to allow multiple inputs for thresh
  for (a in thresh) {

    ## Error Checks-------------------------------------------------------------

    # check to ensure thresh parameter is numeric or a character string of preset
    # values, which are created in thresh_presets above
    if(is.numeric(a)) {
      threshold <- a

      } else if(is.element(a, names(thresh_presets))){
      threshold <- thresh_presets[a]

      } else {
        stop(paste0("thresh must be numeric or: \n    ", paste(names(thresh_presets), collapse = ' | ')))
        }

    # function defaults to read.csv if the parameter "x" is a character vector
    # (assumes this represents a file path). Otherwise, it is loaded in from the
    # environment
    if(is.character(x)) {
      data <- read.csv(x) # read as csv

      } else if(is.data.frame(x)){
        data <- as.data.frame(x) # just read in if its a df

        } else {
          data <- as.data.frame(x) # make data frame
          # as.data.frame added 2023-09-11
    }

    # checks to ensure input data are in correct format. Correct input assumes
    # that columns 4-15 are monthly temperature data and that columns 1-3 are
    # descriptive
    if(ncol(data) != 15) {
      stop("Format of data incorrect. See ?compute_DD_suitability for formatting instructions")
    }

    ## Create dependency objects------------------------------------------------

    # object containing number of days in each month (Jan -> Dec)
    days_per_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)

    # indices of input data frame columns containing temperature data
    temp_indices <- seq(from = 4, to = ncol(data), by = 1)

    ## Main Function Body-------------------------------------------------------

    # function that computes total annual degree days
    degree_days <- function(x) {
      # degree days in one day at average monthly temperatures
      dds_at_avg_temp <- pmin(30 - 10.4, pmax(0, x[temp_indices] - 10.4))
      # total degree days accumulated over one year
      total_annual_dds <- sum(dds_at_avg_temp * days_per_month)
      return (total_annual_dds)
      }

    # create new data frame for degree day accumulations
    dds <- data.frame(data[, 1:3])

    # apply degree_day function to data frame and bind new column of
    # degree day accumulations to output data frame
    dds <- cbind(dds, total_annual_dds = apply(data, 1, degree_days))

    # create function to identify sufficient degree day accumulation
    enough_dds <- function(x){
      if (x[4] >= threshold) {
        return (1)
        } else {
          return (0)
        }
    }

    # apply enough_dds function to data frame and bind new column of
    # suitability binaries to output data frame
    dds <- cbind(dds, suitability = apply(dds, 1, enough_dds))

    # return dds data.frame as output
    return(dds)



    ## Create alternative outputs-----------------------------------------------

    if(create.output == TRUE) {

      ## Create sub directory for files-----------------------------------------

      if (create.dir == FALSE) {
        # print message
        print("proceeding without creating model subdirectory folder")

      } else if (create.dir == TRUE) {
        # create sub directory from ending of mypath object
        dir.create(mypath)
        # print message
        print(paste0("sub directory for files created at: ", mypath))

      }

      ## Error checks for mypath------------------------------------------------

      # ensure directory mypath exists
      if (dir.exists(mypath) == FALSE) {
        stop("Sub directory specified in object 'mypath' does not exist")
      }

      # loop to load in mypath object
      # if not NA and a character string, import
      if (!is.na(mypath) & is.character(mypath)) {
        mypath <- mypath

        # otherwise, stop and warn
      } else {
        stop("parameter 'mypath' must be specified and of type 'character'")

      }

      ## thresh naming object---------------------------------------------------

      # if thresh is numeric, change name to include numeric
      if(is.numeric(a)) {
        thresh_name <- paste0("DD_", a)

        # else, import name as is
      } else if(is.element(a, names(thresh_presets))) {
        thresh_name <- a
      }

      ## Write dds to csv-------------------------------------------------------

      write.csv(x = dds, file = file.path(mypath, paste0(thresh_name, "_DD_suitable_area_global.csv")))

      ## convert dds to raster--------------------------------------------------

      dds_raster <- terra::rast(x = dds)

      # convert 0s to NAs
      dds_raster <- terra::subst(x = dds_raster, from = 0, to = NA)

      # save raster output
      writeRaster(x = dds_raster, filename = file.path(mypath, paste0(thresh_name, "_DD_suitable_area_global.tif")), filetype = "GTiff", overwrite = FALSE)

      # confirmation message
      print(paste0(thresh_name, " raster created and saved at: \n", mypath))

      ## minmax values----------------------------------------------------------

      # check to ensure proper range of values
      print(paste0("Minimum and Maximum values of created raster: \n    ", terra::minmax(dds_raster)))

    } # end of if(create.output == TRUE) statement

  } # end of for (a in thresh) statement

}
