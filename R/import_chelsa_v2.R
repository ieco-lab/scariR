#' Downloads CHELSA version 2.1 High Resolution Climatologies from server
#'
#'@description
#'
#'@param var.type The group of climatological variables to be accessed.
#'One of "climatologies" (bioclimatic variables), "monthly", or "daily". Default is "climatologies".
#'
#'@param var.subtype The type of climatological variable to be downloaded from server.
#'See Description for choices depending on whether `var.type` is "daily", "monthly" or "climatologies".
#'If more than one is specified, it should be formatted as follows: `c("subtype1", "subtype2")`.
#'
#'@param var.value If there are multiple versions of a variable, the number which indicates which variable to download.
#'For example, if `var.type` = "bio" and `var.value` = 12, this will download the 12th bioclimatic variable (annual precipitation).
#'
#'@param var.period The date or range of years for which to download the variable data.
#'If a date range is supplied, it should be formatted as follows: `c("date_range_start", "date_range_end")`.
#'See Description for choices depending on whether `var.type` is "daily", "monthly", or "climatologies".
#'
#'@param ssp If the variable is a projected variable, the Shared Socioeconomic Pathway (SSP) scenario to be downloaded.
#'Choices include "ssp126", "ssp245", "ssp370", and "ssp585".
#'Default is NA, which will download historical bioclim data.
#'
#'@param model If the variable is a projected variable, the climate model chosen to be downloaded.
#'Choices include "gfdl-esm4", "ukesm10-ll", "mpi-esm1-2-hr", "ipslcm6a-lr", and "mriesm2-0" (see technical specifications below for details).
#'Default is NA, which will download historical bioclim data.
#'
#'@param mypath Character. Only required if saving the report to file.
#'A file path to the sub directory where the model output will be stored.
#'Should be used with the [file.path()] function (i.e. with '/' instead of '\\').
#'If this sub directory does not already exist and should be created by the
#'function, set `create.dir` = TRUE. This will create a folder from the last
#'part of the filepath in `mypath`.
#'
#'@param create.dir Logical. Should the last element of `mypath` create a sub
#'directory for the report? If TRUE, the main folder will be created for
#'the model output. If FALSE (ie, the sub directory already exists), no directory
#'will be created.
#'
#'@param save.output Logical. Should the requested downloads be saved to file? File location
#'specified by `mypath`.
#'
#'@details
#'
#'Requires the following packages: "cli", "dplyr", "httr", "terra", "utils"
#'
#'See (CHELSA v2.1 Technical Specifications)[https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf] for more detail on specific climatologies, including their units, usage, and interpretation.
#'
#'# var.subtype
#'
#'For the argument `var.subtype`, the following options are available, depending on the input for `var.type`:
#'`var.type` == "daily":
#'- "pr" (precipitation amount)
#'- "rsds" (Surface downwelling shortwave flux in air)
#'- "tas" (mean air temperature)
#'- "tasmax" (mean maximum air temperature)
#'- "tasmin" (mean minimum air temperature)
#'
#'`var.type` == "monthly":
#'- "clt"
#'- "cmi" (climatic moisture index)
#'- "hurs" (near surface relative humidity)
#'- "pet" (potential evapotranspiration)
#'- "pr" (precipitation amount)
#'- "rsds" (Surface downwelling shortwave flux in air)
#'- "sfcWind" (Near-surface wind speed)
#'- "tas" (mean air temperature)
#'- "tasmax" (mean maximum air temperature)
#'- "tasmin" (mean minimum air temperature)
#'- "vpd" (vapor pressure deficit)
#'
#'`var.type` == "climatologies":
#'- "ai" (aridity index)
#'- "bio" (bioclimatic variables)
#'- "clt"
#'- "cmi" (climatic moisture index)
#'- "hurs" (near surface relative humidity)
#'- "pet" (potential evapotranspiration)
#'- "pr" (precipitation amount)
#'- "rsds" (Surface downwelling shortwave flux in air)
#'- "sfcWind" (Near-surface wind speed)
#'- "tas" (mean air temperature)
#'- "tasmax" (mean maximum air temperature)
#'- "tasmin" (mean minimum air temperature)
#'- "vpd" (vapor pressure deficit)
#'
#'# var.period
#'#'For the argument `var.period`, the date range should be formatted differently, depending on the input for `var.type`:
#'
#'`var.type` == "daily":
#'Date format should be "MM_DD_YYYY" for a single date, or c("MM-DD-YYYY", "MM-DD-YYYY") for a range of days.
#'
#'`var.type` == "monthly":
#'Date format should be "MM_YYYY" for a single date, or c("MM-YYYY", "MM-YYYY") for a range of months.
#'
#'`var.type` == "climatologies":
#'Date format should be a range years. Choices are "1981-2010" (historical data), "2011-2040", "2041-2070", or "2071-2100" (projected data).
#'
#'*Note* Please include all quotation marks in above formats
#'
#'@return
#'
#'Returns a spatRaster object of the requested CHELSA climatology or a saved .tif raster file(s).
#'If save.output = FALSE, the rasters will be imported into the R environment directly.
#'If save.output = TRUE, the raster will be saved to the sub directory specified by `mypath`.
#'
#'@examples
#'
#'# ARGUMENT USAGE:
#'
#'
#'@export
import_chelsa_v2 <- function(var.type = "bio", var.subtype = "bio", var.value = NA, var.period, ssp = NA, model = NA, mypath = NA, create.dir = FALSE, save.output = FALSE) {

  # Error checks----------------------------------------------------------------

  # type numeric
  if (is.numeric(var.value) == FALSE) {
    cli::cli_alert_info("Parameter 'var.value' must be of type numeric")
    stop()
  }

  # type character
  if (!is.na(model) & !is.character(model)) {
    cli::cli_alert_info("Parameter 'model' must be of type character")
    stop()

  }

  if (!is.na(ssp) & !is.character(ssp)) {
    cli::cli_alert_info("Parameter 'ssp' must be of type character")
    stop()
  }


  # Data and argument import----------------------------------------------------

  # var.type
  var.type_internal <- tolower(var.type)

  ## check to ensure proper variables entered for var.type
  if(var.type_internal != "climatologies" && var.type_internal != "monthly" && var.type_internal != "daily") {
    cli::cli_alert_info("Parameter 'var.type' must be one of 'climatologies', 'monthly', or 'daily'")
    stop()
  }


  # var.subtype
  var.subtype_internal <- tolower(var.subtype)

  # var.period
  ## establish var.period-----------------------------------------------------------

  # dates will be fed into date_input object

  # if daily, parse dates into mdy
  if(var.type_internal == "daily") {

    # if a range, break into multiple objects
    if (length(var.period) == 2) {

      # Convert to Date objects
      start_date <- lubridate::mdy(var.period[1])
      end_date <- lubridate::mdy(var.period[2])

      # Generate sequence of dates
      date_seq <- seq(start_date, end_date, by = "day")

      # Format each date as MM_DD_YYYY
      date_input <- format(date_seq, "%m_%d_%Y")

      # if not a range, take as is
    } else if (length(var.period) == 1) {
      # Convert to Date object
      date_input <- lubridate::mdy(var.period)
      # Format date as MM_DD_YYYY
      date_input <- format(date_input, "%m_%d_%Y")

    }



    # if monthly, parse dates into my format
  } else if (var.type_internal == "monthly") {

    # if a range, break into multiple objects
    if (length(var.period) == 2) {

      # Convert to Date objects
      start_date <- lubridate::my(var.period[1])
      end_date <- lubridate::my(var.period[2])

      # Generate sequence of dates
      date_seq <- seq(start_date, end_date, by = "day")

      # Format each date as MM_DD_YYYY
      date_input <- format(date_seq, "%m_%Y")

      # if not a range, take as is
    } else if (length(var.period) == 1) {
      # Convert to Date object
      date_input <- lubridate::my(var.period)
      # Format date as MM_DD_YYYY
      date_input <- format(date_input, "%m_%Y")

    }


    # if climatologies, parse dates into year range
  } else if (var.type_internal == "climatologies") {

    # if correct, import
    if (var.period %in% c("1981-2010", "2011-2040", "2041-2070", "2071-2100") == TRUE) {

      date_input <- var.period

      # else, stop
      } else {
        cli::cli_alert_info("Parameter 'var.period' must be one of '1981-2010', '2011-2040', '2041-2070', or '2071-2100' if retrieving climatologies.")
        stop()
      }

    # otherwise, stop
  } else {
    cli::cli_alert_info("Parameter 'var.period' is not in correct format. Please see details of function for correct formatting of period for data retrieval.")
    stop()
  }


  # Create sub directory for files----------------------------------------------

  if (create.dir == FALSE) {

    # print message
    cli::cli_alert_info("proceeding without creating report output subdirectory folder")

  } else if (create.dir == TRUE) {

    # check if directory exists
    if(dir.exists(mypath) == FALSE) {

      cli::cli_alert_danger(paste0("Report output will not be saved because directory does not exist:\n", mypath))
    }

    # create sub directory from ending of mypath object
    dir.create(mypath)
    # print message
    cli::cli_alert_info(paste0("sub directory for files created at:\n", mypath))

  } else {
    cli::cli_abort("'create.dir' must be of type 'logical'")
    stop()

  }


  # begin function--------------------------------------------------------------

  ## Create URL-----------------------------------------------------------------

  # for each date in the input, open a url
  for (a in date_input) {

    # for each subtype in the var.subtype, open a url
    for (b in var.subtype_internal) {

      # if var.type is daily
      if (var.type_internal == "daily") {
        # format: https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/daily/tas/CHELSA_tas_01_01_1980_V.2.1.tif

        url_obj <- file.path("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL", var.type_internal, b, paste0("CHELSA_", b, "_", a, "_V.2.1.tif"))



        # if var.type is monthly
      } else if (var.type_internal == "monthly") {
        # format 1: https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/pet/CHELSA_pet_penman_01_1980_V.2.1.tif
        # format 2: https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/vpd/CHELSA_vpd_01_1980_V.2.1.tif

        # special circumstance for subtype == pet
        if(var.subtype == "pet") {
          url_obj <- file.path("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL", var.type_internal, b, paste0("CHELSA_", b, "_penman_", a, "_V.2.1.tif"))

          # all others besides pet
        } else {
          url_obj <- file.path("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL", var.type_internal, b, paste0("CHELSA_", b, "_", a, "_V.2.1.tif"))

        }


        # if var.type is climatologies
      } else if (var.type_internal == "climatologies") {

        # if an ssp scenario is provided, go with format of projected data
        if (!is.na(ssp)) {
          # format: https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/2041-2070/GFDL-ESM4/ssp126/bio/CHELSA_bio14_2041-2070_gfdl-esm4_ssp126_V.2.1.tif

          url_obj <- file.path("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL", var.type_internal, a, toupper(model), ssp, var.type, paste0("CHELSA_", var.type, var.value, "_", a, tolower(model), "_V.2.1.tif"))

          # if an ssp scenario isnt provided, go with format of historical data
        } else {
          # format: https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_ai_1981-2010_V.2.1.tif

          url_obj <- file.path("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL", var.type_internal, a, var.type, paste0("CHELSA_", var.type, var.value, "_", a, "_V.2.1.tif"))

        }

      }


      ## Open URL and import------------------------------------------------------

      # access URL
      url_obj_download <- httr::GET(url_obj, warn = FALSE, httr::progress(type = "down"))

      # write to a temporary file
      url_obj_temp <- tempfile(fileext = ".tif")
      # write binary data to url_object_temp
      writeBin(httr::content(url_obj_download, "raw"), url_obj_temp)


      ## import to environment or write to file-----------------------------------

      # if save.output is FALSE, import to environment
      if (save.output == FALSE) {

        # load as a raster
        url_obj_raster <- terra::rast(url_obj_temp)

        names(url_obj_raster) <- paste0(b, "_", a) # rename raster layer to include variable subtype and date

        # remove URL to restart
        rm(url_obj)
        rm(url_obj_download)
        rm(url_obj_temp)
        rm(url_obj_raster)

      # if save.output is TRUE, write to file
      } else if (save.output == TRUE) {

        # load as a raster
        url_obj_raster <- terra::rast(url_obj_temp)

        names(url_obj_raster) <- paste0(as.character(b), "_", as.character(a)) # rename raster layer to include variable subtype and date

        # if mypath is not NA, write to file
        if (!is.na(mypath)) {

          # write raster to file
          terra::writeRaster(
            url_obj_raster,
            filename = file.path(mypath, paste0(a, "_", b, ".tif")),
            overwrite = FALSE
            )

          # success message
          cli::cli_alert_info(paste0("Raster saved to: ", file.path(mypath, basename(url_obj_temp))))

          # otherwise, stop and throw warning
        } else {
          cli::cli_alert_danger("'mypath' not specified, so raster will not be saved to file.")

        }

        # remove URL to restart
        rm(url_obj)
        rm(url_obj_download)
        rm(url_obj_temp)
        rm(url_obj_raster)

      }

    } # end of subtype loop

  } # end of date input

} # end of function

