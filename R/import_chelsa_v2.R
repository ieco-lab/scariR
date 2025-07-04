#' Downloads CHELSA version 2.1 High Resolution Climatologies from server
#'
#'@description
#'
#'@param var.type The type of climatological variable to be downloaded from server. One of "bio" (bioclimatic), "monthly", or "daily". Default is "bio".
#'
#'@param var.value If there are multiple versions of a variable, the number which indicates which variable to download.
#'For example, if var.type = "bio" and var.value = 12, this will download the 12th bioclimatic variable (annual precipitation).
#'
#'@param period.range The range of years for which to download the data. Choices include the following date ranges: 1981-2010,
#'
#'@param ssp If the variable is a projected variable, the Shared Socioeconomic Pathway (SSP) scenario to be downloaded. Choices include "ssp126", "ssp245", "ssp370", and "ssp585". Default is NA, which will download historical bioclim, monthly or daily data.
#'
#'@param model If the variable is a projected variable, the climate model chosen to be downloaded. Choices include "gfdl-esm4", "ukesm10-ll", "mpi-esm1-2-hr", "ipslcm6a-lr", and "mriesm2-0" (see technical specifications below for details). Default is NA, which will download historical bioclim, monthly or daily data.
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
#'@details
#'
#'Requires the following packages:
#'
#'See (CHELSA v2.1 Technical Specifications)[https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf] for more detail on specific climatologies, including their units, usage, and interpretation.
#'
#'@return
#'
#'@examples
#'
#'# ARGUMENT USAGE:
#'
#'https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/pet/CHELSA_pet_penman_01_1980_V.2.1.tif
#'https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/daily/tas/CHELSA_tas_01_01_1980_V.2.1.tif
#'https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_ai_1981-2010_V.2.1.tif
#'https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/2041-2070/GFDL-ESM4/ssp126/bio/CHELSA_bio14_2041-2070_gfdl-esm4_ssp126_V.2.1.tif
#'
#'@export
import_chelsa_v2 <- function(var.type = "bio", var.value = NA, period.range, ssp = NA, model = NA, mypath = NA, create.dir = FALSE) {

  # Error checks----------------------------------------------------------------

  # type numeric
  if (is.numeric(var.value) == FALSE) {
    cli::cli_alert_info("Parameter 'var.value' must be of type numeric")
    stop()
  }

  # type character
  if (!is.na(ssp)) {
    cli::cli_alert_info("Parameter 'locality.iso' must be of type character")
    stop()

  }



  ## Create sub directory for files---------------------------------------------

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

  # Data and argument import----------------------------------------------------

  # var.type
  ## if bioclimatic, add internal placeholder for naming
  if (var.type == "bio") {

    var.type_internal <- "climatologies"

    # else, internal placeholder is same as input
  } else {

    var.type_internal <- var.type

  }

  # period.range
  ## correct

  period.range = "2041-2070"
  var.value = 14
  ssp = "ssp126"
  model = "gfdl-esm4"


  # begin function--------------------------------------------------------------
  ## Create URL-----------------------------------------------------------------



  # if an ssp scenario is provided, go with format of projected data
  if (!is.na(ssp)) {

    file.path("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL", var.type_internal, period.range, toupper(model), ssp, var.type, paste0("CHELSA_", var.type, var.value, "_", year.range, tolower(model), "_V.2.1.tif"))


  # if an ssp scenario isnt provided, go with format of historical data
  } else {


    file.path("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL", var.type_internal, period.range, var.type, paste0("CHELSA_", var.type, var.value, "_", year.range, "_V.2.1.tif"))

  }

}

