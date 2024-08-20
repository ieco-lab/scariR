#'Creates
#'
#'@description blarg
#'
#'@param locality.iso The [alpha-3 ISO code](https://www.iso.org/obp/ui/#search) corresponding to the country of interest.
#'If the desired locality is a state or province, please still enter the ISO code
#'and supply the name of that province in `locality.name`
#'
#'@param locality.name The name of the country, state or province for which to
#'generate the report. This is optional if the report is for a country, but
#'required if the report is for a state or province. Avoid special characters,
#'but please include those used used in the ethnic naming (ex: Côte d'Ivoire).
#'
#'@param locality.type One of "country" or "states_provinces". If you do
#'not know the state or province you are looking for, you might create a report
#'at the country level and then look at the return for the name of the provinces
#'/ states included.
#'
#'@param save.report Logical. Should the report be saved to file? File location
#'specified by `mypath`.
#'
#'@param mypath Character. A file path to the sub directory where the model
#'output will be stored. Should be used with the [file.path()] function
#'(i.e. with '/' instead of '\\'). If this sub directory does not already exist
#'and should be created by the function, set `create.dir` = TRUE. This will
#'create a folder from the last part of the filepath in `mypath`.
#'
#'@param raster.path Character. A file path to the directory containing the rasters
#'necessary to build this function. See details for the rasters that should be
#'included with this data input and for the default path.
#'
#'@param create.dir Logical. Should the last element of `mypath` create a sub
#'directory for the report? If TRUE, the main folder will be created for
#'the model output. If FALSE (ie, the sub directory already exists), no directory
#'will be created.
#'
#'@param map.style List, default is NA. This is used to apply ggplot aesthetics
#'to the mapped outputs. If specified, the given value should be a list of
#'ggplot aesthetic options. If not, the built-in default list will be used
#'(see details). See examples for usage.
#'
#'@param buffer.dist Numeric. The distance (in meters) from each IVR coordinate
#'at which to draw buffers zones on risk maps. Should be the same distance that
#'was used to calculate the predicted suitability for each IVR region.
#'See `predict.xy.suitability.R` for details. If not specified, buffers are not
#'drawn and suitabiltiy prediction method is assumed to be simple (point-wise).
#'
#'@details
#'
#'Requires the following packages: 'tidyverse', 'terra', 'here', 'cli', 'rnaturalearth', 'rnaturalearthhires', 'kableExtra', 'formattable', 'webshot2', 'ggnewscale', 'common'.
#'
#'Note that this function performs downloads from [naturalearthdata.com](https://www.naturalearthdata.com/).
#'The function will automatically create subfolders in `root/data-raw`
#'containing the shapefiles.
#'
#'This function depends on certain files that have been distributed with this
#'package, which will be imported from `root/R/sysdata.rda` when the function
#'is run. The code to create `sysdata.rda` can be found in
#'`root/vignettes/160_generate_risk_report.qmd`.
#'
#'Here is a list of the files included in `sysdata.rda`:
#'
#'* global_model_summary.rds                                            | created in vignette 050
#'* ensemble_thresh_values.rds                                          | created in vignette 110
#'
#'* wineries_tidied.rds                                                 | created in vignette 130
#'* regional_ensemble_wineries_1981-2010_xy_pred_suit.rds               | created in vignette 130
#'* regional_ensemble_wineries_2041-2070_GFDL_ssp_mean_xy_pred_suit.rds | created in vignette 130
#'* global_wineries_1981-2010_xy_pred_suit.rds                          | created in vignette 130
#'* global_wineries_2041-2070_GFDL_ssp_mean_xy_pred_suit.rds            | created in vignette 130
#'
#'Additionally, 3 rasters are used to create maps. These rasters are located in
#'`root/vignette-outputs/rasters`, These are the files:
#'
#'* slf_binarized_summed_1981-2010.asc                                  | created in vignette 120
#'* slf_binarized_summed_2041-2070_ssp_mean_GFDL.asc                    | created in vignette 120
#'* slf_range_shift_summed.asc                                          | created in vignette 120
#'
#'## raster.path
#'
#'Here is a list of the rasters that should be included in this data import.
#'These rasters are located in `root/vignette-outputs/rasters`, These are the files:
#'
#'* slf_binarized_summed_1981-2010.asc                                  | created in vignette 120
#'* slf_binarized_summed_2041-2070_ssp_mean_GFDL.asc                    | created in vignette 120
#'* slf_range_shift_summed_ssp_mean_GFDL.asc                            | created in vignette 120
#'
#'
#'@return
#'
#'returns a report in list format. If save.report = TRUE, will also save the
#'report to file at destination specified by `mypath`. The outputs in the returned list
#'object include:
#'
#'* viticultural_regions_list - a list of known important wine regions within the locality
#'* risk_maps - a current and future map of risk for L delicatula establishment based on 4 climate models
#'* viticultural_risk_plot = a quantified assessment of the risk for L delicatula establishment for known wine regions within the locality.
#'* viticultural_risk_table = a table quantifying the risk plot for viticultural areas
#'* range_shift_map = a map of potential range expansion for L delicatula under climate change
#'
#'Some maps may be formatted strangely because of a country's outlying territories.
#'You may need to further crop the plot using xlim and ylim (see examples).
#'
#'Use caution, this function will overwrite previous files output for the same locality.
#'
#'@examples
#'
#'The output is in list format, so it should be called using this notation:
#'
#'# find viticultural regions in locality
#'viticultural_regions <- slf_risk_report[[2]]
#'
#'# alternatively, call elements by name:
#'risk_map_2055 <- slf_risk_report[["risk_maps"]][["2055_risk_map"]]
#'
#'# sometimes a plot is off-center because the shapefile includes an outlying territory
#'# you can edit this directly in the ggplot object and save over the report output
#'
#'map_current <- slf_risk_report[["risk_maps"]][["current_risk_map"]] +
#'xlim(-10, 5) +
#'ylim(35, 44)
#'
#'If you need to save the report Rdata object and use those figures elsewhere:
#'
#'readr::write_rds(slf_risk_report, file = file.path(here::here(), "vignette-outputs", "reports", "slf_risk_report.rds"))
#'
#'@export
create_risk_report <- function(locality.iso, locality.name = locality.iso, locality.type, save.report = FALSE, mypath, raster.path = file.path(here::here(), "vignette-outputs", "rasters"), create.dir = FALSE, map.style = NA, buffer.dist = NA) {

  # Error checks----------------------------------------------------------------

  if (is.character(locality.iso) == FALSE) {
    cli::cli_alert_info("Parameter 'locality.iso' must be of type character")
    stop()

  }

  if (is.character(locality.name) == FALSE) {
    cli::cli_abort("Parameter 'locality.name' must be of type character")
    stop()
  }


  if (is.character(locality.type) == FALSE) {
    cli::cli_abort("Parameter 'locality.type' must be of type character")
    stop()
  }

  if (is.character(mypath) == FALSE) {
    cli::cli_abort("Parameter 'mypath' must be of type character")
    stop()
  }

  if (is.character(raster.path) == FALSE) {
    cli::cli_abort("Parameter 'raster.path' must be of type character")
    stop()
  }


  # other errors
  if (is.logical(save.report) == FALSE) {
    cli::cli_abort("Parameter 'save.report' must be of type logical")
    stop()
  }

  if (stringr::str_length(locality.iso) != 3) {
    cli::cli_abort("Parameter 'locality.iso' must consist of the alpha-3 country code.")
    stop()
  }



  ## Create sub directory for files---------------------------------------------

  if (create.dir == FALSE) {
    # print message
    cli::cli_alert_info("proceeding without creating report output subdirectory folder")

  } else if (create.dir == TRUE) {
    # create sub directory from ending of mypath object
    dir.create(mypath)
    # print message
    cli::cli_alert_info(paste0("sub directory for files created at:\n", mypath))

  } else {
    cli::cli_abort("'create.dir' must be of type 'logical'")
    stop()

  }



  # Data and argument import----------------------------------------------------

  # dataset generated in vignette 160
  load(file.path(here::here(), "R", "sysdata.rda"))

  # import rasters
  slf_binarized_1995 <- terra::rast(file.path(raster.path, "slf_binarized_summed_1981-2010.asc"))
  slf_binarized_2055 <- terra::rast(file.path(raster.path, "slf_binarized_summed_2041-2070_ssp_mean_GFDL.asc"))
  slf_range_shift <- terra::rast(file.path(raster.path, "slf_range_shift_summed_ssp_mean_GFDL.asc"))

  # map.style
  # if it is not changed from NA, import default style
  if (is.na(map.style)) {

    map_style <- list(
      xlab("longitude"),
      ylab("latitude"),
      # aesthetics
      theme_classic(),
      theme(
        # legend
        legend.position = "bottom",
        legend.key = element_rect(color = "black")
      ),
      guides(fill = guide_legend(nrow = 1, byrow = TRUE)),
      # scales
      scale_x_continuous(expand = c(0, 0)),
      scale_y_continuous(expand = c(0, 0))
    )

    # if it is changed to a list, import the given list
  } else if (is.list(map_style) == TRUE) {
    map_style <- map.style

    # otherwise, warn that given values must be a list
  } else {
    cli::cli_abort("parameter 'map.style' must be of type 'list'")
    stop()

  }

  # locality.iso
  locality_iso_internal <- locality.iso %>%
    toupper() %>%
    gsub(pattern = " ", replacement = "")

  # locality.name
  locality_name_internal <- locality.name %>%
    tolower() %>%
    gsub(pattern = " ", replacement = "_") %>%
    gsub(pattern = "-", replacement = "_") %>%
    gsub(pattern = ".", replacement = "", fixed = TRUE)



  # plotting objects and styles-------------------------------------------------

  # scatter plots
  # axis breaks
  breaks <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
  # axis labels
  labels <- c(0, 2, 4, 6, 8, 10)

  risk_levels <- c("extreme", "high", "moderate", "low")

  # import shapefiles (locality)------------------------------------------------

  # update packages
  rnaturalearth::check_rnaturalearthdata()
  rnaturalearth::check_rnaturalearthhires()

  # import countries, first checking if the file already exists
  if(file.exists(file.path(here::here(), "data-raw", "ne_countries", "ne_10m_admin_0_countries.shp")) == TRUE) {

    countries_sf <- rnaturalearth::ne_load(
      scale = 10,
      type = "admin_0_countries",
      category = "cultural",
      destdir = file.path(here::here(), "data-raw", "ne_countries"),
      returnclass = "sf"
    )

    cli::cli_alert_info(paste0("Importing shapefiles from: ", file.path(here::here(), "data-raw")))

    # if it doesnt exist, create directories and download it
  } else if (file.exists(file.path(here::here(), "data-raw", "ne_countries", "ne_10m_admin_0_countries.shp")) == FALSE) {

    # create directories for shapefiles
    dir.create(here::here(), "data-raw", "ne_countries")

    # retrieve data
    countries_sf <- rnaturalearth::ne_download(
      scale = 10, # highest resolution
      type = "admin_0_countries", # countries
      category = "cultural",
      destdir = file.path(here::here(), "data-raw", "ne_countries"),
      load = TRUE, # load into environment
      returnclass = "sf" # shapefile
    )

  }



  # import states and provinces
  if(file.exists(file.path(here::here(), "data-raw", "ne_states_provinces", "ne_10m_admin_1_states_provinces.shp")) == TRUE) {

    states_provinces_sf <- rnaturalearth::ne_load(
      scale = 10,
      type = "admin_1_states_provinces",
      category = "cultural",
      destdir = file.path(here::here(), "data-raw", "ne_states_provinces"),
      returnclass = "sf"
    )

    # if it doesnt exist, download it
  } else if (file.exists(file.path(here::here(), "data-raw", "ne_states_provinces", "ne_10m_admin_1_states_provinces.shp")) == FALSE) {

    # create directory for file
    dir.create(here::here(), "data-raw", "ne_states_provinces")

    # download
    states_provinces_sf <- rnaturalearth::ne_download(
      scale = 10, # highest resolution
      type = "admin_1_states_provinces", # states and provinces
      category = "cultural",
      destdir = file.path(here::here(), "data-raw", "ne_states_provinces"),
      load = TRUE, # load into environment
      returnclass = "sf" # shapefile
    )

  }

  # tidy shapefiles-------------------------------------------------------------

  # harmonize naming
  countries_sf <- countries_sf %>%
    dplyr::mutate(
      NAME_LONG = tolower(NAME_LONG),
      NAME_LONG = gsub(NAME_LONG, pattern = " ", replacement = "_"),
      NAME_LONG = gsub(NAME_LONG, pattern = "/", replacement = ""),
      NAME_LONG = gsub(NAME_LONG, pattern = "-", replacement = "_"),
      NAME_LONG = gsub(NAME_LONG, pattern = ".", replacement = "", fixed = TRUE),
      # alpha-3 iso used to isolate sf
      ADM0_A3 = toupper(ADM0_A3),
      ADM0_A3 = gsub(ADM0_A3, pattern = " ", replacement = ""),
      # also tidy abbreviations
      ABBREV = toupper(ABBREV),
      ABBREV = gsub(ABBREV, pattern = "/", replacement = ""),
      ABBREV = gsub(ABBREV, pattern = "-", replacement = "_"),
      ABBREV = gsub(ABBREV, pattern = ".", replacement = "", fixed = TRUE)
    )

  states_provinces_sf <- states_provinces_sf %>%
    dplyr::mutate(
      # names of states
      name = tolower(name),
      name = gsub(name, pattern = " ", replacement = "_"),
      name = gsub(name, pattern = "/", replacement = ""),
      name = gsub(name, pattern = "-", replacement = "_"),
      name = gsub(name, pattern = ".", replacement = "", fixed = TRUE),
      # alpha-3 iso used to isolate sf
      adm0_a3 = toupper(adm0_a3),
      adm0_a3 = gsub(adm0_a3, pattern = " ", replacement = "")
    )


  # tidy IVR dataset------------------------------------------------------------

  IVR_locations <- IVR_locations %>%
    dplyr::mutate(
      # country
      Country = tolower(Country),
      Country = gsub(Country, pattern = " ", replacement = "_"),
      Country = gsub(Country, pattern = "/", replacement = ""),
      Country = gsub(Country, pattern = "-", replacement = "_"),
      Country = gsub(Country, pattern = ".", replacement = "", fixed = TRUE),
      # subregion
      Region = tolower(Region),
      Region = gsub(Region, pattern = " ", replacement = "_"),
      Region = gsub(Region, pattern = "/", replacement = ""),
      Region = gsub(Region, pattern = "-", replacement = "_"),
      Region = gsub(Region, pattern = ".", replacement = "", fixed = TRUE),
      Region = gsub(Region, pattern = "–", replacement = "", fixed = TRUE)
    )

  # convert to df
  # DEPRECATED
  #IVR_locations <- as.data.frame(IVR_locations)


  # check for existence of locality name in shapefiles--------------------------

  # data existence check
  # the check if the locality is a country
  if(locality.type == "country") {

    country.name.check <- countries_sf %>%
      dplyr::filter(ADM0_A3 == locality_iso_internal)

      # if at least 1 record, success
    if(nrow(country.name.check) > 0) {
      cli::cli_alert_success("Data exist for locality")

      # if no records, warn
    } else if(nrow(country.name.check) == 0) {
      cli::cli_abort("Data do not exist for locality. Check spelling or try another locality.")
      stop()

    }

    # the check if the locality is a state or province
  } else if(locality.type == "states_provinces") {
    state.name.check <- states_provinces_sf %>%
      dplyr::filter(name == locality_name_internal)

    # if at least 1 record, success
    if(nrow(state.name.check) > 0) {
      cli::cli_alert_success("Data exist for locality")

      # if no records, warn
    } else if(nrow(state.name.check) == 0) {
      cli::cli_abort("Data do not exist for locality. Check spelling or try another locality.")
      stop()

    }

  } else {
    cli::cli_abort("'locality.type' must be one of: 'country' | 'states_provinces'")
    stop()
  }




  # isolate data for that locality into locality_sf-----------------------------

  # if a country, import country sf
  if(locality.type == "country") {
    locality_sf <- countries_sf %>%
      dplyr::filter(ADM0_A3 == locality_iso_internal, na.rm = TRUE)

    # if the locality is a country, I will also map the provinces on top
    locality_sf_plot_layer <- states_provinces_sf %>%
      dplyr::filter(adm0_a3 == locality_iso_internal, na.rm = TRUE)

    # if a state, import state sf
  } else if(locality.type == "states_provinces") {
    locality_sf <- states_provinces_sf %>%
      dplyr::filter(name == locality_name_internal, na.rm = TRUE)

  }


  # begin function--------------------------------------------------------------

  ## isolate IVRs for locality

  # create spatvector of locality_sf (this will be used to mask the maps as well)
  locality_sv <- terra::vect(locality_sf)

  # convert to vector
  IVR_locations_masked <- terra::vect(x = IVR_locations, geom = c("x", "y"), crs = "EPSG:4326") %>%
    # crop by extent area of interest
    terra::mask(., mask = locality_sv) %>%
    # convert to geom, which gets coordinates of a spatVector
    terra::geom()

  # convert back to data frame
  IVR_locations_plot_layer <- terra::as.data.frame(IVR_locations_masked) %>%
    dplyr::select(-c(geom, part, hole))


  # will not need this object again
  rm(IVR_locations_masked)


  ## create polygon of buffer zones around IVRs---------------------------------

  # if no buffer zones, just use point-wise predictions
  if (is.na(buffer.dist)) {
    # alert
    cli::cli_alert_info("Suitability prediction type for IVR regions: simple")



    # if buffer zones, use buffer style predictions
  } else if (!is.na(buffer.dist) & is.numeric(buffer.dist)) {
    # alert
    cli::cli_alert_info(paste0("Suitability prediction type for IVR regions: buffer of ", buffer.dist, "m around points"))

    # convert IVR points to spatvector
    IVR_locations_sf <- sf::st_as_sf(
      x = IVR_locations_plot_layer,
      crs = "EPSG:4326",
      coords = c("x", "y")
    )
    # use sv to create buffers
    IVR_buffers_plot_layer <- sf::st_buffer(
      x = IVR_locations_sf,
      dist = buffer.dist
    )



    # otherwise, stop
  } else {

    cli::cli_abort("Parameter 'buffer.dist' must be of type numeric")
    stop()

  }


  ## plot binarized rasters-----------------------------------------------------

  # first, I will mask these rasters using the locality_sf
  # use the version locality_sv instead for masking

  slf_binarized_1995 <- terra::mask(
    x = slf_binarized_1995,
    mask = locality_sv
  )

  slf_binarized_2055 <- terra::mask(
    x = slf_binarized_2055,
    mask = locality_sv
  )



  # rename values
  names(slf_binarized_1995) <- "global_regional_binarized"
  names(slf_binarized_2055) <- "global_regional_binarized"

  # convert to df for plotting
  slf_binarized_1995_df <- terra::as.data.frame(slf_binarized_1995, xy = TRUE)
  slf_binarized_2055_df <- terra::as.data.frame(slf_binarized_2055, xy = TRUE)


  # binarized maps
  # the possible values of the scale and their order
  breaks.obj <- c(5, 9, 6, 10) # I manually ordered these
  # labels for the values
  labels.obj <- c("low", "moderate", "high", "extreme")
  # vector of colors to classify scale
  values.obj <- c("azure4", "gold", "darkorange", "darkred")


  ### current plot

  # plot with state_province layer if plotting at country level
  if(locality.type == "country") {

  slf_binarized_1995_plot <- ggplot() +
    map_style +
    # data layer
    geom_raster(data = slf_binarized_1995_df, aes(x = x, y = y, fill = as.factor(global_regional_binarized))) +
    # add province layer
    geom_sf(data = locality_sf_plot_layer, aes(geometry = geometry), fill = NA, color = "black", linewidth = 0.3) +
    # fill scale 1
    scale_discrete_manual(
      name = "projected risk",
      values = values.obj,
      breaks = breaks.obj,
      labels = labels.obj,
      aesthetics = "fill",
      guide = guide_colorsteps(frame.colour = "black", ticks.colour = "black", barwidth = 20, draw.ulim = TRUE, draw.llim = TRUE)
    ) +
    # new scale
    ggnewscale::new_scale_fill()

  # whether or not to plot buffer layer

  if (!is.na(buffer.dist)) {

    slf_binarized_1995_plot <- slf_binarized_1995_plot +
      # underlying buffer layer
      geom_sf(data = IVR_buffers_plot_layer, aes(geometry = geometry, fill = "viticultural\narea"), color = "black", alpha = 0.35)

    # otherwise, plot without buffers
  } else if (is.na(buffer.dist)) {
    slf_binarized_1995_plot <- slf_binarized_1995_plot

  }

  slf_binarized_1995_plot <- slf_binarized_1995_plot +
    # points
    geom_point(data = IVR_locations_plot_layer, aes(x = x, y = y, fill = "viticultural\narea"), size = 2.5, shape = 21) +
    # fill scale for points
    scale_fill_manual(name = "", values = c("viticultural\narea" = "orchid1")) +
    # aesthetics
    guides(fill = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(size = 3))) +
    # other stuff
    labs(
      title = "Current projected risk of Lycorma delicatula establishment",
      subtitle = stringr::str_to_title(locality_name_internal),
      caption = ifelse(!is.na(buffer.dist), paste0(buffer.dist, "m buffer used for suitability of viticultural areas"), "")
    ) +
    coord_sf()




  # otherwise, plot without a state_province layer
  } else if(locality.type == "states_provinces") {

    slf_binarized_1995_plot <- ggplot() +
      map_style +
      # data layer
      geom_raster(data = slf_binarized_1995_df, aes(x = x, y = y, fill = as.factor(global_regional_binarized))) +
      # add province layer
      geom_sf(data = locality_sf, aes(geometry = geometry), fill = NA, color = "black", linewidth = 0.3) +
      # fill scale raster
      scale_discrete_manual(
        name = "projected risk",
        values = values.obj,
        breaks = breaks.obj,
        labels = labels.obj,
        aesthetics = "fill",
        guide = guide_colorsteps(frame.colour = "black", ticks.colour = "black", barwidth = 20, draw.ulim = TRUE, draw.llim = TRUE)
      ) +
      # new scale
      ggnewscale::new_scale_fill()

    # whether or not to plot buffer layer
    if (!is.na(buffer.dist)) {

      slf_binarized_1995_plot <- slf_binarized_1995_plot +
        # underlying buffer layer
        geom_sf(data = IVR_buffers_plot_layer, aes(geometry = geometry, fill = "viticultural\narea"), color = "black", alpha = 0.35)

      # dont plot
    } else if (is.na(buffer.dist)) {
      slf_binarized_1995_plot <- slf_binarized_1995_plot

    }

    slf_binarized_1995_plot <- slf_binarized_1995_plot +
      # IVRs
      geom_point(data = IVR_locations_plot_layer, aes(x = x, y = y, fill = "viticultural\narea"), size = 2.5, shape = 21) +
      # fill scale for points
      scale_fill_manual(name = "", values = c("viticultural\narea" = "orchid1")) +
      # aesthetics
      guides(fill = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(size = 3))) +
      # other stuff
      labs(
        title = "Current projected risk of Lycorma delicatula establishment",
        subtitle = stringr::str_to_title(locality_name_internal),
        caption = ifelse(!is.na(buffer.dist), paste0(buffer.dist, "m buffer used for suitability of viticultural areas"), "")
      ) +
      coord_sf()

  }




  ### CMIP6 mean projection

  # plot with state_province layer if plotting at country level
  if(locality.type == "country") {

    slf_binarized_2055_plot <- ggplot() +
      map_style +
      # data layer
      geom_raster(data = slf_binarized_2055_df, aes(x = x, y = y, fill = as.factor(global_regional_binarized))) +
      # add province layer
      geom_sf(data = locality_sf_plot_layer, aes(geometry = geometry), fill = NA, color = "black", linewidth = 0.3) +
      # fill scale raster
      scale_discrete_manual(
        name = "projected risk",
        values = values.obj,
        breaks = breaks.obj,
        labels = labels.obj,
        aesthetics = "fill",
        guide = guide_colorsteps(frame.colour = "black", ticks.colour = "black", barwidth = 20, draw.ulim = TRUE, draw.llim = TRUE)
      ) +
      # new scale
      ggnewscale::new_scale_fill()

    # whether or not to plot buffer layer
    if (!is.na(buffer.dist)) {

      slf_binarized_2055_plot <- slf_binarized_2055_plot +
        # underlying buffer layer
        geom_sf(data = IVR_buffers_plot_layer, aes(geometry = geometry, fill = "viticultural\narea"), color = "black", alpha = 0.35)

    } else if (is.na(buffer.dist)) {
      slf_binarized_2055_plot <- slf_binarized_2055_plot

    }

    slf_binarized_2055_plot <- slf_binarized_2055_plot +
      # IVRs
      geom_point(data = IVR_locations_plot_layer, aes(x = x, y = y, fill = "viticultural\narea"), size = 2.5, shape = 21) +
      # fill scale for points
      scale_fill_manual(name = "", values = c("viticultural\narea" = "purple3")) +
      # aesthetics
      guides(fill = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(size = 3))) +
      # other stuff
      labs(
        title = "Projected risk of Lycorma delicatula establishment under climate change",
        subtitle = paste(stringr::str_to_title(locality_name_internal), "| 2041-2070 | mean of ssp126/370/585 | GFDL-ESM4"),
        caption = ifelse(!is.na(buffer.dist), paste0(buffer.dist, "m buffer used for suitability of viticultural areas"), "")
      ) +
      coord_sf()




    # otherwise, plot without a state_province layer
  } else if(locality.type == "states_provinces") {

    slf_binarized_2055_plot <- ggplot() +
      map_style +
      # data layer
      geom_raster(data = slf_binarized_2055_df, aes(x = x, y = y, fill = as.factor(global_regional_binarized))) +
      # add province layer
      geom_sf(data = locality_sf, aes(geometry = geometry), fill = NA, color = "black", linewidth = 0.3) +
      # fill scale raster
      scale_discrete_manual(
        name = "projected risk",
        values = values.obj,
        breaks = breaks.obj,
        labels = labels.obj,
        aesthetics = "fill",
        guide = guide_colorsteps(frame.colour = "black", ticks.colour = "black", barwidth = 20, draw.ulim = TRUE, draw.llim = TRUE)
      ) +
      # new scale
      ggnewscale::new_scale_fill()


    # whether or not to plot buffer layer
    if (!is.na(buffer.dist)) {

      slf_binarized_2055_plot <- slf_binarized_2055_plot +
        # underlying buffer layer
        geom_sf(data = IVR_buffers_plot_layer, aes(geometry = geometry, fill = "viticultural\narea"), color = "black", alpha = 0.35)

    } else if (is.na(buffer.dist)) {
      slf_binarized_2055_plot <- slf_binarized_2055_plot

    }


    slf_binarized_2055_plot <- slf_binarized_2055_plot +
      # IVRs
      geom_point(data = IVR_locations_plot_layer, aes(x = x, y = y, fill = "viticultural\narea"), size = 2.5, shape = 21) +
      # fill scale for points
      scale_fill_manual(name = "", values = c("viticultural\narea" = "purple3")) +
      # aesthetics
      guides(fill = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(size = 3))) +
      # other stuff
      labs(
        title = "Projected risk of Lycorma delicatula establishment under climate change",
        subtitle = paste(stringr::str_to_title(locality_name_internal), "| 2041-2070 | mean of ssp126/370/585 | GFDL-ESM4"),
        caption = ifelse(!is.na(buffer.dist), paste0(buffer.dist, "m buffer used for suitability of viticultural areas"), "")
      ) +
      coord_sf()

  }



  # success message
  cli::cli_alert_success("Risk maps plotted")

  ## plot lead/trailing edge map------------------------------------------------

  # first, mask rasters
    slf_range_shift <- terra::mask(
      x = slf_range_shift,
      mask = locality_sv
    )



  # rename values
  names(slf_range_shift) <- "range_shift_summed"
  # convert to df
  slf_range_shift_df <- terra::as.data.frame(slf_range_shift, xy = TRUE)


  # now, create vectors of values used to manually edit the scale of the plot
  # the possible values of the scale and their order
  breaks.obj2 <- c(5, 10, 6, 9) # i manually ordered these
  # labels for the values
  labels.obj2 <- c("unsuitable area\nretained", "suitabile area\nretained", "contraction\nof suitable area", "expansion\nof suitable area")
  # vector of colors to classify scale
  values.obj2 <- c("azure4", "azure", "darkred", "darkgreen")



  # if the locality is a country
  if(locality.type == "country") {

  slf_range_shift_plot <- ggplot() +
    map_style +
    # data layer
    geom_raster(data = slf_range_shift_df, aes(x = x, y = y, fill = as.factor(range_shift_summed))) +
    # add province layer
    geom_sf(data = locality_sf_plot_layer, aes(geometry = geometry), fill = NA, color = "black", linewidth = 0.3) +
    # fill scale
    scale_discrete_manual(
      name = "suitability for\nL delicatula",
      values = values.obj2,
      breaks = breaks.obj2,
      labels = labels.obj2,
      aesthetics = "fill",
      guide = guide_colorsteps(frame.colour = "black", ticks.colour = "black", barwidth = 20, draw.ulim = TRUE, draw.llim = TRUE)
    ) +
    # new fill scale
    ggnewscale::new_scale_fill() +
    # IVR regions
    geom_point(data = IVR_locations_plot_layer, aes(x = x, y = y, fill = "viticultural\narea"), size = 2.5, shape = 21) +
    # fill scale for points
    scale_fill_manual(name = "", values = c("viticultural\narea" = "purple3")) +
    # aesthetics
    guides(fill = guide_legend(ncol = 2, byrow = TRUE, override.aes = list(size = 3))) +
    # other stuff
    labs(
      title = "Projected areas suitable for Lycorma delicatula range expansion",
      subtitle = paste0(stringr::str_to_title(locality_name_internal), " | 2041-2070")
    ) +
    theme(legend.title = element_text(hjust = 1)) +
    coord_sf()


  # otherwise, plot without a state_province layer
  } else if(locality.type == "states_provinces") {

    slf_range_shift_plot <- ggplot() +
      map_style +
      # data layer
      geom_raster(data = slf_range_shift_df, aes(x = x, y = y, fill = as.factor(range_shift_summed))) +
      # add province layer
      geom_sf(data = locality_sf, aes(geometry = geometry), fill = NA, color = "black", linewidth = 0.3) +
      # fill scale
      scale_discrete_manual(
        name = stringr::str_wrap("suitability for L delicatula"),
        values = values.obj2,
        breaks = breaks.obj2,
        labels = labels.obj2,
        aesthetics = "fill",
        guide = guide_colorsteps(frame.colour = "black", ticks.colour = "black", barwidth = 20, draw.ulim = TRUE, draw.llim = TRUE)
      ) +
      # new fill scale
      ggnewscale::new_scale_fill() +
      # IVR regions
      geom_point(data = IVR_locations_plot_layer, aes(x = x, y = y, fill = "viticultural\narea"), size = 2.5, shape = 21) +
      # fill scale for points
      scale_fill_manual(name = "", values = c("viticultural\narea" = "purple3")) +
      # aesthetics
      guides(fill = guide_legend(ncol = 2, byrow = TRUE, override.aes = list(size = 3))) +
      # other stuff
      labs(
        title = "Projected areas suitable for Lycorma delicatula range expansion",
        subtitle = paste0(stringr::str_to_title(locality_name_internal), " | 2041-2070")
      ) +
      theme(legend.title = element_text(hjust = 1)) +
      coord_sf()

  }

  # success message
  cli::cli_alert_success("Range shift map plotted")




  ## return IVR_locations selected for locality---------------------------------

  # filter out locations that match plot layer
  IVR_locations_locality <-  dplyr::semi_join(IVR_locations, IVR_locations_plot_layer, by = c("x", "y"))



  ## plot scatter plot of IVRs--------------------------------------------------

  ### tidy data-----------------------------------------------------------------

  # apply internal function rescale_cloglog_suitability
  xy_global_1995_rescaled <- slfSpread::rescale_cloglog_suitability(
    xy.predicted = xy_global_1995,
    thresh = "MTSS",
    exponential.file = threshold_exponential_values,
    summary.file = summary_global,
    rescale.name = "xy_global_1995",
    rescale.thresholds = TRUE
  )
  # separate data from thresholds
  xy_global_1995_rescaled_thresholds <- xy_global_1995_rescaled[[2]]
  xy_global_1995_rescaled <- xy_global_1995_rescaled[[1]]


  xy_global_2055_rescaled <- slfSpread::rescale_cloglog_suitability(
    xy.predicted = xy_global_2055,
    thresh = "MTSS",  # the global model only has 1 MTSS thresh
    exponential.file = threshold_exponential_values,
    summary.file = summary_global,
    rescale.name = "xy_global_2055",
    rescale.thresholds = TRUE
  )

  xy_global_2055_rescaled_thresholds <- xy_global_2055_rescaled[[2]]
  xy_global_2055_rescaled <- xy_global_2055_rescaled[[1]]



  xy_regional_ensemble_1995_rescaled <- slfSpread::rescale_cloglog_suitability(
    xy.predicted = xy_regional_ensemble_1995,
    thresh = "MTSS",
    exponential.file = threshold_exponential_values,
    summary.file = summary_regional_ensemble,
    rescale.name = "xy_regional_ensemble_1995",
    rescale.thresholds = TRUE
  )

  xy_regional_ensemble_1995_rescaled_thresholds <- xy_regional_ensemble_1995_rescaled[[2]]
  xy_regional_ensemble_1995_rescaled <- xy_regional_ensemble_1995_rescaled[[1]]



  xy_regional_ensemble_2055_rescaled <- slfSpread::rescale_cloglog_suitability(
    xy.predicted = xy_regional_ensemble_2055,
    thresh = "MTSS.CC", # the way the thresholds are calculated for the regional_ensemble model means that the threshold will be slightly different for climate change
    exponential.file = threshold_exponential_values,
    summary.file = summary_regional_ensemble,
    rescale.name = "xy_regional_ensemble_2055",
    rescale.thresholds = TRUE
  )

  xy_regional_ensemble_2055_rescaled_thresholds <- xy_regional_ensemble_2055_rescaled[[2]]
  xy_regional_ensemble_2055_rescaled <- xy_regional_ensemble_2055_rescaled[[1]]


  ### join datasets--------------------------------------------------------------

  # join datasets for plotting
  xy_joined_rescaled <-  dplyr::full_join(xy_global_1995_rescaled, xy_regional_ensemble_1995_rescaled, by = c("ID", "x", "y")) %>%
    # join CC datasets
    dplyr::full_join(., xy_global_2055_rescaled, by = c("ID", "x", "y")) %>%
    dplyr::full_join(., xy_regional_ensemble_2055_rescaled, by = c("ID", "x", "y")) %>%
    # order
    dplyr::relocate(ID, x, y, xy_global_1995_rescaled, xy_global_2055_rescaled) %>%
    dplyr::select(-c(xy_global_1995, xy_global_2055, xy_regional_ensemble_1995, xy_regional_ensemble_2055))


  # filter out only records from locality
  xy_joined_rescaled <-  dplyr::semi_join(xy_joined_rescaled, IVR_locations_locality, by = c("ID", "x", "y"))





  ### isolate thresholds---------------------------------------------------------

  # global
  global_MTSS <- as.numeric(xy_global_1995_rescaled_thresholds[2, 2])
  # regional ensemble
  regional_ensemble_MTSS_1995 <- as.numeric(xy_regional_ensemble_1995_rescaled_thresholds[2, 2])
  regional_ensemble_MTSS_2055 <- as.numeric(xy_regional_ensemble_2055_rescaled_thresholds[4, 2])






  ### find points that cross threshold------------------------------------------

  xy_joined_rescaled_intersects <- xy_joined_rescaled %>%
    dplyr::mutate(
      crosses_threshold =  dplyr::case_when(
        # conditional for starting and ending points that overlap a the threshold
        # x-axis
        xy_global_1995_rescaled > global_MTSS & xy_global_2055_rescaled < global_MTSS ~ "crosses",
        xy_global_1995_rescaled < global_MTSS & xy_global_2055_rescaled > global_MTSS ~ "crosses",
        # y-axis
        xy_regional_ensemble_1995_rescaled > regional_ensemble_MTSS_2055 & xy_regional_ensemble_2055_rescaled < regional_ensemble_MTSS_2055 ~ "crosses",
        xy_regional_ensemble_1995_rescaled < regional_ensemble_MTSS_2055 & xy_regional_ensemble_2055_rescaled > regional_ensemble_MTSS_2055 ~ "crosses",
        # else
        .default = "does not cross"
      )
    )

  # filter out the crosses
  xy_joined_rescaled_intersects <- dplyr::filter(
    xy_joined_rescaled_intersects,
    crosses_threshold == "crosses"
  )




  ### plot data-----------------------------------------------------------------


  # figure annotation title
  # "Risk of Lycorma delicatula establishment in globally important viticultural areas, projected for climate change"

  # plot
  xy_joined_rescaled_plot <- ggplot(data = xy_joined_rescaled) +
    # threshold lines
     # MTSS thresholds
     geom_vline(xintercept = global_MTSS, linetype = "dashed", linewidth = 0.7) + # global
     geom_hline(yintercept = regional_ensemble_MTSS_1995, linetype = "dashed", linewidth = 0.7) + # regional_ensemble- there are two MTSS thresholds for this model, but the difference is so small that you will never see it on the plot
     # arrows indicating change
     geom_segment(
       data = xy_joined_rescaled_intersects,
       aes(
         x = xy_global_1995_rescaled,
         xend = xy_global_2055_rescaled,
         y = xy_regional_ensemble_1995_rescaled,
         yend = xy_regional_ensemble_2055_rescaled
       ),
       arrow = grid::arrow(angle = 5.5, type = "closed"), alpha = 0.3, linewidth = 0.25, color = "black"
     ) +
     # historical data
     geom_point(
       aes(x = xy_global_1995_rescaled, y = xy_regional_ensemble_1995_rescaled, shape = "Present"),
       size = 2, stroke = 0.7, color = "black", fill = "orchid1"
     ) +
     # GFDL ssp370 data
     geom_point(
       aes(x = xy_global_2055_rescaled, y = xy_regional_ensemble_2055_rescaled, shape = "2041-2070\nGFDL-ESM4\nmean ssp126/370/585"),
       size = 2, stroke = 0.7, color = "black", fill = "purple3"
     ) +
     # axes scaling
     scale_x_continuous(name = "'global' model risk projection", limits = c(0, 1), breaks = breaks, labels = labels) +
     scale_y_continuous(name = "'regional_ensemble' model risk projection", limits = c(0, 1), breaks = breaks, labels = labels) +
     # quadrant labels
     # extreme risk, top right, quad4
     geom_label(aes(x = 0.75, y = 0.9, label = "extreme risk"), fill = "darkred", color = "azure", size = 5) +
     # high risk, top left, quad3
     geom_label(aes(x = 0.25, y = 0.9, label = "high risk"), fill = "darkorange", color = "azure", size = 5) +
     # moderate risk, bottom right, quad2
     geom_label(aes(x = 0.75, y = 0.1, label = "moderate risk"), fill = "gold", color = "azure", size = 5) +
     # low risk, bottom left, quad1
     geom_label(aes(x = 0.25, y = 0.1, label = "low risk"), fill = "azure4", color = "azure", size = 5) +
     # aesthetics
     scale_shape_manual(name = "Time period", values = c(21, 21)) +
     guides(shape = guide_legend(nrow = 1, override.aes = list(size = 2.5), reverse = TRUE)) +
     theme_bw() +
     theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
     coord_fixed(ratio = 1) +
    labs(
      title = "Projected risk of Lycorma delicatula establishment under climate change",
      subtitle = paste0(stringr::str_to_title(locality_name_internal), ": important viticultural regions"),
      caption = paste0("arrows indicate a region is crossing a risk threshold (dashed lines, MTSS thresh)", ifelse(!is.na(buffer.dist), paste0("\n", buffer.dist, "m buffer used for suitability of viticultural areas"), ""))
    )


  # success message
  cli::cli_alert_success("Viticultural risk plot created")

  ## create IVR summary table---------------------------------------------------

  # join rescaled suitability values with IVR locations
  IVR_locations_joined <- dplyr::left_join(IVR_locations_locality, xy_joined_rescaled, by = c("ID", "x", "y")) %>%
    dplyr::relocate(ID, x, y)


  # calculate risk quadrants
  IVR_locations_risk <- IVR_locations_joined %>%
    dplyr::mutate(
      risk_1995 = slfSpread::calculate_risk_quadrant(
        suit.x = IVR_locations_joined$xy_global_1995_rescaled,
        suit.y = IVR_locations_joined$xy_regional_ensemble_1995_rescaled,
        thresh.x = global_MTSS, # this threshold remains the same
        thresh.y = regional_ensemble_MTSS_1995
      ),
      risk_2055 = slfSpread::calculate_risk_quadrant(
        suit.x = IVR_locations_joined$xy_global_2055_rescaled,
        suit.y = IVR_locations_joined$xy_regional_ensemble_2055_rescaled,
        thresh.x = global_MTSS,
        thresh.y = regional_ensemble_MTSS_2055
      ),
      risk_shift = str_c(risk_1995, risk_2055, sep = "-")
    )


  # create risk table
  IVR_risk_table <- IVR_locations_risk %>%
    # create counts and make into acrostic table
    dplyr::group_by(risk_1995, risk_2055) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = risk_2055, values_from = count) %>%
    dplyr::ungroup()

  # add columns that do not exist
  if(!'extreme' %in% names(IVR_risk_table)) IVR_risk_table <- IVR_risk_table %>% tibble::add_column(extreme = 0)
  if(!'high' %in% names(IVR_risk_table)) IVR_risk_table <- IVR_risk_table %>% tibble::add_column(high = 0)
  if(!'moderate' %in% names(IVR_risk_table)) IVR_risk_table <- IVR_risk_table %>% tibble::add_column(moderate = 0)
  if(!'low' %in% names(IVR_risk_table)) IVR_risk_table <- IVR_risk_table %>% tibble::add_column(low = 0)

  # ensure all combinations of risk exist
  if(!'extreme' %in% IVR_risk_table$risk_1995) IVR_risk_table <- IVR_risk_table %>% tibble::add_row(risk_1995 = "extreme", extreme = 0, high = 0, moderate = 0, low = 0)
  if(!'high' %in% IVR_risk_table$risk_1995) IVR_risk_table <- IVR_risk_table %>% tibble::add_row(risk_1995 = "high", extreme = 0, high = 0, moderate = 0, low = 0)
  if(!'moderate' %in% IVR_risk_table$risk_1995) IVR_risk_table <- IVR_risk_table %>% tibble::add_row(risk_1995 = "moderate", extreme = 0, high = 0, moderate = 0, low = 0)
  if(!'low' %in% IVR_risk_table$risk_1995) IVR_risk_table <- IVR_risk_table %>% tibble::add_row(risk_1995 = "low", extreme = 0, high = 0, moderate = 0, low = 0)

  # tidy
  IVR_risk_table <- IVR_risk_table %>%
    dplyr::rename("rows_1995_cols_2055" = "risk_1995") %>%
    dplyr::relocate("rows_1995_cols_2055", "extreme", "high", "moderate") %>%
    dplyr::arrange(factor(.$rows_1995_cols_2055, levels = risk_levels)) %>%
    # replace missing categories with 0
    replace(is.na(.), 0)

  # more tidying
  # add totals row and column
  IVR_risk_table <- IVR_risk_table %>%
    tibble::add_column("total_present" = rowSums(dplyr::select(., 2:5))) %>%
    tibble::add_row(rows_1995_cols_2055 = "total_2055", extreme = colSums(dplyr::select(., 2)), high = colSums(dplyr::select(., 3)), moderate = colSums(dplyr::select(., 4)), low = colSums(dplyr::select(., 5)), total_present = nrow(IVR_locations_locality)) %>%
    as.data.frame()

  # edit column of rownames
  IVR_risk_table[1:4, 1] <- str_c(IVR_risk_table[1:4, 1], "present", sep = "_")
  # add rownames
  rownames(IVR_risk_table) <- IVR_risk_table[, 1]
  # get rid of names column
  IVR_risk_table <- dplyr::select(IVR_risk_table, -rows_1995_cols_2055)
  # edit column names
  IVR_risk_table <- dplyr::rename(
    IVR_risk_table,
    "extreme_2055" = "extreme",
    "high_2055" = "high",
    "moderate_2055" = "moderate",
    "low_2055" = "low"
  )


  # begin formatting
  # add negative sign to top half
  IVR_risk_table[1, 2] <- -(IVR_risk_table[1, 2])
  IVR_risk_table[1:2, 3] <- -(IVR_risk_table[1:2, 3])
  IVR_risk_table[1:3, 4] <- -(IVR_risk_table[1:3, 4])

  # add positive sign to bottom half
  IVR_risk_table[2:4, 1] <- sprintf("%+.0f", IVR_risk_table[2:4, 1])
  IVR_risk_table[3:4, 2] <- sprintf("%+.0f", IVR_risk_table[3:4, 2])
  IVR_risk_table[4, 3] <- sprintf("%+.0f", IVR_risk_table[4, 3])

  # add color formatting to totals
  # extreme risk
  IVR_risk_table[1, 5] <- cell_spec(IVR_risk_table[1, 5], format = "html", bold = TRUE, escape = FALSE, color = "darkred")
  IVR_risk_table[5, 1] <- cell_spec(IVR_risk_table[5, 1], format = "html", bold = TRUE, escape = FALSE, color = "darkred")
  # high risk
  IVR_risk_table[2, 5] <- cell_spec(IVR_risk_table[2, 5], format = "html", bold = TRUE, escape = FALSE, color = "darkorange")
  IVR_risk_table[5, 2] <- cell_spec(IVR_risk_table[5, 2], format = "html", bold = TRUE, escape = FALSE, color = "darkorange")
  # moderate risk
  IVR_risk_table[3, 5] <- cell_spec(IVR_risk_table[3, 5], format = "html", bold = TRUE, escape = FALSE, color = "gold")
  IVR_risk_table[5, 3] <- cell_spec(IVR_risk_table[5, 3], format = "html", bold = TRUE, escape = FALSE, color = "gold")
  # low risk
  IVR_risk_table[4, 5] <- cell_spec(IVR_risk_table[4, 5], format = "html", bold = TRUE, escape = FALSE, color = "darkgrey")
  IVR_risk_table[5, 4] <- cell_spec(IVR_risk_table[5, 4], format = "html", bold = TRUE, escape = FALSE, color = "darkgrey")

  # bold total
  IVR_risk_table[5, 5] <- cell_spec(IVR_risk_table[5, 5], format = "html", bold = TRUE, escape = FALSE)


  # print table, e.g., in html format
  IVR_risk_kable <- knitr::kable(IVR_risk_table, "html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
    # standardize col width
    kableExtra::column_spec(1:5, width_min = '4cm') %>%
    # add footnotes
    kableExtra::add_footnote("number signs indicate whether climate change is increasing or decreasing risk", notation = "alphabet") %>%
    # styling
    kableExtra::add_header_above(., header = c("Risk of L delicatula establishment for important viticultural regions" = 6), bold = TRUE)  %>%
    # conditional addition of footnote about buffer area
    kableExtra::add_footnote(ifelse(!is.na(buffer.dist), paste0(buffer.dist, "m buffer used for suitability of viticultural areas"), ""), notation = "alphabet")

  ## create range shift table---------------------------------------------------

  # use terra expanse to calculate suitable area
  slf_range_shift_table <- terra::expanse(
    x = slf_range_shift,
    unit = "km",
    byValue = TRUE
  )


  # naming object
  range.tibble <- tibble::tibble(
    Ld_range_shift_type = c("remains_unsuitable", "contraction", "expansion", "retained_suitability"),
    value = c(5, 6, 9, 10)
  )


  #tidying
  # add rows
  slf_range_shift_table <-  slf_range_shift_table %>%
    dplyr::left_join(., range.tibble, by = "value")

  # conditional to add missing categories
  if(!'remains_unsuitable' %in% slf_range_shift_table$Ld_range_shift_type) slf_range_shift_table <- slf_range_shift_table %>% tibble::add_row(layer = 1, value = 5, area = 0, Ld_range_shift_type = "remains_unsuitable")
  if(!'contraction' %in% slf_range_shift_table$Ld_range_shift_type) slf_range_shift_table <- slf_range_shift_table %>% tibble::add_row(layer = 1, value = 6, area = 0, Ld_range_shift_type = "contraction")
  if(!'expansion' %in% slf_range_shift_table$Ld_range_shift_type) slf_range_shift_table <- slf_range_shift_table %>% tibble::add_row(layer = 1, value = 9, area = 0, Ld_range_shift_type = "expansion")
  if(!'retained_suitability' %in% slf_range_shift_table$Ld_range_shift_type) slf_range_shift_table <- slf_range_shift_table %>% tibble::add_row(layer = 1, value = 10, area = 0, Ld_range_shift_type = "retained_suitability")


  # more tidying
  slf_range_shift_table <- slf_range_shift_table %>%
    dplyr::mutate(
      prop_total_area = scales::label_percent()(area / sum(area)),
      area = scales::label_comma()(area)
      ) %>%
    dplyr::select(-c(value, layer)) %>%
    dplyr::relocate(Ld_range_shift_type, area) %>%
    replace(is.na(.), 0)

  # EVEN MORE tidying
  # add superscript
  colnames(slf_range_shift_table)[2] <- paste0("area_km", common::supsc("2"))

  # .html formatting
  # format row colors
  slf_range_shift_table <- slf_range_shift_table %>%
    dplyr::mutate(Ld_range_shift_type = kableExtra::cell_spec(Ld_range_shift_type, format = "html", escape = FALSE, background = dplyr::case_when(
      Ld_range_shift_type == "remains_unsuitable" ~ "azure4",
      Ld_range_shift_type == "contraction" ~ "darkred",
      Ld_range_shift_type == "expansion" ~ "darkgreen",
      Ld_range_shift_type == "retained_suitability" ~ "azure"
    )
    ))

  # convert to kable
  slf_range_shift_kable <- knitr::kable(x = slf_range_shift_table, format = "html", escape = FALSE) %>%
    # standardize col width
    kableExtra::column_spec(1:3, width_min = '4cm') %>%
    # styling
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE)




  # format IVR table------------------------------------------------------------

  # create .csv output
  IVR_locations_output <- IVR_locations_locality %>%
    dplyr::left_join(., xy_joined_rescaled, by = c("ID", "x", "y")) %>%
    dplyr::rename(
      "global_model_risk_present" = "xy_global_1995_rescaled",
      "regional_ensemble_risk_present" = "xy_regional_ensemble_1995_rescaled",
      "global_model_risk_2055" = "xy_global_2055_rescaled",
      "regional_ensemble_risk_2055" = "xy_regional_ensemble_2055_rescaled"
    ) %>%
    dplyr::mutate(
      global_model_risk_present = round((global_model_risk_present * 10), 2),
      regional_ensemble_risk_present = round((regional_ensemble_risk_present * 10), 2),
      global_model_risk_2055 = round((global_model_risk_2055 * 10), 2),
      regional_ensemble_risk_2055 = round((regional_ensemble_risk_2055 * 10), 2)
    ) %>%
    dplyr::relocate(ID, x, y, Continent, Country, Region, `Sub-Region`, global_model_risk_present, regional_ensemble_risk_present, global_model_risk_2055, regional_ensemble_risk_2055)



  # format as .html
  IVR_locations_output_kable <- knitr::kable(IVR_locations_output, "html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
    kableExtra::add_header_above(., header = c("L delicatula risk to important viticultural regions" = 11), bold = TRUE)

  # success message
  cli::cli_alert_success("Viticultural risk table created")



  ## create report--------------------------------------------------------------

  slf_risk_report <- list(
    paste0("Report prepared for: ", stringr::str_to_title(locality_name_internal)),
    "viticultural_regions_list" = IVR_locations_output_kable,
    "risk_maps" = list(
      "current_risk_map" = slf_binarized_1995_plot,
      "2055_risk_map" = slf_binarized_2055_plot
    ),
    "viticultural_risk_plot" = xy_joined_rescaled_plot,
    "viticultural_risk_table" = IVR_risk_kable,
    "range_shift_map" = slf_range_shift_plot,
    "range_shift_table" = slf_range_shift_kable
  )



  # success message
  cli::cli_alert_success("Report created")

  ## return report and save if save.report = TRUE-------------------------------

  if(save.report == TRUE) {

    # return output
    assign(paste0(locality_name_internal, "_slf_risk_report"), slf_risk_report, envir = .GlobalEnv)

    # check if directory exists
    if(dir.exists(mypath) == FALSE) {

      cli::cli_abort(paste0("Report output could not be saved because directory does not exist:\n", mypath))
      stop()
    }

    # save files
    # IVR list
    readr::write_csv(IVR_locations_output, file = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_viticultural_regions_list.csv")))

    # risk maps
    ggsave(
      slf_binarized_1995_plot,
      filename = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_risk_map_present.jpg")),
      height = 8,
      width = 10,
      device = "jpeg",
      dpi = "retina"
    )
    ggsave(
      slf_binarized_2055_plot,
      filename = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_risk_map_2055_ssp_126_370_585_GFDL-ESM4.jpg")),
      height = 8,
      width = 10,
      device = "jpeg",
      dpi = "retina"
    )

    # range shift map
    ggsave(
      slf_range_shift_plot,
      filename = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_range_shift_map_2055_ssp_126_370_585_GFDL-ESM4.jpg")),
      height = 8,
      width = 10,
      device = "jpeg",
      dpi = "retina"
    )

    # risk quadrant plot
    ggsave(
      xy_joined_rescaled_plot,
      filename = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_viticultural_risk_plot.jpg")),
      height = 8,
      width = 8,
      device = "jpeg",
      dpi = "retina"
    )

    # IVR risk table
    # save as .html
    kableExtra::save_kable(
      IVR_risk_kable,
      file = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_viticultural_risk_table.html")),
      self_contained = TRUE
    )

    # convert to jpg
    webshot2::webshot(
      url = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_viticultural_risk_table.html")),
      file = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_viticultural_risk_table.jpg"))
    )

    # slf range shift table
    kableExtra::save_kable(
      slf_range_shift_kable,
      file = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_range_shift_table.html")),
      self_contained = TRUE
    )

    # convert to jpg
    webshot2::webshot(
      url = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_range_shift_table.html")),
      file = file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_range_shift_table.jpg"))
    )



    # remove .html files
    file.remove(
      file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_viticultural_risk_table.html")),
      file.path(mypath, paste0(locality_name_internal, "_L_delicatula_report_range_shift_table.html"))
      )


    # success message
    cli::cli_alert_success("Report saved to file")
    # DONE saving


  } else if(save.report == FALSE) {

    assign(paste0(locality_name_internal, "_slf_risk_report"), slf_risk_report, envir = .GlobalEnv)

  }

  # DONE

}
