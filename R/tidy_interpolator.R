#' Retrieving interpolation parameters from user
#'
#' Internal function to ensure all interpolation parameters are included in the
#' final interpolator
#'
#' This functions ensures that if no parameters are provided, the default ones
#' are used. Also, if params are partially provided, this functions ensures that
#' the rest of the parameters are taken from the defaults, allowing the user to
#' provide only those parameters they want to change from default ones. This
#' function is designed to be used internally by other functions in the package
#'
#' @param params list with the parameters provided by the user
#' @param ref reference parameters to get missing ones from params argument. Default
#'   to \code{\link{defaultInterpolationParams}}
#' @param verbose Logical indicating if the function must show messages and info.
#' Default value checks \code{"meteoland_verbosity"} option and if not set, defaults
#' to TRUE. It can be turned off for the function with FALSE, or session wide with
#' \code{options(meteoland_verbosity = FALSE)}
#'
#' @return A complete parameter list to use in the interpolator object
#' @family interpolator functions
#' @noRd
.safely_create_interpolation_params <- function(params, ref = defaultInterpolationParams(), verbose = getOption("meteoland_verbosity", TRUE)) {
  if (is.null(params)) {
    cli::cli_warn(c(
      "No interpolation parameters provided, using defaults",
      "i" = "Set the {.arg params} argument to modify parameter default values"
    ))
    return(defaultInterpolationParams())
  }

  user_params <- params[names(params) %in% names(ref)]

  if (length(user_params) < length(ref)) {
    .verbosity_control(
      cli::cli_alert_info("Some interpolation parameters are missing, using default values for those"),
      verbose
    )
  }

  if (length(params[!names(params) %in% names(ref)]) > 0) {
    offending_params <- names(
      params[!names(params) %in% names(ref)]
    )
    cli::cli_warn(c(
      "The following interpolation parameter{?s} {?was/were} provided and will not be used: {.val {offending_params}}"
    ))
  }

  for (name in names(user_params)) {
    ref[[name]] <- user_params[[name]]
  }

  return(ref)
}

#' Retrieving interpolation parameters from interpolator object
#'
#' Retrieve the parameter list from and interpolator object
#'
#'
#' @param interpolator interpolator object as returned by
#' \code{\link{create_meteo_interpolator}}
#' @return The complete parameter list from the interpolator object
#' @seealso Other interpolator functions: \code{\link{add_topo}()},
#' \code{\link{create_meteo_interpolator}()},
#' \code{\link{read_interpolator}()}, \code{\link{set_interpolation_params}()},
#' \code{\link{with_meteo}()}, \code{\link{write_interpolator}()}
#'
#' @author 
#' Victor Granda \enc{García}{Garcia}, EMF-CREAF
#' 
#' @examples
#' # example interpolator
#' data(meteoland_interpolator_example)
#' # get the params from the interpolator
#' get_interpolation_params(meteoland_interpolator_example)
#'
#' @export get_interpolation_params
get_interpolation_params <- function(interpolator) {
  return(attr(interpolator, "params"))
}

#' Setting interpolation parameters in an interpolator object
#'
#' Changing or updating interpolation parameters in an interpolator object
#'
#' This function ensures that if no parameters are provided, the default ones
#' are used (see \code{\link{defaultInterpolationParams}}). Also, if params are
#' partially provided, this function ensures that the rest of the parameters
#' are not changed.
#'
#' @param interpolator interpolator object to update
#' @param params list with the parameters provided by the user
#' @param verbose Logical indicating if the function must show messages and info.
#' Default value checks \code{"meteoland_verbosity"} option and if not set, defaults
#' to TRUE. It can be turned off for the function with FALSE, or session wide with
#' \code{options(meteoland_verbosity = FALSE)}
#' @return The same interpolator object provided, with the updated
#' interpolation parameters
#' @seealso Other interpolator functions: \code{\link{add_topo}()},
#' \code{\link{create_meteo_interpolator}()},
#' \code{\link{get_interpolation_params}()}, \code{\link{read_interpolator}()},
#' \code{\link{with_meteo}()}, \code{\link{write_interpolator}()}
#'
#' @author 
#' Victor Granda \enc{García}{Garcia}, EMF-CREAF
#' 
#' @examples
#' # example interpolator
#' data(meteoland_interpolator_example)
#' # store the actual parameters
#' old_parameters <- get_interpolation_params(meteoland_interpolator_example)
#' # we can provide only the parameter we want to change
#' meteoland_interpolator_example <- set_interpolation_params(
#'   meteoland_interpolator_example,
#'   list(debug = TRUE)
#' )
#' # check
#' get_interpolation_params(meteoland_interpolator_example)$debug
#' # compare with old
#' old_parameters$debug
#' # the rest should be the same
#' setdiff(old_parameters, get_interpolation_params(meteoland_interpolator_example))
#'
#' @export set_interpolation_params
set_interpolation_params <- function(interpolator, params = NULL, verbose = getOption("meteoland_verbosity", TRUE)) {

  # sometimes, the interpolator cab lost the params (aggregation, subsetting...),
  # so in that case, we need to go for the default ones
  ref_params <- get_interpolation_params(interpolator)
  if (is.null(ref_params)) {
    ref_params <- defaultInterpolationParams()
  }

  # ensure the params are correct and fill any lacking with the defaults
  safe_params <-
    .safely_create_interpolation_params(params, ref_params, verbose)
  # remove any previous params
  attr(interpolator, "params") <- NULL
  # add new ones
  attr(interpolator, "params") <- safe_params

  return(interpolator)
}

#' Meteoland interpolator creation
#'
#' Function to create the meteoland interpolator
#'
#' This function takes meteorology information and a list of interpolation
#' parameters and creates the interpolator object to be ready to use.
#'
#' @param meteo_with_topo Meteo object, as returned by \code{\link{with_meteo}}
#' @param params Interpolation parameters as a list. Typically the result of
#'   \code{\link{defaultInterpolationParams}}.
#' @param verbose Logical indicating if the function must show messages and info.
#' Default value checks \code{"meteoland_verbosity"} option and if not set, defaults
#' to TRUE. It can be turned off for the function with FALSE, or session wide with
#' \code{options(meteoland_verbosity = FALSE)}
#' @return an interpolator object (stars)
#' @seealso Other interpolator functions: \code{\link{add_topo}()},
#' \code{\link{get_interpolation_params}()}, \code{\link{read_interpolator}()},
#' \code{\link{set_interpolation_params}()}, \code{\link{with_meteo}()},
#' \code{\link{write_interpolator}()}
#'
#' @author 
#' Victor Granda \enc{García}{Garcia}, EMF-CREAF
#' 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' 
#' @examples
#'
#' # example meteo data
#' data(meteoland_meteo_example)
#'
#' # create the interpolator with default params
#' with_meteo(meteoland_meteo_example) |>
#'   create_meteo_interpolator()
#'
#' # create the interpolator with some params changed
#' with_meteo(meteoland_meteo_example) |>
#'   create_meteo_interpolator(params = list(debug = TRUE))
#'
#' @export create_meteo_interpolator
create_meteo_interpolator <- function(meteo_with_topo, params = NULL, verbose = getOption("meteoland_verbosity", TRUE)) {

  ## TODO messaging
  assertthat::assert_that(has_meteo(meteo_with_topo))
  assertthat::assert_that(has_topo(meteo_with_topo))

  assertthat::assert_that(
    is.null(params) || inherits(params, "list"),
    msg = "params must be NULL or a named list with the interpolation parameters"
  )

  .verbosity_control(
    cli::cli_alert_info("Creating interpolator..."),
    verbose
  )

  # get params
  params <- .safely_create_interpolation_params(params, verbose = verbose)

  # data preparation

  # elevation filtering
  if (any(is.na(meteo_with_topo[["elevation"]]))) {

    meteo_with_topo <- meteo_with_topo |>
      dplyr::filter(!is.na(.data$elevation))

    if (nrow(meteo_with_topo) < 1) {
      cli::cli_abort(c(
        "No elevation values for any station, stopping creation of the interpolator"
      ))
    }

    # warning here, because that way if there is no elevation whatsoever we get
    # only the error (as it should be), not the warning AND the error.
    cli::cli_warn(c(
      "Some meteo stations lack values for elevation, filtering those stations out"
    ))
  }

  # helper to avoid NAs in elevation when completing cases
  .fill_elevation <- function(arranged_data) {
    arranged_data |>
      dplyr::group_by(.data$stationID) |>
      dplyr::mutate(
        elevation = dplyr::if_else(
          is.na(.data$elevation),
          unique(purrr::keep(.data$elevation, ~!is.na(.x))),
          .data$elevation
        )
      ) |>
      dplyr::ungroup() |>
      sf::st_as_sf()
  }

  # helper to complete topo when completing cases, also, convert any aspect or slope
  # that is NA in 0 as it's better for the interpolation.
  .fill_topo <- function(arranged_data) {

    # create the aspect and slope vars if they dont exist
    if (is.null(arranged_data[["slope"]])) {
      arranged_data[["slope"]] <- 0
    }
    if (is.null(arranged_data[["aspect"]])) {
      arranged_data[["aspect"]] <- 0
    }
    # fill the variables
    arranged_data |>
      dplyr::group_by(.data$stationID) |>
      dplyr::mutate(
        # fill elevation
        elevation = dplyr::if_else(
          is.na(.data$elevation),
          unique(purrr::keep(.data$elevation, ~!is.na(.x))),
          .data$elevation
        ),
        # fill slope and aspect
        slope = dplyr::if_else(
          is.na(.data$slope),
          0,
          .data$slope
        ),
        aspect = dplyr::if_else(
          is.na(.data$aspect),
          0,
          .data$aspect
        )
      ) |>
      dplyr::ungroup() |>
      sf::st_as_sf()
  }

  # data arranging
  meteo_arranged <- meteo_with_topo |>
    # very important step, as we need all combinations to fill the arrays
    tidyr::complete(.data$dates, .data$stationID, explicit = FALSE) |>
    dplyr::arrange(.data$stationID, .data$dates) |>
    # .fill_elevation()
    .fill_topo()

  # stations and dates
  stations <- meteo_arranged |>
    dplyr::filter(
      !sf::st_is_empty(!!dplyr::sym(attr(meteo_arranged, "sf_column")))
    ) |>
    dplyr::select("stationID") |>
    dplyr::distinct() |>
    sf::st_geometry()
  dates <- unique(meteo_arranged$dates)

  # dimensions
  interpolator_dims <- stars::st_dimensions(
    date = dates, station = stations
  )

  # helper
  .interpolator_arrays_creator <- function(variable, arranged_data, dims) {
    col_names <- arranged_data |>
      dplyr::pull("stationID") |>
      unique()

    if (! variable %in% names(arranged_data)) {
      array_data <- array(NA_real_, dim = dims)
    } else {
      variable_data <- arranged_data |>
        dplyr::pull(!!variable)
      # transform logical NAs to numeric NAs
      if (is.logical(variable_data)) {
        variable_data <- as.numeric(variable_data)
      }
      array_data <- variable_data |>
        array(dim = dims)
    }

    colnames(array_data) <- col_names
    rownames(array_data) <- unique(as.character(arranged_data$dates))

    return(array_data)
  }

  stars_interpolator <-
    list(
      Temperature = "MeanTemperature",
      MinTemperature = "MinTemperature",
      MaxTemperature = "MaxTemperature",
      RelativeHumidity = "MeanRelativeHumidity",
      Precipitation = "Precipitation",
      Radiation = "Radiation",
      WindDirection = "WindDirection",
      WindSpeed = "WindSpeed",
      elevation = "elevation",
      aspect = "aspect",
      slope = "slope"
      # stationID = "stationID"
    ) |>
    purrr::map(
      .interpolator_arrays_creator,
      arranged_data = meteo_arranged,
      dims = dim(interpolator_dims)
    ) |>
    purrr::discard(is.null) |>
    stars::st_as_stars(dimensions = interpolator_dims)


  # add smoothed vars
  .verbosity_control(
    cli::cli_ul("Calculating smoothed variables..."),
    verbose
  )
  stars_interpolator[["SmoothedPrecipitation"]] <-
    .temporalSmoothing(
      t(stars_interpolator[["Precipitation"]]),
      params$St_Precipitation,
      TRUE
    ) |> t()
  colnames(stars_interpolator[["SmoothedPrecipitation"]]) <-
    colnames(stars_interpolator[["Precipitation"]])
  attributes(stars_interpolator[["SmoothedPrecipitation"]]) <-
    attributes(stars_interpolator[["Precipitation"]])

  stars_interpolator[["SmoothedTemperatureRange"]] <-
    .temporalSmoothing(
      t(stars_interpolator[["MaxTemperature"]] - stars_interpolator[["MinTemperature"]]),
      params$St_TemperatureRange,
      FALSE
    ) |> t()
  colnames(stars_interpolator[["SmoothedTemperatureRange"]]) <-
    colnames(stars_interpolator[["MinTemperature"]])
  attributes(stars_interpolator[["SmoothedTemperatureRange"]]) <-
    attributes(stars_interpolator[["MinTemperature"]])

  # update initial Rp in params
  .verbosity_control(
    cli::cli_ul("Updating intial_Rp parameter with the actual stations mean distance..."),
    verbose
  )

  params$initial_Rp <-
    meteo_arranged |>
    # We need to pass the Rp in the same units the coordinates are,
    # because the C++ methods calculate distances with the points as
    # planar/cartesian distances. i.e. If Rp are in meters (which happens
    # if we have a CRS setted), but coordinates are lat/long, C++ methods will
    # calculate distance with points wrongly (as coordinates are in degrees)
    # For this, we extract the coordinates and pass unique. This ensures the
    # CRS is lost, so distances are calculated in the arbitrary units (the
    # same as coordinates).
    # dplyr::select(attr(meteo_arranged, "sf_column")) |>
    sf::st_geometry(meteo_arranged) |>
    unique() |>
    sf::st_as_sfc() |>
    sf::st_distance() |>
    as.numeric() |>
    mean(na.rm = TRUE)

  ### We cannot give a warning now, because we don't know the units a priori
  ### (Unless we recalculate the distance again, this time with units)
  # give a warning if mean distance is bigger than 100 km
  # if (params$initial_Rp > 100000) {
  #   cli::cli_alert_danger(
  #     "initial_Rp value (mean distance between meteorological stations) is {.strong {round(params$initial_Rp / 1000, 2)}km}"
  #   )
  #   cli::cli_alert_info(c(
  #     "Please check the meteorological data. ",
  #     "Interpolation results can be affected when stations are scattered and with great distances between them."
  #   ))
  # }

  # set the params as an attribute of stars_interpolator
  attr(stars_interpolator, "params") <- params

  # return the interpolator
  .verbosity_control(
    cli::cli_alert_success("Interpolator created."),
    verbose
  )
  return(stars_interpolator)
}

# write the interpolator in the NetCDF-CF standard
# https://cfconventions.org/cf-conventions/cf-conventions.html
#' Write the interpolator object
#'
#' Write the interpolator object to a file
#'
#' This function writes the interpolator object created with
#' \code{\link{create_meteo_interpolator}} in a NetCDF-CF standard
#' compliant format, as specified in
#' https://cfconventions.org/cf-conventions/cf-conventions.html
#'
#' @param interpolator meteoland interpolator object, as created by
#' \code{\link{create_meteo_interpolator}}
#' @param filename file name for the interpolator nc file
#' @param .overwrite logical indicating if the file should be overwritten if it
#' already exists
#' @return invisible interpolator object, to allow using this function as a
#' step in a pipe
#' @seealso Other interpolator functions: \code{\link{add_topo}()},
#' \code{\link{create_meteo_interpolator}()},
#' \code{\link{get_interpolation_params}()}, \code{\link{read_interpolator}()},
#' \code{\link{set_interpolation_params}()}, \code{\link{with_meteo}()}
#'
#' @examples
#'
#' \donttest{
#' # example interpolator
#' data(meteoland_interpolator_example)
#'
#' # temporal folder
#' tmp_dir <- tempdir()
#'
#' # write interpolator
#' write_interpolator(
#'   meteoland_interpolator_example,
#'   file.path(tmp_dir, "meteoland_interpolator_example.nc"),
#'   .overwrite = TRUE
#' )
#'
#' # check file exists
#' file.exists(file.path(tmp_dir, "meteoland_interpolator_example.nc"))
#'
#' # read it again
#' read_interpolator(file.path(tmp_dir, "meteoland_interpolator_example.nc"))
#' }
#' 
#' @author 
#' Victor Granda \enc{García}{Garcia}, EMF-CREAF
#'
#' @export write_interpolator
write_interpolator <- function(interpolator, filename, .overwrite = FALSE) {
  # debug
  # browser()

  # assertions
  assertthat::assert_that(assertthat::is.string(filename))
  assertthat::assert_that(is.logical(.overwrite))


  ## overwrite
  if (.overwrite) {
    if (file.exists(filename)) {
      file.remove(filename)
    }
  }

  assertthat::assert_that(
    !file.exists(filename),
    msg = paste0(
      filename,
      " file already exists. Please provide another filename or set .overwrite to TRUE"
    )
  )

  ## prepared data
  prepared_data_list <- names(interpolator) |>
    purrr::map(~ dplyr::as_tibble(interpolator[[.x]])) |>
    # purrr::map(~ purrr::set_names(.x, interpolator[["stationID"]][1,])) |>
    purrr::set_names(names(interpolator))
  # remove stationID because we dont need it in the nc
  # prepared_data_list$stationID <- NULL

  # and now the units, is important
  prepared_data_units <- names(prepared_data_list) |>
    purrr::map(~ switch(
      .x,
      Temperature = 'celsius', MinTemperature = 'celsius', MaxTemperature = 'celsius',
      RelativeHumidity = 'percent', Precipitation = 'mm',
      Radiation = 'MJ/m2', WindDirection = 'degree', WindSpeed = 'm/s',
      elevation = 'm', aspect = 'degree', slope = 'degree',
      SmoothedPrecipitation = 'mm', SmoothedTemperatureRange = 'celsius'
    )) |>
    purrr::set_names(names(prepared_data_list))



  interpolator_attributes <- get_interpolation_params(interpolator)
  interpolator_attributes[["debug"]] <-
    dplyr::if_else(interpolator_attributes[["debug"]], 1, 0)

  ## write logic
  cf_conventions_web <- "https://cfconventions.org/cf-conventions/cf-conventions.html"
  cli::cli_alert_info("Creating nc file following the NetCDF-CF conventions {.url {cf_conventions_web}}")

  ### TODO add correct units
  list(prepared_data_list, names(prepared_data_list), prepared_data_units) |>
    purrr::pwalk(
      ~ ncdfgeom::write_timeseries_dsg(
        nc_file = filename,
        instance_names = names(prepared_data_list[[1]]),
        lats = sf::st_coordinates(
          stars::st_get_dimension_values(interpolator, "station")
        )[,2],
        lons = sf::st_coordinates(
          stars::st_get_dimension_values(interpolator, "station")
        )[,1],
        times = stars::st_get_dimension_values(interpolator, 'date'),
        data = as.data.frame(..1),
        alts = as.numeric(prepared_data_list$elevation[1,]),
        data_unit = ..3,
        data_metadata = list(name = ..2, long_name = ..2),
        time_units = "days since 1970-01-01 00:00:00",
        coordvar_long_names = list(
          instance = "station", time = "date",
          lat = "latitude", lon = "longitude", alt = "elevation"
        ),
        attributes = c(
          interpolator_attributes,
          list(
            title = "meteoland interpolator",
            provider_name = "meteoland R package",
            crs = sf::st_crs(interpolator)$wkt
          )
        ),
        add_to_existing = TRUE
      )
    )

  cli::cli_alert_info("Adding spatial info to nc file")
  filename <- ncdfgeom::write_geometry(
    nc_file = filename,
    geom_data = sf::st_as_sf(stars::st_get_dimension_values(interpolator, "station")),
    variables = c("instance_name", "time", "lat", "lon", names(prepared_data_list))
  )

  cli::cli_alert_success("Done")
  return(invisible(interpolator))
}

#' Read interpolator files
#'
#' Read interpolator files created with \code{\link{write_interpolator}}
#'
#' This function takes the file name of the nc file storing an interpolator
#' object and load it into the work environment
#'
#' @param filename interpolator file name
#' @return an interpolator (stars) object
#' @seealso Other interpolator functions: \code{\link{add_topo}()},
#' \code{\link{create_meteo_interpolator}()},
#' \code{\link{get_interpolation_params}()},
#' \code{\link{set_interpolation_params}()}, \code{\link{with_meteo}()},
#' \code{\link{write_interpolator}()}
#'
#' @examples
#'
#' \donttest{
#' # example interpolator
#' data(meteoland_interpolator_example)
#'
#' # temporal folder
#' tmp_dir <- tempdir()
#'
#' # write interpolator
#' write_interpolator(
#'   meteoland_interpolator_example,
#'   file.path(tmp_dir, "meteoland_interpolator_example.nc")
#' )
#'
#' # check file exists
#' file.exists(file.path(tmp_dir, "meteoland_interpolator_example.nc"))
#'
#' # read it again
#' read_interpolator(file.path(tmp_dir, "meteoland_interpolator_example.nc"))
#' }
#'
#' @author 
#' Victor Granda \enc{García}{Garcia}, EMF-CREAF
#' 
#' @export read_interpolator
read_interpolator <- function(filename) {
  # debug
  # browser()

  # get the attributes
  interpolation_attributes <- ncmeta::nc_meta(filename)$attribute |>
    dplyr::filter(
      .data$variable == "NC_GLOBAL",
      .data$name %in% names(defaultInterpolationParams())
    ) |>
    dplyr::pull(.data$value)

  # Check if attributes exist, if not is not an interpolator
  if (length(interpolation_attributes) < 1) {
    cli::cli_abort(c(
      "{.file {filename}} is not a meteoland interpolator file.",
      "x" = "No interpolation params stored in the file"
    ))
  }

  # remember to convert debug value to logical
  interpolation_attributes$debug <- as.logical(interpolation_attributes$debug)

  # get the data from the nc file
  ts_data <- ncdfgeom::read_timeseries_dsg(filename)

  # get the geometries
  wkt_crs <- ncmeta::nc_meta(filename)$attribute |>
    dplyr::filter(.data$variable == "NC_GLOBAL", .data$name == 'crs') |>
    dplyr::pull(.data$value) |>
    as.character()
  geom_data <- ncdfgeom::read_geometry(filename) |>
    sf::st_transform(crs = sf::st_crs(wkt_crs))

  # get the dates
  dates <- ts_data$time

  # build the interpolator object
  interpolator_dims <- stars::st_dimensions(
    date = dates,
    station = sf::st_geometry(geom_data)
  )

  .array_creation_helper <- function(dataframe, interpolator_dims, dates) {
    array_data <- dataframe |>
      as.matrix() |>
      array(dim = dim(interpolator_dims))

    colnames(array_data) <- names(dataframe)
    rownames(array_data) <- as.character(dates)
    return(array_data)
  }

  stars_interpolator <-
    ts_data$data_frames |>
    purrr::map(
      .array_creation_helper,
      interpolator_dims = interpolator_dims, dates = dates
    ) |>
    stars::st_as_stars(dimensions = interpolator_dims)

  attr(stars_interpolator, 'params') <- interpolation_attributes

  return(stars_interpolator)

}

#' Calibration and validation of interpolation procedures
#'
#' Calibration and validation of interpolation procedures
#'
#' Function \code{interpolator_calibration} determines optimal interpolation
#' parameters \code{"N"} and \code{"alpha"} for a given meteorological
#' variable. Optimization is done by minimizing mean absolute error ("MAE")
#' (Thornton \emph{et al.} 1997). Function
#' \code{interpolation_cross_validation} calculates average mean absolute
#' errors ("MAE") for the prediction period of the interpolator object. In both
#' calibration and cross validation procedures, predictions for each
#' meteorological station are made using a \emph{leave-one-out} procedure (i.e.
#' after excluding the station from the predictive set).
#'
#' @param interpolator A meteoland interpolator object, as created by
#' \code{\link{create_meteo_interpolator}}
#' @param stations A vector with the stations (numeric for station indexes or
#' character for stations id) to be used to calculate \code{"MAE"}. All
#' stations with data are included in the training set but predictive
#' \code{"MAE"} are calculated for the stations subset indicated in
#' \code{stations} param only. If \code{NULL} all stations are used in the
#' predictive \code{"MAE"} calculation.
#' @param variable A string indicating the meteorological variable for which
#' interpolation parameters \code{"N"} and \code{"alpha"} will be calibrated.
#' Accepted values are:
#'   \itemize{
#'     \item{\code{MinTemperature} (kernel for minimum temperature)}
#'     \item{\code{MaxTemperature} (kernel for maximum temperature)}
#'     \item{\code{DewTemperature} (kernel for dew-temperature (i.e. relative humidity))}
#'     \item{\code{Precipitation} (to calibrate the same
#'           kernel for both precipitation events and regression of precipitation amounts;
#'           not recommended)}
#'     \item{\code{PrecipitationAmount} (kernel for regression of precipitation amounts)}
#'     \item{\code{PrecipitationEvent} (kernel for precipitation events)}
#'   }
#' @param N_seq Numeric vector with \code{"N"} values to be tested
#' @param alpha_seq Numeric vector with \code{"alpha"}
#' @param update_interpolation_params Logical indicating if the interpolator
#' object must be updated with the calculated parameters. Default to FALSE
#' @param verbose Logical indicating if the function must show messages and info.
#' Default value checks \code{"meteoland_verbosity"} option and if not set, defaults
#' to TRUE. It can be turned off for the function with FALSE, or session wide with
#' \code{options(meteoland_verbosity = FALSE)}
#' @return If \code{update_interpolation_params} is FALSE (default),
#' \code{interpolator_calibration} returns a list with the following items
#' \itemize{ \item{MAE: A numeric matrix with the mean absolute error values,
#' averaged across stations, for each combination of parameters \code{"N"} and
#' \code{"alpha"}} \item{minMAE: Minimum MAE value} \item{N: Value of parameter
#' \code{"N"} corresponding to the minimum MAE} \item{alpha: Value of parameter
#' \code{"alpha"} corresponding the the minimum MAE} \item{observed: matrix with
#' observed values (meteorological measured values)} \item{predicted: matrix with
#' interpolated values for the optimum parameter combination} } If
#' \code{update_interpolation_params} is FALSE, \code{interpolator_calibration}
#' returns the interpolator provided with the parameters updated
#'
#' @references Thornton, P.E., Running, S.W., 1999. An improved algorithm for
#' estimating incident daily solar radiation from measurements of temperature,
#' humidity, and precipitation. Agric. For. Meteorol. 93, 211–228.
#' doi:10.1016/S0168-1923(98)00126-9.
#'
#' De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating
#' daily meteorological data and downscaling climate models over landscapes.
#' Environmental Modelling and Software 108: 186-196.
#'
#' @author 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' 
#' Victor Granda \enc{García}{Garcia}, EMF-CREAF
#' 
#' @examples
#'
#' \donttest{
#' # example interpolator
#' data("meteoland_interpolator_example")
#'
#' # As the calibration for all stations can be time consuming, we are gonna
#' # interpolate only for the first 5 stations of the 198 and only a handful
#' # of parameter combinations
#' calibration <- interpolator_calibration(
#'   meteoland_interpolator_example,
#'   stations = 1:5,
#'   variable = "MaxTemperature",
#'   N_seq = seq(10, 20, by = 5),
#'   alpha_seq = seq(8, 9, by = 0.25)
#' )
#'
#' # we can update the interpolator params directly:
#' updated_interpolator <- interpolator_calibration(
#'   meteoland_interpolator_example,
#'   stations = 1:5,
#'   update_interpolation_params = TRUE,
#'   variable = "MaxTemperature",
#'   N_seq = seq(10, 20, by = 5),
#'   alpha_seq = seq(8, 9, by = 0.25)
#' )
#'
#'
#' # check the new interpolator have the parameters updated
#' get_interpolation_params(updated_interpolator)$N_MaxTemperature
#' get_interpolation_params(updated_interpolator)$alpha_MaxTemperature
#' }
#'
#' @export interpolator_calibration
interpolator_calibration <- function(
    interpolator,
    stations = NULL,
    update_interpolation_params = FALSE,
    variable = "MinTemperature",
    N_seq = seq(5, 30, by = 5),
    alpha_seq = seq(0.25, 10, by = 0.25),
    verbose = getOption("meteoland_verbosity", TRUE)
) {

  ### assertions
  # interpolator
  assertthat::assert_that(.is_interpolator(interpolator))
  # stations
  assertthat::assert_that(
    is.null(stations) || is.numeric(stations) || is.character(stations),
    msg = "stations must be NULL or a numeric vector with the stations indexes"
  )
  # variables
  assertthat::assert_that(
    is.character(variable), msg = "variable argument must be a character"
  )
  variable <- match.arg(variable, choices = c(
    "MinTemperature", "MaxTemperature", "DewTemperature",
    "Precipitation", "PrecipitationAmount", "PrecipitationEvent"
  ))
  # seqs
  assertthat::assert_that(
    is.numeric(N_seq),
    msg = "N_seq must be a numeric vector"
  )
  assertthat::assert_that(
    is.numeric(alpha_seq),
    msg = "N_seq must be a numeric vector"
  )

  # get selected stations
  stations_sfc <- stars::st_get_dimension_values(interpolator, "station")
  stations_coords <- sf::st_coordinates(stations_sfc)
  stations_elevation <- interpolator[["elevation"]][1,]
  stations_length <- length(stations_sfc)

  # station vector filling. If null, all the indexes, if character, convert to indexes
  stations <- .station_indexes_converter(stations, interpolator)

  selected_stations <- rep(FALSE, stations_length)
  selected_stations[stations] <- TRUE

  # get parameters
  interpolator_params <- get_interpolation_params(interpolator)

  # MAE matrix
  mae_matrix <- matrix(
    0, nrow = length(N_seq), ncol = length(alpha_seq),
    dimnames = list(N_seq = as.character(N_seq), alpha_seq = as.character(alpha_seq))
  )

  # variable data
  variable_matrix <- switch(
    variable,
    "PrecipitationAmount" = t(interpolator[["Precipitation"]]),
    "PrecipitationEvent" = t(interpolator[["Precipitation"]] > 0),
    "DewTemperature" = t(.dewpointTemperatureFromRH(
      0.606 * interpolator[["MaxTemperature"]] +
        0.394 * interpolator[["MinTemperature"]],
      interpolator[["RelativeHumidity"]]
    )),
    t(interpolator[[variable]])
  )
  smoothed_matrix <- t(interpolator[["SmoothedPrecipitation"]])

  .verbosity_control(
    cli::cli_alert_info("Total number of stations: {.val {nrow(variable_matrix)}}"),
    verbose
  )

  stations_no_data <- rowSums(is.na(variable_matrix)) == ncol(variable_matrix)
  variable_matrix <- variable_matrix[!stations_no_data,]
  stations_coords <- stations_coords[!stations_no_data,]
  stations_elevation <- stations_elevation[!stations_no_data]
  selected_stations <- selected_stations[!stations_no_data]
  stations <- which(selected_stations)

  .verbosity_control(
    cli::cli_alert_info("Number of stations with available data: {.val {nrow(variable_matrix)}}"),
    verbose
  )

  # MAE assesment
  selected_variable_matrix <- variable_matrix[stations,]
  selected_stations_coords <- stations_coords[stations,]
  selected_stations_elevation <- stations_elevation[stations]

  .verbosity_control(
    cli::cli_alert_info("Number of stations used for MAE calculation: {.val {length(stations)}}"),
    verbose
  )

  .verbosity_control(
    cli::cli_alert_info("Number of parameters combinations to test: {.val {ncol(mae_matrix)*nrow(mae_matrix)}}"),
    verbose
  )

  min_mae <- 9999999.0
  min_i <- NA
  min_j <- NA

  .verbosity_control(
    cli::cli_alert_info(
      "Starting evaluation of parameter combinations for {.val {variable}}..."
    ),
    verbose
  )

  # super complicated triple loop. This could be improved but I don't know how??
  for (i in N_seq) {
    for (j in alpha_seq) {
      # create the results matrix (and add the dim names)
      predicted_variable_matrix <-
        matrix(0, nrow(selected_variable_matrix), ncol(selected_variable_matrix))
      dimnames(predicted_variable_matrix) <- dimnames(selected_variable_matrix)

      .verbosity_control(
        cli::cli_ul("Evaluating N: {.val {i}}, alpha: {.val {j}}..."),
        verbose
      )

      # and now loop for stations
      # We use 1:length(stations) instead of the stations numeric indexes, because
      # the indexes relates to the interpolator, with all the stations, but in this
      # case, the matrixes already have only the selected stations rows
      # BUT!!!, for some of the values in the functions we need the original station
      # index
      for (station in 1:length(stations)) {
        original_station_index <- stations[station]
        predicted_variable_matrix[station, ] <- switch(
          variable,
          "DewTemperature" = .interpolateTdewSeriesPoints(
            Xp = selected_stations_coords[station, 1],
            Yp = selected_stations_coords[station, 2],
            Zp = selected_stations_elevation[station],
            X = stations_coords[-original_station_index, 1],
            Y = stations_coords[-original_station_index, 2],
            Z = stations_elevation[-original_station_index],
            T = variable_matrix[-original_station_index, ],
            iniRp = interpolator_params$initial_Rp,
            alpha = j, N = i,
            iterations = interpolator_params$iterations
          ),
          "Precipitation" = .interpolatePrecipitationSeriesPoints(
            Xp = selected_stations_coords[station, 1],
            Yp = selected_stations_coords[station, 2],
            Zp = selected_stations_elevation[station],
            X = stations_coords[-original_station_index, 1],
            Y = stations_coords[-original_station_index, 2],
            Z = stations_elevation[-original_station_index],
            P = variable_matrix[-original_station_index, ],
            Psmooth = smoothed_matrix[-original_station_index, ],
            iniRp = interpolator_params$initial_Rp,
            alpha_event = j, alpha_amount = j,
            N_event = i, N_amount = i,
            iterations = interpolator_params$iterations,
            popcrit = interpolator_params$pop_crit,
            fmax = interpolator_params$f_max
          ),
          "PrecipitationAmount" = .interpolatePrecipitationSeriesPoints(
            Xp = selected_stations_coords[station, 1],
            Yp = selected_stations_coords[station, 2],
            Zp = selected_stations_elevation[station],
            X = stations_coords[-original_station_index, 1],
            Y = stations_coords[-original_station_index, 2],
            Z = stations_elevation[-original_station_index],
            P = variable_matrix[-original_station_index, ],
            Psmooth = smoothed_matrix[-original_station_index, ],
            iniRp = interpolator_params$initial_Rp,
            alpha_event = interpolator_params$alpha_PrecipitationEvent,
            alpha_amount = j,
            N_event = interpolator_params$N_PrecipitationEvent,
            N_amount = i,
            iterations = interpolator_params$iterations,
            popcrit = interpolator_params$pop_crit,
            fmax = interpolator_params$f_max
          ),
          "PrecipitationEvent" = .interpolatePrecipitationEventSeriesPoints(
            Xp = selected_stations_coords[station, 1],
            Yp = selected_stations_coords[station, 2],
            Zp = selected_stations_elevation[station],
            X = stations_coords[-original_station_index, 1],
            Y = stations_coords[-original_station_index, 2],
            Z = stations_elevation[-original_station_index],
            Pevent = variable_matrix[-original_station_index, ],
            iniRp = interpolator_params$initial_Rp,
            alpha = j,
            N = i,
            iterations = interpolator_params$iterations,
            popcrit = interpolator_params$pop_crit
          ),
          .interpolateTemperatureSeriesPoints(
            Xp = selected_stations_coords[station, 1],
            Yp = selected_stations_coords[station, 2],
            Zp = selected_stations_elevation[station],
            X = stations_coords[-original_station_index, 1],
            Y = stations_coords[-original_station_index, 2],
            Z = stations_elevation[-original_station_index],
            T = variable_matrix[-original_station_index, ],
            iniRp = interpolator_params$initial_Rp,
            alpha = j, N = i,
            iterations = interpolator_params$iterations
          )
        )
      }

      # Now we have the predicted matrix for each estation for a given combination
      # of N and alpha. Now is time to calculate MAE
      if (variable %in% c("MinTemperature", "MaxTemperature", "DewTemperature")) {
        mae_matrix[as.character(i), as.character(j)] <-
          mean(abs(predicted_variable_matrix - selected_variable_matrix), na.rm = TRUE)
      } else {
        denominator_1 <- 0
        denominator_2 <- 0
        numerator_1 <- 0
        numerator_2 <- 0
        for (col in 1:ncol(selected_variable_matrix)) {
          observed_prec <- selected_variable_matrix[, col]
          predicted_prec <- predicted_variable_matrix[!is.na(observed_prec), col]
          weight <- sum(!is.na(observed_prec))
          sum_observed <- sum(observed_prec, na.rm = TRUE)
          sum_predicted <- sum(predicted_prec, na.rm = TRUE)
          if (weight > 0) {
            numerator_1 = numerator_1 + abs(sum_predicted/weight - sum_observed/weight)
            denominator_1 = denominator_1 + 1
          }
        }
        for (row in 1:nrow(selected_variable_matrix)) {
          observed_prec <- selected_variable_matrix[row, ]
          predicted_prec <- predicted_variable_matrix[row, !is.na(observed_prec)]
          weight <- sum(!is.na(observed_prec))
          sum_observed <- sum(observed_prec, na.rm = TRUE)
          sum_predicted <- sum(predicted_prec, na.rm = TRUE)
          if (weight > 0) {
            numerator_2 = numerator_2 + abs(sum_predicted/weight - sum_observed/weight)
            denominator_2 = denominator_2 + 1
          }
        }

        mae_matrix[as.character(i), as.character(j)] <-
          sqrt((numerator_1/denominator_1) * (numerator_2/denominator_2))
      }

      if (mae_matrix[as.character(i), as.character(j)] < min_mae) {
        min_mae <- mae_matrix[as.character(i), as.character(j)]
        min_i <- i
        min_j <- j
        optimal_observed <- selected_variable_matrix
        optimal_predicted <- predicted_variable_matrix
      }
    }
  }

  .verbosity_control(
    cli::cli_alert_success("Calibration done: Minimum MAE: {.val {min_mae}}; N: {.val {min_i}}; alpha: {.val {min_j}}"),
    verbose
  )

  res <- list(
    MAE = mae_matrix, minMAE = min_mae,
    N = min_i, alpha = min_j,
    observed = optimal_observed,
    predicted = optimal_predicted
  )

  if (isTRUE(update_interpolation_params)) {
    params_names <- c("alpha_", "N_") |>
      paste0(variable)
    interpolator_params <- get_interpolation_params(interpolator)
    interpolator_params[[params_names[1]]] <- res[["alpha"]]
    interpolator_params[[params_names[2]]] <- res[["N"]]

    interpolator_updated <-
      set_interpolation_params(interpolator, interpolator_params)

    return(interpolator_updated)
  }

  return(res)
}
