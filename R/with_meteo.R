#### TODO  list
### 1. ~transpose interpolator, dates must be rows, stations must be cols~ DONE
###
### 1. implement ncdfgeom write method to save the interpolator as nc with attributes DONE
###     - units DONE
###
### 1. implement ncdfgeom read method to read interpolators saved to file
###     - write method has to be changed to create stationID as last attribute DONE
###     - create interpolator method must be changed to attribute arrays to be
###       col named with station ID. this will get rid of the need to create
###       an attribute in the stars with the stationID. write interpolator method
###       must be changed accordingly. DONE
###     - review process, as stars has a as_stars.ncdfgeom method, maybe is more
###       efficient to use that. DONE: Not implemented, it seems that I need to
###       review how i write the interpolator. For the moment it stays as it is
### 1. helper to transform meteospain output to meteo input DONE
###
### 1. BUG!!!
###     - In .interpolator_arrays_creator, when completing the cases, some needed
###       variables, mostly elevation creates NA for the missing cases. This
###       generates problems later on. Fix!!! DONE
### 1. add crs conversion in the interpolation point method.
###     - Implemented a check in .interpolation_point but a conversion must be done
###     before calling this method. DONE
### 1. interpolation process
###     - .interpolation_points method DONE
###     - messages (user informed user happy) DONE
###     - ensure!!! that the points or whatever the user has supplied as input
###       has all the needed (elevation ie) DONE
###     - ensure that lat long and alt are present in the interpolator creation,
###       filter any station without those values DONE
###     - interpolation dispatcher (different spatial formats as inputs)
###       - crs conversion DONE
###     - interpolation general function DONE
###     - transform from raster(stars) to sf: .stars2sf DONE
### 1. Fixes
###     - mean_* variables not getting retrieved in meteospain2meteoland DONE
###     - distance calculation is done with repeated points, causing allocation
###       of big vectors and time wasting. Fix it. DONE
### 1. Fixes
###     - add topo with sf DONE
###     - time spend in joining topo when big meteo. I think is fixed now as i was
###       multiplying the topo as not taking careof repeated rows DONE
###     - ensure joining topo get the correct joining DONE
###     - check for correct params (what happens if user calls debig instead of debug...)
###     - ensure topo is needed with a warning if meteo has topo and topo is added DONE
###     - fix NA logical which causes error in interpolator creation. If a variable
###       is present and is logical (all NA not NA_integer_ nor NA_real_) then
###       transform to NA_real_ DONE
###     - Precip and Rad not interpolated correctly. Possible causes
###         - ~~transposition of smoothed vars~~ Not this
###         - creation of smoothed vars DONE
###         - differences of 1e-05 in correct vars is due to differences in Rp parameter
###         - Precip, some values interpolate to 0 instead of the value in Miquels method.
###             - This is SOLVED. I was doing the precip interpolation incorrectly
###               (some params were missing). DONE
###     - In raster method, aspect and slope, if they exist, are not copied to
###       results object DONE
###     - What happens when trying to read an nc object that is not an interpolator??
###       informative error please. DONE
###     - Informative warning when no aspect or slope is registered, as the results
###       can be really different DONE
# 1. Fixes
#     - Humidity interpolation is wrong when no humidity in meteo is supplied.
#       I can't really test this because bug in old method. .interpolationPointSeries
#       only check if interpolator@RelativeHumidity is NULL, but it can't be NULL ever,
#       as the class check forces it to be a NA's matrix :(
#     - Check with Miquel which are the mandatory variables for meteo (only temperatures?
#       or also precipitation)
# 1. Add wind logic to interpolation process
# 1. Add interpolation cross validation and calibration routines
#    (maybe temporal resolution also)
#     - calibration process. DONE
#     - cross validation
#       - logic DONE
#       - messaging (remove interpolation messages and add custom ones for the
#         cross validation routine) DONE
#       - check results with old method
# 1. Add complete_meteo method
#     - add complete_meteo method DONE
#     - update meteospain2meteoland method
#       - catch all variables
#       - add subdaily = FALSE subroutine. (if is subdaily, aggregate)
#     - add tests DONE



#### check_meteo ####
#' Check if meteo data has the mandatory variables
#'
#' Check if meteo data has the mandatory variables
#'
#' This function ensures that meteo object have the mandatory variables, and is
#' used in combination with \code{\link[assertthat]{on_failure}} to produce a
#' meaningful and human readable error when no mandatory variables are present.
#'
#' @param meteo meteo object
#'
#' @return TRUE if mandatory variables are present, FALSE otherwise
#'
#' @family Argument checks
#' @noRd
has_meteo_names <- function(meteo) {
  # meteo names
  mandatory_meteo_names <- c("MinTemperature", "MaxTemperature")

  all(mandatory_meteo_names %in% names(meteo))
}

assertthat::on_failure(has_meteo_names) <- function(call, env) {
  paste0(
    "Names found in ", deparse(call$meteo), " don't comply with the required names:\n",
    deparse(call$meteo),
    " should have the following meteorology variables:\n",
    "  - MinTemperature ***\n", "  - MaxTemperature ***\n",
    "  - Precipitation\n", "  - RelativeHumidity\n",
    "  - Radiation\n", "  - WindSpeed\n", "  - WindDirection\n",
    "\n ***: mandatory variables"
  )
}

#' Check if meteo data has the mandatory topology variables
#'
#' Check if meteo data has the mandatory topology variables
#'
#' This function ensures that meteo object have the mandatory topology names,
#' and is used in combination with \code{\link[assertthat]{on_failure}} to produce
#' a meaningful and human readable error when no mandatory variables are present.
#'
#' @param meteo meteo object
#'
#' @return TRUE if mandatory variables are present, FALSE otherwise
#'
#' @family Argument checks
#' @noRd
has_topo_names <- function(meteo) {
  # topology names
  mandatory_topo_names <- c("elevation")
  # check
  all(mandatory_topo_names %in% names(meteo))
}

assertthat::on_failure(has_topo_names) <- function(call, env) {
  paste0(
    "Names found in ", deparse(call$meteo), " don't comply with the required names:\n",
    deparse(call$meteo),
    " should have the following topology variables:\n",
    "  - elevation ***\n",
    "  - aspect\n", "  - slope\n",
    "\n ***: mandatory variables\n"
  )
}

#' Checks for meteo
#'
#' Checks for meteo
#'
#' This function checks that meteo object has everything is needed to create the
#' interpolator. Checks done include being a sf POINT object, correct variables,
#' numeric variables, dates and meteo stations names.
#'
#' @param meteo meteo object
#'
#' @family Argument checks
#'
#' @return Informative error when any check fails, invisible TRUE otherwise
#' @noRd
has_meteo <- function(meteo) {
  # sf object
  assertthat::assert_that(inherits(meteo, 'sf'), msg = "meteo must be an sf object")
  assertthat::assert_that(all(sf::st_is(meteo, 'POINT')), msg = "meteo station geometries must be POINT")

  # names
  assertthat::assert_that(has_meteo_names(meteo))

  # number when it matters
  assertthat::assert_that(
    is.numeric(meteo[["MinTemperature"]]),
    is.numeric(meteo[["MaxTemperature"]])
  )

  # dates
  assertthat::assert_that(
    "dates" %in% names(meteo),
    msg = "meteo must have a variable called dates with the interpolation time span"
  )
  assertthat::assert_that(
    assertthat::is.date(meteo$dates) || assertthat::is.time(meteo$dates),
    msg = "dates variable must be a date or time class"
  )

  assertthat::assert_that(
    "stationID" %in% names(meteo),
    msg = "meteo must have a stationID variable identifying the meteorological stations"
  )
}

#' Checks for topo
#'
#' Checks for topo
#'
#' This function checks that topo object has everything is needed to create the
#' interpolator. Checks done include being a dataframe, correct variables,
#' numeric variables, and stations names.
#'
#' @param topo topo object
#'
#' @family Argument checks
#'
#' @return Informative error when any check fails, invisible TRUE otherwise
#' @noRd
has_topo <- function(topo) {

  # dataframe or similar with station_id or geometry
  assertthat::assert_that(
    inherits(topo, 'data.frame'), msg = "topo must be a data.frame/tibble"
  )

  # var names
  assertthat::assert_that(has_topo_names(topo))

  # numeric what it has to be numeric
  assertthat::assert_that(is.numeric(topo[["elevation"]]))

  assertthat::assert_that(
    "stationID" %in% names(topo),
    msg = "topo must have a stationID variable identifying the meteorological stations"
  )
}

#' Ensure meteo object is ready to create an interpolator object
#'
#' Check integrity of meteo objects
#'
#' This function is the first step in the creation of a meteoland interpolator,
#' ensuring the meteo provided contains all the required elements
#'
#' @param meteo meteo object
#'
#' @return invisible meteo object ready to pipe in the interpolator creation
#' @family interpolator functions
#' @export
with_meteo <- function(meteo) {
  usethis::ui_info("Checking meteorology object...")
  assertthat::assert_that(has_meteo(meteo))
  usethis::ui_done("meteorology object ok")
  return(invisible(meteo))
}

#' Add topology data to meteo object
#'
#' Add topology data to meteo object
#'
#' When using meteo data without topology info to create an inteprolator,
#' topology must be addded
#'
#' @param meteo meteo object
#' @param topo topo object
#' @return meteo with the topology info added
#' @family interpolator functions
#' @export
add_topo <- function(meteo, topo) {

  assertthat::assert_that(has_meteo(meteo))
  usethis::ui_info("Checking topology object...")
  assertthat::assert_that(has_topo(topo))
  usethis::ui_done("topology object ok")

  usethis::ui_info("Adding topology to meteo (by station ID)...")

  # check if meteo has topo already
  if (any(c("elevation", "aspect", "slope") %in% names(meteo))) {
    usethis::ui_warn(
      "Topology variables found in the meteo object.\nThey will be ignored as a new topology is provided."
    )
    meteo <- meteo |>
      dplyr::select(!dplyr::any_of(c("elevation", "aspect", "slope")))
  }

  res <- dplyr::left_join(
    meteo,
    # ensure topo is a tibble, with unique rows
    topo |>
      dplyr::as_tibble() |>
      dplyr::select(
        dplyr::any_of(c("stationID", "elevation", "aspect", "slope"))
      ) |>
      dplyr::distinct(),
    by = 'stationID'
  ) |>
    sf::st_as_sf()
  # res <- dplyr::left_join(meteo, dplyr::as_tibble(topo), by = 'stationID')
  usethis::ui_done("topology added")

  return(res)
}

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
#'
#' @return A complete parameter list to use in the interpolator object
#' @family interpolator functions
#' @noRd
.get_params <- function(params) {
  if (is.null(params)) {
    usethis::ui_warn("No interpolation parameters provided, using defaults")
    return(defaultInterpolationParams())
  }

  default_params <- defaultInterpolationParams()
  user_params <- params[names(params) %in% names(default_params)]

  if (length(user_params) < length(default_params)) {
    usethis::ui_info("Some interpolation parameters are missing, using default values for those")
  }

  if (length(params[!names(params) %in% names(default_params)]) > 0) {
    offending_params <- names(
      params[!names(params) %in% names(default_params)]
    )
    usethis::ui_warn(
      "The following interpolation parameters were provided and will not be used:"
    )
    purrr::walk(offending_params, usethis::ui_todo)
  }

  for (name in names(user_params)) {
    default_params[[name]] <- user_params[[name]]
  }

  return(default_params)
}

#' Meteoland interpolator creation
#'
#' Function to create the meteoland interpolator
#'
#' This function takes meteorology information and a list of interpolation
#' parameters and creates the interpolator object to be ready to use.
#'
#' @param meteo_with_topo meteo object, as returned by \code{\link{with_meteo}}
#' @return an interpolator object (stars)
#' @family interpolator functions
#' @export
create_meteo_interpolator <- function(meteo_with_topo, params = NULL, ...) {

  ## TODO messaging
  assertthat::assert_that(has_meteo(meteo_with_topo))
  assertthat::assert_that(has_topo(meteo_with_topo))

  assertthat::assert_that(
    is.null(params) || inherits(params, "list"),
    msg = "params must be NULL or a named list with the interpolation parameters"
  )

  usethis::ui_info("Creating interpolator...")

  # get params
  params <- .get_params(params)

  # data preparation

  # elevation filtering
  if (any(is.na(meteo_with_topo[["elevation"]]))) {
    usethis::ui_warn(
      "Some meteo stations lack values for elevation, filtering those stations out"
    )

    meteo_with_topo <- meteo_with_topo |>
      dplyr::filter(!is.na(elevation))

    if (nrow(meteo_with_topo) < 1) {
      usethis::ui_stop("No elevation values for any station, stopping creation of the interpolator")
    }

  }

  # helper to avoid NAs in elevation when completing cases
  .fill_elevation <- function(arranged_data) {
    arranged_data |>
      dplyr::group_by(stationID) |>
      dplyr::mutate(
        elevation = dplyr::if_else(
          is.na(elevation),
          unique(purrr::keep(elevation, ~!is.na(.x))),
          elevation
        )
      ) |>
      dplyr::ungroup() |>
      sf::st_as_sf()
  }

  # data arranging
  meteo_arranged <- meteo_with_topo |>
    # very important step, as we need all combinations to fill the arrays
    tidyr::complete(dates, stationID, explicit = FALSE) |>
    dplyr::arrange(stationID, dates) |>
    .fill_elevation()

  # stations and dates
  stations <- meteo_arranged |>
    dplyr::filter(
      !sf::st_is_empty(!!dplyr::sym(attr(meteo_arranged, "sf_column")))
    ) |>
    dplyr::select(stationID) |>
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
      dplyr::pull(stationID) |>
      unique()

    if (! variable %in% names(arranged_data)) {
      array_data <- array(NA_real_, dim = dims)
    } else {
      variable_data <- arranged_data|>
        dplyr::pull(!!variable)
      # transform logical NAs to numeric NAs
      if (is.logical(variable_data)) {
        variable_data <- as.numeric(variable_data)
      }
      array_data <- variable_data |>
        array(dim = dims)
    }

    colnames(array_data) <- col_names

    return(array_data)
  }

  stars_interpolator <-
    list(
      MinTemperature = "MinTemperature",
      MaxTemperature = "MaxTemperature",
      RelativeHumidity = "RelativeHumidity",
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
  usethis::ui_todo("Calculating smoothed variables...")
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
  usethis::ui_todo("Updating intial_Rp parameter with the actual stations mean distance...")
  params$initial_Rp <-
    sf::st_geometry(meteo_arranged) |>
    unique() |>
    sf::st_as_sfc() |>
    sf::st_distance() |>
    as.numeric() |>
    mean(na.rm = TRUE)

  # set the params as an attribute of stars_interpolator
  attr(stars_interpolator, "params") <- params

  # return the interpolator
  usethis::ui_done("Interpolator created.")
  return(stars_interpolator)
}

# write the interpolator in the NetCDF-CF standard
# http://cfconventions.org/cf-conventions/cf-conventions.html
#' Write the interpolator object
#'
#' Write the interpolator object to a file
#'
#' This function writes the interpolator object created with
#' \code{\link{create_meteoland_interpolator}} in a NetCDF-CF standard compliant
#' format, as specified in
#' http://cfconventions.org/cf-conventions/cf-conventions.html
#'
#' @param interpolator meteoland interpolator object, as created by
#'   \code{\link{create_meteoland_interpolator}}
#' @param filename file name for the interpolator nc file
#' @param .overwrite logical indicating if the file should be overwrited if it
#'   already exists
#'
#' @family interpolator functions
#' @return invisible interpolator object, to allow using this function as a step
#'   in a pipe
#' @export
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
      MinTemperature = 'celsius', MaxTemperature = 'celsius',
      RelativeHumidity = 'percent', Precipitation = 'mm',
      Radiation = 'MJ/m2', WindDirection = 'degree', WindSpeed = 'm/s',
      elevation = 'm', aspect = 'degree', slope = 'degree',
      SmoothedPrecipitation = 'mm', SmoothedTemperatureRange = 'celsius'
    )) |>
    purrr::set_names(names(prepared_data_list))



  interpolator_attributes <- attr(interpolator, "params")
  interpolator_attributes[["debug"]] <-
    dplyr::if_else(interpolator_attributes[["debug"]], 1, 0)

  ## write logic
  usethis::ui_info("Creating nc file following the NetCDF-CF conventions:")
  usethis::ui_line("http://cfconventions.org/cf-conventions/cf-conventions.html")

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
            crs = as.character(sf::st_crs(interpolator))[1]
          )
        ),
        add_to_existing = TRUE
      )
    )

  usethis::ui_info("Adding spatial info to nc file")
  filename <- ncdfgeom::write_geometry(
    nc_file = filename,
    geom_data = sf::st_as_sf(stars::st_get_dimension_values(interpolator, "station")),
    variables = c("instance_name", "time", "lat", "lon", names(prepared_data_list))
  )

  usethis::ui_done("Done")
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
#'
#' @return an interpolator (stars) object
#' @family interpolator functions
#' @export
read_interpolator <- function(filename) {
  # debug
  # browser()

  # get the attributes
  interpolation_attributes <- ncmeta::nc_meta(filename)$attribute |>
    dplyr::filter(
      variable == "NC_GLOBAL",
      name %in% names(meteoland::defaultInterpolationParams())
    ) |>
    dplyr::pull(value)

  # Check if attributes exist, if not is not an interpolator
  if (length(interpolation_attributes) < 1) {
    usethis::ui_stop("{filename} is not a meteoland interpolator file.")
  }

  # remember to convert debug value to logical
  interpolation_attributes$debug <- as.logical(interpolation_attributes$debug)

  # get the data from the nc file
  ts_data <- ncdfgeom::read_timeseries_dsg(filename)

  # get the geometries
  geom_crs <- ncmeta::nc_meta(filename)$attribute |>
    dplyr::filter(variable == "NC_GLOBAL", name == 'crs') |>
    dplyr::pull(value)
  geom_data <- ncdfgeom::read_geometry(filename) |>
    sf::st_transform(crs = as.numeric(gsub("^EPSG:", "", geom_crs)))

  # get the dates
  dates <- ts_data$time

  # build the interpolator object
  interpolator_dims <- stars::st_dimensions(
    date = dates,
    station = sf::st_geometry(geom_data)
  )

  .array_creation_helper <- function(dataframe, interpolator_dims) {
    array_data <- dataframe |>
      as.matrix() |>
      array(dim = dim(interpolator_dims))

    colnames(array_data) <- names(dataframe)
    return(array_data)
  }

  stars_interpolator <-
    ts_data$data_frames |>
    purrr::map(.array_creation_helper, interpolator_dims = interpolator_dims) |>
    stars::st_as_stars(dimensions = interpolator_dims)

  attr(stars_interpolator, 'params') <- interpolation_attributes

  return(stars_interpolator)

}


#' Interpolation core function
#'
#' Interpolation core process
#'
#' This function takes the points to be interpolated, the interpolator object
#' and an optional vector of dates to perform the interpolation. It uses the
#' vectorised C++ methods, so no need to loop through points or dates.
#'
#' @param sf sf object with the points topology
#' @param interpolator interpolator object
#' @param dates vector with dates (dates must be inside the interpolator date
#'   range)
#'
#' @family interpolator functions
#' @return A tibble for each point, with the dates as rows and the interpolated
#'   data as columns
#'
#' @noRd
.interpolation_point <- function(sf, interpolator, dates = NULL) {
  ## debug
  # browser()

  ## assertions
  # crs assertion. CRS of sf must be changed in the parent env before calling
  # .interpolation_point method, but we check here just in case
  assertthat::assert_that(
    identical(sf::st_crs(interpolator), sf::st_crs(sf)),
    msg = "CRS of sf must be the same as the interpolator"
  )

  ## dates checks
  if (is.null(dates)) {
    dates <- stars::st_get_dimension_values(interpolator, 'date')
  } else {
    dates_ref <- stars::st_get_dimension_values(interpolator, 'date')
    dates_inside <- dates[dates %in% dates_ref]

    # No dates in range
    if (length(dates_inside) < 1) {
      usethis::ui_stop(
        "Dates supplied are outside the interpolator date range. No possible interpolation."
      )
    }

    # some dates in range, not all
    if (length(dates_inside) < length(dates)) {
      usethis::ui_warn(
        "Some dates are outside the interpolator date range, only dates inside will be used"
      )
    }

    # set correct dates
    dates <- dates_inside
  }

  ## checks point
  interpolator_convex_hull <-
    stars::st_get_dimension_values(interpolator, "station") |>
    sf::st_union() |>
    sf::st_convex_hull()
  if (any(!sf::st_contains(interpolator_convex_hull, sf, sparse = FALSE))) {
    usethis::ui_warn("Some points are outside the convex hull of the interpolation object")
  }

  usethis::ui_info("Starting interpolation...")
  ## extraction and calculations
  # filtered interpolator, just once
  filtered_interpolator <- interpolator |> dplyr::filter(date %in% dates)
  tmin_interpolator <- t(filtered_interpolator[["MinTemperature"]])
  tmax_interpolator <- t(filtered_interpolator[["MaxTemperature"]])

  usethis::ui_todo("Interpolating temperature...")
  tmin <- .interpolateTemperatureSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[1,],
    T = tmin_interpolator,
    iniRp = attr(interpolator, "params")$initial_Rp,
    alpha = attr(interpolator, "params")$alpha_MinTemperature,
    N = attr(interpolator, "params")$N_MinTemperature,
    iterations = attr(interpolator, "params")$iterations,
    debug = attr(interpolator, "params")$debug
  )

  tmax <- .interpolateTemperatureSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[1,],
    T = tmax_interpolator,
    iniRp = attr(interpolator, "params")$initial_Rp,
    alpha = attr(interpolator, "params")$alpha_MaxTemperature,
    N = attr(interpolator, "params")$N_MaxTemperature,
    iterations = attr(interpolator, "params")$iterations,
    debug = attr(interpolator, "params")$debug
  )

  tmean <- 0.606*tmax+0.394*tmin

  usethis::ui_todo("Interpolating precipitation...")
  prec <- .interpolatePrecipitationSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[1,],
    P = t(filtered_interpolator[["Precipitation"]]),
    Psmooth = t(filtered_interpolator[["SmoothedPrecipitation"]]),
    iniRp = attr(interpolator, "params")$initial_Rp,
    alpha_event = attr(interpolator, "params")$alpha_PrecipitationEvent,
    alpha_amount = attr(interpolator, "params")$alpha_PrecipitationAmount,
    N_event = attr(interpolator, "params")$N_PrecipitationEvent,
    N_amount = attr(interpolator, "params")$N_PrecipitationAmount,
    iterations = attr(interpolator, "params")$iterations,
    popcrit = attr(interpolator, "params")$pop_crit,
    fmax = attr(interpolator, "params")$f_max,
    debug = attr(interpolator, "params")$debug
  )

  DOY <- as.numeric(format(dates, "%j"))
  J <- radiation_dateStringToJulianDays(as.character(dates))

  .as_interpolator_res_array <- function(vector, ref_dim) {
    return(array(vector, dim = ref_dim))
  }

  usethis::ui_todo("Interpolating relative humidity...")
  # relative humidity, depends on if we have it or not
  # If we dont, estimate VP assuming that dew-point temperature is equal to Tmin
  if (all(is.na(filtered_interpolator[["RelativeHumidity"]]))) {
    rhmean <- .relativeHumidityFromMinMaxTemp(tmin, tmax) |>
      .as_interpolator_res_array(dim(tmin))
    VP <- .temp2SVP(tmin) |> #kPA
      .as_interpolator_res_array(dim(tmin))
    rhmax <- rep(100, length(rhmean)) |>
      .as_interpolator_res_array(dim(tmin))
    rhmin <- pmax(0, .relativeHumidityFromDewpointTemp(tmax, tmin)) |>
      .as_interpolator_res_array(dim(tmin))
  } else {
    TdewM <- .dewpointTemperatureFromRH(
      0.606 * filtered_interpolator[["MaxTemperature"]] +
        0.394 * filtered_interpolator[["MinTemperature"]],
      filtered_interpolator[["RelativeHumidity"]]
    )

    tdew <- .interpolateTdewSeriesPoints(
      Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
      Zp = sf$elevation,
      X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
      Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
      Z = interpolator$elevation[1,],
      T = t(TdewM),
      iniRp = attr(interpolator, "params")$initial_Rp,
      alpha = attr(interpolator, "params")$alpha_DewTemperature,
      N = attr(interpolator, "params")$N_DewTemperature,
      iterations = attr(interpolator, "params")$iterations,
      debug = attr(interpolator, "params")$debug
    )

    rhmean <- .relativeHumidityFromDewpointTemp(tmean, tdew) |>
      .as_interpolator_res_array(dim(tmin))
    VP <- .temp2SVP(tdew) |>
      .as_interpolator_res_array(dim(tmin)) #kPa
    rhmax = pmin(100, .relativeHumidityFromDewpointTemp(tmin, tdew)) |>
      .as_interpolator_res_array(dim(tmin))
    rhmin = pmax(0, .relativeHumidityFromDewpointTemp(tmax, tdew)) |>
      .as_interpolator_res_array(dim(tmin))
  }

  # radiation
  usethis::ui_todo("Interpolating radiation...")
  diffTemp <- abs(tmax - tmin)
  diffTempMonth <- .interpolateTemperatureSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[1,],
    T = t(filtered_interpolator[["SmoothedTemperatureRange"]]),
    iniRp = attr(interpolator, "params")$initial_Rp,
    alpha = attr(interpolator, "params")$alpha_MinTemperature,
    N = attr(interpolator, "params")$N_MinTemperature,
    iterations = attr(interpolator, "params")$iterations,
    debug = attr(interpolator, "params")$debug
  )

  latrad <- sf::st_coordinates(sf::st_transform(sf, 4326))[,2] * (pi/180)
  slorad <- sf[["slope"]] * (pi/180)
  asprad <- sf[["aspect"]] * (pi/180)
  rad <- array(dim = dim(tmin))
  for (i in 1:length(latrad)) {
    rad[i,] <- .radiationSeries(
      latrad[i], sf$elevation[i], slorad[i], asprad[i],
      J,
      diffTemp[i,], diffTempMonth[i,],
      VP[i,], prec[i,]
    )
  }

  # Wind, (not sure if implement or not)
  # PET, not sure if implement or not


  ### TODO assign an id to each element, the sf geom or something!!!!

  # return the res df
  res <- purrr::map(
    1:length(latrad),
    ~ dplyr::tibble(
      dates = dates,
      DOY = DOY,
      MeanTemperature = as.vector(tmean[.x,]),
      MinTemperature = as.vector(tmin[.x,]),
      MaxTemperature = as.vector(tmax[.x,]),
      Precipitation = as.vector(prec[.x,]),
      MeanRelativeHumidity = rhmean[.x,],
      MinRelativeHumidity = rhmin[.x,],
      MaxRelativeHumidity = rhmax[.x,],
      Radiation = rad[.x,],
      WindSpeed = NA,
      WindDirection = NA,
      PET = NA
    )
  )

  usethis::ui_done("Interpolation done...")

  return(res)
}

#' From meteospain to meteoland meteo objects
#'
#' Adapting meteospain meteo objects to meteoland meteo objects
#'
#' This function converts \code{meteospain} R package meteo objects to compatible
#' meteoland meteo objects by seelcting the needed variables and adapting the
#' names to comply with meteoland requirements.
#'
#' @param meteo meteospain meteo object
#' @param complete logical indicating if the meteo data missing variables
#'  should be calculated (if possible)
#' @return a compatible meteo object to use in meteoland
#' @export
meteospain2meteoland <- function(meteo, complete = FALSE) {

  # Renaming mandatory variables
  meteo_temp <- meteo |>
    units::drop_units() |>
    dplyr::select(
      dates = timestamp, stationID = station_id,
      elevation = altitude,
      MinTemperature = min_temperature, MaxTemperature = max_temperature,
      Precipitation = precipitation,
      everything()
    )

  # Renaming optional
  if ("mean_temperature" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(MeanTemperature = mean_temperature)
  }

  if ("mean_relative_humidity" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(RelativeHumidity = mean_relative_humidity)
  }

  if ("min_relative_humidity" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(MinRelativeHumidity = min_relative_humidity)
  }

  if ("max_relative_humidity" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(MaxRelativeHumidity = max_relative_humidity)
  }

  if ("mean_wind_speed" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(WindSpeed = mean_wind_speed)
  }

  if ("mean_wind_direction" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(WindDirection = mean_wind_direction)
  }

  # if ("radiation" %in% names(meteo_temp)) {
  #   meteo_temp <- meteo_temp |>
  #     dplyr::rename(Radiation = radiation)
  # }

  if ("solar_radiation" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(Radiation = solar_radiation)
  }

  if ("global_solar_radiation" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(Radiation = global_solar_radiation)
  }

  res <- meteo_temp |>
    dplyr::select(dplyr::any_of(
      c(
        "dates", "stationID", "elevation",
        "MeanTemperature", "MinTemperature", "MaxTemperature",
        "Precipitation",
        "RelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
        "WindDirection", "WindSpeed",
        "Radiation"
      )
    ))

  if (isTRUE(complete)) {
    res <- complete_meteo(res)
  }

  return(res)
}

#' Complete missing meteo variables
#'
#' Calculates missing meteo variables
#'
#' This function takes a meteo object (with meteoland names) and complete
#' any missing variable if it is possible
#'
#' @param meteo meteoland meteo data
#'
#' @return the same \code{meteo} data provided with the the variables completed
#'
#' @export
complete_meteo <- function(meteo) {

  # assertions
  assertthat::assert_that(has_meteo(meteo))
  assertthat::assert_that(has_topo(meteo))

  # This no!!! We need a function for each row
  .complete_relative_humidity <- function(RelativeHumidity, MinTemperature, MeanTemperature) {
    purrr::pmap_dbl(
      list(RelativeHumidity, MinTemperature, MeanTemperature),
      ~ dplyr::if_else(
        is.na(..1),
        max(min(100 * utils_saturationVP(..2) / utils_saturationVP(..3), 100), 0),
        ..1
      )
    )
  }

  .complete_min_relative_humidity <- function(MinRelativeHumidity, MinTemperature, MaxTemperature) {
    purrr::pmap_dbl(
      list(MinRelativeHumidity, MinTemperature, MaxTemperature),
      ~ dplyr::if_else(
        is.na(..1),
        max(min(100 * utils_saturationVP(..2) / utils_saturationVP(..3), 100), 0),
        ..1
      )
    )
  }

  .complete_max_relative_humidity <- function(MaxRelativeHumidity) {
    dplyr::if_else(
      is.na(MaxRelativeHumidity), 100, MaxRelativeHumidity
    )
  }

  .complete_radiation <- function(
    Radiation, dates, MinTemperature, MaxTemperature,
    MinRelativeHumidity, MaxRelativeHumidity,
    Precipitation,
    elevation, slope, aspect,
    geometry
  ) {
    julian_day <- purrr::map_int(
      dates,
      ~ radiation_julianDay(
        as.numeric(format(.x, "%Y")),
        as.numeric(format(.x, "%m")),
        as.numeric(format(.x, "%d"))
      )
    )
    solar_constant <- purrr::map_dbl(julian_day, radiation_solarConstant)
    declination <- purrr::map_dbl(julian_day, radiation_solarDeclination)
    average_vapour_pressure <- purrr::pmap_dbl(
      list(MinTemperature, MaxTemperature, MinRelativeHumidity, MaxRelativeHumidity),
      utils_averageDailyVP
    )
    slope_rad <- dplyr::if_else(
      is.na(slope), 0, slope*(pi/180)
    )
    aspect_rad <- dplyr::if_else(
      is.na(aspect), 0, aspect*(pi/180)
    )
    latitude_rad <-
      sf::st_coordinates(sf::st_transform(geometry, 4326))[,2] * (pi/180)

    diff_temp <- MaxTemperature - MinTemperature

    purrr::pmap_dbl(
      list(
        solar_constant, latitude_rad, elevation, slope_rad,
        aspect_rad, declination, diff_temp, diff_temp,
        average_vapour_pressure, Precipitation
      ),
      radiation_solarRadiation
    )
  }

  # browser()

  if (is.null(meteo[["MeanTemperature"]])) {
    meteo[["MeanTemperature"]] <- NA_real_
  }

  if (is.null(meteo[["RelativeHumidity"]])) {
    meteo[["RelativeHumidity"]] <- NA_real_
  }

  if (is.null(meteo[["MinRelativeHumidity"]])) {
    meteo[["MinRelativeHumidity"]] <- NA_real_
  }

  if (is.null(meteo[["MaxRelativeHumidity"]])) {
    meteo[["MaxRelativeHumidity"]] <- NA_real_
  }

  if (is.null(meteo[["Radiation"]])) {
    meteo[["Radiation"]] <- NA_real_
  }

  if (is.null(meteo[["aspect"]])) {
    meteo[["aspect"]] <- NA_real_
  }

  if (is.null(meteo[["slope"]])) {
    meteo[["slope"]] <- NA_real_
  }

  meteo_completed <- meteo |>
    dplyr::mutate(
      RelativeHumidity = .complete_relative_humidity(
        RelativeHumidity, MinTemperature, MeanTemperature
      ),
      MinRelativeHumidity = .complete_min_relative_humidity(
        MinRelativeHumidity, MinTemperature, MaxTemperature
      ),
      MaxRelativeHumidity = .complete_max_relative_humidity(MaxRelativeHumidity),
      Radiation = .complete_radiation(
        Radiation, dates, MinTemperature, MaxTemperature,
        MinRelativeHumidity, MaxRelativeHumidity,
        Precipitation,
        elevation, slope, aspect,
        sf::st_geometry(meteo)
      )
    )

  return(meteo_completed)
}


#' Topology raster (stars) cells to points
#'
#' Converting stars cells to a topology point object compatible with
#' .interpolation_point
#'
#' This function takes on a stars raster and check for mandatory variables,
#' returning an sf object
#'
#' @param stars topology stars object with at least elevation variable
#'
#' @return a sf object with raster cells converted to points (taking the center
#'   of the cell)
#'
#' @noRd
.stars2sf <- function(stars) {
  # assertions
  assertthat::assert_that(
    inherits(stars, "stars"), msg = "No stars class object detected"
  )
  assertthat::assert_that(
    "elevation" %in% names(stars),
    msg = "No elevation variable/attribute found in data"
  )

  stars |>
    sf::st_as_sf(as_points = TRUE, na.rm = FALSE) |>
    dplyr::select(dplyr::any_of(c("elevation", "slope", "aspect")))

}

#' Dispatcher of spatial data to interpolation points
#'
#' Dispatcher of different kinds (stars, sf) of spatial data to interpolation
#' point function
#'
#' This function takes the spatial data, checks its class (stars or sf), apply
#' some assertions specific of the class and pass the spatial data to the
#' \code{\linl{.interpolation_point}} function
#'
#' @param spatial_data stars or sf object with the spatial data
#' @param interpolator meteoland interpolator object, as created by
#'   \code{\link{create_meteoland_interpolator}}
#' @param dates vector with dates to interpolate (must be within the
#'   interpolator date range). Default to NULL (all dates present in the
#'   interpolator object)
#'
#' @return the same as \code{\link{.interpolation_point}} function.
#'
#' @noRd
.interpolation_spatial_dispatcher <- function(
    spatial_data, interpolator, dates = NULL
) {

  # sf
  if (inherits(spatial_data, "sf")) {
    # assertions
    assertthat::assert_that(
      inherits(sf::st_geometry(spatial_data), "sfc_POINT"),
      msg = paste0(
        "Spatial data must inhertis from sfc_POINT, not ",
        class(sf::st_geometry(spatial_data))[1]
      )
    )

    # interpolation
    return(
      spatial_data |>
        sf::st_transform(sf::st_crs(interpolator)) |>
        .interpolation_point(interpolator, dates)
    )

  }

  # stars
  if (inherits(spatial_data, 'stars')) {
    return(
      spatial_data |>
        .stars2sf() |>
        sf::st_transform(sf::st_crs(interpolator)) |>
        .interpolation_point(interpolator, dates)
    )
  }

  usethis::ui_stop("No compatible spatial data found. Spatial data must be an sf or a stars object")
}

#' Check class of provided spatial data
#'
#' Check class of provided spatial data
#'
#' This function checks the class of the provided spatial data is in the allowed
#' classes (stars and sf)
#'
#' @param spatial_data spatial data provided
#'
#' @return TRUE if spatial data is a stars or sf object, FALSE otherwise
#'
#' @noRd
.is_spatial_data <- function(spatial_data) {
  inherits(spatial_data, c('sf', 'stars'))
}


assertthat::on_failure(.is_spatial_data) <- function(call, env) {
  paste0(
    "Spatial data provided (", deparse(call$spatial_data), ")\n",
    "must be an sf object (for points) or an stars object (for raster and grids).\n",
    "Other spatial classes (from sp, terra... packages) must be converted first."
  )
}

#' Check if spatial data stars is a raster
#'
#' Check if spatial data stars is a raster
#'
#' This function checks if the provided stars object is a raster, not a vector
#' data cube (which is not allowed)
#'
#' @param spatial_data stars spatial data provided
#'
#' @return TRUE when spatial data is a raster, FALSE when is a vector data cube
#'
#' @noRd
.is_raster <- function(spatial_data) {
  spatial_dimension <-
    purrr::map_lgl(sf::st_coordinates(spatial_data), inherits, what = 'sfc') |>
    any()
  !spatial_dimension
}

assertthat::on_failure(.is_raster) <- function(call, env) {
  paste0(
    "stars object provided (", deparse(call$spatial_data), ")\n",
    "is a vector data cube. Please convert to sf points data or raster data\n",
    "before the interpolatation."
  )
}

#' Assertions to check if the object provided is an interpolator
#'
#' Assertions to check if the object provided is an interpolator
#'
#' This function checks all the mandatory characteristics of an interpolator
#' object (dimensions, parameters, variables).
#'
#' @param interpolator meteoland interpolator object, as created by
#'   \code{\link{create_meteoland_interpolator}}
#'
#' @return invisible TRUE if the object is a meteoland complying interpolator,
#'   an informative error otherwise
#'
#' @noRd
.is_interpolator <- function(interpolator) {
  has_params <- !is.null(attr(interpolator, "params"))
  has_correct_dimensions <-
    all(c('date', 'station') %in% names(stars::st_dimensions(interpolator)))

  assertthat::assert_that(
    has_params,
    msg = "interpolator object is missing the interpolation parameters attribute."
  )
  assertthat::assert_that(
    has_correct_dimensions,
    msg = "interpolator object is missing the correct dimensions (date and station)"
  )
  assertthat::assert_that(has_meteo_names(interpolator))
  assertthat::assert_that(has_topo_names(interpolator))
}

#' Binding interpolation results to spatial data provided
#'
#' Binding interpolation results to spatial data provided
#'
#' This function takes the results list of the interpolation and join it with
#' the provided spatial data (raster or sf)
#'
#' @param res_list results list as generated by \code{\link{.interpolation_point}}
#' @param spatial_data spatial data provided
#'
#' @return an object with the same class and structure as the provided spatial
#'   data with the results of the interpolation joined. In the case of spatial
#'   data being an sf, the results are added as a list-type column that can be
#'   unnested with \code{\link[tidyr]{unnest}}. In the case of a stars raster
#'   object, interpolation results are added as attributes (variables)
#'
#' @noRd
.binding_interpolation_results <- function(res_list, spatial_data) {
  # debug
  # browser()

  ## points
  if (inherits(spatial_data, 'sf')) {
    binded_result <-
      spatial_data |>
      dplyr::as_tibble() |>
      dplyr::mutate(interpolated_data = res_list) |>
      sf::st_as_sf()

    return(binded_result)
  }

  ## rasters
  # check on dimensions
  x_name_index <-
    which(
      names(stars::st_dimensions(spatial_data)) %in% c('x', 'X', 'long', 'longitude')
    )
  y_name_index <-
    which(
      names(stars::st_dimensions(spatial_data)) %in% c('y', 'Y', 'lat', 'latitude')
    )

  assertthat::assert_that(
    length(x_name_index) > 0,
    msg = "Can't find x dimension. x dimension in spatial data must be named 'x', 'X', 'long' or 'longitude'"
  )
  assertthat::assert_that(
    length(y_name_index) > 0,
    msg = "Can't find y dimension. y dimension in spatial data must be named 'y', 'Y', 'lat' or 'latitude'"
  )

  # dimensions
  result_dimensions <- stars::st_dimensions(
    x = stars::st_get_dimension_values(spatial_data, x_name_index),
    y = stars::st_get_dimension_values(spatial_data, y_name_index),
    date = res_list[[1]][['dates']]
  )

  # info
  usethis::ui_info("Binding together interpolation results")

  # helper
  .result_arrays_creator <- function(variable, res_list, dims) {
    variable_vecs <- purrr::map(res_list, ~ .x[[variable]])
    # array_dims <- c(y = dims[['y']], x = dims[['x']], date = dims[['date']])

    var_long_vec <- numeric()
    for (i in 1:length(variable_vecs[[1]])) {
      var_long_vec <- c(
        var_long_vec,
        variable_vecs |> purrr::map_dbl(~.x[i])
      )
    }
    var_long_vec |> array(dim = dims)
  }

  res_vars <- names(res_list[[1]]) |>
    purrr::keep(~ !.x %in% c("dates", "DOY"))
  binded_result <- res_vars |>
    purrr::map(~ .result_arrays_creator(.x, res_list, dim(result_dimensions))) |>
    purrr::set_names(res_vars) |>
    stars::st_as_stars(dimensions = result_dimensions)

  # crs
  sf::st_crs(binded_result) <- sf::st_crs(spatial_data)

  # user data, if slope and/or aspect are null, they will be null
  binded_result[["elevation"]] <- spatial_data[["elevation"]]
  binded_result[["slope"]] <- spatial_data[["slope"]]
  binded_result[["aspect"]] <- spatial_data[["aspect"]]

  usethis::ui_done("Interpolation process finished")
  return(binded_result)

}

#' Interpolation process for spatial data
#'
#' Interpolate spatial data to obtain downscaled meteorologic variables
#'
#' This function takes a spatial data object (sf or stars raster), an interpolator
#' object (\code{\link{create_meteoland_interpolator}}) and a vector of dates to
#' perform the interpolation of the meteorologic variables for the spatial
#' locations present in the \code{spatial_data} object.
#'
#' @section Spatial data:
#'   The spatial data provided must be of two types. (I) A sf object containing
#'   POINT for each location to interpolate or (II) a stars raster object for
#'   which the interpolation should be done.
#'   Independently of the class of \code{spatial_data} it has to have some
#'   mandatory variables, namely \code{elevation}. It should also contain
#'   \code{aspect} and \code{slope} for a better interpolation process, though
#'   this two variables are not mandatory.
#'
#' @section Curvilinear rasters:
#'   Rasters with curvilinear projections are not supported. Rasters provided
#'   must have a planar coordinate system.
#'
#' @param spatial_data An sf or stars raster object to interpolate
#' @param interpolator A meteoland interpolator object, as created by
#'   \code{\link{create_meteoland_interpolator}}
#' @param dates vector with dates to interpolate (must be within the
#'   interpolator date range). Default to NULL (all dates present in the
#'   interpolator object)
#'
#' @return an object with the same class and structure as the provided spatial
#'   data with the results of the interpolation joined. In the case of spatial
#'   data being an sf, the results are added as a list-type column that can be
#'   unnested with \code{\link[tidyr]{unnest}}. In the case of a stars raster
#'   object, interpolation results are added as attributes (variables)
#'
#' @export
interpolate_data <- function(spatial_data, interpolator, dates = NULL) {
  # debug
  # browser()

  ## assertions
  # spatial
  assertthat::assert_that(.is_spatial_data(spatial_data))
  assertthat::assert_that(has_topo_names(spatial_data))

  if (inherits(spatial_data, 'stars')) {
    assertthat::assert_that(.is_raster(spatial_data))
    # curvilinear
    assertthat::assert_that(
      !attr(stars::st_dimensions(spatial_data), "raster")$curvilinear,
      msg = "Curvilinear grids are not yet supported. Please reproject/warp the raster to regular coordinates."
    )
  }

  # dates
  if (!is.null(dates)) {
    assertthat::assert_that(
      assertthat::is.date(dates) | assertthat::is.time(dates),
      msg = "dates object provided doesn't contain dates (POSIX)"
    )
  }

  # interpolator
  assertthat::assert_that(.is_interpolator(interpolator))

  # last spatial check. Here because that way all essential checks are done
  # before
  optional_topo_names <- c("slope", "aspect")
  if (!all(optional_topo_names %in% names(spatial_data))) {
    usethis::ui_warn(
      "'aspect' and 'slope' variables are not mandatory, but the interpolation will be less accurate without them"
    )
  }

  # Interpolation
  res <-
    .interpolation_spatial_dispatcher(spatial_data, interpolator, dates) |>
    # results binding
    .binding_interpolation_results(spatial_data)

  return(res)
}

#' Calibration and validation of interpolation procedures
#'
#' Calibration and validation of interpolation procedures
#'
#' Function \code{interpolator_calibration} determines optimal interpolation
#' parameters \code{"N"} and \code{"alpha"} for a given meteorological variable.
#' Optimization is done by minimizing mean absolute error ("MAE")
#' (Thornton \emph{et al.} 1997). Function \code{interpolation_cross_validation}
#' calculates average mean absolute errors ("MAE") for the prediction period of
#' the interpolator object.
#' In both calibration and cross validation procdeures, predictions for each
#' meteorological station are made using a \emph{leave-one-out} procedure
#' (i.e. after exluding the station form the predictive set).
#'
#' @param interpolator A meteoland interpolator object, as created by
#'   \code{\link{create_meteoland_interpolator}}
#'
#' @param stations A vector with the station indexes (numeric) to be used to
#'   calculate \code{"MAE"}. All stations with data are included in the training
#'   set but predictive \code{"MAE"} are calculated for the stations subset
#'   indicated in \code{stations} param only.
#'
#' @param variable A string indicating the meteorological variable for which
#'   interpolation parameters \code{"N"} and \code{"alpha"} will be calibrated.
#'   Accepted values are \code{MinTemperature}, \code{MaxTemperature},
#'   \code{DewTemperature}, \code{Precipitation} (for precipitation with the
#'   same values for precipitation events an regression of precipitation amounts),
#'   \code{PrecipitationAmount} (for regression of precipitation amounts) and
#'   \code{PrecipitationEvent} (for precipitation events).
#'
#' @param N_seq Numeric vector with \code{"N"} values to be tested
#' @param alpha_seq Numeric vector with \code{"alpha"}
#'
#' @return \code{interpolator_calibration} returns a list with the following items
#' \itemize{
#'   \item{MAE: A numeric matrix with the mean absolute error values, averaged
#'   across stations, for each combination of parameters \code{"N"} and \code{"alpha"}}
#'   \item{minMAE: Minimum MAE value}
#'   \item{N: Value of parameter \code{"N"} corresponding to the minimum MAE}
#'   \item{alpha: Value of parameter \code{"alpha"} corresponding the the
#'   minimum MAE}
#'   \item{observed: matrix with observed values (meteorological measured values)}
#'   \item{predicted: matrix with interpolated values for the optimum parameter
#'   combination}
#' }
#'
#' @export
interpolator_calibration <- function(
    interpolator, stations = NULL,
    variable = c(
      "MinTemperature", "MaxTemperature", "DewTemperature",
      "Precipitation", "PrecipitationAmount", "PrecipitationEvent"
    ) ,
    N_seq = seq(5, 30, by = 5),
    alpha_seq = seq(0.25, 10, by = 0.25)
) {

  ### assertions
  # interpolator
  assertthat::assert_that(.is_interpolator(interpolator))
  # stations
  assertthat::assert_that(
    is.null(stations) || is.numeric(stations),
    msg = "stations must be NULL or a numeric vector with the stations indexes"
  )
  # variables
  assertthat::assert_that(
    is.character(variable), msg = "variable argument must be a character"
  )
  variable <- match.arg(variable)
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
  if (is.null(stations)) {
    stations <- 1:stations_length
  }
  selected_stations <- rep(FALSE, stations_length)
  selected_stations[stations] <- TRUE

  # get parameters
  interpolator_params <- attr(interpolator, 'params')

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

  usethis::ui_info("Total number of stations: {nrow(variable_matrix)}")

  stations_no_data <- rowSums(is.na(variable_matrix)) == ncol(variable_matrix)
  variable_matrix <- variable_matrix[!stations_no_data,]
  stations_coords <- stations_coords[!stations_no_data,]
  stations_elevation <- stations_elevation[!stations_no_data]
  selected_stations <- selected_stations[!stations_no_data]
  stations <- which(selected_stations)

  usethis::ui_info("Number of stations with available data: {nrow(variable_matrix)}")

  # MAE assesment
  selected_variable_matrix <- variable_matrix[stations,]
  selected_stations_coords <- stations_coords[stations,]
  selected_stations_elevation <- stations_elevation[stations]

  usethis::ui_info("Number of stations used for MAE calculation: {length(stations)}")

  usethis::ui_info(
    "Number of parameters combinations to test: {ncol(mae_matrix)*nrow(mae_matrix)}"
  )

  min_mae <- 9999999.0
  min_i <- NA
  min_j <- NA

  usethis::ui_info(
    "Starting evaluation of parameter combinations for {variable}..."
  )

  # super complicated triple loop. This could be improved but I don't know how??
  for (i in N_seq) {
    for (j in alpha_seq) {
      # create the results matrix
      predicted_variable_matrix <-
        matrix(0, nrow(selected_variable_matrix), ncol(selected_variable_matrix))

      usethis::ui_todo("Evaluating N: {i}, alpha: {j}...")

      # and now loop for stations
      for (station in stations) {
        predicted_variable_matrix[station, ] <- switch(
          variable,
          "DewTemperature" = .interpolateTdewSeriesPoints(
            Xp = selected_stations_coords[station, 1],
            Yp = selected_stations_coords[station, 2],
            Zp = selected_stations_elevation[station],
            X = stations_coords[-station, 1],
            Y = stations_coords[-station, 2],
            Z = stations_elevation[-station],
            T = variable_matrix[-station, ],
            iniRp = interpolator_params$initial_Rp,
            alpha = j, N = i,
            iterations = interpolator_params$iterations
          ),
          "Precipitation" = .interpolatePrecipitationSeriesPoints(
            Xp = selected_stations_coords[station, 1],
            Yp = selected_stations_coords[station, 2],
            Zp = selected_stations_elevation[station],
            X = stations_coords[-station, 1],
            Y = stations_coords[-station, 2],
            Z = stations_elevation[-station],
            P = variable_matrix[-station, ],
            Psmooth = smoothed_matrix[-station, ],
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
            X = stations_coords[-station, 1],
            Y = stations_coords[-station, 2],
            Z = stations_elevation[-station],
            P = variable_matrix[-station, ],
            Psmooth = smoothed_matrix[-station, ],
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
            X = stations_coords[-station, 1],
            Y = stations_coords[-station, 2],
            Z = stations_elevation[-station],
            Pevent = variable_matrix[-station, ],
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
            X = stations_coords[-station, 1],
            Y = stations_coords[-station, 2],
            Z = stations_elevation[-station],
            T = variable_matrix[-station, ],
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

  usethis::ui_done(
    "Minimum MAE: {min_mae}; N: {min_i}; alpha: {min_j}"
  )

  res <- list(
    MAE = mae_matrix, minMAE = min_mae,
    N = min_i, alpha = min_j,
    observed = optimal_observed,
    predicted = optimal_predicted
  )

  return(res)
}

#' Safely removing a station from the interpolator object
#'
#' Safely remove a station from the interpolator object for cross validation
#' processes
#'
#' This function removes the desired stations from the interpolator while
#' maintaining interpolation parameters attribute
#'
#' @param interpolator meteoland interpolator object
#' @param station_index numeric index for the station to remove
#'
#' @return An interpolator object without the station provided
#'
#' @noRd
.remove_station_from_interpolator <- function(interpolator, station_index) {
  res <- interpolator[, , -station_index]
  attr(res, "params") <- attr(interpolator, "params")
  return(res)
}

#' Cross validation process for a single station
#'
#' Cross validation process for a singel station
#'
#' This function performs the cross validation process for a single stations,
#' removing the station from the interpolator and using the spatial station
#' info to simulate a spatial data object
#'
#' @param station_index Numeric value with the station index
#' @param interpolator meteoland interpolator object
#'
#' @return A list with the interpolation results for the desired station
#'
#' @noRd
.station_cross_validation <- function(station_index, interpolator) {

  # get the point sf with topology info
  station_sf <- dplyr::tibble(
    elevation = interpolator[["elevation"]][1, station_index],
    aspect = interpolator[["aspect"]][1, station_index],
    slope = interpolator[["slope"]][1, station_index],
    geometry = stars::st_get_dimension_values(interpolator, 'station')[station_index]
  ) |>
    sf::st_as_sf()

  # remove the station from the interpolator
  interpolator_station_removed <-
    .remove_station_from_interpolator(interpolator, station_index)

  # return the interpolation
  suppressMessages(suppressWarnings(
    .interpolation_point(station_sf, interpolator_station_removed)
  ))[[1]] |>
    dplyr::mutate(
      RangeTemperature = MaxTemperature - MinTemperature,
      station = station_index
    ) |>
    dplyr::rename(RelativeHumidity = MeanRelativeHumidity)
}

#' Transform interpolator values to df
#'
#' Transform interpolator values to a data frame
#'
#' This function transform the observed meteorological values in the interpolator
#' object to a data frame for the cross validation process
#'
#' @param interpolator meteoland interpolator object
#'
#' @return A data frame with the station index, dates and meteorological observed
#'   values
#'
#' @noRd
.interpolator2tibble <- function(interpolator) {
  purrr::map_dfr(
    1:length(stars::st_get_dimension_values(interpolator, 'station')),
    function(station_index) {
      res <-
        dplyr::tibble(dates = stars::st_get_dimension_values(interpolator, 'date'))
      for (variable in names(interpolator)) {
        res[[variable]] <- interpolator[[variable]][,station_index]
      }
      res |>
        dplyr::mutate(
          RangeTemperature = MaxTemperature - MinTemperature,
          station = station_index
        )
    }
  )
}

#' Helper to convert predicted values to NA
#'
#' Helper to convert predicted values to NA
#'
#' This function convert predicted values to NA where no observed values are
#' recorded, to allow for correct cross validation calculations
#'
#' @param predicted_df data frame with the predicted (interpolated) values
#' @param observed_df data frame with the observed values
#'
#' @return \code{predicted_df} data frame with NAs where the \code{observed_df}
#'   has NAs
#'
#' @noRd
.set_predicted_nas <- function(predicted_df, observed_df) {

  predicted_df$MinTemperature[is.na(observed_df$MinTemperature)] <- NA
  predicted_df$MaxTemperature[is.na(observed_df$MaxTemperature)] <- NA
  predicted_df$RangeTemperature[is.na(observed_df$RangeTemperature)] <- NA
  predicted_df$Precipitation[is.na(observed_df$Precipitation)] <- NA
  predicted_df$Radiation[is.na(observed_df$Radiation)] <- NA
  predicted_df$RelativeHumidity[is.na(observed_df$RelativeHumidity)] <- NA

  return(predicted_df)

}

#' Correlation between observed and predicted values
#'
#' Correlation between observed and predicted values
#'
#' This function return the correlation values for the provided meteorolgical
#' variable
#'
#' @param variable character with the variable name
#' @param predicted_df data frame with predicted values
#' @param observed_df data frame with observed values
#'
#' @return correlation value (numeric)
#'
#' @noRd
.cv_correlation <- function(variable, predicted_df, observed_df) {

  res <- NA

  if (sum(
    complete.cases(cbind(predicted_df[[variable]], observed_df[[variable]]))
  ) > 0) {
    res <-
      cor(predicted_df[[variable]], observed_df[[variable]], use = "complete.obs")
  }

  return(res)
}

#' Cross validation results processing
#'
#' Cross validation results processing
#'
#' This function creates the results elements for the cross validation returned
#' list
#'
#' @param predicted_df data frame with predicted values
#' @param observed_df data frame with observed values
#'
#' @return A list with the cross validation results (total errors, stations
#' stats, dates stats)
#'
#' @noRd
.cv_processing <- function(predicted_df, observed_df) {

  # calculate errors
  total_errors <- dplyr::tibble(
    dates = predicted_df[["dates"]],
    station = predicted_df[["station"]],
    MinTemperature_error =
      predicted_df[["MinTemperature"]] - observed_df[["MinTemperature"]],
    MaxTemperature_error =
      predicted_df[["MaxTemperature"]] - observed_df[["MaxTemperature"]],
    RangeTemperature_error =
      predicted_df[["RangeTemperature"]] - observed_df[["RangeTemperature"]],
    RelativeHumidity_error =
      predicted_df[["RelativeHumidity"]] - observed_df[["RelativeHumidity"]],
    Radiation_error =
      predicted_df[["Radiation"]] - observed_df[["Radiation"]],
    Precipiation_error =
      predicted_df[["Precipitation"]] - observed_df[["Precipitation"]]
  ) |>
    dplyr::mutate(
      MinTemperature_predicted = predicted_df[["MinTemperature"]],
      MaxTemperature_predicted = predicted_df[["MaxTemperature"]],
      RangeTemperature_predicted = predicted_df[["RangeTemperature"]],
      RelativeHumidity_predicted = predicted_df[["RelativeHumidity"]],
      Radiation_predicted = predicted_df[["Radiation"]],
      Precipitation_predicted = predicted_df[["Precipitation"]],
      MinTemperature_observed = observed_df[["MinTemperature"]],
      MaxTemperature_observed = observed_df[["MaxTemperature"]],
      RangeTemperature_observed = observed_df[["RangeTemperature"]],
      RelativeHumidity_observed = observed_df[["RelativeHumidity"]],
      Radiation_observed = observed_df[["Radiation"]],
      Precipitation_observed = observed_df[["Precipitation"]]
    )

  # bias and mae for stations
  station_stats <- total_errors |>
    dplyr::group_by(station) |>
    dplyr::summarise(
      MinTemperature_station_bias = mean(MinTemperature_error, na.rm = TRUE),
      MaxTemperature_station_bias = mean(MaxTemperature_error, na.rm = TRUE),
      RangeTemperature_station_bias = mean(RangeTemperature_error, na.rm = TRUE),
      RelativeHumidity_station_bias = mean(RelativeHumidity_error, na.rm = TRUE),
      Radiation_station_bias = mean(Radiation_error, na.rm = TRUE),
      MinTemperature_station_mae = mean(abs(MinTemperature_error), na.rm = TRUE),
      MaxTemperature_station_mae = mean(abs(MaxTemperature_error), na.rm = TRUE),
      RangeTemperature_station_mae = mean(abs(RangeTemperature_error), na.rm = TRUE),
      RelativeHumidity_station_mae = mean(abs(RelativeHumidity_error), na.rm = TRUE),
      Radiation_station_mae = mean(abs(Radiation_error), na.rm = TRUE),
      TotalPrecipitation_station_bias =
        sum(Precipitation_observed, na.rm = TRUE) -
        sum(Precipitation_predicted, na.rm = TRUE),
      TotalPrecipitation_station_relative_bias = 100*(
        sum(Precipitation_observed, na.rm = TRUE) -
          sum(Precipitation_predicted, na.rm = TRUE)
      ) / sum(Precipitation_observed, na.rm = TRUE),
      DaysPrecipitation_station_bias =
        sum(Precipitation_observed > 0, na.rm = TRUE) -
        sum(Precipitation_predicted > 0, na.rm = TRUE),
      DaysPrecipitation_station_relative_bias = 100*(
        sum(Precipitation_observed > 0, na.rm = TRUE) -
          sum(Precipitation_predicted > 0, na.rm = TRUE)
      ) / sum(Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_station_observed =
        mean(Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_station_predicted =
        mean(Precipitation_predicted > 0, na.rm = TRUE)
    )

  dates_stats <- total_errors |>
    dplyr::group_by(dates) |>
    dplyr::summarise(
      MinTemperature_date_bias = mean(MinTemperature_error, na.rm = TRUE),
      MaxTemperature_date_bias = mean(MaxTemperature_error, na.rm = TRUE),
      RangeTemperature_date_bias = mean(RangeTemperature_error, na.rm = TRUE),
      RelativeHumidity_date_bias = mean(RelativeHumidity_error, na.rm = TRUE),
      Radiation_date_bias = mean(Radiation_error, na.rm = TRUE),
      MinTemperature_date_mae = mean(abs(MinTemperature_error), na.rm = TRUE),
      MaxTemperature_date_mae = mean(abs(MaxTemperature_error), na.rm = TRUE),
      RangeTemperature_date_mae = mean(abs(RangeTemperature_error), na.rm = TRUE),
      RelativeHumidity_date_mae = mean(abs(RelativeHumidity_error), na.rm = TRUE),
      Radiation_date_mae = mean(abs(Radiation_error), na.rm = TRUE),
      TotalPrecipitation_date_bias =
        sum(Precipitation_observed, na.rm = TRUE) -
        sum(Precipitation_predicted, na.rm = TRUE),
      TotalPrecipitation_date_relative_bias = 100*(
        sum(Precipitation_observed, na.rm = TRUE) -
          sum(Precipitation_predicted, na.rm = TRUE)
      ) / sum(Precipitation_observed, na.rm = TRUE),
      DaysPrecipitation_date_bias =
        sum(Precipitation_observed > 0, na.rm = TRUE) -
        sum(Precipitation_predicted > 0, na.rm = TRUE),
      DaysPrecipitation_date_relative_bias = 100*(
        sum(Precipitation_observed > 0, na.rm = TRUE) -
          sum(Precipitation_predicted > 0, na.rm = TRUE)
      ) / sum(Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_date_observed =
        mean(Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_date_predicted =
        mean(Precipitation_predicted > 0, na.rm = TRUE)
    )

  return(list(
    errors = total_errors,
    station_stats = station_stats,
    dates_stats = dates_stats
  ))
}

#' @describeIn interpolator_calibration
#'
#' @return \code{interpolation_cross_validation} returns a list with the
#' following items
#' \itemize{
#'   \item{total_errors: Data frame with each combination of station and date with
#'   observed variables, predicated variables and the total error
#'   (predicted - observed) calculated for each variable}
#'   \item{station_stats: Data frame with error and bias statistics aggregated by
#'   station}
#'   \item{dates_stats: Data frame with error and bias statistics aggregated by
#'   date}
#'   \item{r2: correlation indexes between observed and predicted values for each
#'   meteorological variable}
#' }
#'
#' @export
interpolation_cross_validation <- function(interpolator, stations = NULL) {

  # debug
  # browser()

  # assertions
  assertthat::assert_that(.is_interpolator(interpolator))
  assertthat::assert_that(
    is.null(stations) || is.numeric(stations),
    msg = "stations must be NULL or a numeric vector with the stations indexes"
  )

  if (is.null(stations)) {
    stations <- 1:length(stars::st_get_dimension_values(interpolator, "station"))
  }

  usethis::ui_info("Starting Cross validation process...")
  observed_values <- .interpolator2tibble(interpolator) |>
    dplyr::filter(station %in% stations)

  usethis::ui_todo("interpolating stations...")
  predicted_values <-
    purrr::map_dfr(stations, .station_cross_validation, interpolator = interpolator) |>
    # set predicted to NA when observed is NA
    .set_predicted_nas(observed_values)

  ### Processing results
  usethis::ui_todo("calculating R squared...")
  r2_list <- c(
    MinTemperature = "MinTemperature", MaxTemperature = "MaxTemperature",
    RangeTemperature = "RangeTemperature", RelativeHumidity = "RelativeHumidity",
    Radiation = "Radiation"
  ) |>
    purrr::map(
      .cv_correlation,
      predicted_df = predicted_values, observed_df = observed_values
    )
  usethis::ui_todo("calculating errors, MAE and bias for interpolated variables...")
  res <- .cv_processing(predicted_values, observed_values)

  res$r2 <- r2_list

  usethis::ui_done("Cross validation done.")
  return(res)

}

