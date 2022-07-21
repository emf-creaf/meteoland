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
# 1. Fixes
#     - add topo with sf DONE
#     - time spend in joining topo when big meteo. I think is fixed now as i was
#       multiplying the topo as not taking careof repeated rows DONE
#     - ensure joining topo get the correct joining DONE
#     - check for correct params (what happens if user calls debig instead of debug...)
#     - ensure topo is needed with a warning if meteo has topo and topo is added DONE
#     - fix NA logical which causes error in interpolator creation. If a variable
#       is present and is logical (all NA not NA_integer_ nor NA_real_) then
#       transform to NA_real_ DONE
#     - What happens when trying to read an nc object that is not an interpolator??
#       informative error please.



#### check_meteo ####
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

has_topo_names <- function(meteo) {
  # topology names
  mandatory_topo_names <- c("elevation")

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

has_topo <- function(topo, ...) {

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

with_meteo <- function(meteo) {
  usethis::ui_info("Checking meteorology object...")
  assertthat::assert_that(has_meteo(meteo))
  usethis::ui_done("meteorology object ok")
  return(invisible(meteo))
}

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
      stars_interpolator[["Precipitation"]],
      params$St_Precipitation,
      TRUE
    )
  colnames(stars_interpolator[["SmoothedPrecipitation"]]) <-
    colnames(stars_interpolator[["Precipitation"]])
  attributes(stars_interpolator[["SmoothedPrecipitation"]]) <-
    attributes(stars_interpolator[["Precipitation"]])

  stars_interpolator[["SmoothedTemperatureRange"]] <-
    .temporalSmoothing(
      stars_interpolator[["MaxTemperature"]] - stars_interpolator[["MinTemperature"]],
      params$St_TemperatureRange,
      FALSE
    )
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
.write_interpolator <- function(interpolator, filename, .overwrite = FALSE) {
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

# read interpolators created with .write_interpolator
.read_interpolator <- function(filename) {
  # debug
  # browser()

  # get the data from the nc file
  ts_data <- ncdfgeom::read_timeseries_dsg(filename)

  # get the attributes
  interpolation_attributes <- ncmeta::nc_meta(filename)$attribute |>
    dplyr::filter(
      variable == "NC_GLOBAL",
      name %in% names(meteoland::defaultInterpolationParams())
    ) |>
    dplyr::pull(value)
  # remember to convert debug value to logical
  interpolation_attributes$debug <- as.logical(interpolation_attributes$debug)

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


## C vectorized version
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
  slorad <- sf$slope * (pi/180)
  asprad <- sf$aspect * (pi/180)
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

.meteospain2meteoland <- function(meteo) {

  # Renaming mandatory variables
  meteo_temp <- meteo |>
    dplyr::select(
      dates = timestamp, stationID = station_id,
      elevation = altitude,
      MinTemperature = min_temperature, MaxTemperature = max_temperature,
      Precipitation = precipitation,
      everything()
    )

  # Renaming optional
  if ("mean_relative_humidity" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(RelativeHumidity = mean_relative_humidity)
  }

  if ("mean_wind_speed" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(WindSpeed = mean_wind_speed)
  }

  if ("mean_wind_direction" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(WindDirection = mean_wind_direction)
  }

  if ("radiation" %in% names(meteo_temp)) {
    meteo_temp <- meteo_temp |>
      dplyr::rename(Radiation = radiation)
  }

  meteo_temp |>
    dplyr::select(dplyr::any_of(
      c(
        "dates", "stationID", "elevation", "MinTemperature", "MaxTemperature",
        "Precipitation", "RelativeHumidity", "WindDirection", "WindSpeed",
        "Radiation"
      )
    ))
}

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
    dplyr::select(elevation)

}

.interpolation_spatial_dispatcher <- function(
    spatial_data, interpolator, dates = NULL
) {

  # general assertions
  assertthat::assert_that(has_topo_names(spatial_data))

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

  sf::st_crs(binded_result) <- sf::st_crs(spatial_data)

  binded_result[["elevation"]] <- spatial_data[["elevation"]]
  usethis::ui_done("Interpolation process finished")
  return(binded_result)

}

interpolate_data <- function(spatial_data, interpolator, dates = NULL) {
  # debug
  # browser()

  ## assertions
  # spatial
  assertthat::assert_that(.is_spatial_data(spatial_data))
  if (inherits(spatial_data, 'stars')) {
    assertthat::assert_that(.is_raster(spatial_data))
    # curvilinear
    assertthat::assert_that(
      !attr(stars::st_dimensions(spatial_data), "raster")$curvilinear,
      msg = "Curvilinear grids are not supported yet. Please reproject/warp the raster to regular coordinates."
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

  # Interpolation
  res <-
    .interpolation_spatial_dispatcher(spatial_data, interpolator, dates) |>
    # results binding
    .binding_interpolation_results(spatial_data)

  return(res)
}




library(meteospain)
library(sf)
#### get meteo
# service_options <- aemet_options(
#   'daily', as.Date("2022-04-01"), as.Date("2022-04-30"),
#   api_key = keyring::key_get('aemet')
# )
#
# meteo_test <- get_meteo_from('aemet', service_options)
# # stations_info_test <- get_stations_info_from('aemet', service_options)
#
# meteo_test
#
# meteo_test |>
#   meteoland:::has_meteo()
#
# foo <- meteo_test |>
#   dplyr::rename(
#     elevation = altitude,
#     MinTemperature = min_temperature,
#     MaxTemperature = max_temperature,
#     stationID = station_id
#   )
#
# foo |>
#   meteoland:::has_meteo()
#
# bar <- foo |>
#   dplyr::rename(
#     dates = timestamp
#   )
#
# baz <- bar |>
#   dplyr::mutate(
#     dates = as.character(dates)
#   )
#
# xyz <- bar |>
#   dplyr::mutate(
#     MinTemperature = as.character(MinTemperature)
#   )
#
# xyzz <- bar |>
#   dplyr::mutate(elevation = as.character(elevation))
#
# fooz <- bar |>
#   dplyr::select(-stationID)
#
# meteoland:::with_meteo(meteo_test) # error meteo vars
# meteoland:::with_meteo(foo) # error no dates
# meteoland:::with_meteo(fooz) # error no stationID
# meteoland:::with_meteo(baz) # error bad dates
# meteoland:::with_meteo(xyz) # error bad meteo vars
# meteoland:::with_meteo(xyzz) # No error with bad topo, because we dont check topo
# meteoland:::with_meteo(bar) # no error, everything ok
#
#
# ## add topo tests ##
#
#
# meteo_ok <- bar |>
#   dplyr::select(-elevation)
#
# topo_tibble_ok <- bar |>
#   dplyr::as_tibble() |>
#   dplyr::select(
#     stationID, elevation
#   ) |>
#   dplyr::mutate(aspect = NA_integer_, slope = NA_integer_) |>
#   dplyr::distinct()
#
# topo_sf_ok <- bar |>
#   dplyr::select(
#     stationID, elevation
#   ) |>
#   dplyr::distinct() |>
#   dplyr::mutate(aspect = NA_integer_, slope = NA_integer_)
#
# topo_no_stations <- topo_tibble_ok |>
#   dplyr::select(-stationID)
#
# topo_bad_elevation <- topo_tibble_ok |>
#   dplyr::mutate(elevation = as.character(elevation))
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_no_stations) # error because no stationID
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_bad_elevation) # error beacuse elevation is not numeric
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(as.matrix(topo_bad_elevation)) # error because topo is not a tibble
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_tibble_ok) # ok
#
# meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_sf_ok) # ok for topos with sf, but geometries get replicated
#
# ## interpolator creation ##
# interpolator <-
#   meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_tibble_ok) |>
#   meteoland:::create_meteo_interpolator()
#
# attr(interpolator, 'params')
#
#
# # interpolator |>
# #   dplyr::filter(date > as.Date("2022-04-25"))
# #
# # interpolator |>
# #   dplyr::filter(date > as.Date("2022-04-25")) |>
# #   magrittr::extract("MinTemperature", 1, 1)
# #
# # interpolator[["MinTemperature"]][interpolator[["stationID"]] == "1059X"]
#
#
# sf_test <- topo_sf_ok |> dplyr::distinct()
# # tictoc::tic()
# # meteoland:::.interpolation_point(interpolator, sf_test)
# # tictoc::toc()
# #
# # tictoc::tic()
# # interpolated_points_high_points_high_dates <- meteoland:::.interpolation_point(interpolator, sf_test)
# # tictoc::toc()
# #
# # tictoc::tic()
# # interpolated_points_purrr_high_points_high_dates <-
# #   purrr::map(1:nrow(sf_test), ~meteoland:::.interpolation_point(interpolator, sf_test[.x,]))
# # tictoc::toc()
# #
# # tictoc::tic()
# # interpolated_points_low_points_high_dates <- meteoland:::.interpolation_point(interpolator, sf_test[1:5,])
# # tictoc::toc()
# #
# # tictoc::tic()
# # interpolated_points_purrr_low_points_high_dates <-
# #   purrr::map(1:nrow(sf_test[1:5,]), ~meteoland:::.interpolation_point(interpolator, sf_test[1:5,][.x,]))
# # tictoc::toc()
# #
# dates_test <- stars::st_get_dimension_values(interpolator, 'date')[1:5]
# #
# # tictoc::tic()
# # interpolated_points_high_points_low_dates <- meteoland:::.interpolation_point(interpolator, sf_test, dates = dates_test)
# # tictoc::toc()
# #
# # tictoc::tic()
# # interpolated_points_purrr_high_points_low_dates <-
# #   purrr::map(1:nrow(sf_test), ~meteoland:::.interpolation_point(interpolator, sf_test[.x,], dates = dates_test))
# # tictoc::toc()
# #
# # tictoc::tic()
# # interpolated_points_low_points_low_dates <- meteoland:::.interpolation_point(interpolator, sf_test[1:5,], dates = dates_test)
# # tictoc::toc()
# #
# # tictoc::tic()
# # interpolated_points_purrr_low_points_low_dates <-
# #   purrr::map(1:nrow(sf_test[1:5,]), ~meteoland:::.interpolation_point(interpolator, sf_test[1:5,][.x,], dates = dates_test))
# # tictoc::toc()
#
#
# # bench::mark(
# #   # meteoland:::.interpolation_point(interpolator, sf_test),
# #   # purrr::map(1:nrow(sf_test), ~meteoland:::.interpolation_point(interpolator, sf_test[.x,])),
# #   meteoland:::.interpolation_point(interpolator, sf_test[1:5,]),
# #   purrr::map(1:nrow(sf_test[1:5,]), ~meteoland:::.interpolation_point(interpolator, sf_test[1:5,][.x,])),
# #   meteoland:::.interpolation_point(interpolator, sf_test, dates = dates_test),
# #   purrr::map(1:nrow(sf_test), ~meteoland:::.interpolation_point(interpolator, sf_test[.x,], dates = dates_test)),
# #   meteoland:::.interpolation_point(interpolator, sf_test[1:5,], dates = dates_test),
# #   purrr::map(1:nrow(sf_test[1:5,]), ~meteoland:::.interpolation_point(interpolator, sf_test[1:5,][.x,], dates = dates_test)),
# #   iterations = 10,
# #   check = FALSE
# # )
# # foo <- bench::mark(
# #   meteoland:::.interpolation_point_recursive(interpolator, sf_test),
# #   meteoland:::.interpolation_point_purrr(interpolator, sf_test),
# #   meteoland:::.interpolation_point_recursive(interpolator, sf_test[1:5,]),
# #   meteoland:::.interpolation_point_purrr(interpolator, sf_test[1:5,]),
# #   meteoland:::.interpolation_point_recursive(interpolator, sf_test, dates = dates_test),
# #   meteoland:::.interpolation_point_purrr(interpolator, sf_test, dates = dates_test),
# #   meteoland:::.interpolation_point_recursive(interpolator, sf_test[1:5,], dates = dates_test),
# #   meteoland:::.interpolation_point_purrr(interpolator, sf_test[1:5,], dates = dates_test),
# #   iterations = 10,
# #   check = FALSE
# # )
# # foo$time
# #
# # recursive_profmem <- profmem::profmem({meteoland:::.interpolation_point_recursive(interpolator, sf_test[1:5,], dates = dates_test)})
# # purrr_profmem <- profmem::profmem({meteoland:::.interpolation_point_purrr(interpolator, sf_test[1:5,], dates = dates_test)})
# #
# # pryr::mem_change({meteoland:::.interpolation_point_recursive(interpolator, sf_test[1:5,], dates = dates_test)})
# # pryr::mem_change({meteoland:::.interpolation_point_purrr(interpolator, sf_test[1:5,], dates = dates_test)})
# #
# pryr::mem_change({recursive_res <- meteoland:::.interpolation_point_recursive(interpolator, sf_test)})
# pryr::mem_change({purrr_res <- meteoland:::.interpolation_point_purrr(interpolator, sf_test)})

# library(meteoland)
# library(meteospain)
# library(sf)
# #### get meteo
# service_options <- aemet_options(
#   'daily', as.Date("2022-04-01"), as.Date("2022-04-30"),
#   api_key = keyring::key_get('aemet')
# )
#
# meteo_ok <- get_meteo_from('aemet', service_options) |>
#   dplyr::rename(
#     dates = timestamp, elevation = altitude, stationID = station_id,
#     MinTemperature = min_temperature, MaxTemperature = max_temperature,
#     Precipitation = precipitation, WindSpeed = mean_wind_speed
#   )
#
# topo_sf_ok <- meteo_ok |>
#   dplyr::select(
#     stationID, elevation
#   ) |>
#   dplyr::distinct() |>
#   dplyr::mutate(aspect = NA_real_, slope = NA_real_)
#
# meteo_ok <- dplyr::select(meteo_ok, -elevation)
#
# interpolator <- meteoland:::with_meteo(meteo_ok) |>
#   meteoland:::add_topo(topo_sf_ok) |>
#   meteoland:::create_meteo_interpolator() |>
#   meteoland:::.write_interpolator(filename = 'interpolator_test.nc', .overwrite = TRUE)
#
# stars_interpolator <- meteoland:::.read_interpolator(filename = 'interpolator_test.nc')
#
# all.equal(interpolator, stars_interpolator)
# identical(interpolator, stars_interpolator)
# identical(interpolator$aspect, stars_interpolator$aspect)
# class(interpolator$aspect[,1])
# class(stars_interpolator$aspect[,1])
#
# points_test <- dplyr::sample_n(meteo_ok, 5) |>
#   meteoland:::with_meteo() |>
#   meteoland:::add_topo(topo_sf_ok)
#
# meteoland:::.interpolation_point(points_test, stars_interpolator)
#
# points_whole <- meteo_ok |>
#   dplyr::arrange(dates, stationID) |>
#   dplyr::slice(1:240) |>
#   dplyr::distinct() |>
#   meteoland:::with_meteo() |>
#   meteoland:::add_topo(topo_sf_ok)
# #
# points_test <- dplyr::sample_n(points_whole, 15)
# #
# selected_dates <- stars::st_get_dimension_values(interpolator, "date")[5:10]
# #
# long_benchmark <- bench::mark(
#   # long
#   meteoland:::.interpolation_point_recursive(interpolator, points_whole),
#   meteoland:::.interpolation_point(interpolator, points_whole),
#   iterations = 3,
#   check = FALSE
# )
# long_benchmark$time
# middle_benchmark <- bench::mark(
#   # middle
#   meteoland:::.interpolation_point_recursive(interpolator, points_test),
#   meteoland:::.interpolation_point(interpolator, points_test),
#   iterations = 10,
#   check = FALSE
# )
# middle_benchmark$time
# middle_2_benchmark <- bench::mark(
#   # middle_2
#   meteoland:::.interpolation_point_recursive(interpolator, points_whole, dates = selected_dates),
#   meteoland:::.interpolation_point(interpolator, points_whole, dates = selected_dates),
#   iterations = 3,
#   check = FALSE
# )
# middle_2_benchmark$time
# short_benchmark <- bench::mark(
#   # short
#   meteoland:::.interpolation_point_recursive(interpolator, points_test, dates = selected_dates),
#   meteoland:::.interpolation_point(interpolator, points_test, dates = selected_dates),
#   iterations = 3,
#   check = FALSE
# )
# short_benchmark$time
#
# recursive_res <- meteoland:::.interpolation_point_recursive(interpolator, points_test)
# c_vec_res <- meteoland:::.interpolation_point(interpolator, points_test)
#
# equality <- character()
# for (i in 1:length(recursive_res)) {
#   equality <- c(equality,all.equal(recursive_res[[i]], c_vec_res[[i]]))
# }
# equality
# all.equal(recursive_res, c_vec_res)

# library(meteoland)
# interpolator <- meteoland:::.read_interpolator('interpolator_test.nc')
# stars_test <- stars::read_ncdf('stars_test.nc') |>
#   purrr::set_names('elevation')
# # rast_test <- raster::raster('stars_test.nc') |>
# #   raster::projectRaster(crs = sf::st_crs(stars_test))
# # names(rast_test) <- "elevation"
#
#
# sf_test <- sf::st_as_sf(stars_test, as_points = TRUE)
# dates_test <- stars::st_get_dimension_values(interpolator, "date")[5:9]
# meteoland:::interpolate_data(stars_test, interpolator, dates = dates_test)
#
# rast_benchmark <- bench::mark(
#   res_rast_big <- meteoland:::interpolate_data(stars_test, interpolator),
#   res_sf_big <- meteoland:::interpolate_data(sf_test, interpolator),
#   iterations = 10,
#   check = FALSE
# )
# rast_benchmark_short <- bench::mark(
#   res_rast_short <- meteoland:::interpolate_data(stars_test, interpolator, dates = dates_test),
#   res_sf_short <- meteoland:::interpolate_data(sf_test, interpolator, dates = dates_test),
#   iterations = 10,
#   check = FALSE
# )
# rast_benchmark$time
# rast_benchmark_short$time

