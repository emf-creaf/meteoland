#### TODO  list
### 1. ~transpose interpolator, dates must be rows, stations must be cols~ DONE
###
### 1. implement ncdfgeom write method to save the interpolator as nc with attributes DONE
###     - units DONE
###
# 1. add crs conversion in the interpolation point method.
#     - Implemented a check in .interpolation_point but a conversion must be done
#     before calling this method.
#
# 1. implement ncdfgeom read method to read interpolators saved to file
#     - write method has to be changed to create stationID as last attribute
#     - create interpolator method must be changed to attribute arrays to be
#       col named with station ID. this will get rid of the need to create
#       an attribute in the stars with the stationID. write interpolator method
#       must be changed accordingly.
#
# 1. interpolation process
#     - .interpolation_points method DONE
#     - messages (user informed user happy)
#     - interpolation dispatcher (different spatial formats as inputs)
#       - crs conversion
#
# 1. helper to transform meteospain output to meteo input



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
  res <- dplyr::left_join(meteo, dplyr::as_tibble(topo), by = 'stationID')
  usethis::ui_done("topology added")

  return(res)
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
  if (is.null(params)) {
    usethis::ui_warn("No interpolation parameters provided, using defaults")
    params <- defaultInterpolationParams()
  }

  # data preparation
  meteo_arranged <- meteo_with_topo |>
    dplyr::arrange(stationID, dates)

  stations <- meteo_arranged |>
    dplyr::select(stationID) |>
    dplyr::distinct() |>
    sf::st_geometry()
  dates <- unique(meteo_arranged$dates)

  interpolator_dims <- stars::st_dimensions(
    date = dates, station = stations
  )

  # helper
  .interpolator_arrays_creator <- function(variable, arranged_data, dims) {
    rearranged_data <- arranged_data |>
      # very important step, as we need all combinations to fill the arrays
      tidyr::complete(dates, stationID) |>
      dplyr::arrange(stationID, dates)
    col_names <- rearranged_data |>
      dplyr::pull(stationID) |>
      unique()

    if (! variable %in% names(arranged_data)) {
      array_data <- array(NA_real_, dim = dims)
    } else {
      array_data <- rearranged_data|>
        dplyr::pull(!!variable) |>
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
    ) %>%
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
  params$initial_Rp <- sf::st_distance(meteo_with_topo) |>
    as.numeric() |>
    mean(na.rm = TRUE)

  # set the params as an attribute of stars_interpolator
  attr(stars_interpolator, "params") <- params

  # return the interpolator
  usethis::ui_done("Interpolator created.")
  return(stars_interpolator)
}


## recursive version, not using it for the moment
.interpolation_point_recursive <- function(interpolator, sf, dates = NULL) {
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

  ## recursive call
  if (nrow(sf) > 1) {
    return(c(
      .interpolation_point_recursive(interpolator, sf |> dplyr::slice(1), dates = dates),
      .interpolation_point_recursive(interpolator, sf |> dplyr::slice(-1), dates = dates)
    ))
  }

  ## checks point
  interpolator_convex_hull <-
    stars::st_get_dimension_values(interpolator, "station") |>
    sf::st_union() |>
    sf::st_convex_hull()
  if (!sf::st_contains(interpolator_convex_hull, sf, sparse = FALSE)) {
    usethis::ui_warn("Point outside the convex hull of interpolation object")
  }

  ## extraction and calculations
  tmin <- .interpolateTemperatureSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[1,],
    T = t(dplyr::filter(interpolator, date %in% dates)[["MinTemperature"]]),
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
    T = t(dplyr::filter(interpolator, date %in% dates)[["MaxTemperature"]]),
    iniRp = attr(interpolator, "params")$initial_Rp,
    alpha = attr(interpolator, "params")$alpha_MaxTemperature,
    N = attr(interpolator, "params")$N_MaxTemperature,
    iterations = attr(interpolator, "params")$iterations,
    debug = attr(interpolator, "params")$debug
  )

  tmean <- 0.606*tmax+0.394*tmin

  prec <- .interpolatePrecipitationSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[1,],
    P = t(dplyr::filter(interpolator, date %in% dates)[["Precipitation"]]),
    Psmooth = dplyr::filter(interpolator, date %in% dates)[["SmoothedPrecipitation"]],
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

  # relative humidity, depends on if we have it or not
  # If we dont, estimate VP assuming that dew-point temperature is equal to Tmin
  if (all(is.na(interpolator[["RelativeHumidity"]]))) {
    rhmean <- .relativeHumidityFromMinMaxTemp(tmin, tmax)
    VP <- .temp2SVP(tmin) #kPA
    rhmax <- rep(100, length(rhmean))
    rhmin <- pmax(0, .relativeHumidityFromDewpointTemp(tmax, tmin))
  } else {
    TdewM <- .dewpointTemperatureFromRH(
      0.606 * t(dplyr::filter(interpolator, date %in% dates)[["MaxTemperature"]]) +
        0.394 * t(dplyr::filter(interpolator, date %in% dates)[["MinTemperature"]]),
      t(dplyr::filter(interpolator, date %in% dates)[["RelativeHumidity"]])
    )

    tdew <- .interpolateTdewSeriesPoints(
      Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
      Zp = sf$elevation,
      X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
      Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
      Z = interpolator$elevation[1,],
      T = TdewM,
      iniRp = attr(interpolator, "params")$initial_Rp,
      alpha = attr(interpolator, "params")$alpha_DewTemperature,
      N = attr(interpolator, "params")$N_DewTemperature,
      iterations = attr(interpolator, "params")$iterations,
      debug = attr(interpolator, "params")$debug
    )

    rhmean <- .relativeHumidityFromDewpointTemp(tmean, tdew)
    VP <- .temp2SVP(tdew) #kPa
    rhmax = pmin(100, .relativeHumidityFromDewpointTemp(tmin, tdew))
    rhmin = pmax(0, .relativeHumidityFromDewpointTemp(tmax, tdew))
  }

  # radiation
  diffTemp <- abs(tmax - tmin)
  diffTempMonth <- .interpolateTemperatureSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[1,],
    T = t(dplyr::filter(interpolator, date %in% dates)[["SmoothedTemperatureRange"]]),
    iniRp = attr(interpolator, "params")$initial_Rp,
    alpha = attr(interpolator, "params")$alpha_MinTemperature,
    N = attr(interpolator, "params")$N_MinTemperature,
    iterations = attr(interpolator, "params")$iterations,
    debug = attr(interpolator, "params")$debug
  )

  latrad <- sf::st_coordinates(sf::st_transform(sf, 4326))[,2] * (pi/180)
  slorad <- sf$slope * (pi/180)
  asprad <- sf$aspect * (pi/180)
  rad <- .radiationSeries(
    latrad, sf$elevation, slorad, asprad, J, diffTemp, diffTempMonth, VP, prec
  )

  # Wind, (not sure if implement or not)
  # PET, not sure if implement or not


  ### TODO assign an id to each element, the sf geom or something!!!!

  # return the res df
  list(dplyr::tibble(
    dates = dates,
    DOY = DOY,
    MeanTemperature = as.vector(tmean),
    MinTemperature = as.vector(tmin),
    MaxTemperature = as.vector(tmax),
    Precipitation = as.vector(prec),
    MeanRelativeHumidity = rhmean,
    MinRelativeHumidity = rhmin,
    MaxRelativeHumidity = rhmax,
    Radiation = rad,
    WindSpeed = NA,
    WindDirection = NA,
    PET = NA
  ))

}

.interpolation_point_purrr <- function(interpolator, sf, dates = NULL) {
  ## assertions


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

  ## iterative call
  if (nrow(sf) > 1) {
    return(purrr::map(1:nrow(sf), ~ .interpolation_point_purrr(interpolator, sf[.x,], dates = dates)))
  }

  ## TODO, convert the coordinates of sf to the ones in the interpolator, store the
  ## old and transform the result if it is spatial

  ## checks point
  interpolator_convex_hull <-
    stars::st_get_dimension_values(interpolator, "station") |>
    sf::st_union() |>
    sf::st_convex_hull()
  if (!sf::st_contains(interpolator_convex_hull, sf, sparse = FALSE)) {
    usethis::ui_warn("Point outside the convex hull of interpolation object")
  }

  ## extraction and calculations
  tmin <- .interpolateTemperatureSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[,1],
    T = dplyr::filter(interpolator, date %in% dates)[["MinTemperature"]],
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
    Z = interpolator$elevation[,1],
    T = dplyr::filter(interpolator, date %in% dates)[["MaxTemperature"]],
    iniRp = attr(interpolator, "params")$initial_Rp,
    alpha = attr(interpolator, "params")$alpha_MaxTemperature,
    N = attr(interpolator, "params")$N_MaxTemperature,
    iterations = attr(interpolator, "params")$iterations,
    debug = attr(interpolator, "params")$debug
  )

  tmean <- 0.606*tmax+0.394*tmin

  prec <- .interpolatePrecipitationSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[,1],
    P = dplyr::filter(interpolator, date %in% dates)[["Precipitation"]],
    Psmooth = dplyr::filter(interpolator, date %in% dates)[["SmoothedPrecipitation"]],
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

  # relative humidity, depends on if we have it or not
  # If we dont, estimate VP assuming that dew-point temperature is equal to Tmin
  if (all(is.na(interpolator[["RelativeHumidity"]]))) {
    rhmean <- .relativeHumidityFromMinMaxTemp(tmin, tmax)
    VP <- .temp2SVP(tmin) #kPA
    rhmax <- rep(100, length(rhmean))
    rhmin <- pmax(0, .relativeHumidityFromDewpointTemp(tmax, tmin))
  } else {
    TdewM <- .dewpointTemperatureFromRH(
      0.606 * dplyr::filter(interpolator, date %in% dates)[["MaxTemperature"]] +
        0.394 * dplyr::filter(interpolator, date %in% dates)[["MinTemperature"]],
      dplyr::filter(interpolator, date %in% dates)[["RelativeHumidity"]]
    )

    tdew <- .interpolateTdewSeriesPoints(
      Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
      Zp = sf$elevation,
      X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
      Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
      Z = interpolator$elevation[,1],
      T = TdewM,
      iniRp = attr(interpolator, "params")$initial_Rp,
      alpha = attr(interpolator, "params")$alpha_DewTemperature,
      N = attr(interpolator, "params")$N_DewTemperature,
      iterations = attr(interpolator, "params")$iterations,
      debug = attr(interpolator, "params")$debug
    )

    rhmean <- .relativeHumidityFromDewpointTemp(tmean, tdew)
    VP <- .temp2SVP(tdew) #kPa
    rhmax = pmin(100, .relativeHumidityFromDewpointTemp(tmin, tdew))
    rhmin = pmax(0, .relativeHumidityFromDewpointTemp(tmax, tdew))
  }

  # radiation
  diffTemp <- abs(tmax - tmin)
  diffTempMonth <- .interpolateTemperatureSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[,1],
    T = dplyr::filter(interpolator, date %in% dates)[["SmoothedTemperatureRange"]],
    iniRp = attr(interpolator, "params")$initial_Rp,
    alpha = attr(interpolator, "params")$alpha_MinTemperature,
    N = attr(interpolator, "params")$N_MinTemperature,
    iterations = attr(interpolator, "params")$iterations,
    debug = attr(interpolator, "params")$debug
  )

  latrad <- sf::st_coordinates(sf::st_transform(sf, 4326))[,2] * (pi/180)
  slorad <- sf$slope * (pi/180)
  asprad <- sf$aspect * (pi/180)
  rad <- .radiationSeries(
    latrad, sf$elevation, slorad, asprad, J, diffTemp, diffTempMonth, VP, prec
  )

  # Wind, (not sure if implement or not)
  # PET, not sure if implement or not


  ### TODO assign an id to each element, the sf geom or something!!!!

  # return the res df
  dplyr::tibble(
    dates = dates,
    DOY = DOY,
    MeanTemperature = as.vector(tmean),
    MinTemperature = as.vector(tmin),
    MaxTemperature = as.vector(tmax),
    Precipitation = as.vector(prec),
    MeanRelativeHumidity = rhmean,
    MinRelativeHumidity = rhmin,
    MaxRelativeHumidity = rhmax,
    Radiation = rad,
    WindSpeed = NA,
    WindDirection = NA,
    PET = NA
  )

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
          lats = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
          lons = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
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

.read_interpolator <- function(filename) {

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

# library(meteospain)
# library(sf)
# #### get meteo ####
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

#
# library(meteospain)
# library(sf)
# #### get meteo ####
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
