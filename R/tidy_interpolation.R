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
    iniRp = get_interpolation_params(interpolator)$initial_Rp,
    alpha = get_interpolation_params(interpolator)$alpha_MinTemperature,
    N = get_interpolation_params(interpolator)$N_MinTemperature,
    iterations = get_interpolation_params(interpolator)$iterations,
    debug = get_interpolation_params(interpolator)$debug
  )

  tmax <- .interpolateTemperatureSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    Zp = sf$elevation,
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    Z = interpolator$elevation[1,],
    T = tmax_interpolator,
    iniRp = get_interpolation_params(interpolator)$initial_Rp,
    alpha = get_interpolation_params(interpolator)$alpha_MaxTemperature,
    N = get_interpolation_params(interpolator)$N_MaxTemperature,
    iterations = get_interpolation_params(interpolator)$iterations,
    debug = get_interpolation_params(interpolator)$debug
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
    iniRp = get_interpolation_params(interpolator)$initial_Rp,
    alpha_event = get_interpolation_params(interpolator)$alpha_PrecipitationEvent,
    alpha_amount = get_interpolation_params(interpolator)$alpha_PrecipitationAmount,
    N_event = get_interpolation_params(interpolator)$N_PrecipitationEvent,
    N_amount = get_interpolation_params(interpolator)$N_PrecipitationAmount,
    iterations = get_interpolation_params(interpolator)$iterations,
    popcrit = get_interpolation_params(interpolator)$pop_crit,
    fmax = get_interpolation_params(interpolator)$f_max,
    debug = get_interpolation_params(interpolator)$debug
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
      iniRp = get_interpolation_params(interpolator)$initial_Rp,
      alpha = get_interpolation_params(interpolator)$alpha_DewTemperature,
      N = get_interpolation_params(interpolator)$N_DewTemperature,
      iterations = get_interpolation_params(interpolator)$iterations,
      debug = get_interpolation_params(interpolator)$debug
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
    T = abs(t(filtered_interpolator[["SmoothedTemperatureRange"]])),
    iniRp = get_interpolation_params(interpolator)$initial_Rp,
    alpha = get_interpolation_params(interpolator)$alpha_MinTemperature,
    N = get_interpolation_params(interpolator)$N_MinTemperature,
    iterations = get_interpolation_params(interpolator)$iterations,
    debug = get_interpolation_params(interpolator)$debug
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

  # Wind
  usethis::ui_todo("Interpolating wind...")
  wind <- .interpolateWindStationSeriesPoints(
    Xp = sf::st_coordinates(sf)[,1], Yp = sf::st_coordinates(sf)[,2],
    WS = t(filtered_interpolator[["WindSpeed"]]),
    WD = t(filtered_interpolator[["WindDirection"]]),
    X = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,1],
    Y = sf::st_coordinates(stars::st_get_dimension_values(interpolator, "station"))[,2],
    iniRp = get_interpolation_params(interpolator)$initial_Rp,
    alpha = get_interpolation_params(interpolator)$alpha_Wind,
    N = get_interpolation_params(interpolator)$N_Wind,
    iterations = get_interpolation_params(interpolator)$iterations
  )

  # PET, not sure if implement or not

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
      WindSpeed = as.vector(wind$WS[.x,]),
      WindDirection = as.vector(wind$WD[.x,]),
      PET = NA
    )
  )

  usethis::ui_done("Interpolation done...")

  return(res)
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

#' Interpolation process for spatial data
#'
#' Interpolate spatial data to obtain downscaled meteorologic variables
#'
#' This function takes a spatial data object (sf or stars raster), an
#' interpolator object (\code{\link{create_meteoland_interpolator}}) and a
#' vector of dates to perform the interpolation of the meteorologic variables
#' for the spatial locations present in the \code{spatial_data} object.
#'
#' @param spatial_data An sf or stars raster object to interpolate
#' @param interpolator A meteoland interpolator object, as created by
#' \code{\link{create_meteoland_interpolator}}
#' @param dates vector with dates to interpolate (must be within the
#' interpolator date range). Default to NULL (all dates present in the
#' interpolator object)
#' @return an object with the same class and structure as the provided spatial
#' data with the results of the interpolation joined. In the case of spatial
#' data being an sf, the results are added as a list-type column that can be
#' unnested with \code{\link[tidyr]{unnest}}. In the case of a stars raster
#' object, interpolation results are added as attributes (variables)
#' @section Spatial data:
#'
#' The spatial data provided must be of two types. (I) A sf object containing
#' POINT for each location to interpolate or (II) a stars raster object for
#' which the interpolation should be done. Independently of the class of
#' \code{spatial_data} it has to have some mandatory variables, namely
#' \code{elevation}. It should also contain \code{aspect} and \code{slope} for
#' a better interpolation process, though this two variables are not mandatory.
#' @export interpolate_data
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
