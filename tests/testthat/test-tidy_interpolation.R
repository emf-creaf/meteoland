# meteo test data, 1 month
meteo_test <- readRDS('meteo_test.rds') |>
  dplyr::filter(timestamp < (as.Date("2021-02-01") |> as.POSIXct()))
# raster, points data for testing
raster_data_test <- readRDS("raster_data.rds")
raster_data_test <- stars::st_warp(
  raster_data_test,
  stars::st_as_stars(sf::st_bbox(raster_data_test), dx = 3000)
  )
points_data_test <- readRDS("points_data.rds") |>
  dplyr::slice(1:10)
# interpolation varaibles names
interpolation_var_names <- c(
  "MeanTemperature", "MinTemperature", "MaxTemperature", "Precipitation",
  "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
  "Radiation", "WindSpeed", "WindDirection", "PET"
)

meteo_test_subdaily_with_errors <- readRDS('subdaily_with_errors.rds') |>
  units::drop_units()
meteo_test_non_unique_ids <- meteo_test
meteo_test_non_unique_ids$geometry[1] <- meteo_test_non_unique_ids$geometry[5000]

# worldmet2meteoland
test_that("worldmet2meteoland works", {

  skip_on_cran()

  # data
  worldmet_stations <- worldmet::getMeta(lat = 42, lon = 0, n = 10, plot = FALSE)
  worldmet_codes <- paste0(worldmet_stations$usaf, "-", worldmet_stations$wban)

  worldmet_subdaily_2022 <-
    worldmet::importNOAA(worldmet_codes, year = 2022, hourly = TRUE, n.cores = 6)


  expect_s3_class(test_res <- worldmet2meteoland(worldmet_subdaily_2022), 'sf')
  expect_true(
    all(c(
      "MeanTemperature", "MinTemperature", "MaxTemperature",
      "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "stationID", "dates", "elevation"
    ) %in% names(test_res))
  )

  meteo_test_no_dates <- worldmet_subdaily_2022 |> dplyr::select(-date)
  meteo_test_no_relative_humidity <- worldmet_subdaily_2022 |>
    dplyr::select(-RH)

  expect_error(
    worldmet2meteoland(meteo_test_no_dates),
    "Provided data has no date or code variables"
  )

  expect_s3_class(
    test_res_norh <- worldmet2meteoland(meteo_test_no_relative_humidity),
    "sf"
  )
  expect_true(
    all(c(
      "MinTemperature", "MaxTemperature", "Precipitation",
      "stationID", "dates"
    ) %in% names(test_res_norh))
  )

  expect_s3_class(
    test_complete_no_changes <- worldmet2meteoland(worldmet_subdaily_2022, complete = TRUE),
    "sf"
  )
  expect_identical(nrow(test_complete_no_changes), nrow(test_res))
  expect_identical(names(test_complete_no_changes), c(names(test_res), 'aspect', 'slope'))
  expect_identical(test_complete_no_changes$MinTemperature, test_res$MinTemperature)
  expect_false(
    identical(test_res$Radiation, test_complete_no_changes$Radiation)
  )


})

test_that("meteospain2meteoland works", {
  # daily
  expect_s3_class(test_res <- meteospain2meteoland(meteo_test), 'sf')
  expect_true(
    all(c(
      "MeanTemperature", "MinTemperature", "MaxTemperature",
      "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "stationID", "dates", "elevation"
    ) %in% names(test_res))
  )

  meteo_test_no_dates <- meteo_test |> dplyr::select(-timestamp)
  meteo_test_no_relative_humidity <- meteo_test |>
    dplyr::select(-mean_relative_humidity)

  expect_error(
    meteospain2meteoland(meteo_test_no_dates),
    "Provided data has no timestamp variable"
  )
  expect_s3_class(
    test_res_norh <- meteospain2meteoland(meteo_test_no_relative_humidity),
    "sf"
  )
  expect_true(
    all(c(
      "MinTemperature", "MaxTemperature", "Precipitation",
      "stationID", "dates"
    ) %in% names(test_res_norh))
  )

  expect_warning(
    meteo_test_fixed <- meteospain2meteoland(meteo_test_non_unique_ids),
    "Choosing the most recent metadata"
  )
  expect_identical(meteo_test_fixed, test_res)

  # complete checks

  expect_s3_class(
    test_complete_no_changes <- meteospain2meteoland(meteo_test, complete = TRUE),
    "sf"
  )
  expect_identical(nrow(test_complete_no_changes), nrow(test_res))
  expect_identical(names(test_complete_no_changes), c(names(test_res), 'aspect', 'slope'))
  expect_identical(test_complete_no_changes$MinTemperature, test_res$MinTemperature)
  expect_false(
    identical(test_res$MeanRelativeHumidity, test_complete_no_changes$MeanRelativeHumidity)
  )

  meteo_test_no_vars <- dplyr::mutate(
    meteo_test,
    mean_relative_humidity = NA_real_,
    min_relative_humidity = NA_real_,
    max_relative_humidity = NA_real_,
    global_solar_radiation = NA_real_
  )

  expect_s3_class(
    test_complete_all <- meteospain2meteoland(meteo_test_no_vars, complete = TRUE),
    "sf"
  )
  expect_identical(nrow(test_complete_all), nrow(test_res))
  expect_identical(names(test_complete_all), c(names(test_res), 'aspect', 'slope'))
  expect_identical(test_complete_all$MinTemperature, test_res$MinTemperature)
  expect_true(any(!is.na(test_complete_all$MeanRelativeHumidity)))
  expect_true(any(!is.na(test_complete_all$MinRelativeHumidity)))
  expect_true(all(!is.na(test_complete_all$MaxRelativeHumidity))) # is all because we put 100 in all
  expect_true(any(!is.na(test_complete_all$Radiation)))

  # subdaily checks
  expect_warning(
    .reshape_meteo(meteo_test_subdaily_with_errors, .meteospain_variables_dictionary(TRUE), FALSE),
    "Choosing the most recent metadata"
  )
  expect_message(
    suppressWarnings(
      .reshape_meteo(meteo_test_subdaily_with_errors, .meteospain_variables_dictionary(TRUE), FALSE)
    ),
    "Provided meteo data seems to be in subdaily time steps"
  )
  # expect_warning(
  #   (subdaily_fixed <- .fix_station_geometries(meteo_test_subdaily_with_errors)),
  #   "Choosing the most recent metadata"
  # )
  # expect_message(
  #   (subdaily_aggregated <- .aggregate_subdaily_meteospain(subdaily_fixed)),
  #   "Provided meteospain data seems to be in subdaily time steps"
  # )
  # expect_true(nrow(subdaily_fixed) > nrow(subdaily_aggregated))
  # expect_identical(
  #   sort(unique(subdaily_fixed$station_id)),
  #   sort(unique(subdaily_aggregated$station_id))
  # )

  # we expect a warning
  expect_warning(test_subdaily <- meteospain2meteoland(meteo_test_subdaily_with_errors))

  expect_s3_class(test_subdaily, 'sf')
  expect_true(
    all(c(
      "MeanTemperature", "MinTemperature", "MaxTemperature",
      "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "stationID", "dates", "elevation"
    ) %in% names(test_res))
  )

  expect_s3_class(
    suppressWarnings(test_subdaily_complete <- meteospain2meteoland(meteo_test_subdaily_with_errors, complete = TRUE)),
    "sf"
  )
  expect_identical(nrow(test_subdaily_complete), nrow(test_subdaily))
  expect_identical(names(test_subdaily_complete), c(names(test_subdaily), 'aspect', 'slope'))
  expect_identical(test_subdaily_complete$MinTemperature, test_subdaily$MinTemperature)
  expect_false(
    identical(test_subdaily$MeanRelativeHumidity, test_subdaily_complete$MeanRelativeHumidity)
  )


})

meteo_test_correct <- meteospain2meteoland(meteo_test)
# test_data
meteo_test_no_sf <- dplyr::as_tibble(meteo_test)
meteo_test_no_dates <- meteo_test_correct |> dplyr::select(-dates)
meteo_test_wrong_date_format <- meteo_test_correct |>
  dplyr::mutate(dates = as.character(dates))
meteo_test_no_id <- meteo_test_correct |> dplyr::select(-stationID)
meteo_test_no_numeric <- meteo_test_correct |>
  dplyr::mutate(MinTemperature = as.character(MinTemperature))
meteo_test_no_points <- meteo_test_correct |>
  dplyr::slice(1:10) |>
  sf::st_union() |>
  sf::st_as_sf()
meteo_test_correct_non_unique_ids <- meteo_test_correct
meteo_test_correct_non_unique_ids$geometry[1] <- meteo_test_correct_non_unique_ids$geometry[5000]

test_that("has_meteo checks work", {
  expect_true(has_meteo(meteo_test_correct))
  expect_error(has_meteo(meteo_test), "Names found in")
  expect_error(has_meteo(meteo_test_no_sf), "meteo must be an sf")
  expect_error(has_meteo(meteo_test_no_dates), "variable called dates")
  expect_error(has_meteo(meteo_test_wrong_date_format), "must be a date")
  expect_error(has_meteo(meteo_test_no_id), "stationID variable")
  expect_error(has_meteo(meteo_test_no_numeric), "MinTemperature")
  expect_error(has_meteo(meteo_test_no_points), "geometries must be")
  expect_error(has_meteo(meteo_test_correct_non_unique_ids), "There are more geometries")
})

topo_test_correct <- meteo_test_correct |>
  dplyr::mutate(aspect = NA_real_, slope = NA_real_) |>
  dplyr::as_tibble() |>
  dplyr::select(stationID, elevation, aspect, slope)
# test data
topo_test_sf <- meteo_test_correct |>
  dplyr::select(stationID, elevation) |>
  dplyr::mutate(aspect = NA_real_, slope = NA_real_)
topo_test_no_names <- topo_test_correct |>
  dplyr::rename(Elevation = elevation)
topo_test_no_id <- topo_test_correct |>
  dplyr::select(-stationID)
topo_test_no_numeric <- topo_test_correct |>
  dplyr::mutate(elevation = as.character(elevation))

test_that("has_topo checks works", {
  expect_true(has_topo(topo_test_correct))
  expect_true(has_topo(topo_test_sf))
  expect_error(has_topo(topo_test_no_names), "elevation")
  expect_error(has_topo(topo_test_no_id), "stationID")
  expect_error(has_topo(topo_test_no_numeric), "elevation")
})

# test_data
meteo_test_no_topo <- meteo_test_correct |>
  dplyr::select(-elevation)

test_that("with_meteo add_topo combo works", {
  expect_equal(meteo_test_no_topo, with_meteo(meteo_test_no_topo))
  expect_error(with_meteo(meteo_test), "Names found in")
  expect_error(with_meteo(meteo_test_no_sf), "meteo must be an sf")
  expect_error(with_meteo(meteo_test_no_dates), "variable called dates")
  expect_error(with_meteo(meteo_test_wrong_date_format), "must be a date")
  expect_error(with_meteo(meteo_test_no_id), "stationID variable")
  expect_error(with_meteo(meteo_test_no_numeric), "MinTemperature")
  expect_error(with_meteo(meteo_test_no_points), "geometries must be")

  expect_error(
    with_meteo(meteo_test_no_topo) |> add_topo(topo_test_no_names), "elevation"
  )
  expect_error(
    with_meteo(meteo_test_no_topo) |> add_topo(topo_test_no_id), "stationID"
  )
  expect_error(
    with_meteo(meteo_test_no_topo) |> add_topo(topo_test_no_numeric), "elevation"
  )
  expect_s3_class(
    meteo_test_with_topo <- with_meteo(meteo_test_no_topo) |>
      add_topo(topo_test_correct),
    "sf"
  )
  expect_identical(
    nrow(meteo_test_with_topo), nrow(meteo_test_no_topo)
  )
  expect_true(
    all(c("elevation", "MinTemperature", "MaxTemperature", "Precipitation")
    %in% names(meteo_test_with_topo))
  )

  # special cases
  # both spatial. Topo is converted internally to tibble, so the process
  # should work, but also return identical results as with no sf object.
  # Also, no duplicated geometries should be found now
  expect_s3_class(
    meteo_test_with_topo_sf <-
      with_meteo(meteo_test_no_topo) |> add_topo(topo_test_sf),
    "sf"
  )
  expect_equal(meteo_test_with_topo, meteo_test_with_topo_sf)
  # meteo has already topo but we joined equally
  expect_warning(
    with_meteo(meteo_test_correct) |> add_topo(topo_test_correct),
    "Topology variables found"
  )
})

meteo_test_with_topo <- with_meteo(meteo_test_no_topo) |>
  add_topo(topo_test_correct)

test_that("create_meteo_interpolator works as expected", {
  expect_error(create_meteo_interpolator(meteo_test_no_topo), "elevation")
  expect_error(
    create_meteo_interpolator(meteo_test_with_topo, params = "tururu"),
    "named list"
  )
  expect_warning(
    create_meteo_interpolator(
      meteo_test_with_topo, params = list(debig = FALSE)
    ),
    "will not be used"
  )
  expect_message(
    create_meteo_interpolator(
      meteo_test_with_topo, params = list(debug = FALSE)
    ),
    "are missing"
  )
  expect_identical(
    suppressWarnings(.safely_create_interpolation_params(NULL)),
    defaultInterpolationParams()
  )
  expect_identical(
    .safely_create_interpolation_params(defaultInterpolationParams()), defaultInterpolationParams()
  )
  expect_identical(
    suppressWarnings(.safely_create_interpolation_params(list(debug = FALSE))),
    defaultInterpolationParams()
  )
  expect_identical(
    .safely_create_interpolation_params(list(debug = TRUE))$wind_height, defaultInterpolationParams()$wind_height
  )
  expect_true(
    .safely_create_interpolation_params(list(debug = TRUE))$debug
  )

  params_names <- c(
    "initial_Rp", "iterations", "alpha_MinTemperature", "alpha_MaxTemperature",
    "alpha_DewTemperature", "alpha_PrecipitationEvent", "alpha_PrecipitationAmount",
    "alpha_Wind", "N_MinTemperature", "N_MaxTemperature", "N_DewTemperature",
    "N_PrecipitationEvent", "N_PrecipitationAmount", "N_Wind", "St_Precipitation",
    "St_TemperatureRange", "pop_crit", "f_max", "wind_height", "debug"
  )
  expect_named(suppressWarnings(.safely_create_interpolation_params(NULL)), params_names)
  expect_named(suppressWarnings(.safely_create_interpolation_params(list(tururu = 'tururu'))), params_names)
  expect_named(suppressWarnings(.safely_create_interpolation_params(list(iterations = 25))), params_names)

  meteo_test_with_topo_elevation_nas <- meteo_test_with_topo
  meteo_test_with_topo_elevation_nas[1:5, "elevation"] <- NA
  meteo_test_with_topo_elevation_all_nas <- meteo_test_with_topo |>
    dplyr::mutate(elevation = NA_real_)
  expect_warning(
    create_meteo_interpolator(
      meteo_test_with_topo_elevation_nas, params = defaultInterpolationParams()
    ),
    "lack values for elevation"
  )
  expect_error(
    suppressWarnings(create_meteo_interpolator(
      meteo_test_with_topo_elevation_all_nas,
      params = defaultInterpolationParams()
    )),
    "No elevation values"
  )
  expect_warning(
    interpolator_test <- create_meteo_interpolator(meteo_test_with_topo),
    "No interpolation parameters"
  )

  expect_false(is.null(dimnames(interpolator_test[[1]])[[1]]))
  expect_false(is.null(dimnames(interpolator_test[[1]])[[2]]))

  # check that aspect and slope are zeroes
  expect_true(all(interpolator_test[["aspect"]] == 0))
  expect_true(all(interpolator_test[["slope"]] == 0))

  # everything should be ok, no error (hence the NA)
  expect_error(.is_interpolator(interpolator_test), NA)

  # test data
  interpolator_test_no_dimensions <- interpolator_test |>
    stars::st_set_dimensions('date', names = 'tururu')
  interpolator_test_no_attr <- interpolator_test
  attr(interpolator_test_no_attr, "params") <- NULL
  interpolator_test_no_meteo_names <- interpolator_test
  temp_index <-
    which(names(interpolator_test_no_meteo_names) == 'MinTemperature')
  names(interpolator_test_no_meteo_names)[temp_index] <- "tururu"
  interpolator_test_no_topo_names <- interpolator_test
  elev_index <-
    which(names(interpolator_test_no_meteo_names) == 'elevation')
  names(interpolator_test_no_topo_names)[elev_index] <- "tururu"

  expect_error(
    .is_interpolator(interpolator_test_no_dimensions),
    "missing the correct dimensions"
  )
  expect_error(
    .is_interpolator(interpolator_test_no_attr),
    "missing the interpolation parameters"
  )
  expect_error(
    .is_interpolator(interpolator_test_no_meteo_names),
    "comply"
  )
  expect_error(
    .is_interpolator(interpolator_test_no_topo_names),
    "comply"
  )
})

suppressWarnings(
  interpolator_test <- create_meteo_interpolator(meteo_test_with_topo)
)

test_that("get and set params methods work as expected", {

  # get
  expect_identical(get_interpolation_params(interpolator_test), attr(interpolator_test, 'params'))

  # set
  expect_warning(set_interpolation_params(interpolator_test), "using defaults")
  expect_identical(
    get_interpolation_params(suppressWarnings(set_interpolation_params(interpolator_test))),
    defaultInterpolationParams()
  )
  expect_warning(set_interpolation_params(interpolator_test, list(debig = FALSE)), "will not be used")
  expect_message(set_interpolation_params(interpolator_test, list(debug = TRUE)), "using default")
  expect_s3_class(set_interpolation_params(interpolator_test, list(debug = TRUE)), "stars")
  expect_true(
    get_interpolation_params(set_interpolation_params(interpolator_test, list(debug = TRUE)))$debug
  )
  interpolator_updated <- set_interpolation_params(interpolator_test, list(N_MinTemperature = 99))
  interpolator_updated_2 <- set_interpolation_params(interpolator_updated, list(N_MaxTemperature = 125))
  expect_true(get_interpolation_params(interpolator_updated)$N_MinTemperature == 99)
  expect_true(
    get_interpolation_params(interpolator_updated_2)$N_MinTemperature == 99 &&
      get_interpolation_params(interpolator_updated_2)$N_MaxTemperature == 125
  )
  expect_false(get_interpolation_params(interpolator_updated)$N_MaxTemperature == 125)
})

test_that("write and read interpolators works as expected", {
  tmp_dir <- tempdir()
  tmp_file <- paste0(tmp_dir, "/test_interpolator.nc")

  expect_equal(
    write_interpolator(interpolator_test, tmp_file),
    interpolator_test
  )
  expect_equal(
    interpolator_write <-
      write_interpolator(interpolator_test, tmp_file, .overwrite = TRUE),
    interpolator_test
  )
  expect_error(write_interpolator(interpolator_test, tmp_file), NULL)
  expect_equal(
    interpolator_read <- read_interpolator(tmp_file),
    interpolator_test
  )
  expect_error(
    read_interpolator(system.file("nc/reduced.nc", package = "stars")),
    "not a meteoland interpolator"
  )

  expect_error(.is_interpolator(interpolator_write), NA)
  expect_error(.is_interpolator(interpolator_read), NA)
})

test_that(".stars2sf works as intended", {
  expect_error(.stars2sf("wrong_object"), "stars class")
  expect_error(.stars2sf(raster_data_test[c("slope", "aspect")]), "No elevation")
  expect_s3_class((res <- .stars2sf(raster_data_test)), "sf")
  expect_named(res, c("elevation", "slope", "aspect", "geometry"))
  expect_identical(
    as.integer(nrow(raster_data_test) * ncol(raster_data_test)),
    nrow(res)
  )
})

test_that(".is_spatial_data, .is_raster works as intended", {
  # .is_spatial_data
  expect_false(.is_spatial_data(25))
  expect_false(.is_spatial_data("25"))
  expect_false(.is_spatial_data(terra::rast(raster_data_test)))
  expect_false(.is_spatial_data(sf::as_Spatial(meteo_test)))
  expect_true(.is_spatial_data(meteo_test))
  expect_true(.is_spatial_data(raster_data_test))
  # .is_raster
  expect_false(.is_raster(interpolator_test))
  expect_true(.is_raster(raster_data_test))
})

test_that("interpolation process works as intended", {
  # argument errors
  expect_error(
    interpolate_data(25, interpolator_test), "must be"
  )
  expect_error(
    interpolate_data(interpolator_test, interpolator_test), "vector data cube"
  )
  expect_error(
    interpolate_data(sf::st_transform(raster_data_test, crs = 4326), interpolator_test),
    "Curvilinear grids"
  )
  expect_error(
    interpolate_data(raster_data_test, interpolator_test, dates = 'tururu'),
    "dates object provided"
  )
  expect_error(interpolate_data(raster_data_test, raster_data_test), NULL)
  expect_warning(
    interpolate_data(dplyr::select(points_data_test, -slope), interpolator_test),
    "not mandatory"
  )

  # points
  expect_s3_class(
    (points_res <- interpolate_data(points_data_test, interpolator_test)),
    'sf'
  )
  expect_identical(nrow(points_res), nrow(points_data_test))
  expect_true(all(names(points_data_test) %in% names(points_res)))
  expect_true("interpolated_data" %in% names(points_res))
  expect_true(all(
    c(interpolation_var_names, "dates", "DOY") %in%
      names(points_res |> tidyr::unnest(cols = c(interpolated_data)))
  ))
  expect_identical(sf::st_geometry(points_res), sf::st_geometry(points_data_test))
  expect_identical(sf::st_crs(points_res), sf::st_crs(points_data_test))

  # raster
  expect_s3_class(
    (raster_res <- interpolate_data(raster_data_test, interpolator_test)),
    'stars'
  )
  expect_identical(dim(raster_res)[1:2], dim(raster_data_test)[1:2])
  expect_true(all(names(raster_data_test) %in% names(raster_res)))
  expect_true(all(interpolation_var_names %in% names(raster_res)))
  expect_identical(sf::st_crs(raster_res), sf::st_crs(raster_data_test))
})

test_that("interpolator calibration works as expected", {
  expect_error(
    interpolator_calibration("tururu"),
    "missing the interpolation parameters"
  )
  interpolator_no_params <- interpolator_test
  attr(interpolator_no_params, "params") <- NULL
  expect_error(
    interpolator_calibration(interpolator_no_params),
    "missing the interpolation parameters"
  )
  interpolator_wrong_dims <- interpolator_test
  dimnames(interpolator_wrong_dims) <- c('tururu', 'larara')
  expect_error(
    interpolator_calibration(interpolator_wrong_dims),
    "missing the correct dimensions"
  )
  interpolator_no_meteo_names <- interpolator_test
  names(interpolator_no_meteo_names) <- c('tururu', names(interpolator_no_meteo_names)[-1])
  expect_error(
    interpolator_calibration(interpolator_no_meteo_names),
    "Names found in interpolator don't comply with the required names"
  )

  expect_type(
    (test_calibration <-
       interpolator_calibration(interpolator_test, N_seq = c(5, 10), alpha_seq = c(9, 9.5))),
    'list'
  )
  expect_named(test_calibration, c("MAE", "minMAE", "N", "alpha", "observed", "predicted"))
  expect_true(is.matrix(test_calibration$MAE))
  expect_identical(dim(test_calibration$MAE), c(2L,2L))
  expect_named(dimnames(test_calibration$MAE), c("N_seq", "alpha_seq"))
  expect_true(is.numeric(test_calibration$minMAE))
  expect_length(test_calibration$minMAE, 1)
  expect_identical(min(test_calibration$MAE, na.rm = TRUE), test_calibration$minMAE)
  expect_true(is.numeric(test_calibration$N))
  expect_length(test_calibration$N, 1)
  expect_true(is.numeric(test_calibration$alpha))
  expect_length(test_calibration$alpha, 1)
  expect_true(is.matrix(test_calibration$observed))
  expect_true(is.matrix(test_calibration$predicted))
  expect_false(is.null(dimnames(test_calibration$observed)[[1]]))
  expect_false(is.null(dimnames(test_calibration$observed)[[2]]))
  expect_false(is.null(dimnames(test_calibration$predicted)[[1]]))
  expect_false(is.null(dimnames(test_calibration$predicted)[[2]]))
  expect_true("2021-01-31" %in% dimnames(test_calibration$predicted)[[2]])

  # selecting stations
  expect_type(
    (test_calibration_three_stations <- interpolator_calibration(
      interpolator_test,
      stations = c(76, 83, 187),
      N_seq = c(5, 10), alpha_seq = c(9, 9.5))),
    'list'
  )

  expect_named(test_calibration_three_stations, c("MAE", "minMAE", "N", "alpha", "observed", "predicted"))
  expect_true(is.matrix(test_calibration_three_stations$MAE))
  expect_identical(dim(test_calibration_three_stations$MAE), c(2L,2L))
  expect_named(dimnames(test_calibration_three_stations$MAE), c("N_seq", "alpha_seq"))
  expect_true(is.numeric(test_calibration_three_stations$minMAE))
  expect_length(test_calibration_three_stations$minMAE, 1)
  expect_identical(min(test_calibration_three_stations$MAE, na.rm = TRUE), test_calibration_three_stations$minMAE)
  expect_true(is.numeric(test_calibration_three_stations$N))
  expect_length(test_calibration_three_stations$N, 1)
  expect_true(is.numeric(test_calibration_three_stations$alpha))
  expect_length(test_calibration_three_stations$alpha, 1)
  expect_true(is.matrix(test_calibration_three_stations$observed))
  expect_length(dimnames(test_calibration_three_stations$observed)[[1]], 3)
  expect_identical(dimnames(test_calibration_three_stations$observed)[[1]], c("V1", "VC", "ZD"))
  expect_true(is.matrix(test_calibration_three_stations$predicted))
  expect_length(dimnames(test_calibration_three_stations$predicted)[[1]], 3)
  expect_identical(dimnames(test_calibration_three_stations$predicted)[[1]], c("V1", "VC", "ZD"))

  # stations as names
  expect_type(
    (test_calibration_three_stations_names <- interpolator_calibration(
      interpolator_test,
      stations = c("V1", "VC", "ZD"),
      N_seq = c(5, 10), alpha_seq = c(9, 9.5))),
    'list'
  )
  expect_identical(test_calibration_three_stations_names, test_calibration_three_stations)

  # returning an interpolator
  expect_s3_class(
    (test_calibrated_interpolator <- interpolator_calibration(
      interpolator_test,
      stations = c(76, 83, 187),
      update_interpolator_params = TRUE,
      N_seq = c(5, 10), alpha_seq = c(9, 9.5))),
    'stars'
  )
  expect_error(.is_interpolator(test_calibrated_interpolator), NA)
  expect_false(
    attr(interpolator_test, 'params')$alpha_MinTemperature ==
      attr(test_calibrated_interpolator, 'params')$alpha_MinTemperature
  )
  expect_false(
    attr(interpolator_test, 'params')$N_MinTemperature ==
      attr(test_calibrated_interpolator, 'params')$N_MinTemperature
  )
  expect_identical(
    interpolator_test$MinTemperature, test_calibrated_interpolator$MinTemperature
  )
  expect_identical(names(interpolator_test), names(test_calibrated_interpolator))
  expect_identical(
    names(attr(interpolator_test, 'params')),
    names(attr(test_calibrated_interpolator, 'params'))
  )

  expect_identical(
    test_calibration_three_stations$N,
    attr(test_calibrated_interpolator, 'params')$N_MinTemperature
  )
  expect_identical(
    test_calibration_three_stations$alpha,
    attr(test_calibrated_interpolator, 'params')$alpha_MinTemperature
  )

})

test_that("interpolation cross validation works as expected", {

  expect_type(
    (crossvalidation_test <- interpolation_cross_validation(interpolator_test)),
    "list"
  )

  expect_named(
    crossvalidation_test, c("errors", "station_stats", "dates_stats", "r2")
  )
  expect_true(all(
    is.data.frame(crossvalidation_test[["errors"]]),
    is.data.frame(crossvalidation_test[["station_stats"]]),
    is.data.frame(crossvalidation_test[["dates_stats"]])
  ))
  expect_type(crossvalidation_test[["r2"]], "list")
  expect_named(
    crossvalidation_test[["errors"]],
    c(
      "dates", "station", "stationID",
      paste0(
        c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation", "Precipitation"),
        "_error"
      ),
      paste0(
        c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation", "Precipitation"),
        "_predicted"
      ),
      paste0(
        c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation", "Precipitation"),
        "_observed"
      )
    )
  )
  expect_named(
    crossvalidation_test[["station_stats"]],
    c(
      "station", "stationID",
      paste0(
        c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation", "TotalPrecipitation", "DaysPrecipitation"),
        "_station_bias"
      ),
      paste0(
        c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation"),
        "_station_mae"
      ),
      paste0(
        c("TotalPrecipitation", "DaysPrecipitation"),
        "_station_relative_bias"
      ),
      paste0("FreqPrecipitation", c("_station_observed", "_station_predicted"))
    ),
    ignore.order = TRUE
  )
  expect_named(
    crossvalidation_test[["dates_stats"]],
    c(
      "dates",
      paste0(
        c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation", "TotalPrecipitation", "DaysPrecipitation"),
        "_date_bias"
      ),
      paste0(
        c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation"),
        "_date_mae"
      ),
      paste0(
        c("TotalPrecipitation", "DaysPrecipitation"),
        "_date_relative_bias"
      ),
      paste0("FreqPrecipitation", c("_date_observed", "_date_predicted"))
    ),
    ignore.order = TRUE
  )
  expect_named(
    crossvalidation_test[["r2"]],
    c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation")
  )
  expect_equal(
    nrow(crossvalidation_test[["errors"]]),
    as.numeric(nrow(interpolator_test[["MinTemperature"]]) * ncol(interpolator_test[["MinTemperature"]]))
  )
  expect_equal(
    nrow(crossvalidation_test[["station_stats"]]),
    as.numeric(ncol(interpolator_test[["MinTemperature"]]))
  )
  expect_equal(
    nrow(crossvalidation_test[["dates_stats"]]),
    as.numeric(nrow(interpolator_test[["MinTemperature"]]))
  )

  # some stations
  expect_type(
    (crossvalidation_three_stations_test <-
       interpolation_cross_validation(interpolator_test, stations = c(76L, 83L, 187L))),
    "list"
  )
  expect_named(
    crossvalidation_three_stations_test, c("errors", "station_stats", "dates_stats", "r2")
  )
  expect_true(all(
    is.data.frame(crossvalidation_three_stations_test[["errors"]]),
    is.data.frame(crossvalidation_three_stations_test[["station_stats"]]),
    is.data.frame(crossvalidation_three_stations_test[["dates_stats"]])
  ))
  expect_type(crossvalidation_three_stations_test[["r2"]], "list")

  expect_length(
    unique(crossvalidation_three_stations_test[["errors"]][["station"]]), 3
  )
  expect_true(
    all(unique(crossvalidation_three_stations_test[["errors"]][["stationID"]]) %in% c("V1", "VC", "ZD"))
  )
  expect_length(
    crossvalidation_three_stations_test[["station_stats"]][["station"]], 3
  )
  expect_true(
    all(crossvalidation_three_stations_test[["station_stats"]][["stationID"]] %in% c("V1", "VC", "ZD"))
  )

  # some stations by name
  expect_type(
    (crossvalidation_three_stations_name_test <-
       interpolation_cross_validation(interpolator_test, stations = c("V1", "VC", "ZD"))),
    "list"
  )
  expect_identical(
    crossvalidation_three_stations_name_test, crossvalidation_three_stations_test
  )

})
