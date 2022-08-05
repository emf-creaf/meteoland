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

test_that(".meteospain2meteoland works", {
  expect_s3_class(test_res <- .meteospain2meteoland(meteo_test), 'sf')
  expect_true(
    all(c(
      "MinTemperature", "MaxTemperature", "Precipitation", "RelativeHumidity",
      "stationID", "dates"
    ) %in% names(test_res))
  )

  meteo_test_no_dates <- meteo_test |> dplyr::select(-timestamp)
  meteo_test_no_relative_humidity <- meteo_test |>
    dplyr::select(-mean_relative_humidity)

  expect_error(
    .meteospain2meteoland(meteo_test_no_dates),
    "Column `timestamp` doesn't exist."
  )
  expect_s3_class(
    test_res_norh <- .meteospain2meteoland(meteo_test_no_relative_humidity),
    "sf"
  )
  expect_true(
    all(c(
      "MinTemperature", "MaxTemperature", "Precipitation",
      "stationID", "dates"
    ) %in% names(test_res_norh))
  )
})

meteo_test_correct <- .meteospain2meteoland(meteo_test)
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

test_that("has_meteo checks work", {
  expect_true(has_meteo(meteo_test_correct))
  expect_error(has_meteo(meteo_test), "Names found in")
  expect_error(has_meteo(meteo_test_no_sf), "meteo must be an sf")
  expect_error(has_meteo(meteo_test_no_dates), "variable called dates")
  expect_error(has_meteo(meteo_test_wrong_date_format), "must be a date")
  expect_error(has_meteo(meteo_test_no_id), "stationID variable")
  expect_error(has_meteo(meteo_test_no_numeric), "MinTemperature")
  expect_error(has_meteo(meteo_test_no_points), "geometries must be")
})

topo_test_correct <- meteo_test_correct |>
  dplyr::mutate(aspect = NA, slope = NA) |>
  dplyr::as_tibble() |>
  dplyr::select(stationID, elevation, aspect, slope)
# test data
topo_test_sf <- meteo_test_correct |>
  dplyr::select(stationID, elevation) |>
  dplyr::mutate(aspect = NA, slope = NA)
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
    suppressWarnings(.get_params(NULL)),
    defaultInterpolationParams()
  )
  expect_identical(
    .get_params(defaultInterpolationParams()), defaultInterpolationParams()
  )
  expect_identical(
    suppressWarnings(.get_params(list(debug = FALSE))),
    defaultInterpolationParams()
  )
  expect_identical(
    .get_params(list(debug = TRUE))$wind_height, defaultInterpolationParams()$wind_height
  )
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

test_that("write and read interpolators works as expected", {
  tmp_dir <- tempdir()
  tmp_file <- paste0(tmp_dir, "/test_interpolator.nc")

  expect_equal(
    .write_interpolator(interpolator_test, tmp_file),
    interpolator_test
  )
  expect_equal(
    interpolator_write <-
      .write_interpolator(interpolator_test, tmp_file, .overwrite = TRUE),
    interpolator_test
  )
  expect_error(.write_interpolator(interpolator_test, tmp_file), NULL)
  expect_equal(
    interpolator_read <- .read_interpolator(tmp_file),
    interpolator_test
  )
  expect_error(
    .read_interpolator(system.file("nc/reduced.nc", package = "stars")),
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
