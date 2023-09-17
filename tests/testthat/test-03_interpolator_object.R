meteo_test_correct <- readRDS("meteo_and_topo_test.rds")
meteo_test_no_topo <- meteo_test_correct |>
  dplyr::select(-elevation, -aspect, -slope)

test_that("create_meteo_interpolator works as expected", {
  expect_error(create_meteo_interpolator(meteo_test_no_topo), "elevation")
  expect_error(
    create_meteo_interpolator(meteo_test_correct, params = "tururu"),
    "named list"
  )
  expect_warning(
    create_meteo_interpolator(
      meteo_test_correct, params = list(debig = FALSE)
    ),
    "will not be used"
  )
  expect_message(
    create_meteo_interpolator(
      meteo_test_correct, params = list(debug = FALSE)
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
    "St_TemperatureRange", "pop_crit", "f_max", "wind_height", 
    "wind_roughness_height", "penman_albedo", "penman_windfun","debug"
  )
  expect_named(suppressWarnings(.safely_create_interpolation_params(NULL)), params_names)
  expect_named(suppressWarnings(.safely_create_interpolation_params(list(tururu = 'tururu'))), params_names)
  expect_named(suppressWarnings(.safely_create_interpolation_params(list(iterations = 25))), params_names)

  meteo_test_correct_elevation_nas <- meteo_test_correct
  meteo_test_correct_elevation_nas[1:5, "elevation"] <- NA
  meteo_test_correct_elevation_all_nas <- meteo_test_correct |>
    dplyr::mutate(elevation = NA_real_)
  expect_warning(
    create_meteo_interpolator(
      meteo_test_correct_elevation_nas, params = defaultInterpolationParams()
    ),
    "lack values for elevation"
  )
  expect_error(
    suppressWarnings(create_meteo_interpolator(
      meteo_test_correct_elevation_all_nas,
      params = defaultInterpolationParams()
    )),
    "No elevation values"
  )
  expect_warning(
    interpolator_test <- create_meteo_interpolator(meteo_test_correct),
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

  ## TODO
  # tests that interpolator has all names when variables are missing from
  # meteo (interpolator variables are created filled with NAs)
  meteo_test_no_relative_humidity <- meteo_test_correct |>
    dplyr::select(!dplyr::contains("RelativeHumidity"))
  meteo_test_no_precipitation <- meteo_test_correct |>
    dplyr::select(!dplyr::contains("Precipitation"))

  expect_warning(
    interpolator_test_no_relative_humidity <-
      create_meteo_interpolator(meteo_test_no_relative_humidity),
    "No interpolation parameters"
  )
  expect_named(
    interpolator_test_no_relative_humidity, names(interpolator_test)
  )
  expect_true(
    all(is.na(interpolator_test_no_relative_humidity[["RelativeHumidity"]]))
  )
  expect_identical(
    interpolator_test_no_relative_humidity[["MinTemperature"]],
    interpolator_test[["MinTemperature"]]
  )

  expect_warning(
    interpolator_test_no_precipitation <-
      create_meteo_interpolator(meteo_test_no_precipitation),
    "No interpolation parameters"
  )
  expect_named(
    interpolator_test_no_precipitation, names(interpolator_test)
  )
  expect_true(
    all(is.na(interpolator_test_no_precipitation[["Precipitation"]]))
  )
  expect_true(
    all(is.na(interpolator_test_no_precipitation[["SmoothedPrecipitation"]]))
  )
  expect_identical(
    interpolator_test_no_precipitation[["MinTemperature"]],
    interpolator_test[["MinTemperature"]]
  )

})

suppressWarnings(
  interpolator_test <- create_meteo_interpolator(meteo_test_correct)
)

test_that("defaultInterpolationParams work as expected", {
  interpolation_param_names <- c(
    "initial_Rp", "iterations", "alpha_MinTemperature", "alpha_MaxTemperature",
    "alpha_DewTemperature", "alpha_PrecipitationEvent", "alpha_PrecipitationAmount",
    "alpha_Wind", "N_MinTemperature", "N_MaxTemperature", "N_DewTemperature",
    "N_PrecipitationEvent", "N_PrecipitationAmount", "N_Wind", "St_Precipitation",
    "St_TemperatureRange", "pop_crit", "f_max", "wind_height", "wind_roughness_height", 
    "penman_albedo", "penman_windfun","debug"
  )
  expect_type(defaultInterpolationParams(), "list")
  expect_length(defaultInterpolationParams(), length(interpolation_param_names))
  expect_named(defaultInterpolationParams(), interpolation_param_names)
})

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
  expect_identical(
    interpolator_read,
    interpolator_write
  )
  expect_error(
    read_interpolator(system.file("nc/reduced.nc", package = "stars")),
    "not a meteoland interpolator"
  )

  expect_error(.is_interpolator(interpolator_write), NA)
  expect_error(.is_interpolator(interpolator_read), NA)
})
