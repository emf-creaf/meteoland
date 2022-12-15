temperature <- seq(5, 30, 5)
relative_humidity <- seq(50, 100, 10)
dew_temperature <- seq(6, 1, -1)
specific_humidity <- seq(0.00266, 0.0266, length.out = 6)
relative_humidity <- seq(30, 80, length.out = 6)

test_that("humidity_relative2dewtemperature works as expected", {

  # vector
  expect_type(
    (test_res <- humidity_relative2dewtemperature(temperature, relative_humidity)),
    "double"
  )
  expect_length(test_res, length(temperature))

  # matrix
  temperature_matrix <- matrix(temperature, nrow = 2, ncol = 3)
  relative_humidity_matrix <- matrix(relative_humidity, nrow = 2, ncol = 3)
  expect_no_error(
    (test_res_matrix <- humidity_relative2dewtemperature(temperature_matrix, relative_humidity_matrix))
  )
  expect_true(inherits(test_res_matrix, "matrix"))
  expect_equal(dim(test_res_matrix), c(2,3))

  # error when no numbers
  expect_error(
    humidity_relative2dewtemperature(letters, LETTERS)
  )
})

test_that("humidity_dewtemperature2relative works as expected", {
  # vector
  expect_type(
    (test_res <- humidity_dewtemperature2relative(temperature, dew_temperature)),
    "double"
  )
  expect_length(test_res, length(temperature))

  # matrix
  temperature_matrix <- matrix(temperature, nrow = 2, ncol = 3)
  dew_temperature_matrix <- matrix(dew_temperature, nrow = 2, ncol = 3)
  expect_no_error(
    (test_res_matrix <- humidity_dewtemperature2relative(temperature_matrix, dew_temperature_matrix))
  )
  expect_identical(test_res, test_res_matrix)

  # it should be no value above 100
  expect_false(any(test_res > 100))

  # error when no numbers
  expect_error(
    humidity_dewtemperature2relative(letters, LETTERS)
  )

})

test_that("humidity_specific2relative works as expected", {
  # vector
  expect_type(
    (test_res <- humidity_specific2relative(temperature, specific_humidity)),
    "double"
  )
  expect_length(test_res, length(temperature))

  # matrix
  temperature_matrix <- matrix(temperature, nrow = 2, ncol = 3)
  specific_humidity_matrix <- matrix(specific_humidity, nrow = 2, ncol = 3)
  expect_no_error(
    (test_res_matrix <- humidity_specific2relative(temperature_matrix, specific_humidity_matrix))
  )
  expect_true(inherits(test_res_matrix, "matrix"))
  expect_equal(dim(test_res_matrix), c(2,3))
  expect_identical(test_res, as.vector(test_res_matrix))

  # it should be no value above 100
  expect_false(any(test_res > 100))
  expect_true(any(humidity_specific2relative(temperature, specific_humidity, TRUE) > 100))

  # error when no numbers
  expect_error(
    humidity_specific2relative(letters, LETTERS)
  )
})

test_that("humidity_relative2specific works as expected", {
  # vector
  expect_type(
    (test_res <- humidity_relative2specific(temperature, relative_humidity)),
    "double"
  )
  expect_length(test_res, length(temperature))

  # matrix
  temperature_matrix <- matrix(temperature, nrow = 2, ncol = 3)
  relative_humidity_matrix <- matrix(relative_humidity, nrow = 2, ncol = 3)
  expect_no_error(
    (test_res_matrix <- humidity_relative2specific(temperature_matrix, relative_humidity_matrix))
  )
  expect_true(inherits(test_res_matrix, "matrix"))
  expect_equal(dim(test_res_matrix), c(2,3))
  expect_identical(test_res, as.vector(test_res_matrix))

  # error when no numbers
  expect_error(
    humidity_relative2specific(letters, LETTERS)
  )
})

test_that("precipitation_rainfall_erosivity works as expected", {
  meteo_data_test <- meteoland_meteo_example |>
    as.data.frame() |>
    dplyr::filter(stationID == "VC") |>
    # magrittr::set_rownames(as.Date(unique(meteoland_meteo_example$dates))) |>
    dplyr::select(-geometry, -stationID, -elevation, -aspect, -slope)
  long_test <- 0.43
  scale_test <- "month"
  average_test <- TRUE

  expect_type(
    (rainfallErosivity_monthly_test <- precipitation_rainfall_erosivity(
      meteo_data_test, long_test, scale_test, average_test
    )),
    'double'
  )
  expect_length(rainfallErosivity_monthly_test, 1)
  expect_named(rainfallErosivity_monthly_test, '4')

  expect_type(
    (rainfallErosivity_yearly_test <- precipitation_rainfall_erosivity(
      meteo_data_test, long_test, "year", average_test
    )),
    'double'
  )
  expect_length(rainfallErosivity_yearly_test, 1)
  expect_named(rainfallErosivity_yearly_test, '2022')
  expect_equal(rainfallErosivity_monthly_test[[1]], rainfallErosivity_yearly_test[[1]])

  # errors
  expect_error(precipitation_rainfall_erosivity(
    "meteo_data_test", long_test, scale_test, average_test
  ))

  expect_error(precipitation_rainfall_erosivity(
    meteo_data_test, "long_test", scale_test, average_test
  ))

  expect_error(precipitation_rainfall_erosivity(
    meteo_data_test, long_test, "scale_test", average_test
  ))

  expect_error(precipitation_rainfall_erosivity(
    meteo_data_test, long_test, scale_test, "average_test"
  ))

  # more than one
  meteo_data_months_test <- meteoland_meteo_example |>
    as.data.frame() |>
    dplyr::filter(stationID %in% c("VC", "VY")) |>
    # simulate another month
    dplyr::mutate(dates = dplyr::if_else(
      stationID == "VY",
      dates + (60*60*24*30),
      dates
    )) |>
    dplyr::select(-geometry, -stationID, -elevation, -aspect, -slope)

  expect_type(
    (rainfallErosivity_monthly_months_test <- precipitation_rainfall_erosivity(
      meteo_data_months_test, long_test, scale_test, average_test
    )),
    'double'
  )
  expect_length(rainfallErosivity_monthly_months_test, 2)
  expect_named(rainfallErosivity_monthly_months_test, c('4', '5'))

  expect_type(
    (rainfallErosivity_yearly_months_test <- precipitation_rainfall_erosivity(
      meteo_data_months_test, long_test, "year", average_test
    )),
    'double'
  )
  expect_length(rainfallErosivity_yearly_months_test, 1)
  expect_named(rainfallErosivity_yearly_months_test, '2022')

  # TODO multiyear tests
})
