# complete meteo
test_that("complete_meteo works", {
  meteo_test <- readRDS('meteospain_daily_test.rds') |> meteospain2meteoland(complete = FALSE)
  meteo_test_no_precip <- meteo_test |> dplyr::select(!"Precipitation")
  meteo_test_no_rad <- meteo_test |> dplyr::select(!"Radiation")

  expect_s3_class(completed_ok <- complete_meteo(meteo_test), "sf")
  expect_s3_class(completed_no_precip <- complete_meteo(meteo_test_no_precip), "sf")
  expect_s3_class(completed_no_rad <- complete_meteo(meteo_test_no_rad), "sf")

  expect_true(sum(is.na(completed_ok$MeanRelativeHumidity)) < sum(is.na(meteo_test$MeanRelativeHumidity)))
  expect_true(sum(is.na(completed_ok$Radiation)) < sum(is.na(meteo_test$Radiation)))

  expect_true(sum(is.na(completed_no_precip$MeanRelativeHumidity)) < sum(is.na(meteo_test_no_precip$MeanRelativeHumidity)))
  expect_true(sum(is.na(completed_no_precip$Radiation)) < sum(is.na(meteo_test_no_precip$Radiation)))

  expect_true(sum(is.na(completed_no_rad$MeanRelativeHumidity)) < sum(is.na(meteo_test_no_rad$MeanRelativeHumidity)))
  expect_true(any(!is.na(completed_no_rad$Radiation)))
})


# worldmet2meteoland
test_that("worldmet2meteoland works", {

  # skip_on_cran()

  # test data
  worldmet_test <- readRDS("worldmet_test.rds")

  expect_s3_class(test_res <- suppressWarnings(worldmet2meteoland(worldmet_test)), 'sf')
  expect_true(
    all(c(
      "MeanTemperature", "MinTemperature", "MaxTemperature",
      "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "stationID", "dates", "elevation"
    ) %in% names(test_res))
  )
  expect_true(sum(duplicated(test_res)) < 1)

  meteo_test_no_dates <- worldmet_test |> dplyr::select(-date)
  meteo_test_no_relative_humidity <- worldmet_test |>
    dplyr::select(-RH)

  expect_error(
    suppressWarnings(worldmet2meteoland(meteo_test_no_dates)),
    "Provided data has no date or code variables"
  )

  expect_s3_class(
    test_res_norh <- suppressWarnings(worldmet2meteoland(meteo_test_no_relative_humidity)),
    "sf"
  )
  expect_true(
    all(c(
      "MinTemperature", "MaxTemperature", "Precipitation",
      "stationID", "dates"
    ) %in% names(test_res_norh))
  )
  expect_true(sum(duplicated(test_res_norh)) < 1)

  expect_s3_class(
    test_complete_no_changes <- suppressWarnings(worldmet2meteoland(worldmet_test, complete = TRUE)),
    "sf"
  )
  expect_identical(nrow(test_complete_no_changes), nrow(test_res))
  expect_identical(names(test_complete_no_changes), c(names(test_res), 'aspect', 'slope'))
  expect_identical(test_complete_no_changes$MinTemperature, test_res$MinTemperature)
  expect_false(
    identical(test_res$Radiation, test_complete_no_changes$Radiation)
  )
  expect_true(sum(duplicated(test_complete_no_changes)) < 1)


})

test_that("meteospain2meteoland works", {
  # test data
  meteo_test <- readRDS('meteospain_daily_test.rds')
  meteo_test_non_unique_ids <- meteo_test
  meteo_test_non_unique_ids$geometry[1] <- meteo_test_non_unique_ids$geometry[5000]
  meteo_test_subdaily_with_errors <- readRDS('meteospain_subdaily_test.rds')
  meteo_test_subdaily_with_errors$geometry[1] <- meteo_test_subdaily_with_errors$geometry[5000]

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
  expect_true(sum(duplicated(test_res)) < 1)

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
  expect_true(sum(duplicated(test_res_norh)) < 1)

  expect_warning(
    meteo_test_fixed <- meteospain2meteoland(meteo_test_non_unique_ids),
    "Choosing the most recent metadata"
  )
  expect_identical(meteo_test_fixed, test_res)
  expect_true(sum(duplicated(meteo_test_fixed)) < 1)

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
  expect_true(sum(duplicated(test_complete_no_changes)) < 1)

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

  expect_true(sum(duplicated(test_complete_all)) < 1)

  # subdaily checks

  # we have two warnings here, warnings are tricky to catch when there are more than one. One is
  # catched, the others trigger and the test fails :(
  # expect_warning(
  #   .reshape_meteo(meteo_test_subdaily_with_errors, .meteospain_variables_dictionary(TRUE), FALSE),
  #   "Choosing the most recent metadata"
  # )
  # expect_warning(
  #   .reshape_meteo(meteo_test_subdaily_with_errors, .meteospain_variables_dictionary(TRUE), FALSE),
  #   "Provided meteo data seems to be in subdaily time steps"
  # )
  # expect_warning(
  #   (subdaily_fixed <- .fix_station_geometries(meteo_test_subdaily_with_errors)),
  #   "Choosing the most recent metadata"
  # )

  # we expect no error
  expect_no_error(test_subdaily <- suppressWarnings(meteospain2meteoland(meteo_test_subdaily_with_errors)))
  expect_s3_class(test_subdaily, 'sf')
  expect_true(
    all(c(
      "MeanTemperature", "MinTemperature", "MaxTemperature",
      "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "stationID", "dates", "elevation"
    ) %in% names(test_subdaily))
  )
  expect_true(sum(duplicated(test_subdaily)) < 1)

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
  expect_true(sum(duplicated(test_subdaily_complete)) < 1)


})

# TODO to test fringe cases when metadata of station changes, test this with
# subdaily_meteo_generating_duplicated_rows_in_meteospain2meteoland.rds which
# generates duplicated rows in the process.
test_that("meteospain2meteoland works with subdaily fringe cases", {

  meteo_test_subdaily_with_errors <-
    readRDS("subdaily_duplicated_rows_generator.rds")
  expect_no_error(test_subdaily <- suppressWarnings(meteospain2meteoland(meteo_test_subdaily_with_errors)))
  expect_s3_class(test_subdaily, 'sf')
  expect_true(
    all(c(
      "MeanTemperature", "MinTemperature", "MaxTemperature",
      "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "stationID", "dates", "elevation"
    ) %in% names(test_subdaily))
  )
  expect_true(sum(duplicated(test_subdaily)) < 1)

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
  expect_true(sum(duplicated(test_subdaily_complete)) < 1)
})
