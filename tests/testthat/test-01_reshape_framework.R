# worldmet2meteoland
test_that("worldmet2meteoland works", {

  # skip_on_cran()

  # test data
  worldmet_test <- readRDS("worldmet_test.rds")

  expect_s3_class(test_res <- worldmet2meteoland(worldmet_test), 'sf')
  expect_true(
    all(c(
      "MeanTemperature", "MinTemperature", "MaxTemperature",
      "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "stationID", "dates", "elevation"
    ) %in% names(test_res))
  )

  meteo_test_no_dates <- worldmet_test |> dplyr::select(-date)
  meteo_test_no_relative_humidity <- worldmet_test |>
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
    test_complete_no_changes <- worldmet2meteoland(worldmet_test, complete = TRUE),
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
