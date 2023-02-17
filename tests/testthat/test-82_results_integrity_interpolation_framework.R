# this tests are only intended to run in local
skip_on_cran()


# reshaping meteo ---------------------------------------------------------
library(meteospain)
meteocat_key <- Sys.getenv("meteocat")
if (meteocat_key == "") {
  meteocat_key <- try(keyring::key_get("meteocat", keyring = "malditobarbudo"))
}
aemet_key <- Sys.getenv("aemet")
if (aemet_key == "") {
  aemet_key <- try(keyring::key_get("aemet", keyring = "malditobarbudo"))
}

# skip if no keys are found
if (any(c(inherits(meteocat_key, 'try-error'), inherits(aemet_key, 'try-error')))) {
  skip("No keys for aemet and/or meteocat")
}

# daily
meteo_cat <- get_meteo_from(
  "meteocat",
  meteocat_options("daily", as.Date("2022-01-01"), api_key = meteocat_key)
)
meteo_aemet <- get_meteo_from(
  "aemet",
  aemet_options("daily", as.Date("2022-01-01"), as.Date("2022-01-31"), api_key = aemet_key)
)
meteo_test <- meteo_cat |>
  dplyr::bind_rows(meteo_aemet) |>
  dplyr::arrange(timestamp)

meteo_completed_old <- suppressWarnings(reshapemeteospain(meteo_test))
meteo_completed_new <- meteospain2meteoland(meteo_test, complete = TRUE)

elevation <- meteo_test |>
  dplyr::group_by(.data$station_id) |>
  dplyr::summarise(elevation = mean(altitude))

suppressWarnings(interpolator_old <- MeteorologyInterpolationData(
  points = meteo_completed_old,
  elevation = as.numeric(elevation$elevation)
))

suppressWarnings(interpolator_new <- with_meteo(meteo_completed_new) |>
                   create_meteo_interpolator())

# interpolation framework -------------------------------------------------
test_that("interpolator object has the same data", {

  expect_true(sum(names(meteo_completed_old@data) != elevation$station_id) < 1)
  expect_s4_class(
    interpolator_old, "MeteorologyInterpolationData"
  )
  expect_s3_class(
    interpolator_new, "stars"
  )

  # expect all variables in both interpolators are the same values
  names(interpolator_new) |>
    # topography is not checked here.
    # Also, Temperature is a new addition in the new workflow, so is not present in the old
    # interpolator.
    # Also WindDirection is wrongly calculated in the old workflow.
    purrr::walk(.f = function(variable) {
      if (variable %in% c('elevation', 'aspect', 'slope', 'Temperature', "WindDirection")) {
        return(invisible(TRUE))
      }
      expect_true(
        abs(sum(t(slot(interpolator_old, variable)) - interpolator_new[[variable]], na.rm = TRUE)) == 0
      )
      # print(variable)
      # print(sum(t(slot(interpolator_old, variable)) - interpolator_new[[variable]], na.rm = TRUE))
    })

  expect_identical(
    interpolator_old@dates |> as.character(),
    stars::st_get_dimension_values(interpolator_new, "date") |> as.character()
  )

  names(attributes(interpolator_old)$params) |>
    purrr::walk(.f = function(param) {
      if (param != "initial_Rp") {
        expect_true(
          attributes(interpolator_old)$params[[param]] == attributes(interpolator_new)$params[[param]]
        )
      } else {
        # Due to implementation differences, initial_Rp is different, so we expect it to be different
        expect_false(
          attributes(interpolator_old)$params[[param]] == attributes(interpolator_new)$params[[param]]
        )
      }
      # print(param)
      # print(attributes(interpolator_old)$params[[param]] == attributes(interpolator_new)$params[[param]])
    })

  # expect coordinates are the same. We convert a numeric to avoid differences in names
  expect_identical(
    as.numeric(c(interpolator_old@coords[,1], interpolator_old@coords[,2])),
    as.numeric(c(
      sf::st_coordinates(stars::st_get_dimension_values(interpolator_new, 'station'))[,1],
      sf::st_coordinates(stars::st_get_dimension_values(interpolator_new, 'station'))[,2]
    ))
  )
})

# To check calibration, cross-validation and results, we need to modify initial_Rp for both
# interpolators have the same data:
interpolator_new <- set_interpolation_params(
  interpolator_new,
  list(initial_Rp = attr(interpolator_old, "params")$initial_Rp),
  verbose = FALSE
)


test_that("interpolator calibration returns same parameters", {

  calibration_min_temperature_new <- interpolator_calibration(
    interpolator_new, variable = "MinTemperature",
    N_seq = c(10, 15),
    alpha_seq = c(5, 6),
    verbose = FALSE
  )

  calibration_min_temperature_old <- suppressWarnings(
    interpolation.calibration(
      interpolator_old, variable = "Tmin",
      N_seq = c(10, 15),
      alpha_seq = c(5, 6),
      verbose = FALSE
    )
  )
  expect_type(calibration_min_temperature_old, "list")
  expect_type(calibration_min_temperature_new, "list")
  expect_identical(
    tolower(names(calibration_min_temperature_old)),
    tolower(names(calibration_min_temperature_new))
  )
  expect_identical(
    calibration_min_temperature_old$minMAE, calibration_min_temperature_new$minMAE,
  )
  expect_identical(
    calibration_min_temperature_old$N, calibration_min_temperature_new$N,
  )
  expect_identical(
    calibration_min_temperature_old$alpha, calibration_min_temperature_new$alpha,
  )
  expect_identical(
    calibration_min_temperature_old$Observed, calibration_min_temperature_new$observed,
  )
  expect_identical(
    calibration_min_temperature_old$Predicted, calibration_min_temperature_new$predicted,
  )

  calibration_max_temperature_new <- interpolator_calibration(
    interpolator_new, variable = "MaxTemperature",
    N_seq = c(10, 15),
    alpha_seq = c(5, 6),
    verbose = FALSE
  )

  calibration_max_temperature_old <- suppressWarnings(
    interpolation.calibration(
      interpolator_old, variable = "Tmax",
      N_seq = c(10, 15),
      alpha_seq = c(5, 6),
      verbose = FALSE
    )
  )
  expect_type(calibration_max_temperature_old, "list")
  expect_type(calibration_max_temperature_new, "list")
  expect_identical(
    tolower(names(calibration_max_temperature_old)),
    tolower(names(calibration_max_temperature_new))
  )
  expect_identical(
    calibration_max_temperature_old$minMAE, calibration_max_temperature_new$minMAE,
  )
  expect_identical(
    calibration_max_temperature_old$N, calibration_max_temperature_new$N,
  )
  expect_identical(
    calibration_max_temperature_old$alpha, calibration_max_temperature_new$alpha,
  )
  expect_identical(
    calibration_max_temperature_old$Observed, calibration_max_temperature_new$observed,
  )
  expect_identical(
    calibration_max_temperature_old$Predicted, calibration_max_temperature_new$predicted,
  )

  calibration_dew_temperature_new <- interpolator_calibration(
    interpolator_new, variable = "DewTemperature",
    N_seq = c(10, 15),
    alpha_seq = c(5, 6),
    verbose = FALSE
  )

  calibration_dew_temperature_old <- suppressWarnings(
    interpolation.calibration(
      interpolator_old, variable = "Tdew",
      N_seq = c(10, 15),
      alpha_seq = c(5, 6),
      verbose = FALSE
    )
  )
  expect_type(calibration_dew_temperature_old, "list")
  expect_type(calibration_dew_temperature_new, "list")
  expect_identical(
    tolower(names(calibration_dew_temperature_old)),
    tolower(names(calibration_dew_temperature_new))
  )
  expect_identical(
    calibration_dew_temperature_old$minMAE, calibration_dew_temperature_new$minMAE,
  )
  expect_identical(
    calibration_dew_temperature_old$N, calibration_dew_temperature_new$N,
  )
  expect_identical(
    calibration_dew_temperature_old$alpha, calibration_dew_temperature_new$alpha,
  )
  expect_identical(
    calibration_dew_temperature_old$Observed, calibration_dew_temperature_new$observed,
  )
  expect_identical(
    calibration_dew_temperature_old$Predicted, calibration_dew_temperature_new$predicted,
  )

  calibration_precipitation_new <- interpolator_calibration(
    interpolator_new, variable = "Precipitation",
    N_seq = c(10, 15),
    alpha_seq = c(5, 6),
    verbose = FALSE
  )

  calibration_precipitation_old <- suppressWarnings(
    interpolation.calibration(
      interpolator_old, variable = "Prec",
      N_seq = c(10, 15),
      alpha_seq = c(5, 6),
      verbose = FALSE
    )
  )
  expect_type(calibration_precipitation_old, "list")
  expect_type(calibration_precipitation_new, "list")
  expect_identical(
    tolower(names(calibration_precipitation_old)),
    tolower(names(calibration_precipitation_new))
  )
  expect_identical(
    calibration_precipitation_old$minMAE, calibration_precipitation_new$minMAE,
  )
  expect_identical(
    calibration_precipitation_old$N, calibration_precipitation_new$N,
  )
  expect_identical(
    calibration_precipitation_old$alpha, calibration_precipitation_new$alpha,
  )
  expect_identical(
    calibration_precipitation_old$Observed, calibration_precipitation_new$observed,
  )
  expect_identical(
    calibration_precipitation_old$Predicted, calibration_precipitation_new$predicted,
  )

  calibration_precip_event_new <- interpolator_calibration(
    interpolator_new, variable = "PrecipitationEvent",
    N_seq = c(10, 15),
    alpha_seq = c(5, 6),
    verbose = FALSE
  )

  calibration_precip_event_old <- suppressWarnings(
    interpolation.calibration(
      interpolator_old, variable = "PrecEvent",
      N_seq = c(10, 15),
      alpha_seq = c(5, 6),
      verbose = FALSE
    )
  )
  expect_type(calibration_precip_event_old, "list")
  expect_type(calibration_precip_event_new, "list")
  expect_identical(
    tolower(names(calibration_precip_event_old)),
    tolower(names(calibration_precip_event_new))
  )
  expect_identical(
    calibration_precip_event_old$minMAE, calibration_precip_event_new$minMAE,
  )
  expect_identical(
    calibration_precip_event_old$N, calibration_precip_event_new$N,
  )
  expect_identical(
    calibration_precip_event_old$alpha, calibration_precip_event_new$alpha,
  )
  expect_identical(
    calibration_precip_event_old$Observed, calibration_precip_event_new$observed,
  )
  expect_identical(
    calibration_precip_event_old$Predicted, calibration_precip_event_new$predicted,
  )

  calibration_precip_amount_new <- interpolator_calibration(
    interpolator_new, variable = "PrecipitationAmount",
    N_seq = c(10, 15),
    alpha_seq = c(5, 6),
    verbose = FALSE
  )

  calibration_precip_amount_old <- suppressWarnings(
    interpolation.calibration(
      interpolator_old, variable = "PrecAmount",
      N_seq = c(10, 15),
      alpha_seq = c(5, 6),
      verbose = FALSE
    )
  )
  expect_type(calibration_precip_amount_old, "list")
  expect_type(calibration_precip_amount_new, "list")
  expect_identical(
    tolower(names(calibration_precip_amount_old)),
    tolower(names(calibration_precip_amount_new))
  )
  expect_identical(
    calibration_precip_amount_old$minMAE, calibration_precip_amount_new$minMAE,
  )
  expect_identical(
    calibration_precip_amount_old$N, calibration_precip_amount_new$N,
  )
  expect_identical(
    calibration_precip_amount_old$alpha, calibration_precip_amount_new$alpha,
  )
  expect_identical(
    calibration_precip_amount_old$Observed, calibration_precip_amount_new$observed,
  )
  expect_identical(
    calibration_precip_amount_old$Predicted, calibration_precip_amount_new$predicted,
  )
})

test_that("interpolation cross validation results are the same", {

  interpolation_cv_old <- suppressWarnings(
    interpolation.cv(interpolator_old, verbose = FALSE)
  )
  interpolation_cv_new <- suppressWarnings(
    interpolation_cross_validation(interpolator_new, verbose = FALSE)
  )

  # r2 slot
  names(interpolation_cv_new$r2) |>
    purrr::walk(.f = function(variable) {
      if (variable == "RangeTemperature") {
        expect_equal(interpolation_cv_old$r2$TemperatureRange, interpolation_cv_new$r2$RangeTemperature)
      } else {
        if (variable == "Radiation") {
          # radiation, due to different calculations shows a bigger difference.
          # Why is that?? because in the old workflow, smoothedTemperatureRange from the station
          # cross validated is never removed (like temp, prec, radiation...) so is used in the
          # calculation of diffTemp matrix. This matrix is only used in the radiation interpolation,
          # hence the same results in the other variables. Also, this only happens in the cross
          # validation, when removing one station from the interpolator and not in the normal
          # interpolation, so thats why in the other tests radiation works as the old workflow
          expect_equal(
            interpolation_cv_old$r2$Radiation, interpolation_cv_new$r2$Radiation,
            tolerance = 0.1
          )
        } else {
          expect_equal(interpolation_cv_old$r2[[variable]], interpolation_cv_new$r2[[variable]])
        }
      }
    })

  # station stats
  names(interpolation_cv_new$station_stats)[-c(1:2)] |>
    purrr::set_names(c(
      "MinTemperature.Bias", "MaxTemperature.Bias", "TemperatureRange.Bias", "RelativeHumidity.Bias",
      "Radiation.Bias",
      "MinTemperature.MAE", "MaxTemperature.MAE", "TemperatureRange.MAE", "RelativeHumidity.MAE",
      "Radiation.MAE",
      "TotalPrec.Obs", "TotalPrec.Pred", "TotalPrec.Bias", "TotalPrec.RelBias",
      "PrecDays.Bias", "PrecDays.RelBias", "PrecFreq.Obs", "PrecFreq.Pred"
    )) |>
    purrr::iwalk(
      .f = function(variable_new, variable_old) {
        if (variable_new %in% c("Radiation_station_bias", "Radiation_station_mae")) {
          expect_equal(
            interpolation_cv_old$stations[[variable_old]],
            interpolation_cv_new$station_stats[[variable_new]],
            tolerance = 0.2
          )
        } else {
          expect_equal(
            interpolation_cv_old$stations[[variable_old]],
            interpolation_cv_new$station_stats[[variable_new]]
          )
        }
      }
    )

  # dates stats
  names(interpolation_cv_new$dates_stats)[-1] |>
    purrr::set_names(c(
      "MinTemperature.Bias", "MaxTemperature.Bias", "TemperatureRange.Bias", "RelativeHumidity.Bias",
      "Radiation.Bias",
      "MinTemperature.MAE", "MaxTemperature.MAE", "TemperatureRange.MAE", "RelativeHumidity.MAE",
      "Radiation.MAE",
      "TotalPrec.Obs", "TotalPrec.Pred", "TotalPrec.Bias", "TotalPrec.RelBias",
      "PrecStations.Bias", "PrecStations.RelBias", "PrecFreq.Obs", "PrecFreq.Pred"
    )) |>
    purrr::iwalk(
      .f = function(variable_new, variable_old) {
        if (variable_new %in% c("Radiation_date_bias", "Radiation_date_mae")) {
          expect_equal(
            interpolation_cv_old$dates[[variable_old]],
            interpolation_cv_new$dates_stats[[variable_new]],
            tolerance = 0.2
          )
        } else {
          expect_equal(
            interpolation_cv_old$dates[[variable_old]],
            interpolation_cv_new$dates_stats[[variable_new]]
          )
        }
      }
    )

  # errors
  names(interpolation_cv_new$errors)[-c(1:3, 16:21)] |>
    purrr::set_names(c(
      "MinTemperatureError", "MaxTemperatureError", "TemperatureRangeError", "RelativeHumidityError",
      "RadiationError", "PrecipitationError",
      "MinTemperature", "MaxTemperature", "TemperatureRange", "RelativeHumidity",
      "Radiation", "Precipitation"
    )) |>
    purrr::iwalk(
      .f = function(variable_new, variable_old) {
        # random date for each variable and each test, all dates will be tested at some point
        random_date <- sample(interpolation_cv_new$errors$dates |> unique() |> as.character(), 1)
        if (variable_new %in% c("Radiation_error", "Radiation_predicted")) {
          expect_equal(
            interpolation_cv_old[[variable_old]][,random_date] |> as.numeric(),
            dplyr::filter(interpolation_cv_new$errors, as.character(dates) == random_date) |>
              dplyr::pull(variable_new) |>
              as.numeric(),
            tolerance = 0.5
          )
        } else {
          expect_identical(
            interpolation_cv_old[[variable_old]][,random_date] |> as.numeric(),
            dplyr::filter(interpolation_cv_new$errors, as.character(dates) == random_date) |>
              dplyr::pull(variable_new) |>
              as.numeric()
          )
        }
      }
    )
})

test_that("[points] interpolation results are the same", {

  # interpolation process for both
  points_old <- as(points_to_interpolate_example, "Spatial")
  points_topography <- suppressWarnings(
    SpatialPointsTopography(
      as(points_old,"SpatialPoints"), elevation = points_old$elevation,
      slope = points_old$slope, aspect = points_old$aspect
    )
  )

  interpolated_data_old <- suppressWarnings(
    interpolationpoints(interpolator_old, points_topography, verbose = FALSE)
  )
  interpolated_data_new <- suppressWarnings(
    interpolate_data(points_to_interpolate_example, interpolator_new, verbose = FALSE)
  )

  # testing differences in the interpolated data for each variable
  names(interpolated_data_old@data[[1]]) |>
    purrr::set_names(names(interpolated_data_old@data[[1]])) |>
    purrr::walk(.f = function(variable) {
      # WindDirection is not corectly calculated in the old workflow
      if (variable %in% c("WindDirection")) {
        return(invisible(TRUE))
      }

      1:length(interpolated_data_old@data) |>
        purrr::walk(
          .f = \(station_index) {
            expect_true(
              abs(mean(
                interpolated_data_old@data[[station_index]][[variable]] -
                  (interpolated_data_new |>
                     dplyr::slice(station_index) |>
                     tidyr::unnest(cols = interpolated_data))[[variable]],
                na.rm = TRUE
              )) < testthat_tolerance()
            )
            expect_true(
              sd(
                interpolated_data_old@data[[station_index]][[variable]] -
                  (interpolated_data_new |>
                     dplyr::slice(station_index) |>
                     tidyr::unnest(cols = interpolated_data))[[variable]],
                na.rm = TRUE
              ) < testthat_tolerance()
            )
          }
        )
    })
})

test_that("[raster] interpolation results are the same", {

  # skip("bug in old meteoland makes this untestable for now")

  # results from interpolationpoints and interpolationgrid are not the same for the same set of
  # coordinates. This makes the tests of integrity in interpolated rasters fail, in Humidity
  # related vars and Radiation. So for now we skip this tests

  # interpolation process for both
  raster_sgdf <- suppressWarnings(
    as(as(raster_to_interpolate_example, "Raster"), "SpatialGridDataFrame")
  )
  raster_sgt <- suppressWarnings(
    SpatialGridTopography(
      as(raster_sgdf, "SpatialGrid"), elevation = raster_sgdf$elevation,
      slope = raster_sgdf$slope, aspect = raster_sgdf$aspect
    )
  )
  # Interpolation
  interpolated_data_old <- suppressWarnings(
    interpolationgrid(interpolator_old, raster_sgt)
  )
  interpolated_data_new <- suppressWarnings(
    interpolate_data(raster_to_interpolate_example, interpolator_new, verbose = FALSE)
  )

  # testing differences in the interpolated data for each variable
  purrr::set_names(names(interpolated_data_old@data[[1]])) |>
    purrr::walk(.f = function(variable) {
      # WindDirection is not correctly calculated in the old workflow
      if (variable %in% c("WindDirection")) {
        return(invisible(TRUE))
      }

      purrr::set_names(names(interpolated_data_old@data)) |>
        purrr::walk(
          .f = \(date_index) {
            expect_true(
              abs(mean(
                interpolated_data_old@data[[date_index]][[variable]] -
                  (interpolated_data_new |>
                     dplyr::filter(as.character(date) == date_index) |>
                     dplyr::pull(variable) |>
                     as.vector()),
                na.rm = TRUE
              )) < testthat_tolerance()
            )
            expect_true(
              sd(
                interpolated_data_old@data[[date_index]][[variable]] -
                  (interpolated_data_new |>
                     dplyr::filter(as.character(date) == date_index) |>
                     dplyr::pull(variable) |>
                     as.vector()),
                na.rm = TRUE
              ) < testthat_tolerance()
            )
          }
        )
    })
})

# summarise_framework -------------------------------------------------------------------------

test_that("[points] summarise interpolated data results are the same", {
  # interpolated data (checked before for integrity)
  points_old <- as(points_to_interpolate_example, "Spatial")
  points_topography <- suppressWarnings(
    SpatialPointsTopography(
      as(points_old,"SpatialPoints"), elevation = points_old$elevation,
      slope = points_old$slope, aspect = points_old$aspect
    )
  )

  interpolated_data_old <- suppressWarnings(
    interpolationpoints(interpolator_old, points_topography, verbose = FALSE)
  )
  interpolated_data_new <- suppressWarnings(
    interpolate_data(points_to_interpolate_example, interpolator_new, verbose = FALSE)
  )

  # correct objects
  expect_s3_class(
    (summarised_data_new <-
       summarise_interpolated_data(interpolated_data_new, freq = 'week', verbose = FALSE)),
  "tbl"
  )

  # For each variable (except winddirection and the week and year)
  names(summarised_data_new$weekly_mean[[1]])[-c(1:2, 12)] |>
    purrr::set_names(names(summarised_data_new$weekly_mean[[1]])[-c(1:2, 12)]) |>
    purrr::walk(
      .f = \(var) {
        # correct object
        suppressWarnings(
          summarised_data_old <-
            summarypoints(interpolated_data_old, var = var, freq = "week")
        )
        expect_s4_class(summarised_data_old, "Spatial")

        # For each station, lets check all results are ok
        1:nrow(summarised_data_old@data) |>
          purrr::walk(
            .f = \(station) {
              expect_equal(
                summarised_data_old@data[station,] |>
                  as.numeric(),
                summarised_data_new$weekly_mean[[station]] |>
                  dplyr::arrange(year, week) |>
                  dplyr::pull(var) |>
                  as.numeric()
              )
            }
          )
      }
    )
})

test_that("[raster]  summarise interpolated data results are the same", {

  # skip("bug in old meteoland makes this untestable for now")

  # interpolation process for both
  raster_sgdf <- suppressWarnings(
    as(as(raster_to_interpolate_example, "Raster"), "SpatialGridDataFrame")
  )
  raster_sgt <- suppressWarnings(
    SpatialGridTopography(
      as(raster_sgdf, "SpatialGrid"), elevation = raster_sgdf$elevation,
      slope = raster_sgdf$slope, aspect = raster_sgdf$aspect
    )
  )
  # Interpolation
  interpolated_data_old <- suppressWarnings(
    interpolationgrid(interpolator_old, raster_sgt, verbose = FALSE)
  )
  interpolated_data_new <- suppressWarnings(
    interpolate_data(raster_to_interpolate_example, interpolator_new, verbose = FALSE)
  )

  # correct objects
  expect_s3_class(
    (summarised_data_new <-
       summarise_interpolated_data(interpolated_data_new, freq = 'week', verbose = FALSE)),
    "stars"
  )

  # For each variable (except winddirection and the week and year)
  names(summarised_data_new)[-c(10)] |>
    purrr::set_names(names(summarised_data_new)[-c(10)]) |>
    purrr::walk(
      .f = \(var) {
        # correct object
        suppressWarnings(
          summarised_data_old <-
            summarygrid(interpolated_data_old, var = var, freq = "week")
        )
        expect_s4_class(summarised_data_old, "Spatial")

        # For each station, lets check all results are ok
        1:length(summarised_data_old@data) |>
          purrr::set_names(names(summarised_data_old@data)) |>
          purrr::walk(
            .f = \(date) {
              expect_equal(
                summarised_data_old@data[[date]] |>
                  as.numeric(),
                summarised_data_new[var, date,,][[1]] |>
                  as.numeric()
              )
            }
          )
      }
    )

})
