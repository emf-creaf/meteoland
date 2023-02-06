test_that("summarise_interpolated_data for sf works as expected", {
  data_test <- interpolate_data(
    points_to_interpolate_example, meteoland_interpolator_example,
    verbose = FALSE
  )

  # errors
  # summarise_interpolated_data arguments errors
  expect_error(
    summarise_interpolated_data(meteoland_topo_example),
    "interpolated_data"
  )
  expect_error(
    summarise_interpolated_data(25),
    "provided is not"
  )
  expect_error(
    summarise_interpolated_data(data_test, fun = 25),
    "fun"
  )
  expect_error(
    summarise_interpolated_data(data_test, fun = "tururu"),
    "tururu"
  )
  expect_error(
    summarise_interpolated_data(data_test, frequency = 25),
    "frequency"
  )
  expect_error(
    summarise_interpolated_data(data_test, frequency = "tururu"),
    "should be one of"
  )
  expect_error(
    summarise_interpolated_data(data_test, vars_to_summary = 25),
    "vars_to_summary"
  )
  expect_error(
    summarise_interpolated_data(data_test, vars_to_summary = "tururu"),
    "vars_to_summary"
  )
  expect_error(
    summarise_interpolated_data(data_test, dates_to_summary = 25),
    "dates_to_summary"
  )
  expect_error(
    summarise_interpolated_data(data_test, months_to_summary = "tururu"),
    "months_to_summary"
  )

  # nested sf
  # defaults
  expect_s3_class(
    (res_nested_defaults_test <- summarise_interpolated_data(data_test)),
    "sf"
  )
  expect_true("all_mean" %in% names(res_nested_defaults_test))
  expect_s3_class(res_nested_defaults_test$all_mean[[1]], "data.frame")
  expect_named(
    res_nested_defaults_test$all_mean[[10]],
    c(
      "MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_true(nrow(res_nested_defaults_test$all_mean[[5]]) == 1)
  # test for data summarised, not NA or NaN
  expect_false(
    is.na(res_nested_defaults_test$all_mean[[6]]$MeanTemperature) ||
      is.nan(res_nested_defaults_test$all_mean[[6]]$MeanTemperature)
  )
  # test that summary dfs are not grouped
  expect_false(inherits(res_nested_defaults_test$all_mean[[8]], "grouped_df"))

  # frequency
  expect_s3_class(
    (res_nested_weekly_test <- summarise_interpolated_data(data_test, frequency = "week")),
    "sf"
  )
  expect_true("weekly_mean" %in% names(res_nested_weekly_test))
  expect_s3_class(res_nested_weekly_test$weekly_mean[[1]], "data.frame")
  expect_named(
    res_nested_weekly_test$weekly_mean[[10]],
    c(
      "week", "year", "MeanTemperature", "MinTemperature","MaxTemperature",
      "Precipitation", "MeanRelativeHumidity", "MinRelativeHumidity",
      "MaxRelativeHumidity", "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_true(nrow(res_nested_weekly_test$weekly_mean[[5]]) == 5)
  # test for data summarised, not NA or NaN
  expect_false(
    all(is.na(res_nested_weekly_test$weekly_mean[[6]]$MeanTemperature)) ||
      all(is.nan(res_nested_weekly_test$weekly_mean[[6]]$MeanTemperature))
  )
  # test that summary dfs are not grouped
  expect_false(inherits(res_nested_weekly_test$weekly_mean[[8]], "grouped_df"))

  # dates to summary
  dates_to_summary_test <- as.Date(unique(data_test$interpolated_data[[3]]$dates)[1:5])
  expect_message(
    (res_nested_dates_test <-
       summarise_interpolated_data(data_test, dates_to_summary = dates_to_summary_test)),
    "Filtering the desired dates"
  )
  expect_s3_class(res_nested_dates_test, "sf")
  expect_true("all_mean" %in% names(res_nested_dates_test))
  expect_s3_class(res_nested_dates_test$all_mean[[1]], "data.frame")
  expect_named(
    res_nested_dates_test$all_mean[[10]],
    c(
      "MeanTemperature", "MinTemperature","MaxTemperature",
      "Precipitation", "MeanRelativeHumidity", "MinRelativeHumidity",
      "MaxRelativeHumidity", "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_true(nrow(res_nested_dates_test$all_mean[[5]]) == 1)
  # test for data summarised, not NA or NaN
  expect_false(
    is.na(res_nested_dates_test$all_mean[[6]]$MeanTemperature) ||
      is.nan(res_nested_dates_test$all_mean[[6]]$MeanTemperature)
  )
  # test that summary dfs are not grouped
  expect_false(inherits(res_nested_dates_test$all_mean[[8]], "grouped_df"))

  # What happens when the dates supplied are not in the data??
  # TODO

  # months_to_summary
  months_to_summary_test <- 4
  expect_s3_class(
    (res_nested_months_test <- summarise_interpolated_data(data_test, months_to_summary = months_to_summary_test)),
    "sf"
  )
  expect_true("all_mean" %in% names(res_nested_months_test))
  expect_s3_class(res_nested_months_test$all_mean[[1]], "data.frame")
  expect_named(
    res_nested_months_test$all_mean[[10]],
    c(
      "MeanTemperature", "MinTemperature","MaxTemperature",
      "Precipitation", "MeanRelativeHumidity", "MinRelativeHumidity",
      "MaxRelativeHumidity", "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_true(nrow(res_nested_months_test$all_mean[[5]]) == 1)
  # test for data summarised, not NA or NaN
  expect_false(
    is.na(res_nested_months_test$all_mean[[6]]$MeanTemperature) ||
      is.nan(res_nested_months_test$all_mean[[6]]$MeanTemperature)
  )

  # as data is only april, this result should be identical to the base one
  expect_identical(res_nested_months_test, res_nested_defaults_test)

  # What happens when the months supplied are not in the data??
  expect_s3_class(
    suppressWarnings(
      res_nested_months_bad_test <- summarise_interpolated_data(data_test, months_to_summary = 1:2)
    ),
    "sf"
  )
  # expecting NA or NaN
  expect_true(
    is.na(res_nested_months_bad_test$all_mean[[6]]$MeanTemperature) ||
      is.nan(res_nested_months_bad_test$all_mean[[6]]$MeanTemperature)
  )
  # test that summary dfs are not grouped
  expect_false(inherits(res_nested_months_test$all_mean[[8]], "grouped_df"))
  expect_false(inherits(res_nested_months_bad_test$all_mean[[8]], "grouped_df"))

  # vars_to_summary
  vars_to_summary_test <- c("Precipitation", "Radiation")
  expect_s3_class(
    (res_nested_vars_test <- summarise_interpolated_data(data_test, vars_to_summary = vars_to_summary_test)),
    "sf"
  )
  expect_true("all_mean" %in% names(res_nested_vars_test))
  expect_s3_class(res_nested_vars_test$all_mean[[1]], "data.frame")
  expect_named(res_nested_vars_test$all_mean[[10]], c("Precipitation", "Radiation"))
  expect_true(nrow(res_nested_vars_test$all_mean[[5]]) == 1)
  # test for data summarised, not NA or NaN
  expect_false(
    is.na(res_nested_vars_test$all_mean[[6]]$Precipitation) ||
      is.nan(res_nested_vars_test$all_mean[[6]]$Precipitation)
  )
  # test that summary dfs are not grouped
  expect_false(inherits(res_nested_vars_test$all_mean[[8]], "grouped_df"))

  # integration tests
  expect_s3_class(
    (res_nested_integration_test <- summarise_interpolated_data(
      data_test,
      fun = "max",
      frequency = "week",
      vars_to_summary = c("Precipitation", "Radiation"),
      dates_to_summary = as.Date(c("2022-04-01", "2022-04-02")),
      months_to_summary = 4,
      na.rm = TRUE,
      verbose = FALSE
    )),
    "sf"
  )
  expect_true("weekly_max" %in% names(res_nested_integration_test))
  expect_s3_class(res_nested_integration_test$weekly_max[[1]], "data.frame")
  expect_named(
    res_nested_integration_test$weekly_max[[10]],
    c("week", "year", "Precipitation", "Radiation")
  )
  expect_true(nrow(res_nested_integration_test$weekly_max[[5]]) == 1)
  # test for data summarised, not NA or NaN
  expect_false(
    all(is.na(res_nested_integration_test$weekly_max[[6]]$Radiation)) ||
      all(is.nan(res_nested_integration_test$weekly_max[[6]]$Radiation))
  )
  # test that summary dfs are not grouped
  expect_false(inherits(res_nested_integration_test$weekly_max[[8]], "grouped_df"))

  # TODO
  # tests for unnested
  # test for individual data frames

})

test_that("summarise_interpolated_data for stars works as expected", {
  data_test <- interpolate_data(
    raster_to_interpolate_example, meteoland_interpolator_example,
    verbose = FALSE
  )

  data_test_no_interpolation <- data_test[c("elevation", "slope", "aspect"),,,1]

  # errors
  expect_error(
    summarise_interpolated_data(data_test_no_interpolation), "interpolated_data"
  )

  # defaults
  expect_s3_class(
    (res_defaults_test <- summarise_interpolated_data(data_test)),
    "stars"
  )
  expect_named(
    res_defaults_test,
    c(
      "MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_length(stars::st_get_dimension_values(res_defaults_test, "time"), 1)
  # test for data summarised, not NA or NaN
  expect_false(
    all(is.na(res_defaults_test$MeanTemperature)) ||
      all(is.nan(res_defaults_test$MeanTemperature))
  )

  # frequency
  expect_s3_class(
    (res_frequency_test <- summarise_interpolated_data(data_test, frequency = "week")),
    "stars"
  )
  expect_named(
    res_frequency_test,
    c(
      "MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_length(stars::st_get_dimension_values(res_frequency_test, "time"), 5)
  # test for data summarised, not NA or NaN
  expect_false(
    all(is.na(res_frequency_test$MeanTemperature)) ||
      all(is.nan(res_frequency_test$MeanTemperature))
  )

  # dates_to_summary
  dates_to_summary_test <- as.Date(stars::st_get_dimension_values(data_test, "date")[1:5])
  expect_message(
    (res_dates_test <- summarise_interpolated_data(data_test, dates_to_summary = dates_to_summary_test)),
    "Filtering the desired dates"
  )
  expect_s3_class(res_dates_test, "stars")
  expect_named(
    res_dates_test,
    c(
      "MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_length(stars::st_get_dimension_values(res_dates_test, "time"), 1)
  # test for data summarised, not NA or NaN
  expect_false(
    all(is.na(res_dates_test$MeanTemperature)) ||
      all(is.nan(res_dates_test$MeanTemperature))
  )

  # What happens when the dates supplied are not in the data??
  # TODO

  # months_to_summary
  months_to_summary_test <- 4
  expect_s3_class(
    (res_months_test <- summarise_interpolated_data(data_test, months_to_summary = months_to_summary_test)),
    "stars"
  )
  expect_named(
    res_months_test,
    c(
      "MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_length(stars::st_get_dimension_values(res_months_test, "time"), 1)
  # test for data summarised, not NA or NaN
  expect_false(
    all(is.na(res_months_test$MeanTemperature)) ||
      all(is.nan(res_months_test$MeanTemperature))
  )

  # as data is only april, this result should be identical to the base one
  expect_identical(res_months_test, res_defaults_test)

  # What happens when the months supplied are not in the data??
  expect_error(
    summarise_interpolated_data(data_test, months_to_summary = 1:2),
    "Selected months"
  )

  # vars_to_summary
  vars_to_summary_test <- c("Precipitation", "Radiation")
  expect_s3_class(
    (res_vars_test <- summarise_interpolated_data(data_test, vars_to_summary = vars_to_summary_test)),
    "stars"
  )
  expect_named(res_vars_test, c("Precipitation", "Radiation"))
  expect_length(stars::st_get_dimension_values(res_vars_test, "time"), 1)
  # test for data summarised, not NA or NaN
  expect_false(
    all(is.na(res_vars_test$Precipitation)) ||
      all(is.nan(res_vars_test$Precipitation))
  )

  # integration tests
  expect_s3_class(
    (res_integration_test <- summarise_interpolated_data(
      data_test,
      fun = "max",
      frequency = "week",
      vars_to_summary = c("Precipitation", "Radiation"),
      dates_to_summary = as.Date(c("2022-04-01", "2022-04-02")),
      months_to_summary = 4,
      na.rm = TRUE,
      verbose = FALSE
    )),
    "stars"
  )
  expect_named(res_integration_test, c("Precipitation", "Radiation"))
  expect_length(stars::st_get_dimension_values(res_integration_test, "time"), 1)
  # test for data summarised, not NA or NaN
  expect_false(
    all(is.na(res_integration_test$Precipitation)) ||
      all(is.nan(res_integration_test$Precipitation))
  )

})


test_that("summarise_interpolator works as expected", {

  data_test <- meteoland_interpolator_example

  # errors
  # summarise_interpolator arguments errors
  expect_error(
    summarise_interpolator(meteoland_topo_example),
    "interpolator"
  )
  expect_error(
    summarise_interpolator(
      interpolate_data(
        raster_to_interpolate_example, meteoland_interpolator_example,
        verbose = FALSE
      )
    ),
    "interpolator"
  )
  expect_error(
    summarise_interpolator(25),
    "interpolator"
  )
  expect_error(
    summarise_interpolator(data_test, fun = 25),
    "fun"
  )
  expect_error(
    summarise_interpolator(data_test, fun = "tururu"),
    "tururu"
  )
  expect_error(
    summarise_interpolator(data_test, frequency = 25),
    "frequency"
  )
  expect_error(
    summarise_interpolator(data_test, frequency = "tururu"),
    "should be one of"
  )
  expect_error(
    summarise_interpolator(data_test, vars_to_summary = 25),
    "vars_to_summary"
  )
  expect_error(
    summarise_interpolator(data_test, vars_to_summary = "tururu"),
    "vars_to_summary"
  )
  expect_error(
    summarise_interpolator(data_test, dates_to_summary = 25),
    "dates_to_summary"
  )
  expect_error(
    summarise_interpolator(data_test, months_to_summary = "tururu"),
    "months_to_summary"
  )

  # default tests
  expect_s3_class(
    (res_interp_defaults <- summarise_interpolator(data_test)),
    "stars"
  )
  # result must be an interpolator
  expect_true(.is_interpolator(res_interp_defaults))
  # result must have the same names
  expect_named(res_interp_defaults, names(data_test), ignore.order = TRUE)
  # result has to have a frequency dependent number of dates
  expect_length(stars::st_get_dimension_values(res_interp_defaults, "date"), 1)
  # result has to have the same stations
  expect_identical(ncol(res_interp_defaults), ncol(data_test))
  # result has to have values for the variables that had values in the interpolator
  expect_false(all(is.na(res_interp_defaults[["MinTemperature"]])))
  # result has not gaps in the topo info
  expect_false(any(is.na(res_interp_defaults[["elevation"]])))
  # result shouldn't have change the values for topo, also, colrownames must be the same
  expect_identical(res_interp_defaults[["elevation"]][1,], data_test[["elevation"]][1,])

  # frequency
  expect_s3_class(
    (res_interp_week <- summarise_interpolator(data_test, frequency = "week")),
    'stars'
  )
  # result must be an interpolator
  expect_true(.is_interpolator(res_interp_week))
  # result must have the same names
  expect_named(res_interp_week, names(data_test), ignore.order = TRUE)
  # result has to have a frequency dependent number of dates
  expect_length(stars::st_get_dimension_values(res_interp_week, "date"), 5)
  # result has to have the same stations
  expect_identical(ncol(res_interp_week), ncol(data_test))
  # result has to have values for the variables that had values in the interpolator
  expect_false(all(is.na(res_interp_week[["MinTemperature"]])))
  # result has not gaps in the topo info
  expect_false(any(is.na(res_interp_week[["elevation"]])))
  # result shouldn't have change the values for topo, also, colrownames must be the same
  expect_identical(res_interp_week[["elevation"]][1,], data_test[["elevation"]][1,])

  # dates_to_summary
  dates_to_summary_test <-
    as.Date(stars::st_get_dimension_values(data_test, "date")[1:5])
  expect_s3_class(
    (res_interp_dates <-
       summarise_interpolator(data_test, dates_to_summary = dates_to_summary_test)),
    'stars'
  )
  # result must be an interpolator
  expect_true(.is_interpolator(res_interp_dates))
  # result must have the same names
  expect_named(res_interp_dates, names(data_test), ignore.order = TRUE)
  # result has to have a frequency dependent number of dates
  expect_length(stars::st_get_dimension_values(res_interp_dates, "date"), 1)
  # result has to have the same stations
  expect_identical(ncol(res_interp_dates), ncol(data_test))
  # result has to have values for the variables that had values in the interpolator
  expect_false(all(is.na(res_interp_dates[["MinTemperature"]])))
  # result has not gaps in the topo info
  expect_false(any(is.na(res_interp_dates[["elevation"]])))
  # result shouldn't have change the values for topo, also, colrownames must be the same
  expect_identical(res_interp_dates[["elevation"]][1,], data_test[["elevation"]][1,])
  # result must be different from the default result, as we are only summarising some dates
  expect_false(
    identical(res_interp_dates[["MinTemperature"]], res_interp_defaults[["MinTemperature"]])
  )
  # What happens when the dates supplied are not in the data??
  # TODO

  # months_to_summary
  months_to_summary_test <- 4
  expect_s3_class(
    (res_interp_months <-
       summarise_interpolator(data_test, months_to_summary = months_to_summary_test)),
    'stars'
  )
  # result must be an interpolator
  expect_true(.is_interpolator(res_interp_months))
  # result must have the same names
  expect_named(res_interp_months, names(data_test), ignore.order = TRUE)
  # result has to have a frequency dependent number of dates
  expect_length(stars::st_get_dimension_values(res_interp_months, "date"), 1)
  # result has to have the same stations
  expect_identical(ncol(res_interp_months), ncol(data_test))
  # result has to have values for the variables that had values in the interpolator
  expect_false(all(is.na(res_interp_months[["MinTemperature"]])))
  # result has not gaps in the topo info
  expect_false(any(is.na(res_interp_months[["elevation"]])))
  # result shouldn't have change the values for topo, also, colrownames must be the same
  expect_identical(res_interp_months[["elevation"]][1,], data_test[["elevation"]][1,])
  # result must be equal from the default result, as we are summarising the only
  # present month
  expect_identical(
    res_interp_months[["MinTemperature"]], res_interp_defaults[["MinTemperature"]]
  )
  # What happens when the months supplied are not in the data??
  expect_error(
    summarise_interpolator(data_test, months_to_summary = 1:2),
    "Selected months"
  )

  # vars_to_summary
  vars_to_summary_test <- c("Precipitation", "Radiation")
  expect_s3_class(
    (res_interp_vars <-
       summarise_interpolator(data_test, vars_to_summary = vars_to_summary_test)),
    'stars'
  )
  # result must be an interpolator, but as we remove the mandatory variables, then
  # is not
  expect_error(.is_interpolator(res_interp_vars), "Names found")
  # result must have the same names
  expect_named(res_interp_vars, c("Precipitation", "Radiation"), ignore.order = TRUE)
  # result has to have a frequency dependent number of dates
  expect_length(stars::st_get_dimension_values(res_interp_vars, "date"), 1)
  # result has to have the same stations
  expect_identical(ncol(res_interp_vars), ncol(data_test))
  # result has to have values for the variables that had values in the interpolator
  expect_false(all(is.na(res_interp_vars[["Precipitation"]])))
  # result must be equal from the default result, as we are summarising the only
  # present month
  expect_identical(
    res_interp_vars[["Precipitation"]], res_interp_defaults[["Precipitation"]]
  )

  # integration tests
  expect_s3_class(
    (res_interp_integration <- summarise_interpolator(
      data_test,
      fun = "max",
      frequency = "week",
      vars_to_summary = c("Precipitation", "Radiation"),
      dates_to_summary = as.Date(c("2022-04-01", "2022-04-02")),
      months_to_summary = 4,
      na.rm = TRUE,
      verbose = FALSE
    )),
    "stars"
  )
  # result must be an interpolator, but as we remove the mandatory variables, then
  # is not
  expect_error(.is_interpolator(res_interp_integration), "Names found")
  # result must have the same names
  expect_named(res_interp_integration, c("Precipitation", "Radiation"), ignore.order = TRUE)
  # result has to have a frequency dependent number of dates
  expect_length(stars::st_get_dimension_values(res_interp_integration, "date"), 1)
  # result has to have the same stations
  expect_identical(ncol(res_interp_integration), ncol(data_test))
  # result has to have values for the variables that had values in the interpolator
  expect_false(all(is.na(res_interp_integration[["Precipitation"]])))
  # result must be different from the week result, as we are using only two dates
  expect_false(
    identical(res_interp_integration[["Precipitation"]], res_interp_week[["Precipitation"]])
  )

})
