test_that(".summary_interpolated_sf works as expected", {
  data_test <- interpolate_data(
    points_to_interpolate_example, meteoland_interpolator_example,
    verbose = FALSE
  )
  
  # nested sf
  # defaults
  expect_s3_class(
    (res_nested_defaults_test <- .summary_interpolated_sf(data_test)),
    "sf"
  )
  expect_true("summary_data" %in% names(res_nested_defaults_test))
  expect_s3_class(res_nested_defaults_test$summary_data[[1]], "data.frame")
  expect_named(
    res_nested_defaults_test$summary_data[[10]],
    c(
      "MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
      "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
      "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_true(nrow(res_nested_defaults_test$summary_data[[5]]) == 1)
  
  # frequency
  expect_s3_class(
    (res_nested_weekly_test <- .summary_interpolated_sf(data_test, frequency = "week")),
    "sf"
  )
  expect_true("summary_data" %in% names(res_nested_weekly_test))
  expect_s3_class(res_nested_weekly_test$summary_data[[1]], "data.frame")
  expect_named(
    res_nested_weekly_test$summary_data[[10]],
    c(
      "week", "year", "MeanTemperature", "MinTemperature","MaxTemperature",
      "Precipitation", "MeanRelativeHumidity", "MinRelativeHumidity",
      "MaxRelativeHumidity", "Radiation", "WindSpeed", "WindDirection", "PET"
    )
  )
  expect_true(nrow(res_nested_weekly_test$summary_data[[5]]) == 6)
  
  # dates to summary
  
  
})
