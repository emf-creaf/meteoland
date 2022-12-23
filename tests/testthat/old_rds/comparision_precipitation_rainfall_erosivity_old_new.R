# CONSISTENCY BETWEEN precipitation_rainfallErosivity and
# precipitation_rainfall_erosivity

test_data <- interpolate_data(
  points_to_interpolate_example, meteoland_interpolator_example,
  verbose = FALSE
)

meteo_data <- test_data[["interpolated_data"]][[1]]

meteo_data_old <- meteo_data |>
  tibble::column_to_rownames(var = "dates")

precipitation_rainfallErosivity(
  meteo_data_old, long = 1, scale = 'month', average = TRUE
)

# several months
meteo_data_months <- meteo_data |>
  dplyr::mutate(dates = dates + lubridate::dmonths(1)) |>
  dplyr::bind_rows(meteo_data) |>
  dplyr::arrange(dates)

meteo_data_months_old <- meteo_data_months |>
  tibble::column_to_rownames(var = "dates")

precipitation_rainfallErosivity(
  meteo_data_months_old, long = 1, scale = 'month', average = TRUE
)
precipitation_rainfallErosivity(
  meteo_data_months_old, long = 1, scale = 'month', average = FALSE
)

precipitation_rainfall_erosivity(
  meteo_data_months, longitude = 1, scale = "month", average = TRUE
)

precipitation_rainfall_erosivity(
  meteo_data_months, longitude = 1, scale = "month", average = FALSE
)

# several years
meteo_data_years <- meteo_data |>
  dplyr::mutate(
    dates = dates + lubridate::dyears(1),
    # randomly generated precipitation to be different from the past year
    Precipitation = rbinom(30, 1, 0.3)*rnorm(30, 3, 1)
  ) |>
  dplyr::bind_rows(meteo_data) |>
  dplyr::arrange(dates)

meteo_data_years_old <- meteo_data_years |>
  tibble::column_to_rownames(var = "dates")

precipitation_rainfallErosivity(
  meteo_data_years_old, long = 1, scale = 'month', average = TRUE
)
precipitation_rainfallErosivity(
  meteo_data_years_old, long = 1, scale = 'month', average = FALSE
)

precipitation_rainfall_erosivity(
  meteo_data_years, longitude = 1, scale = "month", average = TRUE
)

precipitation_rainfall_erosivity(
  meteo_data_years, longitude = 1, scale = "month", average = FALSE
)

precipitation_rainfallErosivity(
  meteo_data_years_old, long = 1, scale = 'year', average = TRUE
)
precipitation_rainfallErosivity(
  meteo_data_years_old, long = 1, scale = 'year', average = FALSE
)

precipitation_rainfall_erosivity(
  meteo_data_years, longitude = 1, scale = "year", average = TRUE
)

precipitation_rainfall_erosivity(
  meteo_data_years, longitude = 1, scale = "year", average = FALSE
)
