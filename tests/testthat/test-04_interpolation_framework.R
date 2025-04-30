test_that(".stars2sf works as intended", {
  expect_error(.stars2sf("wrong_object"), "stars class")
  expect_error(.stars2sf(raster_to_interpolate_example[c("slope", "aspect")]), "No elevation")
  expect_s3_class((res <- .stars2sf(raster_to_interpolate_example)), "sf")
  expect_named(res, c("elevation", "slope", "aspect", "geometry"))
  expect_identical(
    as.integer(nrow(raster_to_interpolate_example) * ncol(raster_to_interpolate_example)),
    nrow(res)
  )
  # geometries in both, stars and sf, must be the same
  expect_identical(
    as.numeric(sf::st_coordinates(res)[,1]),
    as.numeric(sf::st_coordinates(raster_to_interpolate_example)[,1])
  )
  expect_identical(
    as.numeric(sf::st_coordinates(res)[,2]),
    as.numeric(sf::st_coordinates(raster_to_interpolate_example)[,2])
  )
})

test_that(".is_spatial_data, .is_raster works as intended", {
  # .is_spatial_data
  expect_false(.is_spatial_data(25))
  expect_false(.is_spatial_data("25"))
  # expect_false(.is_spatial_data(terra::rast(raster_to_interpolate_example)))
  # expect_false(.is_spatial_data(sf::as_Spatial(points_to_interpolate_example)))
  expect_true(.is_spatial_data(points_to_interpolate_example))
  expect_true(.is_spatial_data(raster_to_interpolate_example))
  # .is_raster
  expect_false(.is_raster(meteoland_interpolator_example))
  expect_true(.is_raster(raster_to_interpolate_example))
})

test_that("interpolation process works as intended", {
  # interpolation varaibles names
  interpolation_var_names <- c(
    "MeanTemperature", "MinTemperature", "MaxTemperature", "Precipitation",
    "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
    "Radiation", "WindSpeed", "WindDirection", "PET"
  )

  # argument errors
  expect_error(
    interpolate_data(25, meteoland_interpolator_example), "must be"
  )
  expect_error(
    interpolate_data(meteoland_interpolator_example, meteoland_interpolator_example), "vector data cube"
  )
  expect_error(
    interpolate_data(sf::st_transform(raster_to_interpolate_example, crs = 4326), meteoland_interpolator_example),
    "Curvilinear grids"
  )
  expect_error(
    interpolate_data(raster_to_interpolate_example, meteoland_interpolator_example, dates = 'tururu'),
    "dates object provided"
  )
  expect_error(
    interpolate_data(raster_to_interpolate_example, meteoland_interpolator_example, variables = 'tururu'),
    "variables argument must be"
  )
  expect_error(interpolate_data(raster_to_interpolate_example, raster_to_interpolate_example), NULL)
  expect_warning(
    interpolate_data(dplyr::select(points_to_interpolate_example, -slope), meteoland_interpolator_example),
    "not mandatory"
  )

  # points
  expect_s3_class(
    (points_res <- interpolate_data(points_to_interpolate_example, meteoland_interpolator_example)),
    'sf'
  )
  expect_identical(nrow(points_res), nrow(points_to_interpolate_example))
  expect_true(all(names(points_to_interpolate_example) %in% names(points_res)))
  expect_true("interpolated_data" %in% names(points_res))
  expect_true(all(
    c(interpolation_var_names, "dates", "DOY") %in%
      names(points_res |> tidyr::unnest(cols = c(interpolated_data)))
  ))
  expect_identical(sf::st_geometry(points_res), sf::st_geometry(points_to_interpolate_example))
  expect_identical(sf::st_crs(points_res), sf::st_crs(points_to_interpolate_example))
  # we expect all vars with data, as we have everything we need to interpolate/calculate any
  # variable
  expect_true(
    (points_res$interpolated_data[[1]] |> dplyr::select(where(\(x) {all(is.na(x))})) |> ncol()) < 1
  )

  # raster
  expect_s3_class(
    (raster_res <- interpolate_data(raster_to_interpolate_example, meteoland_interpolator_example)),
    'stars'
  )
  expect_identical(dim(raster_res)[1:2], dim(raster_to_interpolate_example)[1:2])
  expect_true(all(names(raster_to_interpolate_example) %in% names(raster_res)))
  expect_true(all(interpolation_var_names %in% names(raster_res)))
  expect_identical(sf::st_crs(raster_res), sf::st_crs(raster_to_interpolate_example))
  # we expect all vars with data, as we have everything we need to interpolate/calculate any
  # variable
  purrr::map(
    purrr::set_names(names(raster_res)),
    .f = \(x) {expect_false(all(is.na(raster_res[[x]])))}
  )

  # date checks
  dates_all_ok <- as.Date(c("2022-04-24", "2022-04-25", "2022-04-26"))
  dates_some_ok <- c(dates_all_ok, "2022-05-13")
  dates_none_ok <- as.Date(c("2022-05-01", "2022-05-02"))
  expect_s3_class(
    (points_res_dates <- interpolate_data(
      points_to_interpolate_example, meteoland_interpolator_example, dates_all_ok
    )),
    'sf'
  )
  expect_warning(
    interpolate_data(raster_to_interpolate_example, meteoland_interpolator_example, dates_some_ok),
    "Some `dates` are outside the `interpolator` date range, only dates inside will be used"
  )
  expect_error(
    interpolate_data(raster_to_interpolate_example, meteoland_interpolator_example, dates_none_ok),
    "`dates` supplied are outside the `interpolator` date range. No possible interpolation."
  )
  expect_identical(nrow(points_res_dates), nrow(points_to_interpolate_example))
  expect_true(all(names(points_to_interpolate_example) %in% names(points_res_dates)))
  expect_true("interpolated_data" %in% names(points_res_dates))
  expect_true(all(
    c(interpolation_var_names, "dates", "DOY") %in%
      names(points_res_dates |> tidyr::unnest(cols = c(interpolated_data)))
  ))
  expect_identical(sf::st_geometry(points_res_dates), sf::st_geometry(points_to_interpolate_example))
  expect_identical(sf::st_crs(points_res_dates), sf::st_crs(points_to_interpolate_example))
  expect_identical(
    nrow(points_res_dates[["interpolated_data"]][[1]]), 3L
  )
  expect_identical(
    points_res_dates[["interpolated_data"]][[1]]$dates, dates_all_ok
  )

  points_all_ok <- points_to_interpolate_example
  points_one_bad <- points_all_ok |>
    dplyr::mutate(
      geometry = points_all_ok$geometry + c(5, rep(0, length(points_all_ok$geometry) - 1))
    ) |>
    sf::st_set_crs(4326)
  points_more_bad <- points_all_ok |>
    dplyr::mutate(
      geometry = points_all_ok$geometry + c(5, 5, 5, rep(0, length(points_all_ok$geometry) - 3))
    ) |>
    sf::st_set_crs(4326)

  expect_s3_class(interpolate_data(points_all_ok, meteoland_interpolator_example),'sf')
  expect_warning(
    interpolate_data(points_one_bad, meteoland_interpolator_example),
    'Some points are outside the convex hull'
  )
  expect_error(
    interpolate_data(points_more_bad, meteoland_interpolator_example),
    'More than 10% of the points'
  )

  # variable argument points
  expect_s3_class(
    (points_radiation_res <-
       interpolate_data(points_to_interpolate_example, meteoland_interpolator_example, variables = "Radiation")),
    'sf'
  )
  expect_identical(
    points_res$interpolated_data[[1]]$Radiation,
    points_radiation_res$interpolated_data[[1]]$Radiation
  )
  expect_identical(
    points_res$interpolated_data[[1]]$MinTemperature,
    points_radiation_res$interpolated_data[[1]]$MinTemperature
  )
  expect_identical(
    points_res$interpolated_data[[1]]$MeanRelativeHumidity,
    points_radiation_res$interpolated_data[[1]]$MeanRelativeHumidity
  )
  expect_identical(
    points_res$interpolated_data[[1]]$Precipitation,
    points_radiation_res$interpolated_data[[1]]$Precipitation
  )
  expect_true(
    all(is.na(points_radiation_res$interpolated_data[[1]]$WindDirection))
  )
  expect_true(
    all(is.na(points_radiation_res$interpolated_data[[1]]$PET))
  )

  expect_s3_class(
    (points_twovars_res <-
       interpolate_data(points_to_interpolate_example, meteoland_interpolator_example, variables = c("Precipitation", "Wind"))),
    'sf'
  )
  expect_identical(
    points_res$interpolated_data[[1]]$Precipitation,
    points_twovars_res$interpolated_data[[1]]$Precipitation
  )
  expect_identical(
    points_res$interpolated_data[[1]]$WindDirection,
    points_twovars_res$interpolated_data[[1]]$WindDirection
  )
  expect_true(
    all(is.na(points_twovars_res$interpolated_data[[1]]$MinTemperature))
  )
  expect_true(
    all(is.na(points_twovars_res$interpolated_data[[1]]$Radiation))
  )
  expect_true(
    all(is.na(points_twovars_res$interpolated_data[[1]]$MeanRelativeHumidity))
  )
  expect_true(
    all(is.na(points_twovars_res$interpolated_data[[1]]$PET))
  )

  # variables argument raster
  expect_s3_class(
    (raster_radiation_res <-
       interpolate_data(raster_to_interpolate_example, meteoland_interpolator_example, variables = "Radiation")),
    'stars'
  )
  expect_identical(
    raster_res[["Radiation"]],
    raster_radiation_res[["Radiation"]]
  )
  expect_identical(
    raster_res[["MinTemperature"]],
    raster_radiation_res[["MinTemperature"]]
  )
  expect_identical(
    raster_res[["MeanRelativeHumidity"]],
    raster_radiation_res[["MeanRelativeHumidity"]]
  )
  expect_identical(
    raster_res[["Precipitation"]],
    raster_radiation_res[["Precipitation"]]
  )
  expect_true(
    all(is.na(raster_radiation_res[["WindDirection"]]))
  )
  expect_true(
    all(is.na(raster_radiation_res[["PET"]]))
  )

  expect_s3_class(
    (raster_twovars_res <-
       interpolate_data(raster_to_interpolate_example, meteoland_interpolator_example, variables = c("Precipitation", "Wind"))),
    'stars'
  )
  expect_identical(
    raster_res[["WindDirection"]],
    raster_twovars_res[["WindDirection"]]
  )
  expect_identical(
    raster_res[["Precipitation"]],
    raster_twovars_res[["Precipitation"]]
  )
  expect_true(
    all(is.na(raster_twovars_res[["MinTemperature"]]))
  )
  expect_true(
    all(is.na(raster_twovars_res[["MeanRelativeHumidity"]]))
  )
  expect_true(
    all(is.na(raster_twovars_res[["Radiation"]]))
  )
  expect_true(
    all(is.na(raster_twovars_res[["PET"]]))
  )

  # testing that missing variables in the interpolator behaves as they should
  #   - no relative humidity, then it is interpolated with temperatures, so the
  #     variable has data in the res
  #   - no precipitation, then precipitation is NA in the results, but radiation
  #     is interpolated as clear days, with a warning to the user
  interpolator_test_no_relative_humidity <- meteoland_interpolator_example
  interpolator_test_no_relative_humidity[["RelativeHumidity"]] <- NA_real_
  interpolator_test_no_precipitation <- meteoland_interpolator_example
  interpolator_test_no_precipitation[["Precipitation"]] <- NA_real_
  interpolator_test_no_precipitation[["SmoothedPrecipitation"]] <- NA_real_
  interpolator_test_no_precipitation[["Radiation"]] <- NA_real_

  expect_s3_class(
    points_res_no_relative_humidity <-
      interpolate_data(points_to_interpolate_example, interpolator_test_no_relative_humidity),
    "sf"
  )
  1:length(unique(points_res_no_relative_humidity$plot_id)) |>
    purrr::walk(
      .f = \(point) {
        expect_false(
          any(is.na(points_res_no_relative_humidity$interpolated_data[[point]]$MeanRelativeHumidity))
        )
        expect_false(
          any(is.na(points_res_no_relative_humidity$interpolated_data[[point]]$MinRelativeHumidity))
        )
        expect_false(
          any(is.na(points_res_no_relative_humidity$interpolated_data[[point]]$MaxRelativeHumidity))
        )
      }
    )

  expect_warning(
    points_res_no_precipitation <-
      interpolate_data(points_to_interpolate_example, interpolator_test_no_precipitation),
    "assuming clear days"
  )

  1:length(unique(points_res_no_precipitation$plot_id)) |>
    purrr::walk(
      .f = \(point) {
        expect_true(
          all(is.na(points_res_no_precipitation$interpolated_data[[point]]$Precipitation))
        )
        expect_false(
          any(is.na(points_res_no_precipitation$interpolated_data[[point]]$Radiation))
        )
      }
    )
  
  # test interpolation when using N_*Temperature < 10 and NAs are generated when
  # weights become zero.

  # Browse[1]> selected_stations_coords[station, 1]
  #       X 
  # 0.66789 

  # Browse[1]> selected_stations_coords[station, 2]
  #        Y 
  # 41.35991 
  # Browse[1]> selected_stations_elevation[station]
  #  UM 
  # 505 
  # Browse[1]> stations_elevation[-original_station_index] |> names()
  # Rp 1.13
  req_stations <- c(
    "C6", "C7", "C8", "C9", "CC", "CD", "CE", "CG", "CI", "CJ", "CL", "CP",
    "CQ", "CR", "CT", "CW", "CY", "D1", "D2", "D3", "D4", "D5", "D6", "D7",
    "D8", "D9", "DB", "DF", "DG", "DI", "DJ", "DK", "DL",
    "DN", "DO", "DP", "DQ", "H1", "J5", "KP", "MQ", "MR", "MS", "MV", "U1",
    "U2", "U3", "U4", "U6", "U7", "U9", "UA", "UB", "UE", "UF", "UG", "UH",
    "UI", "UJ", "UK", "UN", "UO", "UP", "UQ", "US", "UU",
    "UW", "UX", "UY", "V1", "V3", "V4", "V5", "V8", "VA", "VB", "VC", "VD",
    "VE", "VH", "VK", "VM", "VN", "VO", "VP", "VQ", "VS", "VU", "VV", "VX",
    "VY", "VZ", "W1", "W4", "W5", "W8", "W9", "WA", "WB",
    "WC", "WD", "WE", "WG", "WI", "WJ", "WK", "WL", "WM", "WN", "WO", "WP",
    "WQ", "WR", "WS", "WT", "WU", "WV", "WW", "WX", "WZ", "X1", "X2", "X3",
    "X4", "X5", "X6", "X7", "X8", "X9", "XA", "XB", "XC",
    "XD", "XE", "XF", "XG", "XH", "XI", "XJ", "XK", "XL", "XM", "XN", "XO",
    "XP", "XQ", "XR", "XS", "XT", "XU", "XV", "XX", "XY", "XZ", "Y4", "Y5",
    "Y6", "YA", "YB", "YC", "YD", "YE", "YF", "YG", "YH",
    "YJ", "YK", "YL", "YM", "YN", "YO", "YP", "YQ", "Z1", "Z2", "Z3", "Z5",
    "Z6", "Z7", "Z8", "Z9", "ZB", "ZC", "ZD"
  )
  interpolator_stations <- meteoland_interpolator_example[["elevation"]] |> colnames()

  low_n_interpolator <- set_interpolation_params(
    meteoland_interpolator_example[,,which(interpolator_stations %in% req_stations)],
    list(N_MinTemperature = 5, alpha_MinTemperature = 9.5, initial_Rp = 1.13644)
  )
  low_n_trigger_points <- dplyr::bind_cols(
    stars::st_get_dimension_values(meteoland_interpolator_example, "station")[65],
    meteoland_interpolator_example[["elevation"]][1,65],
    meteoland_interpolator_example[["aspect"]][1,65],
    meteoland_interpolator_example[["slope"]][1,65]
  ) |>
    purrr::set_names("geometry", "elevation", "aspect", "slope") |>
    sf::st_as_sf()

  expect_warning(
    interpolate_data(low_n_trigger_points, low_n_interpolator),
    "Could not perform temperature interpolation"
  )
})

test_that("interpolator calibration works as expected", {
  expect_error(
    interpolator_calibration("tururu"),
    "missing the interpolation parameters"
  )
  interpolator_no_params <- meteoland_interpolator_example
  attr(interpolator_no_params, "params") <- NULL
  expect_error(
    interpolator_calibration(interpolator_no_params),
    "missing the interpolation parameters"
  )
  interpolator_wrong_dims <- meteoland_interpolator_example
  dimnames(interpolator_wrong_dims) <- c('tururu', 'larara')
  expect_error(
    interpolator_calibration(interpolator_wrong_dims),
    "missing the correct dimensions"
  )
  interpolator_no_meteo_names <- meteoland_interpolator_example
  names(interpolator_no_meteo_names) <- c('tururu', names(interpolator_no_meteo_names)[-2])
  expect_error(
    interpolator_calibration(interpolator_no_meteo_names),
    "Names found in interpolator don't comply with the required names"
  )
  # N under 10 only for temps
  expect_error(
    interpolator_calibration(meteoland_interpolator_example, N_seq = c(5, 10), alpha_seq = c(9, 9.5)),
    "start at 10 or bigger"
  )
  expect_type(
    suppressWarnings(
      interpolator_calibration(
        meteoland_interpolator_example, variable = "DewTemperature",
        N_seq = c(5, 10), alpha_seq = c(9, 9.5)
      )
    ),
    'list'
  )

  expect_type(
    suppressWarnings(test_calibration <-
       interpolator_calibration(meteoland_interpolator_example, N_seq = c(10, 20), alpha_seq = c(9, 9.5))),
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
  expect_true("2022-04-25" %in% dimnames(test_calibration$predicted)[[2]])

  # selecting stations
  expect_type(
    (test_calibration_three_stations <- interpolator_calibration(
      meteoland_interpolator_example,
      stations = c(76, 83, 187),
      N_seq = c(10, 20), alpha_seq = c(9, 9.5))),
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
  expect_identical(dimnames(test_calibration_three_stations$observed)[[1]], c("V3", "VD", "ZB"))
  expect_true(is.matrix(test_calibration_three_stations$predicted))
  expect_length(dimnames(test_calibration_three_stations$predicted)[[1]], 3)
  expect_identical(dimnames(test_calibration_three_stations$predicted)[[1]], c("V3", "VD", "ZB"))

  # stations as names
  expect_type(
    (test_calibration_three_stations_names <- interpolator_calibration(
      meteoland_interpolator_example,
      stations = c("V3", "VD", "ZB"),
      N_seq = c(10, 20), alpha_seq = c(9, 9.5))),
    'list'
  )
  expect_identical(test_calibration_three_stations_names, test_calibration_three_stations)

  # returning an interpolator
  expect_s3_class(
    (test_calibrated_interpolator <- interpolator_calibration(
      meteoland_interpolator_example,
      stations = c(76, 83, 187),
      update_interpolation_params = TRUE,
      N_seq = c(10, 20), alpha_seq = c(9, 9.5))),
    'stars'
  )
  expect_error(.is_interpolator(test_calibrated_interpolator), NA)
  expect_false(
    attr(meteoland_interpolator_example, 'params')$alpha_MinTemperature ==
      attr(test_calibrated_interpolator, 'params')$alpha_MinTemperature
  )
  expect_false(
    attr(meteoland_interpolator_example, 'params')$N_MinTemperature ==
      attr(test_calibrated_interpolator, 'params')$N_MinTemperature
  )
  expect_identical(
    meteoland_interpolator_example$MinTemperature, test_calibrated_interpolator$MinTemperature
  )
  expect_identical(names(meteoland_interpolator_example), names(test_calibrated_interpolator))
  expect_identical(
    names(attr(meteoland_interpolator_example, 'params')),
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
    (crossvalidation_test <- interpolation_cross_validation(meteoland_interpolator_example)),
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
      paste0("FreqPrecipitation", c("_station_observed", "_station_predicted")),
      paste0("TotalPrecipitation", c("_station_observed", "_station_predicted"))
    ),
    ignore.order = TRUE
  )
  expect_named(
    crossvalidation_test[["dates_stats"]],
    c(
      "dates",
      paste0(
        c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation", "TotalPrecipitation", "StationsPrecipitation"),
        "_date_bias"
      ),
      paste0(
        c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation"),
        "_date_mae"
      ),
      paste0(
        c("TotalPrecipitation", "StationsPrecipitation"),
        "_date_relative_bias"
      ),
      paste0("FreqPrecipitation", c("_date_observed", "_date_predicted")),
      paste0("TotalPrecipitation", c("_date_observed", "_date_predicted"))
    ),
    ignore.order = TRUE
  )
  expect_named(
    crossvalidation_test[["r2"]],
    c("MinTemperature", "MaxTemperature", "RangeTemperature", "RelativeHumidity", "Radiation")
  )
  expect_equal(
    nrow(crossvalidation_test[["errors"]]),
    as.numeric(nrow(meteoland_interpolator_example[["MinTemperature"]]) * ncol(meteoland_interpolator_example[["MinTemperature"]]))
  )
  expect_equal(
    nrow(crossvalidation_test[["station_stats"]]),
    as.numeric(ncol(meteoland_interpolator_example[["MinTemperature"]]))
  )
  expect_equal(
    nrow(crossvalidation_test[["dates_stats"]]),
    as.numeric(nrow(meteoland_interpolator_example[["MinTemperature"]]))
  )

  # some stations
  expect_type(
    (crossvalidation_three_stations_test <-
       interpolation_cross_validation(meteoland_interpolator_example, stations = c(76L, 83L, 187L))),
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
    all(unique(crossvalidation_three_stations_test[["errors"]][["stationID"]]) %in% c("V3", "VD", "ZB"))
  )
  expect_length(
    crossvalidation_three_stations_test[["station_stats"]][["station"]], 3
  )
  expect_true(
    all(crossvalidation_three_stations_test[["station_stats"]][["stationID"]] %in% c("V3", "VD", "ZB"))
  )

  # some stations by name
  expect_type(
    (crossvalidation_three_stations_name_test <-
       interpolation_cross_validation(meteoland_interpolator_example, stations = c("V3", "VD", "ZB"))),
    "list"
  )
  expect_identical(
    crossvalidation_three_stations_name_test, crossvalidation_three_stations_test
  )

})
