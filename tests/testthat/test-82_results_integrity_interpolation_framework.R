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

# interpolation framework -------------------------------------------------
test_that("interpolator object has the same data", {
  elevation <- meteo_test |>
    dplyr::group_by(.data$station_id) |>
    dplyr::summarise(elevation = mean(altitude))

  expect_true(sum(names(meteo_completed_old@data) != elevation$station_id) < 1)
  expect_s4_class(
    suppressWarnings(interpolator_old <- MeteorologyInterpolationData(
      points = meteo_completed_old,
      elevation = as.numeric(elevation$elevation)
    )), "MeteorologyInterpolationData"
  )
  expect_s3_class(
    suppressWarnings(interpolator_new <- with_meteo(meteo_completed_new) |>
      create_meteo_interpolator()), "stars"
  )




  ## Interpolator data
  # names(interpolator_new)[!names(interpolator_new) %in% c('elevation', 'aspect', 'slope', 'Temperature')] |>
  #   purrr::walk(.f = function(variable) {
  #     print(variable)
  #     print(sum(t(slot(interpolator_old, variable)) - interpolator_new[[variable]], na.rm = TRUE))
  #   })
  # ## Interpolator dates
  # all.equal(interpolator_old@dates |> as.POSIXct(), stars::st_get_dimension_values(interpolator_new, "date"))
  # ## Interpolator stations
  # all.equal(interpolator_old@coords, stars::st_get_dimension_values(interpolator_new, 'station') |> sf::st_coordinates())
  # ## Interpolator params
  # names(attributes(interpolator_old)$params) |>
  #   purrr::walk(.f = function(param) {
  #     print(param)
  #     print(attributes(interpolator_old)$params[[param]] == attributes(interpolator_new)$params[[param]])
  #   })
  # attributes(interpolator_old)$params[["initial_Rp"]]
  # attributes(interpolator_new)$params[["initial_Rp"]]




})






points_old <- as(points_to_interpolate_example, "Spatial")
points_topography <- SpatialPointsTopography(
  as(points_old,"SpatialPoints"), elevation = points_old$elevation,
  slope = points_old$slope, aspect = points_old$aspect
)

interpolated_data_old <- interpolationpoints(interpolator_old, points_topography)
interpolated_data_new <- interpolate_data(points_to_interpolate_example, interpolator_new)
