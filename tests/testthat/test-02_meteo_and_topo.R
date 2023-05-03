library(sf)
### TODO
### This tests should be done with more than meteospain meteo, as we accept also worldmet and
### issues from that data can be hidden in these tests. The idea is to create a mixture of
### meteospain and worldmet data and use that as meteo_test object
meteo_test_correct <- readRDS("meteo_and_topo_test.rds")
meteo_test_names <- readRDS("meteospain_daily_test.rds")
meteo_test_no_sf <- dplyr::as_tibble(meteo_test_correct)
meteo_test_no_dates <- meteo_test_correct |> dplyr::select(-dates)
meteo_test_wrong_date_format <- meteo_test_correct |>
  dplyr::mutate(dates = as.character(dates))
meteo_test_no_id <- meteo_test_correct |> dplyr::select(-stationID)
meteo_test_no_numeric <- meteo_test_correct |>
  dplyr::mutate(MinTemperature = as.character(MinTemperature))
meteo_test_no_points <- meteo_test_correct |>
  dplyr::slice(1:10) |>
  sf::st_union() |>
  sf::st_as_sf()
meteo_test_correct_non_unique_ids <- meteo_test_correct
meteo_test_correct_non_unique_ids$geometry[1] <- meteo_test_correct_non_unique_ids$geometry[5000]

# # meteo test data, 1 month
# meteo_test <- readRDS('meteo_test.rds') |>
#   dplyr::filter(timestamp < (as.Date("2021-02-01") |> as.POSIXct()))
# meteo_test_correct <- meteospain2meteoland(meteo_test)
# # test_data
# meteo_test_no_sf <- dplyr::as_tibble(meteo_test_correct)
# meteo_test_no_dates <- meteo_test_correct |> dplyr::select(-dates)
# meteo_test_wrong_date_format <- meteo_test_correct |>
#   dplyr::mutate(dates = as.character(dates))
# meteo_test_no_id <- meteo_test_correct |> dplyr::select(-stationID)
# meteo_test_no_numeric <- meteo_test_correct |>
#   dplyr::mutate(MinTemperature = as.character(MinTemperature))
# meteo_test_no_points <- meteo_test_correct |>
#   dplyr::slice(1:10) |>
#   sf::st_union() |>
#   sf::st_as_sf()
# meteo_test_correct_non_unique_ids <- meteo_test_correct
# meteo_test_correct_non_unique_ids$geometry[1] <- meteo_test_correct_non_unique_ids$geometry[5000]

test_that("has_meteo checks work", {
  expect_true(has_meteo(meteo_test_correct))
  expect_error(has_meteo(meteo_test_names), "Names found in")
  expect_error(has_meteo(meteo_test_no_sf), "meteo must be an sf")
  expect_error(has_meteo(meteo_test_no_dates), "variable called dates")
  expect_error(has_meteo(meteo_test_wrong_date_format), "must be a date")
  expect_error(has_meteo(meteo_test_no_id), "stationID variable")
  expect_error(has_meteo(meteo_test_no_numeric), "MinTemperature")
  expect_error(has_meteo(meteo_test_no_points), "geometries must be")
  expect_error(has_meteo(meteo_test_correct_non_unique_ids), "There are more geometries")
})

topo_test_correct <- meteo_test_correct |>
  dplyr::mutate(aspect = NA_real_, slope = NA_real_) |>
  dplyr::as_tibble() |>
  dplyr::select(stationID, elevation, aspect, slope)
# test data
topo_test_sf <- meteo_test_correct |>
  dplyr::select(stationID, elevation) |>
  dplyr::mutate(aspect = NA_real_, slope = NA_real_)
topo_test_no_names <- topo_test_correct |>
  dplyr::rename(Elevation = "elevation")
topo_test_no_id <- topo_test_correct |>
  dplyr::select(-stationID)
topo_test_no_numeric <- topo_test_correct |>
  dplyr::mutate(elevation = as.character(elevation))

test_that("has_topo checks works", {
  expect_true(has_topo(topo_test_correct))
  expect_true(has_topo(topo_test_sf))
  expect_error(has_topo(topo_test_no_names), "elevation")
  expect_error(has_topo(topo_test_no_id), "stationID")
  expect_error(has_topo(topo_test_no_numeric), "elevation")
})

# test_data
meteo_test_no_topo <- meteo_test_correct |>
  dplyr::select(-elevation, -aspect, -slope)

test_that("with_meteo add_topo combo works", {
  expect_equal(meteo_test_no_topo, with_meteo(meteo_test_no_topo))
  expect_error(with_meteo(meteo_test_names), "Names found in")
  expect_error(with_meteo(meteo_test_no_sf), "meteo must be an sf")
  expect_error(with_meteo(meteo_test_no_dates), "variable called dates")
  expect_error(with_meteo(meteo_test_wrong_date_format), "must be a date")
  expect_error(with_meteo(meteo_test_no_id), "stationID variable")
  expect_error(with_meteo(meteo_test_no_numeric), "MinTemperature")
  expect_error(with_meteo(meteo_test_no_points), "geometries must be")

  expect_error(
    with_meteo(meteo_test_no_topo) |> add_topo(topo_test_no_names), "elevation"
  )
  expect_error(
    with_meteo(meteo_test_no_topo) |> add_topo(topo_test_no_id), "stationID"
  )
  expect_error(
    with_meteo(meteo_test_no_topo) |> add_topo(topo_test_no_numeric), "elevation"
  )
  expect_s3_class(
    meteo_test_with_topo <- with_meteo(meteo_test_no_topo) |>
      add_topo(topo_test_correct),
    "sf"
  )
  expect_identical(
    nrow(meteo_test_with_topo), nrow(meteo_test_no_topo)
  )
  expect_true(
    all(c("elevation", "MinTemperature", "MaxTemperature", "Precipitation")
        %in% names(meteo_test_with_topo))
  )

  # special cases
  # both spatial. Topo is converted internally to tibble, so the process
  # should work, but also return identical results as with no sf object.
  # Also, no duplicated geometries should be found now
  expect_s3_class(
    meteo_test_with_topo_sf <-
      with_meteo(meteo_test_no_topo) |> add_topo(topo_test_sf),
    "sf"
  )
  expect_equal(meteo_test_with_topo, meteo_test_with_topo_sf)
  # meteo has already topo but we joined equally
  expect_warning(
    with_meteo(meteo_test_correct) |> add_topo(topo_test_correct),
    "Topography variables found"
  )
})
