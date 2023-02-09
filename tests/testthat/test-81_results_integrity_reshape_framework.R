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

# subdaily
subdaily_meteo_cat <- get_meteo_from(
  "meteocat",
  meteocat_options("hourly", api_key = meteocat_key)
)
subdaily_meteo_aemet <- get_meteo_from(
  "aemet",
  aemet_options("current_day", api_key = aemet_key)
)
subdaily_test <- subdaily_meteo_cat |>
  dplyr::bind_rows(subdaily_meteo_aemet) |>
  dplyr::arrange(timestamp)

# worldmet cat
# worldmet_stations <- worldmet::getMeta(lat = 42, lon = 0, n = 20, plot = FALSE, country = "SP")
# meteo_test <-
#   worldmet::importNOAA(worldmet_stations$code, year = 2022, hourly = TRUE, n.cores = 6) |>
#   dplyr::filter(date < as.Date("2022-02-01"))
meteo_worldmet_test <- readRDS("worldmet_test.rds")


## start testing
test_that("reshaping and completing daily meteospain works the same", {

  # results are the expected classes for each, old and new
  meteo_completed_old <- suppressWarnings(reshapemeteospain(meteo_test, verbose = FALSE))
  meteo_completed_new <- meteospain2meteoland(meteo_test, complete = TRUE)
  expect_s4_class(meteo_completed_old, "SpatialPointsMeteorology")
  expect_s3_class(meteo_completed_new, "sf")

  # values are identical between old and new
  expect_identical(
    meteo_completed_old@data$ZD$MinTemperature,
    meteo_completed_new |>
      dplyr::filter(stationID == "ZD") |>
      dplyr::pull(MinTemperature)
  )
  expect_identical(
    meteo_completed_old@data$`4244X`$MeanRelativeHumidity,
    meteo_completed_new |>
      dplyr::filter(stationID == "4244X") |>
      dplyr::pull(MeanRelativeHumidity)
  )
  expect_identical(
    meteo_completed_old@data$`4244X`$MinRelativeHumidity,
    meteo_completed_new |>
      dplyr::filter(stationID == "4244X") |>
      dplyr::pull(MinRelativeHumidity)
  )
  expect_identical(
    meteo_completed_old@data$`4244X`$MaxRelativeHumidity,
    meteo_completed_new |>
      dplyr::filter(stationID == "4244X") |>
      dplyr::pull(MaxRelativeHumidity)
  )
  expect_identical(
    meteo_completed_old@data$`4244X`$Radiation,
    meteo_completed_new |>
      dplyr::filter(stationID == "4244X") |>
      dplyr::pull(Radiation)
  )

  meteo_completed_joined_old <-
    purrr::imap(meteo_completed_old@data, ~ dplyr::mutate(.x, stationID = .y)) |>
    purrr::map(tibble::rownames_to_column, var = 'dates') |>
    purrr::list_rbind() |>
    # filter NAs in maxrelativehumidity, as they are missing dates already
    # filtered in the new workflow
    dplyr::filter(!is.na(MaxRelativeHumidity))

  expect_identical(nrow(meteo_completed_joined_old), nrow(meteo_completed_new))
  expect_identical(
    meteo_completed_joined_old$dates |>
      unique() |>
      length(),
    meteo_completed_new$dates |>
      unique() |>
      length()
  )
  expect_identical(
    meteo_completed_joined_old$dates |>
      unique() |>
      sort(),
    meteo_completed_new$dates |>
      unique() |>
      sort() |>
      as.character()
  )

  suppressWarnings(
    meteo_completed_joined_old_simplified <- meteo_completed_joined_old |>
      dplyr::arrange(dates, stationID) |>
      dplyr::select(dplyr::one_of(sort(names(meteo_completed_new)))) |>
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, digits = 3))) |>
      dplyr::as_tibble()
  )

  suppressWarnings(
    meteo_completed_new_simplified <- meteo_completed_new |>
      dplyr::as_tibble() |>
      dplyr::arrange(dates, stationID) |>
      dplyr::select(dplyr::one_of(sort(names(meteo_completed_joined_old)))) |>
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, digits = 3)))
  )

  expect_identical(
    names(meteo_completed_joined_old_simplified),
    names(meteo_completed_new_simplified)
  )
  expect_identical(
    meteo_completed_joined_old_simplified["MinRelativeHumidity"],
    meteo_completed_new_simplified["MinRelativeHumidity"],
    ignore_attr = TRUE
  )
  expect_identical(
    meteo_completed_joined_old_simplified["MeanTemperature"],
    meteo_completed_new_simplified["MeanTemperature"],
    ignore_attr = TRUE
  )
  expect_identical(
    meteo_completed_joined_old_simplified["Radiation"],
    meteo_completed_new_simplified["Radiation"],
    ignore_attr = TRUE
  )
  expect_true(
    nrow(dplyr::filter(
      meteo_completed_joined_old_simplified,
      is.na(MinTemperature), is.na(MaxTemperature), is.na(MaxRelativeHumidity)
    )) < 1
  )
  # antijoin between both should be no rows!!
  expect_true(
    nrow(dplyr::anti_join(
      dplyr::select(meteo_completed_joined_old_simplified, dates, stationID) |> dplyr::mutate(dates = as.Date(dates)),
      dplyr::select(meteo_completed_new_simplified, dates, stationID),
    ) |>
      dplyr::left_join(meteo_completed_joined_old_simplified |> dplyr::mutate(dates = as.Date(dates)))) < 1
  )
})

test_that("reshaping and completing subdaily meteospain works the same", {

  # TODO to test fringe cases when metadata of station changes, test this with
  # subdaily_meteo_generating_duplicated_rows_in_meteospain2meteoland.rds which
  # generates duplicated rows in the process.

  # results are the expected classes for each, old and new
  meteo_completed_old <- suppressWarnings(reshapemeteospain(subdaily_test, verbose = FALSE))
  meteo_completed_new <- suppressWarnings(meteospain2meteoland(subdaily_test, complete = TRUE))
  expect_s4_class(meteo_completed_old, "SpatialPointsMeteorology")
  expect_s3_class(meteo_completed_new, "sf")
  # Mean Temperature is a vector of 2 in the old, one as NA beacuse is not
  # available, whereas in the new method, the NAs are removed
  expect_true(sum(is.na(meteo_completed_old@data$ZD$MeanTemperature)) == 1)
  expect_length(meteo_completed_new |>
                  dplyr::filter(stationID == "ZD") |>
                  dplyr::pull(MeanTemperature), 1)
  expect_identical(
    meteo_completed_old@data$ZD$MeanTemperature[2],
    meteo_completed_new |>
      dplyr::filter(stationID == "ZD") |>
      dplyr::pull(MeanTemperature)
  )
  # the rest of variables are the same
  expect_identical(
    meteo_completed_old@data$`4244X`$MeanRelativeHumidity,
    meteo_completed_new |>
      dplyr::filter(stationID == "4244X") |>
      dplyr::pull(MeanRelativeHumidity)
  )
  expect_identical(
    meteo_completed_old@data$`4244X`$MinRelativeHumidity,
    meteo_completed_new |>
      dplyr::filter(stationID == "4244X") |>
      dplyr::pull(MinRelativeHumidity)
  )
  expect_identical(
    meteo_completed_old@data$`4244X`$MaxRelativeHumidity,
    meteo_completed_new |>
      dplyr::filter(stationID == "4244X") |>
      dplyr::pull(MaxRelativeHumidity)
  )
  expect_identical(
    meteo_completed_old@data$`4244X`$Radiation,
    meteo_completed_new |>
      dplyr::filter(stationID == "4244X") |>
      dplyr::pull(Radiation)
  )

  meteo_completed_joined_old <-
    purrr::imap(meteo_completed_old@data, ~ dplyr::mutate(.x, stationID = .y)) |>
    purrr::map(tibble::rownames_to_column, var = 'dates') |>
    purrr::list_rbind() |>
    # filter NAs in maxrelativehumidity, as they are missing dates already
    # filtered in the new workflow
    dplyr::filter(!is.na(MaxRelativeHumidity))

  expect_identical(nrow(meteo_completed_joined_old), nrow(meteo_completed_new))
  expect_identical(
    meteo_completed_joined_old$dates |>
      unique() |>
      length(),
    meteo_completed_new$dates |>
      unique() |>
      length()
  )
  expect_identical(
    meteo_completed_joined_old$dates |>
      unique() |>
      sort(),
    meteo_completed_new$dates |>
      unique() |>
      sort() |>
      as.character()
  )

  suppressWarnings(
    meteo_completed_joined_old_simplified <- meteo_completed_joined_old |>
      dplyr::arrange(dates, stationID) |>
      dplyr::select(dplyr::one_of(sort(names(meteo_completed_new)))) |>
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, digits = 3))) |>
      dplyr::as_tibble()
  )

  suppressWarnings(
    meteo_completed_new_simplified <- meteo_completed_new |>
      dplyr::as_tibble() |>
      dplyr::arrange(dates, stationID) |>
      dplyr::select(dplyr::one_of(sort(names(meteo_completed_joined_old)))) |>
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, digits = 3)))
  )

  expect_identical(
    names(meteo_completed_joined_old_simplified),
    names(meteo_completed_new_simplified)
  )
  expect_identical(
    meteo_completed_joined_old_simplified["MinRelativeHumidity"],
    meteo_completed_new_simplified["MinRelativeHumidity"],
    ignore_attr = TRUE
  )
  expect_identical(
    meteo_completed_joined_old_simplified["MeanTemperature"],
    meteo_completed_new_simplified["MeanTemperature"],
    ignore_attr = TRUE
  )
  # radiation is calculated in the new workflow, not in the old
  # expect_identical(
  #   meteo_completed_joined_old_simplified["Radiation"],
  #   meteo_completed_new_simplified["Radiation"],
  #   ignore_attr = TRUE
  # )
  expect_true(
    nrow(dplyr::filter(
      meteo_completed_joined_old_simplified,
      is.na(MinTemperature), is.na(MaxTemperature), is.na(MaxRelativeHumidity)
    )) < 1
  )
  # antijoin between both should be no rows!!
  expect_true(
    nrow(dplyr::anti_join(
      dplyr::select(meteo_completed_joined_old_simplified, dates, stationID) |> dplyr::mutate(dates = as.Date(dates)),
      dplyr::select(meteo_completed_new_simplified, dates, stationID),
    ) |>
      dplyr::left_join(meteo_completed_joined_old_simplified |> dplyr::mutate(dates = as.Date(dates)))) < 1
  )
})

test_that("reshaping and completing worldmet works the same", {

  # results are the expected classes for each, old and new
  meteo_completed_old <- suppressWarnings(reshapeworldmet(meteo_worldmet_test, verbose = FALSE))
  meteo_completed_new <- suppressWarnings(worldmet2meteoland(meteo_worldmet_test, complete = TRUE))
  expect_s4_class(meteo_completed_old, "SpatialPointsMeteorology")
  expect_s3_class(meteo_completed_new, "sf")

  # values are identical between old and new
  expect_identical(
    meteo_completed_old@data$`089220-99999`$MinTemperature,
    meteo_completed_new |>
      dplyr::filter(stationID == "089220-99999") |>
      dplyr::pull(MinTemperature)
  )
  expect_identical(
    meteo_completed_old@data$`081120-99999`$MeanRelativeHumidity,
    meteo_completed_new |>
      dplyr::filter(stationID == "081120-99999") |>
      dplyr::pull(MeanRelativeHumidity)
  )
  expect_identical(
    meteo_completed_old@data$`081120-99999`$MinRelativeHumidity,
    meteo_completed_new |>
      dplyr::filter(stationID == "081120-99999") |>
      dplyr::pull(MinRelativeHumidity)
  )
  expect_identical(
    meteo_completed_old@data$`081120-99999`$MaxRelativeHumidity,
    meteo_completed_new |>
      dplyr::filter(stationID == "081120-99999") |>
      dplyr::pull(MaxRelativeHumidity)
  )
  expect_identical(
    meteo_completed_old@data$`081120-99999`$Radiation,
    meteo_completed_new |>
      dplyr::filter(stationID == "081120-99999") |>
      dplyr::pull(Radiation)
  )

  meteo_completed_joined_old <-
    purrr::imap(meteo_completed_old@data, ~ dplyr::mutate(.x, stationID = .y)) |>
    purrr::map(tibble::rownames_to_column, var = 'dates') |>
    purrr::list_rbind() |>
    # filter NAs in minrelativehumidity, as they are missing dates already
    # filtered in the new workflow (not using maxRH because has been completed with 100)
    dplyr::filter(!is.na(MinRelativeHumidity))

  expect_identical(nrow(meteo_completed_joined_old), nrow(meteo_completed_new))
  expect_identical(
    meteo_completed_joined_old$dates |>
      unique() |>
      length(),
    meteo_completed_new$dates |>
      unique() |>
      length()
  )
  expect_identical(
    meteo_completed_joined_old$dates |>
      unique() |>
      sort(),
    meteo_completed_new$dates |>
      unique() |>
      sort() |>
      as.character()
  )

  suppressWarnings(
    meteo_completed_joined_old_simplified <- meteo_completed_joined_old |>
      dplyr::arrange(dates, stationID) |>
      dplyr::select(dplyr::one_of(sort(names(meteo_completed_new)))) |>
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, digits = 3))) |>
      dplyr::as_tibble()
  )

  suppressWarnings(
    meteo_completed_new_simplified <- meteo_completed_new |>
      dplyr::as_tibble() |>
      dplyr::arrange(dates, stationID) |>
      dplyr::select(dplyr::one_of(sort(names(meteo_completed_joined_old)))) |>
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, digits = 3)))
  )

  expect_identical(
    names(meteo_completed_joined_old_simplified),
    names(meteo_completed_new_simplified)
  )
  # errors here seems to be because stationID is a factor
  expect_identical(
    meteo_completed_joined_old_simplified["MinRelativeHumidity"],
    meteo_completed_new_simplified["MinRelativeHumidity"],
    ignore_attr = TRUE
  )
  expect_identical(
    meteo_completed_joined_old_simplified["MeanTemperature"],
    meteo_completed_new_simplified["MeanTemperature"],
    ignore_attr = TRUE
  )
  expect_identical(
    meteo_completed_joined_old_simplified["Radiation"],
    meteo_completed_new_simplified["Radiation"],
    ignore_attr = TRUE
  )
  expect_true(
    nrow(dplyr::filter(
      meteo_completed_joined_old_simplified,
      is.na(MinTemperature), is.na(MaxTemperature), is.na(MaxRelativeHumidity)
    )) < 1
  )
  # antijoin between both should be no rows!!
  expect_true(
    nrow(
      dplyr::anti_join(
        dplyr::select(meteo_completed_joined_old_simplified, dates, stationID) |>
          dplyr::mutate(dates = as.Date(dates)),
        dplyr::select(meteo_completed_new_simplified, dates, stationID),
      ) |>
        dplyr::left_join(
          meteo_completed_joined_old_simplified |>
            dplyr::mutate(dates = as.Date(dates))
        )
    ) < 1
  )
})
