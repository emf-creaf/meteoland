library(meteospain)

meteo_cat <- get_meteo_from(
  "meteocat",
  meteocat_options(
    "daily",
    as.Date("2022-01-01"),
    api_key = keyring::key_get('meteocat')
  )
)

meteo_aemet <- get_meteo_from(
  "aemet",
  aemet_options(
    "daily",
    as.Date("2022-01-01"), as.Date("2022-01-31"),
    api_key = keyring::key_get('aemet')
  )
)

meteo_test <- meteo_cat |>
  dplyr::bind_rows(meteo_aemet) |>
  dplyr::arrange(timestamp)

tictoc::tic()
meteo_completed_old <- meteoland::reshapemeteospain(meteo_test)
tictoc::toc()
saveRDS(meteo_completed_old, "tests/testthat/meteo_completed_old.rds")

tictoc::tic()
meteo_completed_new <- meteoland:::meteospain2meteoland(meteo_test, complete = TRUE)
tictoc::toc()
saveRDS(meteo_completed_old, "tests/testthat/meteo_completed_new.rds")

identical(
  meteo_completed_old@data$ZD$MinTemperature,
  meteo_completed_new |>
    dplyr::filter(stationID == "ZD") |>
    dplyr::pull(MinTemperature)
)

identical(
  meteo_completed_old@data$`4244X`$MeanRelativeHumidity,
  meteo_completed_new |>
    dplyr::filter(stationID == "4244X") |>
    dplyr::pull(RelativeHumidity)
)

identical(
  meteo_completed_old@data$`4244X`$MinRelativeHumidity,
  meteo_completed_new |>
    dplyr::filter(stationID == "4244X") |>
    dplyr::pull(MinRelativeHumidity)
)

identical(
  meteo_completed_old@data$`4244X`$MaxRelativeHumidity,
  meteo_completed_new |>
    dplyr::filter(stationID == "4244X") |>
    dplyr::pull(MaxRelativeHumidity)
)

identical(
  meteo_completed_old@data$`4244X`$Radiation,
  meteo_completed_new |>
    dplyr::filter(stationID == "4244X") |>
    dplyr::pull(Radiation)
)

meteo_completed_old_joined <-
  purrr::imap(meteo_completed_old@data, ~ dplyr::mutate(.x, stationID = .y)) |>
  purrr::map(tibble::rownames_to_column, var = 'dates') |>
  purrr::list_rbind()
nrow(meteo_completed_old_joined)
nrow(meteo_completed_new)

meteo_completed_old_joined$dates |>
  unique() |>
  length()
meteo_completed_new$dates |>
  unique() |>
  length()

old_joined_simplified <- meteo_completed_old_joined |>
  dplyr::arrange(dates, stationID) |>
  dplyr::select(dplyr::one_of(sort(names(meteo_completed_new)))) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 3))
new_simplified <- meteo_completed_new |>
  dplyr::as_tibble() |>
  dplyr::arrange(dates, stationID) |>
  dplyr::select(dplyr::one_of(sort(names(meteo_completed_old_joined)))) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 3))

old_joined_simplified[1, "MinTemperature"]
as.vector(new_simplified[1, "MinTemperature"])

nrow(old_joined_all_nas <- dplyr::filter(
  old_joined_simplified, is.na(MinTemperature), is.na(MaxTemperature), is.na(MaxRelativeHumidity)
))

nrow(meteo_completed_old_joined) - nrow(meteo_completed_new)

old_joined_all_nas |> dplyr::select(dates, stationID)
new_simplified |> dplyr::filter(stationID == "CU")
old_joined_simplified |> dplyr::filter(stationID == "CU")

dplyr::anti_join(
  dplyr::select(old_joined_simplified, dates, stationID) |> dplyr::mutate(dates = as.Date(dates)),
  dplyr::select(new_simplified, dates, stationID),
) |>
  dplyr::left_join(old_joined_simplified |> dplyr::mutate(dates = as.Date(dates)))

###################subdaily######################################
# With subdaily data
library(meteospain)

subdaily_meteo_cat <- get_meteo_from(
  "meteocat",
  meteocat_options(
    "hourly",
    api_key = keyring::key_get('meteocat', keyring = "malditobarbudo")
  )
)

subdaily_meteo_aemet <- get_meteo_from(
  "aemet",
  aemet_options(
    "current_day",
    api_key = keyring::key_get('aemet', keyring = "malditobarbudo")
  )
)

subdaily_test <- subdaily_meteo_cat |>
  dplyr::bind_rows(subdaily_meteo_aemet) |>
  dplyr::arrange(timestamp)

tictoc::tic()
subdaily_meteo_test_new <-
  meteoland:::meteospain2meteoland(subdaily_test, complete = TRUE)
tictoc::toc()

tictoc::tic()
subdaily_meteo_test_old <- meteoland::reshapemeteospain(subdaily_test)
tictoc::toc()

identical(
  subdaily_meteo_test_old@data$ZD$MinTemperature,
  subdaily_meteo_test_new |>
    dplyr::filter(stationID == "ZD") |>
    dplyr::pull(MinTemperature)
)

identical(
  subdaily_meteo_test_old@data$`4244X`$MeanRelativeHumidity,
  subdaily_meteo_test_new |>
    dplyr::filter(stationID == "4244X") |>
    dplyr::pull(RelativeHumidity)
)
