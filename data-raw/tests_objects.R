## Data for testing
# Creation of data for testing, this way is reproducible and we can change it here.

## libraries
library(meteospain)
library(worldmet)

# meteospain daily data
meteo_cat <- get_meteo_from(
  "meteocat",
  meteocat_options(
    "daily",
    as.Date("2022-01-01"),
    api_key = keyring::key_get('meteocat', keyring = "malditobarbudo")
  )
)

meteo_aemet <- get_meteo_from(
  "aemet",
  aemet_options(
    "daily",
    as.Date("2022-01-01"), as.Date("2022-01-31"),
    api_key = keyring::key_get('aemet', keyring = "malditobarbudo")
  )
)

meteospain_daily_test <- meteo_cat |>
  dplyr::bind_rows(meteo_aemet) |>
  dplyr::arrange(timestamp) |>
  dplyr::select(-geometry, dplyr::everything(), geometry)

saveRDS(meteospain_daily_test, "tests/testthat/meteospain_daily_test.rds")

# meteospain subdaily data
meteo_cat_subdaily <- get_meteo_from(
  "meteocat",
  meteocat_options(
    "hourly",
    api_key = keyring::key_get('meteocat', keyring = "malditobarbudo")
  )
)

meteo_aemet_subdaily <- get_meteo_from(
  "aemet",
  aemet_options(
    "current_day",
    as.Date("2022-01-01"), as.Date("2022-01-31"),
    api_key = keyring::key_get('aemet', keyring = "malditobarbudo")
  )
)

meteospain_subdaily_test <- meteo_cat_subdaily |>
  dplyr::bind_rows(meteo_aemet_subdaily) |>
  dplyr::arrange(timestamp) |>
  dplyr::select(-geometry, dplyr::everything(), geometry)

saveRDS(meteospain_subdaily_test, "tests/testthat/meteospain_subdaily_test.rds")

# worldmet data
worldmet_stations <- worldmet::getMeta(lat = 42, lon = 0, n = 20, plot = FALSE, country = "SP")
worldmet_codes <- paste0(worldmet_stations$usaf, "-", worldmet_stations$wban)

worldmet_test <-
  worldmet::importNOAA(worldmet_codes, year = 2022, hourly = TRUE, n.cores = 6) |>
  dplyr::filter(date < as.Date("2022-02-01"))

saveRDS(worldmet_test, "tests/testthat/worldmet_test.rds")

# meteo and topo test data
meteospain_completed <- meteospain_daily_test |>
  meteospain2meteoland(complete = TRUE)

worldmet_completed <- worldmet_test |>
  worldmet2meteoland(complete = TRUE)

meteo_and_topo_test <- meteospain_completed |>
  dplyr::bind_rows(worldmet_completed) |>
  dplyr::arrange(dates, stationID)

saveRDS(meteo_and_topo_test, "tests/testthat/meteo_and_topo_test.rds")
