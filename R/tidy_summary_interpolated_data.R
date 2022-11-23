.summary_by_date_and_var <- function(
  meteo_interpolated,
  .f = "mean",
  frequency = c("month", "week", "quarter", "year"),
  vars_to_summary = c(
    "MeanTemperature", "MinTemperature","MaxTemperature", "Precipitation",
    "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
    "Radiation", "WindSpeed", "WindDirection", "PET"
  ),
  dates_to_summary = NULL,
  months_to_summary = 1:12,
  verbose = getOption("meteoland_verbosity", TRUE),
  ...
) {
  
  browser()
  # assertions
  assertthat::assert_that(has_meteo_names(meteo_interpolated))
  assertthat::assert_that(
    assertthat::is.date(dates_to_summary),
    msg = "dates is not a Date object"
  )
  
  # match args
  frequency <- match.arg(frequency)
  
  # filter dates
  if (!is.null(dates_to_summary)) {
    .verbosity_control(
      usethis::ui_info("Filtering the desired dates"),
      verbose
    )
    meteo_interpolated <- meteo_interpolated |>
      dplyr::filter(dates %in% dates_to_summary)
  }
  
  # lubridate frequency function
  frequency_fun <- parse(text = paste0("lubridate::", frequency)) |> eval()
  
  
  meteo_interpolated |>
    # filtering the months to summary
    dplyr::filter(lubridate::month(dates) %in% months_to_summary) |>
    # create and group by the frequency desired
    dplyr::mutate(
      freq_group := frequency_fun(dates)
    ) |>
    dplyr::group_by(freq_group) |>
    # select the vars wanted
    dplyr::select(dplyr::any_of(vars_to_summary)) |>
    # summarise by frequency
    dplyr::summarise(
      dplyr::across(.cols = where(is.numeric), .fns = list(.f), ...)
    ) |>
    dplyr::rename(
      {{frequency}} := freq_group
    )
}
