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
  
  # assertions
  assertthat::assert_that(has_meteo_names(meteo_interpolated))
  assertthat::assert_that(
    assertthat::is.date(dates_to_summary),
    msg = "dates is not a Date object"
  )
  
  # match args
  frequency <- match.arg(frequency)
  
  # filter dates
  if (!is.null(dates)) {
    .verbosity_control(
      usethis::ui_info("Filtering the desired dates"),
      verbose
    )
    meteo_interpolated <- meteo_interpolated |>
      dplyr::filter(dates %in% dates_to_summary)
  }
  
  frequency_fun <- parse(text = paste0("lubridate::", frequency)) |> eval()
  
  browser()
  
  meteo_interpolated |>
    # filtering the months to summary
    dplyr::filter(lubridate::month(dates) %in% months_to_summary) |>
    # summarise by freq
    dplyr::mutate(
      freq_group := frequency_fun(dates)
    ) |>
    dplyr::group_by(freq_group) |>
    dplyr::select(dplyr::any_of(vars_to_summary)) |>
    dplyr::summarise(
      dplyr::across(.cols = where(is.numeric), .fns = list(.f), ...)
    )
    
  
}
