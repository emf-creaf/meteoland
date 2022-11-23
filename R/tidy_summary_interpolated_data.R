.summary_by_date_and_var <- function(
  meteo_interpolated,
  .f = "mean",
  frequency = NULL,
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
  if (!is.null(dates_to_summary)) {
    assertthat::assert_that(
      assertthat::is.date(dates_to_summary),
      msg = "dates is not a Date object"
    )

    # if not null, filter by the dates supplied
    .verbosity_control(
      usethis::ui_info("Filtering the desired dates"),
      verbose
    )
    meteo_interpolated <- meteo_interpolated |>
      dplyr::filter(dates %in% dates_to_summary)
  }

  .group_by_freq <- function(data, frequency) {

    # if there is no frequency, means no frequency_fun, then return data as is
    if (is.null(frequency)) {
      return(data)
    }

    # match args
    frequency <- match.arg(frequency, c("month", "week", "quarter", "year"))
    # lubridate frequency function
    frequency_fun <- parse(text = paste0("lubridate::", frequency)) |> eval()

    if (frequency == "year") {
      yearly_case <- data |>
        # create and group by the frequency desired
        dplyr::mutate(
          {{frequency}} := frequency_fun(dates)
        ) |>
        dplyr::group_by(.data[[frequency]])

      return(yearly_case)
    }

    data |>
      # create and group by the frequency desired
      dplyr::mutate(
        {{frequency}} := frequency_fun(dates),
        year = lubridate::year(dates)
      ) |>
      dplyr::group_by(.data[[frequency]], year)
  }

  ## frequency alone, if data spans more than one year can group together data from different years
  ## (in the original code it was a cut(dates, breaks = freq), so we have to imitate it)
  ## Also, the frequency can be NULL, so, we have to take that into account. We create a helper to
  ## do the mutate and group_by steps depending on the freq data
  meteo_interpolated |>
    # filtering the months to summary
    dplyr::filter(lubridate::month(dates) %in% months_to_summary) |>
    # group by desired frequency (if any)
    .group_by_freq(frequency) |>
    # select the vars wanted
    dplyr::select(dplyr::any_of(vars_to_summary)) |>
    # summarise by frequency
    dplyr::summarise(
      dplyr::across(.fns = parse(text = .f) |> eval(), ...)
    )
}
