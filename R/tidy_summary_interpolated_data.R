.summary_by_date_and_var <- function(
  meteo_interpolated,
  fun = "mean",
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

  # browser()
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
      dplyr::across(.fns = parse(text = fun) |> eval(), ...)
    )
}

.summary_interpolated_sf <- function(
    meteo_interpolated,
    fun = "mean",
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
  
  ## sf nested
  if ("interpolated_data" %in% names(meteo_interpolated) & inherits(meteo_interpolated, "sf")) {
    res <- meteo_interpolated |>
      dplyr::mutate(
        summary_data = purrr::map(
          interpolated_data, .f = .summary_by_date_and_var,
          fun = fun, frequency = frequency, vars_to_summary = vars_to_summary,
          dates_to_summary = dates_to_summary, months_to_summary = months_to_summary,
          verbose = verbose, ...
        )
      )
    
    return(res)
  }
  
  # warning if unnested interpolation is supplied, as all locations will be merged
  # together
  if (
    has_meteo_names(meteo_interpolated) &
    inherits(meteo_interpolated, "sf") &
    !"interpolated_data" %in% names(meteo_interpolated)
  ) {
    usethis::ui_warning(
      "meteo_interpolated object seen to have meteo variables and more than one
      location. All locations will be summarise together"
    )
  }
  
  # data frame not nested
  meteo_interpolated |>
    .summary_by_date_and_var(
      fun = fun, frequency = frequency, vars_to_summary = vars_to_summary,
      dates_to_summary = dates_to_summary, months_to_summary = months_to_summary,
      verbose = verbose, ...
    )
}

.summary_interpolated_stars <- function(
    meteo_interpolated,
    fun = "mean",
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
  
  # first is to filter dates if dates is supplied
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
      dplyr::filter(as.Date(date) %in% dates_to_summary)
  }
  
  # check the NULLness of frequency, if frequency is NULL, the aggregation is
  # done between all dates in date dimension
  if (!is.null(frequency)) {
    # match args if frequency is not null
    frequency <- match.arg(frequency, c("month", "week", "quarter", "year"))
  } else {
    # count the number of days and set it as frequency 
    frequency <- paste0(
      length(stars::st_get_dimension_values(meteo_interpolated, "date")),
      " days"
    )
  }
  
  # stars aggregation by frequency
  meteo_interpolated |>
    # select the vars to summarise
    dplyr::select(dplyr::any_of(vars_to_summary)) |>
    # filter the months to aggregate if supplied, if not all
    dplyr::filter(lubridate::month(date) %in% months_to_summary) |>
    # perform the aggregation
    aggregate(by = frequency, FUN = fun)
}

#' Summarise interpolated data by temporal dimension
#' 
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' Summarises the meteorology in one or more locations in the desired temporal
#' scale
#' 
#' @details 
#' If \code{interpolated_data} is a nested interpolated data sf object, as
#' returned by \code{\link{interpolate_data}}, temporal summary is done for each
#' location present in the interpolated data. If \code{interpolated_data} is
#' an unnested interpolated data sf object, temporal summary is done for all
#' locations together. If \code{interpolated_data} is a single location data.frame
#' containing the dates and the interpolated variables, temporal summary is done
#' for that location. If \code{interpolated_data} is a stars object as returned
#' by \code{\link{interpolate_data}}, temporal summary is done for all the raster.
#' 
#' @param interpolated_data An interpolated data object as returned by
#'   \code{\link{interpolate_data}}.
#' @param fun The function to use for summarising the data.
#' @param frequency A string indicating the interval specification (allowed ones
#'   are "week", "month", "quarter" and "year"). If NULL (default), aggregation
#'   is done in one interval for all the dates present.
#' @param vars_to_summary A character vector with one or more variable names to
#'   summarise. By default, all interpolated variables are summarised.
#' @param dates_to_summary A Date object to define the dates to be summarised.
#'   If NULL (default), all dates in the interpolated data are processed.
#' @param months_to_summary A numeric vector with the month numbers to subset
#'   the data before summarising. (e.g. \code{c(7,8)} for July and August).
#'   This parameter allows studing particular seasons, when combined with
#'   \code{frequency}. For example \code{frequency = "years"} and
#'   \code{months = 6:8} leads to summarizing summer months of each year.
#' @param verbose Logical indicating if the function must show messages and info.
#'   Default value checks \code{"meteoland_verbosity"} option and if not set, defaults
#'   to TRUE. It can be turned off for the function with FALSE, or session wide with
#'   \code{options(meteoland_verbosity = FALSE)}
#' @param ... Arguments needed for \code{fun}
#'
#' @return For a nested interpolated data, the same sf object with a new column
#' (\code{summary_data}) with the temporal summaries. For an unnested interpolated
#' data, a data.frame with the summarised meteo variables. For an interpolated
#' raster (stars object), the same raster with the temporal dimension aggregated
#' as desired.
#' 
#' @author \enc{Víctor}{Victor} Granda \enc{García}{Garcia}, CREAF
#' @examples
#' # TODO
#' 
#' @export
summarise_interpolated_data <- function(
  interpolated_data,
  fun = "mean",
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
  
  # interpolated data dispatcher
  summary_fun <- NULL
  
  if (inherits(interpolated_data, "stars")) {
    summary_fun <- .summary_interpolated_stars
  }
  
  if (inherits(interpolated_data, "sf") || inherits(interpolated_data, "data.frame")) {
    summary_fun <- .summary_interpolated_sf
  }
  
  if (is.null(summary_fun)) {
    usethis::ui_stop(
      "interpolated_data provided is not either a stars object or a sf/data.frame
      object. Are you sure it comes from interpolate_data function?"
    )
  }
  
  res <- summary_fun(
    interpolated_data,
    fun = fun,
    frequency = frequency,
    vars_to_summary = vars_to_summary,
    dates_to_summary = dates_to_summary,
    months_to_summary = months_to_summary,
    verbose = verbose,
    ...
  )
  
  return(res)
}

summarise_interpolator <- function(
  interpolator,
  fun = "mean",
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
  .summary_interpolated_stars(
    interpolator,
    fun = fun,
    frequency = frequency,
    vars_to_summary = vars_to_summary,
    dates_to_summary = dates_to_summary,
    months_to_summary = months_to_summary,
    verbose = verbose,
    ...
  ) |>
    # we are summarising an interpolator, dont forget to add again the parameters
    set_interpolation_params(
      params = get_interpolation_params(interpolator),
      verbose = verbose
    )
}
