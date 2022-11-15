#' Safely removing a station from the interpolator object
#'
#' Safely remove a station from the interpolator object for cross validation
#' processes
#'
#' This function removes the desired stations from the interpolator while
#' maintaining interpolation parameters attribute
#'
#' @param interpolator meteoland interpolator object
#' @param station_index numeric index for the station to remove
#'
#' @return An interpolator object without the station provided
#'
#' @noRd
.remove_station_from_interpolator <- function(interpolator, station_index) {
  res <- interpolator[, , -station_index]
  attr(res, "params") <- attr(interpolator, "params")
  return(res)
}

#' Cross validation process for a single station
#'
#' Cross validation process for a singel station
#'
#' This function performs the cross validation process for a single stations,
#' removing the station from the interpolator and using the spatial station
#' info to simulate a spatial data object
#'
#' @param station_index Numeric value with the station index
#' @param interpolator meteoland interpolator object
#'
#' @return A list with the interpolation results for the desired station
#'
#' @noRd
.station_cross_validation <- function(station_index, interpolator) {

  # messaging (not all, every 5 or 10 maybe?)
  if (station_index %% 5 == 0) {
    usethis::ui_line("Processing station {station_index}.....")
  }


  # get the point sf with topology info
  station_sf <- dplyr::tibble(
    elevation = interpolator[["elevation"]][1, station_index],
    aspect = interpolator[["aspect"]][1, station_index],
    slope = interpolator[["slope"]][1, station_index],
    geometry = stars::st_get_dimension_values(interpolator, 'station')[station_index]
  ) |>
    sf::st_as_sf()

  # remove the station from the interpolator
  interpolator_station_removed <-
    .remove_station_from_interpolator(interpolator, station_index)

  # return the interpolation
  suppressMessages(suppressWarnings(
    .interpolation_point(station_sf, interpolator_station_removed)
  ))[[1]] |>
    dplyr::mutate(
      RangeTemperature = MaxTemperature - MinTemperature,
      station = station_index
    ) |>
    dplyr::rename(RelativeHumidity = MeanRelativeHumidity)
}

#' Transform interpolator values to df
#'
#' Transform interpolator values to a data frame
#'
#' This function transform the observed meteorological values in the interpolator
#' object to a data frame for the cross validation process
#'
#' @param interpolator meteoland interpolator object
#'
#' @return A data frame with the station index, dates and meteorological observed
#'   values
#'
#' @noRd
.interpolator2tibble <- function(interpolator) {
  purrr::map_dfr(
    1:length(stars::st_get_dimension_values(interpolator, 'station')),
    function(station_index) {
      res <-
        dplyr::tibble(dates = stars::st_get_dimension_values(interpolator, 'date'))
      for (variable in names(interpolator)) {
        res[[variable]] <- interpolator[[variable]][,station_index]
      }
      res |>
        dplyr::mutate(
          RangeTemperature = MaxTemperature - MinTemperature,
          station = station_index,
          stationID = colnames(interpolator[[1]])[station_index]
        )
    }
  )
}

#' Helper to convert predicted values to NA
#'
#' Helper to convert predicted values to NA
#'
#' This function convert predicted values to NA where no observed values are
#' recorded, to allow for correct cross validation calculations
#'
#' @param predicted_df data frame with the predicted (interpolated) values
#' @param observed_df data frame with the observed values
#'
#' @return \code{predicted_df} data frame with NAs where the \code{observed_df}
#'   has NAs
#'
#' @noRd
.set_predicted_nas <- function(predicted_df, observed_df) {

  predicted_df$MinTemperature[is.na(observed_df$MinTemperature)] <- NA
  predicted_df$MaxTemperature[is.na(observed_df$MaxTemperature)] <- NA
  predicted_df$RangeTemperature[is.na(observed_df$RangeTemperature)] <- NA
  predicted_df$Precipitation[is.na(observed_df$Precipitation)] <- NA
  predicted_df$Radiation[is.na(observed_df$Radiation)] <- NA
  predicted_df$RelativeHumidity[is.na(observed_df$RelativeHumidity)] <- NA

  return(predicted_df)

}

#' Correlation between observed and predicted values
#'
#' Correlation between observed and predicted values
#'
#' This function return the correlation values for the provided meteorolgical
#' variable
#'
#' @param variable character with the variable name
#' @param predicted_df data frame with predicted values
#' @param observed_df data frame with observed values
#'
#' @return correlation value (numeric)
#'
#' @noRd
.cv_correlation <- function(variable, predicted_df, observed_df) {

  res <- NA

  if (sum(
    complete.cases(cbind(predicted_df[[variable]], observed_df[[variable]]))
  ) > 0) {
    res <-
      cor(predicted_df[[variable]], observed_df[[variable]], use = "complete.obs")
  }

  return(res)
}

#' Cross validation results processing
#'
#' Cross validation results processing
#'
#' This function creates the results elements for the cross validation returned
#' list
#'
#' @param predicted_df data frame with predicted values
#' @param observed_df data frame with observed values
#'
#' @return A list with the cross validation results (total errors, stations
#' stats, dates stats)
#'
#' @noRd
.cv_processing <- function(predicted_df, observed_df) {

  # calculate errors
  total_errors <- dplyr::tibble(
    dates = predicted_df[["dates"]],
    station = predicted_df[["station"]],
    stationID = observed_df[["stationID"]],
    MinTemperature_error =
      predicted_df[["MinTemperature"]] - observed_df[["MinTemperature"]],
    MaxTemperature_error =
      predicted_df[["MaxTemperature"]] - observed_df[["MaxTemperature"]],
    RangeTemperature_error =
      predicted_df[["RangeTemperature"]] - observed_df[["RangeTemperature"]],
    RelativeHumidity_error =
      predicted_df[["RelativeHumidity"]] - observed_df[["RelativeHumidity"]],
    Radiation_error =
      predicted_df[["Radiation"]] - observed_df[["Radiation"]],
    Precipitation_error =
      predicted_df[["Precipitation"]] - observed_df[["Precipitation"]]
  ) |>
    dplyr::mutate(
      MinTemperature_predicted = predicted_df[["MinTemperature"]],
      MaxTemperature_predicted = predicted_df[["MaxTemperature"]],
      RangeTemperature_predicted = predicted_df[["RangeTemperature"]],
      RelativeHumidity_predicted = predicted_df[["RelativeHumidity"]],
      Radiation_predicted = predicted_df[["Radiation"]],
      Precipitation_predicted = predicted_df[["Precipitation"]],
      MinTemperature_observed = observed_df[["MinTemperature"]],
      MaxTemperature_observed = observed_df[["MaxTemperature"]],
      RangeTemperature_observed = observed_df[["RangeTemperature"]],
      RelativeHumidity_observed = observed_df[["RelativeHumidity"]],
      Radiation_observed = observed_df[["Radiation"]],
      Precipitation_observed = observed_df[["Precipitation"]]
    )

  # bias and mae for stations
  station_stats <- total_errors |>
    dplyr::group_by(station) |>
    dplyr::summarise(
      stationID = dplyr::first(stationID),
      MinTemperature_station_bias = mean(MinTemperature_error, na.rm = TRUE),
      MaxTemperature_station_bias = mean(MaxTemperature_error, na.rm = TRUE),
      RangeTemperature_station_bias = mean(RangeTemperature_error, na.rm = TRUE),
      RelativeHumidity_station_bias = mean(RelativeHumidity_error, na.rm = TRUE),
      Radiation_station_bias = mean(Radiation_error, na.rm = TRUE),
      MinTemperature_station_mae = mean(abs(MinTemperature_error), na.rm = TRUE),
      MaxTemperature_station_mae = mean(abs(MaxTemperature_error), na.rm = TRUE),
      RangeTemperature_station_mae = mean(abs(RangeTemperature_error), na.rm = TRUE),
      RelativeHumidity_station_mae = mean(abs(RelativeHumidity_error), na.rm = TRUE),
      Radiation_station_mae = mean(abs(Radiation_error), na.rm = TRUE),
      TotalPrecipitation_station_bias =
        sum(Precipitation_observed, na.rm = TRUE) -
        sum(Precipitation_predicted, na.rm = TRUE),
      TotalPrecipitation_station_relative_bias = 100*(
        sum(Precipitation_observed, na.rm = TRUE) -
          sum(Precipitation_predicted, na.rm = TRUE)
      ) / sum(Precipitation_observed, na.rm = TRUE),
      DaysPrecipitation_station_bias =
        sum(Precipitation_observed > 0, na.rm = TRUE) -
        sum(Precipitation_predicted > 0, na.rm = TRUE),
      DaysPrecipitation_station_relative_bias = 100*(
        sum(Precipitation_observed > 0, na.rm = TRUE) -
          sum(Precipitation_predicted > 0, na.rm = TRUE)
      ) / sum(Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_station_observed =
        mean(Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_station_predicted =
        mean(Precipitation_predicted > 0, na.rm = TRUE)
    )

  dates_stats <- total_errors |>
    dplyr::group_by(dates) |>
    dplyr::summarise(
      MinTemperature_date_bias = mean(MinTemperature_error, na.rm = TRUE),
      MaxTemperature_date_bias = mean(MaxTemperature_error, na.rm = TRUE),
      RangeTemperature_date_bias = mean(RangeTemperature_error, na.rm = TRUE),
      RelativeHumidity_date_bias = mean(RelativeHumidity_error, na.rm = TRUE),
      Radiation_date_bias = mean(Radiation_error, na.rm = TRUE),
      MinTemperature_date_mae = mean(abs(MinTemperature_error), na.rm = TRUE),
      MaxTemperature_date_mae = mean(abs(MaxTemperature_error), na.rm = TRUE),
      RangeTemperature_date_mae = mean(abs(RangeTemperature_error), na.rm = TRUE),
      RelativeHumidity_date_mae = mean(abs(RelativeHumidity_error), na.rm = TRUE),
      Radiation_date_mae = mean(abs(Radiation_error), na.rm = TRUE),
      TotalPrecipitation_date_bias =
        sum(Precipitation_observed, na.rm = TRUE) -
        sum(Precipitation_predicted, na.rm = TRUE),
      TotalPrecipitation_date_relative_bias = 100*(
        sum(Precipitation_observed, na.rm = TRUE) -
          sum(Precipitation_predicted, na.rm = TRUE)
      ) / sum(Precipitation_observed, na.rm = TRUE),
      DaysPrecipitation_date_bias =
        sum(Precipitation_observed > 0, na.rm = TRUE) -
        sum(Precipitation_predicted > 0, na.rm = TRUE),
      DaysPrecipitation_date_relative_bias = 100*(
        sum(Precipitation_observed > 0, na.rm = TRUE) -
          sum(Precipitation_predicted > 0, na.rm = TRUE)
      ) / sum(Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_date_observed =
        mean(Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_date_predicted =
        mean(Precipitation_predicted > 0, na.rm = TRUE)
    )

  return(list(
    errors = total_errors,
    station_stats = station_stats,
    dates_stats = dates_stats
  ))
}

#' @describeIn interpolator_calibration
#'
#' @return \code{interpolation_cross_validation} returns a list with the
#' following items
#' \itemize{
#'   \item{errors: Data frame with each combination of station and date with
#'   observed variables, predicated variables and the total error
#'   (predicted - observed) calculated for each variable}
#'   \item{station_stats: Data frame with error and bias statistics aggregated by
#'   station}
#'   \item{dates_stats: Data frame with error and bias statistics aggregated by
#'   date}
#'   \item{r2: correlation indexes between observed and predicted values for each
#'   meteorological variable}
#' }
#'
#' @export
interpolation_cross_validation <- function(interpolator, stations = NULL) {

  # debug
  # browser()

  ### assertions
  # interpolator
  assertthat::assert_that(.is_interpolator(interpolator))
  # stations
  assertthat::assert_that(
    is.null(stations) || is.numeric(stations) || is.character(stations),
    msg = "stations must be NULL or a numeric vector with the stations indexes"
  )

  stations <- .station_indexes_converter(stations, interpolator)

  usethis::ui_info("Starting Cross Validation process...")
  observed_values <- .interpolator2tibble(interpolator) |>
    dplyr::filter(station %in% stations)

  usethis::ui_todo("interpolating stations...")
  predicted_values <-
    purrr::map_dfr(stations, .station_cross_validation, interpolator = interpolator) |>
    # set predicted to NA when observed is NA
    .set_predicted_nas(observed_values)

  ### Processing results
  usethis::ui_todo("calculating R squared...")
  r2_list <- c(
    MinTemperature = "MinTemperature", MaxTemperature = "MaxTemperature",
    RangeTemperature = "RangeTemperature", RelativeHumidity = "RelativeHumidity",
    Radiation = "Radiation"
  ) |>
    purrr::map(
      .cv_correlation,
      predicted_df = predicted_values, observed_df = observed_values
    )
  usethis::ui_todo("calculating errors, MAE and bias for interpolated variables...")
  res <- .cv_processing(predicted_values, observed_values)

  res$r2 <- r2_list

  usethis::ui_done("Cross validation done.")
  return(res)

}
