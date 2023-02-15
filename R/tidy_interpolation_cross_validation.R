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
#' Cross validation process for a single station
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

  # get the point sf with topography info
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
    .interpolation_point(
      station_sf, interpolator_station_removed, verbose = FALSE,
      .ignore_convex_hull_check = TRUE
    )
  ))[[1]] |>
    dplyr::mutate(
      RangeTemperature = .data$MaxTemperature - .data$MinTemperature,
      station = station_index
    ) |>
    dplyr::rename(RelativeHumidity = "MeanRelativeHumidity")
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
  purrr::map(
    1:length(stars::st_get_dimension_values(interpolator, 'station')),
    function(station_index) {
      res <-
        dplyr::tibble(dates = stars::st_get_dimension_values(interpolator, 'date'))
      for (variable in names(interpolator)) {
        res[[variable]] <- interpolator[[variable]][,station_index]
      }
      res |>
        dplyr::mutate(
          RangeTemperature = .data$MaxTemperature - .data$MinTemperature,
          station = station_index,
          stationID = colnames(interpolator[[1]])[station_index]
        )
    }
  ) |>
    purrr::list_rbind()
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
    dplyr::group_by(.data$station) |>
    dplyr::summarise(
      stationID = dplyr::first(.data$stationID),
      MinTemperature_station_bias = mean(.data$MinTemperature_error, na.rm = TRUE),
      MaxTemperature_station_bias = mean(.data$MaxTemperature_error, na.rm = TRUE),
      RangeTemperature_station_bias = mean(.data$RangeTemperature_error, na.rm = TRUE),
      RelativeHumidity_station_bias = mean(.data$RelativeHumidity_error, na.rm = TRUE),
      Radiation_station_bias = mean(.data$Radiation_error, na.rm = TRUE),
      MinTemperature_station_mae = mean(abs(.data$MinTemperature_error), na.rm = TRUE),
      MaxTemperature_station_mae = mean(abs(.data$MaxTemperature_error), na.rm = TRUE),
      RangeTemperature_station_mae = mean(abs(.data$RangeTemperature_error), na.rm = TRUE),
      RelativeHumidity_station_mae = mean(abs(.data$RelativeHumidity_error), na.rm = TRUE),
      Radiation_station_mae = mean(abs(.data$Radiation_error), na.rm = TRUE),
      TotalPrecipitation_station_observed = sum(.data$Precipitation_observed, na.rm = TRUE),
      TotalPrecipitation_station_predicted = sum(.data$Precipitation_predicted, na.rm = TRUE),
      TotalPrecipitation_station_bias =
        sum(.data$Precipitation_predicted, na.rm = TRUE) -
        sum(.data$Precipitation_observed, na.rm = TRUE),
      TotalPrecipitation_station_relative_bias = 100*(
        sum(.data$Precipitation_predicted, na.rm = TRUE) -
          sum(.data$Precipitation_observed, na.rm = TRUE)
      ) / sum(.data$Precipitation_observed, na.rm = TRUE),
      DaysPrecipitation_station_bias =
        sum(.data$Precipitation_predicted > 0, na.rm = TRUE) -
        sum(.data$Precipitation_observed > 0, na.rm = TRUE),
      DaysPrecipitation_station_relative_bias = 100*(
        sum(.data$Precipitation_predicted > 0, na.rm = TRUE) -
          sum(.data$Precipitation_observed > 0, na.rm = TRUE)
      ) / sum(.data$Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_station_observed =
        mean(.data$Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_station_predicted =
        mean(.data$Precipitation_predicted > 0, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      TotalPrecipitation_station_observed = dplyr::if_else(
        .data$TotalPrecipitation_station_observed == 0, NA_real_, .data$TotalPrecipitation_station_observed
      ),
      TotalPrecipitation_station_predicted = dplyr::if_else(
        .data$TotalPrecipitation_station_predicted == 0, NA_real_, .data$TotalPrecipitation_station_predicted
      ),
      TotalPrecipitation_station_bias = dplyr::if_else(
        .data$TotalPrecipitation_station_bias == 0, NA_real_, .data$TotalPrecipitation_station_bias
      ),
      TotalPrecipitation_station_relative_bias = dplyr::if_else(
        .data$TotalPrecipitation_station_relative_bias == 0, NA_real_, .data$TotalPrecipitation_station_relative_bias
      ),
      TotalPrecipitation_station_bias = dplyr::if_else(
        is.na(.data$TotalPrecipitation_station_predicted), NA_real_, .data$TotalPrecipitation_station_bias
      ),
      TotalPrecipitation_station_relative_bias = dplyr::if_else(
        is.na(.data$TotalPrecipitation_station_predicted), NA_real_, .data$TotalPrecipitation_station_relative_bias
      ),
      DaysPrecipitation_station_bias = dplyr::if_else(
        is.na(.data$TotalPrecipitation_station_predicted), NA_integer_, .data$DaysPrecipitation_station_bias
      ),
      DaysPrecipitation_station_relative_bias = dplyr::if_else(
        is.na(.data$TotalPrecipitation_station_predicted), NA_real_, .data$DaysPrecipitation_station_relative_bias
      ),
      FreqPrecipitation_station_observed = dplyr::if_else(
        is.na(.data$TotalPrecipitation_station_observed), NA_real_, .data$FreqPrecipitation_station_observed
      ),
      FreqPrecipitation_station_predicted = dplyr::if_else(
        is.na(.data$TotalPrecipitation_station_predicted), NA_real_, .data$FreqPrecipitation_station_predicted
      )
    )

  dates_stats <- total_errors |>
    dplyr::group_by(.data$dates) |>
    dplyr::summarise(
      MinTemperature_date_bias = mean(.data$MinTemperature_error, na.rm = TRUE),
      MaxTemperature_date_bias = mean(.data$MaxTemperature_error, na.rm = TRUE),
      RangeTemperature_date_bias = mean(.data$RangeTemperature_error, na.rm = TRUE),
      RelativeHumidity_date_bias = mean(.data$RelativeHumidity_error, na.rm = TRUE),
      Radiation_date_bias = mean(.data$Radiation_error, na.rm = TRUE),
      MinTemperature_date_mae = mean(abs(.data$MinTemperature_error), na.rm = TRUE),
      MaxTemperature_date_mae = mean(abs(.data$MaxTemperature_error), na.rm = TRUE),
      RangeTemperature_date_mae = mean(abs(.data$RangeTemperature_error), na.rm = TRUE),
      RelativeHumidity_date_mae = mean(abs(.data$RelativeHumidity_error), na.rm = TRUE),
      Radiation_date_mae = mean(abs(.data$Radiation_error), na.rm = TRUE),
      TotalPrecipitation_date_observed = sum(.data$Precipitation_observed, na.rm = TRUE),
      TotalPrecipitation_date_predicted = sum(.data$Precipitation_predicted, na.rm = TRUE),
      TotalPrecipitation_date_bias =
        sum(.data$Precipitation_predicted, na.rm = TRUE) -
        sum(.data$Precipitation_observed, na.rm = TRUE),
      TotalPrecipitation_date_relative_bias = 100*(
        sum(.data$Precipitation_predicted, na.rm = TRUE) -
          sum(.data$Precipitation_observed, na.rm = TRUE)
      ) / sum(.data$Precipitation_observed, na.rm = TRUE),
      StationsPrecipitation_date_bias =
        sum(.data$Precipitation_predicted > 0, na.rm = TRUE) -
        sum(.data$Precipitation_observed > 0, na.rm = TRUE),
      StationsPrecipitation_date_relative_bias = 100*(
        sum(.data$Precipitation_predicted > 0, na.rm = TRUE) -
          sum(.data$Precipitation_observed > 0, na.rm = TRUE)
      ) / sum(.data$Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_date_observed =
        mean(.data$Precipitation_observed > 0, na.rm = TRUE),
      FreqPrecipitation_date_predicted =
        mean(.data$Precipitation_predicted > 0, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      TotalPrecipitation_date_observed = dplyr::if_else(
        .data$TotalPrecipitation_date_observed == 0, NA_real_, .data$TotalPrecipitation_date_observed
      ),
      TotalPrecipitation_date_predicted = dplyr::if_else(
        .data$TotalPrecipitation_date_predicted == 0, NA_real_, .data$TotalPrecipitation_date_predicted
      ),
      TotalPrecipitation_date_bias = dplyr::if_else(
        .data$TotalPrecipitation_date_bias == 0, NA_real_, .data$TotalPrecipitation_date_bias
      ),
      TotalPrecipitation_date_relative_bias = dplyr::if_else(
        .data$TotalPrecipitation_date_relative_bias == 0, NA_real_, .data$TotalPrecipitation_date_relative_bias
      ),
      TotalPrecipitation_date_bias = dplyr::if_else(
        is.na(.data$TotalPrecipitation_date_predicted), NA_real_, .data$TotalPrecipitation_date_bias
      ),
      TotalPrecipitation_date_relative_bias = dplyr::if_else(
        is.na(.data$TotalPrecipitation_date_predicted), NA_real_, .data$TotalPrecipitation_date_relative_bias
      ),
      StationsPrecipitation_date_bias = dplyr::if_else(
        is.na(.data$TotalPrecipitation_date_predicted), NA_integer_, .data$StationsPrecipitation_date_bias
      ),
      StationsPrecipitation_date_relative_bias = dplyr::if_else(
        is.na(.data$TotalPrecipitation_date_predicted), NA_real_, .data$StationsPrecipitation_date_relative_bias
      ),
      FreqPrecipitation_date_observed = dplyr::if_else(
        is.na(.data$TotalPrecipitation_date_observed), NA_real_, .data$FreqPrecipitation_date_observed
      ),
      FreqPrecipitation_date_predicted = dplyr::if_else(
        is.na(.data$TotalPrecipitation_date_predicted), NA_real_, .data$FreqPrecipitation_date_predicted
      )
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
#' @examples
#'
#' \donttest{
#' # example interpolator
#' data("meteoland_interpolator_example")
#'
#' # As the cross validation for all stations can be time consuming, we are
#' # gonna use only for the first 5 stations of the 198
#' cv <- interpolation_cross_validation(meteoland_interpolator_example, stations = 1:5)
#'
#' # Inspect the results
#' cv$errors
#' cv$station_stats
#' cv$dates_stats
#' cv$r2
#' }
#'
#' @export
interpolation_cross_validation <- function(interpolator, stations = NULL, verbose = getOption("meteoland_verbosity", TRUE)) {

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

  .verbosity_control(
    cli::cli_alert_info("Starting Cross Validation process..."),
    verbose
  )

  observed_values <- .interpolator2tibble(interpolator) |>
    dplyr::filter(.data$station %in% stations)

  .verbosity_control(
    cli::cli_ul("Interpolating stations..."),
    verbose
  )
  predicted_values <-
    purrr::map(stations, .station_cross_validation, interpolator = interpolator, .progress = verbose) |>
    purrr::list_rbind() |>
    # set predicted to NA when observed is NA
    .set_predicted_nas(observed_values)

  ### Processing results
  .verbosity_control(
    cli::cli_ul("Calculating R squared..."),
    verbose
  )

  r2_list <- c(
    MinTemperature = "MinTemperature", MaxTemperature = "MaxTemperature",
    RangeTemperature = "RangeTemperature", RelativeHumidity = "RelativeHumidity",
    Radiation = "Radiation"
  ) |>
    purrr::map(
      .cv_correlation,
      predicted_df = predicted_values, observed_df = observed_values,
      .progress = verbose
    )

  .verbosity_control(
    cli::cli_ul("calculating errors, MAE and bias for interpolated variables..."),
    verbose
  )

  res <- .cv_processing(predicted_values, observed_values)

  res$r2 <- r2_list

  .verbosity_control(
    cli::cli_alert_success("Cross validation done."),
    verbose
  )

  return(res)

}
