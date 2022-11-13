#' Calibration and validation of interpolation procedures
#' 
#' Calibration and validation of interpolation procedures
#' 
#' Function \code{interpolator_calibration} determines optimal interpolation
#' parameters \code{"N"} and \code{"alpha"} for a given meteorological
#' variable. Optimization is done by minimizing mean absolute error ("MAE")
#' (Thornton \emph{et al.} 1997). Function
#' \code{interpolation_cross_validation} calculates average mean absolute
#' errors ("MAE") for the prediction period of the interpolator object. In both
#' calibration and cross validation procdeures, predictions for each
#' meteorological station are made using a \emph{leave-one-out} procedure (i.e.
#' after exluding the station form the predictive set).
#' 
#' @aliases interpolator_calibration interpolation_cross_validation
#' @param interpolator A meteoland interpolator object, as created by
#' \code{\link{create_meteoland_interpolator}}
#' @param stations A vector with the station indexes (numeric) to be used to
#' calculate \code{"MAE"}. All stations with data are included in the training
#' set but predictive \code{"MAE"} are calculated for the stations subset
#' indicated in \code{stations} param only.
#' @param variable A string indicating the meteorological variable for which
#' interpolation parameters \code{"N"} and \code{"alpha"} will be calibrated.
#' Accepted values are \code{MinTemperature}, \code{MaxTemperature},
#' \code{DewTemperature}, \code{Precipitation} (for precipitation with the same
#' values for precipitation events an regression of precipitation amounts),
#' \code{PrecipitationAmount} (for regression of precipitation amounts) and
#' \code{PrecipitationEvent} (for precipitation events).
#' @param N_seq Numeric vector with \code{"N"} values to be tested
#' @param alpha_seq Numeric vector with \code{"alpha"}
#' @return \code{interpolator_calibration} returns a list with the following
#' items \itemize{ \itemMAE: A numeric matrix with the mean absolute error
#' values, averaged across stations, for each combination of parameters
#' \code{"N"} and \code{"alpha"} \itemminMAE: Minimum MAE value \itemN: Value
#' of parameter \code{"N"} corresponding to the minimum MAE \itemalpha: Value
#' of parameter \code{"alpha"} corresponding the the minimum MAE \itemobserved:
#' matrix with observed values (meteorological measured values) \itempredicted:
#' matrix with interpolated values for the optimum parameter combination }
#' 
#' \code{interpolation_cross_validation} returns a list with the following
#' items \itemize{ \itemtotal_errors: Data frame with each combination of
#' station and date with observed variables, predicated variables and the total
#' error (predicted - observed) calculated for each variable
#' \itemstation_stats: Data frame with error and bias statistics aggregated by
#' station \itemdates_stats: Data frame with error and bias statistics
#' aggregated by date \itemr2: correlation indexes between observed and
#' predicted values for each meteorological variable }
#' @section Functions: \itemize{ \item \code{interpolation_cross_validation()}:
#' 
#' }