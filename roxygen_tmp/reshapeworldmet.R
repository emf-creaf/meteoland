#' Reshapes weather data from 'meteospain', 'worldmet' or 'weathercan'
#' 
#' Reshapes weather station data acquired using the 'meteospain', 'worldmet' or
#' 'weathercan' R packages into formats useful for meteoland
#' 
#' Note that to have precipitation included in downloads from 'worldmet' you
#' should set 'precip = TRUE' when calling function 'importNOAA'. In the case
#' of weathercan, precipitation is only provided for daily data (i.e. setting
#' 'interval="day"' when calling 'weather_dl'), whereas wind speed and relative
#' humidity are only availave for hourly data (i.e., setting 'interval="hour"'
#' when calling 'weather_dl'). Hence, in \code{meteoland} we recommend
#' downloading both daily and hourly data and then calling function
#' \code{reshapeweathercan} to merge the two sources.
#' 
#' @aliases reshapemeteospain reshapeweathercan reshapeworldmet
#' @param weather_data Hourly or daily weather data, in form of an sf (spatial)
#' object, obtained using function 'get_meteo_from()' in package 'meteospain'.
#' @param hourly_data Hourly weather data. In the case of
#' \code{reshapeworldmet}, a tibble or data frame returned by function
#' 'importNOAA'. In the case of \code{reshapeweathercan}, a tibble or data
#' frame returned by function 'weather_dl' with 'interval="hour"'.
#' @param daily_data Daily weather data (only for \code{reshapeweathercan}), a
#' tibble or data frame returned by function 'weather_dl' with
#' 'interval="day"'.
#' @param output Kind of output desired. Either
#' '\code{\link{SpatialPointsTopography}}',
#' '\code{\link{SpatialPointsMeteorology}}' or
#' '\code{\link{MeteorologyInterpolationData}}'.
#' @param proj4string A string or CRS to change the coordinate reference system
#' of the output. If NULL the spatial reference will be geographic coordinates
#' (i.e. \code{CRS("+proj=longlat")}). When reshaping to
#' \code{MeteorologyInterpolationData} it is recommended to use a reference
#' system with meters in units, such as UTM.
#' @param complete A flag to indicate that missing variables should be
#' completed using function \code{\link{meteocomplete}}
#' @param verbose A flag to show information of the reshape process in the
#' console output.
#' @return An object of the class indicated in \code{output}.
#' @author Miquel De \enc{CáceresCaceres} Ainsa, CREAF
#' @seealso \code{\link{meteocomplete}}