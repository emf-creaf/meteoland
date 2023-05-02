#' @describeIn interpolation.cv `r lifecycle::badge("deprecated")`
#' @method summary interpolation.cv
#' @exportS3Method summary interpolation.cv
summary.interpolation.cv<-function(object,...) {

  # deprecation notice
  lifecycle::deprecate_stop(
    when = "2.0.0", what = "summary.interpolation.cv()", with = NULL,
    details = "interpolation_cross_validation() returns a list with the cross validation results for
    dates and for stations. List elements are data frames that can be summarise in any usual way (base, tidyverse...)"
  )

  # mintemp_error = as.matrix(object$MinTemperatureError)
  # maxtemp_error = as.matrix(object$MaxTemperatureError)
  # temprange_error = as.matrix(object$TemperatureRangeError)
  # rh_error = as.matrix(object$RelativeHumidityError)
  # rad_error = as.matrix(object$RadiationError)
  # mintemp_station_bias = object$stations$MinTemperature.Bias
  # mintemp_day_bias = object$dates$MinTemperature.Bias
  # stats <- data.frame(n = rep(NA,13),
  #                     r = rep(NA,13),
  #                     MAE = rep(NA,13),
  #                     sd.station.MAE = rep(NA,13),
  #                     sd.dates.MAE = rep(NA,13),
  #                     Bias = rep(NA,13),
  #                     sd.station.Bias = rep(NA,13),
  #                     sd.dates.Bias = rep(NA,13),
  #                     row.names = c("MinTemperature", "MaxTemperature", "TemperatureRange", "RelativeHumidity", "Radiation",
  #                                   "Station.rainfall", "Station.rainfall.relative",
  #                                   "Station.precdays", "Station.precdays.relative",
  #                                   "Date.rainfall", "Date.rainfall.relative",
  #                                   "Date.precstations", "Date.precstations.relative"))
  #
  # stats["MinTemperature","n"] = sum(!is.na(object$MinTemperatureError))
  # stats["MaxTemperature","n"] = sum(!is.na(object$MaxTemperatureError))
  # stats["TemperatureRange","n"] = sum(!is.na(object$TemperatureRangeError))
  # stats["RelativeHumidity","n"] = sum(!is.na(object$RelativeHumidityError))
  # stats["Radiation","n"] = sum(!is.na(object$RadiationError))
  # stats["Station.rainfall","n"] = sum(!is.na(object$stations$TotalPrec.Bias))
  # stats["Station.rainfall.relative","n"] = sum(!is.na(object$stations$TotalPrec.RelBias))
  # stats["Station.precdays","n"] = sum(!is.na(object$stations$PrecDays.Bias))
  # stats["Station.precdays.relative","n"] = sum(!is.na(object$stations$PrecDays.RelBias))
  # stats["Date.rainfall","n"] = sum(!is.na(object$dates$TotalPrec.Bias))
  # stats["Date.rainfall.relative","n"] = sum(!is.na(object$dates$TotalPrec.RelBias))
  # stats["Date.precstations","n"] = sum(!is.na(object$dates$PrecStations.Bias))
  # stats["Date.precstations.relative","n"] = sum(!is.na(object$dates$PrecStations.RelBias))
  #
  # stats["MinTemperature","r"] = object$r$MinTemperature
  # stats["MaxTemperature","r"] = object$r$MaxTemperature
  # stats["TemperatureRange","r"] = object$r$TemperatureRange
  # stats["RelativeHumidity","r"] = object$r$RelativeHumidity
  # stats["Radiation","r"] = object$r$Radiation
  # stats["Station.rainfall","r"] = cor(object$stations$TotalPrec.Obs, object$stations$TotalPrec.Pred, use="complete.obs")
  # stats["Station.precdays","r"] = cor(object$stations$PrecFreq.Obs, object$stations$PrecFreq.Pred, use="complete.obs")
  # stats["Date.rainfall","r"] = cor(object$dates$TotalPrec.Obs, object$dates$TotalPrec.Pred, use="complete.obs")
  # stats["Date.precstations","r"] = cor(object$dates$PrecFreq.Obs, object$dates$PrecFreq.Pred, use="complete.obs")
  #
  # stats["MinTemperature","MAE"] = mean(abs(mintemp_error),na.rm=TRUE)
  # stats["MaxTemperature","MAE"] = mean(abs(maxtemp_error),na.rm=TRUE)
  # stats["TemperatureRange","MAE"] = mean(abs(temprange_error),na.rm=TRUE)
  # stats["RelativeHumidity", "MAE"] = mean(abs(rh_error), na.rm=TRUE)
  # stats["Radiation", "MAE"] = mean(abs(rad_error), na.rm=TRUE)
  # stats["Station.rainfall", "MAE"] = mean(abs(object$stations$TotalPrec.Bias), na.rm=TRUE)
  # stats["Station.rainfall.relative", "MAE"] = mean(abs(object$stations$TotalPrec.RelBias), na.rm=TRUE)
  # stats["Station.precdays", "MAE"] = mean(abs(object$stations$PrecDays.Bias), na.rm=TRUE)
  # stats["Station.precdays.relative", "MAE"] = mean(abs(object$stations$PrecDays.RelBias), na.rm=TRUE)
  # stats["Date.rainfall", "MAE"] = mean(abs(object$dates$TotalPrec.Bias), na.rm=TRUE)
  # stats["Date.rainfall.relative", "MAE"] = mean(abs(object$dates$TotalPrec.RelBias), na.rm=TRUE)
  # stats["Date.precstations", "MAE"] = mean(abs(object$dates$PrecStations.Bias), na.rm=TRUE)
  # stats["Date.precstations.relative", "MAE"] = mean(abs(object$dates$PrecStations.RelBias), na.rm=TRUE)
  #
  # stats["MinTemperature","sd.station.MAE"] = sqrt(var(object$stations$MinTemperature.MAE,na.rm=TRUE))
  # stats["MaxTemperature","sd.station.MAE"] = sqrt(var(object$stations$MaxTemperature.MAE,na.rm=TRUE))
  # stats["TemperatureRange","sd.station.MAE"] = sqrt(var(object$stations$TemperatureRange.MAE,na.rm=TRUE))
  # stats["RelativeHumidity","sd.station.MAE"] = sqrt(var(object$stations$RelativeHumidity.MAE,na.rm=TRUE))
  # stats["Radiation","sd.station.MAE"] = sqrt(var(object$stations$Radiation.MAE,na.rm=TRUE))
  # stats["Station.rainfall", "sd.station.MAE"] = sqrt(var(abs(object$stations$TotalPrec.Bias),na.rm=TRUE))
  # stats["Station.rainfall.relative", "sd.station.MAE"] = sqrt(var(abs(object$stations$TotalPrec.RelBias),na.rm=TRUE))
  # stats["Station.precdays", "sd.station.MAE"] = sqrt(var(abs(object$stations$PrecDays.Bias),na.rm=TRUE))
  # stats["Station.precdays.relative", "sd.station.MAE"] = sqrt(var(abs(object$stations$PrecDays.RelBias),na.rm=TRUE))
  #
  # stats["MinTemperature","sd.dates.MAE"] = sqrt(var(object$dates$MinTemperature.MAE,na.rm=TRUE))
  # stats["MaxTemperature","sd.dates.MAE"] = sqrt(var(object$dates$MaxTemperature.MAE,na.rm=TRUE))
  # stats["TemperatureRange","sd.dates.MAE"] = sqrt(var(object$dates$TemperatureRange.MAE,na.rm=TRUE))
  # stats["RelativeHumidity","sd.dates.MAE"] = sqrt(var(object$dates$RelativeHumidity.MAE,na.rm=TRUE))
  # stats["Radiation","sd.dates.MAE"] = sqrt(var(object$dates$Radiation.MAE,na.rm=TRUE))
  # stats["Date.rainfall", "sd.dates.MAE"] = sqrt(var(abs(object$dates$TotalPrec.Bias),na.rm=TRUE))
  # stats["Date.rainfall.relative", "sd.dates.MAE"] = sqrt(var(abs(object$dates$TotalPrec.RelBias),na.rm=TRUE))
  # stats["Date.precstations", "sd.dates.MAE"] = sqrt(var(abs(object$dates$PrecStations.Bias),na.rm=TRUE))
  # stats["Date.precstations.relative", "sd.dates.MAE"] = sqrt(var(abs(object$dates$PrecStations.RelBias),na.rm=TRUE))
  #
  # stats["MinTemperature","Bias"] = mean(mintemp_error,na.rm=TRUE)
  # stats["MaxTemperature","Bias"] = mean(maxtemp_error,na.rm=TRUE)
  # stats["TemperatureRange","Bias"] = mean(temprange_error,na.rm=TRUE)
  # stats["RelativeHumidity", "Bias"] = mean(rh_error, na.rm=TRUE)
  # stats["Radiation", "Bias"] = mean(rad_error, na.rm=TRUE)
  # stats["Station.rainfall", "Bias"] = mean(object$stations$TotalPrec.Bias, na.rm=TRUE)
  # stats["Station.rainfall.relative", "Bias"] = mean(object$stations$TotalPrec.RelBias, na.rm=TRUE)
  # stats["Station.precdays", "Bias"] = mean(object$stations$PrecDays.Bias, na.rm=TRUE)
  # stats["Station.precdays.relative", "Bias"] = mean(object$stations$PrecDays.RelBias, na.rm=TRUE)
  # stats["Date.rainfall", "Bias"] = mean(object$dates$TotalPrec.Bias, na.rm=TRUE)
  # stats["Date.rainfall.relative", "Bias"] = mean(object$dates$TotalPrec.RelBias, na.rm=TRUE)
  # stats["Date.precstations", "Bias"] = mean(object$dates$PrecStations.Bias, na.rm=TRUE)
  # stats["Date.precstations.relative", "Bias"] = mean(object$dates$PrecStations.RelBias, na.rm=TRUE)
  #
  # stats["MinTemperature","sd.station.Bias"] = sqrt(var(object$stations$MinTemperature.Bias,na.rm=TRUE))
  # stats["MaxTemperature","sd.station.Bias"] = sqrt(var(object$stations$MaxTemperature.Bias,na.rm=TRUE))
  # stats["TemperatureRange","sd.station.Bias"] = sqrt(var(object$stations$TemperatureRange.Bias,na.rm=TRUE))
  # stats["RelativeHumidity","sd.station.Bias"] = sqrt(var(object$stations$RelativeHumidity.Bias,na.rm=TRUE))
  # stats["Radiation","sd.station.Bias"] = sqrt(var(object$stations$Radiation.Bias,na.rm=TRUE))
  # stats["Station.rainfall", "sd.station.Bias"] = sqrt(var(object$stations$TotalPrec.Bias,na.rm=TRUE))
  # stats["Station.rainfall.relative", "sd.station.Bias"] = sqrt(var(object$stations$TotalPrec.RelBias,na.rm=TRUE))
  # stats["Station.precdays", "sd.station.Bias"] = sqrt(var(object$stations$PrecDays.Bias,na.rm=TRUE))
  # stats["Station.precdays.relative", "sd.station.Bias"] = sqrt(var(object$stations$PrecDays.RelBias,na.rm=TRUE))
  #
  # stats["MinTemperature","sd.dates.Bias"] = sqrt(var(object$dates$MinTemperature.Bias,na.rm=TRUE))
  # stats["MaxTemperature","sd.dates.Bias"] = sqrt(var(object$dates$MaxTemperature.Bias,na.rm=TRUE))
  # stats["TemperatureRange","sd.dates.Bias"] = sqrt(var(object$dates$TemperatureRange.Bias,na.rm=TRUE))
  # stats["RelativeHumidity","sd.dates.Bias"] = sqrt(var(object$dates$RelativeHumidity.Bias,na.rm=TRUE))
  # stats["Radiation","sd.dates.Bias"] = sqrt(var(object$dates$Radiation.Bias,na.rm=TRUE))
  # stats["Date.rainfall", "sd.dates.Bias"] = sqrt(var(object$dates$TotalPrec.Bias,na.rm=TRUE))
  # stats["Date.rainfall.relative", "sd.dates.Bias"] = sqrt(var(object$dates$TotalPrec.RelBias,na.rm=TRUE))
  # stats["Date.precstations", "sd.dates.Bias"] = sqrt(var(object$dates$PrecStations.Bias,na.rm=TRUE))
  # stats["Date.precstations.relative", "sd.dates.Bias"] = sqrt(var(object$dates$PrecStations.RelBias,na.rm=TRUE))
  # return(stats)
}
