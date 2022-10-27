#Default parameters for meteorological correction
defaultCorrectionParams<-function() {

  # deprecation warning
  lifecycle::deprecate_warn(
    when = "1.1.0", what = "defaultCorrectionParams()", with = NULL,
    details = "Better correction methods are provided by other packages (see * and * for example)"
  )

  return(list(
   varmethods = list(MeanTemperature = "unbias",
                 MinTemperature = "quantmap",
                 MaxTemperature = "quantmap",
                 Precipitation = "quantmap",
                 MeanRelativeHumidity = "unbias",
                 Radiation = "unbias",
                 WindSpeed = "quantmap"),
   qstep = 0.01,
   fill_wind = TRUE,
   allow_saturated = FALSE,
   wind_height = 10 #Wind height (in m)
  ))
}
