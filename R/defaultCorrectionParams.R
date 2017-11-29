#Default parameters for meteorological correction
defaultCorrectionParams<-function() {
  return(list(
   varmethods = list(MeanTemperature = "unbias",
                 MinTemperature = "quantmap",
                 MaxTemperature = "quantmap",
                 Precipitation = "quantmap",
                 MeanRelativeHumidity = "unbias",
                 Radiation = "unbias",
                 WindSpeed = "quantmap"),
   fill_wind = TRUE,
   allow_saturated = FALSE,
   wind_height = 10 #Wind height (in m)
  ))
}
