#Default parameters for meteorological correction
defaultCorrectionParams<-function() {
  return(list(
   varmethods = list(MeanTemperature = "unbias",
                 MinTemperature = "scaling",
                 MaxTemperature = "scaling",
                 Precipitation = "quantmap",
                 MeanRelativeHumidity = "unbias",
                 Radiation = "unbias",
                 WindSpeed = "scaling"),
   fill_wind = TRUE,
   wind_height = 10 #Wind height (in m)
  ))
}
