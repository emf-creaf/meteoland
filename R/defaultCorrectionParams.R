#Default parameters for meteorological correction
defaultCorrectionParams<-function() {
  return(list(
   fill_wind = TRUE,
   wind_height = 10 #Wind height (in m)
  ))
}
