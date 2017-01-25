#Default parameters for meteorological downscaling
defaultDownscalingParams<-function() {
  return(list(
   fill_wind = TRUE,
   wind_height = 10 #Wind height (in m)
  ))
}
