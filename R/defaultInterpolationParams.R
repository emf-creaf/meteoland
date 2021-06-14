#Default parameters for meteorological interpolation
defaultInterpolationParams<-function() {
  return(list(
    initial_Rp = 140000, #Initial Rp value (in meters)
    iterations = 3, #Number of iterations in reestimation of Rp
    alpha_MinTemperature = 3.0, #Parameter of the Gaussian filter for temperature
    alpha_MaxTemperature = 3.0, #Parameter of the Gaussian filter for temperature
    alpha_DewTemperature = 3.0, #Parameter of the Gaussian filter for temperature
    alpha_PrecipitationEvent = 5.0, #Parameter of the Gaussian filter for precipitation
    alpha_PrecipitationAmount = 5.0, #Parameter of the Gaussian filter for precipitation
    alpha_Wind = 3.0, #Parameter of the Gaussian filter for precipitation
    N_MinTemperature = 30, #Number of required values for temperature
    N_MaxTemperature = 30, #Number of required values for temperature
    N_DewTemperature = 30, #Number of required values for temperature
    N_PrecipitationEvent = 5, #Number of required values for precipitation
    N_PrecipitationAmount = 20, #Number of required values for precipitation
    N_Wind = 2, #Number of required values for precipitation
    St_Precipitation = 5, #Integer with the number of days for precipitation smoothing
    St_TemperatureRange = 15, #Integer with the number of days for smoothing of temperature range
    pop_crit = 0.50, #Critical value for probability of precipitation
    f_max = 0.60, #Parameter for corrections of precipitation with elevation (f_max<1)
    wind_height = 10, #Wind height (in m)
    debug = FALSE
    ))
}
